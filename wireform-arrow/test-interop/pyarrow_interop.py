#!/usr/bin/env python3
"""pyarrow side of the wireform-arrow interop test-suite.

The Haskell test-suite ``wireform-arrow-pyarrow-interop``
(``test-interop/Main.hs``) drives this script. Both sides define the
same case matrix independently: here every case is a list of
``pyarrow.RecordBatch`` values built with pyarrow's own constructors,
and ``Main.hs`` builds the same data as wireform-arrow ``ColumnArray``
values. Case names tie the two definitions together, and the Haskell
driver fails when the two name sets differ.

Subcommands (all output is line oriented so the Haskell driver can
parse it):

  cases          print every case name, one per line
  check DIR      for every DIR/<case>.<variant>.{arrows,arrow} written
                 by wireform-arrow: open it with ipc.open_stream or
                 ipc.open_file, run validate(full=True) on every batch,
                 and compare schema (including custom metadata) and
                 batch values against the expected batches built here.
                 Prints "PASS <file>" or "FAIL <file>: <reason>".
  write DIR      write every case with pyarrow, once per variant
                 (plain, sliced, zstd, lz4), as both stream and file
                 (stream only for replacement / delta dictionary
                 cases). Prints "WROTE <file>" or
                 "SKIP <case>.<variant>: <reason>".
  regen-goldens  regenerate test/golden/pa_*.arrows, the fixtures the
                 pyarrow-free wireform-arrow-test suite reads.

pyarrow's Python layer cannot materialise INTERVAL(YEAR_MONTH) or
INTERVAL(DAY_TIME) arrays (no Python array class exists for them), so
those cases are built through the Arrow C data interface with ctypes
and handled only through RecordBatch-level C++ operations (validate,
equals, IPC read/write, slice, concat).
"""

from __future__ import annotations

import ctypes
import decimal
import os
import struct
import sys
from pathlib import Path

import pyarrow as pa
import pyarrow.ipc as ipc

ROOT = Path(__file__).resolve().parent.parent
GOLDEN_DIR = ROOT / "test" / "golden"

D = decimal.Decimal


# ---------------------------------------------------------------------------
# Arrow C data interface (only for the two interval types pyarrow's Python
# layer cannot represent).
# ---------------------------------------------------------------------------


class _ArrowSchema(ctypes.Structure):
    pass


class _ArrowArray(ctypes.Structure):
    pass


_SREL = ctypes.CFUNCTYPE(None, ctypes.POINTER(_ArrowSchema))
_AREL = ctypes.CFUNCTYPE(None, ctypes.POINTER(_ArrowArray))
_ArrowSchema._fields_ = [
    ("format", ctypes.c_char_p),
    ("name", ctypes.c_char_p),
    ("metadata", ctypes.c_char_p),
    ("flags", ctypes.c_int64),
    ("n_children", ctypes.c_int64),
    ("children", ctypes.c_void_p),
    ("dictionary", ctypes.c_void_p),
    ("release", _SREL),
    ("private_data", ctypes.c_void_p),
]
_ArrowArray._fields_ = [
    ("length", ctypes.c_int64),
    ("null_count", ctypes.c_int64),
    ("offset", ctypes.c_int64),
    ("n_buffers", ctypes.c_int64),
    ("n_children", ctypes.c_int64),
    ("buffers", ctypes.POINTER(ctypes.c_void_p)),
    ("children", ctypes.POINTER(ctypes.POINTER(_ArrowArray))),
    ("dictionary", ctypes.c_void_p),
    ("release", _AREL),
    ("private_data", ctypes.c_void_p),
]

# Memory handed to pyarrow through the C interface. Never freed: the
# process exits through os._exit, so teardown order cannot run a
# release callback after its ctypes thunk is gone.
_KEEP: list = []


@_SREL
def _schema_release(p):
    p.contents.release = _SREL()


@_AREL
def _array_release(p):
    p.contents.release = _AREL()


def _c_type(fmt: bytes) -> pa.DataType:
    s = _ArrowSchema(format=fmt, name=b"", flags=2, release=_schema_release)
    _KEEP.append(s)
    return pa.DataType._import_from_c(ctypes.addressof(s))


MONTH_INTERVAL = _c_type(b"tiM")
DAY_TIME_INTERVAL = _c_type(b"tiD")


def _raw_batch(schema: pa.Schema, length: int, cols) -> pa.RecordBatch:
    """Import a record batch whose columns are given as raw buffers.

    ``cols`` is a list of ``(null_count, [validity or None, data])``.
    """
    children = []
    for null_count, bufs in cols:
        ptrs = (ctypes.c_void_p * len(bufs))()
        for i, b in enumerate(bufs):
            if b is None:
                ptrs[i] = None
            else:
                cb = ctypes.create_string_buffer(b, len(b) + 8)
                _KEEP.append(cb)
                ptrs[i] = ctypes.addressof(cb)
        arr = _ArrowArray(
            length=length,
            null_count=null_count,
            offset=0,
            n_buffers=len(bufs),
            n_children=0,
            buffers=ptrs,
            release=_array_release,
        )
        _KEEP.extend([ptrs, arr])
        children.append(ctypes.pointer(arr))
    top_bufs = (ctypes.c_void_p * 1)(None)
    kids = (ctypes.POINTER(_ArrowArray) * len(children))(*children)
    top = _ArrowArray(
        length=length,
        null_count=0,
        offset=0,
        n_buffers=1,
        n_children=len(children),
        buffers=top_bufs,
        children=kids,
        release=_array_release,
    )
    _KEEP.extend([top_bufs, kids, top])
    return pa.RecordBatch._import_from_c(ctypes.addressof(top), schema)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def bitmap(valid: list[bool]) -> bytes:
    out = bytearray((len(valid) + 7) // 8)
    for i, v in enumerate(valid):
        if v:
            out[i // 8] |= 1 << (i % 8)
    return bytes(out)


def no_nulls(xs: list) -> list:
    return [x for x in xs if x is not None]


def float16_array(bits: list) -> pa.Array:
    n = len(bits)
    data = struct.pack("<" + "H" * n, *[0 if b is None else b for b in bits])
    nulls = sum(1 for b in bits if b is None)
    validity = pa.py_buffer(bitmap([b is not None for b in bits])) if nulls else None
    return pa.Array.from_buffers(pa.float16(), n, [validity, pa.py_buffer(data)], nulls)


def mdn(t):
    return None if t is None else pa.MonthDayNano(list(t))


def schema1(name: str, typ: pa.DataType, nullable: bool, metadata=None) -> pa.Schema:
    return pa.schema([pa.field(name, typ, nullable=nullable)], metadata=metadata)


def batch1(schema: pa.Schema, arr: pa.Array) -> pa.RecordBatch:
    return pa.record_batch([arr], schema=schema)


class Case:
    def __init__(self, name, schema, batches, stream_only=False, delta=False):
        self.name = name
        self.schema = schema
        self.batches = batches
        self.stream_only = stream_only
        self.delta = delta


CASES: dict[str, Case] = {}


def add(case: Case) -> None:
    if case.name in CASES:
        raise RuntimeError(f"duplicate case {case.name}")
    CASES[case.name] = case


def scalar(name: str, typ: pa.DataType, b1: list, b2: list, mk=None) -> None:
    """Register ``name`` (non-nullable field, nulls dropped from b1) and
    ``name_nullable`` (nullable field, b1 with nulls, b2 without)."""
    build = mk if mk is not None else (lambda xs: pa.array(xs, type=typ))
    s = schema1("x", typ, False)
    add(Case(name, s, [batch1(s, build(no_nulls(b1))), batch1(s, build(b2))]))
    sn = schema1("x", typ, True)
    add(Case(name + "_nullable", sn, [batch1(sn, build(b1)), batch1(sn, build(b2))]))


def raw_scalar(name: str, typ: pa.DataType, fmt: str, b1: list, b2: list) -> None:
    """Like ``scalar`` for types that only exist at the C++ layer."""

    def flat(x):
        return list(x) if isinstance(x, tuple) else [x]

    def mk(schema, xs):
        width = struct.calcsize("<" + fmt)
        data = b"".join(
            b"\x00" * width if x is None else struct.pack("<" + fmt, *flat(x)) for x in xs
        )
        nulls = sum(1 for x in xs if x is None)
        validity = bitmap([x is not None for x in xs]) if nulls else None
        return _raw_batch(schema, len(xs), [(nulls, [validity, data])])

    s = schema1("x", typ, False)
    add(Case(name, s, [mk(s, no_nulls(b1)), mk(s, b2)]))
    sn = schema1("x", typ, True)
    add(Case(name + "_nullable", sn, [mk(sn, b1), mk(sn, b2)]))


# ---------------------------------------------------------------------------
# The case matrix. Keep in sync with test-interop/Main.hs.
# ---------------------------------------------------------------------------


def build_cases() -> None:
    I64MAX = 9223372036854775807
    scalar("int8", pa.int8(), [0, 1, None, -1, 127, -128], [42, -42])
    scalar("int16", pa.int16(), [0, 1, None, -1, 32767, -32768], [1000, -1000])
    scalar("int32", pa.int32(), [0, 1, None, -1, 2147483647, -2147483648], [7, -7])
    scalar("int64", pa.int64(), [0, 1, None, -1, I64MAX, -I64MAX - 1], [123456789012, -5])
    scalar("uint8", pa.uint8(), [0, 1, None, 255], [128])
    scalar("uint16", pa.uint16(), [0, 1, None, 65535], [256])
    scalar("uint32", pa.uint32(), [0, 1, None, 4294967295], [65536])
    scalar("uint64", pa.uint64(), [0, 1, None, 18446744073709551615], [4294967296])
    scalar(
        "float16",
        pa.float16(),
        [0x0000, 0x3C00, None, 0xC000, 0x7BFF, 0x7C00],
        [0x3800],
        mk=float16_array,
    )
    inf = float("inf")
    scalar("float32", pa.float32(), [0.0, 1.5, None, -2.5, inf, 3.4028234663852886e38], [0.25])
    scalar("float64", pa.float64(), [0.0, 1.5, None, -2.5, -inf, 1e300, 5e-324], [3.141592653589793])
    scalar(
        "bool",
        pa.bool_(),
        [True, False, None, True, True, False, False, True, False],
        [False, True],
    )
    strs = ["", "a", None, "h\u00e9llo w\u00f6rld", "\U0001F600 emoji"]
    scalar("utf8", pa.utf8(), strs, ["second batch"])
    scalar("large_utf8", pa.large_utf8(), strs, ["second batch"])
    bins = [b"", b"\x00\x01\x02", None, b"\xff" * 20]
    scalar("binary", pa.binary(), bins, [b"z"])
    scalar("large_binary", pa.large_binary(), bins, [b"z"])
    scalar(
        "fixed_size_binary4",
        pa.binary(4),
        [b"abcd", b"\x00\x00\x00\x00", None, b"\xff\xfe\xfd\xfc"],
        [b"wxyz"],
    )
    views = [
        "",
        "short",
        None,
        "exactly12byt",
        "thirteen byte",
        "this string is definitely longer than twelve bytes",
    ]
    scalar("utf8_view", pa.string_view(), views, ["inline", "an out of line string value"])
    scalar(
        "binary_view",
        pa.binary_view(),
        [None if v is None else v.encode() for v in views],
        [b"inline", b"\x00" * 13],
    )
    scalar("date32", pa.date32(), [0, 18000, None, -1, 19000], [20000])
    scalar("date64", pa.date64(), [0, 1555200000000, None, -86400000], [86400000])
    scalar("time32_s", pa.time32("s"), [0, 3600, None, 86399], [60])
    scalar("time32_ms", pa.time32("ms"), [0, 1000, None, 86399999], [1])
    scalar("time64_us", pa.time64("us"), [0, 12345000000, None, 86399999999], [1])
    scalar("time64_ns", pa.time64("ns"), [0, 1, None, 86399999999999], [2])
    for unit in ["s", "ms", "us", "ns"]:
        ts = [0, 1700000000, None, -1]
        scalar(f"timestamp_{unit}", pa.timestamp(unit), ts, [42])
        scalar(f"timestamp_{unit}_utc", pa.timestamp(unit, tz="UTC"), ts, [42])
        scalar(f"duration_{unit}", pa.duration(unit), [0, 60, None, -1], [1])
    scalar(
        "timestamp_us_new_york",
        pa.timestamp("us", tz="America/New_York"),
        [0, 1700000000000000, None],
        [5],
    )
    scalar(
        "decimal128_18_2",
        pa.decimal128(18, 2),
        [D("0.00"), D("1.23"), None, D("-1.23"), D("9999999999999999.99")],
        [D("0.01")],
    )
    scalar(
        "decimal128_38_10",
        pa.decimal128(38, 10),
        [D("12345678901234567890.1234567890"), None, D("-0.0000000001")],
        [D("1.0000000000")],
    )
    scalar(
        "decimal256_76_5",
        pa.decimal256(76, 5),
        [
            D("1.00000"),
            None,
            D("-12345678901234567890123456789012345678901234567890.12345"),
        ],
        [D("0.00001")],
    )
    scalar(
        "interval_month_day_nano",
        pa.month_day_nano_interval(),
        [mdn((1, 2, 3)), mdn((0, 0, 0)), None, mdn((-1, -2, -3))],
        [mdn((12, 30, 1000000000))],
    )
    raw_scalar("interval_year_month", MONTH_INTERVAL, "i", [0, 14, None, -3], [5])
    raw_scalar(
        "interval_day_time",
        DAY_TIME_INTERVAL,
        "ii",
        [(0, 0), (1, 1000), None, (-1, -1)],
        [(2, 2)],
    )

    # Null type: every slot is null, no buffers.
    s = schema1("x", pa.null(), True)
    add(Case("null", s, [batch1(s, pa.nulls(4)), batch1(s, pa.nulls(1))]))

    # Struct<i: int32 not null, s: utf8>.
    st = pa.struct([pa.field("i", pa.int32(), nullable=False), pa.field("s", pa.utf8())])
    rows = [{"i": 1, "s": "a"}, {"i": 2, "s": None}, {"i": 3, "s": "c"}]
    scalar("struct", st, rows[:1] + [None] + rows[1:], [{"i": 4, "s": "d"}])

    scalar(
        "list_int32",
        pa.list_(pa.int32()),
        [[1, 2], None, [], [3, None, 5]],
        [[6], [7, 8]],
    )
    scalar(
        "large_list_utf8",
        pa.large_list(pa.utf8()),
        [["a", "b"], None, [], ["c"]],
        [["d"]],
    )
    scalar(
        "fixed_size_list_int16_3",
        pa.list_(pa.int16(), 3),
        [[1, 2, 3], None, [4, None, 6]],
        [[7, 8, 9]],
    )
    scalar(
        "map_utf8_int32",
        pa.map_(pa.utf8(), pa.int32()),
        [[("a", 1), ("b", None)], None, [], [("c", 3)]],
        [[("d", 4)]],
    )

    def list_view(large: bool):
        typ = pa.large_list_view(pa.int32()) if large else pa.list_view(pa.int32())
        it = pa.int64() if large else pa.int32()
        child = pa.array([10, 20, 30, None, 50, 60], pa.int32())

        def mk(offs, sizes, valid, values=child):
            mask = None if valid is None else pa.array([not v for v in valid], pa.bool_())
            cls = pa.LargeListViewArray if large else pa.ListViewArray
            return cls.from_arrays(
                pa.array(offs, it), pa.array(sizes, it), values, type=typ, mask=mask
            )

        name = "large_list_view_int32" if large else "list_view_int32"
        b2 = mk([0], [1], None, pa.array([99], pa.int32()))
        s = schema1("x", typ, False)
        add(Case(name, s, [batch1(s, mk([4, 0, 1, 0], [2, 3, 0, 1], None)), batch1(s, b2)]))
        sn = schema1("x", typ, True)
        add(
            Case(
                name + "_nullable",
                sn,
                [
                    batch1(sn, mk([4, 0, 1, 0, 2], [2, 0, 0, 1, 2], [True, False, True, True, True])),
                    batch1(sn, b2),
                ],
            )
        )

    list_view(False)
    list_view(True)

    # Unions. Children are nullable; the union itself has no validity.
    def union(name: str, dense: bool, codes: list[int], nullable_field: bool):
        names = ["i", "s"]
        if dense:
            def mk(types, offsets, ints, strs):
                return pa.UnionArray.from_dense(
                    pa.array(types, pa.int8()),
                    pa.array(offsets, pa.int32()),
                    [pa.array(ints, pa.int32()), pa.array(strs, pa.utf8())],
                    names,
                    codes,
                )

            b1 = mk(
                [codes[0], codes[1], codes[0], codes[0], codes[1]],
                [0, 0, 1, 2, 1],
                [1, 2, None],
                ["a", "b"],
            )
            b2 = mk([codes[1]], [0], [], ["z"])
        else:
            def mk(types, ints, strs):
                return pa.UnionArray.from_sparse(
                    pa.array(types, pa.int8()),
                    [pa.array(ints, pa.int32()), pa.array(strs, pa.utf8())],
                    names,
                    codes,
                )

            b1 = mk(
                [codes[0], codes[1], codes[0], codes[1]],
                [1, 0, None, 0],
                [None, "a", "x", "b"],
            )
            b2 = mk([codes[1]], [0], ["z"])
        s = schema1("x", b1.type, nullable_field)
        add(Case(name, s, [batch1(s, b1), batch1(s, b2)]))

    union("dense_union", True, [0, 1], False)
    union("dense_union_nullable", True, [0, 1], True)
    union("sparse_union", False, [0, 1], False)
    union("sparse_union_nullable", False, [0, 1], True)
    union("dense_union_type_codes", True, [3, 7], True)
    union("sparse_union_type_codes", False, [5, 2], True)

    # Dictionaries.
    def dict_arr(indices, values, index_type=pa.int32()):
        return pa.DictionaryArray.from_arrays(pa.array(indices, index_type), values)

    abc = pa.array(["a", "b", "c"], pa.utf8())
    dt = pa.dictionary(pa.int32(), pa.utf8())
    s = schema1("x", dt, False)
    add(Case("dict_utf8", s, [batch1(s, dict_arr([0, 1, 0, 2, 1], abc)), batch1(s, dict_arr([2, 2, 0], abc))]))
    sn = schema1("x", dt, True)
    add(
        Case(
            "dict_utf8_nullable",
            sn,
            [batch1(sn, dict_arr([0, None, 1, 2], abc)), batch1(sn, dict_arr([1, 0], abc))],
        )
    )
    xy = pa.array(["x", "y"], pa.utf8())
    s8 = schema1("x", pa.dictionary(pa.int8(), pa.utf8()), False)
    add(
        Case(
            "dict_int8_index",
            s8,
            [batch1(s8, dict_arr([1, 0, 1], xy, pa.int8())), batch1(s8, dict_arr([0], xy, pa.int8()))],
        )
    )
    ints = pa.array([100, 200], pa.int64())
    si = schema1("x", pa.dictionary(pa.int32(), pa.int64()), False)
    add(Case("dict_int64_values", si, [batch1(si, dict_arr([1, 0, 1], ints)), batch1(si, dict_arr([0], ints))]))
    sr = schema1("x", dt, False)
    add(
        Case(
            "dict_replacement",
            sr,
            [
                batch1(sr, dict_arr([0, 1, 1], pa.array(["a", "b"]))),
                batch1(sr, dict_arr([2, 0, 1], pa.array(["x", "y", "z"]))),
            ],
            stream_only=True,
        )
    )
    add(
        Case(
            "dict_delta",
            sr,
            [
                batch1(sr, dict_arr([0, 1], pa.array(["a", "b"]))),
                batch1(sr, dict_arr([2, 0], pa.array(["a", "b", "c"]))),
            ],
            stream_only=True,
            delta=True,
        )
    )
    nested_t = pa.struct([pa.field("d", dt)])
    sd = schema1("x", nested_t, False)
    add(
        Case(
            "dict_in_struct",
            sd,
            [
                batch1(sd, pa.StructArray.from_arrays([dict_arr([2, 0, 1], abc)], fields=[pa.field("d", dt)])),
                batch1(sd, pa.StructArray.from_arrays([dict_arr([1], abc)], fields=[pa.field("d", dt)])),
            ],
        )
    )

    # Run-end encoded.
    def ree(name, re_type, val_type, b1, b2, nullable):
        typ = pa.run_end_encoded(re_type, val_type)

        def mk(spec):
            ends, vals = spec
            return pa.RunEndEncodedArray.from_arrays(
                pa.array(ends, re_type), pa.array(vals, val_type), type=typ
            )

        s = schema1("x", typ, nullable)
        add(Case(name, s, [batch1(s, mk(b1)), batch1(s, mk(b2))]))

    ree("ree_int32_int64", pa.int32(), pa.int64(), ([3, 5, 8], [100, 200, 300]), ([2], [7]), False)
    ree("ree_int32_int64_nullable", pa.int32(), pa.int64(), ([3, 5, 8], [100, None, 300]), ([2], [7]), True)
    ree("ree_int16_utf8", pa.int16(), pa.utf8(), ([1, 4], ["a", "bb"]), ([3], ["c"]), True)
    ree("ree_int64_float64", pa.int64(), pa.float64(), ([2, 3], [1.5, None]), ([1], [2.5]), True)

    # Custom schema- and field-level metadata.
    ms = pa.schema(
        [
            pa.field("a", pa.int32(), nullable=False, metadata={"unit": "ms", "note": "field level"}),
            pa.field("b", pa.utf8(), metadata={"k": "v"}),
        ],
        metadata={"origin": "wireform-arrow interop", "k2": "v2"},
    )
    add(
        Case(
            "custom_metadata",
            ms,
            [
                pa.record_batch([pa.array([1, 2], pa.int32()), pa.array(["x", None])], schema=ms),
                pa.record_batch([pa.array([3], pa.int32()), pa.array(["y"])], schema=ms),
            ],
        )
    )

    # Multi-column batches, including zero-row batches.
    mixed_s = pa.schema(
        [
            pa.field("i", pa.int64(), nullable=False),
            pa.field("s", pa.utf8()),
            pa.field("b", pa.bool_()),
            pa.field("l", pa.list_(pa.int32())),
            pa.field("st", pa.struct([pa.field("f", pa.float64())])),
            pa.field("d", dt),
            pa.field("v", pa.string_view()),
        ]
    )

    def mixed(i, s_, b, l, f, d, v):
        return pa.record_batch(
            [
                pa.array(i, pa.int64()),
                pa.array(s_, pa.utf8()),
                pa.array(b, pa.bool_()),
                pa.array(l, pa.list_(pa.int32())),
                pa.array([None if x is None else {"f": x} for x in f], mixed_s.field("st").type),
                dict_arr(d, abc),
                pa.array(v, pa.string_view()),
            ],
            schema=mixed_s,
        )

    m1 = mixed(
        [10, 20, 30],
        ["hello", None, "!"],
        [True, None, False],
        [[1], None, [2, 3]],
        [1.5, None, 2.5],
        [0, 2, None],
        ["v", None, "a view longer than twelve"],
    )
    m0 = mixed([], [], [], [], [], [], [])
    m2 = mixed([40], ["w"], [True], [[]], [0.5], [1], ["z"])
    add(Case("mixed", mixed_s, [m1, m2]))
    add(Case("zero_row_batches", mixed_s, [m1, m0, m2]))
    add(Case("only_zero_row_batch", mixed_s, [m0]))
    add(Case("no_batches", mixed_s, []))


# ---------------------------------------------------------------------------
# check: pyarrow reads what wireform-arrow wrote
# ---------------------------------------------------------------------------


def one_line(msg) -> str:
    return " ".join(str(msg).split())


def column_diff(got: pa.RecordBatch, want: pa.RecordBatch) -> str:
    parts = []
    for i in range(want.num_columns):
        name = want.schema.field(i).name
        try:
            g, w = got.column(i), want.column(i)
        except Exception:  # interval types have no Python array class
            parts.append(f"column {name} differs (no Python view of this type)")
            continue
        if not g.equals(w):
            try:
                parts.append(f"column {name}: got {g.to_pylist()!r} want {w.to_pylist()!r}")
            except Exception as e:  # noqa: BLE001
                parts.append(f"column {name}: got {g!r} want {w!r} ({e})")
    return "; ".join(parts) or "batches differ"


def compare(case: Case, schema: pa.Schema, batches: list[pa.RecordBatch]) -> str | None:
    if not schema.equals(case.schema, check_metadata=True):
        return f"schema mismatch: got {schema!r} (metadata {schema.metadata}) want {case.schema!r} (metadata {case.schema.metadata})"
    if len(batches) != len(case.batches):
        return f"got {len(batches)} batches, want {len(case.batches)}"
    for n, (got, want) in enumerate(zip(batches, case.batches)):
        try:
            got.validate(full=True)
        except Exception as e:  # noqa: BLE001
            return f"batch {n}: validate(full=True) failed: {e}"
        if got.num_rows != want.num_rows:
            return f"batch {n}: got {got.num_rows} rows, want {want.num_rows}"
        if not got.equals(want):
            return f"batch {n}: {column_diff(got, want)}"
    return None


def check(directory: Path) -> int:
    for path in sorted(directory.iterdir()):
        parts = path.name.split(".")
        if len(parts) != 3 or parts[2] not in ("arrows", "arrow"):
            continue
        case = CASES.get(parts[0])
        if case is None:
            print(f"FAIL {path.name}: no pyarrow-side case named {parts[0]}")
            continue
        try:
            if parts[2] == "arrows":
                reader = ipc.open_stream(pa.OSFile(str(path), "rb"))
                schema = reader.schema
                batches = list(reader)
            else:
                reader = ipc.open_file(pa.OSFile(str(path), "rb"))
                schema = reader.schema
                batches = [reader.get_batch(i) for i in range(reader.num_record_batches)]
            err = compare(case, schema, batches)
        except Exception as e:  # noqa: BLE001
            err = f"pyarrow failed to read: {type(e).__name__}: {e}"
        if err is None:
            print(f"PASS {path.name}")
        else:
            print(f"FAIL {path.name}: {one_line(err)}")
    return 0


# ---------------------------------------------------------------------------
# write: wireform-arrow reads what pyarrow wrote
# ---------------------------------------------------------------------------


def sliced(b: pa.RecordBatch) -> pa.RecordBatch:
    """The same rows as ``b``, but as a slice at a non-zero offset of a
    larger batch, so pyarrow has to truncate / rebase buffers when it
    writes them."""
    big = pa.concat_batches([b, b, b])
    return big.slice(b.num_rows, b.num_rows)


VARIANTS = {
    "plain": None,
    "sliced": None,
    "zstd": "zstd",
    "lz4": "lz4",
}


def write(directory: Path) -> int:
    directory.mkdir(parents=True, exist_ok=True)
    for case in CASES.values():
        for variant, codec in VARIANTS.items():
            try:
                batches = case.batches
                if variant == "sliced":
                    batches = [sliced(b) for b in batches]
                opts = ipc.IpcWriteOptions(
                    compression=codec, emit_dictionary_deltas=case.delta
                )
            except Exception as e:  # noqa: BLE001
                print(f"SKIP {case.name}.{variant}: {one_line(e)}")
                continue
            kinds = [("arrows", ipc.new_stream)]
            if not case.stream_only:
                kinds.append(("arrow", ipc.new_file))
            for ext, new in kinds:
                path = directory / f"{case.name}.{variant}.{ext}"
                try:
                    with pa.OSFile(str(path), "wb") as sink:
                        with new(sink, case.schema, options=opts) as w:
                            for b in batches:
                                w.write_batch(b)
                except Exception as e:  # noqa: BLE001
                    print(f"FAIL {path.name}: pyarrow failed to write: {one_line(e)}")
                    continue
                print(f"WROTE {path.name}")
    return 0


# ---------------------------------------------------------------------------
# regen-goldens: fixtures for the pyarrow-free wireform-arrow-test suite
# ---------------------------------------------------------------------------


def regen_goldens() -> int:
    GOLDEN_DIR.mkdir(parents=True, exist_ok=True)

    sch = pa.schema([pa.field("a", pa.int32(), nullable=False)])
    batch = pa.record_batch([pa.array([1, 2, 3, 4, 5], pa.int32())], schema=sch)
    with pa.OSFile(str(GOLDEN_DIR / "pa_int32.arrows"), "wb") as f:
        with ipc.new_stream(f, sch) as w:
            w.write_batch(batch)

    sch2 = pa.schema(
        [
            pa.field("i", pa.int64(), nullable=False),
            pa.field("s", pa.string(), nullable=True),
            pa.field("b", pa.bool_(), nullable=True),
        ]
    )
    batch2 = pa.record_batch(
        [
            pa.array([10, 20, 30], pa.int64()),
            pa.array(["alpha", None, "gamma"], pa.string()),
            pa.array([True, False, None], pa.bool_()),
        ],
        schema=sch2,
    )
    with pa.OSFile(str(GOLDEN_DIR / "pa_mixed.arrows"), "wb") as f:
        with ipc.new_stream(f, sch2) as w:
            w.write_batch(batch2)

    dict_arr = pa.DictionaryArray.from_arrays(
        pa.array([0, 1, 0, 2, 1], pa.int32()), pa.array(["a", "b", "c"], pa.string())
    )
    sch3 = pa.schema([pa.field("d", dict_arr.type)])
    batch3 = pa.record_batch([dict_arr], schema=sch3)
    with pa.OSFile(str(GOLDEN_DIR / "pa_dict.arrows"), "wb") as f:
        with ipc.new_stream(f, sch3) as w:
            w.write_batch(batch3)

    print(f"Regenerated goldens under {GOLDEN_DIR.relative_to(ROOT)}/")
    return 0


def main(argv: list[str]) -> int:
    cmd = argv[1] if len(argv) > 1 else ""
    if cmd == "regen-goldens":
        return regen_goldens()
    build_cases()
    if cmd == "cases":
        for name in CASES:
            print(name)
        return 0
    if cmd == "check" and len(argv) == 3:
        return check(Path(argv[2]))
    if cmd == "write" and len(argv) == 3:
        return write(Path(argv[2]))
    print(__doc__, file=sys.stderr)
    return 2


if __name__ == "__main__":
    rc = main(sys.argv)
    sys.stdout.flush()
    sys.stderr.flush()
    # Skip interpreter teardown: see _KEEP.
    os._exit(rc)
