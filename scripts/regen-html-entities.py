#!/usr/bin/env python3
"""Regenerate ``wireform-html/src/HTML/Entities.hs`` from the WHATWG entity table.

Usage::

    python3 scripts/regen-html-entities.py [path/to/entities.json]

If no path is supplied, the script downloads the canonical
``https://html.spec.whatwg.org/entities.json``.
"""
from __future__ import annotations

import json
import sys
import urllib.request
from collections import defaultdict
from pathlib import Path

ENTITIES_URL = "https://html.spec.whatwg.org/entities.json"
OUT = Path(__file__).resolve().parent.parent / "wireform-html" / "src" / "HTML" / "Entities.hs"


def load(path: str | None) -> dict:
    if path is None:
        with urllib.request.urlopen(ENTITIES_URL) as fp:
            return json.load(fp)
    with open(path) as fp:
        return json.load(fp)


def hs_string(s: str) -> str:
    out: list[str] = []
    for c in s:
        cp = ord(c)
        if cp == ord('"'):
            out.append('\\"')
        elif cp == ord('\\'):
            out.append('\\\\')
        elif 32 <= cp < 127:
            out.append(c)
        else:
            out.append(f'\\x{cp:X}')
    return '"' + "".join(out) + '"'


def main(argv: list[str]) -> int:
    src = argv[1] if len(argv) > 1 else None
    data = load(src)

    forms: dict[str, dict[str, str]] = defaultdict(dict)
    for key, val in data.items():
        name = key[1:]  # strip leading '&'
        chars = val["characters"]
        if name.endswith(";"):
            forms[name[:-1]]["semi"] = chars
        else:
            forms[name]["nosemi"] = chars

    named = sorted((bare, v.get("semi", v.get("nosemi"))) for bare, v in forms.items())
    legacy = sorted(bare for bare, v in forms.items() if "nosemi" in v)

    lines: list[str] = []
    lines.append("-- | The full HTML5 named-character-reference table.")
    lines.append("--")
    lines.append("-- This is the named character reference table from the WHATWG HTML")
    lines.append("-- standard (<https://html.spec.whatwg.org/entities.json>). Both the")
    lines.append("-- with-semicolon and (legacy) without-semicolon forms are folded into a")
    lines.append("-- single map keyed by the bare name (without the leading @&@). The")
    lines.append("-- @legacyEntities@ list enumerates names that may appear without a")
    lines.append("-- terminating semicolon in legacy contexts (per the HTML standard's")
    lines.append("-- error-recovery rules).")
    lines.append("--")
    lines.append("-- The table is generated from @entities.json@ as published by WHATWG.")
    lines.append("-- Do not hand-edit; regenerate by running")
    lines.append("-- @scripts/regen-html-entities.py@.")
    lines.append("module HTML.Entities")
    lines.append("  ( namedEntities")
    lines.append("  , legacyEntities")
    lines.append("  ) where")
    lines.append("")
    lines.append("namedEntities :: [(String, String)]")
    lines.append("namedEntities =")
    first = True
    for name, chars in named:
        prefix = "  [ " if first else "  , "
        first = False
        lines.append(f"{prefix}({hs_string(name)}, {hs_string(chars)})")
    lines.append("  ]")
    lines.append("")
    lines.append("legacyEntities :: [String]")
    lines.append("legacyEntities =")
    first = True
    for name in legacy:
        prefix = "  [ " if first else "  , "
        first = False
        lines.append(f"{prefix}{hs_string(name)}")
    lines.append("  ]")
    lines.append("")

    OUT.write_text("\n".join(lines))
    print(f"wrote {OUT} ({len(named)} named, {len(legacy)} legacy)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
