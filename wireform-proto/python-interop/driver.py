#!/usr/bin/env python3
"""Python driver for wireform-proto ↔ google-protobuf interop tests.

Protocol: newline-delimited JSON over stdin/stdout.

Haskell → Python:
  {"type": "init",  "schema": "<base64 FileDescriptorProto>"}
  {"type": "h2p",   "msg": "full.TypeName", "data": "<base64 proto bytes>"}
  {"type": "p2h",   "msg": "full.TypeName", "fields": {...}}
  {"type": "done"}

Python → Haskell:
  {"status": "ok",    "data": "<base64>"}           -- for h2p / p2h
  {"status": "ready"}                               -- for init
  {"status": "error", "message": "..."}

Field conventions in 'fields' dicts:
  - Scalar types pass through JSON natively.
  - bytes fields: {"__bytes__": "<base64>"}
  - Enum fields: integer wire value.
  - Repeated fields: JSON list.
  - Map fields: JSON object (string keys only for now).
  - Nested messages: nested JSON objects.
  - Optional absent fields: omit the key entirely.
"""
from __future__ import annotations

import base64
import json
import struct
import sys
import traceback
from typing import Any

try:
    from google.protobuf import descriptor_pb2, descriptor_pool, message_factory, json_format
    HAS_PROTOBUF = True
except ImportError:
    HAS_PROTOBUF = False


def b64(data: bytes) -> str:
    return base64.b64encode(data).decode("ascii")


def from_b64(s: str) -> bytes:
    return base64.b64decode(s)


def send(obj: dict) -> None:
    sys.stdout.write(json.dumps(obj) + "\n")
    sys.stdout.flush()


def recv() -> dict | None:
    line = sys.stdin.readline()
    if not line:
        return None
    return json.loads(line.strip())


def fields_to_msg(msg, fields: dict) -> None:
    """Populate a protobuf message from a JSON fields dict.

    Uses json_format.ParseDict which handles all field types (scalars,
    nested messages, repeated, map, bytes-as-base64) automatically.
    The Haskell side sends bytes as {"__bytes__": "<base64>"} in the
    custom protocol, but json_format expects raw base64 strings.
    Rewrite __bytes__ wrappers before calling ParseDict.
    """
    def unwrap(obj):
        if isinstance(obj, dict):
            if "__bytes__" in obj and len(obj) == 1:
                return obj["__bytes__"]  # already base64 string
            return {k: unwrap(v) for k, v in obj.items()}
        if isinstance(obj, list):
            return [unwrap(x) for x in obj]
        return obj

    json_format.ParseDict(unwrap(fields), msg, ignore_unknown_fields=False)


def main():
    if not HAS_PROTOBUF:
        send({"status": "error", "message": "google-protobuf not installed"})
        return

    pool = descriptor_pool.DescriptorPool()
    classes: dict[str, Any] = {}

    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            req = json.loads(line)
        except json.JSONDecodeError as e:
            send({"status": "error", "message": f"JSON decode error: {e}"})
            continue

        try:
            t = req["type"]

            if t == "init":
                fdp_bytes = from_b64(req["schema"])
                fdp = descriptor_pb2.FileDescriptorProto()
                fdp.ParseFromString(fdp_bytes)
                # AddSerializedFile is available since protobuf 3.7;
                # fall back to Add for older installs.
                try:
                    pool.AddSerializedFile(fdp_bytes)
                except AttributeError:
                    pool.Add(fdp)
                for msg_desc in fdp.message_type:
                    full_name = (fdp.package + "." + msg_desc.name) if fdp.package else msg_desc.name
                    desc = pool.FindMessageTypeByName(full_name)
                    # protobuf >= 4.x: GetMessageClass replaces MessageFactory.GetPrototype
                    if hasattr(message_factory, "GetMessageClass"):
                        classes[full_name] = message_factory.GetMessageClass(desc)
                    else:
                        fac = message_factory.MessageFactory(pool=pool)
                        classes[full_name] = fac.GetPrototype(desc)
                send({"status": "ready", "types": list(classes.keys())})

            elif t == "h2p":
                # Haskell encoded → Python decodes → re-encodes → return bytes
                msg_type = req["msg"]
                proto_bytes = from_b64(req["data"])
                Cls = classes[msg_type]
                msg = Cls()
                msg.ParseFromString(proto_bytes)
                send({"status": "ok", "data": b64(msg.SerializeToString())})

            elif t == "p2h":
                # Python encodes from field spec → return bytes for Haskell to decode
                msg_type = req["msg"]
                field_spec = req["fields"]
                Cls = classes[msg_type]
                msg = Cls()
                fields_to_msg(msg, field_spec)
                send({"status": "ok", "data": b64(msg.SerializeToString())})

            elif t == "done":
                send({"status": "done"})
                break

            else:
                send({"status": "error", "message": f"unknown type: {t}"})

        except Exception as e:
            send({"status": "error", "message": traceback.format_exc()})


if __name__ == "__main__":
    main()
