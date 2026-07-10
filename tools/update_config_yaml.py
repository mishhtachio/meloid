#!/usr/bin/env python3

import json
import os
import sys
import tempfile
from collections.abc import MutableMapping

try:
    from ruamel.yaml import YAML
except ModuleNotFoundError as exc:
    print(
        "ruamel.yaml is required to update config.yaml. "
        "Install it with: python3 -m pip install 'ruamel.yaml>=0.19,<0.20'",
        file=sys.stderr,
    )
    raise SystemExit(1) from exc


def fail(message: str) -> "NoReturn":
    print(message, file=sys.stderr)
    raise SystemExit(1)


def load_payload() -> dict[str, object]:
    try:
        payload = json.load(sys.stdin)
    except json.JSONDecodeError as exc:
        fail(f"Invalid config payload: {exc}")

    if not isinstance(payload, dict):
        fail("Config payload must be a JSON object.")

    for key in payload:
        if not isinstance(key, str):
            fail("Config payload keys must be strings.")

    return payload


def load_document(path: str):
    yaml = YAML(typ="rt")
    yaml.preserve_quotes = True
    try:
        with open(path, "r", encoding="utf-8") as handle:
            document = yaml.load(handle)
    except FileNotFoundError as exc:
        fail(f"Config file not found: {path}")
    except Exception as exc:
        fail(f"Failed to parse config.yaml: {exc}")

    if not isinstance(document, MutableMapping):
        fail("config.yaml must be a single-document top-level mapping.")

    return yaml, document


def dump_document(path: str, yaml: YAML, document) -> None:
    directory = os.path.dirname(path) or "."
    basename = os.path.basename(path)
    original_mode = None
    try:
        original_mode = os.stat(path).st_mode
    except OSError:
        original_mode = None

    fd, temp_path = tempfile.mkstemp(prefix=f".{basename}.", suffix=".tmp", dir=directory)
    try:
        os.close(fd)
        if original_mode is not None:
            os.chmod(temp_path, original_mode)
        with open(temp_path, "w", encoding="utf-8", newline="") as handle:
            yaml.dump(document, handle)
        os.replace(temp_path, path)
    except Exception as exc:
        try:
            os.unlink(temp_path)
        except OSError:
            pass
        fail(f"Failed to write config.yaml: {exc}")


def main() -> int:
    if len(sys.argv) != 2:
        fail("Usage: update_config_yaml.py <config-path>")

    path = sys.argv[1]
    payload = load_payload()
    yaml, document = load_document(path)

    for key, value in payload.items():
        document[key] = value

    dump_document(path, yaml, document)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
