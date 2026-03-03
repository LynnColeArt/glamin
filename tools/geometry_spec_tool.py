#!/usr/bin/env python3
import argparse
import hashlib
import json
import sys
from pathlib import Path

try:
    import yaml
except ImportError as exc:
    raise SystemExit("Missing dependency: pyyaml. Install via `pip install pyyaml`.") from exc

try:
    import jsonschema
except ImportError as exc:
    raise SystemExit(
        "Missing dependency: jsonschema. Install via `pip install jsonschema`."
    ) from exc


def default_schema_path() -> Path:
    return Path(__file__).resolve().parents[1] / "docs" / "geometry_spec.schema.json"


def load_yaml(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as handle:
        data = yaml.safe_load(handle)
    if data is None:
        return {}
    if not isinstance(data, dict):
        raise ValueError("Spec root must be a mapping/object.")
    return data


def load_schema(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def validate_spec(spec: dict, schema: dict) -> None:
    jsonschema.validate(instance=spec, schema=schema)


def canonical_json(data: dict) -> str:
    return json.dumps(data, sort_keys=True, separators=(",", ":"), ensure_ascii=False)


def canonical_hash(data: dict) -> str:
    digest = hashlib.sha256(canonical_json(data).encode("utf-8")).hexdigest()
    return f"sha256:{digest}"


def build_manifest(spec: dict) -> dict:
    manifest = {}
    for key in ("spec_version", "spec_id", "title", "created_at", "owner", "notes"):
        if key in spec:
            manifest[key] = spec[key]
    for key in ("embedder", "spaces", "mints", "corridors", "traces"):
        if key in spec:
            manifest[key] = spec[key]
    return manifest


def build_contracts(spec: dict) -> dict:
    embedder_spec = dict(spec["embedder"])
    signature = embedder_spec.pop("signature", "")
    embedder_contract = {
        "spec": embedder_spec,
        "contract_hash": canonical_hash(embedder_spec),
        "signature": signature,
    }

    space_contracts = []
    for space in spec["spaces"]:
        space_contract = dict(space)
        space_contracts.append(
            {
                "spec": space_contract,
                "contract_hash": canonical_hash(space_contract),
                "signature": "",
            }
        )

    return {
        "spec_id": spec["spec_id"],
        "embedder": embedder_contract,
        "spaces": space_contracts,
    }


def write_json(path: Path, payload: dict, pretty: bool = True) -> None:
    with path.open("w", encoding="utf-8") as handle:
        json.dump(
            payload,
            handle,
            indent=2 if pretty else None,
            sort_keys=True,
            ensure_ascii=False,
        )
        handle.write("\n")


def cmd_validate(args: argparse.Namespace) -> int:
    spec = load_yaml(Path(args.spec))
    schema = load_schema(Path(args.schema))
    validate_spec(spec, schema)
    return 0


def cmd_canonicalize(args: argparse.Namespace) -> int:
    spec = load_yaml(Path(args.spec))
    schema = load_schema(Path(args.schema))
    validate_spec(spec, schema)
    canonical = canonical_json(spec)
    if args.output:
        Path(args.output).write_text(canonical + "\n", encoding="utf-8")
    else:
        sys.stdout.write(canonical + "\n")
    return 0


def cmd_compile(args: argparse.Namespace) -> int:
    spec = load_yaml(Path(args.spec))
    schema = load_schema(Path(args.schema))
    validate_spec(spec, schema)

    out_dir = Path(args.out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)

    manifest = build_manifest(spec)
    contracts = build_contracts(spec)

    write_json(out_dir / "manifest.json", manifest, pretty=True)
    write_json(out_dir / "contracts.json", contracts, pretty=True)
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Validate and compile geometry specs.")
    parser.add_argument(
        "--schema",
        default=str(default_schema_path()),
        help="Path to geometry_spec.schema.json",
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    validate_parser = subparsers.add_parser("validate", help="Validate a spec file.")
    validate_parser.add_argument("spec", help="Path to geometry_spec.yaml")
    validate_parser.set_defaults(func=cmd_validate)

    canonical_parser = subparsers.add_parser(
        "canonicalize", help="Output canonical JSON for a spec."
    )
    canonical_parser.add_argument("spec", help="Path to geometry_spec.yaml")
    canonical_parser.add_argument("--output", help="Write canonical JSON to file")
    canonical_parser.set_defaults(func=cmd_canonicalize)

    compile_parser = subparsers.add_parser(
        "compile", help="Emit manifest.json and contracts.json"
    )
    compile_parser.add_argument("spec", help="Path to geometry_spec.yaml")
    compile_parser.add_argument(
        "--out-dir", default="build/specs", help="Output directory"
    )
    compile_parser.set_defaults(func=cmd_compile)

    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
