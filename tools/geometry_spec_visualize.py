#!/usr/bin/env python3
import argparse
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


def quote(value: str) -> str:
    return json.dumps(value)


def build_dot(spec: dict) -> str:
    lines = ["digraph geometry {", "  rankdir=LR;"]

    mints = spec.get("mints", [])
    for mint in mints:
        mint_id = mint["mint_id"]
        lines.append(f"  {quote(mint_id)} [shape=box];")

    corridors = spec.get("corridors", [])
    for corridor in corridors:
        corridor_id = corridor["corridor_id"]
        between = corridor["between"]
        width = corridor["width"]
        label = f"corridor:{corridor_id} (w={width})"
        lines.append(
            f"  {quote(between[0])} -> {quote(between[1])} "
            f"[label={quote(label)}, dir=none, color=\"gray\"];"
        )

    traces = spec.get("traces", [])
    for trace in traces:
        trace_id = trace["trace_id"]
        steps = trace["steps"]
        for start, end in zip(steps, steps[1:]):
            label = f"trace:{trace_id}"
            lines.append(
                f"  {quote(start)} -> {quote(end)} "
                f"[label={quote(label)}, color=\"blue\"];"
            )

    lines.append("}")
    return "\n".join(lines) + "\n"


def cmd_render(args: argparse.Namespace) -> int:
    spec = load_yaml(Path(args.spec))
    schema = load_schema(Path(args.schema))
    validate_spec(spec, schema)

    dot_output = build_dot(spec)
    if args.output:
        Path(args.output).write_text(dot_output, encoding="utf-8")
    else:
        sys.stdout.write(dot_output)
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Render geometry spec visualizations.")
    parser.add_argument(
        "--schema",
        default=str(default_schema_path()),
        help="Path to geometry_spec.schema.json",
    )
    parser.add_argument("spec", help="Path to geometry_spec.yaml")
    parser.add_argument("--output", help="Output DOT file (default: stdout)")
    parser.set_defaults(func=cmd_render)
    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
