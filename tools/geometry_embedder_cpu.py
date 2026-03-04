#!/usr/bin/env python3
import argparse
import hashlib
import json
import re
import sys
from array import array
from pathlib import Path
from typing import Dict, List

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

TOKEN_RE = re.compile(r"[A-Za-z0-9_]+")


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


def normalize_text(text: str) -> str:
    return text.lower()


def tokenize(text: str) -> List[str]:
    return TOKEN_RE.findall(text)


def token_hash(token: str, dim: int) -> int:
    digest = hashlib.sha256(token.encode("utf-8")).digest()
    value = int.from_bytes(digest[:8], byteorder="little", signed=False)
    return value % dim


def build_space_map(spec: dict) -> Dict[str, dict]:
    return {space["space_id"]: space for space in spec["spaces"]}


def build_ordered_mints(spec: dict) -> List[str]:
    spaces = spec["spaces"]
    mints = spec.get("mints", [])
    mints_by_space: Dict[str, List[str]] = {space["space_id"]: [] for space in spaces}

    for mint in mints:
        space_id = mint["space_id"]
        if space_id not in mints_by_space:
            raise ValueError(f"Mint references unknown space_id: {space_id}")
        mints_by_space[space_id].append(mint["mint_id"])

    ordered = []
    for space in spaces:
        ordered.extend(mints_by_space.get(space["space_id"], []))
    return ordered


def embed_text(text: str, dim: int, normalize: str) -> List[float]:
    vector = [0.0] * dim
    tokens = tokenize(normalize_text(text))
    for token in tokens:
        idx = token_hash(token, dim)
        vector[idx] += 1.0

    if normalize == "l2":
        norm = sum(value * value for value in vector) ** 0.5
        if norm > 0.0:
            vector = [value / norm for value in vector]
    return vector


def write_vectors(path: Path, vectors: List[List[float]]) -> None:
    with path.open("wb") as handle:
        for vector in vectors:
            data = array("f", vector)
            if sys.byteorder != "little":
                data.byteswap()
            data.tofile(handle)


def cmd_embed(args: argparse.Namespace) -> int:
    spec = load_yaml(Path(args.spec))
    schema = load_schema(Path(args.schema))
    validate_spec(spec, schema)

    space_map = build_space_map(spec)
    mint_index = {mint["mint_id"]: mint for mint in spec.get("mints", [])}

    ordered_mints = build_ordered_mints(spec)
    vectors: List[List[float]] = []

    for mint_id in ordered_mints:
        mint = mint_index[mint_id]
        space = space_map[mint["space_id"]]
        dim = int(space["dim"])
        normalization = space.get("normalization", "none")
        vector = embed_text(mint["text"], dim, normalization)
        vectors.append(vector)

    out_path = Path(args.output)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    write_vectors(out_path, vectors)
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="CPU-only embedder for geometry specs.")
    parser.add_argument(
        "--schema",
        default=str(default_schema_path()),
        help="Path to geometry_spec.schema.json",
    )
    parser.add_argument("spec", help="Path to geometry_spec.yaml")
    parser.add_argument(
        "--output", default="build/specs/vectors.bin", help="Output vectors.bin path"
    )
    parser.set_defaults(func=cmd_embed)
    return parser


def main() -> int:
    parser = build_parser()
    args = parser.parse_args()
    return args.func(args)


if __name__ == "__main__":
    raise SystemExit(main())
