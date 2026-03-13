# Geometry Authoring Spec

This document defines the **human‑readable authoring spec** used by LLMs and
humans to describe executable geometry. The spec compiles into a canonical
manifest and vector payloads that Glamin validates via contracts.

---

## Files

- `docs/geometry_spec.yaml`: authoring format (YAML, human/LLM friendly)
- `docs/geometry_spec.schema.json`: JSON schema for validation
- Output artifacts from the compiler:
  - `manifest.json`: canonical, sorted contract manifest
  - `vector_layout.json`: vector ordering and offsets
  - `vectors.bin`: raw vector payloads
  - `contracts.json`: embedder + space contracts with signatures

---

## Compile + Contract Flow

1. **Author** `geometry_spec.yaml` (human or LLM).
2. **Validate** against `geometry_spec.schema.json`.
3. **Canonicalize** → deterministic JSON (sorted keys, stable float formatting).
4. **Embed**: the embedder service produces vectors for each mint/asset.
5. **Contract**: embedder contract is hashed + signed.
6. **Emit**: `manifest.json`, `vector_layout.json`, `vectors.bin`, `contracts.json`.
7. **Load**: Glamin rejects vectors that lack a matching embedder contract.

The core only consumes the compiled artifacts. The authoring spec remains the
editable source of truth.

---

## Spec Principles

- **Human readable**: YAML with descriptive text.
- **Deterministic**: canonicalization + hashes remove ambiguity.
- **Auditable**: embedder contract attached to every write.
- **Decoupled**: Glamin stays pure Fortran; embedder runs out‑of‑process.
- **Directed**: optional `entry_mints` + `objectives` describe start points
  and target behaviors for authoring clarity.

---

## Embedder Defaults

- **CPU-first**: the default embedder is CPU-only for easy deployment.
- **GPU/NPU optional**: accelerated embedders are separate contracts that opt
  into specific hardware classes when available.
- **Hardware requirements**: embedder contracts include `hardware_class`,
  `min_ram_mb`, and `min_vram_mb` for deterministic selection against a
  hardware profile.
- **Selection**: prefer an explicit hardware class when provided; otherwise
  fall back to the first supported contract in the list.

---

## Tooling Plan

Near-term tooling targets:

- Schema validator + canonicalizer for `geometry_spec.yaml`
- Compiler output: `manifest.json`, `contracts.json`, `vector_layout.json`,
  and `vectors.bin` (stub)
- First executable example spec using mints, corridors, and traces
- Visualizer stub that emits Graphviz DOT

---

## Tooling Usage

Install tooling dependencies:

```
python3 -m venv build/venv
build/venv/bin/pip install -r tools/requirements.txt
```

Validate a spec:

```
build/venv/bin/python tools/geometry_spec_tool.py validate docs/geometry_spec.yaml
```

Compile to manifest + contracts:

```
build/venv/bin/python tools/geometry_spec_tool.py compile docs/geometry_spec.yaml --out-dir build/specs
```

Embed vectors with the CPU baseline:

```
build/venv/bin/python tools/geometry_embedder_cpu.py docs/geometry_spec.yaml --output build/specs/vectors.bin
```

Load vectors in Fortran (dim/count from `vector_layout.json`):

```
call load_vector_block("build/specs/vectors.bin", dim, count, vectors, status)
```

Load a Flat index directly from the layout:

```
call load_flat_from_layout("build/specs/vector_layout.json", "build/specs/vectors.bin", &
  "geometry.auth", METRIC_L2, index, status)
```

Canonicalize to JSON:

```
build/venv/bin/python tools/geometry_spec_tool.py canonicalize docs/geometry_spec.yaml --output build/specs/spec.json
```

Render a DOT graph:

```
build/venv/bin/python tools/geometry_spec_visualize.py docs/geometry_spec.yaml --output build/specs/spec.dot
```

Run the loader demo (after building + generating specs):

```
gfortran -std=f2018 -Ibuild/mod -o build/geometry_loader_demo \
  examples/geometry_loader_demo.f90 build/libglamin.a
./build/geometry_loader_demo
```

Note: quote version strings and timestamps to avoid YAML coercion.

---

## Example

See `docs/geometry_spec.yaml` for a minimal sample, and
`examples/geometry_spec_auth_flow.yaml` for a richer example.
