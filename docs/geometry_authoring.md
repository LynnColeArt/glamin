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
  - `vectors.bin`: raw vector payloads
  - `contracts.json`: embedder + space contracts with signatures

---

## Compile + Contract Flow

1. **Author** `geometry_spec.yaml` (human or LLM).
2. **Validate** against `geometry_spec.schema.json`.
3. **Canonicalize** → deterministic JSON (sorted keys, stable float formatting).
4. **Embed**: the embedder service produces vectors for each mint/asset.
5. **Contract**: embedder contract is hashed + signed.
6. **Emit**: `manifest.json`, `vectors.bin`, `contracts.json`.
7. **Load**: Glamin rejects vectors that lack a matching embedder contract.

The core only consumes the compiled artifacts. The authoring spec remains the
editable source of truth.

---

## Spec Principles

- **Human readable**: YAML with descriptive text.
- **Deterministic**: canonicalization + hashes remove ambiguity.
- **Auditable**: embedder contract attached to every write.
- **Decoupled**: Glamin stays pure Fortran; embedder runs out‑of‑process.

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
- Compiler output: `manifest.json` and `contracts.json`
- First executable example spec using mints, corridors, and traces

---

## Example

See `docs/geometry_spec.yaml` for a complete sample.
