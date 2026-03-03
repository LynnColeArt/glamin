# Space Contracts and Manifests

This document defines the **contract layer** that keeps document and geometry
spaces compatible, stable, and auditable.

---

## Purpose

Space contracts act as **versioned schemas** for vector spaces. They govern:

- compatibility checks
- validation of incoming vectors
- interchange and transformation rules
- deployment and rollback safety

---

## Contract Fields (Core)

Required fields:

- `space_id` — stable identifier for the space
- `dim` — vector dimensionality
- `metric` — distance metric (`l2`, `ip`, etc.)
- `normalization` — `none`, `l2`, `mean`, etc.
- `embedder` — embedder contract block (id, version, hashes, signature)
- `transform_chain` — ordered transform list
- `created_at` — timestamp
- `owner` — owning subsystem or domain

Optional fields:

- `metric_arg` — when metric requires a parameter
- `notes` — human context for the space
- `risk_profile` — low / medium / high

---

## Invariants

Contracts include **hard invariants** that are non‑negotiable:

- minimum/maximum dimension
- forbidden transforms
- disallowed metrics
- bounded ranges for confidence or distance values

---

## Compatibility Rules

Two spaces are compatible only if:

- `space_id` matches
- `dim` matches
- `metric` matches
- `normalization` matches
- `embedder.id` and `embedder.version` match
- `embedder.model_hash` and `embedder.config_hash` match
- `embedder.preprocess_chain` matches

If any core field changes without a migration plan, it is a **breaking change**.

---

## Embedder Contracts

Every write must include an **embedder contract**. Vectors without a matching
contract are rejected. This prevents silent contamination across spaces.

Embedder contract fields:

- `id` — embedder identifier
- `version` — embedder version
- `input_schema` — expected input shape or format
- `preprocess_chain` — ordered preprocessing steps
- `model_hash` — hash of embedder weights or artifact
- `config_hash` — hash of embedder configuration
- `hardware_class` — `cpu`, `gpu`, `npu`, or `hybrid`
- `min_ram_mb` — minimum system memory requirement
- `min_vram_mb` — minimum accelerator memory requirement
- `signature` — signed attestation of the contract

---

## Transform Contracts

Every transform in `transform_chain` must be:

- versioned (e.g. `doc_to_geom:v2`)
- deterministic
- registered in a transform registry

Transform contracts define:

- input space
- output space
- reversible flag
- required calibration data

---

## Manifest Layout (Example)

```
space_id: geometry.app_state
dim: 1024
metric: l2
normalization: l2
embedder:
  id: geomnet
  version: 0.4.2
  input_schema: app_state_v3
  preprocess_chain: [normalize:l2]
  model_hash: sha256:5a6b...
  config_hash: sha256:9f2c...
  hardware_class: cpu
  min_ram_mb: 128
  min_vram_mb: 0
  signature: ed25519:ab12...
transform_chain:
  - doc_to_geometry:v2
created_at: 2024-11-03T10:14:00Z
owner: core_runtime
invariants:
  - type: metric
    allowed: [l2]
  - type: dim
    equals: 1024
```

---

## Enforcement Points

- **Index creation**: validate against contract.
- **Write path**: reject vectors without a matching embedder contract.
- **Load path**: check manifest vs on‑disk metadata.
- **Deployment**: block if contract diff is breaking.

---

## Governance

Contracts are versioned and signed. Changes flow through:

1. Proposal
2. Diff review
3. Compatibility decision
4. Migration plan (if breaking)
