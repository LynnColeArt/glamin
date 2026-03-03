# Document + Geometry Separation Pattern

This pattern keeps **documents** and **application geometry** cleanly separated
while still enabling deterministic transformation, interchange, and cross-space
workflows.

---

## Core Principle

Documents and geometry live in **distinct vector spaces** with explicit,
versioned contracts. No implicit cross-space search or mixing. All interchange
is opt-in and recorded.

---

## Separation (Hard Boundaries)

- Maintain **separate index families** for each space.
  - `DocumentIndex`: content embeddings, retrieval semantics.
  - `GeometryIndex`: application state, behaviors, trajectories.
- Each vector block belongs to a **space contract**:
  - `space_id`, `dim`, `metric`, `normalization`, `embedder` (id/version/hash),
    `transform_chain`, `created_at`.
- Handles and manifests enforce compatibility (reject mismatched dims/metrics).

---

## Interchange (Explicit Bridges)

Interchange is done through **transform pipelines**, never implicit casts.

```
document → embed → transform → geometry_index
geometry → project → transform → document_index
```

Each bridge is:
- **Named** and **versioned** (e.g., `doc_to_geometry:v2`)
- **Deterministic** (same inputs, same outputs)
- **Auditable** (recorded in metadata and manifests)

---

## Storage Pattern

- FAISS-compatible index files store raw vectors only.
- A **sidecar manifest** stores semantic metadata and contracts.
- Manifests enable round‑trip integrity and cross-space validation.

Example manifest fields:

```
space_id: geometry.app_state
dim: 1024
metric: l2
normalization: l2
embedder:
  id: geomnet
  version: 0.4.2
  preprocess_chain: [normalize:l2]
  model_hash: sha256:5a6b...
transform_chain:
  - doc_to_geometry:v2
created_at: 2024-11-03T10:14:00Z
```

---

## Recommended Workflow

1. **Embed** documents into a document space.
2. **Transform** into geometry only via registered bridges.
3. **Index** geometry separately with its own lifecycle.
4. **Cross-space search** is performed by explicit query transforms.

---

## What This Prevents

- Silent contamination between spaces.
- Accidental mixing of incompatible embeddings.
- Lost provenance when moving vectors between indexes.

---

## What This Enables

- Controlled interchange and traceability.
- Independent optimization of document vs geometry indexes.
- Stable FAISS compatibility with richer semantic context.

---

## Related Docs

- `docs/space_contracts.md`
- `docs/geometry_diff.md`
