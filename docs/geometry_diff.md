# Geometry Diffing Pattern

This document defines how to diff model‑like applications that are expressed as
vector fields rather than imperative code.

---

## Overview

Diffing is multi‑layered. We compare **contracts**, **geometry**, **boundaries**,
and **execution paths** to produce a stable, interpretable change report.

---

## 1) Contract Diff (Deterministic)

Compare immutable metadata from the manifest:

- `space_id`
- `dim`
- `metric`
- `normalization`
- `embedder.id`
- `embedder.version`
- `embedder.model_hash`
- `embedder.config_hash`
- `embedder.preprocess_chain`
- `transform_chain`
- invariant list and hard constraints

If any contract field changes → **breaking diff**.

---

## 2) Geometry Diff (Spatial)

Use a fixed probe set (or canonical sample) to measure field drift:

- **Centroid drift**: mean vector delta
- **Variance shift**: covariance or per‑axis variance delta
- **Neighborhood churn**: % of points with changed nearest‑k neighbors
- **Density shift**: change in average local density

Output: `geometry_shift_score` with component breakdown.

---

## 3) Boundary Diff (Behavioral)

Use a curated set of **boundary probes** (inputs near decision edges).

Metrics:
- **Boundary flips**: % probes that changed corridor side
- **Corridor drift**: avg distance to boundary delta
- **Safety breaches**: probes that crossed hard invariants (must be 0)

Output: `boundary_shift_score` with explicit flips list.

---

## 4) Path Diff (Executable Behavior)

Replay canonical traces (“golden workflows”) through the field.

Metrics:
- **Path divergence**: % traces that changed basin or endpoint
- **Step drift**: average number of path steps changed
- **Outcome delta**: % traces with different final labels

Output: `trace_drift_score`.

---

## Diff Report Structure

```
contract_diff:
  breaking: false
  fields_changed: []
geometry_diff:
  centroid_drift: 0.012
  variance_shift: 0.031
  neighborhood_churn: 0.08
boundary_diff:
  corridor_drift: 0.014
  boundary_flips: 0.02
  invariant_breaches: 0
path_diff:
  path_divergence: 0.05
  outcome_delta: 0.01
summary:
  severity: low
  recommendation: safe_to_deploy
```

---

## Suggested Thresholds (Initial)

- `contract_diff.breaking` → block deployment
- `invariant_breaches > 0` → block deployment
- `boundary_flips > 0.05` → manual review
- `neighborhood_churn > 0.15` → manual review
- `outcome_delta > 0.02` → manual review

These thresholds should be tuned by domain and risk profile.

---

## Operational Notes

- Keep probe sets versioned and deterministic.
- Maintain separate diffs per `space_id`.
- Store diff artifacts alongside the geometry snapshot.
