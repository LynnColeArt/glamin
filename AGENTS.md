# AGENTS.md
Glamin Contribution Guidance (Agents + Humans)

This repo is an experimental async-first vector runtime and geometry authoring
toolchain. Work here is judged on correctness, explicitness, deterministic
behavior, and whether the runtime, docs, schemas, examples, and tests still
describe the same system.

The strongest guidance from sibling repos is preserved here, but adapted to
Glamin's actual shape: Fortran 2018 modules, C shims, contract-bearing
geometry docs, FAISS-compatible IO, optional GPU backends, and smoke/benchmark
surfaces that must stay reviewable.

## Repo Non-Negotiables

- No new CI without Lynn's explicit approval.
- Changes to CI, CD, GitHub Actions, branch protection expectations, or other
  automated enforcement surfaces require Lynn's explicit approval before
  implementation.
- Never push directly to `main`. Use a branch-only strategy. Name the branch
  descriptively and concisely. All changes must go through a pull request and
  be reviewed by Lynn and/or Sam.
- `README.md`, `ARCHITECTURE.md`, `STYLE_GUIDE.md`,
  `docs/space_contracts.md`, `docs/geometry_authoring.md`, and
  `docs/gpu_backends.md` are authority-bearing surfaces. When semantics,
  invariants, or workflows change, keep them aligned in the same change.
- `docs/geometry_spec.yaml`, `docs/geometry_spec.schema.json`,
  `examples/geometry_spec_auth_flow.yaml`, and emitted spec artifacts are
  contract-bearing surfaces. Do not update one and leave the others
  misleading.
- Document/geometry separation, embedder contracts, snapshot semantics, and
  CPU fallback behavior are safety boundaries. Do not silently loosen them.
- Run the most relevant validation before handoff. For runtime or index work,
  this usually means `make` plus targeted `make test-*` coverage. For geometry
  tooling work, run the relevant `make spec-*` targets. For docs-only changes,
  manually sanity-check every command, file path, environment variable, and
  example you touch.

## Working Style

- Keep changes focused and surgical. Avoid opportunistic refactors.
- Match nearby file conventions before introducing new structure, especially
  in `src/`, `include/`, `docs/`, and `tools/`.
- Follow `STYLE_GUIDE.md` literally for naming, kinds, module layout,
  initialization, and comment style.
- Read the nearest governing docs before editing a surface:
  - `README.md` and `ARCHITECTURE.md` for repo framing and module map
  - `STYLE_GUIDE.md` for Fortran conventions and Glamin vocabulary
  - `docs/space_contracts.md` for contract rules
  - `docs/geometry_authoring.md` plus `docs/geometry_spec.schema.json` when
    changing spec/tooling
  - `docs/gpu_backends.md` when changing backend selection or parity
    expectations
  - `tests/README.md`, `benchmarks/README.md`, and `examples/README.md` when
    changing validation or demos
  - `SAM.md` and `LYNN.md` when collaboration context materially affects the
    work
- Use the repo's vocabulary precisely: mint, corridor, trace, manifold,
  contract, snapshot, backend, parity. Do not flatten these into generic
  vector-search language if that would hide intended behavior.
- If requirements are ambiguous on contract compatibility, async semantics,
  FAISS parity, GPU fallback, or geometry/document separation, surface the
  ambiguity instead of guessing.

## Procedural Guidance Standards

This repo should treat contributor guidance the way SkillsBench treats
effective Skills: focused, procedural, reusable, and verifier-facing.

- Prefer compact procedural guidance over omnibus essays. Two or three focused
  modules or subsections beat one giant wall of text.
- For non-obvious workflows, include steps, constraints, failure modes, and at
  least one concrete example command or file path.
- Keep guidance reusable across a class of tasks, not a single branch,
  benchmark run, or one-off example.
- Guidance should tell contributors how to approach the problem, not encode
  the exact answer they are supposed to produce.
- Do not leak task-specific expected outputs, magic constants, benchmark
  answers, or exact solving transcripts into reusable guidance.
- Human-reviewed procedural docs beat auto-generated filler. If AI helps draft
  a guide, tighten it until it reflects actual repo behavior and commands.
- When guidance grows beyond one surface, split it into a focused companion
  doc instead of bloating `AGENTS.md`.
- Prefer deterministic checks over narrative claims. If a workflow says
  something is valid, name the command or assertion that proves it.

## Code And Contract Standards

- No placeholders in production, user-facing docs, or policy-bearing tooling:
  - Do not merge `TODO`, `FIXME`, `HACK`, stub returns, silent bypasses, fake
    GPU success, or placeholder manifests unless clearly non-production and
    explicitly tracked.
- Fail closed, not open:
  - Contract mismatches, malformed specs, unsupported metrics, missing
    embedder metadata, or broken backend selection inputs must not quietly
    downgrade into success.
  - CPU fallback is acceptable only when it is the documented selection path,
    not when it hides a broken invariant.
- Keep failures visible:
  - Surface status codes, validation failures, and backend-selection outcomes
    intentionally.
  - Do not describe stub-mode validation as real CUDA or Vulkan coverage.
- Preserve ABI and serialization boundaries:
  - Changes under `include/`, `src/io/`, `src/runtime/mod_pipeline.f90`, or
    GPU loader/registration code must keep C/Fortran interfaces, symbol names,
    and doc examples aligned.
  - If FAISS compatibility or on-disk layout changes, update the relevant docs
    in the same change.
- Comments should preserve rationale, invariants, and refactor hazards. Do not
  narrate what the code obviously does.

## Verification And Documentation

- New runtime, index, or kernel behavior requires targeted tests or smoke
  coverage.
- Use the most relevant validation for the surface you changed:
  - `make`
  - `make test-distance`
  - `make test-async`
  - `make test-gpu`
  - `make test-gpu-select`
  - `make test-gpu-fallback`
  - `make test-gpu-distance-parity`
  - `make test-gpu-ivf-parity`
  - `make test-gpu-ivfpq-parity`
  - `make test-gpu-hnsw-parity`
  - `make spec-validate`
  - `make spec-compile`
  - `make spec-canonicalize`
  - `make spec-visualize`
  - `make spec-embed`
- Run only the relevant subset, but be honest about what you did not run.
- Docs and examples are part of the product. When behavior changes, update
  nearby docs, schemas, demos, and example commands in the same change.
- For benchmark changes, prefer executable evidence over prose claims. If
  performance or parity is discussed, state the command and environment flags
  used.

## Definition Of Done

Before handing work off for review, verify:

- The diff is focused and reviewable.
- Canonical docs, schemas, examples, and code surfaces stay aligned.
- Relevant validation was run and reported honestly.
- No CI, contract, serialization, or policy boundary was loosened implicitly.
- The work is on a branch and ready for PR review.

## Quick Examples

- Good branch names:
  - `agents-geometry-guidance`
  - `gpu-backend-parity-fix`
  - `spec-contract-doc-sync`
- Bad branch names:
  - `tmp`
  - `misc-updates`
