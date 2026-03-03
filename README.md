Glamin (Geometric Logical Application Meta-Instruction Network) is an experimental,
async-first vector search core written in Fortran 2018.
This repository scaffolds the FAISS migration plan, module layout, and compatibility goals.

Key goals:
- Pure Fortran public API, with internal C shims where required.
- Async-first execution with request handles and snapshot semantics.
- FAISS file format compatibility for supported index types.

Repository layout:
- src/common: shared types, errors, memory utilities
- src/runtime: async runtime, queues, worker pool
- src/kernels: distance kernels and SIMD-friendly primitives
- src/index: index implementations (Flat, IVF, PQ, IVFPQ, HNSW)
- src/io: serialization and FAISS compatibility
- src/gpu: pluggable GPU backend interface
- examples: small usage examples (to be added)
- tests: correctness and parity tests (to be added)
- benchmarks: performance micro-benchmarks (to be added)

Build:
- `make` builds `build/libglamin.a`
- `make clean` removes build outputs
