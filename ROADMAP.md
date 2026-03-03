# Glamin FAISS Migration Roadmap

## Phase 0: Definition
- Produce architecture and compatibility docs.
- Decide initial index set and async semantics.

## Phase 1: Runtime and Async Core
- Implement request lifecycle: submit, poll, wait, cancel.
- Implement job queues and worker pool with C threading.

## Phase 2: Kernels and Data Primitives
- Implement aligned vector blocks and batch processing.
- Implement L2/IP kernels with AVX2 baseline.

## Phase 3: Core Indices
- Flat index with async search and add.
- IVF index with coarse quantizer.
- PQ encoder and IVFPQ composition.

## Phase 4: HNSW
- Graph build, search, and snapshot integration.
- Background build tasks and tuneable parameters.

## Phase 5: Serialization and FAISS Compatibility
- Binary IO streams with endian handling.
- Load/save supported FAISS formats and validate output parity.

## Phase 6: GPU Backend Interface
- Define backend capabilities and dispatch.
- Implement one backend end-to-end for distance and IVF search.

## Phase 7: Performance and Memory
- Cache blocking, prefetching, and batch planning.
- NUMA affinity, huge pages, and memory compaction.

## Phase 8: Developer Experience
- Examples, documentation, and parity test suite.
- Benchmark harness and regression gates.
