# Glamin FAISS Migration Roadmap

## Phase 0: Definition
- [x] Produce architecture and compatibility docs.
- [x] Document document/geometry separation pattern.
- [x] Decide initial index set and async semantics.

## Phase 1: Runtime and Async Core
- [x] Implement request lifecycle: submit, poll, wait, cancel.
- [x] Implement job queues and worker pool with C threading.

## Phase 2: Kernels and Data Primitives
- [x] Implement aligned vector blocks and batch processing.
- [x] Implement L2/IP kernels (scalar baseline; AVX2 pending).

## Phase 2.5: Embedder Contracts
- [ ] Define embedder contract schema and validation.
- [ ] Attach embedder contract to write path (reject mismatches).
- [ ] CPU-only reference embedder (bag-of-words baseline).
- [ ] Embedder contract signing and hash verification hooks.

## Phase 3: Core Indices
- [x] Flat index with async search and add.
- [x] PQ add/search primitives (nbits=8, codebooks required).
- [ ] IVF index with coarse quantizer.
- [ ] PQ training/encode/decode.
- [ ] IVFPQ composition.

## Phase 4: HNSW
- [ ] Graph build, search, and snapshot integration.
- [ ] Background build tasks and tuneable parameters.

## Phase 5: Serialization and FAISS Compatibility
- [x] Binary IO streams with endian handling.
- [x] Load/save FAISS IndexFlatL2/IndexFlatIP/IndexPQ.
- [ ] Load/save IVF/IVFPQ/HNSW formats and validate output parity.

## Phase 6: GPU Backend Interface
- [ ] Define backend capabilities and dispatch.
- [ ] Implement one backend end-to-end for distance and IVF search.

## Phase 6a: ANE backend via Sporkle C bridge
- [ ] Integrate Sporkle C bridge and ANE kernels.
- [ ] Add capability probing and CPU/GPU fallbacks.

## Phase 6b: GPU/NPU embedder acceleration
- [ ] Define embedder hardware classes and selection rules.
- [ ] Add GPU embedder bridge (desktop-class baseline).
- [ ] Add NPU embedder bridge (mobile/edge hardware).

## Phase 7: Performance and Memory
- [ ] Cache blocking, prefetching, and batch planning.
- [ ] NUMA affinity, huge pages, and memory compaction.

## Phase 8: Developer Experience
- [x] Examples for async flat, FAISS round-trip, and PQ.
- [ ] Parity test suite and broader documentation.
- [ ] Benchmark harness and regression gates.
