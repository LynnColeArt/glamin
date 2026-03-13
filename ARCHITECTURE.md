# Glamin Architecture

## Goals
- Async-first APIs with explicit request lifecycle.
- Stable, pure Fortran 2018 public interface.
- FAISS file format compatibility for selected indices.
- Performance on CPU with optional GPU backends.
- Follows `STYLE_GUIDE.md` conventions for module layout and naming.

## Execution Model
- All index operations are submitted as jobs and return a `Request`.
- Requests are polled, waited, or cancelled by the caller.
- Searches read from immutable snapshots; updates build a new snapshot then swap.
- Worker pools execute jobs from priority queues with backpressure.

## Data Model
- Vectors stored in aligned, contiguous blocks with explicit stride and padding.
- Quantization data stored as separate codebook blocks.
- Inverted lists stored as append-only blocks with optional compaction.

## Module Map
- `src/common`
  - `mod_types.f90`: core types and handles
  - `mod_errors.f90`: error codes and status
  - `mod_metrics.f90`: distance metric enums
  - `mod_embedder.f90`: embedder contract metadata
  - `mod_status.f90`: request status enumerations
  - `mod_memory.f90`: aligned allocation and memory utilities
- `src/runtime`
  - `mod_pipeline.f90`: pipeline callbacks for compile/embed
  - `mod_async.f90`: request lifecycle APIs
  - `mod_queue.f90`: job queue implementation
  - `mod_runtime.f90`: runtime context and submission helpers
  - `mod_worker_pool.f90`: worker pool control and lifecycle
  - `thread_pool.c`: pthread-backed job execution
- `src/kernels`
  - `mod_distance.f90`: L2/IP kernels and batch primitives
- `src/index`
  - `mod_flat.f90`, `mod_ivf.f90`, `mod_pq.f90`, `mod_ivfpq.f90`, `mod_hnsw.f90`
- `src/io`
  - `mod_stream.f90`: binary stream abstraction
  - `mod_vector_io.f90`: vector block loading from raw files
  - `mod_geometry_layout.f90`: parse vector layout metadata
  - `mod_geometry_loader.f90`: load vectors into flat index
  - `mod_faiss_io.f90`: FAISS format adapters
- `src/gpu`
  - `mod_gpu_backend.f90`: backend abstraction interface

## Public API Shape
- Pure Fortran modules with derived types and procedures.
- Inputs and outputs are explicit buffers owned by the caller.
- Async functions return `Request` and do not block.

## Threading and Concurrency
- Worker threads created by a C shim and controlled from Fortran.
- Job queues are bounded to enforce backpressure.
- Snapshot swapping uses atomic pointer replacement to ensure lock-free reads.

## GPU Backends
- Backends are loaded and selected by capability at runtime.
- CPU fallback always available when GPU capabilities are missing.

## Serialization
- Binary IO preserves FAISS headers and index layouts where supported.
- Endianness is handled by `mod_stream` utilities.

## Related Docs
- `docs/document_geometry_separation.md`
- `docs/space_contracts.md`
- `docs/geometry_diff.md`
- `docs/geometry_authoring.md`
