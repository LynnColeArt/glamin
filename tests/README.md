Tests will cover correctness, parity against FAISS, and async behavior.

Current targets:
- `make test-gpu` — IVF GPU smoke tests (single query + batch).
- `make test-gpu-plugin` — IVF GPU smoke test with a dynamically loaded CUDA ops plugin.
- `make test-gpu-select` — GPU backend auto-selection smoke test.
- `make test-gpu-fallback` — GPU dispatch fallback smoke test.
- `make test-gpu-distance-parity` — GPU distance dispatch parity vs CPU.
- `make test-gpu-distance-parity-vulkan` — Vulkan backend distance parity vs CPU.
- `make test-gpu-ivf-parity` — IVF GPU search parity vs CPU.
- `make test-gpu-ivf-parity-vulkan` — Vulkan backend IVF parity vs CPU.
- `make test-gpu-ivfpq-parity` — IVF-PQ parity with GPU backend selected.
- `make test-gpu-hnsw-parity` — HNSW parity with GPU backend selected.
- `make test-async` — Async IVF train/add/search and HNSW snapshot smoke tests.
- `make test-distance` — L2/IP distance kernel smoke test.
- `make test-c-abi` — C ABI runtime/index ownership, diagnostics, strided add,
  flat search, and immutable generation activation/pin/rollback smoke tests.
