Tests will cover correctness, parity against FAISS, and async behavior.

Current targets:
- `make test-gpu` — IVF GPU smoke tests (single query + batch).
- `make test-gpu-plugin` — IVF GPU smoke test with a dynamically loaded CUDA ops plugin.
- `make test-gpu-select` — GPU backend auto-selection smoke test.
- `make test-gpu-fallback` — GPU dispatch fallback smoke test.
- `make test-async` — Async IVF train/add/search and HNSW snapshot smoke tests.
- `make test-distance` — L2/IP distance kernel smoke test.
