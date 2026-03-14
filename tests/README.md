Tests will cover correctness, parity against FAISS, and async behavior.

Current targets:
- `make test-gpu` — IVF GPU smoke tests (single query + batch).
- `make test-gpu-plugin` — IVF GPU smoke test with a dynamically loaded CUDA ops plugin.
