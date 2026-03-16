Benchmarks will track kernel throughput and index search latency.

## distance_benchmark.f90

Build and run after `make`:

```
make bench-distance
```

Override defaults via Makefile variables:

```
make bench-distance BENCH_DIM=512 BENCH_QUERIES=512 BENCH_VECTORS=8192 BENCH_ITERS=5
```
