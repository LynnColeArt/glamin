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

## ivf_benchmark.f90

Build and run:

```
make bench-ivf
```

Override defaults:

```
make bench-ivf BENCH_IVF_DIM=256 BENCH_IVF_VECTORS=8192 BENCH_IVF_QUERIES=512 \
  BENCH_IVF_NLIST=64 BENCH_IVF_NPROBE=8 BENCH_IVF_K=10 BENCH_IVF_ITERS=5
```

## hnsw_benchmark.f90

Build and run:

```
make bench-hnsw
```

Override defaults:

```
make bench-hnsw BENCH_HNSW_DIM=256 BENCH_HNSW_VECTORS=8192 BENCH_HNSW_QUERIES=512 \
  BENCH_HNSW_M=16 BENCH_HNSW_EF_CONSTRUCTION=64 BENCH_HNSW_EF_SEARCH=32 \
  BENCH_HNSW_K=10 BENCH_HNSW_ITERS=5 BENCH_HNSW_SNAPSHOT=1
```
