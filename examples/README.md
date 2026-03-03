Examples demonstrate async usage and index workflows.

## async_flat_demo.f90

Build after running `make` in the repo root:

```
gfortran -std=f2018 -Ibuild/mod examples/async_flat_demo.f90 build/libglamin.a \
  -lpthread -o async_flat_demo
```

Run:

```
./async_flat_demo
```

## faiss_round_trip_demo.f90

Build after running `make` in the repo root:

```
gfortran -std=f2018 -Ibuild/mod examples/faiss_round_trip_demo.f90 build/libglamin.a \
  -lpthread -o faiss_round_trip_demo
```

Run:

```
./faiss_round_trip_demo
```
