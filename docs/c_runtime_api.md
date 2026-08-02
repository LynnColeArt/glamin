# C Runtime API

## Status

The C runtime API is experimental and versioned independently from Glamin's
pure Fortran modules. ABI version 2 provides worker-runtime lifecycle,
diagnostics, and synchronous float32 flat-index add and search. Request,
snapshot, manifold, generation, and trace handles remain future extensions.

Include `include/glamin_runtime.h` and link the Glamin static library with the
Fortran, pthread, and OpenMP runtimes used to build it.

## Runtime Lifecycle

```c
glamin_runtime_t runtime = 0;
glamin_status status = glamin_runtime_create(2, &runtime);
if (status != GLAMIN_STATUS_OK) {
  /* Read the global diagnostic with glamin_last_error(0, ...). */
}

status = glamin_runtime_destroy(runtime);
```

Runtime handles are opaque, nonzero, process-local integers. A destroyed handle
is invalid and is never intentionally reused during the process lifetime.
Creation and destruction must be externally serialized in this ABI revision.

A runtime owns every index created through it. Destroy each index before the
runtime; otherwise `glamin_runtime_destroy` returns `GLAMIN_STATUS_NOT_READY`
and leaves the runtime active.

## Flat Index

ABI version 2 adds a synchronous, runtime-owned flat index:

```c
const float vectors[] = {0.0F, 0.0F, 1.0F, 1.0F};
const float query[] = {0.9F, 1.1F};
float distances[1];
uint64_t labels[1];
glamin_index_t index = 0;

status = glamin_flat_index_create(runtime, 2, GLAMIN_METRIC_L2, &index);
status = glamin_index_add_f32(runtime, index, vectors, 2, 2);
status = glamin_index_search_f32(
    runtime, index, query, 1, 2, 1, distances, labels);
status = glamin_index_destroy(runtime, index);
```

Input stride is measured in float elements and must be at least the index
dimension. Padded input rows are supported. Search output is always dense and
caller-owned, with `query_count * k` elements in query-major order. Labels are
zero-based row numbers assigned in add order. `k` must not exceed the number of
indexed rows.

L2 distances are squared Euclidean distances. Inner-product searches return
the largest dot products first.

## Diagnostics

`glamin_last_error` copies a null-terminated diagnostic into caller-owned
memory. Pass runtime `0`, a null buffer, and capacity `0` to query the size of a
global creation or invalid-handle diagnostic:

```c
uint64_t required = 0;
glamin_status status = glamin_last_error(0, NULL, 0, &required);
```

The size query returns `GLAMIN_STATUS_BUFFER_TOO_SMALL` and includes the null
terminator in `required`. Pass a valid runtime handle to read errors from index
operations. Diagnostics are intended for humans and are not a stable
machine-readable contract.

## Verification

```bash
make test-c-abi
```

The smoke tests verify ABI versioning, invalid-argument diagnostics, worker-pool
startup and shutdown, stale handles, runtime/index ownership, strided vector
adds, and exact flat-search labels and distances.
