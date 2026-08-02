# C Runtime API

## Status

The C runtime API is experimental and versioned independently from Glamin's
pure Fortran modules. ABI version 4 provides worker-runtime lifecycle,
diagnostics, synchronous float32 flat-index add and search, and immutable
generation activation, pinning, retirement, and pinned search. It also loads
compiler-emitted flat-index artifacts with space-contract validation. Request,
snapshot, manifold, and trace handles remain future extensions.

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

## Immutable Generations

ABI version 3 binds a populated flat index to an immutable generation:

```c
glamin_generation_t generation = 0;
glamin_generation_pin_t pin = 0;
glamin_generation_t pinned_generation = 0;

status = glamin_generation_create(
    runtime, index, "behavior-a", 10, &generation);
status = glamin_generation_activate(runtime, generation);
status = glamin_generation_pin_active(
    runtime, &pin, &pinned_generation);
status = glamin_generation_search_f32(
    runtime, pin, query, 1, 2, 1, distances, labels);
status = glamin_generation_unpin(runtime, pin);
status = glamin_generation_deactivate(runtime);
status = glamin_generation_retire(runtime, generation);
```

Generation creation freezes its index: add and destroy return
`GLAMIN_STATUS_NOT_READY` until the generation is reclaimed. Activating a new
generation changes future pins only. Existing pins continue to search the exact
generation they resolved, including after that generation is retired.

An active generation must be superseded or deactivated before retirement.
Retirement prevents future activation and reclaims immediately when no pins
exist. Otherwise reclamation waits for the last unpin. Reclamation unbinds the
index so the caller can destroy it. Labels contain 1 to 128 non-null bytes and
can be copied with `glamin_generation_label` using the same size-query pattern
as diagnostics.

Generation and pin registry operations are externally serialized in this ABI
revision. Each runtime has at most one active generation. The fixed registries
support 256 generations and 1024 pins process-wide.

## Persistent Flat Artifacts

ABI version 4 loads one named space from a compiler-emitted artifact directory:

```c
glamin_index_t index = 0;
uint32_t dimension = 0;
uint64_t vector_count = 0;

status = glamin_flat_index_load_artifact(
    runtime,
    "build/specs",
    11,
    "geometry.auth",
    13,
    GLAMIN_METRIC_L2,
    &index,
    &dimension,
    &vector_count);
```

The directory must contain `vector_layout.json`, `vectors.bin`, and
`contracts.json`. Loading validates that the requested space exists, its
dimension and metric match the layout and requested metric, normalization is
declared, and the SHA-256 hash of the canonical space contract matches its
recorded `contract_hash`. The embedder contract and any registered hash or
signature validators are also applied.

The loaded index has the same ownership as a created flat index. It may be
searched directly, published through `glamin_generation_create`, and finally
released with `glamin_index_destroy` after generation reclamation.

Artifact files must remain unchanged for the duration of a load. ABI version 4
does not yet bind the vector file and layout to checksums in a generation
manifest; it validates compatibility and the space-contract hash, not complete
artifact provenance.

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
adds, exact flat-search results, immutable generation indexes, stable old pins,
retirement, reclamation, and deterministic rollback.
