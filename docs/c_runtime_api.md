# C Runtime API

## Status

The C runtime API is experimental and versioned independently from Glamin's
pure Fortran modules. The first revision exposes worker-runtime lifecycle and
diagnostics. Index, request, snapshot, manifold, and trace handles remain future
extensions.

Include `include/glamin_runtime.h` and link the Glamin static library with the
Fortran, pthread, and OpenMP runtimes used to build it.

## Lifecycle

```c
glamin_runtime_t runtime = 0;
glamin_status status = glamin_runtime_create(2, &runtime);
if (status != GLAMIN_STATUS_OK) {
  /* Read the global diagnostic with glamin_last_error(0, ...). */
}

/* Use later C ABI operations with runtime. */

status = glamin_runtime_destroy(runtime);
```

Runtime handles are opaque, nonzero, process-local integers. A destroyed handle
is invalid and is never intentionally reused during the process lifetime.
Creation and destruction must be externally serialized in this ABI revision.

## Diagnostics

`glamin_last_error` copies a null-terminated diagnostic into caller-owned
memory. Pass runtime `0`, a null buffer, and capacity `0` to query the size of a
global creation or invalid-handle diagnostic:

```c
uint64_t required = 0;
glamin_status status = glamin_last_error(0, NULL, 0, &required);
```

The size query returns `GLAMIN_STATUS_BUFFER_TOO_SMALL` and includes the null
terminator in `required`. Diagnostics are intended for humans and are not a
stable machine-readable contract.

## Verification

```bash
make test-c-abi
```

The smoke test verifies ABI versioning, invalid-argument diagnostics, real
worker-pool startup and shutdown, and stale-handle rejection.
