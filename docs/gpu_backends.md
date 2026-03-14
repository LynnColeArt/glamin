# GPU Backend Selection

This document defines how Glamin chooses GPU backends and falls back to CPU.

---

## Goals

- Prefer hardware acceleration when it is available.
- Fall back to CPU without breaking call sites.
- Allow explicit overrides for testing and deployment.

---

## Selection Flow

1. Registered backends expose `init` for capability probing.
2. Auto selection tries CUDA, then Vulkan, then CPU fallback.
3. If no backend initializes, CPU kernels remain active.

CPU fallback means no GPU backend is active; dispatchers route to the
CPU kernels automatically.

IVF search uses the same dispatch path for list distance batches, so GPU
backends apply there as well.

---

## Configuration

Use environment variables to override selection:

- `GLAMIN_GPU_BACKEND`
  - `auto` (default)
  - `cuda`
  - `vulkan`
  - `cpu`
- `GLAMIN_GPU_BACKEND_ORDER`
  - Comma-separated list used when `GLAMIN_GPU_BACKEND=auto`
  - Default: `cuda,vulkan`
- `GLAMIN_CUDA_AVAILABLE`
  - Stub flag for the CUDA backend (set to `1` to allow init)
- `GLAMIN_VULKAN_AVAILABLE`
  - Stub flag for the Vulkan backend (set to `1` to allow init)

The current CUDA backend runs a CPU emulation path when enabled; Vulkan
remains a stub using CPU kernels until GPU kernels land.

CUDA buffer helpers are defined in `src/gpu/mod_cuda_memory.f90` and currently
allocate host memory in the emulation path.

External CUDA implementations can register kernels and memory ops via
`include/glamin_cuda.h` (`glamin_cuda_register_ops`).

The stub path can be registered via `glamin_cuda_register_stub_ops` (C)
or `cuda_register_stub_ops` (Fortran) to validate wiring.

Example (C registration sketch):

```c
#include "glamin_cuda.h"

static int32_t my_distance_l2(const float *queries, int64_t query_count, int32_t query_stride,
    const float *vectors, int64_t vector_count, int32_t vector_stride, int32_t dim,
    float *distances, int32_t distance_stride)
{
  return GLAMIN_OK;
}

int main(void)
{
  glamin_cuda_ops ops = {
    my_distance_l2,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
  };
  return glamin_cuda_register_ops(&ops);
}
```

---

## Integration Example

Register available backends and let auto selection pick the best one:

```
type(CudaBackend) :: cuda_backend
type(VulkanBackend) :: vulkan_backend
integer(int32) :: status

call gpu_register_backend('cuda', cuda_backend, status)
call gpu_register_backend('vulkan', vulkan_backend, status)
call gpu_auto_select_backend(status)
```

Query the active selection:

```
character(len=GPU_BACKEND_NAME_LEN) :: name
call gpu_get_backend_name(name)
```

---

## Evaluation Checklist

- **Correctness parity**: GPU results match CPU within tolerance.
- **Selection determinism**: Auto-selection follows the configured order.
- **Availability probing**: Backend init fails fast; CPU fallback always works.
- **Performance thresholds**: Speedup is measurable at target batch sizes.
- **Overhead bounds**: Init latency and memory footprint stay within caps.
- **Async stability**: No request lifecycle regressions under load.
- **Configurability**: Environment overrides take effect reliably.
- **Portability**: Builds and runs on supported OS/driver targets.
- **Observability**: Active backend and capabilities are queryable.

---

## Baseline Targets (2-Year-Old Gaming PC)

Assume a 6–8 core CPU, 32 GB RAM, and a GPU with 8–12 GB VRAM.

- **Distance parity**: L2/IP absolute error ≤ `1e-4` and relative error ≤ `1e-4`.
- **Top-k stability**: ≥ 99% of queries have identical top-10 sets.
- **IVF parity**: For `nprobe=16`, ≥ 98% top-10 overlap vs CPU.
- **Distance speedup**: ≥ 2.0× CPU throughput for `Q≥256`, `D≥768`.
- **IVF speedup**: ≥ 1.5× CPU throughput for `Q≥128`, `nb≥1e6`, `k=10`.
- **Init latency**: ≤ 250 ms cold start on GPU-capable systems.
- **Memory overhead**: Persistent GPU buffers ≤ 1.25× index size.
