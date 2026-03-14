#include <stddef.h>
#include <stdint.h>

#include "glamin_cuda.h"

static int32_t glamin_cuda_stub_distance(const float *queries, int64_t query_count, int32_t query_stride,
    const float *vectors, int64_t vector_count, int32_t vector_stride, int32_t dim,
    float *distances, int32_t distance_stride)
{
  (void)queries;
  (void)query_count;
  (void)query_stride;
  (void)vectors;
  (void)vector_count;
  (void)vector_stride;
  (void)dim;
  (void)distances;
  (void)distance_stride;
  return GLAMIN_ERR_NOT_READY;
}

static int32_t glamin_cuda_stub_alloc(int64_t bytes, void **device_ptr)
{
  (void)bytes;
  if (device_ptr)
  {
    *device_ptr = NULL;
  }
  return GLAMIN_ERR_NOT_READY;
}

static int32_t glamin_cuda_stub_free(void *device_ptr)
{
  (void)device_ptr;
  return GLAMIN_OK;
}

static int32_t glamin_cuda_stub_copy(const void *source_ptr, void *target_ptr, int64_t bytes)
{
  (void)source_ptr;
  (void)target_ptr;
  (void)bytes;
  return GLAMIN_ERR_NOT_READY;
}

int32_t glamin_cuda_register_stub_ops(void)
{
  glamin_cuda_ops ops = {
    glamin_cuda_stub_distance,
    glamin_cuda_stub_distance,
    glamin_cuda_stub_alloc,
    glamin_cuda_stub_free,
    glamin_cuda_stub_copy,
    glamin_cuda_stub_copy
  };
  return glamin_cuda_register_ops(&ops);
}
