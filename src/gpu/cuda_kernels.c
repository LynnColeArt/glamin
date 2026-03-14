#include <stdint.h>
#include <stdlib.h>

#include "glamin_cuda.h"

static int32_t glamin_cuda_emulation_enabled(void)
{
  const char *value = getenv("GLAMIN_CUDA_AVAILABLE");
  if (!value || value[0] == '\0')
  {
    return 0;
  }

  switch (value[0])
  {
    case '1':
    case 't':
    case 'T':
    case 'y':
    case 'Y':
    case 'o':
    case 'O':
      return 1;
    default:
      return 0;
  }
}

int32_t glamin_cuda_is_available(void)
{
  const glamin_cuda_ops *ops = glamin_cuda_get_ops();
  if (ops && (ops->distance_l2 || ops->distance_ip || ops->alloc || ops->upload || ops->download))
  {
    return 1;
  }

  return glamin_cuda_emulation_enabled();
}

void glamin_cuda_distance_l2(const float *queries, int64_t query_count, int32_t query_stride,
    const float *vectors, int64_t vector_count, int32_t vector_stride, int32_t dim,
    float *distances, int32_t distance_stride, int32_t *status)
{
  int64_t query_index;
  int64_t vector_index;
  int32_t dim_index;

  if (!queries || !vectors || !distances || query_count <= 0 || vector_count <= 0 || dim <= 0)
  {
    if (status)
    {
      *status = GLAMIN_OK;
    }
    return;
  }

  {
    const glamin_cuda_ops *ops = glamin_cuda_get_ops();
    if (ops && ops->distance_l2)
    {
      int32_t result = ops->distance_l2(queries, query_count, query_stride, vectors, vector_count,
        vector_stride, dim, distances, distance_stride);
      if (status)
      {
        *status = result;
      }
      if (result == GLAMIN_OK || !glamin_cuda_emulation_enabled())
      {
        return;
      }
    }
  }

  if (!glamin_cuda_emulation_enabled())
  {
    if (status)
    {
      *status = GLAMIN_ERR_NOT_READY;
    }
    return;
  }

  for (query_index = 0; query_index < query_count; ++query_index)
  {
    const float *query_ptr = queries + query_index * query_stride;
    float *distance_ptr = distances + query_index * distance_stride;

    for (vector_index = 0; vector_index < vector_count; ++vector_index)
    {
      const float *vector_ptr = vectors + vector_index * vector_stride;
      float accum = 0.0f;

      for (dim_index = 0; dim_index < dim; ++dim_index)
      {
        float diff = query_ptr[dim_index] - vector_ptr[dim_index];
        accum += diff * diff;
      }

      distance_ptr[vector_index] = accum;
    }
  }

  if (status)
  {
    *status = GLAMIN_OK;
  }
}

void glamin_cuda_distance_ip(const float *queries, int64_t query_count, int32_t query_stride,
    const float *vectors, int64_t vector_count, int32_t vector_stride, int32_t dim,
    float *distances, int32_t distance_stride, int32_t *status)
{
  int64_t query_index;
  int64_t vector_index;
  int32_t dim_index;

  if (!queries || !vectors || !distances || query_count <= 0 || vector_count <= 0 || dim <= 0)
  {
    if (status)
    {
      *status = GLAMIN_OK;
    }
    return;
  }

  {
    const glamin_cuda_ops *ops = glamin_cuda_get_ops();
    if (ops && ops->distance_ip)
    {
      int32_t result = ops->distance_ip(queries, query_count, query_stride, vectors, vector_count,
        vector_stride, dim, distances, distance_stride);
      if (status)
      {
        *status = result;
      }
      if (result == GLAMIN_OK || !glamin_cuda_emulation_enabled())
      {
        return;
      }
    }
  }

  if (!glamin_cuda_emulation_enabled())
  {
    if (status)
    {
      *status = GLAMIN_ERR_NOT_READY;
    }
    return;
  }

  for (query_index = 0; query_index < query_count; ++query_index)
  {
    const float *query_ptr = queries + query_index * query_stride;
    float *distance_ptr = distances + query_index * distance_stride;

    for (vector_index = 0; vector_index < vector_count; ++vector_index)
    {
      const float *vector_ptr = vectors + vector_index * vector_stride;
      float accum = 0.0f;

      for (dim_index = 0; dim_index < dim; ++dim_index)
      {
        accum += query_ptr[dim_index] * vector_ptr[dim_index];
      }

      distance_ptr[vector_index] = accum;
    }
  }

  if (status)
  {
    *status = GLAMIN_OK;
  }
}
