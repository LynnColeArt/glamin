#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "glamin_cuda.h"

static int32_t plugin_distance_l2(const float *queries, int64_t query_count, int32_t query_stride,
    const float *vectors, int64_t vector_count, int32_t vector_stride, int32_t dim,
    float *distances, int32_t distance_stride)
{
  int64_t query_index;
  int64_t vector_index;
  int32_t dim_index;

  if (!queries || !vectors || !distances || query_count <= 0 || vector_count <= 0 || dim <= 0)
  {
    return GLAMIN_ERR_INVALID_ARG;
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

  return GLAMIN_OK;
}

static int32_t plugin_distance_ip(const float *queries, int64_t query_count, int32_t query_stride,
    const float *vectors, int64_t vector_count, int32_t vector_stride, int32_t dim,
    float *distances, int32_t distance_stride)
{
  int64_t query_index;
  int64_t vector_index;
  int32_t dim_index;

  if (!queries || !vectors || !distances || query_count <= 0 || vector_count <= 0 || dim <= 0)
  {
    return GLAMIN_ERR_INVALID_ARG;
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

  return GLAMIN_OK;
}

static int32_t plugin_alloc(int64_t bytes, void **device_ptr)
{
  if (!device_ptr || bytes <= 0)
  {
    return GLAMIN_ERR_INVALID_ARG;
  }

  *device_ptr = malloc((size_t)bytes);
  if (!*device_ptr)
  {
    return GLAMIN_ERR_NOT_READY;
  }

  return GLAMIN_OK;
}

static int32_t plugin_free(void *device_ptr)
{
  free(device_ptr);
  return GLAMIN_OK;
}

static int32_t plugin_copy(const void *source_ptr, void *target_ptr, int64_t bytes)
{
  if (!source_ptr || !target_ptr || bytes <= 0)
  {
    return GLAMIN_ERR_INVALID_ARG;
  }

  memcpy(target_ptr, source_ptr, (size_t)bytes);
  return GLAMIN_OK;
}

int32_t glamin_cuda_plugin_init(glamin_cuda_ops *ops)
{
  if (!ops)
  {
    return GLAMIN_ERR_INVALID_ARG;
  }

  ops->distance_l2 = plugin_distance_l2;
  ops->distance_ip = plugin_distance_ip;
  ops->alloc = plugin_alloc;
  ops->free = plugin_free;
  ops->upload = plugin_copy;
  ops->download = plugin_copy;
  return GLAMIN_OK;
}
