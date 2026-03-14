#ifndef GLAMIN_CUDA_H
#define GLAMIN_CUDA_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

enum
{
  GLAMIN_OK = 0,
  GLAMIN_ERR_INVALID_ARG = 2,
  GLAMIN_ERR_NOT_READY = 4
};

typedef int32_t (*glamin_cuda_distance_fn)(const float *queries, int64_t query_count, int32_t query_stride,
    const float *vectors, int64_t vector_count, int32_t vector_stride, int32_t dim,
    float *distances, int32_t distance_stride);

typedef int32_t (*glamin_cuda_alloc_fn)(int64_t bytes, void **device_ptr);
typedef int32_t (*glamin_cuda_free_fn)(void *device_ptr);
typedef int32_t (*glamin_cuda_copy_fn)(const void *source_ptr, void *target_ptr, int64_t bytes);

typedef struct glamin_cuda_ops
{
  glamin_cuda_distance_fn distance_l2;
  glamin_cuda_distance_fn distance_ip;
  glamin_cuda_alloc_fn alloc;
  glamin_cuda_free_fn free;
  glamin_cuda_copy_fn upload;
  glamin_cuda_copy_fn download;
} glamin_cuda_ops;

int32_t glamin_cuda_register_ops(const glamin_cuda_ops *ops);
int32_t glamin_cuda_has_ops(void);
const glamin_cuda_ops *glamin_cuda_get_ops(void);

int32_t glamin_cuda_is_available(void);

#ifdef __cplusplus
}
#endif

#endif
