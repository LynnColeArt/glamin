#ifndef GLAMIN_RUNTIME_H
#define GLAMIN_RUNTIME_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GLAMIN_ABI_VERSION 3u

typedef uint64_t glamin_runtime_t;
typedef uint64_t glamin_index_t;
typedef uint64_t glamin_generation_t;
typedef uint64_t glamin_generation_pin_t;

typedef enum glamin_status {
  GLAMIN_STATUS_OK = 0,
  GLAMIN_STATUS_UNKNOWN = 1,
  GLAMIN_STATUS_INVALID_ARGUMENT = 2,
  GLAMIN_STATUS_OUT_OF_MEMORY = 3,
  GLAMIN_STATUS_NOT_READY = 4,
  GLAMIN_STATUS_CANCELLED = 5,
  GLAMIN_STATUS_BUFFER_TOO_SMALL = 6
} glamin_status;

typedef enum glamin_metric {
  GLAMIN_METRIC_L2 = 0,
  GLAMIN_METRIC_INNER_PRODUCT = 1
} glamin_metric;

uint32_t glamin_abi_version(void);

glamin_status glamin_runtime_create(uint32_t worker_count,
                                    glamin_runtime_t *out_runtime);

glamin_status glamin_runtime_destroy(glamin_runtime_t runtime);

glamin_status glamin_flat_index_create(glamin_runtime_t runtime,
                                       uint32_t dimension,
                                       glamin_metric metric,
                                       glamin_index_t *out_index);

glamin_status glamin_index_destroy(glamin_runtime_t runtime,
                                   glamin_index_t index);

glamin_status glamin_index_add_f32(glamin_runtime_t runtime,
                                   glamin_index_t index,
                                   const float *vectors,
                                   uint64_t vector_count,
                                   uint32_t vector_stride);

glamin_status glamin_index_search_f32(glamin_runtime_t runtime,
                                      glamin_index_t index,
                                      const float *queries,
                                      uint64_t query_count,
                                      uint32_t query_stride,
                                      uint32_t k,
                                      float *out_distances,
                                      uint64_t *out_labels);

glamin_status glamin_generation_create(glamin_runtime_t runtime,
                                       glamin_index_t index,
                                       const char *label,
                                       uint64_t label_length,
                                       glamin_generation_t *out_generation);

glamin_status glamin_generation_activate(glamin_runtime_t runtime,
                                         glamin_generation_t generation);

glamin_status glamin_generation_deactivate(glamin_runtime_t runtime);

glamin_status glamin_generation_pin_active(
    glamin_runtime_t runtime,
    glamin_generation_pin_t *out_pin,
    glamin_generation_t *out_generation);

glamin_status glamin_generation_unpin(glamin_runtime_t runtime,
                                      glamin_generation_pin_t pin);

glamin_status glamin_generation_retire(glamin_runtime_t runtime,
                                       glamin_generation_t generation);

glamin_status glamin_generation_label(glamin_runtime_t runtime,
                                      glamin_generation_t generation,
                                      char *buffer,
                                      uint64_t capacity,
                                      uint64_t *out_required);

glamin_status glamin_generation_search_f32(
    glamin_runtime_t runtime,
    glamin_generation_pin_t pin,
    const float *queries,
    uint64_t query_count,
    uint32_t query_stride,
    uint32_t k,
    float *out_distances,
    uint64_t *out_labels);

glamin_status glamin_last_error(glamin_runtime_t runtime,
                                char *buffer,
                                uint64_t capacity,
                                uint64_t *out_required);

#ifdef __cplusplus
}
#endif

#endif
