#ifndef GLAMIN_RUNTIME_H
#define GLAMIN_RUNTIME_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GLAMIN_ABI_VERSION 1u

typedef uint64_t glamin_runtime_t;

typedef enum glamin_status {
  GLAMIN_STATUS_OK = 0,
  GLAMIN_STATUS_UNKNOWN = 1,
  GLAMIN_STATUS_INVALID_ARGUMENT = 2,
  GLAMIN_STATUS_OUT_OF_MEMORY = 3,
  GLAMIN_STATUS_NOT_READY = 4,
  GLAMIN_STATUS_CANCELLED = 5,
  GLAMIN_STATUS_BUFFER_TOO_SMALL = 6
} glamin_status;

uint32_t glamin_abi_version(void);

glamin_status glamin_runtime_create(uint32_t worker_count,
                                    glamin_runtime_t *out_runtime);

glamin_status glamin_runtime_destroy(glamin_runtime_t runtime);

glamin_status glamin_last_error(glamin_runtime_t runtime,
                                char *buffer,
                                uint64_t capacity,
                                uint64_t *out_required);

#ifdef __cplusplus
}
#endif

#endif
