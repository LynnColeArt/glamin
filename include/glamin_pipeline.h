#ifndef GLAMIN_PIPELINE_H
#define GLAMIN_PIPELINE_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*glamin_pipeline_step_cb)(const char *spec_path, const char *out_dir,
                                        int32_t *status);

void glamin_set_pipeline_callbacks(glamin_pipeline_step_cb compile_cb,
                                   glamin_pipeline_step_cb embed_cb);

void glamin_clear_pipeline_callbacks(void);

#ifdef __cplusplus
}
#endif

#endif
