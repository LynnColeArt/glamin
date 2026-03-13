#include <stdio.h>

#include "glamin_pipeline.h"

static void pipeline_compile(const char *spec_path, const char *out_dir, int32_t *status) {
  printf("[compile] spec=%s out=%s\n", spec_path, out_dir);
  if (status) {
    *status = 0;
  }
}

static void pipeline_embed(const char *spec_path, const char *out_dir, int32_t *status) {
  printf("[embed] spec=%s out=%s\n", spec_path, out_dir);
  if (status) {
    *status = 0;
  }
}

int main(void) {
  glamin_set_pipeline_callbacks(pipeline_compile, pipeline_embed);
  printf("Pipeline callbacks registered. Submit a pipeline request from Fortran.\n");
  glamin_clear_pipeline_callbacks();
  return 0;
}
