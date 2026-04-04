#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "glamin_pipeline.h"

void glamin_pipeline_demo_run(int32_t *status);

static int run_command(const char *command) {
  int rc = system(command);
  if (rc != 0) {
    fprintf(stderr, "Command failed: %s\n", command);
    return 1;
  }
  return 0;
}

static void pipeline_compile(const char *spec_path, const char *out_dir, int32_t *status) {
  char command[1024];

  snprintf(command, sizeof(command),
           "build/glamin_spec_tool compile \"%s\" --out-dir \"%s\"",
           spec_path, out_dir);
  if (run_command(command) != 0) {
    if (status) {
      *status = 1;
    }
    return;
  }

  if (status) {
    *status = 0;
  }
}

static void pipeline_embed(const char *spec_path, const char *out_dir, int32_t *status) {
  char command[1024];

  snprintf(command, sizeof(command),
           "build/glamin_spec_tool embed \"%s\" --output \"%s/vectors.bin\"",
           spec_path, out_dir);
  if (run_command(command) != 0) {
    if (status) {
      *status = 1;
    }
    return;
  }

  if (status) {
    *status = 0;
  }
}

int main(void) {
  int32_t status = 0;

  glamin_set_pipeline_callbacks(pipeline_compile, pipeline_embed);
  glamin_pipeline_demo_run(&status);
  glamin_clear_pipeline_callbacks();

  printf("Pipeline demo status: %d\n", status);
  return status == 0 ? 0 : 1;
}
