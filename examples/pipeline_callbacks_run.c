#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

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

static int ensure_env(void) {
  static int ready = 0;

  if (ready) {
    return 0;
  }

  if (access("build/venv/bin/python", F_OK) != 0) {
    if (run_command("python3 -m venv build/venv") != 0) {
      return 1;
    }
  }

  if (run_command("build/venv/bin/python -m pip install -r tools/requirements.txt") != 0) {
    return 1;
  }

  ready = 1;
  return 0;
}

static void pipeline_compile(const char *spec_path, const char *out_dir, int32_t *status) {
  char command[1024];

  if (ensure_env() != 0) {
    if (status) {
      *status = 1;
    }
    return;
  }

  snprintf(command, sizeof(command),
           "build/venv/bin/python tools/geometry_spec_tool.py compile \"%s\" --out-dir \"%s\"",
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

  if (ensure_env() != 0) {
    if (status) {
      *status = 1;
    }
    return;
  }

  snprintf(command, sizeof(command),
           "build/venv/bin/python tools/geometry_embedder_cpu.py \"%s\" --output \"%s/vectors.bin\"",
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
