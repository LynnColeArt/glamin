#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "glamin_runtime.h"

static int require(int condition, const char *message) {
  if (!condition) {
    fprintf(stderr, "c runtime abi smoke failed: %s\n", message);
    return 0;
  }
  return 1;
}

int main(void) {
  glamin_runtime_t runtime = 0;
  uint64_t required = 0;
  char *diagnostic = NULL;
  glamin_status status;

  if (!require(glamin_abi_version() == GLAMIN_ABI_VERSION, "ABI version mismatch")) {
    return EXIT_FAILURE;
  }

  status = glamin_runtime_create(1, NULL);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "null runtime output must be rejected")) {
    return EXIT_FAILURE;
  }

  status = glamin_runtime_create(0, &runtime);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "zero worker count must be rejected") ||
      !require(runtime == 0, "failed creation must not return a handle")) {
    return EXIT_FAILURE;
  }

  status = glamin_last_error(0, NULL, 0, &required);
  if (!require(status == GLAMIN_STATUS_BUFFER_TOO_SMALL,
               "diagnostic size query must report buffer too small") ||
      !require(required > 1, "diagnostic size must include a message")) {
    return EXIT_FAILURE;
  }

  diagnostic = malloc((size_t)required);
  if (!require(diagnostic != NULL, "diagnostic allocation failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_last_error(0, diagnostic, required, &required);
  if (!require(status == GLAMIN_STATUS_OK, "diagnostic retrieval failed") ||
      !require(strstr(diagnostic, "worker_count") != NULL,
               "diagnostic does not explain the rejected worker count")) {
    free(diagnostic);
    return EXIT_FAILURE;
  }
  free(diagnostic);

  status = glamin_last_error(0, NULL, 0, NULL);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "null diagnostic size output must be rejected")) {
    return EXIT_FAILURE;
  }

  status = glamin_runtime_create(2, &runtime);
  if (!require(status == GLAMIN_STATUS_OK, "runtime creation failed") ||
      !require(runtime != 0, "runtime creation returned an invalid handle")) {
    return EXIT_FAILURE;
  }

  status = glamin_runtime_destroy(runtime);
  if (!require(status == GLAMIN_STATUS_OK, "runtime destruction failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_runtime_destroy(runtime);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "destroyed runtime handle must become invalid")) {
    return EXIT_FAILURE;
  }

  puts("c runtime abi smoke ok");
  return EXIT_SUCCESS;
}
