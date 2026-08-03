#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "glamin_runtime.h"

static int require(int condition, const char *message) {
  if (!condition) {
    fprintf(stderr, "c flat index abi smoke failed: %s\n", message);
    return 0;
  }
  return 1;
}

static float absolute_value(float value) {
  return value < 0.0F ? -value : value;
}

static int require_near(float actual, float expected, const char *message) {
  return require(absolute_value(actual - expected) < 1.0e-5F, message);
}

int main(void) {
  const float vectors[] = {
      0.0F, 0.0F, 99.0F,
      1.0F, 1.0F, 99.0F,
      2.0F, 2.0F, 99.0F,
  };
  const float queries[] = {
      0.1F, 0.1F, 99.0F,
      1.9F, 2.1F, 99.0F,
  };
  const uint64_t expected_labels[] = {0, 1, 2, 1};
  const float expected_distances[] = {0.02F, 1.62F, 0.02F, 2.02F};
  const float basis_vectors[] = {1.0F, 0.0F, 0.0F, 1.0F};
  const float basis_query[] = {0.1F, 0.9F};
  glamin_runtime_t runtime = 0;
  glamin_runtime_t other_runtime = 0;
  glamin_index_t index = 0;
  float distances[4] = {0};
  uint64_t labels[4] = {0};
  char diagnostic[256] = {0};
  uint64_t required = 0;
  glamin_status status;
  size_t result_index;

  status = glamin_runtime_create(2, &runtime);
  if (!require(status == GLAMIN_STATUS_OK, "runtime creation failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_flat_index_create(runtime, 2, GLAMIN_METRIC_L2, &index);
  if (!require(status == GLAMIN_STATUS_OK, "flat index creation failed") ||
      !require(index != 0, "flat index creation returned an invalid handle")) {
    return EXIT_FAILURE;
  }

  status = glamin_runtime_create(1, &other_runtime);
  if (!require(status == GLAMIN_STATUS_OK, "second runtime creation failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_index_destroy(other_runtime, index);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "a different runtime must not destroy the index")) {
    return EXIT_FAILURE;
  }
  status = glamin_runtime_destroy(other_runtime);
  if (!require(status == GLAMIN_STATUS_OK, "second runtime destruction failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_runtime_destroy(runtime);
  if (!require(status == GLAMIN_STATUS_NOT_READY,
               "runtime destruction must reject a live index")) {
    return EXIT_FAILURE;
  }
  status = glamin_last_error(runtime, diagnostic, sizeof(diagnostic), &required);
  if (!require(status == GLAMIN_STATUS_OK, "runtime diagnostic retrieval failed") ||
      !require(strstr(diagnostic, "indexes") != NULL,
               "runtime diagnostic must explain the live index")) {
    return EXIT_FAILURE;
  }

  status = glamin_index_add_f32(runtime, index, vectors, 3, 3);
  if (!require(status == GLAMIN_STATUS_OK, "strided vector add failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_index_search_f32(runtime, index, queries, 2, 3, 2,
                                   distances, labels);
  if (!require(status == GLAMIN_STATUS_OK, "strided flat search failed")) {
    return EXIT_FAILURE;
  }

  for (result_index = 0; result_index < 4; ++result_index) {
    if (!require(labels[result_index] == expected_labels[result_index],
                 "search label mismatch") ||
        !require_near(distances[result_index], expected_distances[result_index],
                      "search distance mismatch")) {
      return EXIT_FAILURE;
    }
  }

  status = glamin_index_search_f32(runtime, index, queries, 2, 3, 4,
                                   distances, labels);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "k larger than the index must be rejected")) {
    return EXIT_FAILURE;
  }

  status = glamin_index_destroy(runtime, index);
  if (!require(status == GLAMIN_STATUS_OK, "flat index destruction failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_index_destroy(runtime, index);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "destroyed index handle must become invalid")) {
    return EXIT_FAILURE;
  }

  status = glamin_flat_index_create(
      runtime, 2, GLAMIN_METRIC_INNER_PRODUCT, &index);
  if (!require(status == GLAMIN_STATUS_OK, "inner-product index creation failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_index_add_f32(runtime, index, basis_vectors, 2, 2);
  if (!require(status == GLAMIN_STATUS_OK, "inner-product vector add failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_index_search_f32(
      runtime, index, basis_query, 1, 2, 1, distances, labels);
  if (!require(status == GLAMIN_STATUS_OK, "inner-product search failed") ||
      !require(labels[0] == 1, "inner-product label mismatch") ||
      !require_near(distances[0], 0.9F, "inner-product distance mismatch")) {
    return EXIT_FAILURE;
  }
  status = glamin_index_destroy(runtime, index);
  if (!require(status == GLAMIN_STATUS_OK, "inner-product index destruction failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_runtime_destroy(runtime);
  if (!require(status == GLAMIN_STATUS_OK, "runtime destruction failed")) {
    return EXIT_FAILURE;
  }

  puts("c flat index abi smoke ok");
  return EXIT_SUCCESS;
}
