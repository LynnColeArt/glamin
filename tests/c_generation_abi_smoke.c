#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "glamin_runtime.h"

static int require(int condition, const char *message) {
  if (!condition) {
    fprintf(stderr, "c generation abi smoke failed: %s\n", message);
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

static int search_one(glamin_runtime_t runtime,
                      glamin_generation_pin_t pin,
                      const float *query,
                      uint64_t expected_label,
                      float expected_distance) {
  float distance = 0.0F;
  uint64_t label = 0;
  glamin_status status = glamin_generation_search_f32(
      runtime, pin, query, 1, 2, 1, &distance, &label);

  return require(status == GLAMIN_STATUS_OK, "pinned generation search failed") &&
         require(label == expected_label, "pinned generation label mismatch") &&
         require_near(distance, expected_distance,
                      "pinned generation distance mismatch");
}

int main(void) {
  static const char label_a[] = "behavior-a";
  static const char label_b[] = "behavior-b";
  const float vectors_a[] = {0.0F, 0.0F, 10.0F, 10.0F};
  const float vectors_b[] = {100.0F, 100.0F, 2.0F, 2.0F};
  const float query[] = {1.9F, 2.1F};
  glamin_runtime_t runtime = 0;
  glamin_index_t index_a = 0;
  glamin_index_t index_b = 0;
  glamin_generation_t generation_a = 0;
  glamin_generation_t generation_b = 0;
  glamin_generation_t pinned_generation = 0;
  glamin_generation_pin_t pin_a = 0;
  glamin_generation_pin_t pin_b = 0;
  glamin_generation_pin_t rollback_pin = 0;
  char returned_label[32] = {0};
  uint64_t required = 0;
  glamin_status status;

  status = glamin_runtime_create(2, &runtime);
  if (!require(status == GLAMIN_STATUS_OK, "runtime creation failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_flat_index_create(runtime, 2, GLAMIN_METRIC_L2, &index_a);
  if (!require(status == GLAMIN_STATUS_OK, "generation A index creation failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_index_add_f32(runtime, index_a, vectors_a, 2, 2);
  if (!require(status == GLAMIN_STATUS_OK, "generation A vector add failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_create(
      runtime, index_a, label_a, sizeof(label_a) - 1, &generation_a);
  if (!require(status == GLAMIN_STATUS_OK, "generation A creation failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_index_add_f32(runtime, index_a, vectors_a, 2, 2);
  if (!require(status == GLAMIN_STATUS_NOT_READY,
               "mounted generation index must be immutable")) {
    return EXIT_FAILURE;
  }
  status = glamin_index_destroy(runtime, index_a);
  if (!require(status == GLAMIN_STATUS_NOT_READY,
               "mounted generation index must not be destroyed")) {
    return EXIT_FAILURE;
  }

  status = glamin_flat_index_create(runtime, 2, GLAMIN_METRIC_L2, &index_b);
  if (!require(status == GLAMIN_STATUS_OK, "generation B index creation failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_index_add_f32(runtime, index_b, vectors_b, 2, 2);
  if (!require(status == GLAMIN_STATUS_OK, "generation B vector add failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_create(
      runtime, index_b, label_b, sizeof(label_b) - 1, &generation_b);
  if (!require(status == GLAMIN_STATUS_OK, "generation B creation failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_generation_pin_active(runtime, &pin_a, &pinned_generation);
  if (!require(status == GLAMIN_STATUS_NOT_READY,
               "pinning without an active generation must fail")) {
    return EXIT_FAILURE;
  }

  status = glamin_generation_activate(runtime, generation_a);
  if (!require(status == GLAMIN_STATUS_OK, "generation A activation failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_retire(runtime, generation_a);
  if (!require(status == GLAMIN_STATUS_NOT_READY,
               "active generation retirement must fail")) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_pin_active(runtime, &pin_a, &pinned_generation);
  if (!require(status == GLAMIN_STATUS_OK, "generation A pin failed") ||
      !require(pinned_generation == generation_a,
               "generation A pin resolved the wrong generation") ||
      !search_one(runtime, pin_a, query, 0, 8.02F)) {
    return EXIT_FAILURE;
  }

  status = glamin_generation_label(runtime, generation_a, NULL, 0, &required);
  if (!require(status == GLAMIN_STATUS_BUFFER_TOO_SMALL,
               "generation label size query failed") ||
      !require(required == sizeof(label_a), "generation label size mismatch")) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_label(
      runtime, generation_a, returned_label, sizeof(returned_label), &required);
  if (!require(status == GLAMIN_STATUS_OK, "generation label retrieval failed") ||
      !require(strcmp(returned_label, label_a) == 0,
               "generation label content mismatch")) {
    return EXIT_FAILURE;
  }

  status = glamin_generation_activate(runtime, generation_b);
  if (!require(status == GLAMIN_STATUS_OK, "generation B activation failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_pin_active(runtime, &pin_b, &pinned_generation);
  if (!require(status == GLAMIN_STATUS_OK, "generation B pin failed") ||
      !require(pinned_generation == generation_b,
               "generation B pin resolved the wrong generation") ||
      !search_one(runtime, pin_b, query, 1, 0.02F) ||
      !search_one(runtime, pin_a, query, 0, 8.02F)) {
    return EXIT_FAILURE;
  }

  status = glamin_generation_activate(runtime, generation_a);
  if (!require(status == GLAMIN_STATUS_OK, "generation A rollback failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_pin_active(runtime, &rollback_pin, &pinned_generation);
  if (!require(status == GLAMIN_STATUS_OK, "rollback pin failed") ||
      !require(pinned_generation == generation_a,
               "rollback pin resolved the wrong generation") ||
      !search_one(runtime, rollback_pin, query, 0, 8.02F)) {
    return EXIT_FAILURE;
  }

  status = glamin_generation_deactivate(runtime);
  if (!require(status == GLAMIN_STATUS_OK, "generation deactivation failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_retire(runtime, generation_a);
  if (!require(status == GLAMIN_STATUS_OK, "generation A retirement failed") ||
      !search_one(runtime, pin_a, query, 0, 8.02F)) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_activate(runtime, generation_a);
  if (!require(status == GLAMIN_STATUS_NOT_READY,
               "retired generation activation must fail")) {
    return EXIT_FAILURE;
  }

  status = glamin_generation_unpin(runtime, rollback_pin);
  if (!require(status == GLAMIN_STATUS_OK, "rollback unpin failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_unpin(runtime, pin_a);
  if (!require(status == GLAMIN_STATUS_OK, "generation A unpin failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_unpin(runtime, pin_a);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "stale generation pin must be rejected")) {
    return EXIT_FAILURE;
  }

  status = glamin_generation_retire(runtime, generation_b);
  if (!require(status == GLAMIN_STATUS_OK, "generation B retirement failed") ||
      !search_one(runtime, pin_b, query, 1, 0.02F)) {
    return EXIT_FAILURE;
  }
  status = glamin_generation_unpin(runtime, pin_b);
  if (!require(status == GLAMIN_STATUS_OK, "generation B unpin failed")) {
    return EXIT_FAILURE;
  }

  status = glamin_index_destroy(runtime, index_a);
  if (!require(status == GLAMIN_STATUS_OK, "generation A index destruction failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_index_destroy(runtime, index_b);
  if (!require(status == GLAMIN_STATUS_OK, "generation B index destruction failed")) {
    return EXIT_FAILURE;
  }
  status = glamin_runtime_destroy(runtime);
  if (!require(status == GLAMIN_STATUS_OK, "runtime destruction failed")) {
    return EXIT_FAILURE;
  }

  puts("c generation abi smoke ok");
  return EXIT_SUCCESS;
}
