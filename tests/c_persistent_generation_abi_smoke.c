#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

#include "glamin_runtime.h"

static int require(int condition, const char *message) {
  if (!condition) {
    fprintf(stderr, "c persistent generation abi smoke failed: %s\n", message);
    return 0;
  }
  return 1;
}

static int artifact_file(char *output,
                         size_t capacity,
                         const char *directory,
                         const char *name) {
  int length = snprintf(output, capacity, "%s/%s", directory, name);
  return length > 0 && (size_t)length < capacity;
}

static int write_layout(const char *directory) {
  char path[4096];
  FILE *file;

  if (!artifact_file(path, sizeof(path), directory, "vector_layout.json")) {
    return 0;
  }
  file = fopen(path, "wb");
  if (!file) {
    return 0;
  }
  fputs("{\"dtype\": \"float32\",\"endianness\": \"little\","
        "\"total_vectors\": 2,\"total_bytes\": 16,\"spaces\":[{"
        "\"space_id\": \"fixture.space\",\"dim\": 2,\"count\": 2,"
        "\"byte_stride\": 8,\"offset_bytes\": 0}]}\n",
        file);
  return fclose(file) == 0;
}

static int write_vectors(const char *directory) {
  const float vectors[] = {0.0F, 0.0F, 2.0F, 2.0F};
  char path[4096];
  FILE *file;
  size_t written;

  if (!artifact_file(path, sizeof(path), directory, "vectors.bin")) {
    return 0;
  }
  file = fopen(path, "wb");
  if (!file) {
    return 0;
  }
  written = fwrite(vectors, sizeof(float), 4, file);
  return fclose(file) == 0 && written == 4;
}

static int write_contracts(const char *directory, int valid_hash) {
  const char *space_hash = valid_hash
      ? "sha256:c761b2699931aba1583df083fa99155e82b3ad08545bcb2e2ce3521034f80f88"
      : "sha256:0761b2699931aba1583df083fa99155e82b3ad08545bcb2e2ce3521034f80f88";
  char path[4096];
  FILE *file;

  if (!artifact_file(path, sizeof(path), directory, "contracts.json")) {
    return 0;
  }
  file = fopen(path, "wb");
  if (!file) {
    return 0;
  }
  fprintf(file,
          "{\"spec_id\": \"fixture\",\"embedder\": {\"spec\": {"
          "\"id\": \"fixture\",\"version\": \"1\","
          "\"input_schema\": \"f32\",\"preprocess_chain\": [\"none\"],"
          "\"model_hash\": \"sha256:model\","
          "\"config_hash\": \"sha256:config\","
          "\"hardware_class\": \"cpu\",\"min_ram_mb\": 0,"
          "\"min_vram_mb\": 0},\"contract_hash\": \"sha256:embed\","
          "\"signature\": \"\"},\"spaces\":[{\"spec\": {"
          "\"space_id\": \"fixture.space\",\"dim\": 2,"
          "\"metric\": \"l2\",\"normalization\": \"none\"},"
          "\"contract_hash\": \"%s\",\"signature\": \"\"}]}\n",
          space_hash);
  return fclose(file) == 0;
}

static void clean_artifact(const char *directory) {
  static const char *names[] = {
      "vector_layout.json", "vectors.bin", "contracts.json"};
  char path[4096];
  size_t index;

  for (index = 0; index < sizeof(names) / sizeof(names[0]); ++index) {
    if (artifact_file(path, sizeof(path), directory, names[index])) {
      (void)remove(path);
    }
  }
  (void)rmdir(directory);
}

int main(int argc, char **argv) {
  const float query[] = {1.9F, 2.1F};
  const char label[] = "persistent-a";
  const char space_id[] = "fixture.space";
  char artifact_directory[4096];
  char vectors_path[4096];
  int directory_length;
  glamin_runtime_t runtime = 0;
  glamin_index_t index = 0;
  glamin_generation_t generation = 0;
  glamin_generation_t pinned_generation = 0;
  glamin_generation_pin_t pin = 0;
  uint32_t dimension = 0;
  uint64_t vector_count = 0;
  float distance = 0.0F;
  uint64_t result_label = 0;
  glamin_status status;

  directory_length = argc == 2
                         ? snprintf(artifact_directory,
                                    sizeof(artifact_directory),
                                    "%s/c_persistent_artifact",
                                    argv[1])
                         : -1;
  if (directory_length <= 0 ||
      (size_t)directory_length >= sizeof(artifact_directory)) {
    return EXIT_FAILURE;
  }
  clean_artifact(artifact_directory);
  if (mkdir(artifact_directory, 0700) != 0 && errno != EEXIST) {
    return EXIT_FAILURE;
  }
  if (!write_layout(artifact_directory) || !write_vectors(artifact_directory) ||
      !write_contracts(artifact_directory, 1)) {
    clean_artifact(artifact_directory);
    return EXIT_FAILURE;
  }

  status = glamin_runtime_create(2, &runtime);
  if (!require(status == GLAMIN_STATUS_OK, "runtime creation failed")) {
    clean_artifact(artifact_directory);
    return EXIT_FAILURE;
  }
  status = glamin_flat_index_load_artifact(
      runtime,
      artifact_directory,
      strlen(artifact_directory),
      space_id,
      sizeof(space_id) - 1,
      GLAMIN_METRIC_L2,
      &index,
      &dimension,
      &vector_count);
  if (!require(status == GLAMIN_STATUS_OK, "valid artifact load failed") ||
      !require(dimension == 2, "loaded dimension mismatch") ||
      !require(vector_count == 2, "loaded vector count mismatch")) {
    clean_artifact(artifact_directory);
    return EXIT_FAILURE;
  }

  status = glamin_generation_create(
      runtime, index, label, sizeof(label) - 1, &generation);
  status = status == GLAMIN_STATUS_OK
               ? glamin_generation_activate(runtime, generation)
               : status;
  status = status == GLAMIN_STATUS_OK
               ? glamin_generation_pin_active(runtime, &pin, &pinned_generation)
               : status;
  status = status == GLAMIN_STATUS_OK
               ? glamin_generation_search_f32(
                     runtime, pin, query, 1, 2, 1, &distance, &result_label)
               : status;
  if (!require(status == GLAMIN_STATUS_OK, "persistent pinned search failed") ||
      !require(pinned_generation == generation, "persistent pin mismatch") ||
      !require(result_label == 1, "persistent search label mismatch") ||
      !require(distance > 0.019F && distance < 0.021F,
               "persistent search distance mismatch")) {
    clean_artifact(artifact_directory);
    return EXIT_FAILURE;
  }

  dimension = 99;
  vector_count = 99;
  status = glamin_flat_index_load_artifact(
      runtime,
      artifact_directory,
      strlen(artifact_directory),
      "missing.space",
      strlen("missing.space"),
      GLAMIN_METRIC_L2,
      &result_label,
      &dimension,
      &vector_count);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "missing space contract was accepted") ||
      !require(result_label == 0 && dimension == 0 && vector_count == 0,
               "failed load did not clear its outputs")) {
    clean_artifact(artifact_directory);
    return EXIT_FAILURE;
  }

  status = glamin_flat_index_load_artifact(
      runtime,
      artifact_directory,
      strlen(artifact_directory),
      space_id,
      sizeof(space_id) - 1,
      GLAMIN_METRIC_INNER_PRODUCT,
      &result_label,
      &dimension,
      &vector_count);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "mismatched space metric was accepted")) {
    clean_artifact(artifact_directory);
    return EXIT_FAILURE;
  }

  if (!write_contracts(artifact_directory, 0)) {
    clean_artifact(artifact_directory);
    return EXIT_FAILURE;
  }
  status = glamin_flat_index_load_artifact(
      runtime,
      artifact_directory,
      strlen(artifact_directory),
      space_id,
      sizeof(space_id) - 1,
      GLAMIN_METRIC_L2,
      &result_label,
      &dimension,
      &vector_count);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "invalid space contract hash was accepted")) {
    clean_artifact(artifact_directory);
    return EXIT_FAILURE;
  }

  if (!write_contracts(artifact_directory, 1) ||
      !artifact_file(vectors_path,
                     sizeof(vectors_path),
                     artifact_directory,
                     "vectors.bin") ||
      remove(vectors_path) != 0) {
    clean_artifact(artifact_directory);
    return EXIT_FAILURE;
  }
  status = glamin_flat_index_load_artifact(
      runtime,
      artifact_directory,
      strlen(artifact_directory),
      space_id,
      sizeof(space_id) - 1,
      GLAMIN_METRIC_L2,
      &result_label,
      &dimension,
      &vector_count);
  if (!require(status == GLAMIN_STATUS_INVALID_ARGUMENT,
               "missing vector file did not fail safely")) {
    clean_artifact(artifact_directory);
    return EXIT_FAILURE;
  }

  status = glamin_generation_deactivate(runtime);
  status = status == GLAMIN_STATUS_OK
               ? glamin_generation_retire(runtime, generation)
               : status;
  status = status == GLAMIN_STATUS_OK ? glamin_generation_unpin(runtime, pin) : status;
  status = status == GLAMIN_STATUS_OK ? glamin_index_destroy(runtime, index) : status;
  status = status == GLAMIN_STATUS_OK ? glamin_runtime_destroy(runtime) : status;
  clean_artifact(artifact_directory);
  if (!require(status == GLAMIN_STATUS_OK, "persistent resource cleanup failed")) {
    return EXIT_FAILURE;
  }

  puts("c persistent generation abi smoke ok");
  return EXIT_SUCCESS;
}
