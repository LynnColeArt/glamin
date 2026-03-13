#include <stdio.h>

#include "glamin_contracts.h"

static void validate_hash(const char *embedder_id,
                          const char *embedder_version,
                          const char *input_schema,
                          const char *preprocess_chain,
                          const char *model_hash,
                          const char *config_hash,
                          const char *hardware_class,
                          int32_t min_ram_mb,
                          int32_t min_vram_mb,
                          const char *contract_hash,
                          int32_t *status) {
  (void)embedder_id;
  (void)embedder_version;
  (void)input_schema;
  (void)preprocess_chain;
  (void)model_hash;
  (void)config_hash;
  (void)hardware_class;
  (void)min_ram_mb;
  (void)min_vram_mb;
  (void)contract_hash;

  if (status) {
    *status = 0;
  }
}

static void validate_signature(const char *embedder_id,
                               const char *embedder_version,
                               const char *input_schema,
                               const char *preprocess_chain,
                               const char *model_hash,
                               const char *config_hash,
                               const char *hardware_class,
                               int32_t min_ram_mb,
                               int32_t min_vram_mb,
                               const char *contract_hash,
                               const char *signature,
                               int32_t *status) {
  (void)embedder_id;
  (void)embedder_version;
  (void)input_schema;
  (void)preprocess_chain;
  (void)model_hash;
  (void)config_hash;
  (void)hardware_class;
  (void)min_ram_mb;
  (void)min_vram_mb;
  (void)contract_hash;
  (void)signature;

  if (status) {
    *status = 0;
  }
}

int main(void) {
  glamin_set_contract_validators(validate_hash, validate_signature);
  printf("Contract validators registered. Loader will invoke them on validation.\n");
  glamin_clear_contract_validators();
  return 0;
}
