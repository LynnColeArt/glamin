#ifndef GLAMIN_CONTRACTS_H
#define GLAMIN_CONTRACTS_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void (*glamin_embedder_hash_validator_cb)(
    const char *embedder_id,
    const char *embedder_version,
    const char *input_schema,
    const char *preprocess_chain,
    const char *model_hash,
    const char *config_hash,
    const char *hardware_class,
    int32_t min_ram_mb,
    int32_t min_vram_mb,
    const char *contract_hash,
    int32_t *status);

typedef void (*glamin_embedder_signature_validator_cb)(
    const char *embedder_id,
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
    int32_t *status);

void glamin_set_contract_validators(glamin_embedder_hash_validator_cb hash_cb,
                                    glamin_embedder_signature_validator_cb signature_cb);

void glamin_clear_contract_validators(void);

#ifdef __cplusplus
}
#endif

#endif
