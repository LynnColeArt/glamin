#include <stddef.h>

#include "glamin_cuda.h"

static glamin_cuda_ops glamin_cuda_ops_state;
static int32_t glamin_cuda_has_ops_state = 0;

int32_t glamin_cuda_register_ops(const glamin_cuda_ops *ops)
{
  if (!ops)
  {
    return GLAMIN_ERR_INVALID_ARG;
  }

  glamin_cuda_ops_state = *ops;
  glamin_cuda_has_ops_state = 1;
  return GLAMIN_OK;
}

int32_t glamin_cuda_has_ops(void)
{
  return glamin_cuda_has_ops_state;
}

const glamin_cuda_ops *glamin_cuda_get_ops(void)
{
  if (!glamin_cuda_has_ops_state)
  {
    return NULL;
  }
  return &glamin_cuda_ops_state;
}
