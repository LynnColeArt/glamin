#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "glamin_cuda.h"

static int32_t glamin_cuda_emulation_enabled(void)
{
  const char *value = getenv("GLAMIN_CUDA_AVAILABLE");
  if (!value || value[0] == '\0')
  {
    return 0;
  }

  switch (value[0])
  {
    case '1':
    case 't':
    case 'T':
    case 'y':
    case 'Y':
    case 'o':
    case 'O':
      return 1;
    default:
      return 0;
  }
}

void glamin_cuda_alloc(int64_t bytes, void **device_ptr, int32_t *status)
{
  if (!device_ptr)
  {
    if (status)
    {
      *status = GLAMIN_ERR_INVALID_ARG;
    }
    return;
  }

  *device_ptr = NULL;
  if (bytes <= 0)
  {
    if (status)
    {
      *status = GLAMIN_ERR_NOT_READY;
    }
    return;
  }

  {
    const glamin_cuda_ops *ops = glamin_cuda_get_ops();
    if (ops && ops->alloc)
    {
      int32_t result = ops->alloc(bytes, device_ptr);
      if (status)
      {
        *status = result;
      }
      if (result == GLAMIN_OK || !glamin_cuda_emulation_enabled())
      {
        return;
      }
    }
  }

  if (!glamin_cuda_emulation_enabled())
  {
    if (status)
    {
      *status = GLAMIN_ERR_NOT_READY;
    }
    return;
  }

  *device_ptr = malloc((size_t)bytes);
  if (!*device_ptr)
  {
    if (status)
    {
      *status = GLAMIN_ERR_NOT_READY;
    }
    return;
  }

  if (status)
  {
    *status = GLAMIN_OK;
  }
}

void glamin_cuda_free(void *device_ptr, int32_t *status)
{
  {
    const glamin_cuda_ops *ops = glamin_cuda_get_ops();
    if (ops && ops->free)
    {
      int32_t result = ops->free(device_ptr);
      if (status)
      {
        *status = result;
      }
      return;
    }
  }

  if (device_ptr)
  {
    free(device_ptr);
  }
  if (status)
  {
    *status = GLAMIN_OK;
  }
}

void glamin_cuda_upload(const void *host_ptr, void *device_ptr, int64_t bytes, int32_t *status)
{
  if (!host_ptr || !device_ptr || bytes <= 0)
  {
    if (status)
    {
      *status = GLAMIN_ERR_NOT_READY;
    }
    return;
  }

  {
    const glamin_cuda_ops *ops = glamin_cuda_get_ops();
    if (ops && ops->upload)
    {
      int32_t result = ops->upload(host_ptr, device_ptr, bytes);
      if (status)
      {
        *status = result;
      }
      if (result == GLAMIN_OK || !glamin_cuda_emulation_enabled())
      {
        return;
      }
    }
  }

  if (!glamin_cuda_emulation_enabled())
  {
    if (status)
    {
      *status = GLAMIN_ERR_NOT_READY;
    }
    return;
  }

  memcpy(device_ptr, host_ptr, (size_t)bytes);
  if (status)
  {
    *status = GLAMIN_OK;
  }
}

void glamin_cuda_download(const void *device_ptr, void *host_ptr, int64_t bytes, int32_t *status)
{
  if (!host_ptr || !device_ptr || bytes <= 0)
  {
    if (status)
    {
      *status = GLAMIN_ERR_NOT_READY;
    }
    return;
  }

  {
    const glamin_cuda_ops *ops = glamin_cuda_get_ops();
    if (ops && ops->download)
    {
      int32_t result = ops->download(device_ptr, host_ptr, bytes);
      if (status)
      {
        *status = result;
      }
      if (result == GLAMIN_OK || !glamin_cuda_emulation_enabled())
      {
        return;
      }
    }
  }

  if (!glamin_cuda_emulation_enabled())
  {
    if (status)
    {
      *status = GLAMIN_ERR_NOT_READY;
    }
    return;
  }

  memcpy(host_ptr, device_ptr, (size_t)bytes);
  if (status)
  {
    *status = GLAMIN_OK;
  }
}
