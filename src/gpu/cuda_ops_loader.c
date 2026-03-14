#include <dlfcn.h>
#include <stddef.h>

#include "glamin_cuda.h"

static void *glamin_cuda_ops_handle = NULL;

int32_t glamin_cuda_load_ops(const char *path)
{
  if (!path || path[0] == '\0')
  {
    return GLAMIN_ERR_INVALID_ARG;
  }

  void *handle = dlopen(path, RTLD_NOW);
  if (!handle)
  {
    return GLAMIN_ERR_NOT_READY;
  }

  glamin_cuda_plugin_init_fn init_fn =
    (glamin_cuda_plugin_init_fn)dlsym(handle, "glamin_cuda_plugin_init");
  if (!init_fn)
  {
    dlclose(handle);
    return GLAMIN_ERR_NOT_READY;
  }

  glamin_cuda_ops ops = {0};
  int32_t status = init_fn(&ops);
  if (status != GLAMIN_OK)
  {
    dlclose(handle);
    return status;
  }

  status = glamin_cuda_register_ops(&ops);
  if (status != GLAMIN_OK)
  {
    dlclose(handle);
    return status;
  }

  if (glamin_cuda_ops_handle)
  {
    dlclose(glamin_cuda_ops_handle);
  }
  glamin_cuda_ops_handle = handle;
  return GLAMIN_OK;
}

int32_t glamin_cuda_unload_ops(void)
{
  if (glamin_cuda_ops_handle)
  {
    dlclose(glamin_cuda_ops_handle);
    glamin_cuda_ops_handle = NULL;
  }
  return GLAMIN_OK;
}
