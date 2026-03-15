program glamin_gpu_backend_select_smoke
  use iso_fortran_env, only: int32
  use glamin_cuda_backend, only: CudaBackend
  use glamin_gpu_backend, only: gpu_auto_select_backend, gpu_clear_backend, gpu_get_backend_name, &
    gpu_register_backend, GPU_BACKEND_NAME_LEN
  use glamin_vulkan_backend, only: VulkanBackend
  implicit none

  type(CudaBackend) :: cuda_backend
  type(VulkanBackend) :: vulkan_backend
  character(len=GPU_BACKEND_NAME_LEN) :: expected
  character(len=GPU_BACKEND_NAME_LEN) :: selected
  integer(int32) :: status
  integer :: argc

  argc = command_argument_count()
  if (argc < 1) error stop "expected backend argument"
  call get_command_argument(1, expected)

  call gpu_register_backend('cuda', cuda_backend, status)
  if (status /= 0_int32) error stop "gpu_register_backend cuda failed"
  call gpu_register_backend('vulkan', vulkan_backend, status)
  if (status /= 0_int32) error stop "gpu_register_backend vulkan failed"

  call gpu_auto_select_backend(status)
  if (status /= 0_int32) error stop "gpu_auto_select_backend failed"

  call gpu_get_backend_name(selected)
  if (trim(selected) /= trim(expected)) then
    error stop "gpu backend selection mismatch"
  end if

  call gpu_clear_backend(status)

  write (*, '(a)') 'gpu backend select ok'
end program glamin_gpu_backend_select_smoke
