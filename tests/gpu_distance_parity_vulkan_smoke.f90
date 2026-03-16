program glamin_gpu_distance_parity_vulkan_smoke
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_loc
  use glamin_distance, only: distance_ip_batch, distance_l2_batch
  use glamin_errors, only: GLAMIN_OK
  use glamin_gpu_backend, only: GPU_BACKEND_NAME_LEN, gpu_clear_backend, &
    gpu_distance_ip_dispatch, gpu_distance_l2_dispatch, gpu_get_backend_name, &
    gpu_register_backend, gpu_select_backend
  use glamin_memory, only: free_aligned
  use glamin_types, only: VectorBlock
  use glamin_vulkan_backend, only: VulkanBackend
  implicit none

  type(VulkanBackend) :: vulkan_backend
  real(real32), target :: vector_data(6)
  real(real32), target :: query_data(4)
  type(VectorBlock) :: vectors
  type(VectorBlock) :: queries
  type(VectorBlock) :: cpu_distances
  type(VectorBlock) :: gpu_distances
  real(real32), pointer :: cpu_ptr(:)
  real(real32), pointer :: gpu_ptr(:)
  integer(int32) :: elem_bytes
  integer(int32) :: status
  integer(int32) :: free_status
  integer(int64) :: distance_count
  character(len=GPU_BACKEND_NAME_LEN) :: backend_name
  real(real32), parameter :: tol = 1.0e-6_real32

  elem_bytes = int(storage_size(0.0_real32) / 8, int32)

  vector_data = [0.0_real32, 0.0_real32, 1.0_real32, 0.0_real32, 0.0_real32, 1.0_real32]
  query_data = [0.0_real32, 0.0_real32, 1.0_real32, 1.0_real32]

  vectors = VectorBlock()
  vectors%data = c_loc(vector_data(1))
  vectors%length = 3_int64
  vectors%dim = 2
  vectors%stride = 2
  vectors%elem_size = elem_bytes

  queries = VectorBlock()
  queries%data = c_loc(query_data(1))
  queries%length = 2_int64
  queries%dim = 2
  queries%stride = 2
  queries%elem_size = elem_bytes

  call gpu_register_backend('vulkan', vulkan_backend, status)
  if (status /= GLAMIN_OK) error stop "gpu_register_backend failed"
  call gpu_select_backend('vulkan', status)
  if (status /= GLAMIN_OK) error stop "gpu_select_backend failed"
  call gpu_get_backend_name(backend_name)
  if (trim(backend_name) /= 'vulkan') error stop "unexpected backend"

  distance_count = vectors%length * queries%length

  cpu_distances = VectorBlock()
  call distance_l2_batch(queries, vectors, cpu_distances, status)
  if (status /= GLAMIN_OK) error stop "distance_l2_batch failed"

  gpu_distances = VectorBlock()
  call gpu_distance_l2_dispatch(queries, vectors, gpu_distances, status)
  if (status /= GLAMIN_OK) error stop "gpu_distance_l2_dispatch failed"

  call c_f_pointer(cpu_distances%data, cpu_ptr, [int(distance_count)])
  call c_f_pointer(gpu_distances%data, gpu_ptr, [int(distance_count)])
  if (any(abs(cpu_ptr - gpu_ptr) > tol)) error stop "L2 parity mismatch"

  call free_aligned(cpu_distances%data, free_status)
  call free_aligned(gpu_distances%data, free_status)

  cpu_distances = VectorBlock()
  call distance_ip_batch(queries, vectors, cpu_distances, status)
  if (status /= GLAMIN_OK) error stop "distance_ip_batch failed"

  gpu_distances = VectorBlock()
  call gpu_distance_ip_dispatch(queries, vectors, gpu_distances, status)
  if (status /= GLAMIN_OK) error stop "gpu_distance_ip_dispatch failed"

  call c_f_pointer(cpu_distances%data, cpu_ptr, [int(distance_count)])
  call c_f_pointer(gpu_distances%data, gpu_ptr, [int(distance_count)])
  if (any(abs(cpu_ptr - gpu_ptr) > tol)) error stop "IP parity mismatch"

  call free_aligned(cpu_distances%data, free_status)
  call free_aligned(gpu_distances%data, free_status)
  call gpu_clear_backend(status)

  write (*, '(a)') 'gpu distance parity vulkan ok'
end program glamin_gpu_distance_parity_vulkan_smoke
