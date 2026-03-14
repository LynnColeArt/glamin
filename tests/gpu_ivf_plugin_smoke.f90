program glamin_gpu_ivf_plugin_smoke
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_loc
  use glamin_cuda_backend, only: CudaBackend
  use glamin_cuda_ops, only: cuda_has_ops, cuda_load_ops, cuda_unload_ops
  use glamin_gpu_backend, only: gpu_auto_select_backend, gpu_register_backend
  use glamin_index_ivf, only: IvfIndex, ivf_add, ivf_create, ivf_search, ivf_train
  use glamin_metrics, only: METRIC_L2
  use glamin_types, only: VectorBlock
  implicit none

  type(IvfIndex) :: index
  type(VectorBlock) :: vectors
  type(VectorBlock) :: queries
  type(VectorBlock) :: distances
  type(VectorBlock) :: labels
  type(CudaBackend) :: cuda_backend
  real(real32), target :: vector_data(6)
  real(real32), target :: query_data(6)
  integer(int32), pointer :: label_ptr(:)
  integer(int32) :: status
  integer(int32) :: elem_bytes
  integer :: argc
  character(len=512) :: plugin_path

  argc = command_argument_count()
  if (argc < 1) error stop "plugin path required"
  call get_command_argument(1, plugin_path)
  if (len_trim(plugin_path) == 0) error stop "plugin path required"

  call cuda_load_ops(trim(plugin_path), status)
  if (status /= 0_int32) error stop "cuda_load_ops failed"
  if (.not. cuda_has_ops()) error stop "cuda ops missing"

  elem_bytes = int(storage_size(0.0_real32) / 8, int32)

  vector_data = [0.0_real32, 0.0_real32, 1.0_real32, 1.0_real32, 2.0_real32, 2.0_real32]
  query_data = [0.1_real32, 0.1_real32, 1.2_real32, 1.1_real32, 2.1_real32, 2.0_real32]

  vectors = VectorBlock()
  vectors%data = c_loc(vector_data(1))
  vectors%length = 3_int64
  vectors%dim = 2
  vectors%stride = 2
  vectors%elem_size = elem_bytes

  queries = VectorBlock()
  queries%data = c_loc(query_data(1))
  queries%length = 3_int64
  queries%dim = 2
  queries%stride = 2
  queries%elem_size = elem_bytes

  call ivf_create(index, 2_int32, 1_int32, METRIC_L2, status)
  if (status /= 0_int32) error stop "ivf_create failed"
  call ivf_train(index, vectors, status)
  if (status /= 0_int32) error stop "ivf_train failed"
  call ivf_add(index, vectors, status)
  if (status /= 0_int32) error stop "ivf_add failed"

  call gpu_register_backend('cuda', cuda_backend, status)
  if (status /= 0_int32) error stop "gpu_register_backend failed"
  call gpu_auto_select_backend(status)
  if (status /= 0_int32) error stop "gpu_auto_select_backend failed"

  call ivf_search(index, queries, 1_int32, 1_int32, distances, labels, status)
  if (status /= 0_int32) error stop "ivf_search failed"

  call c_f_pointer(labels%data, label_ptr, [3])
  if (label_ptr(1) /= 1_int32) error stop "label 1 mismatch"
  if (label_ptr(2) /= 2_int32) error stop "label 2 mismatch"
  if (label_ptr(3) /= 3_int32) error stop "label 3 mismatch"

  call cuda_unload_ops(status)
  if (status /= 0_int32) error stop "cuda_unload_ops failed"

  write (*, '(a)') 'gpu ivf plugin smoke ok'
end program glamin_gpu_ivf_plugin_smoke
