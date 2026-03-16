program glamin_gpu_hnsw_parity_smoke
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_loc
  use glamin_cuda_backend, only: CudaBackend
  use glamin_cuda_ops, only: cuda_register_stub_ops
  use glamin_errors, only: GLAMIN_OK
  use glamin_gpu_backend, only: gpu_clear_backend, gpu_register_backend, gpu_select_backend
  use glamin_index_hnsw, only: HnswIndex, hnsw_add, hnsw_create, hnsw_search
  use glamin_memory, only: free_aligned
  use glamin_metrics, only: METRIC_L2
  use glamin_types, only: VectorBlock
  implicit none

  type(HnswIndex) :: index
  type(VectorBlock) :: vectors
  type(VectorBlock) :: queries
  type(VectorBlock) :: cpu_distances
  type(VectorBlock) :: cpu_labels
  type(VectorBlock) :: gpu_distances
  type(VectorBlock) :: gpu_labels
  type(CudaBackend) :: cuda_backend
  real(real32), target :: vector_data(8)
  real(real32), target :: query_data(4)
  real(real32), pointer :: cpu_dist_ptr(:)
  real(real32), pointer :: gpu_dist_ptr(:)
  integer(int32), pointer :: cpu_label_ptr(:)
  integer(int32), pointer :: gpu_label_ptr(:)
  integer(int32) :: status
  integer(int32) :: free_status
  integer(int32) :: elem_bytes
  integer(int64) :: result_count
  integer(int32), parameter :: neighbor_limit = 2_int32
  integer(int32), parameter :: construction_ef = 4_int32
  integer(int32), parameter :: search_ef = 4_int32
  integer(int32), parameter :: result_k = 2_int32
  real(real32), parameter :: tol = 1.0e-6_real32

  elem_bytes = int(storage_size(0.0_real32) / 8, int32)

  vector_data = [0.0_real32, 0.0_real32, 1.0_real32, 1.0_real32, 2.0_real32, 2.0_real32, &
    3.0_real32, 3.0_real32]
  query_data = [0.1_real32, 0.1_real32, 2.1_real32, 2.0_real32]

  vectors = VectorBlock()
  vectors%data = c_loc(vector_data(1))
  vectors%length = 4_int64
  vectors%dim = 2
  vectors%stride = 2
  vectors%elem_size = elem_bytes

  queries = VectorBlock()
  queries%data = c_loc(query_data(1))
  queries%length = 2_int64
  queries%dim = 2
  queries%stride = 2
  queries%elem_size = elem_bytes

  call hnsw_create(index, 2_int32, neighbor_limit, construction_ef, METRIC_L2, status)
  if (status /= GLAMIN_OK) error stop "hnsw_create failed"
  call hnsw_add(index, vectors, status)
  if (status /= GLAMIN_OK) error stop "hnsw_add failed"

  call gpu_clear_backend(status)

  cpu_distances = VectorBlock()
  cpu_labels = VectorBlock()
  call hnsw_search(index, queries, result_k, search_ef, cpu_distances, cpu_labels, status)
  if (status /= GLAMIN_OK) error stop "hnsw_search cpu failed"

  result_count = queries%length * result_k
  call c_f_pointer(cpu_labels%data, cpu_label_ptr, [int(result_count)])

  call cuda_register_stub_ops(status)
  if (status /= GLAMIN_OK) error stop "cuda_register_stub_ops failed"
  call gpu_register_backend('cuda', cuda_backend, status)
  if (status /= GLAMIN_OK) error stop "gpu_register_backend failed"
  call gpu_select_backend('cuda', status)
  if (status /= GLAMIN_OK) error stop "gpu_select_backend failed"

  gpu_distances = VectorBlock()
  gpu_labels = VectorBlock()
  call hnsw_search(index, queries, result_k, search_ef, gpu_distances, gpu_labels, status)
  if (status /= GLAMIN_OK) error stop "hnsw_search gpu failed"

  call c_f_pointer(cpu_distances%data, cpu_dist_ptr, [int(result_count)])
  call c_f_pointer(gpu_distances%data, gpu_dist_ptr, [int(result_count)])
  if (any(abs(cpu_dist_ptr - gpu_dist_ptr) > tol)) then
    error stop "distance parity mismatch"
  end if

  call c_f_pointer(gpu_labels%data, gpu_label_ptr, [int(result_count)])
  if (any(cpu_label_ptr /= gpu_label_ptr)) then
    error stop "label parity mismatch"
  end if

  call free_aligned(cpu_distances%data, free_status)
  call free_aligned(cpu_labels%data, free_status)
  call free_aligned(gpu_distances%data, free_status)
  call free_aligned(gpu_labels%data, free_status)
  call gpu_clear_backend(status)

  write (*, '(a)') 'gpu hnsw parity ok'
end program glamin_gpu_hnsw_parity_smoke
