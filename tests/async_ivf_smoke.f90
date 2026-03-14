program glamin_async_ivf_smoke
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_ptr, c_size_t
  use glamin_async, only: get_search_results, wait_request
  use glamin_errors, only: GLAMIN_OK
  use glamin_index_ivf, only: ivf_create_handle, ivf_destroy_handle
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_L2
  use glamin_runtime, only: RuntimeContext, start_runtime, stop_runtime, &
    submit_add_async, submit_search_async, submit_train_async
  use glamin_status, only: REQUEST_COMPLETED
  use glamin_types, only: IndexHandle, Request, SearchPlan, VectorBlock
  implicit none

  integer(int32), parameter :: DIM = 2_int32
  integer(int64), parameter :: VECTOR_COUNT = 3_int64
  integer(int64), parameter :: QUERY_COUNT = 3_int64
  integer(c_size_t), parameter :: ALIGN_BYTES = 64_c_size_t

  type(RuntimeContext) :: runtime
  type(IndexHandle) :: index
  type(VectorBlock) :: vectors
  type(VectorBlock) :: queries
  type(VectorBlock) :: distances
  type(VectorBlock) :: labels
  type(SearchPlan) :: plan
  type(Request) :: train_request
  type(Request) :: add_request
  type(Request) :: search_request
  integer(int32) :: status
  integer(int32) :: elem_bytes
  integer(int32) :: free_status
  type(c_ptr) :: vector_ptr
  type(c_ptr) :: query_ptr
  real(real32), pointer :: vector_data(:)
  real(real32), pointer :: query_data(:)
  integer(int32), pointer :: label_data(:)

  elem_bytes = int(storage_size(0.0_real32) / 8, int32)

  call ivf_create_handle(index, DIM, 1_int32, METRIC_L2, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to create IVF index"
  end if

  call start_runtime(runtime, 2_int32, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to start runtime"
  end if

  call allocate_aligned(vector_ptr, int(DIM, int64) * VECTOR_COUNT * elem_bytes, &
    ALIGN_BYTES, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to allocate vectors"
  end if

  call allocate_aligned(query_ptr, int(DIM, int64) * QUERY_COUNT * elem_bytes, &
    ALIGN_BYTES, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to allocate queries"
  end if

  call c_f_pointer(vector_ptr, vector_data, [int(DIM, int64) * VECTOR_COUNT])
  vector_data = [ &
    0.0_real32, 0.0_real32, &
    1.0_real32, 1.0_real32, &
    2.0_real32, 2.0_real32  &
  ]

  call c_f_pointer(query_ptr, query_data, [int(DIM, int64) * QUERY_COUNT])
  query_data = [ &
    0.1_real32, 0.1_real32, &
    1.2_real32, 1.0_real32, &
    2.1_real32, 2.0_real32  &
  ]

  vectors%data = vector_ptr
  vectors%length = VECTOR_COUNT
  vectors%dim = DIM
  vectors%stride = DIM
  vectors%elem_size = elem_bytes
  vectors%alignment = int(ALIGN_BYTES, int32)

  queries%data = query_ptr
  queries%length = QUERY_COUNT
  queries%dim = DIM
  queries%stride = DIM
  queries%elem_size = elem_bytes
  queries%alignment = int(ALIGN_BYTES, int32)

  call submit_train_async(runtime, index, vectors, train_request, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to submit train request"
  end if
  call wait_request(train_request, status)
  if (train_request%status /= REQUEST_COMPLETED .or. train_request%error_code /= GLAMIN_OK) then
    error stop "Train request failed"
  end if

  call submit_add_async(runtime, index, vectors, add_request, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to submit add request"
  end if
  call wait_request(add_request, status)
  if (add_request%status /= REQUEST_COMPLETED .or. add_request%error_code /= GLAMIN_OK) then
    error stop "Add request failed"
  end if

  plan%k = 1_int32
  plan%nprobe = 1_int32
  plan%batch_size = 0_int32
  plan%metric = METRIC_L2

  call submit_search_async(runtime, index, plan, queries, search_request, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to submit search request"
  end if
  call wait_request(search_request, status)
  if (search_request%status /= REQUEST_COMPLETED .or. search_request%error_code /= GLAMIN_OK) then
    error stop "Search request failed"
  end if

  call get_search_results(search_request, distances, labels, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to read search results"
  end if

  call c_f_pointer(labels%data, label_data, [int(plan%k, int64) * QUERY_COUNT])
  if (any(label_data /= [1_int32, 2_int32, 3_int32])) then
    error stop "Async IVF labels mismatch"
  end if

  call free_aligned(distances%data, free_status)
  call free_aligned(labels%data, free_status)
  call free_aligned(vector_ptr, free_status)
  call free_aligned(query_ptr, free_status)
  call stop_runtime(runtime, status)
  call ivf_destroy_handle(index, status)

  write (*, '(a)') 'async ivf smoke ok'
end program glamin_async_ivf_smoke
