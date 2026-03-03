program async_flat_demo
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_ptr, c_size_t
  use glamin_async, only: get_search_results, wait_request
  use glamin_errors, only: GLAMIN_OK
  use glamin_index_flat, only: flat_create_handle, flat_destroy_handle
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_L2
  use glamin_runtime, only: RuntimeContext, start_runtime, stop_runtime, &
    submit_add_async, submit_search_async
  use glamin_status, only: REQUEST_COMPLETED
  use glamin_types, only: IndexHandle, Request, SearchPlan, VectorBlock
  implicit none

  integer(int32), parameter :: DIM = 2_int32
  integer(int64), parameter :: VECTOR_COUNT = 4_int64
  integer(int64), parameter :: QUERY_COUNT = 2_int64
  integer(c_size_t), parameter :: ALIGN_BYTES = 64_c_size_t

  type(RuntimeContext) :: runtime
  type(IndexHandle) :: index
  type(VectorBlock) :: vectors
  type(VectorBlock) :: queries
  type(VectorBlock) :: distances
  type(VectorBlock) :: labels
  type(SearchPlan) :: plan
  type(Request) :: add_request
  type(Request) :: search_request
  integer(int32) :: status
  integer(int32) :: elem_bytes
  integer(int32) :: free_status
  integer(int64) :: query_index
  integer(int32) :: neighbor_index
  integer(int64) :: offset
  type(c_ptr) :: vector_ptr
  type(c_ptr) :: query_ptr
  real(real32), pointer :: vector_data(:)
  real(real32), pointer :: query_data(:)
  real(real32), pointer :: distance_data(:)
  integer(int32), pointer :: label_data(:)

  elem_bytes = int(storage_size(0.0_real32) / 8, int32)

  call flat_create_handle(index, DIM, METRIC_L2, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to create index"
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
    1.0_real32, 0.0_real32, &
    0.0_real32, 1.0_real32, &
    1.0_real32, 1.0_real32  &
  ]

  call c_f_pointer(query_ptr, query_data, [int(DIM, int64) * QUERY_COUNT])
  query_data = [ &
    0.0_real32, 0.2_real32, &
    1.0_real32, 0.8_real32  &
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

  call submit_add_async(runtime, index, vectors, add_request, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to submit add request"
  end if
  call wait_request(add_request, status)
  if (add_request%status /= REQUEST_COMPLETED .or. add_request%error_code /= GLAMIN_OK) then
    error stop "Add request failed"
  end if

  plan%k = 2_int32
  plan%nprobe = 0_int32
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

  call c_f_pointer(distances%data, distance_data, [int(plan%k, int64) * QUERY_COUNT])
  call c_f_pointer(labels%data, label_data, [int(plan%k, int64) * QUERY_COUNT])

  do query_index = 1_int64, QUERY_COUNT
    offset = (query_index - 1_int64) * plan%k
    write (*, "(a,i0)") "Query ", query_index
    do neighbor_index = 1_int32, plan%k
      write (*, "(a,i0,a,f8.4)") "  label ", label_data(offset + neighbor_index), &
        " dist ", distance_data(offset + neighbor_index)
    end do
  end do

  call free_aligned(distances%data, free_status)
  call free_aligned(labels%data, free_status)
  call free_aligned(vector_ptr, free_status)
  call free_aligned(query_ptr, free_status)
  call stop_runtime(runtime, status)
  call flat_destroy_handle(index, status)
end program async_flat_demo
