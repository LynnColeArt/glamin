program ivfpq_demo
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK
  use glamin_index_ivfpq, only: IvfProductQuantizerIndex, ivfpq_add, ivfpq_create, &
    ivfpq_destroy, ivfpq_search, ivfpq_train
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_L2
  use glamin_types, only: VectorBlock
  implicit none

  integer(int32), parameter :: DIM = 4_int32
  integer(int32), parameter :: NLIST = 2_int32
  integer(int32), parameter :: M = 2_int32
  integer(int32), parameter :: KSUB = 8_int32
  integer(int32), parameter :: K = 3_int32
  integer(int32), parameter :: NPROBE = 2_int32
  integer(int64), parameter :: VECTOR_COUNT = 6_int64
  integer(int64), parameter :: QUERY_COUNT = 2_int64
  integer(c_size_t), parameter :: ALIGN_BYTES = 64_c_size_t

  type(IvfProductQuantizerIndex) :: index
  type(VectorBlock) :: vectors
  type(VectorBlock) :: queries
  type(VectorBlock) :: distances
  type(VectorBlock) :: labels
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

  call ivfpq_create(index, DIM, NLIST, M, KSUB, METRIC_L2, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to create IVFPQ index"
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
    0.0_real32, 0.0_real32, 0.0_real32, 0.0_real32, &
    1.0_real32, 0.0_real32, 0.0_real32, 0.0_real32, &
    0.0_real32, 1.0_real32, 0.0_real32, 0.0_real32, &
    0.0_real32, 0.0_real32, 1.0_real32, 0.0_real32, &
    0.0_real32, 0.0_real32, 0.0_real32, 1.0_real32, &
    1.0_real32, 1.0_real32, 1.0_real32, 1.0_real32  &
  ]

  call c_f_pointer(query_ptr, query_data, [int(DIM, int64) * QUERY_COUNT])
  query_data = [ &
    0.1_real32, 0.0_real32, 0.0_real32, 0.0_real32, &
    0.0_real32, 0.9_real32, 0.0_real32, 0.0_real32  &
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

  call ivfpq_train(index, vectors, status)
  if (status /= GLAMIN_OK) then
    error stop "IVFPQ train failed"
  end if

  call ivfpq_add(index, vectors, status)
  if (status /= GLAMIN_OK) then
    error stop "IVFPQ add failed"
  end if

  call ivfpq_search(index, queries, K, NPROBE, distances, labels, status)
  if (status /= GLAMIN_OK) then
    error stop "IVFPQ search failed"
  end if

  call c_f_pointer(distances%data, distance_data, [int(K, int64) * QUERY_COUNT])
  call c_f_pointer(labels%data, label_data, [int(K, int64) * QUERY_COUNT])

  do query_index = 1_int64, QUERY_COUNT
    offset = (query_index - 1_int64) * K
    write (*, "(a,i0)") "Query ", query_index
    do neighbor_index = 1_int32, K
      write (*, "(a,i0,a,f8.4)") "  label ", label_data(offset + neighbor_index), &
        " dist ", distance_data(offset + neighbor_index)
    end do
  end do

  call free_aligned(distances%data, free_status)
  call free_aligned(labels%data, free_status)
  call free_aligned(vector_ptr, free_status)
  call free_aligned(query_ptr, free_status)
  call ivfpq_destroy(index, status)
end program ivfpq_demo
