program pq_demo
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK
  use glamin_index_pq, only: pq_add, pq_create_handle, pq_destroy_handle, pq_search, &
    pq_set_codebooks
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_L2
  use glamin_types, only: IndexHandle, VectorBlock
  implicit none

  integer(int32), parameter :: DIM = 2_int32
  integer(int32), parameter :: M = 1_int32
  integer(int32), parameter :: NBITS = 8_int32
  integer(int32), parameter :: K = 2_int32
  integer(int64), parameter :: VECTOR_COUNT = 4_int64
  integer(int64), parameter :: QUERY_COUNT = 2_int64
  integer(int32), parameter :: KSUB = 256_int32
  integer(c_size_t), parameter :: ALIGN_BYTES = 64_c_size_t

  type(IndexHandle) :: index
  type(VectorBlock) :: codebooks
  type(VectorBlock) :: vectors
  type(VectorBlock) :: queries
  type(VectorBlock) :: distances
  type(VectorBlock) :: labels
  integer(int32) :: status
  integer(int32) :: elem_bytes
  integer(int32) :: free_status
  integer(int64) :: total_codebook_entries
  integer(int32) :: centroid_index
  integer(int64) :: query_index
  integer(int32) :: neighbor_index
  integer(int64) :: offset
  type(c_ptr) :: codebook_ptr
  type(c_ptr) :: vector_ptr
  type(c_ptr) :: query_ptr
  real(real32), pointer :: codebook_data(:)
  real(real32), pointer :: vector_data(:)
  real(real32), pointer :: query_data(:)
  real(real32), pointer :: distance_data(:)
  integer(int32), pointer :: label_data(:)
  real(real32) :: centroid_value

  elem_bytes = int(storage_size(0.0_real32) / 8, int32)

  call pq_create_handle(index, DIM, M, NBITS, METRIC_L2, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to create PQ index"
  end if

  total_codebook_entries = int(KSUB, int64) * int(DIM, int64)
  call allocate_aligned(codebook_ptr, total_codebook_entries * elem_bytes, ALIGN_BYTES, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to allocate codebooks"
  end if
  call c_f_pointer(codebook_ptr, codebook_data, [total_codebook_entries])

  do centroid_index = 0_int32, KSUB - 1_int32
    centroid_value = real(centroid_index, real32) / real(KSUB - 1_int32, real32)
    codebook_data(centroid_index * DIM + 1_int32) = centroid_value
    codebook_data(centroid_index * DIM + 2_int32) = centroid_value
  end do

  codebooks%data = codebook_ptr
  codebooks%length = KSUB
  codebooks%dim = DIM
  codebooks%stride = DIM
  codebooks%elem_size = elem_bytes
  codebooks%alignment = int(ALIGN_BYTES, int32)

  call pq_set_codebooks(index, codebooks, status)
  if (status /= GLAMIN_OK) then
    error stop "Failed to set PQ codebooks"
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

  call pq_add(index, vectors, status)
  if (status /= GLAMIN_OK) then
    error stop "PQ add failed"
  end if

  call pq_search(index, queries, K, distances, labels, status)
  if (status /= GLAMIN_OK) then
    error stop "PQ search failed"
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
  call free_aligned(codebook_ptr, free_status)
  call pq_destroy_handle(index, status)
end program pq_demo
