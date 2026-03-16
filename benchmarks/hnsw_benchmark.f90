program glamin_hnsw_benchmark
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK
  use glamin_index_hnsw, only: HnswIndex, hnsw_add, hnsw_build_snapshot, hnsw_create, &
    hnsw_destroy, hnsw_search
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_L2
  use glamin_types, only: VectorBlock
  implicit none

  integer(int32), parameter :: DEFAULT_DIM = 128_int32
  integer(int64), parameter :: DEFAULT_VECTOR_COUNT = 4096_int64
  integer(int64), parameter :: DEFAULT_QUERY_COUNT = 256_int64
  integer(int32), parameter :: DEFAULT_M = 16_int32
  integer(int32), parameter :: DEFAULT_EF_CONSTRUCTION = 64_int32
  integer(int32), parameter :: DEFAULT_EF_SEARCH = 32_int32
  integer(int32), parameter :: DEFAULT_K = 10_int32
  integer(int32), parameter :: DEFAULT_ITERS = 3_int32
  integer(int32), parameter :: DEFAULT_SNAPSHOT = 1_int32
  integer(c_size_t), parameter :: ALIGN_BYTES = 64_c_size_t

  type(HnswIndex) :: index
  type(VectorBlock) :: vectors
  type(VectorBlock) :: queries
  type(VectorBlock) :: distances
  type(VectorBlock) :: labels
  real(real32), pointer :: vector_data(:)
  real(real32), pointer :: query_data(:)
  type(c_ptr) :: vector_ptr
  type(c_ptr) :: query_ptr
  integer(int32) :: dim
  integer(int64) :: vector_count
  integer(int64) :: query_count
  integer(int32) :: m
  integer(int32) :: ef_construction
  integer(int32) :: ef_search
  integer(int32) :: k
  integer(int32) :: iterations
  integer(int32) :: snapshot_flag
  integer(int32) :: elem_bytes
  integer(int32) :: status
  integer(int32) :: free_status

  call read_arguments(dim, vector_count, query_count, m, ef_construction, ef_search, k, &
    iterations, snapshot_flag)
  elem_bytes = int(storage_size(0.0_real32) / 8, int32)

  call allocate_aligned(vector_ptr, int(dim, int64) * vector_count * elem_bytes, &
    ALIGN_BYTES, status)
  if (status /= GLAMIN_OK) error stop "Failed to allocate vector buffer"

  call allocate_aligned(query_ptr, int(dim, int64) * query_count * elem_bytes, &
    ALIGN_BYTES, status)
  if (status /= GLAMIN_OK) error stop "Failed to allocate query buffer"

  call c_f_pointer(vector_ptr, vector_data, [int(dim, int64) * vector_count])
  call c_f_pointer(query_ptr, query_data, [int(dim, int64) * query_count])

  call random_seed()
  call random_number(vector_data)
  call random_number(query_data)

  vectors = VectorBlock()
  vectors%data = vector_ptr
  vectors%length = vector_count
  vectors%dim = dim
  vectors%stride = dim
  vectors%elem_size = elem_bytes
  vectors%alignment = int(ALIGN_BYTES, int32)

  queries = VectorBlock()
  queries%data = query_ptr
  queries%length = query_count
  queries%dim = dim
  queries%stride = dim
  queries%elem_size = elem_bytes
  queries%alignment = int(ALIGN_BYTES, int32)

  call hnsw_create(index, dim, m, ef_construction, METRIC_L2, status)
  if (status /= GLAMIN_OK) error stop "hnsw_create failed"

  call hnsw_add(index, vectors, status)
  if (status /= GLAMIN_OK) error stop "hnsw_add failed"

  if (snapshot_flag /= 0_int32) then
    call hnsw_build_snapshot(index, status)
    if (status /= GLAMIN_OK) error stop "hnsw_build_snapshot failed"
  end if

  distances = VectorBlock()
  labels = VectorBlock()
  call run_search(index, queries, ef_search, k, iterations, distances, labels)

  call free_aligned(distances%data, free_status)
  call free_aligned(labels%data, free_status)
  call free_aligned(vector_ptr, free_status)
  call free_aligned(query_ptr, free_status)
  call hnsw_destroy(index, status)
contains
  subroutine run_search(index, queries, ef_search, k, iterations, distances, labels)
    type(HnswIndex), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: ef_search
    integer(int32), intent(in) :: k
    integer(int32), intent(in) :: iterations
    type(VectorBlock), intent(inout) :: distances
    type(VectorBlock), intent(inout) :: labels
    integer(int32) :: iter
    integer(int32) :: status
    integer(int64) :: clock_start
    integer(int64) :: clock_end
    integer(int64) :: clock_rate
    real(real64) :: elapsed
    real(real64) :: per_iter
    real(real64) :: queries_per_sec

    call system_clock(count_rate=clock_rate)
    call system_clock(clock_start)

    do iter = 1, iterations
      call hnsw_search(index, queries, k, ef_search, distances, labels, status)
      if (status /= GLAMIN_OK) error stop "hnsw_search failed"
    end do

    call system_clock(clock_end)
    elapsed = real(clock_end - clock_start, real64) / real(clock_rate, real64)
    per_iter = elapsed / real(iterations, real64)
    queries_per_sec = real(queries%length, real64) / per_iter

    write (*, '(a)') 'HNSW benchmark'
    write (*, '(a,i0)') '  dim: ', queries%dim
    write (*, '(a,i0)') '  vectors: ', int(index%ntotal)
    write (*, '(a,i0)') '  queries: ', int(queries%length)
    write (*, '(a,i0)') '  m: ', index%m
    write (*, '(a,i0)') '  ef_search: ', ef_search
    write (*, '(a,i0)') '  k: ', k
    write (*, '(a,f10.4)') '  sec/iter: ', real(per_iter, real32)
    write (*, '(a,es12.4)') '  queries/s: ', queries_per_sec
  end subroutine run_search

  subroutine read_arguments(dim, vector_count, query_count, m, ef_construction, ef_search, k, &
      iterations, snapshot_flag)
    integer(int32), intent(out) :: dim
    integer(int64), intent(out) :: vector_count
    integer(int64), intent(out) :: query_count
    integer(int32), intent(out) :: m
    integer(int32), intent(out) :: ef_construction
    integer(int32), intent(out) :: ef_search
    integer(int32), intent(out) :: k
    integer(int32), intent(out) :: iterations
    integer(int32), intent(out) :: snapshot_flag
    character(len=64) :: arg

    dim = DEFAULT_DIM
    vector_count = DEFAULT_VECTOR_COUNT
    query_count = DEFAULT_QUERY_COUNT
    m = DEFAULT_M
    ef_construction = DEFAULT_EF_CONSTRUCTION
    ef_search = DEFAULT_EF_SEARCH
    k = DEFAULT_K
    iterations = DEFAULT_ITERS
    snapshot_flag = DEFAULT_SNAPSHOT

    call get_command_argument(1, arg)
    if (len_trim(arg) > 0) dim = parse_int32(arg, DEFAULT_DIM)
    call get_command_argument(2, arg)
    if (len_trim(arg) > 0) vector_count = parse_int64(arg, DEFAULT_VECTOR_COUNT)
    call get_command_argument(3, arg)
    if (len_trim(arg) > 0) query_count = parse_int64(arg, DEFAULT_QUERY_COUNT)
    call get_command_argument(4, arg)
    if (len_trim(arg) > 0) m = parse_int32(arg, DEFAULT_M)
    call get_command_argument(5, arg)
    if (len_trim(arg) > 0) ef_construction = parse_int32(arg, DEFAULT_EF_CONSTRUCTION)
    call get_command_argument(6, arg)
    if (len_trim(arg) > 0) ef_search = parse_int32(arg, DEFAULT_EF_SEARCH)
    call get_command_argument(7, arg)
    if (len_trim(arg) > 0) k = parse_int32(arg, DEFAULT_K)
    call get_command_argument(8, arg)
    if (len_trim(arg) > 0) iterations = parse_int32(arg, DEFAULT_ITERS)
    call get_command_argument(9, arg)
    if (len_trim(arg) > 0) snapshot_flag = parse_int32(arg, DEFAULT_SNAPSHOT)
  end subroutine read_arguments

  integer(int32) function parse_int32(arg, fallback)
    character(len=*), intent(in) :: arg
    integer(int32), intent(in) :: fallback
    integer :: io_status

    read(arg, *, iostat=io_status) parse_int32
    if (io_status /= 0) parse_int32 = fallback
  end function parse_int32

  integer(int64) function parse_int64(arg, fallback)
    character(len=*), intent(in) :: arg
    integer(int64), intent(in) :: fallback
    integer :: io_status

    read(arg, *, iostat=io_status) parse_int64
    if (io_status /= 0) parse_int64 = fallback
  end function parse_int64
end program glamin_hnsw_benchmark
