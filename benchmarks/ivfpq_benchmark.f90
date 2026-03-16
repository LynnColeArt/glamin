program glamin_ivfpq_benchmark
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK
  use glamin_index_ivfpq, only: IvfProductQuantizerIndex, ivfpq_add, ivfpq_create, &
    ivfpq_destroy, ivfpq_search, ivfpq_train
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_L2
  use glamin_types, only: VectorBlock
  implicit none

  integer(int32), parameter :: DEFAULT_DIM = 128_int32
  integer(int64), parameter :: DEFAULT_VECTOR_COUNT = 4096_int64
  integer(int64), parameter :: DEFAULT_QUERY_COUNT = 256_int64
  integer(int32), parameter :: DEFAULT_NLIST = 64_int32
  integer(int32), parameter :: DEFAULT_NPROBE = 8_int32
  integer(int32), parameter :: DEFAULT_M = 8_int32
  integer(int32), parameter :: DEFAULT_KSUB = 256_int32
  integer(int32), parameter :: DEFAULT_K = 10_int32
  integer(int32), parameter :: DEFAULT_ITERS = 3_int32
  integer(c_size_t), parameter :: ALIGN_BYTES = 64_c_size_t

  type(IvfProductQuantizerIndex) :: index
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
  integer(int32) :: nlist
  integer(int32) :: nprobe
  integer(int32) :: m
  integer(int32) :: ksub
  integer(int32) :: k
  integer(int32) :: iterations
  integer(int32) :: elem_bytes
  integer(int32) :: status
  integer(int32) :: free_status

  call read_arguments(dim, vector_count, query_count, nlist, nprobe, m, ksub, k, iterations)
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

  call ivfpq_create(index, dim, nlist, m, ksub, METRIC_L2, status)
  if (status /= GLAMIN_OK) error stop "ivfpq_create failed"

  call ivfpq_train(index, vectors, status)
  if (status /= GLAMIN_OK) error stop "ivfpq_train failed"

  call ivfpq_add(index, vectors, status)
  if (status /= GLAMIN_OK) error stop "ivfpq_add failed"

  distances = VectorBlock()
  labels = VectorBlock()
  call run_search(index, queries, nprobe, k, iterations, distances, labels)

  call free_aligned(distances%data, free_status)
  call free_aligned(labels%data, free_status)
  call free_aligned(vector_ptr, free_status)
  call free_aligned(query_ptr, free_status)
  call ivfpq_destroy(index, status)
contains
  subroutine run_search(index, queries, nprobe, k, iterations, distances, labels)
    type(IvfProductQuantizerIndex), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: nprobe
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
      call ivfpq_search(index, queries, k, nprobe, distances, labels, status)
      if (status /= GLAMIN_OK) error stop "ivfpq_search failed"
    end do

    call system_clock(clock_end)
    elapsed = real(clock_end - clock_start, real64) / real(clock_rate, real64)
    per_iter = elapsed / real(iterations, real64)
    queries_per_sec = real(queries%length, real64) / per_iter

    write (*, '(a)') 'IVFPQ benchmark'
    write (*, '(a,i0)') '  dim: ', queries%dim
    write (*, '(a,i0)') '  vectors: ', int(index%ntotal)
    write (*, '(a,i0)') '  queries: ', int(queries%length)
    write (*, '(a,i0)') '  nlist: ', index%nlist
    write (*, '(a,i0)') '  nprobe: ', nprobe
    write (*, '(a,i0)') '  m: ', index%m
    write (*, '(a,i0)') '  ksub: ', index%ksub
    write (*, '(a,i0)') '  k: ', k
    write (*, '(a,f10.4)') '  sec/iter: ', real(per_iter, real32)
    write (*, '(a,es12.4)') '  queries/s: ', queries_per_sec
  end subroutine run_search

  subroutine read_arguments(dim, vector_count, query_count, nlist, nprobe, m, ksub, k, iterations)
    integer(int32), intent(out) :: dim
    integer(int64), intent(out) :: vector_count
    integer(int64), intent(out) :: query_count
    integer(int32), intent(out) :: nlist
    integer(int32), intent(out) :: nprobe
    integer(int32), intent(out) :: m
    integer(int32), intent(out) :: ksub
    integer(int32), intent(out) :: k
    integer(int32), intent(out) :: iterations
    character(len=64) :: arg

    dim = DEFAULT_DIM
    vector_count = DEFAULT_VECTOR_COUNT
    query_count = DEFAULT_QUERY_COUNT
    nlist = DEFAULT_NLIST
    nprobe = DEFAULT_NPROBE
    m = DEFAULT_M
    ksub = DEFAULT_KSUB
    k = DEFAULT_K
    iterations = DEFAULT_ITERS

    call get_command_argument(1, arg)
    if (len_trim(arg) > 0) dim = parse_int32(arg, DEFAULT_DIM)
    call get_command_argument(2, arg)
    if (len_trim(arg) > 0) vector_count = parse_int64(arg, DEFAULT_VECTOR_COUNT)
    call get_command_argument(3, arg)
    if (len_trim(arg) > 0) query_count = parse_int64(arg, DEFAULT_QUERY_COUNT)
    call get_command_argument(4, arg)
    if (len_trim(arg) > 0) nlist = parse_int32(arg, DEFAULT_NLIST)
    call get_command_argument(5, arg)
    if (len_trim(arg) > 0) nprobe = parse_int32(arg, DEFAULT_NPROBE)
    call get_command_argument(6, arg)
    if (len_trim(arg) > 0) m = parse_int32(arg, DEFAULT_M)
    call get_command_argument(7, arg)
    if (len_trim(arg) > 0) ksub = parse_int32(arg, DEFAULT_KSUB)
    call get_command_argument(8, arg)
    if (len_trim(arg) > 0) k = parse_int32(arg, DEFAULT_K)
    call get_command_argument(9, arg)
    if (len_trim(arg) > 0) iterations = parse_int32(arg, DEFAULT_ITERS)
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
end program glamin_ivfpq_benchmark
