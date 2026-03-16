program glamin_gpu_distance_benchmark
  use iso_fortran_env, only: int32, int64, real32, real64
  use iso_c_binding, only: c_f_pointer, c_ptr, c_size_t
  use glamin_cuda_backend, only: CudaBackend
  use glamin_cuda_ops, only: cuda_register_stub_ops
  use glamin_errors, only: GLAMIN_OK
  use glamin_gpu_backend, only: GPU_BACKEND_NAME_LEN, gpu_auto_select_backend, &
    gpu_clear_backend, gpu_distance_ip_dispatch, gpu_distance_l2_dispatch, &
    gpu_get_backend_name, gpu_register_backend
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_types, only: VectorBlock
  use glamin_vulkan_backend, only: VulkanBackend
  implicit none

  integer(int32), parameter :: DEFAULT_DIM = 256_int32
  integer(int64), parameter :: DEFAULT_QUERY_COUNT = 256_int64
  integer(int64), parameter :: DEFAULT_VECTOR_COUNT = 4096_int64
  integer(int32), parameter :: DEFAULT_ITERS = 3_int32
  integer(c_size_t), parameter :: ALIGN_BYTES = 64_c_size_t

  type(CudaBackend) :: cuda_backend
  type(VulkanBackend) :: vulkan_backend
  type(VectorBlock) :: queries
  type(VectorBlock) :: vectors
  type(VectorBlock) :: distances
  real(real32), pointer :: query_data(:)
  real(real32), pointer :: vector_data(:)
  type(c_ptr) :: query_ptr
  type(c_ptr) :: vector_ptr
  character(len=GPU_BACKEND_NAME_LEN) :: backend_name
  integer(int32) :: dim
  integer(int64) :: query_count
  integer(int64) :: vector_count
  integer(int32) :: iterations
  integer(int32) :: elem_bytes
  integer(int32) :: status
  integer(int32) :: free_status

  call read_arguments(dim, query_count, vector_count, iterations)
  elem_bytes = int(storage_size(0.0_real32) / 8, int32)

  call cuda_register_stub_ops(status)
  if (status /= GLAMIN_OK) error stop "cuda_register_stub_ops failed"

  call gpu_register_backend('cuda', cuda_backend, status)
  if (status /= GLAMIN_OK) error stop "gpu_register_backend failed"
  call gpu_register_backend('vulkan', vulkan_backend, status)
  if (status /= GLAMIN_OK) error stop "gpu_register_backend failed"
  call gpu_auto_select_backend(status)
  if (status /= GLAMIN_OK) error stop "gpu_auto_select_backend failed"
  call gpu_get_backend_name(backend_name)
  write (*, '(a,1x,a)') 'Backend:', trim(backend_name)

  call allocate_aligned(query_ptr, int(dim, int64) * query_count * elem_bytes, &
    ALIGN_BYTES, status)
  if (status /= GLAMIN_OK) error stop "Failed to allocate query buffer"

  call allocate_aligned(vector_ptr, int(dim, int64) * vector_count * elem_bytes, &
    ALIGN_BYTES, status)
  if (status /= GLAMIN_OK) error stop "Failed to allocate vector buffer"

  call c_f_pointer(query_ptr, query_data, [int(dim, int64) * query_count])
  call c_f_pointer(vector_ptr, vector_data, [int(dim, int64) * vector_count])

  call random_seed()
  call random_number(query_data)
  call random_number(vector_data)

  queries = VectorBlock()
  queries%data = query_ptr
  queries%length = query_count
  queries%dim = dim
  queries%stride = dim
  queries%elem_size = elem_bytes
  queries%alignment = int(ALIGN_BYTES, int32)

  vectors = VectorBlock()
  vectors%data = vector_ptr
  vectors%length = vector_count
  vectors%dim = dim
  vectors%stride = dim
  vectors%elem_size = elem_bytes
  vectors%alignment = int(ALIGN_BYTES, int32)

  distances = VectorBlock()
  call gpu_distance_l2_dispatch(queries, vectors, distances, status)
  if (status /= GLAMIN_OK) error stop "GPU L2 warmup failed"
  call free_aligned(distances%data, free_status)

  call run_benchmark(.true., queries, vectors, iterations, "GPU L2 dispatch")
  call run_benchmark(.false., queries, vectors, iterations, "GPU IP dispatch")

  call free_aligned(query_ptr, free_status)
  call free_aligned(vector_ptr, free_status)
  call gpu_clear_backend(status)
contains
  subroutine run_benchmark(use_l2, queries, vectors, iterations, label)
    logical, intent(in) :: use_l2
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(in) :: iterations
    character(len=*), intent(in) :: label
    type(VectorBlock) :: distances
    integer(int32) :: iter
    integer(int32) :: status
    integer(int32) :: free_status
    integer(int64) :: clock_start
    integer(int64) :: clock_end
    integer(int64) :: clock_rate
    real(real64) :: elapsed
    real(real64) :: per_iter
    real(real64) :: throughput
    real(real64) :: distance_pairs

    distances = VectorBlock()
    call system_clock(count_rate=clock_rate)
    call system_clock(clock_start)

    do iter = 1, iterations
      if (use_l2) then
        call gpu_distance_l2_dispatch(queries, vectors, distances, status)
      else
        call gpu_distance_ip_dispatch(queries, vectors, distances, status)
      end if
      if (status /= GLAMIN_OK) then
        error stop "GPU distance dispatch failed"
      end if
    end do

    call system_clock(clock_end)
    elapsed = real(clock_end - clock_start, real64) / real(clock_rate, real64)
    per_iter = elapsed / real(iterations, real64)
    distance_pairs = real(queries%length, real64) * real(vectors%length, real64)
    throughput = distance_pairs / per_iter

    write (*, '(a,1x,a)') 'Kernel:', trim(label)
    write (*, '(a,i0)') '  dim: ', queries%dim
    write (*, '(a,i0)') '  queries: ', int(queries%length)
    write (*, '(a,i0)') '  vectors: ', int(vectors%length)
    write (*, '(a,f10.4)') '  sec/iter: ', real(per_iter, real32)
    write (*, '(a,es12.4)') '  distances/s: ', throughput

    call free_aligned(distances%data, free_status)
  end subroutine run_benchmark

  subroutine read_arguments(dim, query_count, vector_count, iterations)
    integer(int32), intent(out) :: dim
    integer(int64), intent(out) :: query_count
    integer(int64), intent(out) :: vector_count
    integer(int32), intent(out) :: iterations
    character(len=64) :: arg

    dim = DEFAULT_DIM
    query_count = DEFAULT_QUERY_COUNT
    vector_count = DEFAULT_VECTOR_COUNT
    iterations = DEFAULT_ITERS

    call get_command_argument(1, arg)
    if (len_trim(arg) > 0) dim = parse_int32(arg, DEFAULT_DIM)
    call get_command_argument(2, arg)
    if (len_trim(arg) > 0) query_count = parse_int64(arg, DEFAULT_QUERY_COUNT)
    call get_command_argument(3, arg)
    if (len_trim(arg) > 0) vector_count = parse_int64(arg, DEFAULT_VECTOR_COUNT)
    call get_command_argument(4, arg)
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
end program glamin_gpu_distance_benchmark
