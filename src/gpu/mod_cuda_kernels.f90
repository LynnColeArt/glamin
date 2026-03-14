module glamin_cuda_kernels
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_int32_t, c_int64_t, c_ptr, c_size_t
  use glamin_cuda_memory, only: CudaBuffer, cuda_buffer_allocate, cuda_buffer_download, &
    cuda_buffer_release, cuda_buffer_upload
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_NOT_READY
  use glamin_memory, only: allocate_aligned
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: cuda_kernels_available
  public :: cuda_distance_l2
  public :: cuda_distance_ip

  integer(int32), parameter :: DEFAULT_ALIGNMENT = 64

  interface
    function glamin_cuda_is_available() bind(c) result(flag)
      import :: c_int32_t
      integer(c_int32_t) :: flag
    end function glamin_cuda_is_available

    subroutine glamin_cuda_distance_l2(queries, query_count, query_stride, vectors, vector_count, &
        vector_stride, dim, distances, distance_stride, status) bind(c)
      import :: c_int32_t, c_int64_t, c_ptr
      type(c_ptr), value :: queries
      integer(c_int64_t), value :: query_count
      integer(c_int32_t), value :: query_stride
      type(c_ptr), value :: vectors
      integer(c_int64_t), value :: vector_count
      integer(c_int32_t), value :: vector_stride
      integer(c_int32_t), value :: dim
      type(c_ptr), value :: distances
      integer(c_int32_t), value :: distance_stride
      integer(c_int32_t) :: status
    end subroutine glamin_cuda_distance_l2

    subroutine glamin_cuda_distance_ip(queries, query_count, query_stride, vectors, vector_count, &
        vector_stride, dim, distances, distance_stride, status) bind(c)
      import :: c_int32_t, c_int64_t, c_ptr
      type(c_ptr), value :: queries
      integer(c_int64_t), value :: query_count
      integer(c_int32_t), value :: query_stride
      type(c_ptr), value :: vectors
      integer(c_int64_t), value :: vector_count
      integer(c_int32_t), value :: vector_stride
      integer(c_int32_t), value :: dim
      type(c_ptr), value :: distances
      integer(c_int32_t), value :: distance_stride
      integer(c_int32_t) :: status
    end subroutine glamin_cuda_distance_ip
  end interface

contains
  logical function cuda_kernels_available()
    integer(c_int32_t) :: flag

    flag = glamin_cuda_is_available()
    cuda_kernels_available = flag /= 0_c_int32_t
  end function cuda_kernels_available

  subroutine cuda_distance_l2(queries, vectors, distances, status)
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status
    call run_cuda_distance(queries, vectors, distances, status, .true.)
  end subroutine cuda_distance_l2

  subroutine cuda_distance_ip(queries, vectors, distances, status)
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status
    call run_cuda_distance(queries, vectors, distances, status, .false.)
  end subroutine cuda_distance_ip

  subroutine run_cuda_distance(queries, vectors, distances, status, use_l2)
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status
    logical, intent(in) :: use_l2
    type(CudaBuffer) :: query_buffer
    type(CudaBuffer) :: vector_buffer
    type(CudaBuffer) :: distance_buffer
    integer(int64) :: query_count
    integer(int64) :: vector_count
    integer(int32) :: dim
    integer(int32) :: stride_q
    integer(int32) :: stride_v
    integer(int32) :: stride_d
    integer(int32) :: elem_bytes
    integer(int64) :: query_bytes
    integer(int64) :: vector_bytes
    integer(int64) :: distance_bytes
    integer(int64) :: total_bytes
    integer(int32) :: alloc_status
    integer(c_int32_t) :: status_c

    status = GLAMIN_OK
    if (.not. cuda_kernels_available()) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if
    if (.not. c_associated(queries%data) .or. .not. c_associated(vectors%data)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    dim = queries%dim
    if (dim <= 0_int32 .or. vectors%dim /= dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    query_count = queries%length
    vector_count = vectors%length
    if (query_count <= 0_int64 .or. vector_count <= 0_int64) then
      status = GLAMIN_OK
      return
    end if

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    if ((queries%elem_size /= 0_int32 .and. queries%elem_size /= elem_bytes) .or. &
        (vectors%elem_size /= 0_int32 .and. vectors%elem_size /= elem_bytes)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if
    if (distances%elem_size /= 0_int32 .and. distances%elem_size /= elem_bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    stride_q = queries%stride
    stride_v = vectors%stride
    if (stride_q <= 0_int32) stride_q = dim
    if (stride_v <= 0_int32) stride_v = dim
    if (stride_q < dim .or. stride_v < dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    stride_d = distances%stride
    if (stride_d <= 0_int32) stride_d = int(vector_count, int32)
    if (stride_d < vector_count) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    distances%dim = int(vector_count, int32)
    distances%length = query_count
    distances%stride = stride_d
    distances%elem_size = elem_bytes
    distances%alignment = DEFAULT_ALIGNMENT

    if (.not. c_associated(distances%data)) then
      total_bytes = int(stride_d, int64) * query_count * elem_bytes
      call allocate_aligned(distances%data, int(total_bytes, c_size_t), &
        int(DEFAULT_ALIGNMENT, c_size_t), alloc_status)
      if (alloc_status /= GLAMIN_OK) then
        status = alloc_status
        return
      end if
    end if

    query_bytes = int(stride_q, int64) * query_count * elem_bytes
    vector_bytes = int(stride_v, int64) * vector_count * elem_bytes
    distance_bytes = int(stride_d, int64) * query_count * elem_bytes

    call cuda_buffer_allocate(query_buffer, query_bytes, status)
    if (status /= GLAMIN_OK) goto 100
    call cuda_buffer_allocate(vector_buffer, vector_bytes, status)
    if (status /= GLAMIN_OK) goto 100
    call cuda_buffer_allocate(distance_buffer, distance_bytes, status)
    if (status /= GLAMIN_OK) goto 100

    call cuda_buffer_upload(query_buffer, queries%data, query_bytes, status)
    if (status /= GLAMIN_OK) goto 100
    call cuda_buffer_upload(vector_buffer, vectors%data, vector_bytes, status)
    if (status /= GLAMIN_OK) goto 100

    status_c = GLAMIN_OK
    if (use_l2) then
      call glamin_cuda_distance_l2(query_buffer%device_ptr, int(query_count, c_int64_t), &
        int(stride_q, c_int32_t), vector_buffer%device_ptr, int(vector_count, c_int64_t), &
        int(stride_v, c_int32_t), int(dim, c_int32_t), distance_buffer%device_ptr, &
        int(stride_d, c_int32_t), status_c)
    else
      call glamin_cuda_distance_ip(query_buffer%device_ptr, int(query_count, c_int64_t), &
        int(stride_q, c_int32_t), vector_buffer%device_ptr, int(vector_count, c_int64_t), &
        int(stride_v, c_int32_t), int(dim, c_int32_t), distance_buffer%device_ptr, &
        int(stride_d, c_int32_t), status_c)
    end if
    status = int(status_c, int32)
    if (status /= GLAMIN_OK) goto 100

    call cuda_buffer_download(distance_buffer, distances%data, distance_bytes, status)

100 continue
    call cuda_buffer_release(distance_buffer, alloc_status)
    call cuda_buffer_release(vector_buffer, alloc_status)
    call cuda_buffer_release(query_buffer, alloc_status)
  end subroutine run_cuda_distance
end module glamin_cuda_kernels
