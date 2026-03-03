module glamin_faiss_io
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_int8_t, c_int32_t, c_loc, &
    c_null_ptr, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK
  use glamin_index_flat, only: FlatIndex, flat_create_handle, flat_handle
  use glamin_memory, only: free_aligned
  use glamin_metrics, only: METRIC_IP, METRIC_L2
  use glamin_stream, only: IoStream, read_bytes, write_bytes
  use glamin_types, only: IndexHandle
  implicit none
  private

  public :: load_faiss_index
  public :: save_faiss_index

  integer(int64), parameter :: BYTES_INT32 = storage_size(0_int32) / 8
  integer(int64), parameter :: BYTES_INT64 = storage_size(0_int64) / 8
  integer(int64), parameter :: BYTES_INT8 = storage_size(0_c_int8_t) / 8
  integer(int64), parameter :: BYTES_REAL32 = storage_size(0.0_real32) / 8
  integer(int64), parameter :: BYTES_SIZE_T = storage_size(0_c_size_t) / 8

  integer(int64), parameter :: INDEX_DUMMY = 1_int64 * 2_int64**20
  integer(int32), parameter :: FAISS_METRIC_IP = 0_int32
  integer(int32), parameter :: FAISS_METRIC_L2 = 1_int32

contains
  subroutine load_faiss_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(out) :: index
    type(FlatIndex), pointer :: flat_index
    type(c_ptr) :: buffer
    integer(int32) :: fourcc_code
    integer(int32) :: dim
    integer(int32) :: metric_type
    integer(int32) :: glamin_metric
    integer(int32) :: status
    integer(int64) :: ntotal
    integer(int64) :: dummy
    integer(int64) :: expected_count
    integer(int64) :: bytes_to_read
    integer(int64) :: float_count
    integer(c_size_t) :: size_value
    integer(c_int8_t) :: trained_flag
    real(real32) :: metric_arg

    index%impl = c_null_ptr
    call read_int32(stream, fourcc_code)

    if (.not. is_flat_fourcc(fourcc_code)) then
      error stop "Unsupported FAISS index type"
    end if

    call read_int32(stream, dim)
    call read_int64(stream, ntotal)
    call read_int64(stream, dummy)
    call read_int64(stream, dummy)
    call read_int8(stream, trained_flag)
    call read_int32(stream, metric_type)
    if (metric_type > FAISS_METRIC_L2) then
      call read_real32(stream, metric_arg)
    end if

    if (metric_type == FAISS_METRIC_IP) then
      glamin_metric = METRIC_IP
    else if (metric_type == FAISS_METRIC_L2) then
      glamin_metric = METRIC_L2
    else
      error stop "Unsupported FAISS metric type"
    end if

    call flat_create_handle(index, dim, glamin_metric, status)
    if (status /= GLAMIN_OK) then
      error stop "Failed to create flat index handle"
    end if

    call flat_handle(index, flat_index)
    if (.not. associated(flat_index)) then
      error stop "Failed to resolve flat index handle"
    end if

    call read_size(stream, size_value)
    float_count = int(size_value, int64)
    expected_count = ntotal * int(dim, int64)
    if (float_count /= expected_count) then
      error stop "Invalid FAISS flat vector count"
    end if

    bytes_to_read = float_count * BYTES_REAL32
    buffer = c_null_ptr
    if (bytes_to_read > 0_int64) then
      call read_bytes(stream, buffer, bytes_to_read)
      if (.not. c_associated(buffer)) then
        error stop "Failed to read FAISS flat vectors"
      end if
    end if

    flat_index%data%data = buffer
    flat_index%data%length = ntotal
    flat_index%data%dim = dim
    flat_index%data%stride = dim
    flat_index%data%elem_size = int(BYTES_REAL32, int32)
    flat_index%data%alignment = 64
  end subroutine load_faiss_index

  subroutine save_faiss_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(in) :: index
    type(FlatIndex), pointer :: flat_index
    integer(int32) :: metric_type
    integer(int32) :: fourcc_code
    integer(int32) :: dim
    integer(int64) :: ntotal
    integer(int64) :: float_count
    integer(int64) :: bytes_to_write
    integer(c_int8_t) :: trained_flag
    integer(c_size_t) :: size_value

    call flat_handle(index, flat_index)
    if (.not. associated(flat_index)) then
      error stop "save_faiss_index only supports flat indices"
    end if

    dim = flat_index%dim
    ntotal = flat_index%data%length
    if (dim <= 0_int32) then
      error stop "Invalid flat index dimension"
    end if

    if (flat_index%metric == METRIC_IP) then
      metric_type = FAISS_METRIC_IP
      fourcc_code = fourcc_from_string("IxFI")
    else if (flat_index%metric == METRIC_L2) then
      metric_type = FAISS_METRIC_L2
      fourcc_code = fourcc_from_string("IxF2")
    else
      error stop "Unsupported flat index metric"
    end if

    call write_int32(stream, fourcc_code)
    call write_int32(stream, dim)
    call write_int64(stream, ntotal)
    call write_int64(stream, INDEX_DUMMY)
    call write_int64(stream, INDEX_DUMMY)
    trained_flag = 1_c_int8_t
    call write_int8(stream, trained_flag)
    call write_int32(stream, metric_type)

    float_count = ntotal * int(dim, int64)
    size_value = int(float_count, c_size_t)
    call write_size(stream, size_value)

    if (float_count > 0_int64) then
      if (.not. c_associated(flat_index%data%data)) then
        error stop "Flat index data missing"
      end if
      bytes_to_write = float_count * BYTES_REAL32
      call write_bytes(stream, flat_index%data%data, bytes_to_write)
    end if
  end subroutine save_faiss_index

  logical function is_flat_fourcc(code)
    integer(int32), intent(in) :: code

    is_flat_fourcc = (code == fourcc_from_string("IxFI")) .or. &
      (code == fourcc_from_string("IxF2")) .or. (code == fourcc_from_string("IxFl"))
  end function is_flat_fourcc

  pure integer(int32) function fourcc_from_string(value)
    character(len=*), intent(in) :: value
    integer(int32) :: b0
    integer(int32) :: b1
    integer(int32) :: b2
    integer(int32) :: b3

    if (len_trim(value) /= 4) then
      fourcc_from_string = 0_int32
      return
    end if

    b0 = iachar(value(1:1))
    b1 = iachar(value(2:2))
    b2 = iachar(value(3:3))
    b3 = iachar(value(4:4))

    fourcc_from_string = b0 + ishft(b1, 8) + ishft(b2, 16) + ishft(b3, 24)
  end function fourcc_from_string

  subroutine read_int32(stream, value)
    type(IoStream), intent(inout) :: stream
    integer(int32), intent(out) :: value
    type(c_ptr) :: buffer
    integer(int32), pointer :: data(:)
    integer(int32) :: free_status

    call read_bytes(stream, buffer, BYTES_INT32)
    call c_f_pointer(buffer, data, [1])
    value = data(1)
    call free_aligned(buffer, free_status)
  end subroutine read_int32

  subroutine read_int64(stream, value)
    type(IoStream), intent(inout) :: stream
    integer(int64), intent(out) :: value
    type(c_ptr) :: buffer
    integer(int64), pointer :: data(:)
    integer(int32) :: free_status

    call read_bytes(stream, buffer, BYTES_INT64)
    call c_f_pointer(buffer, data, [1])
    value = data(1)
    call free_aligned(buffer, free_status)
  end subroutine read_int64

  subroutine read_int8(stream, value)
    type(IoStream), intent(inout) :: stream
    integer(c_int8_t), intent(out) :: value
    type(c_ptr) :: buffer
    integer(c_int8_t), pointer :: data(:)
    integer(int32) :: free_status

    call read_bytes(stream, buffer, BYTES_INT8)
    call c_f_pointer(buffer, data, [1])
    value = data(1)
    call free_aligned(buffer, free_status)
  end subroutine read_int8

  subroutine read_real32(stream, value)
    type(IoStream), intent(inout) :: stream
    real(real32), intent(out) :: value
    type(c_ptr) :: buffer
    real(real32), pointer :: data(:)
    integer(int32) :: free_status

    call read_bytes(stream, buffer, BYTES_REAL32)
    call c_f_pointer(buffer, data, [1])
    value = data(1)
    call free_aligned(buffer, free_status)
  end subroutine read_real32

  subroutine read_size(stream, value)
    type(IoStream), intent(inout) :: stream
    integer(c_size_t), intent(out) :: value
    type(c_ptr) :: buffer
    integer(c_size_t), pointer :: data(:)
    integer(int32) :: free_status

    call read_bytes(stream, buffer, BYTES_SIZE_T)
    call c_f_pointer(buffer, data, [1])
    value = data(1)
    call free_aligned(buffer, free_status)
  end subroutine read_size

  subroutine write_int32(stream, value)
    type(IoStream), intent(inout) :: stream
    integer(int32), intent(in) :: value
    integer(int32), target :: local_value

    local_value = value
    call write_bytes(stream, c_loc(local_value), BYTES_INT32)
  end subroutine write_int32

  subroutine write_int64(stream, value)
    type(IoStream), intent(inout) :: stream
    integer(int64), intent(in) :: value
    integer(int64), target :: local_value

    local_value = value
    call write_bytes(stream, c_loc(local_value), BYTES_INT64)
  end subroutine write_int64

  subroutine write_int8(stream, value)
    type(IoStream), intent(inout) :: stream
    integer(c_int8_t), intent(in) :: value
    integer(c_int8_t), target :: local_value

    local_value = value
    call write_bytes(stream, c_loc(local_value), BYTES_INT8)
  end subroutine write_int8

  subroutine write_size(stream, value)
    type(IoStream), intent(inout) :: stream
    integer(c_size_t), intent(in) :: value
    integer(c_size_t), target :: local_value

    local_value = value
    call write_bytes(stream, c_loc(local_value), BYTES_SIZE_T)
  end subroutine write_size
end module glamin_faiss_io
