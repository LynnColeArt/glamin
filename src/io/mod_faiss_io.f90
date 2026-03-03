module glamin_faiss_io
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_int8_t, c_int32_t, c_loc, &
    c_null_ptr, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK
  use glamin_index_flat, only: FlatIndex, flat_create_handle, flat_handle
  use glamin_index_pq, only: PQIndex, pq_create_handle, pq_handle
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
    integer(int32) :: fourcc_code

    index%impl = c_null_ptr
    call read_int32(stream, fourcc_code)

    if (is_flat_fourcc(fourcc_code)) then
      call load_flat_index(stream, index)
    else if (is_pq_fourcc(fourcc_code)) then
      call load_pq_index(stream, index)
    else
      error stop "Unsupported FAISS index type"
    end if
  end subroutine load_faiss_index

  subroutine save_faiss_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(in) :: index
    type(FlatIndex), pointer :: flat_index

    call flat_handle(index, flat_index)
    if (associated(flat_index)) then
      call save_flat_index(stream, flat_index)
      return
    end if

    call save_pq_index(stream, index)
  end subroutine save_faiss_index

  subroutine load_flat_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(out) :: index
    type(FlatIndex), pointer :: flat_index
    type(c_ptr) :: buffer
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
  end subroutine load_flat_index

  subroutine load_pq_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(out) :: index
    type(PQIndex), pointer :: pq_index
    type(c_ptr) :: buffer
    integer(int32) :: dim
    integer(int32) :: pq_dim
    integer(int32) :: metric_type
    integer(int32) :: glamin_metric
    integer(int32) :: status
    integer(int32) :: m
    integer(int32) :: nbits
    integer(int32) :: code_size
    integer(int32) :: dsub
    integer(int32) :: search_type
    integer(int32) :: polysemous_ht
    integer(int64) :: ntotal
    integer(int64) :: dummy
    integer(int64) :: float_count
    integer(int64) :: expected_count
    integer(int64) :: bytes_to_read
    integer(int64) :: code_count
    integer(c_size_t) :: size_value
    integer(c_int8_t) :: trained_flag
    integer(c_int8_t) :: encode_signs
    real(real32) :: metric_arg

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

    call read_int32(stream, pq_dim)
    call read_int32(stream, m)
    call read_int32(stream, nbits)

    if (pq_dim /= dim) then
      error stop "PQ dimension mismatch"
    end if

    if (m <= 0_int32 .or. nbits <= 0_int32) then
      error stop "Invalid PQ parameters"
    end if

    if (mod(dim, m) /= 0_int32) then
      error stop "PQ dimension not divisible by m"
    end if

    code_size = int((nbits * m + 7_int32) / 8_int32, int32)
    dsub = dim / m

    call pq_create_handle(index, dim, m, nbits, glamin_metric, status)
    if (status /= GLAMIN_OK) then
      error stop "Failed to create PQ index handle"
    end if

    call pq_handle(index, pq_index)
    if (.not. associated(pq_index)) then
      error stop "Failed to resolve PQ index handle"
    end if

    call read_size(stream, size_value)
    float_count = int(size_value, int64)
    expected_count = int(dim, int64) * int(2_int64**nbits, int64)
    if (float_count /= expected_count) then
      error stop "Invalid PQ codebook size"
    end if

    bytes_to_read = float_count * BYTES_REAL32
    buffer = c_null_ptr
    if (bytes_to_read > 0_int64) then
      call read_bytes(stream, buffer, bytes_to_read)
      if (.not. c_associated(buffer)) then
        error stop "Failed to read PQ codebooks"
      end if
    end if

    pq_index%codebooks%data = buffer
    pq_index%codebooks%length = int(m, int64) * int(2_int64**nbits, int64)
    pq_index%codebooks%dim = dsub
    pq_index%codebooks%stride = dsub
    pq_index%codebooks%elem_size = int(BYTES_REAL32, int32)
    pq_index%codebooks%alignment = 64

    call read_size(stream, size_value)
    code_count = int(size_value, int64)
    if (code_count /= ntotal * int(code_size, int64)) then
      error stop "Invalid PQ code payload size"
    end if

    bytes_to_read = code_count * BYTES_INT8
    buffer = c_null_ptr
    if (bytes_to_read > 0_int64) then
      call read_bytes(stream, buffer, bytes_to_read)
      if (.not. c_associated(buffer)) then
        error stop "Failed to read PQ codes"
      end if
    end if

    pq_index%codes%data = buffer
    pq_index%codes%length = ntotal
    pq_index%codes%dim = code_size
    pq_index%codes%stride = code_size
    pq_index%codes%elem_size = 1_int32
    pq_index%codes%alignment = 64

    call read_int32(stream, search_type)
    call read_int8(stream, encode_signs)
    call read_int32(stream, polysemous_ht)

    pq_index%ntotal = ntotal
    pq_index%is_trained = (trained_flag /= 0_c_int8_t)
    pq_index%search_type = search_type
    pq_index%encode_signs = (encode_signs /= 0_c_int8_t)
    pq_index%polysemous_ht = polysemous_ht
  end subroutine load_pq_index

  subroutine save_flat_index(stream, flat_index)
    type(IoStream), intent(inout) :: stream
    type(FlatIndex), intent(in) :: flat_index
    integer(int32) :: metric_type
    integer(int32) :: fourcc_code
    integer(int32) :: dim
    integer(int64) :: ntotal
    integer(int64) :: float_count
    integer(int64) :: bytes_to_write
    integer(c_int8_t) :: trained_flag
    integer(c_size_t) :: size_value

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
  end subroutine save_flat_index

  subroutine save_pq_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(in) :: index
    type(PQIndex), pointer :: pq_index
    integer(int32) :: metric_type
    integer(int32) :: fourcc_code
    integer(int32) :: dim
    integer(int32) :: m
    integer(int32) :: nbits
    integer(int32) :: code_size
    integer(int32) :: dsub
    integer(int64) :: ntotal
    integer(int64) :: float_count
    integer(int64) :: code_count
    integer(int64) :: bytes_to_write
    integer(c_int8_t) :: trained_flag
    integer(c_int8_t) :: encode_signs
    integer(c_size_t) :: size_value

    call pq_handle(index, pq_index)
    if (.not. associated(pq_index)) then
      error stop "save_faiss_index only supports flat or PQ indices"
    end if

    dim = pq_index%dim
    m = pq_index%m
    nbits = pq_index%nbits
    code_size = pq_index%code_size
    ntotal = pq_index%ntotal

    if (dim <= 0_int32 .or. m <= 0_int32 .or. nbits <= 0_int32) then
      error stop "Invalid PQ index parameters"
    end if

    if (mod(dim, m) /= 0_int32) then
      error stop "PQ dimension not divisible by m"
    end if

    dsub = dim / m
    if (code_size <= 0_int32) then
      code_size = int((nbits * m + 7_int32) / 8_int32, int32)
    end if
    if (code_size <= 0_int32) then
      error stop "Invalid PQ code size"
    end if

    if (pq_index%codes%length > 0_int64) then
      ntotal = pq_index%codes%length
    end if

    if (pq_index%metric == METRIC_IP) then
      metric_type = FAISS_METRIC_IP
    else if (pq_index%metric == METRIC_L2) then
      metric_type = FAISS_METRIC_L2
    else
      error stop "Unsupported PQ metric"
    end if

    fourcc_code = fourcc_from_string("IxPq")

    call write_int32(stream, fourcc_code)
    call write_int32(stream, dim)
    call write_int64(stream, ntotal)
    call write_int64(stream, INDEX_DUMMY)
    call write_int64(stream, INDEX_DUMMY)
    trained_flag = 0_c_int8_t
    if (pq_index%is_trained) trained_flag = 1_c_int8_t
    call write_int8(stream, trained_flag)
    call write_int32(stream, metric_type)

    call write_int32(stream, dim)
    call write_int32(stream, m)
    call write_int32(stream, nbits)

    float_count = int(dim, int64) * int(2_int64**nbits, int64)
    if (.not. c_associated(pq_index%codebooks%data)) then
      error stop "PQ codebooks missing"
    end if

    if (pq_index%codebooks%dim /= dsub) then
      error stop "PQ codebook dimension mismatch"
    end if

    if (pq_index%codebooks%length * int(pq_index%codebooks%dim, int64) /= float_count) then
      error stop "PQ codebook shape mismatch"
    end if

    size_value = int(float_count, c_size_t)
    call write_size(stream, size_value)
    bytes_to_write = float_count * BYTES_REAL32
    call write_bytes(stream, pq_index%codebooks%data, bytes_to_write)

    if (.not. c_associated(pq_index%codes%data) .and. ntotal > 0_int64) then
      error stop "PQ codes missing"
    end if

    code_count = ntotal * int(code_size, int64)
    size_value = int(code_count, c_size_t)
    call write_size(stream, size_value)
    if (code_count > 0_int64) then
      bytes_to_write = code_count * BYTES_INT8
      call write_bytes(stream, pq_index%codes%data, bytes_to_write)
    end if

    call write_int32(stream, pq_index%search_type)
    encode_signs = 0_c_int8_t
    if (pq_index%encode_signs) encode_signs = 1_c_int8_t
    call write_int8(stream, encode_signs)
    call write_int32(stream, pq_index%polysemous_ht)
  end subroutine save_pq_index

  logical function is_flat_fourcc(code)
    integer(int32), intent(in) :: code

    is_flat_fourcc = (code == fourcc_from_string("IxFI")) .or. &
      (code == fourcc_from_string("IxF2")) .or. (code == fourcc_from_string("IxFl"))
  end function is_flat_fourcc

  logical function is_pq_fourcc(code)
    integer(int32), intent(in) :: code

    is_pq_fourcc = (code == fourcc_from_string("IxPq")) .or. &
      (code == fourcc_from_string("IxPQ")) .or. (code == fourcc_from_string("IxPo"))
  end function is_pq_fourcc

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
