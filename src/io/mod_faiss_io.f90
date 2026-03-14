module glamin_faiss_io
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_int8_t, c_int32_t, c_loc, &
    c_null_ptr, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK
  use glamin_index_flat, only: FlatIndex, flat_create_handle, flat_handle
  use glamin_index_ivf, only: IvfIndex, ivf_create_handle, ivf_handle
  use glamin_index_ivfpq, only: IvfProductQuantizerIndex, ivfpq_create_handle, ivfpq_handle
  use glamin_index_hnsw, only: HnswIndex, hnsw_create_handle, hnsw_handle
  use glamin_index_pq, only: PQIndex, pq_create_handle, pq_handle
  use glamin_memory, only: free_aligned
  use glamin_metrics, only: METRIC_IP, METRIC_L2
  use glamin_stream, only: IoStream, read_bytes, write_bytes
  use glamin_types, only: IndexHandle, VectorBlock
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
    else if (is_ivf_fourcc(fourcc_code)) then
      call load_ivf_index(stream, index)
    else if (is_ivfpq_fourcc(fourcc_code)) then
      call load_ivfpq_index(stream, index)
    else if (is_hnsw_fourcc(fourcc_code)) then
      call load_hnsw_index(stream, index)
    else
      error stop "Unsupported FAISS index type"
    end if
  end subroutine load_faiss_index

  subroutine save_faiss_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(in) :: index
    type(FlatIndex), pointer :: flat_index
    type(IvfIndex), pointer :: ivf_index
    type(IvfProductQuantizerIndex), pointer :: ivfpq_index
    type(HnswIndex), pointer :: hnsw_index

    call flat_handle(index, flat_index)
    if (associated(flat_index)) then
      call save_flat_index(stream, flat_index)
      return
    end if

    call ivf_handle(index, ivf_index)
    if (associated(ivf_index)) then
      call save_ivf_index(stream, ivf_index)
      return
    end if

    call ivfpq_handle(index, ivfpq_index)
    if (associated(ivfpq_index)) then
      call save_ivfpq_index(stream, ivfpq_index)
      return
    end if

    call hnsw_handle(index, hnsw_index)
    if (associated(hnsw_index)) then
      call save_hnsw_index(stream, hnsw_index)
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

  subroutine load_ivf_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(out) :: index
    type(IvfIndex), pointer :: ivf_index
    type(VectorBlock) :: quantizer_data
    integer(int32) :: dim
    integer(int32) :: metric
    integer(int32) :: status
    integer(int64) :: ntotal
    integer(int64) :: total_count
    integer(int64) :: expected_code_size
    integer(c_size_t) :: nlist_value
    integer(c_size_t) :: nprobe_value
    integer(c_size_t) :: code_size_value
    integer(int64) :: list_count
    integer(int32) :: list_index
    integer(int32) :: list_fourcc
    integer(int64) :: code_bytes
    integer(int64) :: id_bytes
    integer(int32) :: free_status
    type(c_ptr) :: code_buffer
    type(c_ptr) :: id_buffer
    integer(int64), allocatable :: sizes(:)
    integer(int64), pointer :: id_data(:)
    logical :: is_trained

    call read_index_header_fields(stream, dim, ntotal, metric, is_trained)
    call read_size(stream, nlist_value)
    call read_size(stream, nprobe_value)

    if (nlist_value <= 0_c_size_t) then
      error stop "Invalid IVF nlist"
    end if

    call read_flat_index_data(stream, quantizer_data)
    if (quantizer_data%dim /= dim) then
      error stop "IVF quantizer dimension mismatch"
    end if
    if (quantizer_data%length /= int(nlist_value, int64)) then
      error stop "IVF quantizer count mismatch"
    end if

    call read_direct_map_skip(stream)

    call read_int32(stream, list_fourcc)
    if (list_fourcc == fourcc_from_string("il00")) then
      call ivf_create_handle(index, dim, int(nlist_value, int32), metric, status)
      if (status /= GLAMIN_OK) then
        error stop "Failed to create IVF handle"
      end if
      call ivf_handle(index, ivf_index)
      ivf_index%centroids = quantizer_data
      ivf_index%is_trained = is_trained
      ivf_index%nprobe = int(nprobe_value, int32)
      ivf_index%ntotal = 0_int64
      return
    end if

    if (list_fourcc /= fourcc_from_string("ilar")) then
      error stop "Unsupported IVF inverted lists format"
    end if

    call read_size(stream, nlist_value)
    call read_size(stream, code_size_value)

    if (nlist_value /= int(ivf_index%nlist, c_size_t)) then
      error stop "IVF list count mismatch"
    end if

    expected_code_size = int(dim, int64) * BYTES_REAL32
    if (int(code_size_value, int64) /= expected_code_size) then
      error stop "IVF code size mismatch"
    end if

    allocate(sizes(int(nlist_value, int32)))
    call read_inverted_list_sizes(stream, sizes)

    call ivf_create_handle(index, dim, int(nlist_value, int32), metric, status)
    if (status /= GLAMIN_OK) then
      error stop "Failed to create IVF handle"
    end if
    call ivf_handle(index, ivf_index)
    ivf_index%centroids = quantizer_data
    ivf_index%is_trained = is_trained
    ivf_index%nprobe = int(nprobe_value, int32)

    total_count = 0_int64
    do list_index = 1_int32, size(sizes)
      list_count = sizes(list_index)
      if (list_count > 0_int64) then
        code_bytes = list_count * int(code_size_value, int64)
        call read_bytes(stream, code_buffer, code_bytes)

        ivf_index%lists(list_index)%vectors%data = code_buffer
        ivf_index%lists(list_index)%vectors%length = list_count
        ivf_index%lists(list_index)%vectors%dim = dim
        ivf_index%lists(list_index)%vectors%stride = dim
        ivf_index%lists(list_index)%vectors%elem_size = int(BYTES_REAL32, int32)
        ivf_index%lists(list_index)%vectors%alignment = 64
        ivf_index%lists(list_index)%count = list_count

        id_bytes = list_count * BYTES_INT64
        call read_bytes(stream, id_buffer, id_bytes)
        call c_f_pointer(id_buffer, id_data, [list_count])
        allocate(ivf_index%lists(list_index)%labels(list_count))
        ivf_index%lists(list_index)%labels = 0_int32
        call assign_labels_int64(ivf_index%lists(list_index)%labels, id_data, list_count)
        call free_aligned(id_buffer, free_status)
      end if
      total_count = total_count + list_count
    end do

    ivf_index%ntotal = total_count
    if (ntotal > 0_int64 .and. total_count /= ntotal) then
      error stop "IVF ntotal mismatch"
    end if
  end subroutine load_ivf_index

  subroutine load_ivfpq_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(out) :: index
    type(IvfProductQuantizerIndex), pointer :: ivfpq_index
    type(VectorBlock) :: quantizer_data
    type(VectorBlock) :: codebooks
    integer(int32) :: dim
    integer(int32) :: metric
    integer(int32) :: status
    integer(int32) :: pq_dim
    integer(int32) :: pq_m
    integer(int32) :: pq_nbits
    integer(int32) :: ksub
    integer(int64) :: ntotal
    integer(int64) :: total_count
    integer(int64) :: list_count
    integer(int32) :: list_index
    integer(int32) :: list_fourcc
    integer(c_int8_t) :: by_residual
    integer(c_size_t) :: nlist_value
    integer(c_size_t) :: nprobe_value
    integer(c_size_t) :: code_size_value
    integer(int64) :: code_bytes
    integer(int64) :: id_bytes
    integer(int32) :: free_status
    type(c_ptr) :: code_buffer
    type(c_ptr) :: id_buffer
    integer(int64), allocatable :: sizes(:)
    integer(int64), pointer :: id_data(:)
    logical :: is_trained

    call read_index_header_fields(stream, dim, ntotal, metric, is_trained)
    call read_size(stream, nlist_value)
    call read_size(stream, nprobe_value)

    if (nlist_value <= 0_c_size_t) then
      error stop "Invalid IVFPQ nlist"
    end if

    call read_flat_index_data(stream, quantizer_data)
    if (quantizer_data%dim /= dim) then
      error stop "IVFPQ quantizer dimension mismatch"
    end if
    if (quantizer_data%length /= int(nlist_value, int64)) then
      error stop "IVFPQ quantizer count mismatch"
    end if

    call read_direct_map_skip(stream)

    call read_int8(stream, by_residual)
    if (by_residual == 0_c_int8_t) then
      error stop "IVFPQ without residuals not supported"
    end if

    call read_size(stream, code_size_value)
    call read_product_quantizer(stream, pq_dim, pq_m, pq_nbits, codebooks)

    if (pq_dim /= dim) then
      error stop "IVFPQ PQ dim mismatch"
    end if

    ksub = 2_int32**pq_nbits
    if (int(code_size_value, int64) /= int(pq_m, int64)) then
      error stop "IVFPQ code size mismatch"
    end if

    call ivfpq_create_handle(index, dim, int(nlist_value, int32), pq_m, ksub, metric, status)
    if (status /= GLAMIN_OK) then
      error stop "Failed to create IVFPQ handle"
    end if
    call ivfpq_handle(index, ivfpq_index)
    ivfpq_index%centroids = quantizer_data
    ivfpq_index%codebooks = codebooks
    ivfpq_index%is_trained = is_trained
    ivfpq_index%nprobe = int(nprobe_value, int32)

    call read_int32(stream, list_fourcc)
    if (list_fourcc == fourcc_from_string("il00")) then
      ivfpq_index%ntotal = 0_int64
      return
    end if

    if (list_fourcc /= fourcc_from_string("ilar")) then
      error stop "Unsupported IVFPQ inverted lists format"
    end if

    call read_size(stream, nlist_value)
    call read_size(stream, code_size_value)

    if (nlist_value /= int(ivfpq_index%nlist, c_size_t)) then
      error stop "IVFPQ list count mismatch"
    end if

    if (int(code_size_value, int64) /= int(pq_m, int64)) then
      error stop "IVFPQ list code size mismatch"
    end if

    allocate(sizes(int(nlist_value, int32)))
    call read_inverted_list_sizes(stream, sizes)

    total_count = 0_int64
    do list_index = 1_int32, size(sizes)
      list_count = sizes(list_index)
      if (list_count > 0_int64) then
        code_bytes = list_count * int(code_size_value, int64)
        call read_bytes(stream, code_buffer, code_bytes)

        ivfpq_index%lists(list_index)%codes%data = code_buffer
        ivfpq_index%lists(list_index)%codes%length = list_count
        ivfpq_index%lists(list_index)%codes%dim = int(code_size_value, int32)
        ivfpq_index%lists(list_index)%codes%stride = int(code_size_value, int32)
        ivfpq_index%lists(list_index)%codes%elem_size = 1_int32
        ivfpq_index%lists(list_index)%codes%alignment = 64
        ivfpq_index%lists(list_index)%count = list_count

        id_bytes = list_count * BYTES_INT64
        call read_bytes(stream, id_buffer, id_bytes)
        call c_f_pointer(id_buffer, id_data, [list_count])
        allocate(ivfpq_index%lists(list_index)%labels(list_count))
        ivfpq_index%lists(list_index)%labels = 0_int32
        call assign_labels_int64(ivfpq_index%lists(list_index)%labels, id_data, list_count)
        call free_aligned(id_buffer, free_status)
      end if
      total_count = total_count + list_count
    end do

    ivfpq_index%ntotal = total_count
    if (ntotal > 0_int64 .and. total_count /= ntotal) then
      error stop "IVFPQ ntotal mismatch"
    end if
  end subroutine load_ivfpq_index

  subroutine load_hnsw_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(out) :: index
    type(HnswIndex), pointer :: hnsw_index
    type(VectorBlock) :: storage_data
    integer(int32) :: dim
    integer(int32) :: metric
    integer(int32) :: status
    integer(int64) :: ntotal
    integer(int32) :: entry_point
    integer(int32) :: max_level
    integer(int32) :: ef_construction
    integer(int32) :: ef_search
    integer(int32) :: upper_beam
    integer(int32) :: list_index
    integer(int32) :: neighbor_index
    integer(int64) :: base_offset
    integer(int32) :: cap
    integer(int32), allocatable :: levels(:)
    integer(int32), allocatable :: neighbors(:)
    integer(int32), allocatable :: cum_neighbors(:)
    integer(int64), allocatable :: offsets(:)
    real(real32), allocatable :: assign_probas(:)
    logical :: is_trained

    call read_index_header_fields(stream, dim, ntotal, metric, is_trained)
    call read_vector_real32(stream, assign_probas)
    call read_vector_int32(stream, cum_neighbors)
    call read_vector_int32(stream, levels)
    call read_vector_int64(stream, offsets)
    call read_vector_int32(stream, neighbors)

    call read_int32(stream, entry_point)
    call read_int32(stream, max_level)
    call read_int32(stream, ef_construction)
    call read_int32(stream, ef_search)
    call read_int32(stream, upper_beam)

    if (size(cum_neighbors) >= 2) then
      cap = cum_neighbors(2) - cum_neighbors(1)
    else
      cap = 1_int32
    end if
    if (cap <= 0_int32) then
      cap = 1_int32
    end if

    call hnsw_create_handle(index, dim, cap, ef_construction, metric, status)
    if (status /= GLAMIN_OK) then
      error stop "Failed to create HNSW handle"
    end if
    call hnsw_handle(index, hnsw_index)
    hnsw_index%ef_search = ef_search

    call read_flat_index_data(stream, storage_data)
    if (storage_data%dim /= dim) then
      error stop "HNSW storage dimension mismatch"
    end if
    if (ntotal > 0_int64 .and. storage_data%length /= ntotal) then
      error stop "HNSW storage count mismatch"
    end if

    hnsw_index%data = storage_data
    hnsw_index%ntotal = storage_data%length

    if (hnsw_index%ntotal <= 0_int64) then
      return
    end if

    if (size(levels) /= int(hnsw_index%ntotal, int32)) then
      error stop "HNSW levels size mismatch"
    end if

    if (size(offsets) /= int(hnsw_index%ntotal + 1_int64, int32)) then
      error stop "HNSW offsets size mismatch"
    end if

    if (size(cum_neighbors) < 2) then
      error stop "HNSW neighbor levels missing"
    end if

    if (cum_neighbors(1) /= 0_int32) then
      error stop "HNSW neighbor base mismatch"
    end if

    if (maxval(levels) > 1_int32) then
      error stop "Multi-level HNSW not supported"
    end if

    cap = cum_neighbors(2) - cum_neighbors(1)
    if (cap <= 0_int32) then
      error stop "Invalid HNSW neighbor capacity"
    end if
    if (cap /= hnsw_index%m) then
      error stop "HNSW neighbor capacity mismatch"
    end if

    allocate(hnsw_index%neighbors(cap, int(hnsw_index%ntotal, int32)))
    allocate(hnsw_index%neighbor_counts(int(hnsw_index%ntotal, int32)))
    hnsw_index%neighbors = 0_int32
    hnsw_index%neighbor_counts = 0_int32

    do list_index = 1_int32, int(hnsw_index%ntotal, int32)
      base_offset = offsets(list_index) + 1_int64
      do neighbor_index = 1_int32, cap
        if (base_offset + neighbor_index - 1_int64 > int(size(neighbors), int64)) then
          exit
        end if
        if (neighbors(base_offset + neighbor_index - 1_int64) >= 0_int32) then
          hnsw_index%neighbors(neighbor_index, list_index) = &
            neighbors(base_offset + neighbor_index - 1_int64) + 1_int32
          hnsw_index%neighbor_counts(list_index) = hnsw_index%neighbor_counts(list_index) + 1_int32
        end if
      end do
    end do
  end subroutine load_hnsw_index

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

  subroutine save_ivf_index(stream, ivf_index)
    type(IoStream), intent(inout) :: stream
    type(IvfIndex), intent(in) :: ivf_index
    type(FlatIndex) :: quantizer
    integer(int32) :: fourcc_code
    integer(int32) :: dim
    integer(int32) :: list_index
    integer(int64) :: ntotal
    integer(int64) :: total_count
    integer(int64) :: expected_code_size
    integer(int64) :: list_count
    integer(int64) :: code_bytes
    integer(int64) :: id_bytes
    integer(c_size_t) :: size_value
    integer(int64), allocatable :: sizes(:)
    integer(int64), allocatable, target :: ids(:)

    dim = ivf_index%dim
    if (dim <= 0_int32 .or. ivf_index%nlist <= 0_int32) then
      error stop "Invalid IVF index parameters"
    end if

    if (.not. c_associated(ivf_index%centroids%data)) then
      error stop "IVF centroids missing"
    end if
    if (ivf_index%centroids%length /= int(ivf_index%nlist, int64)) then
      error stop "IVF centroid count mismatch"
    end if

    expected_code_size = int(dim, int64) * BYTES_REAL32

    total_count = 0_int64
    allocate(sizes(ivf_index%nlist))
    do list_index = 1_int32, ivf_index%nlist
      list_count = ivf_index%lists(list_index)%count
      if (list_count <= 0_int64) then
        list_count = ivf_index%lists(list_index)%vectors%length
      end if
      sizes(list_index) = list_count
      total_count = total_count + list_count
    end do

    if (ivf_index%ntotal > 0_int64) then
      if (ivf_index%ntotal /= total_count) then
        error stop "IVF ntotal mismatch"
      end if
      ntotal = ivf_index%ntotal
    else
      ntotal = total_count
    end if

    fourcc_code = fourcc_from_string("IwFl")
    call write_int32(stream, fourcc_code)
    call write_index_header_fields(stream, dim, ntotal, ivf_index%metric, ivf_index%is_trained)

    size_value = int(ivf_index%nlist, c_size_t)
    call write_size(stream, size_value)
    size_value = int(ivf_index%nprobe, c_size_t)
    call write_size(stream, size_value)

    quantizer%dim = dim
    quantizer%metric = ivf_index%metric
    quantizer%data = ivf_index%centroids
    call save_flat_index(stream, quantizer)

    call write_direct_map_empty(stream)

    call write_int32(stream, fourcc_from_string("ilar"))
    size_value = int(ivf_index%nlist, c_size_t)
    call write_size(stream, size_value)
    size_value = int(expected_code_size, c_size_t)
    call write_size(stream, size_value)
    call write_int32(stream, fourcc_from_string("full"))
    call write_vector_size_t(stream, sizes)

    do list_index = 1_int32, ivf_index%nlist
      list_count = sizes(list_index)
      if (list_count > 0_int64) then
        if (.not. c_associated(ivf_index%lists(list_index)%vectors%data)) then
          error stop "IVF list vectors missing"
        end if
        if (.not. allocated(ivf_index%lists(list_index)%labels)) then
          error stop "IVF list labels missing"
        end if
        code_bytes = list_count * expected_code_size
        call write_bytes(stream, ivf_index%lists(list_index)%vectors%data, code_bytes)

        allocate(ids(list_count))
        call pack_labels_int64(ids, ivf_index%lists(list_index)%labels, list_count)
        id_bytes = list_count * BYTES_INT64
        call write_bytes(stream, c_loc(ids), id_bytes)
        deallocate(ids)
      end if
    end do
  end subroutine save_ivf_index

  subroutine save_ivfpq_index(stream, ivfpq_index)
    type(IoStream), intent(inout) :: stream
    type(IvfProductQuantizerIndex), intent(in) :: ivfpq_index
    type(FlatIndex) :: quantizer
    integer(int32) :: fourcc_code
    integer(int32) :: dim
    integer(int32) :: list_index
    integer(int32) :: nbits
    integer(int32) :: ksub
    integer(int64) :: ntotal
    integer(int64) :: total_count
    integer(int64) :: list_count
    integer(int64) :: code_bytes
    integer(int64) :: id_bytes
    integer(c_int8_t) :: by_residual
    integer(c_size_t) :: size_value
    integer(int64), allocatable :: sizes(:)
    integer(int64), allocatable, target :: ids(:)

    dim = ivfpq_index%dim
    if (dim <= 0_int32 .or. ivfpq_index%nlist <= 0_int32) then
      error stop "Invalid IVFPQ index parameters"
    end if

    if (.not. c_associated(ivfpq_index%centroids%data)) then
      error stop "IVFPQ centroids missing"
    end if
    if (ivfpq_index%centroids%length /= int(ivfpq_index%nlist, int64)) then
      error stop "IVFPQ centroid count mismatch"
    end if

    if (.not. c_associated(ivfpq_index%codebooks%data)) then
      error stop "IVFPQ codebooks missing"
    end if

    ksub = ivfpq_index%ksub
    nbits = compute_nbits(ksub)

    total_count = 0_int64
    allocate(sizes(ivfpq_index%nlist))
    do list_index = 1_int32, ivfpq_index%nlist
      list_count = ivfpq_index%lists(list_index)%count
      if (list_count <= 0_int64) then
        list_count = ivfpq_index%lists(list_index)%codes%length
      end if
      sizes(list_index) = list_count
      total_count = total_count + list_count
    end do

    if (ivfpq_index%ntotal > 0_int64) then
      if (ivfpq_index%ntotal /= total_count) then
        error stop "IVFPQ ntotal mismatch"
      end if
      ntotal = ivfpq_index%ntotal
    else
      ntotal = total_count
    end if

    fourcc_code = fourcc_from_string("IwPQ")
    call write_int32(stream, fourcc_code)
    call write_index_header_fields(stream, dim, ntotal, ivfpq_index%metric, ivfpq_index%is_trained)

    size_value = int(ivfpq_index%nlist, c_size_t)
    call write_size(stream, size_value)
    size_value = int(ivfpq_index%nprobe, c_size_t)
    call write_size(stream, size_value)

    quantizer%dim = dim
    quantizer%metric = ivfpq_index%metric
    quantizer%data = ivfpq_index%centroids
    call save_flat_index(stream, quantizer)

    call write_direct_map_empty(stream)

    by_residual = 1_c_int8_t
    call write_int8(stream, by_residual)
    size_value = int(ivfpq_index%m, c_size_t)
    call write_size(stream, size_value)
    call write_product_quantizer(stream, dim, ivfpq_index%m, nbits, ivfpq_index%codebooks)

    call write_int32(stream, fourcc_from_string("ilar"))
    size_value = int(ivfpq_index%nlist, c_size_t)
    call write_size(stream, size_value)
    size_value = int(ivfpq_index%m, c_size_t)
    call write_size(stream, size_value)
    call write_int32(stream, fourcc_from_string("full"))
    call write_vector_size_t(stream, sizes)

    do list_index = 1_int32, ivfpq_index%nlist
      list_count = sizes(list_index)
      if (list_count > 0_int64) then
        if (.not. c_associated(ivfpq_index%lists(list_index)%codes%data)) then
          error stop "IVFPQ list codes missing"
        end if
        if (.not. allocated(ivfpq_index%lists(list_index)%labels)) then
          error stop "IVFPQ list labels missing"
        end if
        code_bytes = list_count * int(ivfpq_index%m, int64)
        call write_bytes(stream, ivfpq_index%lists(list_index)%codes%data, code_bytes)

        allocate(ids(list_count))
        call pack_labels_int64(ids, ivfpq_index%lists(list_index)%labels, list_count)
        id_bytes = list_count * BYTES_INT64
        call write_bytes(stream, c_loc(ids), id_bytes)
        deallocate(ids)
      end if
    end do
  end subroutine save_ivfpq_index

  subroutine save_hnsw_index(stream, hnsw_index)
    type(IoStream), intent(inout) :: stream
    type(HnswIndex), intent(in) :: hnsw_index
    type(FlatIndex) :: storage
    integer(int32) :: fourcc_code
    integer(int32) :: dim
    integer(int32) :: entry_point
    integer(int32) :: max_level
    integer(int32) :: ef_construction
    integer(int32) :: ef_search
    integer(int32) :: upper_beam
    integer(int32) :: neighbor_index
    integer(int32) :: node_index
    integer(int64) :: ntotal
    integer(int32) :: cap
    integer(int64) :: offset_value
    integer(int32) :: neighbor_count
    integer(int64), allocatable :: offsets(:)
    integer(int32), allocatable :: levels(:)
    integer(int32), allocatable :: cum_neighbors(:)
    integer(int32), allocatable :: neighbors(:)
    real(real32), allocatable :: assign_probas(:)

    dim = hnsw_index%dim
    ntotal = hnsw_index%ntotal
    cap = hnsw_index%m
    if (dim <= 0_int32 .or. cap <= 0_int32) then
      error stop "Invalid HNSW index parameters"
    end if

    if (.not. c_associated(hnsw_index%data%data)) then
      error stop "HNSW storage data missing"
    end if
    if (hnsw_index%data%length /= ntotal) then
      error stop "HNSW ntotal mismatch"
    end if
    if (.not. allocated(hnsw_index%neighbors)) then
      error stop "HNSW neighbors missing"
    end if
    if (.not. allocated(hnsw_index%neighbor_counts)) then
      error stop "HNSW neighbor counts missing"
    end if
    if (size(hnsw_index%neighbors, 1) /= cap .or. &
        size(hnsw_index%neighbors, 2) /= int(ntotal, int32)) then
      error stop "HNSW neighbor shape mismatch"
    end if

    fourcc_code = fourcc_from_string("IHNf")
    call write_int32(stream, fourcc_code)
    call write_index_header_fields(stream, dim, ntotal, hnsw_index%metric, .true.)

    allocate(assign_probas(1))
    assign_probas(1) = 1.0_real32
    call write_vector_real32(stream, assign_probas)

    allocate(cum_neighbors(2))
    cum_neighbors(1) = 0_int32
    cum_neighbors(2) = cap
    call write_vector_int32(stream, cum_neighbors)

    allocate(levels(int(ntotal, int32)))
    levels = 1_int32
    call write_vector_int32(stream, levels)

    allocate(offsets(int(ntotal + 1_int64, int32)))
    offsets(1) = 0_int64
    do node_index = 2_int32, size(offsets)
      offsets(node_index) = offsets(node_index - 1_int32) + int(cap, int64)
    end do
    call write_vector_int64(stream, offsets)

    allocate(neighbors(int(ntotal * int(cap, int64), int32)))
    neighbors = -1_int32
    do node_index = 1_int32, int(ntotal, int32)
      offset_value = offsets(node_index) + 1_int64
      neighbor_count = min(cap, hnsw_index%neighbor_counts(node_index))
      do neighbor_index = 1_int32, neighbor_count
        neighbors(offset_value + neighbor_index - 1_int64) = &
          hnsw_index%neighbors(neighbor_index, node_index) - 1_int32
      end do
    end do
    call write_vector_int32(stream, neighbors)

    if (ntotal > 0_int64) then
      entry_point = 0_int32
      max_level = 0_int32
    else
      entry_point = -1_int32
      max_level = -1_int32
    end if
    ef_construction = max(1_int32, hnsw_index%ef_construction)
    ef_search = max(1_int32, hnsw_index%ef_search)
    upper_beam = 1_int32

    call write_int32(stream, entry_point)
    call write_int32(stream, max_level)
    call write_int32(stream, ef_construction)
    call write_int32(stream, ef_search)
    call write_int32(stream, upper_beam)

    storage%dim = dim
    storage%metric = hnsw_index%metric
    storage%data = hnsw_index%data
    call save_flat_index(stream, storage)
  end subroutine save_hnsw_index

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

  logical function is_ivf_fourcc(code)
    integer(int32), intent(in) :: code

    is_ivf_fourcc = (code == fourcc_from_string("IwFl"))
  end function is_ivf_fourcc

  logical function is_ivfpq_fourcc(code)
    integer(int32), intent(in) :: code

    is_ivfpq_fourcc = (code == fourcc_from_string("IwPQ"))
  end function is_ivfpq_fourcc

  logical function is_hnsw_fourcc(code)
    integer(int32), intent(in) :: code

    is_hnsw_fourcc = (code == fourcc_from_string("IHNf"))
  end function is_hnsw_fourcc

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

  subroutine read_index_header_fields(stream, dim, ntotal, metric, is_trained)
    type(IoStream), intent(inout) :: stream
    integer(int32), intent(out) :: dim
    integer(int64), intent(out) :: ntotal
    integer(int32), intent(out) :: metric
    logical, intent(out) :: is_trained
    integer(int32) :: metric_type
    integer(int64) :: dummy
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
      metric = METRIC_IP
    else if (metric_type == FAISS_METRIC_L2) then
      metric = METRIC_L2
    else
      error stop "Unsupported FAISS metric type"
    end if

    is_trained = (trained_flag /= 0_c_int8_t)
  end subroutine read_index_header_fields

  subroutine write_index_header_fields(stream, dim, ntotal, metric, is_trained)
    type(IoStream), intent(inout) :: stream
    integer(int32), intent(in) :: dim
    integer(int64), intent(in) :: ntotal
    integer(int32), intent(in) :: metric
    logical, intent(in) :: is_trained
    integer(int32) :: metric_type
    integer(c_int8_t) :: trained_flag

    if (metric == METRIC_IP) then
      metric_type = FAISS_METRIC_IP
    else if (metric == METRIC_L2) then
      metric_type = FAISS_METRIC_L2
    else
      error stop "Unsupported FAISS metric type"
    end if

    trained_flag = 0_c_int8_t
    if (is_trained) trained_flag = 1_c_int8_t

    call write_int32(stream, dim)
    call write_int64(stream, ntotal)
    call write_int64(stream, INDEX_DUMMY)
    call write_int64(stream, INDEX_DUMMY)
    call write_int8(stream, trained_flag)
    call write_int32(stream, metric_type)
  end subroutine write_index_header_fields

  subroutine read_flat_index_data(stream, data)
    type(IoStream), intent(inout) :: stream
    type(VectorBlock), intent(out) :: data
    integer(int32) :: fourcc_code
    integer(int32) :: dim
    integer(int32) :: metric
    integer(int64) :: ntotal
    integer(int64) :: float_count
    integer(int64) :: expected_count
    integer(int64) :: bytes_to_read
    integer(c_size_t) :: size_value
    logical :: is_trained
    type(c_ptr) :: buffer

    call read_int32(stream, fourcc_code)
    if (.not. is_flat_fourcc(fourcc_code)) then
      error stop "Unsupported FAISS quantizer index"
    end if

    call read_index_header_fields(stream, dim, ntotal, metric, is_trained)

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

    data%data = buffer
    data%length = ntotal
    data%dim = dim
    data%stride = dim
    data%elem_size = int(BYTES_REAL32, int32)
    data%alignment = 64
  end subroutine read_flat_index_data

  subroutine read_direct_map_skip(stream)
    type(IoStream), intent(inout) :: stream
    integer(c_int8_t) :: map_type
    integer(c_size_t) :: size_value
    integer(int64) :: bytes_to_read
    type(c_ptr) :: buffer
    integer(int32) :: free_status

    call read_int8(stream, map_type)
    call read_size(stream, size_value)
    if (size_value > 0_c_size_t) then
      bytes_to_read = int(size_value, int64) * BYTES_INT64
      call read_bytes(stream, buffer, bytes_to_read)
      call free_aligned(buffer, free_status)
    end if

    if (map_type == 2_c_int8_t) then
      call read_size(stream, size_value)
      if (size_value > 0_c_size_t) then
        bytes_to_read = int(size_value, int64) * 2_int64 * BYTES_INT64
        call read_bytes(stream, buffer, bytes_to_read)
        call free_aligned(buffer, free_status)
      end if
    end if
  end subroutine read_direct_map_skip

  subroutine write_direct_map_empty(stream)
    type(IoStream), intent(inout) :: stream
    integer(c_int8_t) :: map_type
    integer(c_size_t) :: size_value

    map_type = 0_c_int8_t
    size_value = 0_c_size_t
    call write_int8(stream, map_type)
    call write_size(stream, size_value)
  end subroutine write_direct_map_empty

  subroutine read_inverted_list_sizes(stream, sizes)
    type(IoStream), intent(inout) :: stream
    integer(int64), intent(inout) :: sizes(:)
    integer(int32) :: list_type
    integer(int64), allocatable :: idsizes(:)
    integer(int64), allocatable :: read_sizes(:)
    integer(int64) :: index

    sizes = 0_int64
    call read_int32(stream, list_type)
    if (list_type == fourcc_from_string("full")) then
      call read_vector_int64(stream, read_sizes)
      if (size(read_sizes) /= size(sizes)) then
        error stop "Inverted list size mismatch"
      end if
      sizes = read_sizes
    else if (list_type == fourcc_from_string("sprs")) then
      call read_vector_int64(stream, idsizes)
      if (mod(size(idsizes), 2) /= 0) then
        error stop "Invalid sparse inverted list size"
      end if
      do index = 1_int64, int(size(idsizes), int64), 2_int64
        if (idsizes(index) + 1_int64 > size(sizes)) then
          error stop "Sparse inverted list index out of range"
        end if
        sizes(idsizes(index) + 1_int64) = idsizes(index + 1_int64)
      end do
    else
      error stop "Unsupported inverted list format"
    end if
  end subroutine read_inverted_list_sizes

  subroutine read_product_quantizer(stream, dim, m, nbits, codebooks)
    type(IoStream), intent(inout) :: stream
    integer(int32), intent(out) :: dim
    integer(int32), intent(out) :: m
    integer(int32), intent(out) :: nbits
    type(VectorBlock), intent(out) :: codebooks
    integer(int32) :: dsub
    integer(int32) :: ksub
    integer(int64) :: float_count
    integer(int64) :: expected_count
    integer(int64) :: bytes_to_read
    integer(c_size_t) :: size_value
    type(c_ptr) :: buffer

    call read_int32(stream, dim)
    call read_int32(stream, m)
    call read_int32(stream, nbits)

    if (dim <= 0_int32 .or. m <= 0_int32 .or. nbits <= 0_int32) then
      error stop "Invalid PQ parameters"
    end if

    if (mod(dim, m) /= 0_int32) then
      error stop "PQ dimension not divisible"
    end if

    dsub = dim / m
    ksub = 2_int32**nbits

    call read_size(stream, size_value)
    float_count = int(size_value, int64)
    expected_count = int(dim, int64) * int(ksub, int64)
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

    codebooks%data = buffer
    codebooks%length = int(m, int64) * int(ksub, int64)
    codebooks%dim = dsub
    codebooks%stride = dsub
    codebooks%elem_size = int(BYTES_REAL32, int32)
    codebooks%alignment = 64
  end subroutine read_product_quantizer

  subroutine write_product_quantizer(stream, dim, m, nbits, codebooks)
    type(IoStream), intent(inout) :: stream
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: nbits
    type(VectorBlock), intent(in) :: codebooks
    integer(int32) :: ksub
    integer(int64) :: float_count
    integer(int64) :: expected_count
    integer(int64) :: bytes_to_write
    integer(c_size_t) :: size_value

    if (.not. c_associated(codebooks%data)) then
      error stop "PQ codebooks missing"
    end if

    if (mod(dim, m) /= 0_int32) then
      error stop "PQ dimension not divisible"
    end if

    ksub = 2_int32**nbits
    float_count = int(dim, int64) * int(ksub, int64)
    expected_count = int(codebooks%length, int64) * int(codebooks%dim, int64)
    if (float_count /= expected_count) then
      error stop "PQ codebook shape mismatch"
    end if

    call write_int32(stream, dim)
    call write_int32(stream, m)
    call write_int32(stream, nbits)

    size_value = int(float_count, c_size_t)
    call write_size(stream, size_value)

    bytes_to_write = float_count * BYTES_REAL32
    call write_bytes(stream, codebooks%data, bytes_to_write)
  end subroutine write_product_quantizer

  subroutine assign_labels_int64(labels, ids, count)
    integer(int32), intent(out) :: labels(:)
    integer(int64), intent(in) :: ids(:)
    integer(int64), intent(in) :: count
    integer(int64) :: index

    if (count <= 0_int64) return
    do index = 1_int64, count
      if (ids(index) < 0_int64) then
        error stop "Negative id in FAISS list"
      end if
      if (ids(index) > huge(0_int32)) then
        error stop "FAISS id overflow"
      end if
      labels(index) = int(ids(index) + 1_int64, int32)
    end do
  end subroutine assign_labels_int64

  subroutine pack_labels_int64(ids, labels, count)
    integer(int64), intent(out) :: ids(:)
    integer(int32), intent(in) :: labels(:)
    integer(int64), intent(in) :: count
    integer(int64) :: index

    if (count <= 0_int64) return
    do index = 1_int64, count
      if (labels(index) <= 0_int32) then
        error stop "Invalid IVF label"
      else
        ids(index) = int(labels(index) - 1_int32, int64)
      end if
    end do
  end subroutine pack_labels_int64

  integer(int32) function compute_nbits(ksub)
    integer(int32), intent(in) :: ksub
    integer(int32) :: value

    if (ksub <= 0_int32) then
      error stop "Invalid ksub"
    end if

    compute_nbits = 0_int32
    value = ksub
    do while (value > 1_int32)
      if (mod(value, 2_int32) /= 0_int32) then
        error stop "ksub is not power of two"
      end if
      value = value / 2_int32
      compute_nbits = compute_nbits + 1_int32
    end do
  end function compute_nbits

  subroutine read_vector_real32(stream, values)
    type(IoStream), intent(inout) :: stream
    real(real32), allocatable, intent(out) :: values(:)
    integer(c_size_t) :: size_value
    integer(int64) :: count
    type(c_ptr) :: buffer
    real(real32), pointer :: data(:)
    integer(int32) :: free_status

    call read_size(stream, size_value)
    count = int(size_value, int64)
    allocate(values(count))
    if (count <= 0_int64) then
      return
    end if

    call read_bytes(stream, buffer, count * BYTES_REAL32)
    call c_f_pointer(buffer, data, [count])
    values = data
    call free_aligned(buffer, free_status)
  end subroutine read_vector_real32

  subroutine read_vector_int32(stream, values)
    type(IoStream), intent(inout) :: stream
    integer(int32), allocatable, intent(out) :: values(:)
    integer(c_size_t) :: size_value
    integer(int64) :: count
    type(c_ptr) :: buffer
    integer(int32), pointer :: data(:)
    integer(int32) :: free_status

    call read_size(stream, size_value)
    count = int(size_value, int64)
    allocate(values(count))
    if (count <= 0_int64) then
      return
    end if

    call read_bytes(stream, buffer, count * BYTES_INT32)
    call c_f_pointer(buffer, data, [count])
    values = data
    call free_aligned(buffer, free_status)
  end subroutine read_vector_int32

  subroutine read_vector_int64(stream, values)
    type(IoStream), intent(inout) :: stream
    integer(int64), allocatable, intent(out) :: values(:)
    integer(c_size_t) :: size_value
    integer(int64) :: count
    type(c_ptr) :: buffer
    integer(int64), pointer :: data(:)
    integer(int32) :: free_status

    call read_size(stream, size_value)
    count = int(size_value, int64)
    allocate(values(count))
    if (count <= 0_int64) then
      return
    end if

    call read_bytes(stream, buffer, count * BYTES_INT64)
    call c_f_pointer(buffer, data, [count])
    values = data
    call free_aligned(buffer, free_status)
  end subroutine read_vector_int64

  subroutine write_vector_real32(stream, values)
    type(IoStream), intent(inout) :: stream
    real(real32), intent(in), target :: values(:)
    integer(c_size_t) :: size_value
    integer(int64) :: count

    count = size(values)
    size_value = int(count, c_size_t)
    call write_size(stream, size_value)
    if (count <= 0_int64) then
      return
    end if
    call write_bytes(stream, c_loc(values), count * BYTES_REAL32)
  end subroutine write_vector_real32

  subroutine write_vector_int32(stream, values)
    type(IoStream), intent(inout) :: stream
    integer(int32), intent(in), target :: values(:)
    integer(c_size_t) :: size_value
    integer(int64) :: count

    count = size(values)
    size_value = int(count, c_size_t)
    call write_size(stream, size_value)
    if (count <= 0_int64) then
      return
    end if
    call write_bytes(stream, c_loc(values), count * BYTES_INT32)
  end subroutine write_vector_int32

  subroutine write_vector_int64(stream, values)
    type(IoStream), intent(inout) :: stream
    integer(int64), intent(in), target :: values(:)
    integer(c_size_t) :: size_value
    integer(int64) :: count

    count = size(values)
    size_value = int(count, c_size_t)
    call write_size(stream, size_value)
    if (count <= 0_int64) then
      return
    end if
    call write_bytes(stream, c_loc(values), count * BYTES_INT64)
  end subroutine write_vector_int64

  subroutine write_vector_size_t(stream, values)
    type(IoStream), intent(inout) :: stream
    integer(int64), intent(in) :: values(:)
    integer(c_size_t), allocatable, target :: size_values(:)
    integer(c_size_t) :: size_value
    integer(int64) :: count
    integer(int64) :: index

    count = size(values)
    size_value = int(count, c_size_t)
    call write_size(stream, size_value)
    if (count <= 0_int64) then
      return
    end if

    allocate(size_values(count))
    do index = 1_int64, count
      size_values(index) = int(values(index), c_size_t)
    end do

    call write_bytes(stream, c_loc(size_values), count * BYTES_SIZE_T)
    deallocate(size_values)
  end subroutine write_vector_size_t

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
