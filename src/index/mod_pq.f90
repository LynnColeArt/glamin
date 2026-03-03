module glamin_index_pq
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_int8_t, c_loc, c_null_ptr, c_ptr, &
    c_size_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_IP, METRIC_L2
  use glamin_types, only: VectorBlock, IndexHandle, INDEX_KIND_PQ, INDEX_KIND_UNKNOWN
  implicit none
  private

  public :: ProductQuantizerCodebook
  public :: PQIndex
  public :: pq_create_handle
  public :: pq_destroy_handle
  public :: pq_handle
  public :: pq_set_codebooks
  public :: pq_add
  public :: pq_search
  public :: pq_create
  public :: pq_train
  public :: pq_encode
  public :: pq_decode

  type :: ProductQuantizerCodebook
    integer(int32) :: dim = 0
    integer(int32) :: m = 0
    integer(int32) :: ksub = 0
    type(VectorBlock) :: codebooks
  end type ProductQuantizerCodebook

  type :: PQIndex
    integer(int32) :: dim = 0
    integer(int32) :: metric = 0
    integer(int32) :: m = 0
    integer(int32) :: nbits = 0
    integer(int32) :: code_size = 0
    integer(int64) :: ntotal = 0
    logical :: is_trained = .false.
    integer(int32) :: search_type = 0
    logical :: encode_signs = .false.
    integer(int32) :: polysemous_ht = 0
    type(VectorBlock) :: codebooks
    type(VectorBlock) :: codes
  end type PQIndex

contains
  subroutine pq_set_codebooks(index, codebooks, status)
    type(IndexHandle), intent(inout) :: index
    type(VectorBlock), intent(in) :: codebooks
    integer(int32), intent(out) :: status
    type(PQIndex), pointer :: pq_index
    integer(int32) :: stride_cb
    integer(int32) :: dsub
    integer(int32) :: ksub
    integer(int32) :: elem_bytes
    integer(int64) :: expected_length
    integer(int64) :: total_elements
    integer(int64) :: total_bytes
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int64) :: block_index
    integer(int64) :: src_offset
    integer(int64) :: dst_offset
    real(real32), pointer :: src(:)
    real(real32), pointer :: dst(:)
    type(c_ptr) :: new_data

    call pq_handle(index, pq_index)
    if (.not. associated(pq_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (pq_index%nbits /= 8_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    ksub = 2_int32**pq_index%nbits
    if (ksub <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (mod(pq_index%dim, pq_index%m) /= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    dsub = pq_index%dim / pq_index%m
    if (codebooks%dim /= dsub) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    expected_length = int(pq_index%m, int64) * int(ksub, int64)
    if (codebooks%length /= expected_length) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    if (codebooks%elem_size /= 0_int32 .and. codebooks%elem_size /= elem_bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    stride_cb = codebooks%stride
    if (stride_cb <= 0_int32) stride_cb = codebooks%dim
    if (stride_cb < codebooks%dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    total_elements = expected_length * int(codebooks%dim, int64)
    total_bytes = total_elements * int(elem_bytes, int64)

    call allocate_aligned(new_data, int(total_bytes, c_size_t), int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      status = alloc_status
      return
    end if

    call c_f_pointer(codebooks%data, src, [int(stride_cb, int64) * expected_length])
    call c_f_pointer(new_data, dst, [total_elements])

    do block_index = 1_int64, expected_length
      src_offset = (block_index - 1_int64) * stride_cb
      dst_offset = (block_index - 1_int64) * codebooks%dim
      dst(dst_offset + 1_int64:dst_offset + codebooks%dim) = &
        src(src_offset + 1_int64:src_offset + codebooks%dim)
    end do

    if (c_associated(pq_index%codebooks%data)) then
      call free_aligned(pq_index%codebooks%data, free_status)
    end if

    pq_index%codebooks%data = new_data
    pq_index%codebooks%length = expected_length
    pq_index%codebooks%dim = codebooks%dim
    pq_index%codebooks%stride = codebooks%dim
    pq_index%codebooks%elem_size = elem_bytes
    pq_index%codebooks%alignment = 64

    pq_index%is_trained = .true.
    status = GLAMIN_OK
  end subroutine pq_set_codebooks

  subroutine pq_add(index, vectors, status)
    type(IndexHandle), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(out) :: status
    type(PQIndex), pointer :: pq_index
    integer(int32) :: dim
    integer(int32) :: stride_vec
    integer(int32) :: elem_bytes
    integer(int32) :: m
    integer(int32) :: dsub
    integer(int32) :: ksub
    integer(int32) :: code_size
    integer(int64) :: existing
    integer(int64) :: incoming
    integer(int64) :: total
    integer(int64) :: total_bytes
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int64) :: vec_index
    integer(int32) :: sub_index
    integer(int32) :: centroid_index
    integer(int64) :: query_offset
    integer(int64) :: code_offset
    integer(int64) :: centroid_offset
    integer(int32) :: dim_index
    real(real32) :: diff
    real(real32) :: accum
    real(real32) :: best_distance
    integer(int32) :: best_label
    type(c_ptr) :: new_codes
    type(c_ptr) :: old_codes
    real(real32), pointer :: vector_data(:)
    real(real32), pointer :: codebook_data(:)
    integer(c_int8_t), pointer :: dst_codes(:)
    integer(c_int8_t), pointer :: src_codes(:)

    call pq_handle(index, pq_index)
    if (.not. associated(pq_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (.not. c_associated(pq_index%codebooks%data)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (pq_index%nbits /= 8_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    dim = vectors%dim
    if (dim <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (dim /= pq_index%dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    stride_vec = vectors%stride
    if (stride_vec <= 0_int32) stride_vec = dim
    if (stride_vec < dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    if (vectors%elem_size /= 0_int32 .and. vectors%elem_size /= elem_bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    incoming = vectors%length
    if (incoming <= 0_int64) then
      status = GLAMIN_OK
      return
    end if

    m = pq_index%m
    dsub = dim / m
    ksub = 2_int32**pq_index%nbits
    code_size = pq_index%code_size
    if (code_size <= 0_int32) then
      code_size = int((pq_index%nbits * m + 7_int32) / 8_int32, int32)
    end if

    if (code_size /= m) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    existing = pq_index%codes%length
    total = existing + incoming
    total_bytes = total * int(code_size, int64)

    old_codes = pq_index%codes%data
    call allocate_aligned(new_codes, int(total_bytes, c_size_t), int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      status = alloc_status
      return
    end if

    call c_f_pointer(new_codes, dst_codes, [total * int(code_size, int64)])
    if (existing > 0_int64 .and. c_associated(old_codes)) then
      call c_f_pointer(old_codes, src_codes, [existing * int(code_size, int64)])
      dst_codes(1:existing * int(code_size, int64)) = src_codes(1:existing * int(code_size, int64))
    end if

    call c_f_pointer(vectors%data, vector_data, [int(stride_vec, int64) * incoming])
    call c_f_pointer(pq_index%codebooks%data, codebook_data, &
      [int(pq_index%codebooks%length, int64) * int(pq_index%codebooks%dim, int64)])

    do vec_index = 1_int64, incoming
      query_offset = (vec_index - 1_int64) * stride_vec
      code_offset = (existing + vec_index - 1_int64) * int(code_size, int64)
      do sub_index = 1_int32, m
        best_distance = huge(1.0_real32)
        best_label = 0_int32
        centroid_offset = int(sub_index - 1_int32, int64) * int(ksub * dsub, int64)
        do centroid_index = 0_int32, ksub - 1_int32
          accum = 0.0_real32
          do dim_index = 1_int32, dsub
            diff = vector_data(query_offset + int((sub_index - 1_int32) * dsub + dim_index, int64)) - &
              codebook_data(centroid_offset + int(centroid_index * dsub + dim_index, int64))
            accum = accum + diff * diff
          end do
          if (accum < best_distance) then
            best_distance = accum
            best_label = centroid_index
          end if
        end do
        dst_codes(code_offset + sub_index) = int(best_label, c_int8_t)
      end do
    end do

    if (c_associated(old_codes)) then
      call free_aligned(old_codes, free_status)
    end if

    pq_index%codes%data = new_codes
    pq_index%codes%length = total
    pq_index%codes%dim = code_size
    pq_index%codes%stride = code_size
    pq_index%codes%elem_size = 1_int32
    pq_index%codes%alignment = 64
    pq_index%ntotal = total
    status = GLAMIN_OK
  end subroutine pq_add

  subroutine pq_search(index, queries, k, distances, labels, status)
    type(IndexHandle), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    type(VectorBlock), intent(inout) :: distances
    type(VectorBlock), intent(inout) :: labels
    integer(int32), intent(out) :: status
    type(PQIndex), pointer :: pq_index
    integer(int32) :: elem_bytes
    integer(int32) :: label_bytes
    integer(int32) :: result_k
    integer(int32) :: stride_q
    integer(int32) :: dim
    integer(int32) :: m
    integer(int32) :: dsub
    integer(int32) :: ksub
    integer(int32) :: code_size
    integer(int64) :: query_count
    integer(int64) :: vector_count
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int64) :: query_index
    integer(int64) :: vector_index
    integer(int64) :: query_offset
    integer(int64) :: code_offset
    integer(int64) :: output_offset
    integer(int64) :: centroid_offset
    integer(int32) :: sub_index
    integer(int32) :: dim_index
    integer(int32) :: code_value
    real(real32) :: accum
    real(real32) :: diff
    logical :: use_l2
    real(real32), allocatable :: top_distances(:)
    integer(int32), allocatable :: top_labels(:)
    real(real32), pointer :: query_data(:)
    real(real32), pointer :: codebook_data(:)
    real(real32), pointer :: distance_data(:)
    integer(int32), pointer :: label_data(:)
    integer(c_int8_t), pointer :: code_data(:)

    call pq_handle(index, pq_index)
    if (.not. associated(pq_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (.not. c_associated(pq_index%codebooks%data) .or. &
        .not. c_associated(pq_index%codes%data)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (pq_index%nbits /= 8_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    dim = pq_index%dim
    if (queries%dim /= dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    label_bytes = int(storage_size(0_int32) / 8, int32)

    if (queries%elem_size /= 0_int32 .and. queries%elem_size /= elem_bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (pq_index%codebooks%elem_size /= 0_int32 .and. pq_index%codebooks%elem_size /= elem_bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    query_count = queries%length
    vector_count = pq_index%codes%length
    if (query_count <= 0_int64 .or. vector_count <= 0_int64) then
      status = GLAMIN_OK
      return
    end if

    result_k = min(max(1_int32, k), int(vector_count, int32))
    if (result_k <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (c_associated(distances%data)) then
      call free_aligned(distances%data, free_status)
    end if
    if (c_associated(labels%data)) then
      call free_aligned(labels%data, free_status)
    end if

    call allocate_aligned(distances%data, &
      int(query_count * int(result_k, int64) * elem_bytes, c_size_t), &
      int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      status = alloc_status
      return
    end if

    call allocate_aligned(labels%data, &
      int(query_count * int(result_k, int64) * label_bytes, c_size_t), &
      int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      status = alloc_status
      return
    end if

    distances%dim = result_k
    distances%length = query_count
    distances%stride = result_k
    distances%elem_size = elem_bytes
    distances%alignment = 64

    labels%dim = result_k
    labels%length = query_count
    labels%stride = result_k
    labels%elem_size = label_bytes
    labels%alignment = 64

    stride_q = queries%stride
    if (stride_q <= 0_int32) stride_q = dim
    if (stride_q < dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    m = pq_index%m
    dsub = dim / m
    ksub = 2_int32**pq_index%nbits
    code_size = pq_index%code_size
    if (code_size <= 0_int32) then
      code_size = int((pq_index%nbits * m + 7_int32) / 8_int32, int32)
    end if

    if (code_size /= m) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (pq_index%metric == METRIC_IP) then
      use_l2 = .false.
    else if (pq_index%metric == METRIC_L2) then
      use_l2 = .true.
    else
      status = GLAMIN_ERR_INVALID_ARG
      deallocate(top_distances)
      deallocate(top_labels)
      return
    end if

    allocate(top_distances(result_k), stat=alloc_status)
    if (alloc_status /= 0_int32) then
      status = GLAMIN_ERR_OOM
      return
    end if
    allocate(top_labels(result_k), stat=alloc_status)
    if (alloc_status /= 0_int32) then
      deallocate(top_distances)
      status = GLAMIN_ERR_OOM
      return
    end if

    call c_f_pointer(queries%data, query_data, [int(stride_q, int64) * query_count])
    call c_f_pointer(pq_index%codebooks%data, codebook_data, &
      [int(pq_index%codebooks%length, int64) * int(pq_index%codebooks%dim, int64)])
    call c_f_pointer(pq_index%codes%data, code_data, [vector_count * int(code_size, int64)])
    call c_f_pointer(distances%data, distance_data, [int(result_k, int64) * query_count])
    call c_f_pointer(labels%data, label_data, [int(result_k, int64) * query_count])

    do query_index = 1_int64, query_count
      if (use_l2) then
        top_distances = huge(1.0_real32)
      else
        top_distances = -huge(1.0_real32)
      end if
      top_labels = -1_int32
      query_offset = (query_index - 1_int64) * stride_q
      do vector_index = 1_int64, vector_count
        code_offset = (vector_index - 1_int64) * int(code_size, int64)
        accum = 0.0_real32
        do sub_index = 1_int32, m
          code_value = int(code_data(code_offset + sub_index), int32)
          if (code_value < 0_int32) code_value = code_value + 256_int32
          if (code_value >= ksub) then
            status = GLAMIN_ERR_INVALID_ARG
            deallocate(top_distances)
            deallocate(top_labels)
            return
          end if
          centroid_offset = int(sub_index - 1_int32, int64) * int(ksub * dsub, int64) + &
            int(code_value * dsub, int64)
          do dim_index = 1_int32, dsub
            if (use_l2) then
              diff = query_data(query_offset + int((sub_index - 1_int32) * dsub + dim_index, int64)) - &
                codebook_data(centroid_offset + dim_index)
              accum = accum + diff * diff
            else
              accum = accum + query_data(query_offset + int((sub_index - 1_int32) * dsub + dim_index, int64)) * &
                codebook_data(centroid_offset + dim_index)
            end if
          end do
        end do
        call update_top_k(accum, int(vector_index - 1_int64, int32), use_l2, &
          top_distances, top_labels)
      end do

      output_offset = (query_index - 1_int64) * result_k
      distance_data(output_offset + 1_int64:output_offset + result_k) = top_distances
      label_data(output_offset + 1_int64:output_offset + result_k) = top_labels
    end do

    deallocate(top_distances)
    deallocate(top_labels)
    status = GLAMIN_OK
  end subroutine pq_search

  subroutine pq_create_handle(handle, dim, m, nbits, metric, status)
    type(IndexHandle), intent(out) :: handle
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: nbits
    integer(int32), intent(in) :: metric
    integer(int32), intent(out) :: status
    type(PQIndex), pointer :: pq_index
    integer(int32) :: alloc_status
    integer(int32) :: code_size

    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_PQ

    if (dim <= 0_int32 .or. m <= 0_int32 .or. nbits <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (mod(dim, m) /= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    code_size = int((nbits * m + 7_int32) / 8_int32, int32)

    allocate(pq_index, stat=alloc_status)
    if (alloc_status /= 0_int32) then
      status = GLAMIN_ERR_OOM
      return
    end if

    pq_index%dim = dim
    pq_index%metric = metric
    pq_index%m = m
    pq_index%nbits = nbits
    pq_index%code_size = code_size
    pq_index%ntotal = 0_int64
    pq_index%is_trained = .false.
    pq_index%search_type = 0_int32
    pq_index%encode_signs = .false.
    pq_index%polysemous_ht = 0_int32
    pq_index%codebooks = VectorBlock()
    pq_index%codes = VectorBlock()

    handle%impl = c_loc(pq_index)
    handle%kind = INDEX_KIND_PQ
    status = GLAMIN_OK
  end subroutine pq_create_handle

  subroutine pq_destroy_handle(handle, status)
    type(IndexHandle), intent(inout) :: handle
    integer(int32), intent(out) :: status
    type(PQIndex), pointer :: pq_index
    integer(int32) :: free_status
    integer(int32) :: alloc_status

    call pq_handle(handle, pq_index)
    if (.not. associated(pq_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (c_associated(pq_index%codebooks%data)) then
      call free_aligned(pq_index%codebooks%data, free_status)
    end if
    if (c_associated(pq_index%codes%data)) then
      call free_aligned(pq_index%codes%data, free_status)
    end if

    deallocate(pq_index, stat=alloc_status)
    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_UNKNOWN
    status = GLAMIN_OK
  end subroutine pq_destroy_handle

  subroutine pq_handle(index_handle, pq_index)
    type(IndexHandle), intent(in) :: index_handle
    type(PQIndex), pointer :: pq_index

    pq_index => null()
    if (.not. c_associated(index_handle%impl)) then
      return
    end if

    if (index_handle%kind /= INDEX_KIND_PQ) then
      return
    end if

    call c_f_pointer(index_handle%impl, pq_index)
  end subroutine pq_handle

  subroutine pq_create(codebook, dim, m, ksub)
    type(ProductQuantizerCodebook), intent(out) :: codebook
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: ksub
    error stop "pq_create not implemented"
  end subroutine pq_create

  subroutine pq_train(codebook, vectors)
    type(ProductQuantizerCodebook), intent(inout) :: codebook
    type(VectorBlock), intent(in) :: vectors
    error stop "pq_train not implemented"
  end subroutine pq_train

  subroutine pq_encode(codebook, vectors, codes)
    type(ProductQuantizerCodebook), intent(in) :: codebook
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(out) :: codes
    error stop "pq_encode not implemented"
  end subroutine pq_encode

  subroutine pq_decode(codebook, codes, vectors)
    type(ProductQuantizerCodebook), intent(in) :: codebook
    type(VectorBlock), intent(in) :: codes
    type(VectorBlock), intent(out) :: vectors
    error stop "pq_decode not implemented"
  end subroutine pq_decode

  subroutine update_top_k(distance, label, use_l2, top_distances, top_labels)
    real(real32), intent(in) :: distance
    integer(int32), intent(in) :: label
    logical, intent(in) :: use_l2
    real(real32), intent(inout) :: top_distances(:)
    integer(int32), intent(inout) :: top_labels(:)
    integer(int32) :: last
    integer(int32) :: index
    real(real32) :: swap_distance
    integer(int32) :: swap_label

    last = size(top_distances)
    if (use_l2) then
      if (distance >= top_distances(last)) then
        return
      end if
    else
      if (distance <= top_distances(last)) then
        return
      end if
    end if

    top_distances(last) = distance
    top_labels(last) = label

    do index = last - 1_int32, 1_int32, -1_int32
      if (use_l2) then
        if (top_distances(index) <= top_distances(index + 1_int32)) exit
      else
        if (top_distances(index) >= top_distances(index + 1_int32)) exit
      end if

      swap_distance = top_distances(index)
      top_distances(index) = top_distances(index + 1_int32)
      top_distances(index + 1_int32) = swap_distance

      swap_label = top_labels(index)
      top_labels(index) = top_labels(index + 1_int32)
      top_labels(index + 1_int32) = swap_label
    end do
  end subroutine update_top_k
end module glamin_index_pq
