module glamin_index_ivfpq
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_int8_t, c_loc, c_null_ptr, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  use glamin_index_pq, only: ProductQuantizerCodebook, pq_create, pq_train
  use glamin_kmeans, only: kmeans_train
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_IP, METRIC_L2
  use glamin_types, only: IndexHandle, VectorBlock, INDEX_KIND_IVFPQ, INDEX_KIND_UNKNOWN
  implicit none
  private

  public :: IvfProductQuantizerIndex
  public :: ivfpq_create
  public :: ivfpq_train
  public :: ivfpq_add
  public :: ivfpq_search
  public :: ivfpq_destroy
  public :: ivfpq_create_handle
  public :: ivfpq_destroy_handle
  public :: ivfpq_handle

  type :: IvfPQList
    type(VectorBlock) :: codes
    integer(int32), allocatable :: labels(:)
    integer(int64) :: count = 0
  end type IvfPQList

  type :: IvfProductQuantizerIndex
    integer(int32) :: dim = 0
    integer(int32) :: nlist = 0
    integer(int32) :: nprobe = 1
    integer(int32) :: m = 0
    integer(int32) :: ksub = 0
    integer(int32) :: metric = 0
    type(VectorBlock) :: centroids
    type(VectorBlock) :: codebooks
    type(IvfPQList), allocatable :: lists(:)
    integer(int64) :: ntotal = 0
    logical :: is_trained = .false.
  end type IvfProductQuantizerIndex

contains
  subroutine ivfpq_create(index, dim, nlist, m, ksub, metric, status)
    type(IvfProductQuantizerIndex), intent(out) :: index
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: nlist
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: ksub
    integer(int32), intent(in) :: metric
    integer(int32), intent(out), optional :: status
    integer(int32) :: list_index

    call set_status(status, GLAMIN_OK, "")

    if (dim <= 0_int32 .or. nlist <= 0_int32 .or. m <= 0_int32 .or. ksub <= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_create invalid args")
      return
    end if

    if (mod(dim, m) /= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_create dim not divisible")
      return
    end if

    if (metric /= METRIC_L2 .and. metric /= METRIC_IP) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_create unsupported metric")
      return
    end if

    index%dim = dim
    index%nlist = nlist
    index%nprobe = 1_int32
    index%m = m
    index%ksub = ksub
    index%metric = metric
    index%centroids = VectorBlock()
    index%codebooks = VectorBlock()
    index%ntotal = 0_int64
    index%is_trained = .false.

    allocate(index%lists(nlist))
    do list_index = 1_int32, nlist
      call reset_list(index%lists(list_index))
    end do
  end subroutine ivfpq_create

  subroutine ivfpq_create_handle(handle, dim, nlist, m, ksub, metric, status)
    type(IndexHandle), intent(out) :: handle
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: nlist
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: ksub
    integer(int32), intent(in) :: metric
    integer(int32), intent(out) :: status
    type(IvfProductQuantizerIndex), pointer :: ivfpq_index
    integer(int32) :: alloc_status

    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_IVFPQ

    allocate(ivfpq_index, stat=alloc_status)
    if (alloc_status /= 0_int32) then
      status = GLAMIN_ERR_OOM
      return
    end if

    call ivfpq_create(ivfpq_index, dim, nlist, m, ksub, metric, status)
    if (status /= GLAMIN_OK) then
      deallocate(ivfpq_index, stat=alloc_status)
      return
    end if

    handle%impl = c_loc(ivfpq_index)
    handle%kind = INDEX_KIND_IVFPQ
    status = GLAMIN_OK
  end subroutine ivfpq_create_handle

  subroutine ivfpq_destroy_handle(handle, status)
    type(IndexHandle), intent(inout) :: handle
    integer(int32), intent(out) :: status
    type(IvfProductQuantizerIndex), pointer :: ivfpq_index
    integer(int32) :: alloc_status

    call ivfpq_handle(handle, ivfpq_index)
    if (.not. associated(ivfpq_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call ivfpq_destroy(ivfpq_index, status)
    deallocate(ivfpq_index, stat=alloc_status)
    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_UNKNOWN
    status = GLAMIN_OK
  end subroutine ivfpq_destroy_handle

  subroutine ivfpq_handle(index_handle, ivfpq_index)
    type(IndexHandle), intent(in) :: index_handle
    type(IvfProductQuantizerIndex), pointer :: ivfpq_index

    ivfpq_index => null()
    if (.not. c_associated(index_handle%impl)) then
      return
    end if

    if (index_handle%kind /= INDEX_KIND_IVFPQ) then
      return
    end if

    call c_f_pointer(index_handle%impl, ivfpq_index)
  end subroutine ivfpq_handle

  subroutine ivfpq_train(index, vectors, status)
    type(IvfProductQuantizerIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(out), optional :: status
    integer(int32) :: local_status
    integer(int32) :: dim
    integer(int32) :: stride_v
    integer(int64) :: vector_count
    integer(int32) :: elem_bytes
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int64) :: vec_index
    integer(int64) :: vec_offset
    integer(int64) :: residual_offset
    integer(int32) :: list_index
    type(ProductQuantizerCodebook) :: pq_codebook
    type(VectorBlock) :: residuals
    real(real32), pointer :: vector_data(:)
    real(real32), pointer :: centroid_data(:)
    real(real32), pointer :: residual_data(:)

    call set_status(status, GLAMIN_OK, "")

    if (index%dim <= 0_int32 .or. index%nlist <= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_train invalid index")
      return
    end if

    dim = index%dim
    if (vectors%dim /= dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_train dim mismatch")
      return
    end if

    if (mod(dim, index%m) /= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_train dim not divisible")
      return
    end if

    call kmeans_train(vectors, index%nlist, index%metric, index%centroids, local_status)
    if (local_status /= GLAMIN_OK) then
      call set_status(status, local_status, "ivfpq_train kmeans failed")
      return
    end if

    vector_count = vectors%length
    if (vector_count <= 0_int64) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_train empty vectors")
      return
    end if

    stride_v = vectors%stride
    if (stride_v <= 0_int32) stride_v = dim
    if (stride_v < dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_train stride invalid")
      return
    end if

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)

    residuals = VectorBlock()
    call allocate_aligned(residuals%data, int(vector_count * int(dim, int64) * elem_bytes, c_size_t), &
      int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      call set_status(status, alloc_status, "ivfpq_train residual alloc failed")
      return
    end if

    residuals%length = vector_count
    residuals%dim = dim
    residuals%stride = dim
    residuals%elem_size = elem_bytes
    residuals%alignment = 64

    call c_f_pointer(vectors%data, vector_data, [int(stride_v, int64) * vector_count])
    call c_f_pointer(index%centroids%data, centroid_data, [int(dim, int64) * index%nlist])
    call c_f_pointer(residuals%data, residual_data, [int(dim, int64) * vector_count])

    do vec_index = 1_int64, vector_count
      vec_offset = (vec_index - 1_int64) * stride_v
      residual_offset = (vec_index - 1_int64) * dim
      call assign_list(index, vector_data(vec_offset + 1_int64:vec_offset + dim), list_index, &
        local_status)
      if (local_status /= GLAMIN_OK) then
        call free_aligned(residuals%data, free_status)
        call set_status(status, local_status, "ivfpq_train assignment failed")
        return
      end if

      residual_data(residual_offset + 1_int64:residual_offset + dim) = &
        vector_data(vec_offset + 1_int64:vec_offset + dim) - &
        centroid_data(int(list_index - 1_int32, int64) * dim + 1_int64: &
          int(list_index - 1_int32, int64) * dim + dim)
    end do

    pq_codebook = ProductQuantizerCodebook()
    call pq_create(pq_codebook, dim, index%m, index%ksub, local_status)
    if (local_status /= GLAMIN_OK) then
      call free_aligned(residuals%data, free_status)
      call set_status(status, local_status, "ivfpq_train pq create failed")
      return
    end if

    call pq_train(pq_codebook, residuals, local_status)
    call free_aligned(residuals%data, free_status)
    if (local_status /= GLAMIN_OK) then
      call set_status(status, local_status, "ivfpq_train pq train failed")
      return
    end if

    if (c_associated(index%codebooks%data)) then
      call free_aligned(index%codebooks%data, free_status)
    end if
    index%codebooks = pq_codebook%codebooks

    call reset_lists(index)
    index%is_trained = .true.
    index%ntotal = 0_int64
  end subroutine ivfpq_train

  subroutine ivfpq_add(index, vectors, status)
    type(IvfProductQuantizerIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(out), optional :: status
    integer(int32) :: local_status
    integer(int32) :: dim
    integer(int32) :: stride_v
    integer(int32) :: dsub
    integer(int64) :: vector_count
    integer(int64) :: vec_index
    integer(int64) :: vec_offset
    integer(int32) :: list_index
    integer(int32) :: sub_index
    integer(int32) :: centroid_index
    integer(int32) :: axis_index
    integer(int32) :: best_label
    integer(int64) :: code_offset
    integer(int64) :: centroid_offset
    real(real32) :: diff
    real(real32) :: accum
    real(real32) :: best_distance
    real(real32), pointer :: vector_data(:)
    real(real32), pointer :: centroid_data(:)
    real(real32), pointer :: codebook_data(:)
    integer(c_int8_t) :: codes(256)

    call set_status(status, GLAMIN_OK, "")

    if (.not. index%is_trained) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_add index not trained")
      return
    end if

    if (.not. c_associated(vectors%data) .or. .not. c_associated(index%codebooks%data)) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_add missing data")
      return
    end if

    dim = index%dim
    if (vectors%dim /= dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_add dim mismatch")
      return
    end if

    if (index%m > size(codes)) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_add m too large")
      return
    end if

    dsub = dim / index%m
    vector_count = vectors%length
    if (vector_count <= 0_int64) then
      return
    end if

    stride_v = vectors%stride
    if (stride_v <= 0_int32) stride_v = dim
    if (stride_v < dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_add stride invalid")
      return
    end if

    call c_f_pointer(vectors%data, vector_data, [int(stride_v, int64) * vector_count])
    call c_f_pointer(index%centroids%data, centroid_data, [int(dim, int64) * index%nlist])
    call c_f_pointer(index%codebooks%data, codebook_data, &
      [int(index%codebooks%length, int64) * int(index%codebooks%dim, int64)])

    do vec_index = 1_int64, vector_count
      vec_offset = (vec_index - 1_int64) * stride_v
      call assign_list(index, vector_data(vec_offset + 1_int64:vec_offset + dim), list_index, &
        local_status)
      if (local_status /= GLAMIN_OK) then
        call set_status(status, local_status, "ivfpq_add assignment failed")
        return
      end if

      do sub_index = 1_int32, index%m
        best_distance = huge(1.0_real32)
        best_label = 0_int32
        do centroid_index = 0_int32, index%ksub - 1_int32
          accum = 0.0_real32
          code_offset = int(sub_index - 1_int32, int64) * int(index%ksub * dsub, int64) + &
            int(centroid_index * dsub, int64)
          centroid_offset = int(list_index - 1_int32, int64) * dim + &
            int((sub_index - 1_int32) * dsub, int64)
          do axis_index = 1_int32, dsub
            diff = (vector_data(vec_offset + centroid_offset + axis_index) - &
              centroid_data(centroid_offset + axis_index)) - &
              codebook_data(code_offset + axis_index)
            accum = accum + diff * diff
          end do
          if (accum < best_distance) then
            best_distance = accum
            best_label = centroid_index
          end if
        end do
        codes(sub_index) = int(best_label, c_int8_t)
      end do

      call append_codes(index%lists(list_index), codes, index%m, int(index%ntotal + vec_index, int32), &
        local_status)
      if (local_status /= GLAMIN_OK) then
        call set_status(status, local_status, "ivfpq_add append failed")
        return
      end if
    end do

    index%ntotal = index%ntotal + vector_count
  end subroutine ivfpq_add

  subroutine ivfpq_search(index, queries, k, nprobe, distances, labels, status)
    type(IvfProductQuantizerIndex), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    integer(int32), intent(in) :: nprobe
    type(VectorBlock), intent(inout) :: distances
    type(VectorBlock), intent(inout) :: labels
    integer(int32), intent(out), optional :: status
    integer(int32) :: local_status
    integer(int32) :: dim
    integer(int32) :: stride_q
    integer(int32) :: dsub
    integer(int32) :: result_k
    integer(int32) :: probe_count
    integer(int32) :: elem_bytes
    integer(int32) :: label_bytes
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int64) :: query_count
    integer(int64) :: query_index
    integer(int64) :: query_offset
    integer(int64) :: output_offset
    integer(int32) :: probe_index
    integer(int32) :: list_index
    integer(int64) :: list_vector_index
    integer(int64) :: code_offset
    integer(int64) :: centroid_offset
    integer(int32) :: sub_index
    integer(int32) :: axis_index
    integer(int32) :: code_value
    real(real32) :: distance
    real(real32) :: diff
    real(real32), allocatable :: top_distances(:)
    integer(int32), allocatable :: top_labels(:)
    integer(int32), allocatable :: probe_lists(:)
    real(real32), pointer :: query_data(:)
    real(real32), pointer :: centroid_data(:)
    real(real32), pointer :: codebook_data(:)
    real(real32), pointer :: distance_data(:)
    integer(int32), pointer :: label_data(:)
    integer(c_int8_t), pointer :: code_data(:)
    logical :: use_l2

    call set_status(status, GLAMIN_OK, "")

    if (.not. index%is_trained) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_search index not trained")
      return
    end if

    if (.not. c_associated(queries%data)) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_search queries missing")
      return
    end if

    dim = index%dim
    if (queries%dim /= dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_search dim mismatch")
      return
    end if

    query_count = queries%length
    if (query_count <= 0_int64 .or. index%ntotal <= 0_int64) then
      return
    end if

    use_l2 = index%metric == METRIC_L2
    dsub = dim / index%m

    result_k = min(max(1_int32, k), int(index%ntotal, int32))
    if (result_k <= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_search invalid k")
      return
    end if

    probe_count = min(max(1_int32, nprobe), index%nlist)

    stride_q = queries%stride
    if (stride_q <= 0_int32) stride_q = dim
    if (stride_q < dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_search stride invalid")
      return
    end if

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    label_bytes = int(storage_size(0_int32) / 8, int32)

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
      call set_status(status, alloc_status, "ivfpq_search alloc distances failed")
      return
    end if

    call allocate_aligned(labels%data, &
      int(query_count * int(result_k, int64) * label_bytes, c_size_t), &
      int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      call set_status(status, alloc_status, "ivfpq_search alloc labels failed")
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

    allocate(top_distances(result_k))
    allocate(top_labels(result_k))
    allocate(probe_lists(probe_count))

    call c_f_pointer(queries%data, query_data, [int(stride_q, int64) * query_count])
    call c_f_pointer(index%centroids%data, centroid_data, [int(dim, int64) * index%nlist])
    call c_f_pointer(index%codebooks%data, codebook_data, &
      [int(index%codebooks%length, int64) * int(index%codebooks%dim, int64)])
    call c_f_pointer(distances%data, distance_data, [int(result_k, int64) * query_count])
    call c_f_pointer(labels%data, label_data, [int(result_k, int64) * query_count])

    do query_index = 1_int64, query_count
      query_offset = (query_index - 1_int64) * stride_q
      call init_top_k(top_distances, top_labels, use_l2)

      call select_probe_lists(index, query_data(query_offset + 1_int64:query_offset + dim), &
        probe_count, probe_lists, local_status)
      if (local_status /= GLAMIN_OK) then
        deallocate(top_distances, top_labels, probe_lists)
        call set_status(status, local_status, "ivfpq_search probe selection failed")
        return
      end if

      do probe_index = 1_int32, probe_count
        list_index = probe_lists(probe_index)
        if (list_index <= 0_int32 .or. list_index > size(index%lists)) cycle
        if (index%lists(list_index)%count <= 0_int64) cycle

        call c_f_pointer(index%lists(list_index)%codes%data, code_data, &
          [index%lists(list_index)%count * int(index%m, int64)])

        centroid_offset = int(list_index - 1_int32, int64) * dim

        do list_vector_index = 1_int64, index%lists(list_index)%count
          code_offset = (list_vector_index - 1_int64) * int(index%m, int64)
          distance = 0.0_real32

          do sub_index = 1_int32, index%m
            code_value = int(code_data(code_offset + sub_index), int32)
            if (code_value < 0_int32) code_value = code_value + 256_int32
            if (code_value >= index%ksub) then
              deallocate(top_distances, top_labels, probe_lists)
              call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivfpq_search code out of range")
              return
            end if
            do axis_index = 1_int32, dsub
              diff = (query_data(query_offset + int((sub_index - 1_int32) * dsub + axis_index, int64)) - &
                centroid_data(centroid_offset + int((sub_index - 1_int32) * dsub + axis_index, int64))) - &
                codebook_data(int(sub_index - 1_int32, int64) * int(index%ksub * dsub, int64) + &
                  int(code_value * dsub + axis_index, int64))
              if (use_l2) then
                distance = distance + diff * diff
              else
                distance = distance + diff * codebook_data(int(sub_index - 1_int32, int64) * &
                  int(index%ksub * dsub, int64) + int(code_value * dsub + axis_index, int64))
              end if
            end do
          end do

          call update_top_k(distance, index%lists(list_index)%labels(list_vector_index), use_l2, &
            top_distances, top_labels)
        end do
      end do

      output_offset = (query_index - 1_int64) * result_k
      distance_data(output_offset + 1_int64:output_offset + result_k) = top_distances
      label_data(output_offset + 1_int64:output_offset + result_k) = top_labels
    end do

    deallocate(top_distances, top_labels, probe_lists)
  end subroutine ivfpq_search

  subroutine ivfpq_destroy(index, status)
    type(IvfProductQuantizerIndex), intent(inout) :: index
    integer(int32), intent(out), optional :: status
    integer(int32) :: free_status

    call set_status(status, GLAMIN_OK, "")

    call reset_lists(index)

    if (c_associated(index%centroids%data)) then
      call free_aligned(index%centroids%data, free_status)
    end if
    if (c_associated(index%codebooks%data)) then
      call free_aligned(index%codebooks%data, free_status)
    end if

    index%centroids = VectorBlock()
    index%codebooks = VectorBlock()
    index%is_trained = .false.
    index%ntotal = 0_int64
  end subroutine ivfpq_destroy

  subroutine reset_lists(index)
    type(IvfProductQuantizerIndex), intent(inout) :: index
    integer(int32) :: list_index

    if (.not. allocated(index%lists)) return
    do list_index = 1_int32, size(index%lists)
      call reset_list(index%lists(list_index))
    end do
  end subroutine reset_lists

  subroutine reset_list(list)
    type(IvfPQList), intent(inout) :: list
    integer(int32) :: free_status

    if (c_associated(list%codes%data)) then
      call free_aligned(list%codes%data, free_status)
    end if
    list%codes = VectorBlock()
    list%count = 0_int64
    if (allocated(list%labels)) then
      deallocate(list%labels)
    end if
  end subroutine reset_list

  subroutine append_codes(list, codes, m, label, status)
    type(IvfPQList), intent(inout) :: list
    integer(c_int8_t), intent(in) :: codes(:)
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: label
    integer(int32), intent(out) :: status
    integer(int64) :: new_count
    integer(int64) :: total_bytes
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int64) :: offset
    integer(int32) :: idx
    integer(c_int8_t), pointer :: dst(:)
    integer(c_int8_t), pointer :: src(:)
    type(c_ptr) :: new_data
    integer(int32), allocatable :: new_labels(:)

    status = GLAMIN_OK
    if (size(codes) < m) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    new_count = list%count + 1_int64
    total_bytes = new_count * int(m, int64)

    call allocate_aligned(new_data, int(total_bytes, c_size_t), int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      status = alloc_status
      return
    end if

    call c_f_pointer(new_data, dst, [new_count * int(m, int64)])
    if (list%count > 0_int64) then
      call c_f_pointer(list%codes%data, src, [list%count * int(m, int64)])
      dst(1_int64:list%count * int(m, int64)) = src(1_int64:list%count * int(m, int64))
    end if

    offset = (new_count - 1_int64) * int(m, int64)
    do idx = 1_int32, m
      dst(offset + idx) = codes(idx)
    end do

    if (allocated(list%labels)) then
      allocate(new_labels(new_count))
      new_labels(1_int64:list%count) = list%labels
      new_labels(new_count) = label
      deallocate(list%labels)
      list%labels = new_labels
    else
      allocate(list%labels(new_count))
      list%labels(1) = label
    end if

    if (c_associated(list%codes%data)) then
      call free_aligned(list%codes%data, free_status)
    end if

    list%codes%data = new_data
    list%codes%length = new_count
    list%codes%dim = m
    list%codes%stride = m
    list%codes%elem_size = 1_int32
    list%codes%alignment = 64
    list%count = new_count
  end subroutine append_codes

  subroutine assign_list(index, vector, list_index, status)
    type(IvfProductQuantizerIndex), intent(in) :: index
    real(real32), intent(in) :: vector(:)
    integer(int32), intent(out) :: list_index
    integer(int32), intent(out) :: status
    integer(int32) :: dim
    integer(int32) :: centroid_index
    integer(int32) :: axis_index
    integer(int64) :: centroid_offset
    real(real32) :: best_distance
    real(real32) :: distance
    real(real32) :: diff
    real(real32), pointer :: centroid_data(:)
    logical :: use_l2

    status = GLAMIN_OK
    list_index = 1_int32
    dim = index%dim
    if (size(vector) < dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    use_l2 = index%metric == METRIC_L2
    best_distance = huge(1.0_real32)
    if (.not. use_l2) best_distance = -huge(1.0_real32)

    call c_f_pointer(index%centroids%data, centroid_data, [int(dim, int64) * index%nlist])

    do centroid_index = 1_int32, index%nlist
      centroid_offset = int(centroid_index - 1_int32, int64) * dim
      distance = 0.0_real32
      if (use_l2) then
        do axis_index = 1_int32, dim
          diff = vector(axis_index) - centroid_data(centroid_offset + axis_index)
          distance = distance + diff * diff
        end do
        if (distance < best_distance) then
          best_distance = distance
          list_index = centroid_index
        end if
      else
        do axis_index = 1_int32, dim
          distance = distance + vector(axis_index) * centroid_data(centroid_offset + axis_index)
        end do
        if (distance > best_distance) then
          best_distance = distance
          list_index = centroid_index
        end if
      end if
    end do
  end subroutine assign_list

  subroutine select_probe_lists(index, vector, nprobe, probe_lists, status)
    type(IvfProductQuantizerIndex), intent(in) :: index
    real(real32), intent(in) :: vector(:)
    integer(int32), intent(in) :: nprobe
    integer(int32), intent(out) :: probe_lists(:)
    integer(int32), intent(out) :: status
    integer(int32) :: centroid_index
    integer(int32) :: axis_index
    integer(int32) :: target_probe
    integer(int64) :: centroid_offset
    real(real32) :: distance
    real(real32), allocatable :: top_distances(:)
    integer(int32), allocatable :: top_labels(:)
    real(real32), pointer :: centroid_data(:)
    logical :: use_l2

    status = GLAMIN_OK
    target_probe = min(nprobe, index%nlist)
    if (target_probe <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (size(probe_lists) < target_probe) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    allocate(top_distances(target_probe))
    allocate(top_labels(target_probe))

    use_l2 = index%metric == METRIC_L2
    call init_top_k(top_distances, top_labels, use_l2)
    call c_f_pointer(index%centroids%data, centroid_data, [int(index%dim, int64) * index%nlist])

    do centroid_index = 1_int32, index%nlist
      centroid_offset = int(centroid_index - 1_int32, int64) * index%dim
      distance = 0.0_real32
      if (use_l2) then
        do axis_index = 1_int32, index%dim
          distance = distance + (vector(axis_index) - &
            centroid_data(centroid_offset + axis_index))**2
        end do
      else
        do axis_index = 1_int32, index%dim
          distance = distance + vector(axis_index) * centroid_data(centroid_offset + axis_index)
        end do
      end if

      call update_top_k(distance, centroid_index, use_l2, top_distances, top_labels)
    end do

    probe_lists(1:target_probe) = top_labels
    deallocate(top_distances, top_labels)
  end subroutine select_probe_lists

  subroutine init_top_k(top_distances, top_labels, use_l2)
    real(real32), intent(inout) :: top_distances(:)
    integer(int32), intent(inout) :: top_labels(:)
    logical, intent(in) :: use_l2

    if (use_l2) then
      top_distances = huge(1.0_real32)
    else
      top_distances = -huge(1.0_real32)
    end if
    top_labels = 0_int32
  end subroutine init_top_k

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

  subroutine set_status(status, code, message)
    integer(int32), intent(out), optional :: status
    integer(int32), intent(in) :: code
    character(len=*), intent(in) :: message

    if (present(status)) then
      status = code
    else if (code /= GLAMIN_OK) then
      error stop message
    end if
  end subroutine set_status
end module glamin_index_ivfpq
