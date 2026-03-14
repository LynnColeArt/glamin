module glamin_index_ivf
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_loc, c_null_ptr, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  use glamin_gpu_backend, only: gpu_distance_ip_dispatch, gpu_distance_l2_dispatch
  use glamin_kmeans, only: kmeans_train
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_IP, METRIC_L2
  use glamin_types, only: IndexHandle, VectorBlock, INDEX_KIND_IVF, INDEX_KIND_UNKNOWN
  implicit none
  private

  public :: IvfIndex
  public :: ivf_create
  public :: ivf_train
  public :: ivf_add
  public :: ivf_search
  public :: ivf_destroy
  public :: ivf_create_handle
  public :: ivf_destroy_handle
  public :: ivf_handle

  type :: IvfList
    type(VectorBlock) :: vectors
    integer(int32), allocatable :: labels(:)
    integer(int64) :: count = 0
  end type IvfList

  type :: IvfIndex
    integer(int32) :: dim = 0
    integer(int32) :: nlist = 0
    integer(int32) :: nprobe = 1
    integer(int32) :: metric = 0
    type(VectorBlock) :: centroids
    type(IvfList), allocatable :: lists(:)
    integer(int64) :: ntotal = 0
    logical :: is_trained = .false.
  end type IvfIndex

  integer(int32), parameter :: IVF_QUERY_BATCH = 4

contains
  subroutine ivf_create(index, dim, nlist, metric, status)
    type(IvfIndex), intent(out) :: index
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: nlist
    integer(int32), intent(in) :: metric
    integer(int32), intent(out), optional :: status
    integer(int32) :: list_index

    call set_status(status, GLAMIN_OK, "")

    if (dim <= 0_int32 .or. nlist <= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_create invalid args")
      return
    end if

    if (metric /= METRIC_L2 .and. metric /= METRIC_IP) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_create unsupported metric")
      return
    end if

    index%dim = dim
    index%nlist = nlist
    index%nprobe = 1_int32
    index%metric = metric
    index%centroids = VectorBlock()
    index%ntotal = 0_int64
    index%is_trained = .false.

    allocate(index%lists(nlist))
    do list_index = 1_int32, nlist
      call reset_list(index%lists(list_index))
    end do
  end subroutine ivf_create

  subroutine ivf_create_handle(handle, dim, nlist, metric, status)
    type(IndexHandle), intent(out) :: handle
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: nlist
    integer(int32), intent(in) :: metric
    integer(int32), intent(out) :: status
    type(IvfIndex), pointer :: ivf_index
    integer(int32) :: alloc_status

    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_IVF

    allocate(ivf_index, stat=alloc_status)
    if (alloc_status /= 0_int32) then
      status = GLAMIN_ERR_OOM
      return
    end if

    call ivf_create(ivf_index, dim, nlist, metric, status)
    if (status /= GLAMIN_OK) then
      deallocate(ivf_index, stat=alloc_status)
      return
    end if

    handle%impl = c_loc(ivf_index)
    handle%kind = INDEX_KIND_IVF
    status = GLAMIN_OK
  end subroutine ivf_create_handle

  subroutine ivf_destroy_handle(handle, status)
    type(IndexHandle), intent(inout) :: handle
    integer(int32), intent(out) :: status
    type(IvfIndex), pointer :: ivf_index
    integer(int32) :: alloc_status

    call ivf_handle(handle, ivf_index)
    if (.not. associated(ivf_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call ivf_destroy(ivf_index, status)
    deallocate(ivf_index, stat=alloc_status)
    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_UNKNOWN
    status = GLAMIN_OK
  end subroutine ivf_destroy_handle

  subroutine ivf_handle(index_handle, ivf_index)
    type(IndexHandle), intent(in) :: index_handle
    type(IvfIndex), pointer :: ivf_index

    ivf_index => null()
    if (.not. c_associated(index_handle%impl)) then
      return
    end if

    if (index_handle%kind /= INDEX_KIND_IVF) then
      return
    end if

    call c_f_pointer(index_handle%impl, ivf_index)
  end subroutine ivf_handle

  subroutine ivf_train(index, vectors, status)
    type(IvfIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(out), optional :: status
    integer(int32) :: local_status

    call set_status(status, GLAMIN_OK, "")

    if (index%dim <= 0_int32 .or. index%nlist <= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_train invalid index")
      return
    end if

    if (vectors%dim /= index%dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_train dim mismatch")
      return
    end if

    call kmeans_train(vectors, index%nlist, index%metric, index%centroids, local_status)
    if (local_status /= GLAMIN_OK) then
      call set_status(status, local_status, "ivf_train kmeans failed")
      return
    end if

    call reset_lists(index)
    index%is_trained = .true.
    index%ntotal = 0_int64
  end subroutine ivf_train

  subroutine ivf_add(index, vectors, status)
    type(IvfIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(out), optional :: status
    integer(int32) :: local_status
    integer(int64) :: vector_count
    integer(int32) :: stride_v
    integer(int32) :: dim
    integer(int64) :: vec_index
    integer(int64) :: vec_offset
    integer(int32) :: list_index
    real(real32), pointer :: vector_data(:)

    call set_status(status, GLAMIN_OK, "")

    if (.not. index%is_trained) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_add index not trained")
      return
    end if

    if (.not. c_associated(vectors%data)) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_add vectors missing")
      return
    end if

    dim = index%dim
    if (vectors%dim /= dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_add dim mismatch")
      return
    end if

    vector_count = vectors%length
    if (vector_count <= 0_int64) then
      return
    end if

    stride_v = vectors%stride
    if (stride_v <= 0_int32) stride_v = dim
    if (stride_v < dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_add stride invalid")
      return
    end if

    call c_f_pointer(vectors%data, vector_data, [int(stride_v, int64) * vector_count])

    do vec_index = 1_int64, vector_count
      vec_offset = (vec_index - 1_int64) * stride_v
      call assign_list(index, vector_data(vec_offset + 1_int64:vec_offset + dim), list_index, &
        local_status)
      if (local_status /= GLAMIN_OK) then
        call set_status(status, local_status, "ivf_add assignment failed")
        return
      end if

      call append_to_list(index%lists(list_index), vector_data(vec_offset + 1_int64:vec_offset + dim), &
        dim, int(index%ntotal + vec_index, int32), local_status)
      if (local_status /= GLAMIN_OK) then
        call set_status(status, local_status, "ivf_add append failed")
        return
      end if
    end do

    index%ntotal = index%ntotal + vector_count
  end subroutine ivf_add

  subroutine ivf_search(index, queries, k, nprobe, distances, labels, status)
    type(IvfIndex), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    integer(int32), intent(in) :: nprobe
    type(VectorBlock), intent(inout) :: distances
    type(VectorBlock), intent(inout) :: labels
    integer(int32), intent(out), optional :: status
    integer(int32) :: local_status
    integer(int64) :: query_count
    integer(int32) :: dim
    integer(int32) :: stride_q
    integer(int32) :: result_k
    integer(int32) :: probe_count
    integer(int32) :: elem_bytes
    integer(int32) :: label_bytes
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int64) :: query_index
    integer(int64) :: query_offset
    integer(int64) :: output_offset
    integer(int64) :: batch_start
    integer(int32) :: probe_index
    integer(int32) :: list_index
    integer(int64) :: list_vector_index
    integer(int64) :: list_count
    integer(int32) :: batch_index
    integer(int32) :: batch_count
    integer(int64) :: list_distance_count
    real(real32), allocatable :: top_distances(:,:)
    integer(int32), allocatable :: top_labels(:,:)
    integer(int32), allocatable :: probe_lists(:,:)
    real(real32), allocatable, target :: query_buffer(:)
    logical, allocatable :: list_mask(:)
    logical :: has_list
    real(real32), pointer :: query_data(:)
    real(real32), pointer :: distance_data(:)
    real(real32), pointer :: list_distance_data(:)
    integer(int32), pointer :: label_data(:)
    logical :: use_l2
    type(VectorBlock) :: query_block
    type(VectorBlock) :: list_block
    type(VectorBlock) :: list_distances

    call set_status(status, GLAMIN_OK, "")

    if (.not. index%is_trained) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_search index not trained")
      return
    end if

    if (.not. c_associated(queries%data)) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_search queries missing")
      return
    end if

    dim = index%dim
    if (queries%dim /= dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_search dim mismatch")
      return
    end if

    query_count = queries%length
    if (query_count <= 0_int64 .or. index%ntotal <= 0_int64) then
      return
    end if

    use_l2 = index%metric == METRIC_L2

    result_k = min(max(1_int32, k), int(index%ntotal, int32))
    if (result_k <= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_search invalid k")
      return
    end if

    probe_count = min(max(1_int32, nprobe), index%nlist)

    stride_q = queries%stride
    if (stride_q <= 0_int32) stride_q = dim
    if (stride_q < dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "ivf_search stride invalid")
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
      call set_status(status, alloc_status, "ivf_search alloc distances failed")
      return
    end if

    call allocate_aligned(labels%data, &
      int(query_count * int(result_k, int64) * label_bytes, c_size_t), &
      int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      call set_status(status, alloc_status, "ivf_search alloc labels failed")
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

    allocate(list_mask(index%nlist), stat=alloc_status)
    if (alloc_status /= 0_int32) then
      call set_status(status, GLAMIN_ERR_OOM, "ivf_search list mask alloc failed")
      return
    end if

    call c_f_pointer(queries%data, query_data, [int(stride_q, int64) * query_count])
    call c_f_pointer(distances%data, distance_data, [int(result_k, int64) * query_count])
    call c_f_pointer(labels%data, label_data, [int(result_k, int64) * query_count])

    batch_start = 1_int64
    do while (batch_start <= query_count)
      batch_count = min(int(query_count - batch_start + 1_int64, int32), IVF_QUERY_BATCH)
      allocate(top_distances(result_k, batch_count))
      allocate(top_labels(result_k, batch_count))
      allocate(probe_lists(probe_count, batch_count))
      allocate(query_buffer(dim * batch_count), stat=alloc_status)
      if (alloc_status /= 0_int32) then
        deallocate(top_distances, top_labels, probe_lists)
        deallocate(list_mask)
        call set_status(status, GLAMIN_ERR_OOM, "ivf_search query buffer alloc failed")
        return
      end if

      query_block = VectorBlock()
      query_block%data = c_loc(query_buffer(1))
      query_block%length = int(batch_count, int64)
      query_block%dim = dim
      query_block%stride = dim
      query_block%elem_size = elem_bytes

      list_mask = .false.

      do batch_index = 1_int32, batch_count
        query_index = batch_start + int(batch_index - 1_int32, int64)
        query_offset = (query_index - 1_int64) * stride_q
        query_buffer((batch_index - 1_int32) * dim + 1_int32:batch_index * dim) = &
          query_data(query_offset + 1_int64:query_offset + dim)
        call init_top_k(top_distances(:, batch_index), top_labels(:, batch_index), use_l2)

        call select_probe_lists(index, query_data(query_offset + 1_int64:query_offset + dim), &
          probe_count, probe_lists(:, batch_index), local_status)
        if (local_status /= GLAMIN_OK) then
          deallocate(top_distances, top_labels, probe_lists, query_buffer)
          deallocate(list_mask)
          call set_status(status, local_status, "ivf_search probe selection failed")
          return
        end if

        do probe_index = 1_int32, probe_count
          list_index = probe_lists(probe_index, batch_index)
          if (list_index <= 0_int32 .or. list_index > size(index%lists)) cycle
          list_mask(list_index) = .true.
        end do
      end do

      do list_index = 1_int32, size(index%lists)
        if (.not. list_mask(list_index)) cycle
        if (index%lists(list_index)%count <= 0_int64) cycle

        list_count = index%lists(list_index)%count
        list_block = index%lists(list_index)%vectors
        if (list_block%stride <= 0_int32) list_block%stride = dim

        list_distances = VectorBlock()
        if (use_l2) then
          call gpu_distance_l2_dispatch(query_block, list_block, list_distances, local_status)
        else
          call gpu_distance_ip_dispatch(query_block, list_block, list_distances, local_status)
        end if
        if (local_status /= GLAMIN_OK) then
          if (c_associated(list_distances%data)) then
            call free_aligned(list_distances%data, free_status)
          end if
          deallocate(top_distances, top_labels, probe_lists, query_buffer)
          deallocate(list_mask)
          call set_status(status, local_status, "ivf_search list distance failed")
          return
        end if

        list_distance_count = list_count * int(batch_count, int64)
        call c_f_pointer(list_distances%data, list_distance_data, [int(list_distance_count, int64)])

        do batch_index = 1_int32, batch_count
          has_list = .false.
          do probe_index = 1_int32, probe_count
            if (probe_lists(probe_index, batch_index) == list_index) then
              has_list = .true.
              exit
            end if
          end do
          if (.not. has_list) cycle

          output_offset = int(batch_index - 1_int32, int64) * list_count
          do list_vector_index = 1_int64, list_count
            call update_top_k(list_distance_data(output_offset + list_vector_index), &
              index%lists(list_index)%labels(list_vector_index), use_l2, &
              top_distances(:, batch_index), top_labels(:, batch_index))
          end do
        end do

        if (c_associated(list_distances%data)) then
          call free_aligned(list_distances%data, free_status)
        end if
      end do

      do batch_index = 1_int32, batch_count
        query_index = batch_start + int(batch_index - 1_int32, int64)
        output_offset = (query_index - 1_int64) * result_k
        distance_data(output_offset + 1_int64:output_offset + result_k) = top_distances(:, batch_index)
        label_data(output_offset + 1_int64:output_offset + result_k) = top_labels(:, batch_index)
      end do

      deallocate(top_distances, top_labels, probe_lists, query_buffer)
      batch_start = batch_start + int(batch_count, int64)
    end do

    deallocate(list_mask)
  end subroutine ivf_search

  subroutine ivf_destroy(index, status)
    type(IvfIndex), intent(inout) :: index
    integer(int32), intent(out), optional :: status
    integer(int32) :: free_status

    call set_status(status, GLAMIN_OK, "")

    call reset_lists(index)

    if (c_associated(index%centroids%data)) then
      call free_aligned(index%centroids%data, free_status)
    end if
    index%centroids = VectorBlock()
    index%is_trained = .false.
    index%ntotal = 0_int64
  end subroutine ivf_destroy

  subroutine reset_lists(index)
    type(IvfIndex), intent(inout) :: index
    integer(int32) :: list_index

    if (.not. allocated(index%lists)) return
    do list_index = 1_int32, size(index%lists)
      call reset_list(index%lists(list_index))
    end do
  end subroutine reset_lists

  subroutine reset_list(list)
    type(IvfList), intent(inout) :: list
    integer(int32) :: free_status

    if (c_associated(list%vectors%data)) then
      call free_aligned(list%vectors%data, free_status)
    end if
    list%vectors = VectorBlock()
    list%count = 0_int64
    if (allocated(list%labels)) then
      deallocate(list%labels)
    end if
  end subroutine reset_list

  subroutine append_to_list(list, vector, dim, label, status)
    type(IvfList), intent(inout) :: list
    real(real32), intent(in) :: vector(:)
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: label
    integer(int32), intent(out) :: status
    integer(int64) :: new_count
    integer(int32) :: elem_bytes
    integer(int64) :: total_bytes
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int64) :: offset
    integer(int32) :: idx
    real(real32), pointer :: dst(:)
    real(real32), pointer :: src(:)
    type(c_ptr) :: new_data
    integer(int32), allocatable :: new_labels(:)

    status = GLAMIN_OK
    if (size(vector) < dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    new_count = list%count + 1_int64
    total_bytes = new_count * int(dim, int64) * int(elem_bytes, int64)

    call allocate_aligned(new_data, int(total_bytes, c_size_t), int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      status = alloc_status
      return
    end if

    call c_f_pointer(new_data, dst, [int(dim, int64) * new_count])
    if (list%count > 0_int64) then
      call c_f_pointer(list%vectors%data, src, [int(dim, int64) * list%count])
      dst(1_int64:int(dim, int64) * list%count) = src(1_int64:int(dim, int64) * list%count)
    end if

    offset = (new_count - 1_int64) * dim
    do idx = 1_int32, dim
      dst(offset + idx) = vector(idx)
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

    if (c_associated(list%vectors%data)) then
      call free_aligned(list%vectors%data, free_status)
    end if

    list%vectors%data = new_data
    list%vectors%length = new_count
    list%vectors%dim = dim
    list%vectors%stride = dim
    list%vectors%elem_size = elem_bytes
    list%vectors%alignment = 64
    list%count = new_count
  end subroutine append_to_list

  subroutine assign_list(index, vector, list_index, status)
    type(IvfIndex), intent(in) :: index
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
    type(IvfIndex), intent(in) :: index
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
end module glamin_index_ivf
