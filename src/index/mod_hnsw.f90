module glamin_index_hnsw
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_loc, c_null_ptr, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_IP, METRIC_L2
  use glamin_types, only: IndexHandle, VectorBlock, INDEX_KIND_HNSW, INDEX_KIND_UNKNOWN
  implicit none
  private

  public :: HnswIndex
  public :: hnsw_create
  public :: hnsw_add
  public :: hnsw_search
  public :: hnsw_destroy
  public :: hnsw_create_handle
  public :: hnsw_destroy_handle
  public :: hnsw_handle

  integer(int32), parameter :: DEFAULT_ALIGNMENT = 64

  type :: HnswIndex
    integer(int32) :: dim = 0
    integer(int32) :: m = 0
    integer(int32) :: ef_construction = 0
    integer(int32) :: ef_search = 0
    integer(int32) :: metric = 0
    type(VectorBlock) :: data
    integer(int64) :: ntotal = 0
    integer(int32), allocatable :: neighbors(:,:)
    integer(int32), allocatable :: neighbor_counts(:)
  end type HnswIndex

contains
  subroutine hnsw_create(index, dim, m, ef_construction, metric, status)
    type(HnswIndex), intent(out) :: index
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: ef_construction
    integer(int32), intent(in) :: metric
    integer(int32), intent(out), optional :: status

    call set_status(status, GLAMIN_OK, "")

    if (dim <= 0_int32 .or. m <= 0_int32 .or. ef_construction <= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_create invalid args")
      return
    end if

    if (metric /= METRIC_L2 .and. metric /= METRIC_IP) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_create unsupported metric")
      return
    end if

    index%dim = dim
    index%m = m
    index%ef_construction = ef_construction
    index%metric = metric
    index%ef_search = ef_construction
    index%data = VectorBlock()
    index%ntotal = 0_int64
  end subroutine hnsw_create

  subroutine hnsw_create_handle(handle, dim, m, ef_construction, metric, status)
    type(IndexHandle), intent(out) :: handle
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: ef_construction
    integer(int32), intent(in) :: metric
    integer(int32), intent(out) :: status
    type(HnswIndex), pointer :: hnsw_index
    integer(int32) :: alloc_status

    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_HNSW

    allocate(hnsw_index, stat=alloc_status)
    if (alloc_status /= 0_int32) then
      status = GLAMIN_ERR_OOM
      return
    end if

    call hnsw_create(hnsw_index, dim, m, ef_construction, metric, status)
    if (status /= GLAMIN_OK) then
      deallocate(hnsw_index, stat=alloc_status)
      return
    end if

    handle%impl = c_loc(hnsw_index)
    handle%kind = INDEX_KIND_HNSW
    status = GLAMIN_OK
  end subroutine hnsw_create_handle

  subroutine hnsw_destroy_handle(handle, status)
    type(IndexHandle), intent(inout) :: handle
    integer(int32), intent(out) :: status
    type(HnswIndex), pointer :: hnsw_index
    integer(int32) :: alloc_status

    call hnsw_handle(handle, hnsw_index)
    if (.not. associated(hnsw_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call hnsw_destroy(hnsw_index, status)
    deallocate(hnsw_index, stat=alloc_status)
    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_UNKNOWN
    status = GLAMIN_OK
  end subroutine hnsw_destroy_handle

  subroutine hnsw_handle(index_handle, hnsw_index)
    type(IndexHandle), intent(in) :: index_handle
    type(HnswIndex), pointer :: hnsw_index

    hnsw_index => null()
    if (.not. c_associated(index_handle%impl)) then
      return
    end if

    if (index_handle%kind /= INDEX_KIND_HNSW) then
      return
    end if

    call c_f_pointer(index_handle%impl, hnsw_index)
  end subroutine hnsw_handle

  subroutine hnsw_add(index, vectors, status)
    type(HnswIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(out), optional :: status
    integer(int32) :: dim
    integer(int32) :: stride_v
    integer(int32) :: elem_bytes
    integer(int64) :: vector_count
    integer(int64) :: old_total
    integer(int64) :: new_total
    integer(int64) :: total_bytes
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int64) :: vec_index
    integer(int64) :: vec_offset
    integer(int64) :: dst_offset
    integer(int32) :: node_index
    integer(int32) :: candidate
    integer(int32) :: neighbor_count
    integer(int32) :: neighbor_index
    integer(int32) :: top_count
    integer(int32), allocatable :: new_counts(:)
    integer(int32), allocatable :: new_neighbors(:,:)
    integer(int32), allocatable :: best_labels(:)
    real(real32), allocatable :: best_distances(:)
    type(c_ptr) :: new_data
    real(real32), pointer :: vector_data(:)
    real(real32), pointer :: dst_data(:)
    real(real32), pointer :: data(:)
    logical :: use_l2

    call set_status(status, GLAMIN_OK, "")

    if (index%dim <= 0_int32 .or. index%m <= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_add invalid index")
      return
    end if

    if (.not. c_associated(vectors%data)) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_add vectors missing")
      return
    end if

    dim = index%dim
    if (vectors%dim /= dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_add dim mismatch")
      return
    end if

    vector_count = vectors%length
    if (vector_count <= 0_int64) then
      return
    end if

    stride_v = vectors%stride
    if (stride_v <= 0_int32) stride_v = dim
    if (stride_v < dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_add stride invalid")
      return
    end if

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    if (vectors%elem_size /= 0_int32 .and. vectors%elem_size /= elem_bytes) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_add elem size")
      return
    end if

    old_total = index%ntotal
    new_total = old_total + vector_count
    total_bytes = new_total * int(dim, int64) * int(elem_bytes, int64)

    call allocate_aligned(new_data, int(total_bytes, c_size_t), &
      int(DEFAULT_ALIGNMENT, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      call set_status(status, alloc_status, "hnsw_add alloc failed")
      return
    end if

    call c_f_pointer(new_data, dst_data, [new_total * int(dim, int64)])
    if (old_total > 0_int64 .and. c_associated(index%data%data)) then
      call c_f_pointer(index%data%data, data, [old_total * int(dim, int64)])
      dst_data(1_int64:old_total * int(dim, int64)) = data(1_int64:old_total * int(dim, int64))
      call free_aligned(index%data%data, free_status)
    end if

    call c_f_pointer(vectors%data, vector_data, [int(stride_v, int64) * vector_count])
    do vec_index = 1_int64, vector_count
      vec_offset = (vec_index - 1_int64) * stride_v
      dst_offset = (old_total + vec_index - 1_int64) * dim
      dst_data(dst_offset + 1_int64:dst_offset + dim) = &
        vector_data(vec_offset + 1_int64:vec_offset + dim)
    end do

    index%data%data = new_data
    index%data%length = new_total
    index%data%dim = dim
    index%data%stride = dim
    index%data%elem_size = elem_bytes
    index%data%alignment = DEFAULT_ALIGNMENT
    index%ntotal = new_total

    allocate(new_neighbors(index%m, new_total))
    allocate(new_counts(new_total))
    new_neighbors = 0_int32
    new_counts = 0_int32
    if (allocated(index%neighbors)) then
      new_neighbors(:, 1:old_total) = index%neighbors(:, 1:old_total)
      deallocate(index%neighbors)
    end if
    if (allocated(index%neighbor_counts)) then
      new_counts(1:old_total) = index%neighbor_counts(1:old_total)
      deallocate(index%neighbor_counts)
    end if

    index%neighbors = new_neighbors
    index%neighbor_counts = new_counts

    use_l2 = index%metric == METRIC_L2
    allocate(best_labels(index%m))
    allocate(best_distances(index%m))

    call c_f_pointer(index%data%data, data, [new_total * int(dim, int64)])

    do node_index = int(old_total + 1_int64, int32), int(new_total, int32)
      index%neighbor_counts(node_index) = 0_int32
      if (node_index <= 1_int32) cycle

      call init_top_k(best_distances, best_labels, use_l2)
      top_count = 0_int32

      do candidate = 1_int32, node_index - 1_int32
        call update_top_k(distance_between(data, dim, node_index, candidate, use_l2), candidate, &
          use_l2, best_distances, best_labels, top_count, index%m)
      end do

      neighbor_count = min(node_index - 1_int32, index%m, top_count)
      if (neighbor_count > 0_int32) then
        index%neighbors(1:neighbor_count, node_index) = best_labels(1:neighbor_count)
        index%neighbor_counts(node_index) = neighbor_count
      end if

      do neighbor_index = 1_int32, neighbor_count
        call add_backlink(index, data, dim, best_labels(neighbor_index), node_index, &
          best_distances(neighbor_index), use_l2)
      end do
    end do

    deallocate(best_labels, best_distances)
  end subroutine hnsw_add

  subroutine hnsw_search(index, queries, k, ef_search, distances, labels, status)
    type(HnswIndex), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    integer(int32), intent(in) :: ef_search
    type(VectorBlock), intent(inout) :: distances
    type(VectorBlock), intent(inout) :: labels
    integer(int32), intent(out), optional :: status
    integer(int32) :: dim
    integer(int32) :: stride_q
    integer(int32) :: elem_bytes
    integer(int32) :: label_bytes
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int32) :: result_k
    integer(int32) :: effective_ef
    integer(int64) :: query_count
    integer(int32) :: query_index
    integer(int64) :: query_offset
    integer(int64) :: output_offset
    integer(int32) :: candidate_count
    integer(int32) :: candidate_index
    integer(int32) :: best_index
    integer(int32) :: best_label
    integer(int32) :: neighbor_index
    integer(int32) :: neighbor
    integer(int32) :: top_count
    integer(int64) :: total_queries
    integer(int64) :: data_offset
    real(real32) :: best_distance
    real(real32) :: distance
    real(real32), allocatable :: candidate_distances(:)
    integer(int32), allocatable :: candidate_labels(:)
    real(real32), allocatable :: top_distances(:)
    integer(int32), allocatable :: top_labels(:)
    logical, allocatable :: visited(:)
    real(real32), pointer :: query_data(:)
    real(real32), pointer :: data(:)
    real(real32), pointer :: distance_data(:)
    integer(int32), pointer :: label_data(:)
    logical :: use_l2

    call set_status(status, GLAMIN_OK, "")

    if (index%ntotal <= 0_int64) then
      return
    end if

    if (.not. c_associated(queries%data)) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_search queries missing")
      return
    end if

    dim = index%dim
    if (queries%dim /= dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_search dim mismatch")
      return
    end if

    query_count = queries%length
    if (query_count <= 0_int64) then
      return
    end if

    result_k = min(max(1_int32, k), int(index%ntotal, int32))
    if (result_k <= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_search invalid k")
      return
    end if

    effective_ef = max(result_k, ef_search)
    effective_ef = min(effective_ef, int(index%ntotal, int32))
    if (effective_ef <= 0_int32) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_search invalid ef")
      return
    end if

    stride_q = queries%stride
    if (stride_q <= 0_int32) stride_q = dim
    if (stride_q < dim) then
      call set_status(status, GLAMIN_ERR_INVALID_ARG, "hnsw_search stride invalid")
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

    total_queries = query_count * int(result_k, int64)
    call allocate_aligned(distances%data, int(total_queries * elem_bytes, c_size_t), &
      int(DEFAULT_ALIGNMENT, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      call set_status(status, alloc_status, "hnsw_search alloc distances failed")
      return
    end if

    call allocate_aligned(labels%data, int(total_queries * label_bytes, c_size_t), &
      int(DEFAULT_ALIGNMENT, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      call set_status(status, alloc_status, "hnsw_search alloc labels failed")
      return
    end if

    distances%dim = result_k
    distances%length = query_count
    distances%stride = result_k
    distances%elem_size = elem_bytes
    distances%alignment = DEFAULT_ALIGNMENT

    labels%dim = result_k
    labels%length = query_count
    labels%stride = result_k
    labels%elem_size = label_bytes
    labels%alignment = DEFAULT_ALIGNMENT

    allocate(candidate_distances(index%ntotal))
    allocate(candidate_labels(index%ntotal))
    allocate(top_distances(effective_ef))
    allocate(top_labels(effective_ef))
    allocate(visited(index%ntotal))

    call c_f_pointer(queries%data, query_data, [int(stride_q, int64) * query_count])
    call c_f_pointer(index%data%data, data, [index%ntotal * int(dim, int64)])
    call c_f_pointer(distances%data, distance_data, [int(result_k, int64) * query_count])
    call c_f_pointer(labels%data, label_data, [int(result_k, int64) * query_count])

    use_l2 = index%metric == METRIC_L2

    do query_index = 1_int32, int(query_count, int32)
      query_offset = int(query_index - 1_int32, int64) * stride_q
      visited = .false.
      candidate_count = 1_int32
      top_count = 0_int32

      data_offset = 0_int64
      distance = distance_query(query_data(query_offset + 1_int64:query_offset + dim), &
        data(data_offset + 1_int64:data_offset + dim), use_l2)

      candidate_labels(1_int32) = 1_int32
      candidate_distances(1_int32) = distance
      visited(1_int32) = .true.

      call init_top_k(top_distances, top_labels, use_l2)
      call update_top_k(distance, 1_int32, use_l2, top_distances, top_labels, top_count, effective_ef)

      do
        if (candidate_count <= 0_int32) exit

        best_index = 1_int32
        best_distance = candidate_distances(1_int32)
        if (.not. use_l2) best_distance = -huge(1.0_real32)
        do candidate_index = 1_int32, candidate_count
          if (use_l2) then
            if (candidate_distances(candidate_index) < best_distance) then
              best_distance = candidate_distances(candidate_index)
              best_index = candidate_index
            end if
          else
            if (candidate_distances(candidate_index) > best_distance) then
              best_distance = candidate_distances(candidate_index)
              best_index = candidate_index
            end if
          end if
        end do

        if (top_count >= effective_ef) then
          if (use_l2) then
            if (best_distance > top_distances(top_count)) exit
          else
            if (best_distance < top_distances(top_count)) exit
          end if
        end if

        best_label = candidate_labels(best_index)
        candidate_labels(best_index) = candidate_labels(candidate_count)
        candidate_distances(best_index) = candidate_distances(candidate_count)
        candidate_count = candidate_count - 1_int32

        do neighbor_index = 1_int32, index%neighbor_counts(best_label)
          neighbor = index%neighbors(neighbor_index, best_label)
          if (neighbor <= 0_int32) cycle
          if (visited(neighbor)) cycle
          visited(neighbor) = .true.

          data_offset = int(neighbor - 1_int32, int64) * dim
          distance = distance_query(query_data(query_offset + 1_int64:query_offset + dim), &
            data(data_offset + 1_int64:data_offset + dim), use_l2)

          if (top_count < effective_ef) then
            candidate_count = candidate_count + 1_int32
            candidate_labels(candidate_count) = neighbor
            candidate_distances(candidate_count) = distance
            call update_top_k(distance, neighbor, use_l2, top_distances, top_labels, top_count, &
              effective_ef)
          else if (use_l2 .and. distance < top_distances(top_count)) then
            candidate_count = candidate_count + 1_int32
            candidate_labels(candidate_count) = neighbor
            candidate_distances(candidate_count) = distance
            call update_top_k(distance, neighbor, use_l2, top_distances, top_labels, top_count, &
              effective_ef)
          else if ((.not. use_l2) .and. distance > top_distances(top_count)) then
            candidate_count = candidate_count + 1_int32
            candidate_labels(candidate_count) = neighbor
            candidate_distances(candidate_count) = distance
            call update_top_k(distance, neighbor, use_l2, top_distances, top_labels, top_count, &
              effective_ef)
          end if
        end do
      end do

      output_offset = int(query_index - 1_int32, int64) * result_k
      distance_data(output_offset + 1_int64:output_offset + result_k) = top_distances(1:result_k)
      label_data(output_offset + 1_int64:output_offset + result_k) = top_labels(1:result_k)
    end do

    deallocate(candidate_distances, candidate_labels, top_distances, top_labels, visited)
  end subroutine hnsw_search

  subroutine hnsw_destroy(index, status)
    type(HnswIndex), intent(inout) :: index
    integer(int32), intent(out), optional :: status
    integer(int32) :: free_status

    call set_status(status, GLAMIN_OK, "")

    if (c_associated(index%data%data)) then
      call free_aligned(index%data%data, free_status)
    end if

    if (allocated(index%neighbors)) then
      deallocate(index%neighbors)
    end if
    if (allocated(index%neighbor_counts)) then
      deallocate(index%neighbor_counts)
    end if

    index%data = VectorBlock()
    index%ntotal = 0_int64
  end subroutine hnsw_destroy

  subroutine add_backlink(index, data, dim, node_id, neighbor_id, distance, use_l2)
    type(HnswIndex), intent(inout) :: index
    real(real32), intent(in) :: data(:)
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: node_id
    integer(int32), intent(in) :: neighbor_id
    real(real32), intent(in) :: distance
    logical, intent(in) :: use_l2
    integer(int32) :: count
    integer(int32) :: idx
    integer(int32) :: worst_idx
    real(real32) :: worst_distance
    real(real32) :: candidate_distance

    if (node_id <= 0_int32 .or. neighbor_id <= 0_int32) return
    if (node_id > size(index%neighbor_counts)) return

    count = index%neighbor_counts(node_id)
    do idx = 1_int32, count
      if (index%neighbors(idx, node_id) == neighbor_id) return
    end do

    if (count < index%m) then
      index%neighbor_counts(node_id) = count + 1_int32
      index%neighbors(count + 1_int32, node_id) = neighbor_id
      return
    end if

    worst_idx = 1_int32
    worst_distance = distance_between(data, dim, node_id, index%neighbors(1_int32, node_id), use_l2)
    do idx = 2_int32, index%m
      if (index%neighbors(idx, node_id) <= 0_int32) cycle
      candidate_distance = distance_between(data, dim, node_id, index%neighbors(idx, node_id), use_l2)
      if (use_l2) then
        if (candidate_distance > worst_distance) then
          worst_distance = candidate_distance
          worst_idx = idx
        end if
      else
        if (candidate_distance < worst_distance) then
          worst_distance = candidate_distance
          worst_idx = idx
        end if
      end if
    end do

    if (use_l2) then
      if (distance < worst_distance) then
        index%neighbors(worst_idx, node_id) = neighbor_id
      end if
    else
      if (distance > worst_distance) then
        index%neighbors(worst_idx, node_id) = neighbor_id
      end if
    end if
  end subroutine add_backlink

  function distance_between(data, dim, index_a, index_b, use_l2) result(distance)
    real(real32), intent(in) :: data(:)
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: index_a
    integer(int32), intent(in) :: index_b
    logical, intent(in) :: use_l2
    real(real32) :: distance
    integer(int64) :: offset_a
    integer(int64) :: offset_b
    integer(int32) :: axis_index
    real(real32) :: diff

    offset_a = int(index_a - 1_int32, int64) * dim
    offset_b = int(index_b - 1_int32, int64) * dim
    distance = 0.0_real32
    if (use_l2) then
      do axis_index = 1_int32, dim
        diff = data(offset_a + axis_index) - data(offset_b + axis_index)
        distance = distance + diff * diff
      end do
    else
      do axis_index = 1_int32, dim
        distance = distance + data(offset_a + axis_index) * data(offset_b + axis_index)
      end do
    end if
  end function distance_between

  function distance_query(query, vector, use_l2) result(distance)
    real(real32), intent(in) :: query(:)
    real(real32), intent(in) :: vector(:)
    logical, intent(in) :: use_l2
    real(real32) :: distance
    integer(int32) :: axis_index
    real(real32) :: diff

    distance = 0.0_real32
    if (use_l2) then
      do axis_index = 1_int32, size(query)
        diff = query(axis_index) - vector(axis_index)
        distance = distance + diff * diff
      end do
    else
      do axis_index = 1_int32, size(query)
        distance = distance + query(axis_index) * vector(axis_index)
      end do
    end if
  end function distance_query

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

  subroutine update_top_k(distance, label, use_l2, top_distances, top_labels, top_count, max_count)
    real(real32), intent(in) :: distance
    integer(int32), intent(in) :: label
    logical, intent(in) :: use_l2
    real(real32), intent(inout) :: top_distances(:)
    integer(int32), intent(inout) :: top_labels(:)
    integer(int32), intent(inout) :: top_count
    integer(int32), intent(in) :: max_count
    integer(int32) :: insert_index
    integer(int32) :: idx
    real(real32) :: swap_distance
    integer(int32) :: swap_label

    if (max_count <= 0_int32) return

    if (top_count < max_count) then
      top_count = top_count + 1_int32
      insert_index = top_count
      top_distances(insert_index) = distance
      top_labels(insert_index) = label
    else
      insert_index = max_count
      if (use_l2) then
        if (distance >= top_distances(insert_index)) then
          return
        end if
      else
        if (distance <= top_distances(insert_index)) then
          return
        end if
      end if
      top_distances(insert_index) = distance
      top_labels(insert_index) = label
    end if

    do idx = insert_index - 1_int32, 1_int32, -1_int32
      if (use_l2) then
        if (top_distances(idx) <= top_distances(idx + 1_int32)) exit
      else
        if (top_distances(idx) >= top_distances(idx + 1_int32)) exit
      end if

      swap_distance = top_distances(idx)
      top_distances(idx) = top_distances(idx + 1_int32)
      top_distances(idx + 1_int32) = swap_distance

      swap_label = top_labels(idx)
      top_labels(idx) = top_labels(idx + 1_int32)
      top_labels(idx + 1_int32) = swap_label
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
end module glamin_index_hnsw
