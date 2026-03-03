module glamin_index_flat
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_loc, c_null_ptr, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  use glamin_metrics, only: METRIC_L2, METRIC_IP
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_types, only: VectorBlock, IndexHandle, INDEX_KIND_FLAT, INDEX_KIND_UNKNOWN
  implicit none
  private

  public :: FlatIndex
  public :: flat_create
  public :: flat_add
  public :: flat_search
  public :: flat_create_handle
  public :: flat_destroy_handle
  public :: flat_handle

  type :: FlatIndex
    integer(int32) :: dim = 0
    integer(int32) :: metric = 0
    type(VectorBlock) :: data
  end type FlatIndex

contains
  subroutine flat_create(index, dim, metric)
    type(FlatIndex), intent(out) :: index
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: metric

    index%dim = dim
    index%metric = metric
    index%data = VectorBlock()
  end subroutine flat_create

  subroutine flat_create_handle(handle, dim, metric, status)
    type(IndexHandle), intent(out) :: handle
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: metric
    integer(int32), intent(out) :: status
    type(FlatIndex), pointer :: flat_index
    integer(int32) :: alloc_status

    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_FLAT
    if (dim <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    allocate(flat_index, stat=alloc_status)
    if (alloc_status /= 0_int32) then
      status = GLAMIN_ERR_OOM
      return
    end if

    call flat_create(flat_index, dim, metric)
    handle%impl = c_loc(flat_index)
    handle%kind = INDEX_KIND_FLAT
    status = GLAMIN_OK
  end subroutine flat_create_handle

  subroutine flat_destroy_handle(handle, status)
    type(IndexHandle), intent(inout) :: handle
    integer(int32), intent(out) :: status
    type(FlatIndex), pointer :: flat_index
    integer(int32) :: free_status
    integer(int32) :: alloc_status

    call flat_handle(handle, flat_index)
    if (.not. associated(flat_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (c_associated(flat_index%data%data)) then
      call free_aligned(flat_index%data%data, free_status)
    end if

    deallocate(flat_index, stat=alloc_status)
    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_UNKNOWN
    status = GLAMIN_OK
  end subroutine flat_destroy_handle

  subroutine flat_add(index, vectors, status)
    type(IndexHandle), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(out) :: status
    type(FlatIndex), pointer :: flat_index

    call flat_handle(index, flat_index)
    if (.not. associated(flat_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call append_vectors(flat_index, vectors, status)
  end subroutine flat_add

  subroutine flat_search(index, queries, k, distances, labels, status)
    type(IndexHandle), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    type(VectorBlock), intent(inout) :: distances
    type(VectorBlock), intent(inout) :: labels
    integer(int32), intent(out) :: status
    type(FlatIndex), pointer :: flat_index

    call flat_handle(index, flat_index)
    if (.not. associated(flat_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call compute_flat_search(flat_index, queries, k, distances, labels, status)
  end subroutine flat_search

  subroutine flat_handle(index_handle, flat_index)
    type(IndexHandle), intent(in) :: index_handle
    type(FlatIndex), pointer :: flat_index

    flat_index => null()
    if (.not. c_associated(index_handle%impl)) then
      return
    end if

    if (index_handle%kind /= INDEX_KIND_FLAT .and. &
        index_handle%kind /= INDEX_KIND_UNKNOWN) then
      return
    end if

    call c_f_pointer(index_handle%impl, flat_index)
  end subroutine flat_handle

  subroutine append_vectors(flat_index, vectors, status)
    type(FlatIndex), intent(inout) :: flat_index
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(out) :: status
    integer(int32) :: dim
    integer(int32) :: stride
    integer(int32) :: elem_bytes
    integer(int64) :: existing
    integer(int64) :: incoming
    integer(int64) :: total
    integer(int64) :: total_bytes
    integer(int32) :: alloc_status
    type(c_ptr) :: new_data
    type(c_ptr) :: old_data
    integer(int32) :: free_status
    real(real32), pointer :: dst(:)
    real(real32), pointer :: src(:)
    real(real32), pointer :: old_src(:)

    status = GLAMIN_OK
    dim = vectors%dim
    if (dim <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (flat_index%dim == 0_int32) then
      flat_index%dim = dim
    end if

    if (flat_index%dim /= dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    stride = vectors%stride
    if (stride <= 0_int32) stride = dim
    if (stride < dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    if (vectors%elem_size /= 0_int32 .and. vectors%elem_size /= elem_bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    existing = flat_index%data%length
    incoming = vectors%length
    if (incoming <= 0_int64) then
      return
    end if

    total = existing + incoming
    total_bytes = int(flat_index%dim, int64) * total * elem_bytes

    old_data = flat_index%data%data
    call allocate_aligned(new_data, int(total_bytes, c_size_t), &
      int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      status = alloc_status
      return
    end if

    call c_f_pointer(new_data, dst, [int(flat_index%dim, int64) * total])
    call c_f_pointer(vectors%data, src, [int(stride, int64) * incoming])

    if (existing > 0_int64) then
      call c_f_pointer(old_data, old_src, [int(flat_index%dim, int64) * existing])
      dst(1:int(flat_index%dim, int64) * existing) = &
        old_src(1:int(flat_index%dim, int64) * existing)
    end if

    dst(int(flat_index%dim, int64) * existing + 1_int64: &
        int(flat_index%dim, int64) * total) = &
      src(1:int(stride, int64) * incoming)

    if (c_associated(old_data)) then
      call free_aligned(old_data, free_status)
    end if

    flat_index%data%data = new_data
    flat_index%data%length = total
    flat_index%data%dim = flat_index%dim
    flat_index%data%stride = flat_index%dim
    flat_index%data%elem_size = elem_bytes
    flat_index%data%alignment = 64
  end subroutine append_vectors

  subroutine compute_flat_search(flat_index, queries, k, distances, labels, status)
    type(FlatIndex), intent(in) :: flat_index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    type(VectorBlock), intent(inout) :: distances
    type(VectorBlock), intent(inout) :: labels
    integer(int32), intent(out) :: status
    integer(int32) :: elem_bytes
    integer(int32) :: label_bytes
    integer(int32) :: result_k
    integer(int64) :: query_count
    integer(int64) :: vector_count
    integer(int32) :: alloc_status
    integer(int32) :: stride_q
    integer(int32) :: dim
    integer(int32) :: dim_index
    integer(int64) :: query_index
    integer(int64) :: vector_index
    integer(int64) :: query_offset
    integer(int64) :: vector_offset
    integer(int64) :: output_offset
    real(real32) :: accum
    real(real32) :: diff
    logical :: use_l2
    real(real32), allocatable :: top_distances(:)
    integer(int32), allocatable :: top_labels(:)
    real(real32), pointer :: query_data(:)
    real(real32), pointer :: vector_data(:)
    real(real32), pointer :: distance_data(:)
    integer(int32), pointer :: label_data(:)
    integer(int32) :: free_status

    status = GLAMIN_OK
    if (.not. c_associated(flat_index%data%data) .or. .not. c_associated(queries%data)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (flat_index%data%length <= 0_int64) then
      return
    end if

    if (queries%dim /= flat_index%dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    query_count = queries%length
    vector_count = flat_index%data%length
    if (query_count <= 0_int64 .or. vector_count <= 0_int64) then
      return
    end if

    dim = flat_index%dim
    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    label_bytes = int32_size()

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

    if (queries%elem_size /= 0_int32 .and. queries%elem_size /= elem_bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (flat_index%data%elem_size /= 0_int32 .and. flat_index%data%elem_size /= elem_bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (flat_index%metric == METRIC_L2) then
      use_l2 = .true.
    else if (flat_index%metric == METRIC_IP) then
      use_l2 = .false.
    else
      status = GLAMIN_ERR_INVALID_ARG
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
    call c_f_pointer(flat_index%data%data, vector_data, [int(dim, int64) * vector_count])
    call c_f_pointer(distances%data, distance_data, [int(result_k, int64) * query_count])
    call c_f_pointer(labels%data, label_data, [int(result_k, int64) * query_count])

    do query_index = 1_int64, query_count
      query_offset = (query_index - 1_int64) * stride_q
      output_offset = (query_index - 1_int64) * result_k
      if (use_l2) then
        top_distances = huge(0.0_real32)
      else
        top_distances = -huge(0.0_real32)
      end if
      top_labels = -1_int32

      do vector_index = 1_int64, vector_count
        vector_offset = (vector_index - 1_int64) * dim
        accum = 0.0_real32
        do dim_index = 1_int32, dim
          if (use_l2) then
            diff = query_data(query_offset + dim_index) - &
              vector_data(vector_offset + dim_index)
            accum = accum + diff * diff
          else
            accum = accum + query_data(query_offset + dim_index) * &
              vector_data(vector_offset + dim_index)
          end if
        end do

        call update_top_k(accum, int(vector_index - 1_int64, int32), use_l2, &
          top_distances, top_labels)
      end do

      distance_data(output_offset + 1_int64:output_offset + result_k) = top_distances
      label_data(output_offset + 1_int64:output_offset + result_k) = top_labels
    end do

    deallocate(top_distances)
    deallocate(top_labels)
  end subroutine compute_flat_search

  pure integer(int32) function int32_size()
    int32_size = int(storage_size(0_int32) / 8, int32)
  end function int32_size

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
end module glamin_index_flat
