module glamin_index_flat
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_size_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  use glamin_memory, only: allocate_aligned
  use glamin_types, only: VectorBlock, IndexHandle
  implicit none
  private

  public :: FlatIndex
  public :: flat_create
  public :: flat_add
  public :: flat_search
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

  subroutine flat_add(index, vectors)
    type(IndexHandle), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    type(FlatIndex), pointer :: flat_index
    integer(int32) :: status

    call flat_handle(index, flat_index)
    if (.not. associated(flat_index)) then
      return
    end if

    call append_vectors(flat_index, vectors, status)
  end subroutine flat_add

  subroutine flat_search(index, queries, k, distances, labels)
    type(IndexHandle), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    type(VectorBlock), intent(inout) :: distances
    type(VectorBlock), intent(inout) :: labels
    type(FlatIndex), pointer :: flat_index
    integer(int32) :: status

    call flat_handle(index, flat_index)
    if (.not. associated(flat_index)) then
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
    real(real32), pointer :: dst(:)
    real(real32), pointer :: src(:)

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

    if (.not. c_associated(flat_index%data%data)) then
      call allocate_aligned(flat_index%data%data, int(total_bytes, c_size_t), &
        int(64, c_size_t), alloc_status)
      if (alloc_status /= GLAMIN_OK) then
        status = alloc_status
        return
      end if
    else
      call allocate_aligned(flat_index%data%data, int(total_bytes, c_size_t), &
        int(64, c_size_t), alloc_status)
      if (alloc_status /= GLAMIN_OK) then
        status = alloc_status
        return
      end if
    end if

    call c_f_pointer(flat_index%data%data, dst, [int(flat_index%dim, int64) * total])
    call c_f_pointer(vectors%data, src, [int(stride, int64) * incoming])

    if (existing > 0_int64) then
      ! Existing data is not preserved in this placeholder implementation.
    end if

    dst(1:int(flat_index%dim, int64) * incoming) = &
      src(1:int(stride, int64) * incoming)

    flat_index%data%length = total
    flat_index%data%dim = flat_index%dim
    flat_index%data%stride = flat_index%dim
    flat_index%data%elem_size = elem_bytes
  end subroutine append_vectors

  subroutine compute_flat_search(flat_index, queries, k, distances, labels, status)
    type(FlatIndex), intent(in) :: flat_index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    type(VectorBlock), intent(inout) :: distances
    type(VectorBlock), intent(inout) :: labels
    integer(int32), intent(out) :: status
    integer(int32) :: elem_bytes
    integer(int64) :: query_count
    integer(int64) :: vector_count
    integer(int64) :: total_distances
    integer(int64) :: total_labels
    integer(int32) :: alloc_status
    real(real32), pointer :: distance_data(:)
    integer(int32), pointer :: label_data(:)

    status = GLAMIN_OK
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

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    total_distances = query_count * vector_count
    call allocate_aligned(distances%data, int(total_distances * elem_bytes, c_size_t), &
      int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      status = alloc_status
      return
    end if

    total_labels = query_count * max(1_int32, k)
    call allocate_aligned(labels%data, int(total_labels * int32_size(), c_size_t), &
      int(64, c_size_t), alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      status = alloc_status
      return
    end if

    distances%dim = int(vector_count, int32)
    distances%length = query_count
    distances%stride = int(vector_count, int32)
    distances%elem_size = elem_bytes

    labels%dim = max(1_int32, k)
    labels%length = query_count
    labels%stride = labels%dim
    labels%elem_size = int32_size()

    call c_f_pointer(distances%data, distance_data, [int(total_distances, int64)])
    call c_f_pointer(labels%data, label_data, [int(total_labels, int64)])

    distance_data = 0.0_real32
    label_data = 0_int32
  end subroutine compute_flat_search

  pure integer(int32) function int32_size()
    int32_size = int(storage_size(0_int32) / 8, int32)
  end function int32_size
end module glamin_index_flat
