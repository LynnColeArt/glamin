module glamin_distance
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_size_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  use glamin_memory, only: allocate_aligned
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: distance_l2_batch
  public :: distance_ip_batch

  integer(int32), parameter :: DEFAULT_ALIGNMENT = 64
#ifndef GLAMIN_QUERY_BLOCK
#define GLAMIN_QUERY_BLOCK 16
#endif
#ifndef GLAMIN_VECTOR_BLOCK
#define GLAMIN_VECTOR_BLOCK 128
#endif
  integer(int32), parameter :: QUERY_BLOCK = GLAMIN_QUERY_BLOCK
  integer(int32), parameter :: VECTOR_BLOCK = GLAMIN_VECTOR_BLOCK

contains
  subroutine distance_l2_batch(queries, vectors, distances, status)
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status

    call compute_distance(queries, vectors, distances, status, .true.)
  end subroutine distance_l2_batch

  subroutine distance_ip_batch(queries, vectors, distances, status)
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status

    call compute_distance(queries, vectors, distances, status, .false.)
  end subroutine distance_ip_batch

  subroutine compute_distance(queries, vectors, distances, status, use_l2)
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status
    logical, intent(in) :: use_l2
    integer(int64) :: query_count
    integer(int64) :: vector_count
    integer(int32) :: dim
    integer(int32) :: stride_q
    integer(int32) :: stride_v
    integer(int32) :: stride_d
    integer(int32) :: elem_bytes
    integer(int64) :: total_bytes
    integer(int32) :: alloc_status
    integer(int64) :: query_index
    integer(int64) :: vector_index
    integer(int64) :: query_offset
    integer(int64) :: vector_offset
    integer(int64) :: distance_offset
    integer(int64) :: query_start
    integer(int64) :: query_end
    integer(int64) :: vector_start
    integer(int64) :: vector_end
    real(real32) :: accum
    real(real32) :: dot_value
    real(real32), pointer :: query_data(:)
    real(real32), pointer :: vector_data(:)
    real(real32), pointer :: distance_data(:)
    real(real32), pointer :: query_slice(:)
    real(real32), pointer :: vector_slice(:)
    real(real32), allocatable :: query_norms(:)
    real(real32), allocatable :: vector_norms(:)

    status = GLAMIN_OK
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

    stride_q = queries%stride
    stride_v = vectors%stride
    if (stride_q <= 0_int32) stride_q = dim
    if (stride_v <= 0_int32) stride_v = dim

    if (stride_q < dim .or. stride_v < dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    stride_d = int(vector_count, int32)
    distances%dim = stride_d
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

    call c_f_pointer(queries%data, query_data, [int(stride_q, int64) * query_count])
    call c_f_pointer(vectors%data, vector_data, [int(stride_v, int64) * vector_count])
    call c_f_pointer(distances%data, distance_data, [int(stride_d, int64) * query_count])

    if (use_l2) then
      allocate(query_norms(query_count))
      allocate(vector_norms(vector_count))

      do query_index = 1_int64, query_count
        query_offset = (query_index - 1_int64) * stride_q
        query_slice => query_data(query_offset + 1_int64:query_offset + dim)
        query_norms(query_index) = dot_product(query_slice, query_slice)
      end do

      do vector_index = 1_int64, vector_count
        vector_offset = (vector_index - 1_int64) * stride_v
        vector_slice => vector_data(vector_offset + 1_int64:vector_offset + dim)
        vector_norms(vector_index) = dot_product(vector_slice, vector_slice)
      end do

      query_start = 1_int64
      do while (query_start <= query_count)
        query_end = min(query_count, query_start + int(QUERY_BLOCK, int64) - 1_int64)
        vector_start = 1_int64
        do while (vector_start <= vector_count)
          vector_end = min(vector_count, vector_start + int(VECTOR_BLOCK, int64) - 1_int64)
          do query_index = query_start, query_end
            query_offset = (query_index - 1_int64) * stride_q
            distance_offset = (query_index - 1_int64) * stride_d
            query_slice => query_data(query_offset + 1_int64:query_offset + dim)
            do vector_index = vector_start, vector_end
              vector_offset = (vector_index - 1_int64) * stride_v
              vector_slice => vector_data(vector_offset + 1_int64:vector_offset + dim)
              dot_value = dot_product(query_slice, vector_slice)
              accum = query_norms(query_index) + vector_norms(vector_index) - &
                2.0_real32 * dot_value
              if (accum < 0.0_real32) accum = 0.0_real32
              distance_data(distance_offset + vector_index) = accum
            end do
          end do
          vector_start = vector_end + 1_int64
        end do
        query_start = query_end + 1_int64
      end do

      deallocate(query_norms, vector_norms)
    else
      query_start = 1_int64
      do while (query_start <= query_count)
        query_end = min(query_count, query_start + int(QUERY_BLOCK, int64) - 1_int64)
        vector_start = 1_int64
        do while (vector_start <= vector_count)
          vector_end = min(vector_count, vector_start + int(VECTOR_BLOCK, int64) - 1_int64)
          do query_index = query_start, query_end
            query_offset = (query_index - 1_int64) * stride_q
            distance_offset = (query_index - 1_int64) * stride_d
            query_slice => query_data(query_offset + 1_int64:query_offset + dim)
            do vector_index = vector_start, vector_end
              vector_offset = (vector_index - 1_int64) * stride_v
              vector_slice => vector_data(vector_offset + 1_int64:vector_offset + dim)
              dot_value = dot_product(query_slice, vector_slice)
              distance_data(distance_offset + vector_index) = dot_value
            end do
          end do
          vector_start = vector_end + 1_int64
        end do
        query_start = query_end + 1_int64
      end do
    end if
  end subroutine compute_distance
end module glamin_distance
