module glamin_kmeans
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_metrics, only: METRIC_IP, METRIC_L2
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: kmeans_train

  integer(int32), parameter :: DEFAULT_ITERS = 5
  integer(int32), parameter :: DEFAULT_ALIGNMENT = 64

contains
  subroutine kmeans_train(vectors, k, metric, centroids, status)
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(in) :: k
    integer(int32), intent(in) :: metric
    type(VectorBlock), intent(inout) :: centroids
    integer(int32), intent(out) :: status
    integer(int32) :: dim
    integer(int32) :: stride_v
    integer(int64) :: vector_count
    integer(int32) :: elem_bytes
    integer(int64) :: total_bytes
    integer(int32) :: alloc_status
    integer(int32) :: free_status
    integer(int32) :: effective_k
    integer(int32) :: iter
    integer(int64) :: vec_index
    integer(int32) :: centroid_index
    integer(int32) :: dim_index
    integer(int32) :: axis_index
    integer(int32) :: best_index
    integer(int64) :: vec_offset
    integer(int64) :: centroid_offset
    real(real32) :: best_distance
    real(real32) :: distance
    real(real32) :: diff
    real(real32), pointer :: vector_data(:)
    real(real32), pointer :: centroid_data(:)
    real(real32), allocatable :: sums(:,:)
    integer(int32), allocatable :: counts(:)
    logical :: use_l2

    status = GLAMIN_OK
    if (.not. c_associated(vectors%data)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    dim = vectors%dim
    if (dim <= 0_int32 .or. k <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    vector_count = vectors%length
    if (vector_count <= 0_int64) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    use_l2 = metric == METRIC_L2
    if (.not. use_l2 .and. metric /= METRIC_IP) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    stride_v = vectors%stride
    if (stride_v <= 0_int32) stride_v = dim
    if (stride_v < dim) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    elem_bytes = int(storage_size(0.0_real32) / 8, int32)
    if (vectors%elem_size /= 0_int32 .and. vectors%elem_size /= elem_bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (k > int(vector_count, int32)) then
      effective_k = int(vector_count, int32)
    else
      effective_k = k
    end if

    if (.not. c_associated(centroids%data) .or. centroids%length /= k .or. &
        centroids%dim /= dim) then
      if (c_associated(centroids%data)) then
        call free_aligned(centroids%data, free_status)
      end if
      total_bytes = int(k, int64) * int(dim, int64) * int(elem_bytes, int64)
      call allocate_aligned(centroids%data, int(total_bytes, c_size_t), &
        int(DEFAULT_ALIGNMENT, c_size_t), alloc_status)
      if (alloc_status /= GLAMIN_OK) then
        status = alloc_status
        return
      end if
    end if

    centroids%length = k
    centroids%dim = dim
    centroids%stride = dim
    centroids%elem_size = elem_bytes
    centroids%alignment = DEFAULT_ALIGNMENT

    call c_f_pointer(vectors%data, vector_data, [int(stride_v, int64) * vector_count])
    call c_f_pointer(centroids%data, centroid_data, [int(dim, int64) * k])

    do centroid_index = 1_int32, k
      centroid_offset = int(centroid_index - 1_int32, int64) * dim
      if (centroid_index <= int(vector_count, int32)) then
        vec_offset = int(centroid_index - 1_int32, int64) * stride_v
        centroid_data(centroid_offset + 1_int64:centroid_offset + dim) = &
          vector_data(vec_offset + 1_int64:vec_offset + dim)
      else
        centroid_data(centroid_offset + 1_int64:centroid_offset + dim) = 0.0_real32
      end if
    end do

    if (vector_count <= 1_int64 .or. effective_k <= 1_int32) then
      status = GLAMIN_OK
      return
    end if

    allocate(sums(effective_k, dim), counts(effective_k))

    do iter = 1_int32, DEFAULT_ITERS
      sums = 0.0_real32
      counts = 0_int32

      do vec_index = 1_int64, vector_count
        vec_offset = (vec_index - 1_int64) * stride_v
        best_distance = huge(1.0_real32)
        if (.not. use_l2) best_distance = -huge(1.0_real32)
        best_index = 1_int32

        do dim_index = 1_int32, effective_k
          centroid_offset = int(dim_index - 1_int32, int64) * dim
          distance = 0.0_real32
          if (use_l2) then
            do axis_index = 1_int32, dim
              diff = vector_data(vec_offset + axis_index) - &
                centroid_data(centroid_offset + axis_index)
              distance = distance + diff * diff
            end do
            if (distance < best_distance) then
              best_distance = distance
              best_index = dim_index
            end if
          else
            do axis_index = 1_int32, dim
              distance = distance + vector_data(vec_offset + axis_index) * &
                centroid_data(centroid_offset + axis_index)
            end do
            if (distance > best_distance) then
              best_distance = distance
              best_index = dim_index
            end if
          end if
        end do

        counts(best_index) = counts(best_index) + 1_int32
        sums(best_index, 1:dim) = sums(best_index, 1:dim) + &
          vector_data(vec_offset + 1_int64:vec_offset + dim)
      end do

      do centroid_index = 1_int32, effective_k
        if (counts(centroid_index) > 0_int32) then
          centroid_offset = int(centroid_index - 1_int32, int64) * dim
          centroid_data(centroid_offset + 1_int64:centroid_offset + dim) = &
            sums(centroid_index, 1:dim) / real(counts(centroid_index), real32)
        end if
      end do
    end do

    deallocate(sums, counts)
    status = GLAMIN_OK
  end subroutine kmeans_train
end module glamin_kmeans
