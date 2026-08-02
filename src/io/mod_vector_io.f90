module glamin_vector_io
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_null_ptr, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG
  use glamin_memory, only: allocate_aligned, free_aligned
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: load_vector_block
  public :: load_vector_block_slice
  public :: free_vector_block

  integer(int32), parameter :: VECTOR_ALIGNMENT = 64

contains
  subroutine load_vector_block(path, dim, count, block, status)
    character(len=*), intent(in) :: path
    integer(int32), intent(in) :: dim
    integer(int64), intent(in) :: count
    type(VectorBlock), intent(inout) :: block
    integer(int32), intent(out) :: status
    call load_vector_block_slice(path, dim, count, 0_int64, block, status)
  end subroutine load_vector_block

  subroutine load_vector_block_slice(path, dim, count, offset_bytes, block, status)
    character(len=*), intent(in) :: path
    integer(int32), intent(in) :: dim
    integer(int64), intent(in) :: count
    integer(int64), intent(in) :: offset_bytes
    type(VectorBlock), intent(inout) :: block
    integer(int32), intent(out) :: status
    type(c_ptr) :: buffer
    real(real32), pointer :: values(:)
    integer :: io_status
    integer :: unit
    integer(int64) :: element_count
    integer(int32) :: free_status
    integer(int64) :: total_bytes
    integer(int32) :: elem_size

    status = GLAMIN_OK
    buffer = c_null_ptr
    if (dim <= 0_int32 .or. count < 0_int64 .or. offset_bytes < 0_int64) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call free_vector_block(block, free_status)
    if (free_status /= GLAMIN_OK) then
      status = free_status
      return
    end if

    if (count == 0_int64) then
      block = VectorBlock()
      return
    end if

    elem_size = int(storage_size(0.0_real32) / 8, int32)
    if (count > int(huge(0_int32), int64) / int(dim, int64)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if
    element_count = int(dim, int64) * count
    if (element_count > huge(0_int64) / int(elem_size, int64)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if
    total_bytes = element_count * int(elem_size, int64)

    call allocate_aligned(buffer, int(total_bytes, c_size_t), &
      int(VECTOR_ALIGNMENT, c_size_t), status)
    if (status /= GLAMIN_OK) return
    call c_f_pointer(buffer, values, [int(element_count, int32)])

    open(newunit=unit, file=path, status='old', action='read', access='stream', &
      form='unformatted', iostat=io_status)
    if (io_status == 0) then
      read(unit, pos=offset_bytes + 1_int64, iostat=io_status) values
      close(unit, iostat=free_status)
      if (io_status == 0 .and. free_status /= 0) io_status = free_status
    end if
    if (io_status /= 0) then
      call free_aligned(buffer, free_status)
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    block%data = buffer
    block%length = count
    block%dim = dim
    block%stride = dim
    block%elem_size = elem_size
    block%alignment = VECTOR_ALIGNMENT
  end subroutine load_vector_block_slice

  subroutine free_vector_block(block, status)
    type(VectorBlock), intent(inout) :: block
    integer(int32), intent(out) :: status
    integer(int32) :: free_status

    status = GLAMIN_OK
    if (c_associated(block%data)) then
      call free_aligned(block%data, free_status)
      status = free_status
    end if

    block%data = c_null_ptr
    block%length = 0_int64
    block%dim = 0_int32
    block%stride = 0_int32
    block%elem_size = 0_int32
    block%alignment = 0_int32
  end subroutine free_vector_block
end module glamin_vector_io
