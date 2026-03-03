module glamin_memory
  use iso_fortran_env, only: int32
  use iso_c_binding, only: c_associated, c_int, c_null_ptr, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  implicit none
  private

  public :: allocate_aligned
  public :: free_aligned

  integer(c_int), parameter :: POSIX_EINVAL = 22
  integer(c_int), parameter :: POSIX_ENOMEM = 12

  interface
    function posix_memalign(ptr, alignment, bytes) bind(c, name="posix_memalign") &
      result(result_code)
      import :: c_ptr, c_size_t, c_int
      type(c_ptr), intent(out) :: ptr
      integer(c_size_t), value :: alignment
      integer(c_size_t), value :: bytes
      integer(c_int) :: result_code
    end function posix_memalign

    subroutine c_free(ptr) bind(c, name="free")
      import :: c_ptr
      type(c_ptr), value :: ptr
    end subroutine c_free
  end interface

contains
  subroutine allocate_aligned(ptr, bytes, alignment, status)
    type(c_ptr), intent(out) :: ptr
    integer(c_size_t), intent(in) :: bytes
    integer(c_size_t), intent(in) :: alignment
    integer(int32), intent(out) :: status
    integer(c_int) :: result_code

    ptr = c_null_ptr
    if (bytes <= 0_c_size_t .or. alignment <= 0_c_size_t) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    result_code = posix_memalign(ptr, alignment, bytes)
    if (result_code /= 0_c_int) then
      ptr = c_null_ptr
      if (result_code == POSIX_EINVAL) then
        status = GLAMIN_ERR_INVALID_ARG
      else
        status = GLAMIN_ERR_OOM
      end if
      return
    end if

    status = GLAMIN_OK
  end subroutine allocate_aligned

  subroutine free_aligned(ptr, status)
    type(c_ptr), intent(inout) :: ptr
    integer(int32), intent(out) :: status

    if (c_associated(ptr)) then
      call c_free(ptr)
      ptr = c_null_ptr
    end if

    status = GLAMIN_OK
  end subroutine free_aligned
end module glamin_memory
