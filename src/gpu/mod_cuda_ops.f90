module glamin_cuda_ops
  use iso_fortran_env, only: int32
  use iso_c_binding, only: c_char, c_int32_t, c_loc, c_null_char, c_ptr
  use glamin_errors, only: GLAMIN_ERR_INVALID_ARG
  implicit none
  private

  public :: cuda_register_stub_ops
  public :: cuda_has_ops
  public :: cuda_load_ops
  public :: cuda_unload_ops

  interface
    function glamin_cuda_register_stub_ops() bind(c) result(status)
      import :: c_int32_t
      integer(c_int32_t) :: status
    end function glamin_cuda_register_stub_ops

    function glamin_cuda_has_ops() bind(c) result(flag)
      import :: c_int32_t
      integer(c_int32_t) :: flag
    end function glamin_cuda_has_ops

    function glamin_cuda_load_ops(path) bind(c) result(status)
      import :: c_int32_t, c_ptr
      type(c_ptr), value :: path
      integer(c_int32_t) :: status
    end function glamin_cuda_load_ops

    function glamin_cuda_unload_ops() bind(c) result(status)
      import :: c_int32_t
      integer(c_int32_t) :: status
    end function glamin_cuda_unload_ops
  end interface

contains
  subroutine cuda_register_stub_ops(status)
    integer(int32), intent(out) :: status

    status = int(glamin_cuda_register_stub_ops(), int32)
  end subroutine cuda_register_stub_ops

  logical function cuda_has_ops()
    cuda_has_ops = glamin_cuda_has_ops() /= 0_c_int32_t
  end function cuda_has_ops

  subroutine cuda_load_ops(path, status)
    character(len=*), intent(in) :: path
    integer(int32), intent(out) :: status
    character(kind=c_char), allocatable, target :: path_c(:)
    integer :: idx
    integer :: length

    length = len_trim(path)
    if (length <= 0) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    allocate(path_c(length + 1))
    do idx = 1, length
      path_c(idx) = path(idx:idx)
    end do
    path_c(length + 1) = c_null_char

    status = int(glamin_cuda_load_ops(c_loc(path_c(1))), int32)
  end subroutine cuda_load_ops

  subroutine cuda_unload_ops(status)
    integer(int32), intent(out) :: status

    status = int(glamin_cuda_unload_ops(), int32)
  end subroutine cuda_unload_ops
end module glamin_cuda_ops
