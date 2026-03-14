module glamin_cuda_ops
  use iso_fortran_env, only: int32
  use iso_c_binding, only: c_int32_t
  implicit none
  private

  public :: cuda_register_stub_ops
  public :: cuda_has_ops

  interface
    function glamin_cuda_register_stub_ops() bind(c) result(status)
      import :: c_int32_t
      integer(c_int32_t) :: status
    end function glamin_cuda_register_stub_ops

    function glamin_cuda_has_ops() bind(c) result(flag)
      import :: c_int32_t
      integer(c_int32_t) :: flag
    end function glamin_cuda_has_ops
  end interface

contains
  subroutine cuda_register_stub_ops(status)
    integer(int32), intent(out) :: status

    status = int(glamin_cuda_register_stub_ops(), int32)
  end subroutine cuda_register_stub_ops

  logical function cuda_has_ops()
    cuda_has_ops = glamin_cuda_has_ops() /= 0_c_int32_t
  end function cuda_has_ops
end module glamin_cuda_ops
