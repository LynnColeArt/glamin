module glamin_errors
  use iso_fortran_env, only: int32
  implicit none
  private

  public :: GLAMIN_OK
  public :: GLAMIN_ERR_UNKNOWN
  public :: GLAMIN_ERR_INVALID_ARG
  public :: GLAMIN_ERR_OOM
  public :: GLAMIN_ERR_NOT_READY
  public :: GLAMIN_ERR_CANCELLED

  integer(int32), parameter :: GLAMIN_OK = 0
  integer(int32), parameter :: GLAMIN_ERR_UNKNOWN = 1
  integer(int32), parameter :: GLAMIN_ERR_INVALID_ARG = 2
  integer(int32), parameter :: GLAMIN_ERR_OOM = 3
  integer(int32), parameter :: GLAMIN_ERR_NOT_READY = 4
  integer(int32), parameter :: GLAMIN_ERR_CANCELLED = 5
end module glamin_errors
