module glamin_stream
  use iso_fortran_env, only: int64
  use iso_c_binding, only: c_ptr, c_null_ptr
  implicit none
  private

  public :: IoStream
  public :: open_stream
  public :: close_stream
  public :: read_bytes
  public :: write_bytes
  public :: stream_seek

  type :: IoStream
    type(c_ptr) :: handle = c_null_ptr
    integer(int64) :: position = 0
  end type IoStream

contains
  subroutine open_stream(stream, path, mode)
    type(IoStream), intent(out) :: stream
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: mode
    error stop "open_stream not implemented"
  end subroutine open_stream

  subroutine close_stream(stream)
    type(IoStream), intent(inout) :: stream
    error stop "close_stream not implemented"
  end subroutine close_stream

  subroutine read_bytes(stream, buffer, nbytes)
    type(IoStream), intent(inout) :: stream
    type(c_ptr), intent(out) :: buffer
    integer(int64), intent(in) :: nbytes
    error stop "read_bytes not implemented"
  end subroutine read_bytes

  subroutine write_bytes(stream, buffer, nbytes)
    type(IoStream), intent(inout) :: stream
    type(c_ptr), intent(in) :: buffer
    integer(int64), intent(in) :: nbytes
    error stop "write_bytes not implemented"
  end subroutine write_bytes

  subroutine stream_seek(stream, offset)
    type(IoStream), intent(inout) :: stream
    integer(int64), intent(in) :: offset
    error stop "stream_seek not implemented"
  end subroutine stream_seek
end module glamin_stream
