module glamin_stream
  use iso_fortran_env, only: int64, int32
  use iso_c_binding, only: c_associated, c_char, c_int, c_long, c_null_char, &
    c_null_ptr, c_ptr, c_size_t
  use glamin_errors, only: GLAMIN_OK
  use glamin_memory, only: allocate_aligned
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

  integer(c_int), parameter :: SEEK_SET = 0
  integer(c_size_t), parameter :: STREAM_ALIGN_BYTES = 64_c_size_t

  interface
    function c_fopen(path, mode) bind(c, name="fopen") result(handle)
      import :: c_char, c_ptr
      character(kind=c_char), dimension(*) :: path
      character(kind=c_char), dimension(*) :: mode
      type(c_ptr) :: handle
    end function c_fopen

    function c_fclose(stream) bind(c, name="fclose") result(status)
      import :: c_int, c_ptr
      type(c_ptr), value :: stream
      integer(c_int) :: status
    end function c_fclose

    function c_fread(buffer, size, count, stream) bind(c, name="fread") result(read_count)
      import :: c_ptr, c_size_t
      type(c_ptr), value :: buffer
      integer(c_size_t), value :: size
      integer(c_size_t), value :: count
      type(c_ptr), value :: stream
      integer(c_size_t) :: read_count
    end function c_fread

    function c_fwrite(buffer, size, count, stream) bind(c, name="fwrite") result(write_count)
      import :: c_ptr, c_size_t
      type(c_ptr), value :: buffer
      integer(c_size_t), value :: size
      integer(c_size_t), value :: count
      type(c_ptr), value :: stream
      integer(c_size_t) :: write_count
    end function c_fwrite

    function c_fseek(stream, offset, whence) bind(c, name="fseek") result(status)
      import :: c_int, c_long, c_ptr
      type(c_ptr), value :: stream
      integer(c_long), value :: offset
      integer(c_int), value :: whence
      integer(c_int) :: status
    end function c_fseek

    function c_ftell(stream) bind(c, name="ftell") result(position)
      import :: c_long, c_ptr
      type(c_ptr), value :: stream
      integer(c_long) :: position
    end function c_ftell
  end interface

contains
  subroutine open_stream(stream, path, mode)
    type(IoStream), intent(out) :: stream
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: mode
    character(kind=c_char, len=:), allocatable :: c_path
    character(kind=c_char, len=:), allocatable :: c_mode

    stream%handle = c_null_ptr
    stream%position = 0_int64

    c_path = to_c_string(path)
    c_mode = to_c_string(mode)
    stream%handle = c_fopen(c_path, c_mode)
    if (.not. c_associated(stream%handle)) then
      error stop "open_stream failed"
    end if

    stream%position = int(c_ftell(stream%handle), int64)
  end subroutine open_stream

  subroutine close_stream(stream)
    type(IoStream), intent(inout) :: stream
    integer(c_int) :: status

    if (c_associated(stream%handle)) then
      status = c_fclose(stream%handle)
      if (status /= 0_c_int) then
        error stop "close_stream failed"
      end if
    end if

    stream%handle = c_null_ptr
    stream%position = 0_int64
  end subroutine close_stream

  subroutine read_bytes(stream, buffer, nbytes)
    type(IoStream), intent(inout) :: stream
    type(c_ptr), intent(out) :: buffer
    integer(int64), intent(in) :: nbytes
    integer(int32) :: alloc_status
    integer(c_size_t) :: bytes_read
    integer(c_size_t) :: count

    buffer = c_null_ptr
    if (.not. c_associated(stream%handle)) then
      error stop "read_bytes requires open stream"
    end if

    if (nbytes <= 0_int64) then
      return
    end if

    call allocate_aligned(buffer, int(nbytes, c_size_t), STREAM_ALIGN_BYTES, alloc_status)
    if (alloc_status /= GLAMIN_OK) then
      error stop "read_bytes allocation failed"
    end if

    count = int(nbytes, c_size_t)
    bytes_read = c_fread(buffer, 1_c_size_t, count, stream%handle)
    if (bytes_read /= count) then
      error stop "read_bytes failed"
    end if

    stream%position = int(c_ftell(stream%handle), int64)
  end subroutine read_bytes

  subroutine write_bytes(stream, buffer, nbytes)
    type(IoStream), intent(inout) :: stream
    type(c_ptr), intent(in) :: buffer
    integer(int64), intent(in) :: nbytes
    integer(c_size_t) :: bytes_written
    integer(c_size_t) :: count

    if (.not. c_associated(stream%handle)) then
      error stop "write_bytes requires open stream"
    end if

    if (.not. c_associated(buffer)) then
      error stop "write_bytes requires buffer"
    end if

    if (nbytes <= 0_int64) then
      return
    end if

    count = int(nbytes, c_size_t)
    bytes_written = c_fwrite(buffer, 1_c_size_t, count, stream%handle)
    if (bytes_written /= count) then
      error stop "write_bytes failed"
    end if

    stream%position = int(c_ftell(stream%handle), int64)
  end subroutine write_bytes

  subroutine stream_seek(stream, offset)
    type(IoStream), intent(inout) :: stream
    integer(int64), intent(in) :: offset
    integer(c_int) :: status

    if (.not. c_associated(stream%handle)) then
      error stop "stream_seek requires open stream"
    end if

    if (offset < 0_int64) then
      error stop "stream_seek requires non-negative offset"
    end if

    status = c_fseek(stream%handle, int(offset, c_long), SEEK_SET)
    if (status /= 0_c_int) then
      error stop "stream_seek failed"
    end if

    stream%position = int(c_ftell(stream%handle), int64)
  end subroutine stream_seek

  pure function to_c_string(input) result(output)
    character(len=*), intent(in) :: input
    character(kind=c_char, len=:), allocatable :: output
    integer :: length

    length = len_trim(input)
    allocate(character(kind=c_char, len=length + 1) :: output)
    if (length > 0) then
      output(1:length) = input(1:length)
    end if
    output(length + 1:length + 1) = c_null_char
  end function to_c_string
end module glamin_stream
