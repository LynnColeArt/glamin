module glamin_geometry_layout
  use iso_fortran_env, only: int32, int64
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG
  implicit none
  private

  public :: load_vector_layout

contains
  subroutine load_vector_layout(path, space_id, dim, count, offset_bytes, status)
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: space_id
    integer(int32), intent(out) :: dim
    integer(int64), intent(out) :: count
    integer(int64), intent(out) :: offset_bytes
    integer(int32), intent(out) :: status
    character(len=:), allocatable :: content
    character(len=:), allocatable :: pattern
    integer :: space_pos
    logical :: found
    integer(int64) :: temp_value

    dim = 0_int32
    count = 0_int64
    offset_bytes = 0_int64
    status = GLAMIN_OK

    call read_text_file(path, content, status)
    if (status /= GLAMIN_OK) then
      return
    end if

    pattern = '"space_id": "' // trim(space_id) // '"'
    space_pos = index(content, pattern)
    if (space_pos == 0) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call extract_int(content, '"dim"', space_pos, temp_value, found)
    if (.not. found) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if
    dim = int(temp_value, int32)

    call extract_int(content, '"count"', space_pos, temp_value, found)
    if (.not. found) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if
    count = temp_value

    call extract_int(content, '"offset_bytes"', space_pos, temp_value, found)
    if (.not. found) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if
    offset_bytes = temp_value
  end subroutine load_vector_layout

  subroutine read_text_file(path, content, status)
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: content
    integer(int32), intent(out) :: status
    integer :: unit
    integer :: io_status
    character(len=4096) :: line

    status = GLAMIN_OK
    content = ''

    open(newunit=unit, file=path, status='old', action='read', iostat=io_status)
    if (io_status /= 0) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    do
      read(unit, '(A)', iostat=io_status) line
      if (io_status /= 0) exit
      content = content // trim(line) // ' '
    end do

    close(unit)
  end subroutine read_text_file

  subroutine extract_int(content, key, start_pos, value, found)
    character(len=*), intent(in) :: content
    character(len=*), intent(in) :: key
    integer, intent(in) :: start_pos
    integer(int64), intent(out) :: value
    logical, intent(out) :: found
    integer :: key_pos
    integer :: colon_pos
    integer :: idx
    integer :: end_idx
    integer :: io_status

    value = 0_int64
    found = .false.

    key_pos = index(content(start_pos:), key)
    if (key_pos == 0) return

    idx = start_pos + key_pos - 1 + len_trim(key)
    colon_pos = index(content(idx:), ':')
    if (colon_pos == 0) return
    idx = idx + colon_pos

    call skip_whitespace(content, idx)
    if (idx > len(content)) return

    end_idx = idx
    do while (end_idx <= len(content) .and. is_digit(content(end_idx:end_idx)))
      end_idx = end_idx + 1
    end do

    if (end_idx == idx) return
    read(content(idx:end_idx - 1), *, iostat=io_status) value
    if (io_status /= 0) return
    found = .true.
  end subroutine extract_int

  subroutine skip_whitespace(content, idx)
    character(len=*), intent(in) :: content
    integer, intent(inout) :: idx

    do while (idx <= len(content))
      if (.not. is_space(content(idx:idx))) exit
      idx = idx + 1
    end do
  end subroutine skip_whitespace

  pure logical function is_space(ch)
    character(len=1), intent(in) :: ch
    integer :: code

    code = iachar(ch)
    is_space = code == 32 .or. code == 9 .or. code == 10 .or. code == 13
  end function is_space

  pure logical function is_digit(ch)
    character(len=1), intent(in) :: ch
    integer :: code

    code = iachar(ch)
    is_digit = code >= iachar('0') .and. code <= iachar('9')
  end function is_digit
end module glamin_geometry_layout
