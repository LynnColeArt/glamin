module glamin_native_hash
  use iso_fortran_env, only: int32, int64
  implicit none
  private

  public :: hash64
  public :: hash256_hex

  integer(int64), parameter :: FNV_OFFSET = int(z'CBF29CE484222325', int64)
  integer(int64), parameter :: FNV_PRIME = int(z'00000100000001B3', int64)

contains
  pure function hash64(text, seed) result(value)
    character(len=*), intent(in) :: text
    integer(int64), intent(in), optional :: seed
    integer(int64) :: value
    integer(int64) :: mixed
    integer :: idx

    if (present(seed)) then
      value = ieor(FNV_OFFSET, seed)
    else
      value = FNV_OFFSET
    end if

    do idx = 1, len_trim(text)
      mixed = ieor(value, int(iachar(text(idx:idx)), int64))
      value = mixed * FNV_PRIME
    end do
  end function hash64

  pure function hash256_hex(text) result(value)
    character(len=*), intent(in) :: text
    character(len=72) :: value
    integer(int64) :: h1
    integer(int64) :: h2
    integer(int64) :: h3
    integer(int64) :: h4

    h1 = hash64(text, int(z'6A09E667F3BCC909', int64))
    h2 = hash64(text, int(z'BB67AE8584CAA73B', int64))
    h3 = hash64(text, int(z'3C6EF372FE94F82B', int64))
    h4 = hash64(text, int(z'A54FF53A5F1D36F1', int64))
    value = 'glamin:' // to_hex64(h1) // to_hex64(h2) // to_hex64(h3) // to_hex64(h4)
  end function hash256_hex

  pure function to_hex64(value) result(hex)
    integer(int64), intent(in) :: value
    character(len=16) :: hex
    character(len=*), parameter :: digits = '0123456789abcdef'
    integer(int64) :: work
    integer :: idx
    integer :: nibble

    work = value
    do idx = 16, 1, -1
      nibble = int(iand(work, int(z'F', int64)))
      hex(idx:idx) = digits(nibble + 1:nibble + 1)
      work = shiftr(work, 4)
    end do
  end function to_hex64
end module glamin_native_hash
