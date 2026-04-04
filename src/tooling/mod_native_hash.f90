module glamin_native_hash
  use iso_fortran_env, only: int64
  implicit none
  private

  public :: hash64
  public :: hash256_hex

  integer(int64), parameter :: FNV_OFFSET = int(z'CBF29CE484222325', int64)
  integer(int64), parameter :: FNV_PRIME = int(z'00000100000001B3', int64)
  integer(int64), parameter :: WORD_MASK = int(z'00000000FFFFFFFF', int64)
  integer(int64), parameter :: SHA256_K(64) = [ &
    int(z'428A2F98', int64), int(z'71374491', int64), int(z'B5C0FBCF', int64), int(z'E9B5DBA5', int64), &
    int(z'3956C25B', int64), int(z'59F111F1', int64), int(z'923F82A4', int64), int(z'AB1C5ED5', int64), &
    int(z'D807AA98', int64), int(z'12835B01', int64), int(z'243185BE', int64), int(z'550C7DC3', int64), &
    int(z'72BE5D74', int64), int(z'80DEB1FE', int64), int(z'9BDC06A7', int64), int(z'C19BF174', int64), &
    int(z'E49B69C1', int64), int(z'EFBE4786', int64), int(z'0FC19DC6', int64), int(z'240CA1CC', int64), &
    int(z'2DE92C6F', int64), int(z'4A7484AA', int64), int(z'5CB0A9DC', int64), int(z'76F988DA', int64), &
    int(z'983E5152', int64), int(z'A831C66D', int64), int(z'B00327C8', int64), int(z'BF597FC7', int64), &
    int(z'C6E00BF3', int64), int(z'D5A79147', int64), int(z'06CA6351', int64), int(z'14292967', int64), &
    int(z'27B70A85', int64), int(z'2E1B2138', int64), int(z'4D2C6DFC', int64), int(z'53380D13', int64), &
    int(z'650A7354', int64), int(z'766A0ABB', int64), int(z'81C2C92E', int64), int(z'92722C85', int64), &
    int(z'A2BFE8A1', int64), int(z'A81A664B', int64), int(z'C24B8B70', int64), int(z'C76C51A3', int64), &
    int(z'D192E819', int64), int(z'D6990624', int64), int(z'F40E3585', int64), int(z'106AA070', int64), &
    int(z'19A4C116', int64), int(z'1E376C08', int64), int(z'2748774C', int64), int(z'34B0BCB5', int64), &
    int(z'391C0CB3', int64), int(z'4ED8AA4A', int64), int(z'5B9CCA4F', int64), int(z'682E6FF3', int64), &
    int(z'748F82EE', int64), int(z'78A5636F', int64), int(z'84C87814', int64), int(z'8CC70208', int64), &
    int(z'90BEFFFA', int64), int(z'A4506CEB', int64), int(z'BEF9A3F7', int64), int(z'C67178F2', int64) ]

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

  function hash256_hex(text) result(value)
    character(len=*), intent(in) :: text
    character(len=71) :: value
    integer(int64) :: state(8)
    integer(int64), allocatable :: bytes(:)
    integer(int64) :: block_words(64)
    integer(int64) :: bit_length
    integer :: total_length
    integer :: block_index
    integer :: word_index
    integer :: byte_index

    state = [ &
      int(z'6A09E667', int64), int(z'BB67AE85', int64), int(z'3C6EF372', int64), int(z'A54FF53A', int64), &
      int(z'510E527F', int64), int(z'9B05688C', int64), int(z'1F83D9AB', int64), int(z'5BE0CD19', int64) ]

    total_length = padded_length(len(text))
    allocate(bytes(total_length))
    bytes = 0_int64

    do byte_index = 1, len(text)
      bytes(byte_index) = int(iachar(text(byte_index:byte_index)), int64)
    end do
    bytes(len(text) + 1) = int(z'80', int64)

    bit_length = int(len(text), int64) * 8_int64
    do byte_index = 0, 7
      bytes(total_length - 7 + byte_index) = iand(shiftr(bit_length, 8 * (7 - byte_index)), int(z'FF', int64))
    end do

    do block_index = 1, total_length, 64
      do word_index = 0, 15
        byte_index = block_index + word_index * 4
        block_words(word_index + 1) = mask32( &
          shiftl(bytes(byte_index), 24) + &
          shiftl(bytes(byte_index + 1), 16) + &
          shiftl(bytes(byte_index + 2), 8) + &
          bytes(byte_index + 3))
      end do

      do word_index = 17, 64
        block_words(word_index) = mask32( &
          small_sigma1(block_words(word_index - 2)) + &
          block_words(word_index - 7) + &
          small_sigma0(block_words(word_index - 15)) + &
          block_words(word_index - 16))
      end do

      call compress_block(block_words, state)
    end do

    value = 'sha256:' // to_hex32(state(1)) // to_hex32(state(2)) // to_hex32(state(3)) // to_hex32(state(4)) // &
      to_hex32(state(5)) // to_hex32(state(6)) // to_hex32(state(7)) // to_hex32(state(8))
  end function hash256_hex

  pure integer function padded_length(input_length) result(total_length)
    integer, intent(in) :: input_length
    integer :: payload_length

    payload_length = input_length + 1 + 8
    total_length = ((payload_length + 63) / 64) * 64
  end function padded_length

  subroutine compress_block(words, state)
    integer(int64), intent(in) :: words(64)
    integer(int64), intent(inout) :: state(8)
    integer(int64) :: a
    integer(int64) :: b
    integer(int64) :: c
    integer(int64) :: d
    integer(int64) :: e
    integer(int64) :: f
    integer(int64) :: g
    integer(int64) :: h
    integer(int64) :: t1
    integer(int64) :: t2
    integer :: idx

    a = state(1)
    b = state(2)
    c = state(3)
    d = state(4)
    e = state(5)
    f = state(6)
    g = state(7)
    h = state(8)

    do idx = 1, 64
      t1 = mask32(h + big_sigma1(e) + choose_word(e, f, g) + SHA256_K(idx) + words(idx))
      t2 = mask32(big_sigma0(a) + majority(a, b, c))
      h = g
      g = f
      f = e
      e = mask32(d + t1)
      d = c
      c = b
      b = a
      a = mask32(t1 + t2)
    end do

    state(1) = mask32(state(1) + a)
    state(2) = mask32(state(2) + b)
    state(3) = mask32(state(3) + c)
    state(4) = mask32(state(4) + d)
    state(5) = mask32(state(5) + e)
    state(6) = mask32(state(6) + f)
    state(7) = mask32(state(7) + g)
    state(8) = mask32(state(8) + h)
  end subroutine compress_block

  pure integer(int64) function mask32(value) result(masked)
    integer(int64), intent(in) :: value

    masked = iand(value, WORD_MASK)
  end function mask32

  pure integer(int64) function rotate_right(value, shift) result(rotated)
    integer(int64), intent(in) :: value
    integer, intent(in) :: shift
    integer(int64) :: word

    word = mask32(value)
    rotated = mask32(ior(shiftr(word, shift), shiftl(word, 32 - shift)))
  end function rotate_right

  pure integer(int64) function choose_word(x, y, z) result(value)
    integer(int64), intent(in) :: x
    integer(int64), intent(in) :: y
    integer(int64), intent(in) :: z
    integer(int64) :: nx

    nx = iand(not(mask32(x)), WORD_MASK)
    value = mask32(ieor(iand(mask32(x), mask32(y)), iand(nx, mask32(z))))
  end function choose_word

  pure integer(int64) function majority(x, y, z) result(value)
    integer(int64), intent(in) :: x
    integer(int64), intent(in) :: y
    integer(int64), intent(in) :: z

    value = mask32(ieor(ieor(iand(mask32(x), mask32(y)), iand(mask32(x), mask32(z))), &
      iand(mask32(y), mask32(z))))
  end function majority

  pure integer(int64) function big_sigma0(x) result(value)
    integer(int64), intent(in) :: x

    value = mask32(ieor(ieor(rotate_right(x, 2), rotate_right(x, 13)), rotate_right(x, 22)))
  end function big_sigma0

  pure integer(int64) function big_sigma1(x) result(value)
    integer(int64), intent(in) :: x

    value = mask32(ieor(ieor(rotate_right(x, 6), rotate_right(x, 11)), rotate_right(x, 25)))
  end function big_sigma1

  pure integer(int64) function small_sigma0(x) result(value)
    integer(int64), intent(in) :: x

    value = mask32(ieor(ieor(rotate_right(x, 7), rotate_right(x, 18)), shiftr(mask32(x), 3)))
  end function small_sigma0

  pure integer(int64) function small_sigma1(x) result(value)
    integer(int64), intent(in) :: x

    value = mask32(ieor(ieor(rotate_right(x, 17), rotate_right(x, 19)), shiftr(mask32(x), 10)))
  end function small_sigma1

  pure function to_hex32(value) result(hex)
    integer(int64), intent(in) :: value
    character(len=8) :: hex
    character(len=*), parameter :: digits = '0123456789abcdef'
    integer(int64) :: work
    integer :: idx
    integer :: nibble

    work = mask32(value)
    do idx = 8, 1, -1
      nibble = int(iand(work, int(z'F', int64)))
      hex(idx:idx) = digits(nibble + 1:nibble + 1)
      work = shiftr(work, 4)
    end do
  end function to_hex32
end module glamin_native_hash
