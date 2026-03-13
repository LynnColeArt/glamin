module glamin_contracts
  use iso_fortran_env, only: int32, int64
  use iso_c_binding, only: c_associated, c_char, c_f_procpointer, c_funptr, c_int32_t, &
    c_loc, c_null_char, c_null_funptr, c_ptr
  use glamin_embedder, only: EmbedderContract, EmbedderSpec
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_NOT_READY
  implicit none
  private

  public :: load_embedder_contract
  public :: validate_embedder_contract
  public :: ContractValidators
  public :: set_contract_validators
  public :: clear_contract_validators
  public :: set_contract_validators_c
  public :: clear_contract_validators_c

  abstract interface
    subroutine embedder_hash_validator_iface(spec, contract_hash, status)
      import :: EmbedderSpec, int32
      type(EmbedderSpec), intent(in) :: spec
      character(len=*), intent(in) :: contract_hash
      integer(int32), intent(out) :: status
    end subroutine embedder_hash_validator_iface
  end interface

  abstract interface
    subroutine embedder_signature_validator_iface(spec, contract_hash, signature, status)
      import :: EmbedderSpec, int32
      type(EmbedderSpec), intent(in) :: spec
      character(len=*), intent(in) :: contract_hash
      character(len=*), intent(in) :: signature
      integer(int32), intent(out) :: status
    end subroutine embedder_signature_validator_iface
  end interface

  abstract interface
    subroutine embedder_hash_validator_c(embedder_id, embedder_version, input_schema, &
        preprocess_chain, model_hash, config_hash, hardware_class, min_ram_mb, min_vram_mb, &
        contract_hash, status) bind(c)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: embedder_id
      type(c_ptr), value :: embedder_version
      type(c_ptr), value :: input_schema
      type(c_ptr), value :: preprocess_chain
      type(c_ptr), value :: model_hash
      type(c_ptr), value :: config_hash
      type(c_ptr), value :: hardware_class
      integer(c_int32_t), value :: min_ram_mb
      integer(c_int32_t), value :: min_vram_mb
      type(c_ptr), value :: contract_hash
      integer(c_int32_t) :: status
    end subroutine embedder_hash_validator_c
  end interface

  abstract interface
    subroutine embedder_signature_validator_c(embedder_id, embedder_version, input_schema, &
        preprocess_chain, model_hash, config_hash, hardware_class, min_ram_mb, min_vram_mb, &
        contract_hash, signature, status) bind(c)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: embedder_id
      type(c_ptr), value :: embedder_version
      type(c_ptr), value :: input_schema
      type(c_ptr), value :: preprocess_chain
      type(c_ptr), value :: model_hash
      type(c_ptr), value :: config_hash
      type(c_ptr), value :: hardware_class
      integer(c_int32_t), value :: min_ram_mb
      integer(c_int32_t), value :: min_vram_mb
      type(c_ptr), value :: contract_hash
      type(c_ptr), value :: signature
      integer(c_int32_t) :: status
    end subroutine embedder_signature_validator_c
  end interface

  type :: ContractValidators
    procedure(embedder_hash_validator_iface), pointer, nopass :: hash => null()
    procedure(embedder_signature_validator_iface), pointer, nopass :: signature => null()
  end type ContractValidators

  type(ContractValidators), save :: validators = ContractValidators()
  type(c_funptr), save :: hash_validator_c = c_null_funptr
  type(c_funptr), save :: signature_validator_c = c_null_funptr

contains
  subroutine set_contract_validators(new_validators)
    type(ContractValidators), intent(in) :: new_validators

    validators = new_validators
  end subroutine set_contract_validators

  subroutine clear_contract_validators()
    type(ContractValidators) :: empty_validators

    nullify(empty_validators%hash)
    nullify(empty_validators%signature)
    validators = empty_validators
  end subroutine clear_contract_validators

  subroutine set_contract_validators_c(hash_cb, signature_cb) bind(c, name="glamin_set_contract_validators")
    type(c_funptr), value :: hash_cb
    type(c_funptr), value :: signature_cb
    type(ContractValidators) :: new_validators

    hash_validator_c = hash_cb
    signature_validator_c = signature_cb

    nullify(new_validators%hash)
    nullify(new_validators%signature)

    if (c_associated(hash_cb)) new_validators%hash => hash_validator_bridge
    if (c_associated(signature_cb)) new_validators%signature => signature_validator_bridge

    call set_contract_validators(new_validators)
  end subroutine set_contract_validators_c

  subroutine clear_contract_validators_c() bind(c, name="glamin_clear_contract_validators")
    hash_validator_c = c_null_funptr
    signature_validator_c = c_null_funptr
    call clear_contract_validators()
  end subroutine clear_contract_validators_c

  subroutine load_embedder_contract(path, contract, status)
    character(len=*), intent(in) :: path
    type(EmbedderContract), intent(out) :: contract
    integer(int32), intent(out) :: status
    character(len=:), allocatable :: content
    integer :: embed_start
    integer :: embed_end
    integer :: spec_start
    integer :: spec_end
    logical :: found
    integer(int64) :: temp_value

    contract = EmbedderContract()
    status = GLAMIN_OK

    call read_text_file(path, content, status)
    if (status /= GLAMIN_OK) then
      return
    end if

    call find_object_bounds(content, '"embedder"', embed_start, embed_end, found)
    if (.not. found) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call find_object_bounds(content(embed_start:embed_end), '"spec"', spec_start, spec_end, found)
    if (.not. found) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if
    spec_start = embed_start + spec_start - 1
    spec_end = embed_start + spec_end - 1

    call extract_string(content(spec_start:spec_end), '"id"', 1, contract%spec%embedder_id, found)
    call extract_string(content(spec_start:spec_end), '"version"', 1, contract%spec%embedder_version, found)
    call extract_string(content(spec_start:spec_end), '"input_schema"', 1, contract%spec%input_schema, found)
    call extract_string_array(content(spec_start:spec_end), '"preprocess_chain"', 1, &
      contract%spec%preprocess_chain, found)
    if (.not. found) then
      call extract_string(content(spec_start:spec_end), '"preprocess_chain"', 1, &
        contract%spec%preprocess_chain, found)
    end if
    call extract_string(content(spec_start:spec_end), '"model_hash"', 1, contract%spec%model_hash, found)
    call extract_string(content(spec_start:spec_end), '"config_hash"', 1, contract%spec%config_hash, found)
    call extract_string(content(spec_start:spec_end), '"hardware_class"', 1, &
      contract%spec%hardware_class, found)

    call extract_int(content(spec_start:spec_end), '"min_ram_mb"', 1, temp_value, found)
    if (found) contract%spec%min_ram_mb = int(temp_value, int32)
    call extract_int(content(spec_start:spec_end), '"min_vram_mb"', 1, temp_value, found)
    if (found) contract%spec%min_vram_mb = int(temp_value, int32)

    call extract_string(content(embed_start:embed_end), '"contract_hash"', 1, &
      contract%contract_hash, found)
    call extract_string(content(embed_start:embed_end), '"signature"', 1, contract%signature, found)
  end subroutine load_embedder_contract

  subroutine validate_embedder_contract(contract, status)
    type(EmbedderContract), intent(in) :: contract
    integer(int32), intent(out) :: status

    status = GLAMIN_OK
    if (len_trim(contract%spec%embedder_id) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(contract%spec%embedder_version) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(contract%spec%input_schema) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(contract%spec%preprocess_chain) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(contract%spec%model_hash) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(contract%spec%config_hash) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(contract%spec%hardware_class) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(contract%contract_hash) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (contract%spec%min_ram_mb < 0_int32) status = GLAMIN_ERR_INVALID_ARG
    if (contract%spec%min_vram_mb < 0_int32) status = GLAMIN_ERR_INVALID_ARG
    if (status /= GLAMIN_OK) return

    call validate_hash_hook(contract, status)
    if (status /= GLAMIN_OK) return

    call validate_signature_hook(contract, status)
  end subroutine validate_embedder_contract

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

  subroutine find_object_bounds(content, key, obj_start, obj_end, found)
    character(len=*), intent(in) :: content
    character(len=*), intent(in) :: key
    integer, intent(out) :: obj_start
    integer, intent(out) :: obj_end
    logical, intent(out) :: found
    integer :: key_pos
    integer :: brace_pos
    integer :: idx
    integer :: depth

    obj_start = 0
    obj_end = 0
    found = .false.

    key_pos = index(content, key)
    if (key_pos == 0) return

    brace_pos = index(content(key_pos:), '{')
    if (brace_pos == 0) return
    obj_start = key_pos + brace_pos - 1
    depth = 0

    do idx = obj_start, len(content)
      select case (content(idx:idx))
      case ('{')
        depth = depth + 1
      case ('}')
        if (depth > 0) depth = depth - 1
        if (depth == 0) then
          obj_end = idx
          found = .true.
          return
        end if
      case default
      end select
    end do
  end subroutine find_object_bounds

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

  subroutine extract_string(content, key, start_pos, value, found)
    character(len=*), intent(in) :: content
    character(len=*), intent(in) :: key
    integer, intent(in) :: start_pos
    character(len=*), intent(inout) :: value
    logical, intent(out) :: found
    integer :: key_pos
    integer :: colon_pos
    integer :: idx
    integer :: end_idx

    value = ''
    found = .false.

    key_pos = index(content(start_pos:), key)
    if (key_pos == 0) return

    idx = start_pos + key_pos - 1 + len_trim(key)
    colon_pos = index(content(idx:), ':')
    if (colon_pos == 0) return
    idx = idx + colon_pos

    call skip_whitespace(content, idx)
    if (idx > len(content)) return
    if (content(idx:idx) /= '"') return
    idx = idx + 1

    end_idx = idx
    do while (end_idx <= len(content) .and. content(end_idx:end_idx) /= '"')
      end_idx = end_idx + 1
    end do
    if (end_idx > len(content)) return
    value = content(idx:end_idx - 1)
    found = .true.
  end subroutine extract_string

  subroutine extract_string_array(content, key, start_pos, value, found)
    character(len=*), intent(in) :: content
    character(len=*), intent(in) :: key
    integer, intent(in) :: start_pos
    character(len=*), intent(inout) :: value
    logical, intent(out) :: found
    integer :: key_pos
    integer :: array_pos
    integer :: idx
    integer :: end_idx
    integer :: value_len
    character(len=len(value)) :: buffer
    logical :: has_item

    value = ''
    buffer = ''
    found = .false.
    has_item = .false.

    key_pos = index(content(start_pos:), key)
    if (key_pos == 0) return

    idx = start_pos + key_pos - 1 + len_trim(key)
    array_pos = index(content(idx:), '[')
    if (array_pos == 0) return
    idx = idx + array_pos

    do while (idx <= len(content))
      if (content(idx:idx) == ']') exit
      if (content(idx:idx) == '"') then
        idx = idx + 1
        end_idx = idx
        do while (end_idx <= len(content) .and. content(end_idx:end_idx) /= '"')
          end_idx = end_idx + 1
        end do
        if (end_idx > len(content)) exit
        value_len = len_trim(buffer)
        if (value_len > 0) then
          buffer = trim(buffer) // ',' // content(idx:end_idx - 1)
        else
          buffer = content(idx:end_idx - 1)
        end if
        has_item = .true.
        idx = end_idx + 1
      else
        idx = idx + 1
      end if
    end do

    if (has_item) then
      value = trim(buffer)
      found = .true.
    end if
  end subroutine extract_string_array

  subroutine validate_hash_hook(contract, status)
    type(EmbedderContract), intent(in) :: contract
    integer(int32), intent(out) :: status

    if (associated(validators%hash)) then
      call validators%hash(contract%spec, contract%contract_hash, status)
    else
      status = GLAMIN_OK
    end if
  end subroutine validate_hash_hook

  subroutine validate_signature_hook(contract, status)
    type(EmbedderContract), intent(in) :: contract
    integer(int32), intent(out) :: status

    if (.not. associated(validators%signature)) then
      status = GLAMIN_OK
      return
    end if

    if (len_trim(contract%signature) == 0) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call validators%signature(contract%spec, contract%contract_hash, contract%signature, status)
  end subroutine validate_signature_hook

  subroutine hash_validator_bridge(spec, contract_hash, status)
    type(EmbedderSpec), intent(in) :: spec
    character(len=*), intent(in) :: contract_hash
    integer(int32), intent(out) :: status

    call dispatch_hash_callback(hash_validator_c, spec, contract_hash, status)
  end subroutine hash_validator_bridge

  subroutine signature_validator_bridge(spec, contract_hash, signature, status)
    type(EmbedderSpec), intent(in) :: spec
    character(len=*), intent(in) :: contract_hash
    character(len=*), intent(in) :: signature
    integer(int32), intent(out) :: status

    call dispatch_signature_callback(signature_validator_c, spec, contract_hash, signature, status)
  end subroutine signature_validator_bridge

  subroutine dispatch_hash_callback(callback, spec, contract_hash, status)
    type(c_funptr), intent(in) :: callback
    type(EmbedderSpec), intent(in) :: spec
    character(len=*), intent(in) :: contract_hash
    integer(int32), intent(out) :: status
    procedure(embedder_hash_validator_c), pointer :: cb
    character(kind=c_char), allocatable, target :: id_c(:)
    character(kind=c_char), allocatable, target :: version_c(:)
    character(kind=c_char), allocatable, target :: schema_c(:)
    character(kind=c_char), allocatable, target :: chain_c(:)
    character(kind=c_char), allocatable, target :: model_c(:)
    character(kind=c_char), allocatable, target :: config_c(:)
    character(kind=c_char), allocatable, target :: hardware_c(:)
    character(kind=c_char), allocatable, target :: hash_c(:)
    integer(c_int32_t) :: c_status

    if (.not. c_associated(callback)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call c_f_procpointer(callback, cb)
    if (.not. associated(cb)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call to_c_string(spec%embedder_id, id_c)
    call to_c_string(spec%embedder_version, version_c)
    call to_c_string(spec%input_schema, schema_c)
    call to_c_string(spec%preprocess_chain, chain_c)
    call to_c_string(spec%model_hash, model_c)
    call to_c_string(spec%config_hash, config_c)
    call to_c_string(spec%hardware_class, hardware_c)
    call to_c_string(contract_hash, hash_c)

    c_status = 0_c_int32_t
    call cb(c_loc(id_c(1)), c_loc(version_c(1)), c_loc(schema_c(1)), c_loc(chain_c(1)), &
      c_loc(model_c(1)), c_loc(config_c(1)), c_loc(hardware_c(1)), &
      int(spec%min_ram_mb, c_int32_t), int(spec%min_vram_mb, c_int32_t), c_loc(hash_c(1)), &
      c_status)
    status = int(c_status, int32)
  end subroutine dispatch_hash_callback

  subroutine dispatch_signature_callback(callback, spec, contract_hash, signature, status)
    type(c_funptr), intent(in) :: callback
    type(EmbedderSpec), intent(in) :: spec
    character(len=*), intent(in) :: contract_hash
    character(len=*), intent(in) :: signature
    integer(int32), intent(out) :: status
    procedure(embedder_signature_validator_c), pointer :: cb
    character(kind=c_char), allocatable, target :: id_c(:)
    character(kind=c_char), allocatable, target :: version_c(:)
    character(kind=c_char), allocatable, target :: schema_c(:)
    character(kind=c_char), allocatable, target :: chain_c(:)
    character(kind=c_char), allocatable, target :: model_c(:)
    character(kind=c_char), allocatable, target :: config_c(:)
    character(kind=c_char), allocatable, target :: hardware_c(:)
    character(kind=c_char), allocatable, target :: hash_c(:)
    character(kind=c_char), allocatable, target :: signature_c(:)
    integer(c_int32_t) :: c_status

    if (.not. c_associated(callback)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call c_f_procpointer(callback, cb)
    if (.not. associated(cb)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call to_c_string(spec%embedder_id, id_c)
    call to_c_string(spec%embedder_version, version_c)
    call to_c_string(spec%input_schema, schema_c)
    call to_c_string(spec%preprocess_chain, chain_c)
    call to_c_string(spec%model_hash, model_c)
    call to_c_string(spec%config_hash, config_c)
    call to_c_string(spec%hardware_class, hardware_c)
    call to_c_string(contract_hash, hash_c)
    call to_c_string(signature, signature_c)

    c_status = 0_c_int32_t
    call cb(c_loc(id_c(1)), c_loc(version_c(1)), c_loc(schema_c(1)), c_loc(chain_c(1)), &
      c_loc(model_c(1)), c_loc(config_c(1)), c_loc(hardware_c(1)), &
      int(spec%min_ram_mb, c_int32_t), int(spec%min_vram_mb, c_int32_t), c_loc(hash_c(1)), &
      c_loc(signature_c(1)), c_status)
    status = int(c_status, int32)
  end subroutine dispatch_signature_callback

  subroutine to_c_string(input, output)
    character(len=*), intent(in) :: input
    character(kind=c_char), allocatable, intent(out) :: output(:)
    integer :: length
    integer :: idx

    length = len_trim(input)
    allocate(output(length + 1))
    do idx = 1, length
      output(idx) = achar(iachar(input(idx:idx)), kind=c_char)
    end do
    output(length + 1) = c_null_char
  end subroutine to_c_string

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
end module glamin_contracts
