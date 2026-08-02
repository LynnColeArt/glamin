module glamin_c_api
  use iso_fortran_env, only: int32, int64
  use iso_c_binding, only: c_associated, c_char, c_f_pointer, c_int32_t, c_int64_t, &
    c_null_char, c_ptr
  use glamin_errors, only: GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM, GLAMIN_OK
  use glamin_runtime, only: RuntimeContext, start_runtime, stop_runtime
  implicit none
  private

  public :: glamin_abi_version_c
  public :: glamin_runtime_create_c
  public :: glamin_runtime_destroy_c
  public :: glamin_last_error_c

  integer(int32), parameter :: MAX_RUNTIME_COUNT = 64_int32
  integer(int32), parameter :: ERROR_MESSAGE_LENGTH = 512_int32
  integer(c_int32_t), parameter :: GLAMIN_ABI_VERSION = 1_c_int32_t
  integer(c_int32_t), parameter :: GLAMIN_STATUS_BUFFER_TOO_SMALL = 6_c_int32_t

  type :: RuntimeSlot
    type(RuntimeContext) :: context
    integer(int64) :: handle = 0_int64
    character(len=ERROR_MESSAGE_LENGTH) :: last_error = ""
    logical :: is_used = .false.
  end type RuntimeSlot

  type(RuntimeSlot), save :: runtime_slots(MAX_RUNTIME_COUNT)
  integer(int64), save :: next_runtime_handle = 1_int64
  character(len=ERROR_MESSAGE_LENGTH), save :: global_last_error = ""

contains
  function glamin_abi_version_c() bind(c, name="glamin_abi_version") result(version)
    integer(c_int32_t) :: version

    version = GLAMIN_ABI_VERSION
  end function glamin_abi_version_c

  function glamin_runtime_create_c(worker_count, out_runtime) &
      bind(c, name="glamin_runtime_create") result(status)
    integer(c_int32_t), value :: worker_count
    type(c_ptr), value :: out_runtime
    integer(c_int32_t) :: status
    integer(c_int64_t), pointer :: runtime_output
    integer(int32) :: runtime_status
    integer(int32) :: slot_index

    if (.not. c_associated(out_runtime)) then
      call set_global_error("out_runtime must not be null")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    call c_f_pointer(out_runtime, runtime_output)
    runtime_output = 0_c_int64_t

    if (worker_count <= 0_c_int32_t) then
      call set_global_error("worker_count must be positive")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    slot_index = find_free_runtime_slot()
    if (slot_index == 0_int32) then
      call set_global_error("runtime registry is full")
      status = int(GLAMIN_ERR_OOM, c_int32_t)
      return
    end if

    if (next_runtime_handle == huge(next_runtime_handle)) then
      call set_global_error("runtime handle space is exhausted")
      status = int(GLAMIN_ERR_OOM, c_int32_t)
      return
    end if

    call start_runtime(runtime_slots(slot_index)%context, int(worker_count, int32), runtime_status)
    if (runtime_status /= GLAMIN_OK) then
      call set_global_error("failed to start the runtime worker pool")
      status = int(runtime_status, c_int32_t)
      return
    end if

    runtime_slots(slot_index)%handle = next_runtime_handle
    runtime_slots(slot_index)%last_error = ""
    runtime_slots(slot_index)%is_used = .true.
    runtime_output = int(next_runtime_handle, c_int64_t)
    next_runtime_handle = next_runtime_handle + 1_int64
    global_last_error = ""
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_runtime_create_c

  function glamin_runtime_destroy_c(runtime) &
      bind(c, name="glamin_runtime_destroy") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int32_t) :: status
    integer(int32) :: runtime_status
    integer(int32) :: slot_index

    slot_index = find_runtime_slot(int(runtime, int64))
    if (slot_index == 0_int32) then
      call set_global_error("runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    call stop_runtime(runtime_slots(slot_index)%context, runtime_status)
    if (runtime_status /= GLAMIN_OK) then
      runtime_slots(slot_index)%last_error = "failed to stop the runtime worker pool"
      status = int(runtime_status, c_int32_t)
      return
    end if

    runtime_slots(slot_index)%handle = 0_int64
    runtime_slots(slot_index)%last_error = ""
    runtime_slots(slot_index)%is_used = .false.
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_runtime_destroy_c

  function glamin_last_error_c(runtime, buffer, capacity, out_required) &
      bind(c, name="glamin_last_error") result(status)
    integer(c_int64_t), value :: runtime
    type(c_ptr), value :: buffer
    integer(c_int64_t), value :: capacity
    type(c_ptr), value :: out_required
    integer(c_int32_t) :: status
    character(kind=c_char), pointer :: buffer_characters(:)
    character(len=ERROR_MESSAGE_LENGTH) :: message
    integer(c_int64_t), pointer :: required_output
    integer(int32) :: character_index
    integer(int32) :: message_length
    integer(int32) :: slot_index

    if (.not. c_associated(out_required)) then
      call set_global_error("out_required must not be null")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    call c_f_pointer(out_required, required_output)

    if (runtime == 0_c_int64_t) then
      message = global_last_error
    else
      slot_index = find_runtime_slot(int(runtime, int64))
      if (slot_index == 0_int32) then
        call set_global_error("runtime handle is invalid or no longer active")
        required_output = 0_c_int64_t
        status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
        return
      end if
      message = runtime_slots(slot_index)%last_error
    end if

    message_length = len_trim(message)
    required_output = int(message_length + 1_int32, c_int64_t)

    if (.not. c_associated(buffer) .or. capacity < required_output) then
      status = GLAMIN_STATUS_BUFFER_TOO_SMALL
      return
    end if

    if (capacity > int(huge(0_int32), c_int64_t)) then
      call set_global_error("diagnostic buffer capacity exceeds the supported range")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    call c_f_pointer(buffer, buffer_characters, [int(capacity, int32)])
    do character_index = 1_int32, message_length
      buffer_characters(character_index) = &
        achar(iachar(message(character_index:character_index)), kind=c_char)
    end do
    buffer_characters(message_length + 1_int32) = c_null_char
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_last_error_c

  function find_free_runtime_slot() result(slot_index)
    integer(int32) :: slot_index
    integer(int32) :: candidate_index

    slot_index = 0_int32
    do candidate_index = 1_int32, MAX_RUNTIME_COUNT
      if (.not. runtime_slots(candidate_index)%is_used) then
        slot_index = candidate_index
        return
      end if
    end do
  end function find_free_runtime_slot

  function find_runtime_slot(runtime_handle) result(slot_index)
    integer(int64), intent(in) :: runtime_handle
    integer(int32) :: slot_index
    integer(int32) :: candidate_index

    slot_index = 0_int32
    if (runtime_handle == 0_int64) then
      return
    end if

    do candidate_index = 1_int32, MAX_RUNTIME_COUNT
      if (runtime_slots(candidate_index)%is_used .and. &
          runtime_slots(candidate_index)%handle == runtime_handle) then
        slot_index = candidate_index
        return
      end if
    end do
  end function find_runtime_slot

  subroutine set_global_error(message)
    character(len=*), intent(in) :: message

    global_last_error = message
  end subroutine set_global_error
end module glamin_c_api
