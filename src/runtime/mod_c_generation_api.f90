module glamin_c_generation_api
  use iso_fortran_env, only: int32, int64
  use iso_c_binding, only: c_associated, c_char, c_f_pointer, c_int32_t, &
    c_int64_t, c_null_char, c_ptr
  use glamin_c_api, only: c_api_bind_index_generation, &
    c_api_clear_runtime_error, c_api_runtime_is_active, c_api_set_runtime_error, &
    c_api_unbind_index_generation, glamin_index_search_f32_c
  use glamin_errors, only: GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_NOT_READY, &
    GLAMIN_ERR_OOM, GLAMIN_ERR_UNKNOWN, GLAMIN_OK
  implicit none
  private

  public :: glamin_generation_create_c
  public :: glamin_generation_activate_c
  public :: glamin_generation_deactivate_c
  public :: glamin_generation_pin_active_c
  public :: glamin_generation_unpin_c
  public :: glamin_generation_retire_c
  public :: glamin_generation_label_c
  public :: glamin_generation_search_f32_c

  integer(int32), parameter :: MAX_GENERATION_COUNT = 256_int32
  integer(int32), parameter :: MAX_PIN_COUNT = 1024_int32
  integer(int32), parameter :: MAX_LABEL_LENGTH = 128_int32
  integer(c_int32_t), parameter :: GLAMIN_STATUS_BUFFER_TOO_SMALL = 6_c_int32_t

  type :: GenerationSlot
    integer(int64) :: handle = 0_int64
    integer(int64) :: owner_runtime = 0_int64
    integer(int64) :: index_handle = 0_int64
    integer(int32) :: pin_count = 0_int32
    integer(int32) :: label_length = 0_int32
    character(len=MAX_LABEL_LENGTH) :: label = ""
    logical :: is_active = .false.
    logical :: is_retired = .false.
    logical :: is_used = .false.
  end type GenerationSlot

  type :: PinSlot
    integer(int64) :: handle = 0_int64
    integer(int64) :: owner_runtime = 0_int64
    integer(int64) :: generation_handle = 0_int64
    logical :: is_used = .false.
  end type PinSlot

  type(GenerationSlot), save :: generation_slots(MAX_GENERATION_COUNT)
  type(PinSlot), save :: pin_slots(MAX_PIN_COUNT)
  integer(int64), save :: next_generation_handle = 1_int64
  integer(int64), save :: next_pin_handle = 1_int64

contains
  function glamin_generation_create_c(runtime, index, label, label_length, &
      out_generation) bind(c, name="glamin_generation_create") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int64_t), value :: index
    type(c_ptr), value :: label
    integer(c_int64_t), value :: label_length
    type(c_ptr), value :: out_generation
    integer(c_int32_t) :: status
    character(kind=c_char), pointer :: label_characters(:)
    integer(c_int64_t), pointer :: generation_output
    integer(int32) :: bind_status
    integer(int32) :: character_index
    integer(int32) :: slot_index

    if (.not. c_api_runtime_is_active(int(runtime, int64))) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (.not. c_associated(out_generation)) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "out_generation must not be null")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if
    call c_f_pointer(out_generation, generation_output)
    generation_output = 0_c_int64_t

    if (.not. c_associated(label) .or. label_length <= 0_c_int64_t .or. &
        label_length > int(MAX_LABEL_LENGTH, c_int64_t)) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "generation label must contain 1 to 128 bytes")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    slot_index = find_free_generation_slot()
    if (slot_index == 0_int32) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "generation registry is full")
      status = int(GLAMIN_ERR_OOM, c_int32_t)
      return
    end if

    if (next_generation_handle == huge(next_generation_handle)) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "generation handle space is exhausted")
      status = int(GLAMIN_ERR_OOM, c_int32_t)
      return
    end if

    call c_f_pointer(label, label_characters, [int(label_length, int32)])
    generation_slots(slot_index)%label = ""
    do character_index = 1_int32, int(label_length, int32)
      if (label_characters(character_index) == c_null_char) then
        call c_api_set_runtime_error(int(runtime, int64), &
          "generation label must not contain a null byte")
        status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
        return
      end if
      generation_slots(slot_index)%label(character_index:character_index) = &
        achar(iachar(label_characters(character_index)))
    end do

    call c_api_bind_index_generation(int(runtime, int64), int(index, int64), &
      next_generation_handle, bind_status)
    if (bind_status /= GLAMIN_OK) then
      if (bind_status == GLAMIN_ERR_NOT_READY) then
        call c_api_set_runtime_error(int(runtime, int64), &
          "index already belongs to a generation")
      else
        call c_api_set_runtime_error(int(runtime, int64), &
          "index handle is invalid or is owned by a different runtime")
      end if
      status = int(bind_status, c_int32_t)
      return
    end if

    generation_slots(slot_index)%handle = next_generation_handle
    generation_slots(slot_index)%owner_runtime = int(runtime, int64)
    generation_slots(slot_index)%index_handle = int(index, int64)
    generation_slots(slot_index)%pin_count = 0_int32
    generation_slots(slot_index)%label_length = int(label_length, int32)
    generation_slots(slot_index)%is_active = .false.
    generation_slots(slot_index)%is_retired = .false.
    generation_slots(slot_index)%is_used = .true.
    generation_output = int(next_generation_handle, c_int64_t)
    next_generation_handle = next_generation_handle + 1_int64
    call c_api_clear_runtime_error(int(runtime, int64))
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_generation_create_c

  function glamin_generation_activate_c(runtime, generation) &
      bind(c, name="glamin_generation_activate") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int64_t), value :: generation
    integer(c_int32_t) :: status
    integer(int32) :: candidate_index
    integer(int32) :: slot_index

    if (.not. c_api_runtime_is_active(int(runtime, int64))) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    slot_index = find_generation_slot(int(runtime, int64), int(generation, int64))
    if (slot_index == 0_int32) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "generation handle is invalid or is owned by a different runtime")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if
    if (generation_slots(slot_index)%is_retired) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "retired generation cannot be activated")
      status = int(GLAMIN_ERR_NOT_READY, c_int32_t)
      return
    end if

    do candidate_index = 1_int32, MAX_GENERATION_COUNT
      if (generation_slots(candidate_index)%is_used .and. &
          generation_slots(candidate_index)%owner_runtime == int(runtime, int64)) then
        generation_slots(candidate_index)%is_active = .false.
      end if
    end do
    generation_slots(slot_index)%is_active = .true.
    call c_api_clear_runtime_error(int(runtime, int64))
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_generation_activate_c

  function glamin_generation_deactivate_c(runtime) &
      bind(c, name="glamin_generation_deactivate") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int32_t) :: status
    integer(int32) :: candidate_index

    if (.not. c_api_runtime_is_active(int(runtime, int64))) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    do candidate_index = 1_int32, MAX_GENERATION_COUNT
      if (generation_slots(candidate_index)%is_used .and. &
          generation_slots(candidate_index)%owner_runtime == int(runtime, int64)) then
        generation_slots(candidate_index)%is_active = .false.
      end if
    end do
    call c_api_clear_runtime_error(int(runtime, int64))
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_generation_deactivate_c

  function glamin_generation_pin_active_c(runtime, out_pin, out_generation) &
      bind(c, name="glamin_generation_pin_active") result(status)
    integer(c_int64_t), value :: runtime
    type(c_ptr), value :: out_pin
    type(c_ptr), value :: out_generation
    integer(c_int32_t) :: status
    integer(c_int64_t), pointer :: generation_output
    integer(c_int64_t), pointer :: pin_output
    integer(int32) :: generation_slot_index
    integer(int32) :: pin_slot_index

    if (.not. c_api_runtime_is_active(int(runtime, int64))) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (.not. c_associated(out_pin) .or. .not. c_associated(out_generation)) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "out_pin and out_generation must not be null")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if
    call c_f_pointer(out_pin, pin_output)
    call c_f_pointer(out_generation, generation_output)
    pin_output = 0_c_int64_t
    generation_output = 0_c_int64_t

    generation_slot_index = find_active_generation_slot(int(runtime, int64))
    if (generation_slot_index == 0_int32) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "no active generation is available to pin")
      status = int(GLAMIN_ERR_NOT_READY, c_int32_t)
      return
    end if

    pin_slot_index = find_free_pin_slot()
    if (pin_slot_index == 0_int32) then
      call c_api_set_runtime_error(int(runtime, int64), "pin registry is full")
      status = int(GLAMIN_ERR_OOM, c_int32_t)
      return
    end if
    if (next_pin_handle == huge(next_pin_handle)) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "pin handle space is exhausted")
      status = int(GLAMIN_ERR_OOM, c_int32_t)
      return
    end if

    pin_slots(pin_slot_index)%handle = next_pin_handle
    pin_slots(pin_slot_index)%owner_runtime = int(runtime, int64)
    pin_slots(pin_slot_index)%generation_handle = &
      generation_slots(generation_slot_index)%handle
    pin_slots(pin_slot_index)%is_used = .true.
    generation_slots(generation_slot_index)%pin_count = &
      generation_slots(generation_slot_index)%pin_count + 1_int32
    pin_output = int(next_pin_handle, c_int64_t)
    generation_output = int(generation_slots(generation_slot_index)%handle, c_int64_t)
    next_pin_handle = next_pin_handle + 1_int64
    call c_api_clear_runtime_error(int(runtime, int64))
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_generation_pin_active_c

  function glamin_generation_unpin_c(runtime, pin) &
      bind(c, name="glamin_generation_unpin") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int64_t), value :: pin
    integer(c_int32_t) :: status
    integer(int32) :: generation_slot_index
    integer(int32) :: pin_slot_index
    integer(int32) :: reclaim_status

    if (.not. c_api_runtime_is_active(int(runtime, int64))) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    pin_slot_index = find_pin_slot(int(runtime, int64), int(pin, int64))
    if (pin_slot_index == 0_int32) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "pin handle is invalid or is owned by a different runtime")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    generation_slot_index = find_generation_slot(int(runtime, int64), &
      pin_slots(pin_slot_index)%generation_handle)
    if (generation_slot_index == 0_int32) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "pin refers to an unavailable generation")
      status = int(GLAMIN_ERR_UNKNOWN, c_int32_t)
      return
    end if

    pin_slots(pin_slot_index) = PinSlot()
    generation_slots(generation_slot_index)%pin_count = &
      generation_slots(generation_slot_index)%pin_count - 1_int32
    if (generation_slots(generation_slot_index)%is_retired .and. &
        generation_slots(generation_slot_index)%pin_count == 0_int32) then
      call reclaim_generation(generation_slot_index, reclaim_status)
      if (reclaim_status /= GLAMIN_OK) then
        call c_api_set_runtime_error(int(runtime, int64), &
          "failed to reclaim retired generation")
        status = int(reclaim_status, c_int32_t)
        return
      end if
    end if

    call c_api_clear_runtime_error(int(runtime, int64))
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_generation_unpin_c

  function glamin_generation_retire_c(runtime, generation) &
      bind(c, name="glamin_generation_retire") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int64_t), value :: generation
    integer(c_int32_t) :: status
    integer(int32) :: reclaim_status
    integer(int32) :: slot_index

    if (.not. c_api_runtime_is_active(int(runtime, int64))) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    slot_index = find_generation_slot(int(runtime, int64), int(generation, int64))
    if (slot_index == 0_int32) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "generation handle is invalid or is owned by a different runtime")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if
    if (generation_slots(slot_index)%is_active) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "active generation must be superseded or deactivated before retirement")
      status = int(GLAMIN_ERR_NOT_READY, c_int32_t)
      return
    end if
    if (generation_slots(slot_index)%is_retired) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "generation is already retired")
      status = int(GLAMIN_ERR_NOT_READY, c_int32_t)
      return
    end if

    generation_slots(slot_index)%is_retired = .true.
    if (generation_slots(slot_index)%pin_count == 0_int32) then
      call reclaim_generation(slot_index, reclaim_status)
      if (reclaim_status /= GLAMIN_OK) then
        call c_api_set_runtime_error(int(runtime, int64), &
          "failed to reclaim retired generation")
        status = int(reclaim_status, c_int32_t)
        return
      end if
    end if

    call c_api_clear_runtime_error(int(runtime, int64))
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_generation_retire_c

  function glamin_generation_label_c(runtime, generation, buffer, capacity, &
      out_required) bind(c, name="glamin_generation_label") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int64_t), value :: generation
    type(c_ptr), value :: buffer
    integer(c_int64_t), value :: capacity
    type(c_ptr), value :: out_required
    integer(c_int32_t) :: status
    character(kind=c_char), pointer :: buffer_characters(:)
    integer(c_int64_t), pointer :: required_output
    integer(int32) :: character_index
    integer(int32) :: slot_index

    if (.not. c_api_runtime_is_active(int(runtime, int64))) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if
    if (.not. c_associated(out_required)) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "out_required must not be null")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if
    call c_f_pointer(out_required, required_output)

    slot_index = find_generation_slot(int(runtime, int64), int(generation, int64))
    if (slot_index == 0_int32) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "generation handle is invalid or is owned by a different runtime")
      required_output = 0_c_int64_t
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    required_output = int(generation_slots(slot_index)%label_length + 1_int32, &
      c_int64_t)
    if (.not. c_associated(buffer) .or. capacity < required_output) then
      status = GLAMIN_STATUS_BUFFER_TOO_SMALL
      return
    end if
    if (capacity > int(huge(0_int32), c_int64_t)) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "generation label buffer capacity exceeds the supported range")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    call c_f_pointer(buffer, buffer_characters, [int(capacity, int32)])
    do character_index = 1_int32, generation_slots(slot_index)%label_length
      buffer_characters(character_index) = achar( &
        iachar(generation_slots(slot_index)%label(character_index:character_index)), &
        kind=c_char)
    end do
    buffer_characters(generation_slots(slot_index)%label_length + 1_int32) = &
      c_null_char
    call c_api_clear_runtime_error(int(runtime, int64))
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_generation_label_c

  function glamin_generation_search_f32_c(runtime, pin, queries, query_count, &
      query_stride, k, out_distances, out_labels) &
      bind(c, name="glamin_generation_search_f32") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int64_t), value :: pin
    type(c_ptr), value :: queries
    integer(c_int64_t), value :: query_count
    integer(c_int32_t), value :: query_stride
    integer(c_int32_t), value :: k
    type(c_ptr), value :: out_distances
    type(c_ptr), value :: out_labels
    integer(c_int32_t) :: status
    integer(int32) :: generation_slot_index
    integer(int32) :: pin_slot_index

    if (.not. c_api_runtime_is_active(int(runtime, int64))) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    pin_slot_index = find_pin_slot(int(runtime, int64), int(pin, int64))
    if (pin_slot_index == 0_int32) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "pin handle is invalid or is owned by a different runtime")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if
    generation_slot_index = find_generation_slot(int(runtime, int64), &
      pin_slots(pin_slot_index)%generation_handle)
    if (generation_slot_index == 0_int32) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "pin refers to an unavailable generation")
      status = int(GLAMIN_ERR_UNKNOWN, c_int32_t)
      return
    end if

    status = glamin_index_search_f32_c(runtime, &
      int(generation_slots(generation_slot_index)%index_handle, c_int64_t), &
      queries, query_count, query_stride, k, out_distances, out_labels)
  end function glamin_generation_search_f32_c

  function find_free_generation_slot() result(slot_index)
    integer(int32) :: slot_index
    integer(int32) :: candidate_index

    slot_index = 0_int32
    do candidate_index = 1_int32, MAX_GENERATION_COUNT
      if (.not. generation_slots(candidate_index)%is_used) then
        slot_index = candidate_index
        return
      end if
    end do
  end function find_free_generation_slot

  function find_generation_slot(runtime_handle, generation_handle) result(slot_index)
    integer(int64), intent(in) :: runtime_handle
    integer(int64), intent(in) :: generation_handle
    integer(int32) :: slot_index
    integer(int32) :: candidate_index

    slot_index = 0_int32
    if (runtime_handle == 0_int64 .or. generation_handle == 0_int64) return
    do candidate_index = 1_int32, MAX_GENERATION_COUNT
      if (generation_slots(candidate_index)%is_used .and. &
          generation_slots(candidate_index)%owner_runtime == runtime_handle .and. &
          generation_slots(candidate_index)%handle == generation_handle) then
        slot_index = candidate_index
        return
      end if
    end do
  end function find_generation_slot

  function find_active_generation_slot(runtime_handle) result(slot_index)
    integer(int64), intent(in) :: runtime_handle
    integer(int32) :: slot_index
    integer(int32) :: candidate_index

    slot_index = 0_int32
    do candidate_index = 1_int32, MAX_GENERATION_COUNT
      if (generation_slots(candidate_index)%is_used .and. &
          generation_slots(candidate_index)%owner_runtime == runtime_handle .and. &
          generation_slots(candidate_index)%is_active .and. &
          .not. generation_slots(candidate_index)%is_retired) then
        slot_index = candidate_index
        return
      end if
    end do
  end function find_active_generation_slot

  function find_free_pin_slot() result(slot_index)
    integer(int32) :: slot_index
    integer(int32) :: candidate_index

    slot_index = 0_int32
    do candidate_index = 1_int32, MAX_PIN_COUNT
      if (.not. pin_slots(candidate_index)%is_used) then
        slot_index = candidate_index
        return
      end if
    end do
  end function find_free_pin_slot

  function find_pin_slot(runtime_handle, pin_handle) result(slot_index)
    integer(int64), intent(in) :: runtime_handle
    integer(int64), intent(in) :: pin_handle
    integer(int32) :: slot_index
    integer(int32) :: candidate_index

    slot_index = 0_int32
    if (runtime_handle == 0_int64 .or. pin_handle == 0_int64) return
    do candidate_index = 1_int32, MAX_PIN_COUNT
      if (pin_slots(candidate_index)%is_used .and. &
          pin_slots(candidate_index)%owner_runtime == runtime_handle .and. &
          pin_slots(candidate_index)%handle == pin_handle) then
        slot_index = candidate_index
        return
      end if
    end do
  end function find_pin_slot

  subroutine reclaim_generation(slot_index, status)
    integer(int32), intent(in) :: slot_index
    integer(int32), intent(out) :: status

    call c_api_unbind_index_generation( &
      generation_slots(slot_index)%owner_runtime, &
      generation_slots(slot_index)%index_handle, &
      generation_slots(slot_index)%handle, status)
    if (status /= GLAMIN_OK) return
    generation_slots(slot_index) = GenerationSlot()
  end subroutine reclaim_generation
end module glamin_c_generation_api
