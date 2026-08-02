module glamin_c_api
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_char, c_f_pointer, c_float, c_int32_t, &
    c_int64_t, c_null_char, c_ptr
  use glamin_errors, only: GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_NOT_READY, &
    GLAMIN_ERR_OOM, GLAMIN_OK
  use glamin_index_flat, only: flat_add, flat_create_handle, flat_destroy_handle, &
    flat_search
  use glamin_memory, only: free_aligned
  use glamin_metrics, only: METRIC_IP, METRIC_L2
  use glamin_runtime, only: RuntimeContext, start_runtime, stop_runtime
  use glamin_types, only: IndexHandle, VectorBlock
  implicit none
  private

  public :: glamin_abi_version_c
  public :: glamin_runtime_create_c
  public :: glamin_runtime_destroy_c
  public :: glamin_flat_index_create_c
  public :: glamin_index_destroy_c
  public :: glamin_index_add_f32_c
  public :: glamin_index_search_f32_c
  public :: glamin_last_error_c
  public :: c_api_runtime_is_active
  public :: c_api_set_runtime_error
  public :: c_api_clear_runtime_error
  public :: c_api_bind_index_generation
  public :: c_api_unbind_index_generation

  integer(int32), parameter :: MAX_RUNTIME_COUNT = 64_int32
  integer(int32), parameter :: MAX_INDEX_COUNT = 256_int32
  integer(int32), parameter :: ERROR_MESSAGE_LENGTH = 512_int32
  integer(int64), parameter :: FLOAT_BYTES = &
    int(storage_size(0.0_real32) / 8, int64)
  integer(int64), parameter :: MAX_FLOAT_ELEMENTS = &
    (huge(0_int64) - modulo(huge(0_int64), FLOAT_BYTES)) / FLOAT_BYTES
  integer(c_int32_t), parameter :: GLAMIN_ABI_VERSION = 3_c_int32_t
  integer(c_int32_t), parameter :: GLAMIN_STATUS_BUFFER_TOO_SMALL = 6_c_int32_t

  type :: RuntimeSlot
    type(RuntimeContext) :: context
    integer(int64) :: handle = 0_int64
    character(len=ERROR_MESSAGE_LENGTH) :: last_error = ""
    logical :: is_used = .false.
  end type RuntimeSlot

  type :: IndexSlot
    type(IndexHandle) :: native_handle
    integer(int64) :: handle = 0_int64
    integer(int64) :: owner_runtime = 0_int64
    integer(int64) :: generation_handle = 0_int64
    integer(int64) :: vector_count = 0_int64
    integer(int32) :: dimension = 0_int32
    integer(int32) :: metric = 0_int32
    logical :: is_used = .false.
  end type IndexSlot

  type(RuntimeSlot), save :: runtime_slots(MAX_RUNTIME_COUNT)
  type(IndexSlot), save :: index_slots(MAX_INDEX_COUNT)
  integer(int64), save :: next_runtime_handle = 1_int64
  integer(int64), save :: next_index_handle = 1_int64
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

    if (runtime_has_indexes(int(runtime, int64))) then
      runtime_slots(slot_index)%last_error = &
        "runtime still owns indexes; destroy them before stopping the runtime"
      status = int(GLAMIN_ERR_NOT_READY, c_int32_t)
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

  function glamin_flat_index_create_c(runtime, dimension, metric, out_index) &
      bind(c, name="glamin_flat_index_create") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int32_t), value :: dimension
    integer(c_int32_t), value :: metric
    type(c_ptr), value :: out_index
    integer(c_int32_t) :: status
    integer(c_int64_t), pointer :: index_output
    type(IndexHandle) :: native_handle
    integer(int32) :: index_status
    integer(int32) :: runtime_slot_index
    integer(int32) :: slot_index

    runtime_slot_index = find_runtime_slot(int(runtime, int64))
    if (runtime_slot_index == 0_int32) then
      call set_global_error("runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (.not. c_associated(out_index)) then
      runtime_slots(runtime_slot_index)%last_error = "out_index must not be null"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    call c_f_pointer(out_index, index_output)
    index_output = 0_c_int64_t

    if (dimension <= 0_c_int32_t) then
      runtime_slots(runtime_slot_index)%last_error = "index dimension must be positive"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (metric /= int(METRIC_L2, c_int32_t) .and. &
        metric /= int(METRIC_IP, c_int32_t)) then
      runtime_slots(runtime_slot_index)%last_error = "flat index metric is unsupported"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    slot_index = find_free_index_slot()
    if (slot_index == 0_int32) then
      runtime_slots(runtime_slot_index)%last_error = "index registry is full"
      status = int(GLAMIN_ERR_OOM, c_int32_t)
      return
    end if

    if (next_index_handle == huge(next_index_handle)) then
      runtime_slots(runtime_slot_index)%last_error = "index handle space is exhausted"
      status = int(GLAMIN_ERR_OOM, c_int32_t)
      return
    end if

    call flat_create_handle(native_handle, int(dimension, int32), &
      int(metric, int32), index_status)
    if (index_status /= GLAMIN_OK) then
      runtime_slots(runtime_slot_index)%last_error = "failed to create flat index"
      status = int(index_status, c_int32_t)
      return
    end if

    index_slots(slot_index)%native_handle = native_handle
    index_slots(slot_index)%handle = next_index_handle
    index_slots(slot_index)%owner_runtime = int(runtime, int64)
    index_slots(slot_index)%vector_count = 0_int64
    index_slots(slot_index)%dimension = int(dimension, int32)
    index_slots(slot_index)%metric = int(metric, int32)
    index_slots(slot_index)%is_used = .true.
    index_output = int(next_index_handle, c_int64_t)
    next_index_handle = next_index_handle + 1_int64
    runtime_slots(runtime_slot_index)%last_error = ""
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_flat_index_create_c

  function glamin_index_destroy_c(runtime, index) &
      bind(c, name="glamin_index_destroy") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int64_t), value :: index
    integer(c_int32_t) :: status
    integer(int32) :: index_status
    integer(int32) :: runtime_slot_index
    integer(int32) :: slot_index

    runtime_slot_index = find_runtime_slot(int(runtime, int64))
    if (runtime_slot_index == 0_int32) then
      call set_global_error("runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    slot_index = find_index_slot(int(runtime, int64), int(index, int64))
    if (slot_index == 0_int32) then
      runtime_slots(runtime_slot_index)%last_error = &
        "index handle is invalid or is owned by a different runtime"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (index_slots(slot_index)%generation_handle /= 0_int64) then
      runtime_slots(runtime_slot_index)%last_error = &
        "index belongs to a mounted or pinned generation"
      status = int(GLAMIN_ERR_NOT_READY, c_int32_t)
      return
    end if

    call flat_destroy_handle(index_slots(slot_index)%native_handle, index_status)
    if (index_status /= GLAMIN_OK) then
      runtime_slots(runtime_slot_index)%last_error = "failed to destroy flat index"
      status = int(index_status, c_int32_t)
      return
    end if

    index_slots(slot_index) = IndexSlot()
    runtime_slots(runtime_slot_index)%last_error = ""
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_index_destroy_c

  function glamin_index_add_f32_c(runtime, index, vectors, vector_count, &
      vector_stride) bind(c, name="glamin_index_add_f32") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int64_t), value :: index
    type(c_ptr), value :: vectors
    integer(c_int64_t), value :: vector_count
    integer(c_int32_t), value :: vector_stride
    integer(c_int32_t) :: status
    type(VectorBlock) :: vector_block
    integer(int32) :: index_status
    integer(int32) :: runtime_slot_index
    integer(int32) :: slot_index

    runtime_slot_index = find_runtime_slot(int(runtime, int64))
    if (runtime_slot_index == 0_int32) then
      call set_global_error("runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    slot_index = find_index_slot(int(runtime, int64), int(index, int64))
    if (slot_index == 0_int32) then
      runtime_slots(runtime_slot_index)%last_error = &
        "index handle is invalid or is owned by a different runtime"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (index_slots(slot_index)%generation_handle /= 0_int64) then
      runtime_slots(runtime_slot_index)%last_error = &
        "cannot add vectors after an index is mounted as a generation"
      status = int(GLAMIN_ERR_NOT_READY, c_int32_t)
      return
    end if

    if (.not. c_associated(vectors)) then
      runtime_slots(runtime_slot_index)%last_error = "vectors must not be null"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (vector_count <= 0_c_int64_t) then
      runtime_slots(runtime_slot_index)%last_error = "vector_count must be positive"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (vector_stride < int(index_slots(slot_index)%dimension, c_int32_t)) then
      runtime_slots(runtime_slot_index)%last_error = &
        "vector_stride must be at least the index dimension"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (int(vector_count, int64) > &
        int(huge(0_int32), int64) - index_slots(slot_index)%vector_count) then
      runtime_slots(runtime_slot_index)%last_error = &
        "flat index row count exceeds the supported label range"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (int(index_slots(slot_index)%dimension, int64) > &
        MAX_FLOAT_ELEMENTS / &
        (index_slots(slot_index)%vector_count + int(vector_count, int64))) then
      runtime_slots(runtime_slot_index)%last_error = "flat index allocation size overflows int64"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    vector_block = VectorBlock()
    vector_block%data = vectors
    vector_block%length = int(vector_count, int64)
    vector_block%dim = index_slots(slot_index)%dimension
    vector_block%stride = int(vector_stride, int32)
    vector_block%elem_size = int(storage_size(0.0_real32) / 8, int32)

    call flat_add(index_slots(slot_index)%native_handle, vector_block, index_status)
    if (index_status /= GLAMIN_OK) then
      runtime_slots(runtime_slot_index)%last_error = "failed to add vectors to flat index"
      status = int(index_status, c_int32_t)
      return
    end if

    index_slots(slot_index)%vector_count = index_slots(slot_index)%vector_count + &
      int(vector_count, int64)
    runtime_slots(runtime_slot_index)%last_error = ""
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_index_add_f32_c

  function glamin_index_search_f32_c(runtime, index, queries, query_count, &
      query_stride, k, out_distances, out_labels) &
      bind(c, name="glamin_index_search_f32") result(status)
    integer(c_int64_t), value :: runtime
    integer(c_int64_t), value :: index
    type(c_ptr), value :: queries
    integer(c_int64_t), value :: query_count
    integer(c_int32_t), value :: query_stride
    integer(c_int32_t), value :: k
    type(c_ptr), value :: out_distances
    type(c_ptr), value :: out_labels
    integer(c_int32_t) :: status
    type(VectorBlock) :: query_block
    type(VectorBlock) :: distances
    type(VectorBlock) :: labels
    real(c_float), pointer :: distance_output(:)
    real(real32), pointer :: native_distances(:)
    integer(c_int32_t), pointer :: native_labels(:)
    integer(c_int64_t), pointer :: label_output(:)
    integer(int64) :: result_count
    integer(int64) :: result_index
    integer(int32) :: index_status
    integer(int32) :: runtime_slot_index
    integer(int32) :: slot_index

    runtime_slot_index = find_runtime_slot(int(runtime, int64))
    if (runtime_slot_index == 0_int32) then
      call set_global_error("runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    slot_index = find_index_slot(int(runtime, int64), int(index, int64))
    if (slot_index == 0_int32) then
      runtime_slots(runtime_slot_index)%last_error = &
        "index handle is invalid or is owned by a different runtime"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (.not. c_associated(queries) .or. .not. c_associated(out_distances) .or. &
        .not. c_associated(out_labels)) then
      runtime_slots(runtime_slot_index)%last_error = &
        "queries and search output buffers must not be null"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (query_count <= 0_c_int64_t .or. k <= 0_c_int32_t) then
      runtime_slots(runtime_slot_index)%last_error = &
        "query_count and k must be positive"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (query_stride < int(index_slots(slot_index)%dimension, c_int32_t)) then
      runtime_slots(runtime_slot_index)%last_error = &
        "query_stride must be at least the index dimension"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (int(k, int64) > index_slots(slot_index)%vector_count) then
      runtime_slots(runtime_slot_index)%last_error = &
        "k must not exceed the number of indexed vectors"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    if (int(query_count, int64) > huge(0_int64) / int(k, int64)) then
      runtime_slots(runtime_slot_index)%last_error = "search result size overflows int64"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if
    if (int(query_count, int64) > &
        huge(0_int64) / int(query_stride, int64) .or. &
        int(query_count, int64) > &
        MAX_FLOAT_ELEMENTS / int(k, int64)) then
      runtime_slots(runtime_slot_index)%last_error = "search buffer size overflows int64"
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if
    result_count = int(query_count, int64) * int(k, int64)

    query_block = VectorBlock()
    query_block%data = queries
    query_block%length = int(query_count, int64)
    query_block%dim = index_slots(slot_index)%dimension
    query_block%stride = int(query_stride, int32)
    query_block%elem_size = int(storage_size(0.0_real32) / 8, int32)
    distances = VectorBlock()
    labels = VectorBlock()

    call flat_search(index_slots(slot_index)%native_handle, query_block, &
      int(k, int32), distances, labels, index_status)
    if (index_status /= GLAMIN_OK) then
      call release_vector_block(distances)
      call release_vector_block(labels)
      runtime_slots(runtime_slot_index)%last_error = "flat index search failed"
      status = int(index_status, c_int32_t)
      return
    end if

    call c_f_pointer(distances%data, native_distances, [result_count])
    call c_f_pointer(labels%data, native_labels, [result_count])
    call c_f_pointer(out_distances, distance_output, [result_count])
    call c_f_pointer(out_labels, label_output, [result_count])
    distance_output = native_distances
    do result_index = 1_int64, result_count
      label_output(result_index) = int(native_labels(result_index), c_int64_t)
    end do

    call release_vector_block(distances)
    call release_vector_block(labels)
    runtime_slots(runtime_slot_index)%last_error = ""
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_index_search_f32_c

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

  function find_free_index_slot() result(slot_index)
    integer(int32) :: slot_index
    integer(int32) :: candidate_index

    slot_index = 0_int32
    do candidate_index = 1_int32, MAX_INDEX_COUNT
      if (.not. index_slots(candidate_index)%is_used) then
        slot_index = candidate_index
        return
      end if
    end do
  end function find_free_index_slot

  function find_index_slot(runtime_handle, index_handle) result(slot_index)
    integer(int64), intent(in) :: runtime_handle
    integer(int64), intent(in) :: index_handle
    integer(int32) :: slot_index
    integer(int32) :: candidate_index

    slot_index = 0_int32
    if (runtime_handle == 0_int64 .or. index_handle == 0_int64) then
      return
    end if

    do candidate_index = 1_int32, MAX_INDEX_COUNT
      if (index_slots(candidate_index)%is_used .and. &
          index_slots(candidate_index)%owner_runtime == runtime_handle .and. &
          index_slots(candidate_index)%handle == index_handle) then
        slot_index = candidate_index
        return
      end if
    end do
  end function find_index_slot

  function runtime_has_indexes(runtime_handle) result(has_indexes)
    integer(int64), intent(in) :: runtime_handle
    logical :: has_indexes
    integer(int32) :: candidate_index

    has_indexes = .false.
    do candidate_index = 1_int32, MAX_INDEX_COUNT
      if (index_slots(candidate_index)%is_used .and. &
          index_slots(candidate_index)%owner_runtime == runtime_handle) then
        has_indexes = .true.
        return
      end if
    end do
  end function runtime_has_indexes

  subroutine release_vector_block(block)
    type(VectorBlock), intent(inout) :: block
    integer(int32) :: free_status

    if (c_associated(block%data)) then
      call free_aligned(block%data, free_status)
    end if
    block = VectorBlock()
  end subroutine release_vector_block

  function c_api_runtime_is_active(runtime_handle) result(is_active)
    integer(int64), intent(in) :: runtime_handle
    logical :: is_active

    is_active = find_runtime_slot(runtime_handle) /= 0_int32
  end function c_api_runtime_is_active

  subroutine c_api_set_runtime_error(runtime_handle, message)
    integer(int64), intent(in) :: runtime_handle
    character(len=*), intent(in) :: message
    integer(int32) :: slot_index

    slot_index = find_runtime_slot(runtime_handle)
    if (slot_index == 0_int32) then
      call set_global_error(message)
      return
    end if
    runtime_slots(slot_index)%last_error = message
  end subroutine c_api_set_runtime_error

  subroutine c_api_clear_runtime_error(runtime_handle)
    integer(int64), intent(in) :: runtime_handle
    integer(int32) :: slot_index

    slot_index = find_runtime_slot(runtime_handle)
    if (slot_index /= 0_int32) then
      runtime_slots(slot_index)%last_error = ""
    end if
  end subroutine c_api_clear_runtime_error

  subroutine c_api_bind_index_generation(runtime_handle, index_handle, &
      generation_handle, status)
    integer(int64), intent(in) :: runtime_handle
    integer(int64), intent(in) :: index_handle
    integer(int64), intent(in) :: generation_handle
    integer(int32), intent(out) :: status
    integer(int32) :: slot_index

    slot_index = find_index_slot(runtime_handle, index_handle)
    if (slot_index == 0_int32 .or. generation_handle == 0_int64) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if
    if (index_slots(slot_index)%generation_handle /= 0_int64) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    index_slots(slot_index)%generation_handle = generation_handle
    status = GLAMIN_OK
  end subroutine c_api_bind_index_generation

  subroutine c_api_unbind_index_generation(runtime_handle, index_handle, &
      generation_handle, status)
    integer(int64), intent(in) :: runtime_handle
    integer(int64), intent(in) :: index_handle
    integer(int64), intent(in) :: generation_handle
    integer(int32), intent(out) :: status
    integer(int32) :: slot_index

    slot_index = find_index_slot(runtime_handle, index_handle)
    if (slot_index == 0_int32 .or. &
        index_slots(slot_index)%generation_handle /= generation_handle) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    index_slots(slot_index)%generation_handle = 0_int64
    status = GLAMIN_OK
  end subroutine c_api_unbind_index_generation

  subroutine set_global_error(message)
    character(len=*), intent(in) :: message

    global_last_error = message
  end subroutine set_global_error
end module glamin_c_api
