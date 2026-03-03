module glamin_async
  use iso_fortran_env, only: int32, int64
  use iso_c_binding, only: c_associated, c_f_pointer, c_loc, c_null_ptr, c_ptr, c_int32_t, c_int64_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM, &
    GLAMIN_ERR_NOT_READY
  use glamin_status, only: REQUEST_PENDING, REQUEST_RUNNING, REQUEST_COMPLETED, REQUEST_CANCELLED, &
    REQUEST_FAILED
  use glamin_types, only: Request, VectorBlock, SearchPlan, IndexHandle
  use glamin_index_flat, only: flat_add, flat_search
  use glamin_worker_pool, only: WorkerPool, JobCallback, submit_request_job_with_callback
  implicit none
  private

  public :: submit_search
  public :: submit_add
  public :: submit_train
  public :: poll_request
  public :: wait_request
  public :: cancel_request
  public :: schedule_request
  public :: get_search_results

  type :: RequestContext
    integer(int64) :: request_id = 0
  end type RequestContext

  enum, bind(c)
    enumerator :: REQUEST_KIND_NONE = 0
    enumerator :: REQUEST_KIND_SEARCH = 1
    enumerator :: REQUEST_KIND_ADD = 2
    enumerator :: REQUEST_KIND_TRAIN = 3
  end enum

  type :: RequestPayload
    integer(int32) :: kind = REQUEST_KIND_NONE
    type(IndexHandle) :: index
    type(SearchPlan) :: plan
    type(VectorBlock) :: queries
    type(VectorBlock) :: vectors
    type(VectorBlock) :: distances
    type(VectorBlock) :: labels
  end type RequestPayload

  integer(int64), save :: next_request_id = 1_int64
  integer(int32), allocatable, save :: request_status(:)
  integer(int32), allocatable, save :: request_error(:)
  type(RequestPayload), allocatable, save :: request_payload(:)
  type(RequestContext), allocatable, target, save :: request_context(:)
  integer(c_int32_t), parameter :: WAIT_SLEEP_MS = 1_c_int32_t

  interface
    subroutine glamin_sleep_ms(milliseconds) bind(c, name="glamin_sleep_ms")
      import :: c_int32_t
      integer(c_int32_t), value :: milliseconds
    end subroutine glamin_sleep_ms
  end interface

contains
  subroutine submit_search(index, plan, queries, request_handle)
    type(IndexHandle), intent(in) :: index
    type(SearchPlan), intent(in) :: plan
    type(VectorBlock), intent(in) :: queries
    type(Request), intent(out) :: request_handle
    integer(int32) :: status

    call register_request(request_handle, status)
    if (status == GLAMIN_OK) then
      call set_payload_search(request_handle%id, index, plan, queries)
    end if
  end subroutine submit_search

  subroutine submit_add(index, vectors, request_handle)
    type(IndexHandle), intent(in) :: index
    type(VectorBlock), intent(in) :: vectors
    type(Request), intent(out) :: request_handle
    integer(int32) :: status

    call register_request(request_handle, status)
    if (status == GLAMIN_OK) then
      call set_payload_vectors(request_handle%id, index, vectors, REQUEST_KIND_ADD)
    end if
  end subroutine submit_add

  subroutine submit_train(index, vectors, request_handle)
    type(IndexHandle), intent(in) :: index
    type(VectorBlock), intent(in) :: vectors
    type(Request), intent(out) :: request_handle
    integer(int32) :: status

    call register_request(request_handle, status)
    if (status == GLAMIN_OK) then
      call set_payload_vectors(request_handle%id, index, vectors, REQUEST_KIND_TRAIN)
    end if
  end subroutine submit_train

  subroutine poll_request(request_handle, status)
    type(Request), intent(inout) :: request_handle
    integer(int32), intent(out) :: status

    if (.not. is_valid_request(request_handle%id)) then
      call set_request_state(request_handle, REQUEST_FAILED, GLAMIN_ERR_INVALID_ARG)
      status = request_handle%status
      return
    end if

    status = request_status(request_handle%id)
    request_handle%status = status
    request_handle%error_code = request_error(request_handle%id)
  end subroutine poll_request

  subroutine wait_request(request_handle, status)
    type(Request), intent(inout) :: request_handle
    integer(int32), intent(out) :: status
    integer(int32) :: current_status

    if (.not. is_valid_request(request_handle%id)) then
      call set_request_state(request_handle, REQUEST_FAILED, GLAMIN_ERR_INVALID_ARG)
      status = request_handle%status
      return
    end if

    do
      current_status = request_status(request_handle%id)
      if (current_status /= REQUEST_PENDING .and. current_status /= REQUEST_RUNNING) then
        exit
      end if
      call glamin_sleep_ms(WAIT_SLEEP_MS)
    end do

    status = current_status
    request_handle%status = current_status
    request_handle%error_code = request_error(request_handle%id)
  end subroutine wait_request

  subroutine cancel_request(request_handle, status)
    type(Request), intent(inout) :: request_handle
    integer(int32), intent(out) :: status

    if (.not. is_valid_request(request_handle%id)) then
      call set_request_state(request_handle, REQUEST_FAILED, GLAMIN_ERR_INVALID_ARG)
      status = request_handle%status
      return
    end if

    if (request_status(request_handle%id) /= REQUEST_COMPLETED) then
      request_status(request_handle%id) = REQUEST_CANCELLED
      request_error(request_handle%id) = GLAMIN_OK
    end if

    status = request_status(request_handle%id)
    request_handle%status = status
    request_handle%error_code = request_error(request_handle%id)
  end subroutine cancel_request

  subroutine schedule_request(pool, request_handle, status)
    type(WorkerPool), intent(inout) :: pool
    type(Request), intent(inout) :: request_handle
    integer(int32), intent(out) :: status

    if (.not. is_valid_request(request_handle%id)) then
      call set_request_state(request_handle, REQUEST_FAILED, GLAMIN_ERR_INVALID_ARG)
      status = request_handle%status
      return
    end if

    request_status(request_handle%id) = REQUEST_RUNNING
    request_error(request_handle%id) = GLAMIN_OK

    call ensure_context_capacity(request_handle%id, status)
    if (status /= GLAMIN_OK) then
      request_status(request_handle%id) = REQUEST_FAILED
      request_error(request_handle%id) = status
      request_handle%status = request_status(request_handle%id)
      request_handle%error_code = request_error(request_handle%id)
      return
    end if

    request_context(request_handle%id)%request_id = request_handle%id

    call submit_request_job_with_callback(pool, request_handle%id, execute_request_job, &
      c_loc(request_context(request_handle%id)), status)
    if (status /= GLAMIN_OK) then
      request_status(request_handle%id) = REQUEST_FAILED
      request_error(request_handle%id) = status
      request_handle%status = request_status(request_handle%id)
      request_handle%error_code = request_error(request_handle%id)
      return
    end if

    request_handle%status = request_status(request_handle%id)
    request_handle%error_code = request_error(request_handle%id)
  end subroutine schedule_request

  subroutine get_search_results(request_handle, distances, labels, status)
    type(Request), intent(in) :: request_handle
    type(VectorBlock), intent(out) :: distances
    type(VectorBlock), intent(out) :: labels
    integer(int32), intent(out) :: status

    if (.not. is_valid_request(request_handle%id)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (request_status(request_handle%id) /= REQUEST_COMPLETED) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    if (request_payload(request_handle%id)%kind /= REQUEST_KIND_SEARCH) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    distances = request_payload(request_handle%id)%distances
    labels = request_payload(request_handle%id)%labels
    status = GLAMIN_OK
  end subroutine get_search_results

  subroutine glamin_mark_request_status(request_id, status_code, error_code) &
    bind(c, name="glamin_mark_request_status")
    integer(c_int64_t), value :: request_id
    integer(c_int32_t), value :: status_code
    integer(c_int32_t), value :: error_code
    integer(int32) :: status

    if (request_id <= 0_int64) then
      return
    end if

    call ensure_capacity(request_id, status)
    if (status /= GLAMIN_OK) then
      return
    end if

    request_status(request_id) = status_code
    request_error(request_id) = error_code
  end subroutine glamin_mark_request_status

  subroutine register_request(request_handle, status)
    type(Request), intent(out) :: request_handle
    integer(int32), intent(out) :: status
    integer(int64) :: request_id

    request_id = next_request_id
    call ensure_capacity(request_id, status)
    if (status /= GLAMIN_OK) then
      request_handle%id = 0_int64
      request_handle%status = REQUEST_FAILED
      request_handle%error_code = status
      request_handle%payload = c_null_ptr
      return
    end if

    request_status(request_id) = REQUEST_PENDING
    request_error(request_id) = GLAMIN_OK
    request_payload(request_id) = RequestPayload()

    request_handle%id = request_id
    request_handle%status = REQUEST_PENDING
    request_handle%error_code = GLAMIN_OK
    request_handle%payload = c_null_ptr

    next_request_id = next_request_id + 1_int64
  end subroutine register_request

  subroutine ensure_capacity(request_id, status)
    integer(int64), intent(in) :: request_id
    integer(int32), intent(out) :: status
    integer(int32) :: required
    integer(int32) :: current_size
    integer(int32) :: new_size
    integer(int32) :: alloc_status
    integer(int32), allocatable :: new_status(:)
    integer(int32), allocatable :: new_error(:)
    type(RequestPayload), allocatable :: new_payload(:)
    type(RequestContext), allocatable :: new_context(:)

    if (request_id <= 0_int64) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    required = int(request_id, int32)
    if (required <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (.not. allocated(request_status)) then
      allocate(request_status(required), stat=alloc_status)
      if (alloc_status /= 0_int32) then
        status = GLAMIN_ERR_OOM
        return
      end if

      allocate(request_error(required), stat=alloc_status)
      if (alloc_status /= 0_int32) then
        deallocate(request_status)
        status = GLAMIN_ERR_OOM
        return
      end if

      allocate(request_payload(required), stat=alloc_status)
      if (alloc_status /= 0_int32) then
        deallocate(request_error)
        deallocate(request_status)
        status = GLAMIN_ERR_OOM
        return
      end if

      allocate(request_context(required), stat=alloc_status)
      if (alloc_status /= 0_int32) then
        deallocate(request_payload)
        deallocate(request_error)
        deallocate(request_status)
        status = GLAMIN_ERR_OOM
        return
      end if

      request_status = REQUEST_PENDING
      request_error = GLAMIN_OK
      request_payload = RequestPayload()
      request_context = RequestContext()
      status = GLAMIN_OK
      return
    end if

    if (size(request_status) >= required) then
      status = GLAMIN_OK
      return
    end if

    current_size = size(request_status)
    new_size = max(required, current_size * 2_int32)

    allocate(new_status(new_size), stat=alloc_status)
    if (alloc_status /= 0_int32) then
      status = GLAMIN_ERR_OOM
      return
    end if

    allocate(new_error(new_size), stat=alloc_status)
    if (alloc_status /= 0_int32) then
      deallocate(new_status)
      status = GLAMIN_ERR_OOM
      return
    end if

    allocate(new_payload(new_size), stat=alloc_status)
    if (alloc_status /= 0_int32) then
      deallocate(new_error)
      deallocate(new_status)
      status = GLAMIN_ERR_OOM
      return
    end if

    allocate(new_context(new_size), stat=alloc_status)
    if (alloc_status /= 0_int32) then
      deallocate(new_payload)
      deallocate(new_error)
      deallocate(new_status)
      status = GLAMIN_ERR_OOM
      return
    end if

    new_status = REQUEST_PENDING
    new_error = GLAMIN_OK
    new_payload = RequestPayload()
    new_context = RequestContext()
    new_status(1:current_size) = request_status
    new_error(1:current_size) = request_error
    new_payload(1:current_size) = request_payload
    new_context(1:current_size) = request_context

    call move_alloc(new_status, request_status)
    call move_alloc(new_error, request_error)
    call move_alloc(new_payload, request_payload)
    call move_alloc(new_context, request_context)
    status = GLAMIN_OK
  end subroutine ensure_capacity

  subroutine set_request_state(request_handle, status_code, error_code)
    type(Request), intent(inout) :: request_handle
    integer(int32), intent(in) :: status_code
    integer(int32), intent(in) :: error_code

    request_handle%status = status_code
    request_handle%error_code = error_code
  end subroutine set_request_state

  logical function is_valid_request(request_id)
    integer(int64), intent(in) :: request_id

    if (.not. allocated(request_status)) then
      is_valid_request = .false.
      return
    end if

    is_valid_request = request_id > 0_int64 .and. &
      request_id <= int(size(request_status), int64)
  end function is_valid_request

  subroutine set_payload_search(request_id, index, plan, queries)
    integer(int64), intent(in) :: request_id
    type(IndexHandle), intent(in) :: index
    type(SearchPlan), intent(in) :: plan
    type(VectorBlock), intent(in) :: queries
    integer(int32) :: payload_index

    payload_index = int(request_id, int32)
    if (payload_index <= 0_int32) then
      return
    end if

    request_payload(payload_index)%kind = REQUEST_KIND_SEARCH
    request_payload(payload_index)%index = index
    request_payload(payload_index)%plan = plan
    request_payload(payload_index)%queries = queries
    request_payload(payload_index)%distances = VectorBlock()
    request_payload(payload_index)%labels = VectorBlock()
  end subroutine set_payload_search

  subroutine set_payload_vectors(request_id, index, vectors, kind)
    integer(int64), intent(in) :: request_id
    type(IndexHandle), intent(in) :: index
    type(VectorBlock), intent(in) :: vectors
    integer(int32), intent(in) :: kind
    integer(int32) :: payload_index

    payload_index = int(request_id, int32)
    if (payload_index <= 0_int32) then
      return
    end if

    request_payload(payload_index)%kind = kind
    request_payload(payload_index)%index = index
    request_payload(payload_index)%vectors = vectors
  end subroutine set_payload_vectors

  subroutine ensure_context_capacity(request_id, status)
    integer(int64), intent(in) :: request_id
    integer(int32), intent(out) :: status

    call ensure_capacity(request_id, status)
  end subroutine ensure_context_capacity

  subroutine execute_request_job(context) bind(c)
    type(c_ptr), value :: context
    type(RequestContext), pointer :: request_state

    call c_f_pointer(context, request_state)
    if (.not. associated(request_state)) then
      return
    end if

    call execute_request(request_state%request_id)
  end subroutine execute_request_job

  subroutine execute_request(request_id)
    integer(int64), intent(in) :: request_id
    integer(int32) :: status

    if (.not. is_valid_request(request_id)) then
      return
    end if

    status = request_status(request_id)
    if (status == REQUEST_CANCELLED) then
      return
    end if

    select case (request_payload(request_id)%kind)
    case (REQUEST_KIND_SEARCH)
      call execute_search(request_id)
    case (REQUEST_KIND_ADD)
      call execute_add(request_id)
    case (REQUEST_KIND_TRAIN)
      call execute_train(request_id)
    case default
      call mark_request_failed(request_id, GLAMIN_ERR_INVALID_ARG)
    end select

    if (request_status(request_id) == REQUEST_RUNNING) then
      request_status(request_id) = REQUEST_COMPLETED
    end if
  end subroutine execute_request

  subroutine execute_search(request_id)
    integer(int64), intent(in) :: request_id
    type(SearchPlan) :: plan
    type(VectorBlock) :: queries
    type(IndexHandle) :: index
    integer(int32) :: status

    plan = request_payload(request_id)%plan
    queries = request_payload(request_id)%queries
    index = request_payload(request_id)%index

    call flat_search(index, queries, plan%k, request_payload(request_id)%distances, &
      request_payload(request_id)%labels, status)
    if (status /= GLAMIN_OK) then
      call mark_request_failed(request_id, status)
      return
    end if

    request_error(request_id) = GLAMIN_OK
  end subroutine execute_search

  subroutine execute_add(request_id)
    integer(int64), intent(in) :: request_id
    type(VectorBlock) :: vectors
    type(IndexHandle) :: index
    integer(int32) :: status

    vectors = request_payload(request_id)%vectors
    index = request_payload(request_id)%index

    call flat_add(index, vectors, status)
    if (status /= GLAMIN_OK) then
      call mark_request_failed(request_id, status)
      return
    end if

    request_error(request_id) = GLAMIN_OK
  end subroutine execute_add

  subroutine execute_train(request_id)
    integer(int64), intent(in) :: request_id
    type(VectorBlock) :: vectors
    type(IndexHandle) :: index
    integer(int32) :: status

    vectors = request_payload(request_id)%vectors
    index = request_payload(request_id)%index

    call flat_add(index, vectors, status)
    if (status /= GLAMIN_OK) then
      call mark_request_failed(request_id, status)
      return
    end if

    request_error(request_id) = GLAMIN_OK
  end subroutine execute_train

  subroutine mark_request_failed(request_id, error_code)
    integer(int64), intent(in) :: request_id
    integer(int32), intent(in) :: error_code

    request_error(request_id) = error_code
    request_status(request_id) = REQUEST_FAILED
  end subroutine mark_request_failed
end module glamin_async
