module glamin_worker_pool
  use iso_fortran_env, only: int32, int64
  use iso_c_binding, only: c_ptr, c_null_ptr, c_funptr, c_associated, c_funloc, &
    c_int32_t, c_int64_t
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_NOT_READY, GLAMIN_ERR_OOM
  implicit none
  private

  public :: WorkerPool
  public :: JobCallback
  public :: start_pool
  public :: stop_pool
  public :: submit_job
  public :: submit_request_job
  public :: submit_request_job_with_callback

  integer(int32), parameter :: DEFAULT_QUEUE_MULTIPLIER = 8
  integer(int32), parameter :: MIN_QUEUE_CAPACITY = 64

  type :: WorkerPool
    type(c_ptr) :: impl = c_null_ptr
    integer(int32) :: size = 0
    logical :: is_running = .false.
  end type WorkerPool

  abstract interface
    subroutine JobCallback(context) bind(c)
      import :: c_ptr
      type(c_ptr), value :: context
    end subroutine JobCallback
  end interface

  interface
    function glamin_thread_pool_create(thread_count, capacity) &
      bind(c, name="glamin_thread_pool_create") result(handle)
      import :: c_int32_t, c_ptr
      integer(c_int32_t), value :: thread_count
      integer(c_int32_t), value :: capacity
      type(c_ptr) :: handle
    end function glamin_thread_pool_create

    subroutine glamin_thread_pool_destroy(handle) bind(c, name="glamin_thread_pool_destroy")
      import :: c_ptr
      type(c_ptr), value :: handle
    end subroutine glamin_thread_pool_destroy

    function glamin_thread_pool_submit(handle, callback, context) &
      bind(c, name="glamin_thread_pool_submit") result(status)
      import :: c_ptr, c_funptr, c_int32_t
      type(c_ptr), value :: handle
      type(c_funptr), value :: callback
      type(c_ptr), value :: context
      integer(c_int32_t) :: status
    end function glamin_thread_pool_submit

    function glamin_thread_pool_submit_request(handle, request_id) &
      bind(c, name="glamin_thread_pool_submit_request") result(status)
      import :: c_ptr, c_int32_t, c_int64_t
      type(c_ptr), value :: handle
      integer(c_int64_t), value :: request_id
      integer(c_int32_t) :: status
    end function glamin_thread_pool_submit_request

    function glamin_thread_pool_submit_request_with_job(handle, request_id, callback, context) &
      bind(c, name="glamin_thread_pool_submit_request_with_job") result(status)
      import :: c_ptr, c_funptr, c_int32_t, c_int64_t
      type(c_ptr), value :: handle
      integer(c_int64_t), value :: request_id
      type(c_funptr), value :: callback
      type(c_ptr), value :: context
      integer(c_int32_t) :: status
    end function glamin_thread_pool_submit_request_with_job
  end interface

contains
  subroutine start_pool(pool, size, status)
    type(WorkerPool), intent(out) :: pool
    integer(int32), intent(in) :: size
    integer(int32), intent(out) :: status
    integer(int32) :: capacity

    if (size <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    capacity = max(MIN_QUEUE_CAPACITY, size * DEFAULT_QUEUE_MULTIPLIER)
    pool%impl = glamin_thread_pool_create(size, capacity)
    if (.not. c_associated(pool%impl)) then
      status = GLAMIN_ERR_OOM
      return
    end if
    pool%size = size
    pool%is_running = .true.
    status = GLAMIN_OK
  end subroutine start_pool

  subroutine stop_pool(pool, status)
    type(WorkerPool), intent(inout) :: pool
    integer(int32), intent(out) :: status

    if (pool%is_running .and. c_associated(pool%impl)) then
      call glamin_thread_pool_destroy(pool%impl)
    end if

    pool%impl = c_null_ptr
    pool%size = 0
    pool%is_running = .false.
    status = GLAMIN_OK
  end subroutine stop_pool

  subroutine submit_job(pool, job_callback, context, status)
    type(WorkerPool), intent(inout) :: pool
    procedure(JobCallback) :: job_callback
    type(c_ptr), intent(in) :: context
    integer(int32), intent(out) :: status
    type(c_funptr) :: callback_ptr

    if (.not. pool%is_running .or. .not. c_associated(pool%impl)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    callback_ptr = c_funloc(job_callback)
    status = glamin_thread_pool_submit(pool%impl, callback_ptr, context)
  end subroutine submit_job

  subroutine submit_request_job(pool, request_id, status)
    type(WorkerPool), intent(inout) :: pool
    integer(int64), intent(in) :: request_id
    integer(int32), intent(out) :: status

    if (.not. pool%is_running .or. .not. c_associated(pool%impl)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    status = glamin_thread_pool_submit_request(pool%impl, request_id)
  end subroutine submit_request_job

  subroutine submit_request_job_with_callback(pool, request_id, job_callback, context, status)
    type(WorkerPool), intent(inout) :: pool
    integer(int64), intent(in) :: request_id
    procedure(JobCallback) :: job_callback
    type(c_ptr), intent(in) :: context
    integer(int32), intent(out) :: status
    type(c_funptr) :: callback_ptr

    if (.not. pool%is_running .or. .not. c_associated(pool%impl)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    callback_ptr = c_funloc(job_callback)
    status = glamin_thread_pool_submit_request_with_job(pool%impl, request_id, callback_ptr, context)
  end subroutine submit_request_job_with_callback
end module glamin_worker_pool
