module glamin_queue
  use iso_fortran_env, only: int32
  use iso_c_binding, only: c_ptr, c_null_ptr
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM, GLAMIN_ERR_NOT_READY
  implicit none
  private

  public :: JobQueue
  public :: init_queue
  public :: destroy_queue
  public :: enqueue_job
  public :: dequeue_job

  type :: JobQueue
    type(c_ptr), allocatable :: jobs(:)
    integer(int32) :: capacity = 0
    integer(int32) :: head = 1
    integer(int32) :: tail = 1
    integer(int32) :: count = 0
  end type JobQueue

contains
  subroutine init_queue(queue, capacity, status)
    type(JobQueue), intent(out) :: queue
    integer(int32), intent(in) :: capacity
    integer(int32), intent(out) :: status
    integer(int32) :: alloc_status

    if (capacity <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    allocate(queue%jobs(capacity), stat=alloc_status)
    if (alloc_status /= 0_int32) then
      status = GLAMIN_ERR_OOM
      return
    end if

    queue%jobs = c_null_ptr
    queue%capacity = capacity
    queue%head = 1
    queue%tail = 1
    queue%count = 0
    status = GLAMIN_OK
  end subroutine init_queue

  subroutine destroy_queue(queue, status)
    type(JobQueue), intent(inout) :: queue
    integer(int32), intent(out) :: status

    if (allocated(queue%jobs)) then
      deallocate(queue%jobs)
    end if

    queue%capacity = 0
    queue%head = 1
    queue%tail = 1
    queue%count = 0
    status = GLAMIN_OK
  end subroutine destroy_queue

  subroutine enqueue_job(queue, job, status)
    type(JobQueue), intent(inout) :: queue
    type(c_ptr), intent(in) :: job
    integer(int32), intent(out) :: status

    if (.not. allocated(queue%jobs)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    if (queue%count >= queue%capacity) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    queue%jobs(queue%tail) = job
    queue%tail = queue%tail + 1
    if (queue%tail > queue%capacity) then
      queue%tail = 1
    end if
    queue%count = queue%count + 1
    status = GLAMIN_OK
  end subroutine enqueue_job

  subroutine dequeue_job(queue, job, status)
    type(JobQueue), intent(inout) :: queue
    type(c_ptr), intent(out) :: job
    integer(int32), intent(out) :: status

    job = c_null_ptr
    if (.not. allocated(queue%jobs)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    if (queue%count == 0_int32) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    job = queue%jobs(queue%head)
    queue%jobs(queue%head) = c_null_ptr
    queue%head = queue%head + 1
    if (queue%head > queue%capacity) then
      queue%head = 1
    end if
    queue%count = queue%count - 1
    status = GLAMIN_OK
  end subroutine dequeue_job
end module glamin_queue
