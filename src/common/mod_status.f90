module glamin_status
  implicit none
  private

  public :: REQUEST_PENDING
  public :: REQUEST_RUNNING
  public :: REQUEST_COMPLETED
  public :: REQUEST_FAILED
  public :: REQUEST_CANCELLED

  enum, bind(c)
    enumerator :: REQUEST_PENDING = 0
    enumerator :: REQUEST_RUNNING = 1
    enumerator :: REQUEST_COMPLETED = 2
    enumerator :: REQUEST_FAILED = 3
    enumerator :: REQUEST_CANCELLED = 4
  end enum
end module glamin_status
