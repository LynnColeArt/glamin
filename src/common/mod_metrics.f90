module glamin_metrics
  implicit none
  private

  public :: METRIC_L2
  public :: METRIC_IP
  public :: METRIC_COSINE

  enum, bind(c)
    enumerator :: METRIC_L2 = 0
    enumerator :: METRIC_IP = 1
    enumerator :: METRIC_COSINE = 2
  end enum
end module glamin_metrics
