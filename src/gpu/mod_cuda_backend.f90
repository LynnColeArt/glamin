module glamin_cuda_backend
  use iso_fortran_env, only: int32
  use glamin_cuda_kernels, only: cuda_kernels_available, cuda_distance_ip, cuda_distance_l2
  use glamin_distance, only: distance_ip_batch, distance_l2_batch
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_NOT_READY
  use glamin_gpu_backend, only: GpuBackend, GpuCapabilities
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: CudaBackend

  type, extends(GpuBackend) :: CudaBackend
    logical :: is_available = .false.
  contains
    procedure :: init => cuda_backend_init
    procedure :: shutdown => cuda_backend_shutdown
    procedure :: capabilities => cuda_backend_capabilities
    procedure :: distance_l2 => cuda_backend_distance_l2
    procedure :: distance_ip => cuda_backend_distance_ip
  end type CudaBackend

contains
  subroutine cuda_backend_init(self, status)
    class(CudaBackend), intent(inout) :: self
    integer(int32), intent(out) :: status
    self%is_available = cuda_kernels_available()
    if (self%is_available) then
      status = GLAMIN_OK
    else
      status = GLAMIN_ERR_NOT_READY
    end if
  end subroutine cuda_backend_init

  subroutine cuda_backend_shutdown(self, status)
    class(CudaBackend), intent(inout) :: self
    integer(int32), intent(out) :: status

    self%is_available = .false.
    status = GLAMIN_OK
  end subroutine cuda_backend_shutdown

  subroutine cuda_backend_capabilities(self, caps)
    class(CudaBackend), intent(in) :: self
    type(GpuCapabilities), intent(out) :: caps

    caps = GpuCapabilities()
    if (self%is_available) then
      caps%supports_l2 = 1
      caps%supports_ip = 1
      caps%supports_ivf = 0
      caps%supports_pq = 0
    end if
  end subroutine cuda_backend_capabilities

  subroutine cuda_backend_distance_l2(self, queries, vectors, distances, status)
    class(CudaBackend), intent(inout) :: self
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status

    if (.not. self%is_available) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if
    call cuda_distance_l2(queries, vectors, distances, status)
    if (status /= GLAMIN_OK) then
      call distance_l2_batch(queries, vectors, distances, status)
    end if
  end subroutine cuda_backend_distance_l2

  subroutine cuda_backend_distance_ip(self, queries, vectors, distances, status)
    class(CudaBackend), intent(inout) :: self
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status

    if (.not. self%is_available) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if
    call cuda_distance_ip(queries, vectors, distances, status)
    if (status /= GLAMIN_OK) then
      call distance_ip_batch(queries, vectors, distances, status)
    end if
  end subroutine cuda_backend_distance_ip
end module glamin_cuda_backend
