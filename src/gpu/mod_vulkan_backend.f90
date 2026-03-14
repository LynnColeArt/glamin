module glamin_vulkan_backend
  use iso_fortran_env, only: int32
  use glamin_distance, only: distance_ip_batch, distance_l2_batch
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_NOT_READY
  use glamin_gpu_backend, only: GpuBackend, GpuCapabilities
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: VulkanBackend

  type, extends(GpuBackend) :: VulkanBackend
    logical :: is_available = .false.
  contains
    procedure :: init => vulkan_backend_init
    procedure :: shutdown => vulkan_backend_shutdown
    procedure :: capabilities => vulkan_backend_capabilities
    procedure :: distance_l2 => vulkan_backend_distance_l2
    procedure :: distance_ip => vulkan_backend_distance_ip
  end type VulkanBackend

contains
  subroutine vulkan_backend_init(self, status)
    class(VulkanBackend), intent(inout) :: self
    integer(int32), intent(out) :: status
    logical :: enabled

    call read_env_flag('GLAMIN_VULKAN_AVAILABLE', enabled)
    self%is_available = enabled
    if (self%is_available) then
      status = GLAMIN_OK
    else
      status = GLAMIN_ERR_NOT_READY
    end if
  end subroutine vulkan_backend_init

  subroutine vulkan_backend_shutdown(self, status)
    class(VulkanBackend), intent(inout) :: self
    integer(int32), intent(out) :: status

    self%is_available = .false.
    status = GLAMIN_OK
  end subroutine vulkan_backend_shutdown

  subroutine vulkan_backend_capabilities(self, caps)
    class(VulkanBackend), intent(in) :: self
    type(GpuCapabilities), intent(out) :: caps

    caps = GpuCapabilities()
    if (self%is_available) then
      caps%supports_l2 = 1
      caps%supports_ip = 1
      caps%supports_ivf = 0
      caps%supports_pq = 0
    end if
  end subroutine vulkan_backend_capabilities

  subroutine vulkan_backend_distance_l2(self, queries, vectors, distances, status)
    class(VulkanBackend), intent(inout) :: self
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status

    if (.not. self%is_available) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if
    call distance_l2_batch(queries, vectors, distances, status)
  end subroutine vulkan_backend_distance_l2

  subroutine vulkan_backend_distance_ip(self, queries, vectors, distances, status)
    class(VulkanBackend), intent(inout) :: self
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status

    if (.not. self%is_available) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if
    call distance_ip_batch(queries, vectors, distances, status)
  end subroutine vulkan_backend_distance_ip

  subroutine read_env_flag(key, enabled)
    character(len=*), intent(in) :: key
    logical, intent(out) :: enabled
    character(len=8) :: value
    integer :: length
    integer :: env_status

    enabled = .false.
    value = ''
    call get_environment_variable(key, value, length, env_status, .true.)
    if (env_status /= 0 .or. length <= 0) return

    select case (value(1:1))
    case ('1', 't', 'T', 'y', 'Y', 'o', 'O')
      enabled = .true.
    case default
      enabled = .false.
    end select
  end subroutine read_env_flag
end module glamin_vulkan_backend
