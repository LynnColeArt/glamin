module glamin_gpu_backend
  use iso_fortran_env, only: int32
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: GpuCapabilities
  public :: GpuBackend

  type :: GpuCapabilities
    integer(int32) :: supports_l2 = 0
    integer(int32) :: supports_ip = 0
    integer(int32) :: supports_ivf = 0
    integer(int32) :: supports_pq = 0
  end type GpuCapabilities

  type, abstract :: GpuBackend
  contains
    procedure(gpu_init_iface), deferred :: init
    procedure(gpu_shutdown_iface), deferred :: shutdown
    procedure(gpu_caps_iface), deferred :: capabilities
    procedure(gpu_l2_iface), deferred :: distance_l2
    procedure(gpu_ip_iface), deferred :: distance_ip
  end type GpuBackend

  abstract interface
    subroutine gpu_init_iface(self, status)
      import :: GpuBackend, int32
      class(GpuBackend), intent(inout) :: self
      integer(int32), intent(out) :: status
    end subroutine gpu_init_iface

    subroutine gpu_shutdown_iface(self, status)
      import :: GpuBackend, int32
      class(GpuBackend), intent(inout) :: self
      integer(int32), intent(out) :: status
    end subroutine gpu_shutdown_iface

    subroutine gpu_caps_iface(self, caps)
      import :: GpuBackend, GpuCapabilities
      class(GpuBackend), intent(in) :: self
      type(GpuCapabilities), intent(out) :: caps
    end subroutine gpu_caps_iface

    subroutine gpu_l2_iface(self, queries, vectors, distances, status)
      import :: GpuBackend, VectorBlock, int32
      class(GpuBackend), intent(inout) :: self
      type(VectorBlock), intent(in) :: queries
      type(VectorBlock), intent(in) :: vectors
      type(VectorBlock), intent(out) :: distances
      integer(int32), intent(out) :: status
    end subroutine gpu_l2_iface

    subroutine gpu_ip_iface(self, queries, vectors, distances, status)
      import :: GpuBackend, VectorBlock, int32
      class(GpuBackend), intent(inout) :: self
      type(VectorBlock), intent(in) :: queries
      type(VectorBlock), intent(in) :: vectors
      type(VectorBlock), intent(out) :: distances
      integer(int32), intent(out) :: status
    end subroutine gpu_ip_iface
  end interface
end module glamin_gpu_backend
