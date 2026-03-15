module glamin_gpu_backend
  use iso_fortran_env, only: int32
  use glamin_distance, only: distance_ip_batch, distance_l2_batch
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: GpuCapabilities
  public :: GpuBackend
  public :: GPU_BACKEND_NAME_LEN
  public :: GPU_BACKEND_ORDER_LEN
  public :: gpu_set_backend
  public :: gpu_register_backend
  public :: gpu_select_backend
  public :: gpu_auto_select_backend
  public :: gpu_clear_backend
  public :: gpu_get_capabilities
  public :: gpu_has_backend
  public :: gpu_get_backend_name
  public :: gpu_distance_l2_dispatch
  public :: gpu_distance_ip_dispatch

  integer, parameter :: GPU_BACKEND_NAME_LEN = 32
  integer, parameter :: GPU_BACKEND_ORDER_LEN = 128
  integer, parameter :: MAX_GPU_BACKENDS = 8
  character(len=GPU_BACKEND_ORDER_LEN), parameter :: DEFAULT_BACKEND_ORDER = 'cuda,vulkan'

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

  type :: GpuBackendEntry
    character(len=GPU_BACKEND_NAME_LEN) :: name = ''
    class(GpuBackend), pointer :: backend => null()
  end type GpuBackendEntry

  class(GpuBackend), pointer :: active_backend => null()
  character(len=GPU_BACKEND_NAME_LEN) :: active_backend_name = ''
  type(GpuBackendEntry) :: backend_registry(MAX_GPU_BACKENDS)
  integer :: backend_registry_count = 0

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
      type(VectorBlock), intent(inout) :: distances
      integer(int32), intent(out) :: status
    end subroutine gpu_l2_iface

    subroutine gpu_ip_iface(self, queries, vectors, distances, status)
      import :: GpuBackend, VectorBlock, int32
      class(GpuBackend), intent(inout) :: self
      type(VectorBlock), intent(in) :: queries
      type(VectorBlock), intent(in) :: vectors
      type(VectorBlock), intent(inout) :: distances
      integer(int32), intent(out) :: status
    end subroutine gpu_ip_iface
  end interface

contains
  subroutine gpu_set_backend(backend, status, name)
    class(GpuBackend), target, intent(inout) :: backend
    integer(int32), intent(out) :: status
    character(len=*), intent(in), optional :: name
    class(GpuBackend), pointer :: backend_ptr

    backend_ptr => backend
    call gpu_set_backend_ptr(backend_ptr, status, name)
  end subroutine gpu_set_backend

  subroutine gpu_register_backend(name, backend, status)
    character(len=*), intent(in) :: name
    class(GpuBackend), target, intent(inout) :: backend
    integer(int32), intent(out) :: status
    character(len=GPU_BACKEND_NAME_LEN) :: normalized
    integer :: idx

    status = GLAMIN_OK
    normalized = normalize_backend_name(name)
    if (len_trim(normalized) == 0) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    do idx = 1, backend_registry_count
      if (trim(backend_registry(idx)%name) == trim(normalized)) then
        backend_registry(idx)%backend => backend
        return
      end if
    end do

    if (backend_registry_count >= MAX_GPU_BACKENDS) then
      status = GLAMIN_ERR_OOM
      return
    end if

    backend_registry_count = backend_registry_count + 1
    backend_registry(backend_registry_count)%name = normalized
    backend_registry(backend_registry_count)%backend => backend
  end subroutine gpu_register_backend

  subroutine gpu_select_backend(name, status)
    character(len=*), intent(in) :: name
    integer(int32), intent(out) :: status
    character(len=GPU_BACKEND_NAME_LEN) :: normalized
    integer :: idx

    normalized = normalize_backend_name(name)
    if (len_trim(normalized) == 0) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (trim(normalized) == 'cpu') then
      call gpu_clear_backend(status)
      return
    end if

    idx = find_backend_index(normalized)
    if (idx == 0) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call gpu_set_backend_ptr(backend_registry(idx)%backend, status, normalized)
  end subroutine gpu_select_backend

  subroutine gpu_auto_select_backend(status)
    integer(int32), intent(out) :: status
    character(len=GPU_BACKEND_NAME_LEN) :: requested
    character(len=GPU_BACKEND_ORDER_LEN) :: order
    integer(int32) :: local_status
    integer :: position

    call read_env_value('GLAMIN_GPU_BACKEND', requested)
    requested = normalize_backend_name(requested)

    call gpu_clear_backend(local_status)

    if (len_trim(requested) == 0 .or. trim(requested) == 'auto') then
      call read_env_value('GLAMIN_GPU_BACKEND_ORDER', order)
      if (len_trim(order) == 0) order = DEFAULT_BACKEND_ORDER
      position = 1
      do while (position <= len_trim(order))
        call next_backend_token(order, position, requested)
        if (len_trim(requested) == 0) cycle
        call gpu_select_backend(requested, local_status)
        if (local_status == GLAMIN_OK .and. gpu_has_backend()) then
          status = GLAMIN_OK
          return
        end if
      end do
      call gpu_clear_backend(local_status)
      status = GLAMIN_OK
      return
    end if

    call gpu_select_backend(requested, status)
  end subroutine gpu_auto_select_backend

  subroutine gpu_clear_backend(status)
    integer(int32), intent(out) :: status
    integer(int32) :: local_status

    status = GLAMIN_OK
    if (associated(active_backend)) then
      call active_backend%shutdown(local_status)
    end if
    nullify(active_backend)
    active_backend_name = ''
  end subroutine gpu_clear_backend

  subroutine gpu_get_capabilities(caps, status)
    type(GpuCapabilities), intent(out) :: caps
    integer(int32), intent(out) :: status

    status = GLAMIN_OK
    caps = GpuCapabilities()
    if (associated(active_backend)) then
      call active_backend%capabilities(caps)
    end if
  end subroutine gpu_get_capabilities

  logical function gpu_has_backend()
    gpu_has_backend = associated(active_backend)
  end function gpu_has_backend

  subroutine gpu_get_backend_name(name)
    character(len=*), intent(out) :: name

    if (len_trim(active_backend_name) == 0) then
      name = 'cpu'
    else
      name = active_backend_name
    end if
  end subroutine gpu_get_backend_name

  subroutine gpu_distance_l2_dispatch(queries, vectors, distances, status)
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status
    type(GpuCapabilities) :: caps

    if (associated(active_backend)) then
      call active_backend%capabilities(caps)
      if (caps%supports_l2 /= 0_int32) then
        call active_backend%distance_l2(queries, vectors, distances, status)
        if (status == GLAMIN_OK) then
          return
        end if
      end if
    end if

    call distance_l2_batch(queries, vectors, distances, status)
  end subroutine gpu_distance_l2_dispatch

  subroutine gpu_distance_ip_dispatch(queries, vectors, distances, status)
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status
    type(GpuCapabilities) :: caps

    if (associated(active_backend)) then
      call active_backend%capabilities(caps)
      if (caps%supports_ip /= 0_int32) then
        call active_backend%distance_ip(queries, vectors, distances, status)
        if (status == GLAMIN_OK) then
          return
        end if
      end if
    end if

    call distance_ip_batch(queries, vectors, distances, status)
  end subroutine gpu_distance_ip_dispatch

  subroutine gpu_set_backend_ptr(backend, status, name)
    class(GpuBackend), pointer, intent(inout) :: backend
    integer(int32), intent(out) :: status
    character(len=*), intent(in), optional :: name
    integer(int32) :: local_status

    status = GLAMIN_OK
    if (associated(active_backend)) then
      call active_backend%shutdown(local_status)
    end if

    active_backend => backend
    call active_backend%init(status)
    if (status /= GLAMIN_OK) then
      call active_backend%shutdown(local_status)
      nullify(active_backend)
      active_backend_name = ''
      return
    end if

    if (present(name)) then
      active_backend_name = normalize_backend_name(name)
    else
      active_backend_name = ''
    end if
  end subroutine gpu_set_backend_ptr

  pure function normalize_backend_name(value) result(normalized)
    character(len=*), intent(in) :: value
    character(len=GPU_BACKEND_NAME_LEN) :: normalized
    integer :: idx
    integer :: code

    normalized = ''
    do idx = 1, min(len_trim(value), GPU_BACKEND_NAME_LEN)
      code = iachar(value(idx:idx))
      if (code >= iachar('A') .and. code <= iachar('Z')) then
        normalized(idx:idx) = achar(code + 32)
      else
        normalized(idx:idx) = value(idx:idx)
      end if
    end do
    normalized = adjustl(normalized)
  end function normalize_backend_name

  subroutine read_env_value(key, value)
    character(len=*), intent(in) :: key
    character(len=*), intent(out) :: value
    integer :: length
    integer :: env_status

    value = ''
    call get_environment_variable(key, value, length, env_status, .true.)
    if (env_status /= 0 .or. length <= 0) then
      value = ''
    else if (length < len(value)) then
      value = value(1:length)
    end if
  end subroutine read_env_value

  subroutine next_backend_token(value, position, token)
    character(len=*), intent(in) :: value
    integer, intent(inout) :: position
    character(len=GPU_BACKEND_NAME_LEN), intent(out) :: token
    integer :: finish
    integer :: length

    token = ''
    length = len_trim(value)
    if (position > length) return

    do while (position <= length)
      if (value(position:position) /= ',' .and. value(position:position) /= ' ') exit
      position = position + 1
    end do

    if (position > length) return

    finish = position
    do while (finish <= length)
      if (value(finish:finish) == ',') exit
      finish = finish + 1
    end do

    if (finish > position) token = adjustl(value(position:finish - 1))
    token = normalize_backend_name(token)
    position = finish + 1
  end subroutine next_backend_token

  integer function find_backend_index(name)
    character(len=*), intent(in) :: name
    integer :: idx

    find_backend_index = 0
    do idx = 1, backend_registry_count
      if (trim(backend_registry(idx)%name) == trim(name)) then
        find_backend_index = idx
        exit
      end if
    end do
  end function find_backend_index
end module glamin_gpu_backend
