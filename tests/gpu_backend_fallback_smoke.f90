module glamin_gpu_backend_fallback_helpers
  use iso_fortran_env, only: int32
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_NOT_READY
  use glamin_gpu_backend, only: GpuBackend, GpuCapabilities
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: FailBackend

  type, extends(GpuBackend) :: FailBackend
    logical :: is_ready = .false.
  contains
    procedure :: init => fail_backend_init
    procedure :: shutdown => fail_backend_shutdown
    procedure :: capabilities => fail_backend_caps
    procedure :: distance_l2 => fail_backend_l2
    procedure :: distance_ip => fail_backend_ip
  end type FailBackend

contains
  subroutine fail_backend_init(self, status)
    class(FailBackend), intent(inout) :: self
    integer(int32), intent(out) :: status

    self%is_ready = .true.
    status = GLAMIN_OK
  end subroutine fail_backend_init

  subroutine fail_backend_shutdown(self, status)
    class(FailBackend), intent(inout) :: self
    integer(int32), intent(out) :: status

    self%is_ready = .false.
    status = GLAMIN_OK
  end subroutine fail_backend_shutdown

  subroutine fail_backend_caps(self, caps)
    class(FailBackend), intent(in) :: self
    type(GpuCapabilities), intent(out) :: caps

    caps = GpuCapabilities()
    if (self%is_ready) then
      caps%supports_l2 = 1
      caps%supports_ip = 1
    end if
  end subroutine fail_backend_caps

  subroutine fail_backend_l2(self, queries, vectors, distances, status)
    class(FailBackend), intent(inout) :: self
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status
    integer(int32) :: ignore

    if (.false.) then
      ignore = queries%dim + vectors%dim + distances%dim + merge(1_int32, 0_int32, self%is_ready)
    end if
    status = GLAMIN_ERR_NOT_READY
  end subroutine fail_backend_l2

  subroutine fail_backend_ip(self, queries, vectors, distances, status)
    class(FailBackend), intent(inout) :: self
    type(VectorBlock), intent(in) :: queries
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(inout) :: distances
    integer(int32), intent(out) :: status
    integer(int32) :: ignore

    if (.false.) then
      ignore = queries%dim + vectors%dim + distances%dim + merge(1_int32, 0_int32, self%is_ready)
    end if
    status = GLAMIN_ERR_NOT_READY
  end subroutine fail_backend_ip
end module glamin_gpu_backend_fallback_helpers

program glamin_gpu_backend_fallback_smoke
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_loc
  use glamin_errors, only: GLAMIN_OK
  use glamin_gpu_backend, only: gpu_clear_backend, gpu_distance_ip_dispatch, &
    gpu_distance_l2_dispatch, gpu_set_backend
  use glamin_memory, only: free_aligned
  use glamin_types, only: VectorBlock
  use glamin_gpu_backend_fallback_helpers, only: FailBackend
  implicit none

  type(FailBackend) :: backend
  real(real32), target :: vector_data(6)
  real(real32), target :: query_data(4)
  type(VectorBlock) :: vectors
  type(VectorBlock) :: queries
  type(VectorBlock) :: distances
  real(real32), pointer :: distance_ptr(:)
  integer(int32) :: elem_bytes
  integer(int32) :: status
  integer(int32) :: free_status

  elem_bytes = int(storage_size(0.0_real32) / 8, int32)

  vector_data = [0.0_real32, 0.0_real32, 1.0_real32, 0.0_real32, 0.0_real32, 1.0_real32]
  query_data = [0.0_real32, 0.0_real32, 1.0_real32, 1.0_real32]

  vectors = VectorBlock()
  vectors%data = c_loc(vector_data(1))
  vectors%length = 3_int64
  vectors%dim = 2
  vectors%stride = 2
  vectors%elem_size = elem_bytes

  queries = VectorBlock()
  queries%data = c_loc(query_data(1))
  queries%length = 2_int64
  queries%dim = 2
  queries%stride = 2
  queries%elem_size = elem_bytes

  call gpu_set_backend(backend, status, 'fail')
  if (status /= GLAMIN_OK) error stop "gpu_set_backend failed"

  distances = VectorBlock()
  call gpu_distance_l2_dispatch(queries, vectors, distances, status)
  if (status /= GLAMIN_OK) error stop "gpu_distance_l2_dispatch failed"

  call c_f_pointer(distances%data, distance_ptr, [6])
  if (any(abs(distance_ptr - [0.0_real32, 1.0_real32, 1.0_real32, &
      2.0_real32, 1.0_real32, 1.0_real32]) > 1.0e-6_real32)) then
    error stop "fallback L2 mismatch"
  end if

  call free_aligned(distances%data, free_status)

  distances = VectorBlock()
  call gpu_distance_ip_dispatch(queries, vectors, distances, status)
  if (status /= GLAMIN_OK) error stop "gpu_distance_ip_dispatch failed"

  call c_f_pointer(distances%data, distance_ptr, [6])
  if (any(abs(distance_ptr - [0.0_real32, 0.0_real32, 0.0_real32, &
      0.0_real32, 1.0_real32, 1.0_real32]) > 1.0e-6_real32)) then
    error stop "fallback IP mismatch"
  end if

  call free_aligned(distances%data, free_status)
  call gpu_clear_backend(status)

  write (*, '(a)') 'gpu backend fallback ok'
end program glamin_gpu_backend_fallback_smoke
