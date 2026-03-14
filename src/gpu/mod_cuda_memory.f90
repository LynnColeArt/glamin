module glamin_cuda_memory
  use iso_fortran_env, only: int32, int64
  use iso_c_binding, only: c_associated, c_int32_t, c_int64_t, c_null_ptr, c_ptr
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_NOT_READY
  implicit none
  private

  public :: CudaBuffer
  public :: cuda_buffer_allocate
  public :: cuda_buffer_release
  public :: cuda_buffer_upload
  public :: cuda_buffer_download
  public :: cuda_buffer_is_ready

  type :: CudaBuffer
    type(c_ptr) :: device_ptr = c_null_ptr
    integer(int64) :: bytes = 0
  end type CudaBuffer

  interface
    subroutine glamin_cuda_alloc(bytes, device_ptr, status) bind(c)
      import :: c_int64_t, c_ptr, c_int32_t
      integer(c_int64_t), value :: bytes
      type(c_ptr) :: device_ptr
      integer(c_int32_t) :: status
    end subroutine glamin_cuda_alloc

    subroutine glamin_cuda_free(device_ptr, status) bind(c)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: device_ptr
      integer(c_int32_t) :: status
    end subroutine glamin_cuda_free

    subroutine glamin_cuda_upload(host_ptr, device_ptr, bytes, status) bind(c)
      import :: c_ptr, c_int64_t, c_int32_t
      type(c_ptr), value :: host_ptr
      type(c_ptr), value :: device_ptr
      integer(c_int64_t), value :: bytes
      integer(c_int32_t) :: status
    end subroutine glamin_cuda_upload

    subroutine glamin_cuda_download(device_ptr, host_ptr, bytes, status) bind(c)
      import :: c_ptr, c_int64_t, c_int32_t
      type(c_ptr), value :: device_ptr
      type(c_ptr), value :: host_ptr
      integer(c_int64_t), value :: bytes
      integer(c_int32_t) :: status
    end subroutine glamin_cuda_download
  end interface

contains
  logical function cuda_buffer_is_ready(buffer)
    type(CudaBuffer), intent(in) :: buffer

    cuda_buffer_is_ready = c_associated(buffer%device_ptr)
  end function cuda_buffer_is_ready

  subroutine cuda_buffer_allocate(buffer, bytes, status)
    type(CudaBuffer), intent(inout) :: buffer
    integer(int64), intent(in) :: bytes
    integer(int32), intent(out) :: status
    integer(c_int32_t) :: status_c

    status = GLAMIN_OK
    if (bytes <= 0_int64) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    status_c = GLAMIN_OK
    call glamin_cuda_alloc(int(bytes, c_int64_t), buffer%device_ptr, status_c)
    status = int(status_c, int32)
    if (status == GLAMIN_OK) then
      buffer%bytes = bytes
    else
      buffer%device_ptr = c_null_ptr
      buffer%bytes = 0_int64
    end if
  end subroutine cuda_buffer_allocate

  subroutine cuda_buffer_release(buffer, status)
    type(CudaBuffer), intent(inout) :: buffer
    integer(int32), intent(out) :: status
    integer(c_int32_t) :: status_c

    status = GLAMIN_OK
    if (.not. c_associated(buffer%device_ptr)) then
      buffer%bytes = 0_int64
      return
    end if

    status_c = GLAMIN_OK
    call glamin_cuda_free(buffer%device_ptr, status_c)
    status = int(status_c, int32)
    buffer%device_ptr = c_null_ptr
    buffer%bytes = 0_int64
  end subroutine cuda_buffer_release

  subroutine cuda_buffer_upload(buffer, host_ptr, bytes, status)
    type(CudaBuffer), intent(in) :: buffer
    type(c_ptr), intent(in) :: host_ptr
    integer(int64), intent(in) :: bytes
    integer(int32), intent(out) :: status
    integer(c_int32_t) :: status_c

    status = GLAMIN_OK
    if (.not. c_associated(buffer%device_ptr) .or. .not. c_associated(host_ptr)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if
    if (bytes <= 0_int64 .or. bytes > buffer%bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    status_c = GLAMIN_OK
    call glamin_cuda_upload(host_ptr, buffer%device_ptr, int(bytes, c_int64_t), status_c)
    status = int(status_c, int32)
  end subroutine cuda_buffer_upload

  subroutine cuda_buffer_download(buffer, host_ptr, bytes, status)
    type(CudaBuffer), intent(in) :: buffer
    type(c_ptr), intent(in) :: host_ptr
    integer(int64), intent(in) :: bytes
    integer(int32), intent(out) :: status
    integer(c_int32_t) :: status_c

    status = GLAMIN_OK
    if (.not. c_associated(buffer%device_ptr) .or. .not. c_associated(host_ptr)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if
    if (bytes <= 0_int64 .or. bytes > buffer%bytes) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    status_c = GLAMIN_OK
    call glamin_cuda_download(buffer%device_ptr, host_ptr, int(bytes, c_int64_t), status_c)
    status = int(status_c, int32)
  end subroutine cuda_buffer_download
end module glamin_cuda_memory
