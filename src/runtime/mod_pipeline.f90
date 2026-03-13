module glamin_pipeline
  use iso_fortran_env, only: int32
  use iso_c_binding, only: c_associated, c_char, c_f_procpointer, c_funptr, c_int32_t, &
    c_loc, c_null_char, c_null_funptr, c_ptr
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_NOT_READY
  implicit none
  private

  public :: PipelineCallbacks
  public :: set_pipeline_callbacks
  public :: clear_pipeline_callbacks
  public :: run_pipeline
  public :: set_pipeline_callbacks_c
  public :: clear_pipeline_callbacks_c

  abstract interface
    subroutine pipeline_step_iface(spec_path, out_dir, status)
      import :: int32
      character(len=*), intent(in) :: spec_path
      character(len=*), intent(in) :: out_dir
      integer(int32), intent(out) :: status
    end subroutine pipeline_step_iface
  end interface

  abstract interface
    subroutine pipeline_step_c(spec_path, out_dir, status) bind(c)
      import :: c_ptr, c_int32_t
      type(c_ptr), value :: spec_path
      type(c_ptr), value :: out_dir
      integer(c_int32_t) :: status
    end subroutine pipeline_step_c
  end interface

  type :: PipelineCallbacks
    procedure(pipeline_step_iface), pointer, nopass :: compile => null()
    procedure(pipeline_step_iface), pointer, nopass :: embed => null()
  end type PipelineCallbacks

  type(PipelineCallbacks), save :: callbacks = PipelineCallbacks()
  type(c_funptr), save :: compile_callback_c = c_null_funptr
  type(c_funptr), save :: embed_callback_c = c_null_funptr

contains
  subroutine set_pipeline_callbacks(new_callbacks)
    type(PipelineCallbacks), intent(in) :: new_callbacks

    callbacks = new_callbacks
  end subroutine set_pipeline_callbacks

  subroutine clear_pipeline_callbacks()
    callbacks = PipelineCallbacks()
  end subroutine clear_pipeline_callbacks

  subroutine set_pipeline_callbacks_c(compile_cb, embed_cb) bind(c, name="glamin_set_pipeline_callbacks")
    type(c_funptr), value :: compile_cb
    type(c_funptr), value :: embed_cb
    type(PipelineCallbacks) :: new_callbacks

    compile_callback_c = compile_cb
    embed_callback_c = embed_cb

    new_callbacks%compile => pipeline_compile_bridge
    new_callbacks%embed => pipeline_embed_bridge
    call set_pipeline_callbacks(new_callbacks)
  end subroutine set_pipeline_callbacks_c

  subroutine clear_pipeline_callbacks_c() bind(c, name="glamin_clear_pipeline_callbacks")
    compile_callback_c = c_null_funptr
    embed_callback_c = c_null_funptr
    call clear_pipeline_callbacks()
  end subroutine clear_pipeline_callbacks_c

  subroutine run_pipeline(spec_path, out_dir, status)
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: out_dir
    integer(int32), intent(out) :: status

    if (len_trim(spec_path) == 0) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (.not. associated(callbacks%compile) .or. .not. associated(callbacks%embed)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call callbacks%compile(spec_path, out_dir, status)
    if (status /= GLAMIN_OK) then
      return
    end if

    call callbacks%embed(spec_path, out_dir, status)
  end subroutine run_pipeline

  subroutine pipeline_compile_bridge(spec_path, out_dir, status)
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: out_dir
    integer(int32), intent(out) :: status

    call dispatch_c_callback(compile_callback_c, spec_path, out_dir, status)
  end subroutine pipeline_compile_bridge

  subroutine pipeline_embed_bridge(spec_path, out_dir, status)
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: out_dir
    integer(int32), intent(out) :: status

    call dispatch_c_callback(embed_callback_c, spec_path, out_dir, status)
  end subroutine pipeline_embed_bridge

  subroutine dispatch_c_callback(callback, spec_path, out_dir, status)
    type(c_funptr), intent(in) :: callback
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: out_dir
    integer(int32), intent(out) :: status
    procedure(pipeline_step_c), pointer :: cb
    character(kind=c_char), allocatable, target :: spec_c(:)
    character(kind=c_char), allocatable, target :: out_c(:)
    integer(c_int32_t) :: c_status

    if (.not. c_associated(callback)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call c_f_procpointer(callback, cb)
    if (.not. associated(cb)) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call to_c_string(spec_path, spec_c)
    call to_c_string(out_dir, out_c)

    c_status = 0_c_int32_t
    call cb(c_loc(spec_c(1)), c_loc(out_c(1)), c_status)
    status = int(c_status, int32)
  end subroutine dispatch_c_callback

  subroutine to_c_string(input, output)
    character(len=*), intent(in) :: input
    character(kind=c_char), allocatable, intent(out) :: output(:)
    integer :: length
    integer :: idx

    length = len_trim(input)
    allocate(output(length + 1))
    do idx = 1, length
      output(idx) = achar(iachar(input(idx:idx)), kind=c_char)
    end do
    output(length + 1) = c_null_char
  end subroutine to_c_string
end module glamin_pipeline
