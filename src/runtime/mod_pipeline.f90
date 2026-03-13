module glamin_pipeline
  use iso_fortran_env, only: int32
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_NOT_READY
  implicit none
  private

  public :: PipelineCallbacks
  public :: set_pipeline_callbacks
  public :: clear_pipeline_callbacks
  public :: run_pipeline

  abstract interface
    subroutine pipeline_step_iface(spec_path, out_dir, status)
      import :: int32
      character(len=*), intent(in) :: spec_path
      character(len=*), intent(in) :: out_dir
      integer(int32), intent(out) :: status
    end subroutine pipeline_step_iface
  end interface

  type :: PipelineCallbacks
    procedure(pipeline_step_iface), pointer, nopass :: compile => null()
    procedure(pipeline_step_iface), pointer, nopass :: embed => null()
  end type PipelineCallbacks

  type(PipelineCallbacks), save :: callbacks = PipelineCallbacks()

contains
  subroutine set_pipeline_callbacks(new_callbacks)
    type(PipelineCallbacks), intent(in) :: new_callbacks

    callbacks = new_callbacks
  end subroutine set_pipeline_callbacks

  subroutine clear_pipeline_callbacks()
    callbacks = PipelineCallbacks()
  end subroutine clear_pipeline_callbacks

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
end module glamin_pipeline
