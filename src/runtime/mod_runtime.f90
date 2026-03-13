module glamin_runtime
  use iso_fortran_env, only: int32
  use glamin_errors, only: GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_NOT_READY, GLAMIN_OK
  use glamin_types, only: Request, VectorBlock, SearchPlan, IndexHandle
  use glamin_worker_pool, only: WorkerPool, start_pool, stop_pool
  use glamin_async, only: schedule_request, submit_add, submit_search, submit_train, submit_load_flat
  implicit none
  private

  public :: RuntimeContext
  public :: start_runtime
  public :: stop_runtime
  public :: submit_search_async
  public :: submit_add_async
  public :: submit_train_async
  public :: submit_load_flat_async

  type :: RuntimeContext
    type(WorkerPool) :: pool
    logical :: is_ready = .false.
  end type RuntimeContext

contains
  subroutine start_runtime(context, worker_count, status)
    type(RuntimeContext), intent(inout) :: context
    integer(int32), intent(in) :: worker_count
    integer(int32), intent(out) :: status

    if (worker_count <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call start_pool(context%pool, worker_count, status)
    if (status == GLAMIN_OK) then
      context%is_ready = .true.
    end if
  end subroutine start_runtime

  subroutine stop_runtime(context, status)
    type(RuntimeContext), intent(inout) :: context
    integer(int32), intent(out) :: status

    if (context%is_ready) then
      call stop_pool(context%pool, status)
    else
      status = GLAMIN_OK
    end if

    context%is_ready = .false.
  end subroutine stop_runtime

  subroutine submit_search_async(context, index, plan, queries, request_handle, status)
    type(RuntimeContext), intent(inout) :: context
    type(IndexHandle), intent(in) :: index
    type(SearchPlan), intent(in) :: plan
    type(VectorBlock), intent(in) :: queries
    type(Request), intent(out) :: request_handle
    integer(int32), intent(out) :: status

    if (.not. context%is_ready) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call submit_search(index, plan, queries, request_handle)
    call schedule_request(context%pool, request_handle, status)
  end subroutine submit_search_async

  subroutine submit_add_async(context, index, vectors, request_handle, status)
    type(RuntimeContext), intent(inout) :: context
    type(IndexHandle), intent(in) :: index
    type(VectorBlock), intent(in) :: vectors
    type(Request), intent(out) :: request_handle
    integer(int32), intent(out) :: status

    if (.not. context%is_ready) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call submit_add(index, vectors, request_handle)
    call schedule_request(context%pool, request_handle, status)
  end subroutine submit_add_async

  subroutine submit_train_async(context, index, vectors, request_handle, status)
    type(RuntimeContext), intent(inout) :: context
    type(IndexHandle), intent(in) :: index
    type(VectorBlock), intent(in) :: vectors
    type(Request), intent(out) :: request_handle
    integer(int32), intent(out) :: status

    if (.not. context%is_ready) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call submit_train(index, vectors, request_handle)
    call schedule_request(context%pool, request_handle, status)
  end subroutine submit_train_async

  subroutine submit_load_flat_async(context, layout_path, vectors_path, space_id, metric, &
      request_handle, status)
    type(RuntimeContext), intent(inout) :: context
    character(len=*), intent(in) :: layout_path
    character(len=*), intent(in) :: vectors_path
    character(len=*), intent(in) :: space_id
    integer(int32), intent(in) :: metric
    type(Request), intent(out) :: request_handle
    integer(int32), intent(out) :: status

    if (.not. context%is_ready) then
      status = GLAMIN_ERR_NOT_READY
      return
    end if

    call submit_load_flat(layout_path, vectors_path, space_id, metric, request_handle)
    call schedule_request(context%pool, request_handle, status)
  end subroutine submit_load_flat_async
end module glamin_runtime
