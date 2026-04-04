program async_geometry_pipeline_demo
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_loc
  use glamin_async, only: wait_request, get_loaded_index
  use glamin_errors, only: GLAMIN_OK
  use glamin_geometry_layout, only: load_vector_layout
  use glamin_geometry_spec_compiler, only: compile_geometry_spec, embed_geometry_spec
  use glamin_index_flat, only: flat_destroy_handle, flat_search
  use glamin_metrics, only: METRIC_L2
  use glamin_pipeline, only: PipelineCallbacks, set_pipeline_callbacks
  use glamin_runtime, only: RuntimeContext, start_runtime, stop_runtime, &
    submit_pipeline_async
  use glamin_status, only: REQUEST_COMPLETED
  use glamin_types, only: IndexHandle, Request, VectorBlock
  implicit none

  type(RuntimeContext) :: runtime
  type(Request) :: request_handle
  type(IndexHandle) :: index
  type(VectorBlock) :: query_block
  type(VectorBlock) :: distances
  type(VectorBlock) :: labels
  real(real32), allocatable, target :: query(:)
  real(real32), pointer :: dist_ptr(:)
  integer(int32), pointer :: label_ptr(:)
  integer(int32) :: status
  integer(int32) :: dim
  integer(int64) :: count
  integer(int64) :: offset_bytes

  call register_pipeline_hooks(status)
  if (status /= GLAMIN_OK) then
    error stop "register_pipeline_hooks failed"
  end if

  call start_runtime(runtime, 2_int32, status)
  if (status /= GLAMIN_OK) then
    error stop "start_runtime failed"
  end if

  call submit_pipeline_async(runtime, "docs/geometry_spec.yaml", "build/specs", &
    "geometry.auth", METRIC_L2, request_handle, status)
  if (status /= GLAMIN_OK) then
    call stop_runtime(runtime, status)
    error stop "submit_pipeline_async failed"
  end if

  call wait_request(request_handle, status)
  if (request_handle%status /= REQUEST_COMPLETED .or. request_handle%error_code /= GLAMIN_OK) then
    call stop_runtime(runtime, status)
    error stop "pipeline request failed"
  end if

  call get_loaded_index(request_handle, index, status)
  if (status /= GLAMIN_OK) then
    call stop_runtime(runtime, status)
    error stop "get_loaded_index failed"
  end if

  call load_vector_layout("build/specs/vector_layout.json", "geometry.auth", dim, count, &
    offset_bytes, status)
  if (status /= GLAMIN_OK) then
    call flat_destroy_handle(index, status)
    call stop_runtime(runtime, status)
    error stop "load_vector_layout failed"
  end if

  allocate(query(dim))
  query = 0.0_real32
  if (dim > 0) query(1) = 1.0_real32

  query_block%data = c_loc(query(1))
  query_block%length = 1
  query_block%dim = dim
  query_block%stride = dim
  query_block%elem_size = int(storage_size(0.0_real32) / 8, int32)
  query_block%alignment = 0

  distances = VectorBlock()
  labels = VectorBlock()
  call flat_search(index, query_block, 3_int32, distances, labels, status)
  if (status /= GLAMIN_OK) then
    call flat_destroy_handle(index, status)
    call stop_runtime(runtime, status)
    error stop "flat_search failed"
  end if

  call c_f_pointer(distances%data, dist_ptr, [3])
  call c_f_pointer(labels%data, label_ptr, [3])

  write(*, '(A, 3(F8.4,1X))') "Top distances: ", dist_ptr(1:3)
  write(*, '(A, 3(I8,1X))') "Top labels:    ", label_ptr(1:3)

  call flat_destroy_handle(index, status)
  call stop_runtime(runtime, status)
  deallocate(query)

contains
  subroutine register_pipeline_hooks(status)
    integer(int32), intent(out) :: status
    type(PipelineCallbacks) :: callbacks

    callbacks%compile => pipeline_compile
    callbacks%embed => pipeline_embed
    call set_pipeline_callbacks(callbacks)
    status = GLAMIN_OK
  end subroutine register_pipeline_hooks

  subroutine pipeline_compile(spec_path, out_dir, status)
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: out_dir
    integer(int32), intent(out) :: status

    call compile_geometry_spec(spec_path, out_dir, status)
  end subroutine pipeline_compile

  subroutine pipeline_embed(spec_path, out_dir, status)
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: out_dir
    integer(int32), intent(out) :: status

    call embed_geometry_spec(spec_path, out_dir, status)
  end subroutine pipeline_embed
end program async_geometry_pipeline_demo
