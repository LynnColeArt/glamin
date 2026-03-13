module glamin_pipeline_bridge
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_associated, c_f_pointer, c_int32_t, c_loc
  use glamin_async, only: wait_request, get_loaded_index
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_UNKNOWN
  use glamin_geometry_layout, only: load_vector_layout
  use glamin_index_flat, only: flat_destroy_handle, flat_search
  use glamin_memory, only: free_aligned
  use glamin_metrics, only: METRIC_L2
  use glamin_runtime, only: RuntimeContext, start_runtime, stop_runtime, submit_pipeline_async
  use glamin_status, only: REQUEST_COMPLETED
  use glamin_types, only: IndexHandle, Request, VectorBlock
  implicit none
  private

  public :: glamin_pipeline_demo_run

contains
  subroutine glamin_pipeline_demo_run(status) bind(c, name="glamin_pipeline_demo_run")
    integer(c_int32_t), intent(out) :: status
    type(RuntimeContext) :: runtime
    type(Request) :: request_handle
    type(IndexHandle) :: index
    type(VectorBlock) :: query_block
    type(VectorBlock) :: distances
    type(VectorBlock) :: labels
    real(real32), allocatable, target :: query(:)
    real(real32), pointer :: dist_ptr(:)
    integer(int32), pointer :: label_ptr(:)
    integer(int32) :: status_local
    integer(int32) :: free_status
    integer(int32) :: dim
    integer(int64) :: count
    integer(int64) :: offset_bytes
    logical :: runtime_ready
    logical :: index_ready

    status_local = GLAMIN_OK
    runtime_ready = .false.
    index_ready = .false.
    distances = VectorBlock()
    labels = VectorBlock()

    call start_runtime(runtime, 2_int32, status_local)
    if (status_local /= GLAMIN_OK) then
      call finalize_demo()
      status = int(status_local, c_int32_t)
      return
    end if
    runtime_ready = .true.

    call submit_pipeline_async(runtime, "docs/geometry_spec.yaml", "build/specs", &
      "geometry.auth", METRIC_L2, request_handle, status_local)
    if (status_local /= GLAMIN_OK) then
      call finalize_demo()
      status = int(status_local, c_int32_t)
      return
    end if

    call wait_request(request_handle, status_local)
    if (request_handle%status /= REQUEST_COMPLETED .or. request_handle%error_code /= GLAMIN_OK) then
      status_local = request_handle%error_code
      if (status_local == GLAMIN_OK) then
        status_local = GLAMIN_ERR_UNKNOWN
      end if
      call finalize_demo()
      status = int(status_local, c_int32_t)
      return
    end if

    call get_loaded_index(request_handle, index, status_local)
    if (status_local /= GLAMIN_OK) then
      call finalize_demo()
      status = int(status_local, c_int32_t)
      return
    end if
    index_ready = .true.

    call load_vector_layout("build/specs/vector_layout.json", "geometry.auth", dim, count, &
      offset_bytes, status_local)
    if (status_local /= GLAMIN_OK) then
      call finalize_demo()
      status = int(status_local, c_int32_t)
      return
    end if

    if (count <= 0_int64 .or. dim <= 0_int32) then
      status_local = GLAMIN_ERR_INVALID_ARG
      call finalize_demo()
      status = int(status_local, c_int32_t)
      return
    end if

    allocate(query(dim))
    query = 0.0_real32
    query(1) = 1.0_real32

    query_block%data = c_loc(query(1))
    query_block%length = 1
    query_block%dim = dim
    query_block%stride = dim
    query_block%elem_size = int(storage_size(0.0_real32) / 8, int32)
    query_block%alignment = 0

    call flat_search(index, query_block, 3_int32, distances, labels, status_local)
    if (status_local /= GLAMIN_OK) then
      call finalize_demo()
      status = int(status_local, c_int32_t)
      return
    end if

    call c_f_pointer(distances%data, dist_ptr, [3])
    call c_f_pointer(labels%data, label_ptr, [3])
    write(*, '(A, 3(F8.4,1X))') "Top distances: ", dist_ptr(1:3)
    write(*, '(A, 3(I8,1X))') "Top labels:    ", label_ptr(1:3)

    call finalize_demo()
    status = int(status_local, c_int32_t)

  contains
    subroutine finalize_demo()
      if (c_associated(distances%data)) then
        call free_aligned(distances%data, free_status)
      end if
      if (c_associated(labels%data)) then
        call free_aligned(labels%data, free_status)
      end if
      if (allocated(query)) then
        deallocate(query)
      end if
      if (index_ready) then
        call flat_destroy_handle(index, free_status)
      end if
      if (runtime_ready) then
        call stop_runtime(runtime, free_status)
      end if
    end subroutine finalize_demo
  end subroutine glamin_pipeline_demo_run
end module glamin_pipeline_bridge
