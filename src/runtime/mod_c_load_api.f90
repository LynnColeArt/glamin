module glamin_c_load_api
  use iso_fortran_env, only: int32, int64
  use iso_c_binding, only: c_associated, c_char, c_f_pointer, c_int32_t, &
    c_int64_t, c_null_char, c_ptr
  use glamin_c_api, only: c_api_register_loaded_flat_index, &
    c_api_runtime_is_active, c_api_set_runtime_error
  use glamin_errors, only: GLAMIN_ERR_INVALID_ARG, GLAMIN_OK
  use glamin_geometry_layout, only: load_vector_layout
  use glamin_geometry_loader, only: load_flat_from_layout
  use glamin_index_flat, only: flat_destroy_handle
  use glamin_metrics, only: METRIC_IP, METRIC_L2
  use glamin_types, only: IndexHandle
  implicit none
  private

  public :: glamin_flat_index_load_artifact_c

  integer(int32), parameter :: MAX_ARTIFACT_PATH_LENGTH = 4000_int32
  integer(int32), parameter :: MAX_SPACE_ID_LENGTH = 128_int32

contains
  function glamin_flat_index_load_artifact_c(runtime, artifact_directory, &
      artifact_directory_length, space_id, space_id_length, metric, out_index, &
      out_dimension, out_vector_count) &
      bind(c, name="glamin_flat_index_load_artifact") result(status)
    integer(c_int64_t), value :: runtime
    type(c_ptr), value :: artifact_directory
    integer(c_int64_t), value :: artifact_directory_length
    type(c_ptr), value :: space_id
    integer(c_int64_t), value :: space_id_length
    integer(c_int32_t), value :: metric
    type(c_ptr), value :: out_index
    type(c_ptr), value :: out_dimension
    type(c_ptr), value :: out_vector_count
    integer(c_int32_t) :: status
    character(len=:), allocatable :: artifact_path
    character(len=:), allocatable :: contracts_path
    character(len=:), allocatable :: layout_path
    character(len=:), allocatable :: native_space_id
    character(len=:), allocatable :: vectors_path
    integer(c_int32_t), pointer :: dimension_output
    integer(c_int64_t), pointer :: index_output
    integer(c_int64_t), pointer :: vector_count_output
    type(IndexHandle) :: native_index
    integer(int32) :: dimension
    integer(int64) :: index_handle
    integer(int64) :: offset_bytes
    integer(int64) :: vector_count
    integer(int32) :: destroy_status
    integer(int32) :: load_status

    if (.not. c_api_runtime_is_active(int(runtime, int64))) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "runtime handle is invalid or no longer active")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if
    if (.not. c_associated(out_index) .or. .not. c_associated(out_dimension) .or. &
        .not. c_associated(out_vector_count)) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "persistent index output pointers must not be null")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    call c_f_pointer(out_index, index_output)
    call c_f_pointer(out_dimension, dimension_output)
    call c_f_pointer(out_vector_count, vector_count_output)
    index_output = 0_c_int64_t
    dimension_output = 0_c_int32_t
    vector_count_output = 0_c_int64_t

    call read_c_text(artifact_directory, artifact_directory_length, &
      MAX_ARTIFACT_PATH_LENGTH, artifact_path, load_status)
    if (load_status /= GLAMIN_OK) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "artifact directory must contain 1 to 4000 non-null bytes")
      status = int(load_status, c_int32_t)
      return
    end if
    call read_c_text(space_id, space_id_length, MAX_SPACE_ID_LENGTH, &
      native_space_id, load_status)
    if (load_status /= GLAMIN_OK) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "space_id must contain 1 to 128 non-null bytes")
      status = int(load_status, c_int32_t)
      return
    end if
    if (metric /= int(METRIC_L2, c_int32_t) .and. &
        metric /= int(METRIC_IP, c_int32_t)) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "persistent flat index metric is invalid")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    layout_path = artifact_file(artifact_path, "vector_layout.json")
    vectors_path = artifact_file(artifact_path, "vectors.bin")
    contracts_path = artifact_file(artifact_path, "contracts.json")
    call load_vector_layout(layout_path, native_space_id, dimension, vector_count, &
      offset_bytes, load_status)
    if (load_status /= GLAMIN_OK .or. vector_count <= 0_int64) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "persistent artifact has no valid vector layout for the requested space")
      status = int(GLAMIN_ERR_INVALID_ARG, c_int32_t)
      return
    end if

    native_index = IndexHandle()
    call load_flat_from_layout(layout_path, vectors_path, native_space_id, &
      int(metric, int32), native_index, load_status, contracts_path)
    if (load_status /= GLAMIN_OK) then
      call c_api_set_runtime_error(int(runtime, int64), &
        "persistent artifact failed vector or contract validation")
      status = int(load_status, c_int32_t)
      return
    end if

    call c_api_register_loaded_flat_index(int(runtime, int64), native_index, &
      dimension, vector_count, int(metric, int32), index_handle, load_status)
    if (load_status /= GLAMIN_OK) then
      call flat_destroy_handle(native_index, destroy_status)
      call c_api_set_runtime_error(int(runtime, int64), &
        "failed to register the loaded persistent index")
      status = int(load_status, c_int32_t)
      return
    end if

    index_output = int(index_handle, c_int64_t)
    dimension_output = int(dimension, c_int32_t)
    vector_count_output = int(vector_count, c_int64_t)
    status = int(GLAMIN_OK, c_int32_t)
  end function glamin_flat_index_load_artifact_c

  subroutine read_c_text(input, input_length, maximum_length, output, status)
    type(c_ptr), value :: input
    integer(c_int64_t), intent(in) :: input_length
    integer(int32), intent(in) :: maximum_length
    character(len=:), allocatable, intent(out) :: output
    integer(int32), intent(out) :: status
    character(kind=c_char), pointer :: input_characters(:)
    integer(int32) :: character_index

    if (.not. c_associated(input) .or. input_length <= 0_c_int64_t .or. &
        input_length > int(maximum_length, c_int64_t)) then
      output = ''
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call c_f_pointer(input, input_characters, [int(input_length, int32)])
    allocate(character(len=int(input_length, int32)) :: output)
    do character_index = 1_int32, int(input_length, int32)
      if (input_characters(character_index) == c_null_char) then
        output = ''
        status = GLAMIN_ERR_INVALID_ARG
        return
      end if
      output(character_index:character_index) = &
        achar(iachar(input_characters(character_index)))
    end do
    status = GLAMIN_OK
  end subroutine read_c_text

  function artifact_file(directory, file_name) result(path)
    character(len=*), intent(in) :: directory
    character(len=*), intent(in) :: file_name
    character(len=:), allocatable :: path
    integer :: directory_length

    directory_length = len_trim(directory)
    if (directory(directory_length:directory_length) == '/') then
      path = trim(directory) // file_name
    else
      path = trim(directory) // '/' // file_name
    end if
  end function artifact_file
end module glamin_c_load_api
