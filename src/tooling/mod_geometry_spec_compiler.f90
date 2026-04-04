module glamin_geometry_spec_compiler
  use iso_fortran_env, only: error_unit, int32, int64, real32, real64
  use glamin_errors, only: GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_UNKNOWN, GLAMIN_OK
  use glamin_native_hash, only: hash64, hash256_hex
  implicit none
  private

  public :: validate_geometry_spec_file
  public :: canonicalize_geometry_spec_file
  public :: compile_geometry_spec
  public :: embed_geometry_spec
  public :: embed_geometry_spec_file
  public :: visualize_geometry_spec_file

  integer, parameter :: STR_LEN = 512

  type :: EmbedderAuthoring
    character(len=STR_LEN) :: embedder_id = ''
    character(len=STR_LEN) :: embedder_version = ''
    character(len=STR_LEN) :: input_schema = ''
    character(len=STR_LEN), allocatable :: preprocess_chain(:)
    character(len=STR_LEN) :: model_hash = ''
    character(len=STR_LEN) :: config_hash = ''
    character(len=STR_LEN) :: hardware_class = ''
    integer(int32) :: min_ram_mb = 0
    integer(int32) :: min_vram_mb = 0
    character(len=STR_LEN) :: signature = ''
  end type EmbedderAuthoring

  type :: ObjectiveSpec
    character(len=STR_LEN) :: objective_id = ''
    character(len=STR_LEN) :: description = ''
    character(len=STR_LEN), allocatable :: target_mints(:)
    integer(int32) :: priority = 0
    logical :: has_priority = .false.
  end type ObjectiveSpec

  type :: InvariantSpec
    character(len=STR_LEN) :: type_name = ''
    character(len=STR_LEN) :: equals = ''
    character(len=STR_LEN), allocatable :: allowed(:)
    real(real64) :: min_value = 0.0_real64
    real(real64) :: max_value = 0.0_real64
    logical :: has_equals = .false.
    logical :: has_allowed = .false.
    logical :: has_min = .false.
    logical :: has_max = .false.
  end type InvariantSpec

  type :: SpaceSpec
    character(len=STR_LEN) :: space_id = ''
    integer(int32) :: dim = 0
    character(len=STR_LEN) :: metric = ''
    character(len=STR_LEN) :: normalization = ''
    character(len=STR_LEN), allocatable :: transform_chain(:)
    type(InvariantSpec), allocatable :: invariants(:)
  end type SpaceSpec

  type :: MintSpec
    character(len=STR_LEN) :: mint_id = ''
    character(len=STR_LEN) :: space_id = ''
    character(len=STR_LEN) :: text = ''
    character(len=STR_LEN), allocatable :: tags(:)
  end type MintSpec

  type :: CorridorSpec
    character(len=STR_LEN) :: corridor_id = ''
    character(len=STR_LEN) :: space_id = ''
    character(len=STR_LEN), allocatable :: between(:)
    real(real64) :: width = 0.0_real64
    character(len=STR_LEN) :: risk_profile = ''
    character(len=STR_LEN) :: notes = ''
  end type CorridorSpec

  type :: TraceSpec
    character(len=STR_LEN) :: trace_id = ''
    character(len=STR_LEN) :: space_id = ''
    character(len=STR_LEN), allocatable :: steps(:)
    real(real64) :: confidence = 0.0_real64
    logical :: has_confidence = .false.
    character(len=STR_LEN), allocatable :: timestamps(:)
    character(len=STR_LEN) :: notes = ''
  end type TraceSpec

  type :: GeometrySpec
    character(len=STR_LEN) :: spec_version = ''
    character(len=STR_LEN) :: spec_id = ''
    character(len=STR_LEN) :: title = ''
    character(len=STR_LEN) :: created_at = ''
    character(len=STR_LEN) :: owner = ''
    character(len=STR_LEN) :: notes = ''
    type(EmbedderAuthoring) :: embedder
    character(len=STR_LEN), allocatable :: entry_mints(:)
    type(ObjectiveSpec), allocatable :: objectives(:)
    type(SpaceSpec), allocatable :: spaces(:)
    type(MintSpec), allocatable :: mints(:)
    type(CorridorSpec), allocatable :: corridors(:)
    type(TraceSpec), allocatable :: traces(:)
  end type GeometrySpec

contains
  subroutine validate_geometry_spec_file(spec_path, status)
    character(len=*), intent(in) :: spec_path
    integer(int32), intent(out) :: status
    type(GeometrySpec) :: spec

    call load_geometry_spec(spec_path, spec, status)
  end subroutine validate_geometry_spec_file

  subroutine canonicalize_geometry_spec_file(spec_path, output_path, status)
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: output_path
    integer(int32), intent(out) :: status
    type(GeometrySpec) :: spec
    character(len=:), allocatable :: json

    call load_geometry_spec(spec_path, spec, status)
    if (status /= GLAMIN_OK) return

    json = geometry_spec_to_json(spec)
    call ensure_parent_directory(output_path, status)
    if (status /= GLAMIN_OK) return
    call write_text_file(output_path, json // new_line('a'), status)
  end subroutine canonicalize_geometry_spec_file

  subroutine compile_geometry_spec(spec_path, out_dir, status)
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: out_dir
    integer(int32), intent(out) :: status
    type(GeometrySpec) :: spec
    character(len=:), allocatable :: manifest_path
    character(len=:), allocatable :: contracts_path
    character(len=:), allocatable :: layout_path

    call load_geometry_spec(spec_path, spec, status)
    if (status /= GLAMIN_OK) return

    call ensure_directory(trim(out_dir), status)
    if (status /= GLAMIN_OK) return

    manifest_path = join_path(trim(out_dir), 'manifest.json')
    contracts_path = join_path(trim(out_dir), 'contracts.json')
    layout_path = join_path(trim(out_dir), 'vector_layout.json')

    call write_text_file(manifest_path, geometry_spec_to_json(spec) // new_line('a'), status)
    if (status /= GLAMIN_OK) return
    call write_text_file(contracts_path, contracts_to_json(spec) // new_line('a'), status)
    if (status /= GLAMIN_OK) return
    call write_text_file(layout_path, vector_layout_to_json(spec) // new_line('a'), status)
  end subroutine compile_geometry_spec

  subroutine embed_geometry_spec(spec_path, out_dir, status)
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: out_dir
    integer(int32), intent(out) :: status

    call embed_geometry_spec_file(spec_path, join_path(trim(out_dir), 'vectors.bin'), status)
  end subroutine embed_geometry_spec

  subroutine embed_geometry_spec_file(spec_path, output_path, status)
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: output_path
    integer(int32), intent(out) :: status
    type(GeometrySpec) :: spec
    integer :: unit_id
    integer :: io_status
    integer :: mint_index
    integer :: space_index
    real(real32), allocatable :: vector(:)

    call load_geometry_spec(spec_path, spec, status)
    if (status /= GLAMIN_OK) return

    call ensure_parent_directory(output_path, status)
    if (status /= GLAMIN_OK) return

    open(newunit=unit_id, file=trim(output_path), access='stream', form='unformatted', &
      status='replace', action='write', iostat=io_status)
    if (io_status /= 0) then
      status = GLAMIN_ERR_UNKNOWN
      write(error_unit, '(A)') 'glamin_spec_tool: failed to open output vectors file'
      return
    end if

    do mint_index = 1, size(spec%mints)
      space_index = find_space_index(spec, spec%mints(mint_index)%space_id)
      allocate(vector(spec%spaces(space_index)%dim))
      call embed_text(spec%mints(mint_index)%text, spec%spaces(space_index)%dim, &
        spec%spaces(space_index)%normalization, vector)
      write(unit_id, iostat=io_status) vector
      deallocate(vector)
      if (io_status /= 0) then
        close(unit_id)
        status = GLAMIN_ERR_UNKNOWN
        write(error_unit, '(A)') 'glamin_spec_tool: failed while writing vectors'
        return
      end if
    end do

    close(unit_id)
    status = GLAMIN_OK
  end subroutine embed_geometry_spec_file

  subroutine visualize_geometry_spec_file(spec_path, output_path, status)
    character(len=*), intent(in) :: spec_path
    character(len=*), intent(in) :: output_path
    integer(int32), intent(out) :: status
    type(GeometrySpec) :: spec
    character(len=:), allocatable :: dot

    call load_geometry_spec(spec_path, spec, status)
    if (status /= GLAMIN_OK) return

    dot = geometry_spec_to_dot(spec)
    call ensure_parent_directory(output_path, status)
    if (status /= GLAMIN_OK) return
    call write_text_file(output_path, dot, status)
  end subroutine visualize_geometry_spec_file

  subroutine load_geometry_spec(path, spec, status)
    character(len=*), intent(in) :: path
    type(GeometrySpec), intent(out) :: spec
    integer(int32), intent(out) :: status
    integer :: unit_id
    integer :: io_status
    character(len=2048) :: raw_line
    integer :: indent
    character(len=STR_LEN) :: line
    character(len=STR_LEN) :: section
    character(len=STR_LEN) :: sublist
    integer :: current_objective
    integer :: current_space
    integer :: current_invariant
    integer :: current_mint
    integer :: current_corridor
    integer :: current_trace

    spec = GeometrySpec()
    status = GLAMIN_OK
    section = ''
    sublist = ''
    current_objective = 0
    current_space = 0
    current_invariant = 0
    current_mint = 0
    current_corridor = 0
    current_trace = 0

    open(newunit=unit_id, file=trim(path), status='old', action='read', iostat=io_status)
    if (io_status /= 0) then
      status = GLAMIN_ERR_INVALID_ARG
      write(error_unit, '(A)') 'glamin_spec_tool: failed to open spec file'
      return
    end if

    do
      read(unit_id, '(A)', iostat=io_status) raw_line
      if (io_status /= 0) exit

      call preprocess_line(raw_line, indent, line)
      if (len_trim(line) == 0) cycle

      if (indent == 0) then
        current_invariant = 0
        call parse_top_level_line(spec, trim(line), section, sublist, status)
      else
        select case (trim(section))
        case ('embedder')
          call parse_embedder_line(spec, indent, trim(line), sublist, status)
        case ('entry_mints')
          call parse_entry_mints_line(spec, indent, trim(line), status)
        case ('objectives')
          call parse_objectives_line(spec, indent, trim(line), sublist, current_objective, status)
        case ('spaces')
          call parse_spaces_line(spec, indent, trim(line), sublist, current_space, current_invariant, status)
        case ('mints')
          call parse_mints_line(spec, indent, trim(line), sublist, current_mint, status)
        case ('corridors')
          call parse_corridors_line(spec, indent, trim(line), current_corridor, status)
        case ('traces')
          call parse_traces_line(spec, indent, trim(line), sublist, current_trace, status)
        case default
          status = GLAMIN_ERR_INVALID_ARG
          write(error_unit, '(A)') 'glamin_spec_tool: invalid top-level section in spec'
        end select
      end if

      if (status /= GLAMIN_OK) exit
    end do

    close(unit_id)
    if (status /= GLAMIN_OK) then
      write(error_unit, '(A,I0,A,A,A,A,A)') 'glamin_spec_tool: parse failed at indent ', indent, &
        ' in section "', trim(section), '" for line "', trim(line), '"'
      return
    end if

    call validate_geometry_spec(spec, status)
  end subroutine load_geometry_spec

  subroutine parse_top_level_line(spec, line, section, sublist, status)
    type(GeometrySpec), intent(inout) :: spec
    character(len=*), intent(in) :: line
    character(len=STR_LEN), intent(inout) :: section
    character(len=STR_LEN), intent(inout) :: sublist
    integer(int32), intent(out) :: status
    character(len=STR_LEN) :: key
    character(len=STR_LEN) :: value
    logical :: has_value

    status = GLAMIN_OK
    section = ''
    sublist = ''

    call split_key_value(line, key, value, has_value, status)
    if (status /= GLAMIN_OK) return

    select case (trim(key))
    case ('spec_version')
      call require_value(has_value, status)
      if (status == GLAMIN_OK) spec%spec_version = trim(unquote(value))
    case ('spec_id')
      call require_value(has_value, status)
      if (status == GLAMIN_OK) spec%spec_id = trim(unquote(value))
    case ('title')
      call require_value(has_value, status)
      if (status == GLAMIN_OK) spec%title = trim(unquote(value))
    case ('created_at')
      call require_value(has_value, status)
      if (status == GLAMIN_OK) spec%created_at = trim(unquote(value))
    case ('owner')
      call require_value(has_value, status)
      if (status == GLAMIN_OK) spec%owner = trim(unquote(value))
    case ('notes')
      call require_value(has_value, status)
      if (status == GLAMIN_OK) spec%notes = trim(unquote(value))
    case ('embedder', 'entry_mints', 'objectives', 'spaces', 'mints', 'corridors', 'traces')
      if (has_value) then
        status = GLAMIN_ERR_INVALID_ARG
      else
        section = trim(key)
      end if
    case default
      status = GLAMIN_ERR_INVALID_ARG
    end select

    if (status /= GLAMIN_OK) then
      write(error_unit, '(A,1X,A)') 'glamin_spec_tool: unsupported top-level key', trim(key)
    end if
  end subroutine parse_top_level_line

  subroutine parse_embedder_line(spec, indent, line, sublist, status)
    type(GeometrySpec), intent(inout) :: spec
    integer, intent(in) :: indent
    character(len=*), intent(in) :: line
    character(len=STR_LEN), intent(inout) :: sublist
    integer(int32), intent(out) :: status
    character(len=STR_LEN) :: key
    character(len=STR_LEN) :: value
    logical :: has_value

    status = GLAMIN_OK
    if (indent == 2) then
      sublist = ''
      call split_key_value(line, key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      select case (trim(key))
      case ('id')
        spec%embedder%embedder_id = trim(unquote(value))
      case ('version')
        spec%embedder%embedder_version = trim(unquote(value))
      case ('input_schema')
        spec%embedder%input_schema = trim(unquote(value))
      case ('preprocess_chain')
        if (has_value) then
          call set_string_array(spec%embedder%preprocess_chain, value, status)
        else
          sublist = 'embedder.preprocess_chain'
        end if
      case ('model_hash')
        spec%embedder%model_hash = trim(unquote(value))
      case ('config_hash')
        spec%embedder%config_hash = trim(unquote(value))
      case ('hardware_class')
        spec%embedder%hardware_class = trim(unquote(value))
      case ('min_ram_mb')
        call parse_int32(value, spec%embedder%min_ram_mb, status)
      case ('min_vram_mb')
        call parse_int32(value, spec%embedder%min_vram_mb, status)
      case ('signature')
        spec%embedder%signature = trim(unquote(value))
      case default
        status = GLAMIN_ERR_INVALID_ARG
      end select
    else if (indent == 4 .and. trim(sublist) == 'embedder.preprocess_chain') then
      call append_list_item(spec%embedder%preprocess_chain, line, status)
    else
      status = GLAMIN_ERR_INVALID_ARG
    end if
  end subroutine parse_embedder_line

  subroutine parse_entry_mints_line(spec, indent, line, status)
    type(GeometrySpec), intent(inout) :: spec
    integer, intent(in) :: indent
    character(len=*), intent(in) :: line
    integer(int32), intent(out) :: status

    status = GLAMIN_OK
    if (indent /= 2) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    call append_list_item(spec%entry_mints, line, status)
  end subroutine parse_entry_mints_line

  subroutine parse_objectives_line(spec, indent, line, sublist, current_objective, status)
    type(GeometrySpec), intent(inout) :: spec
    integer, intent(in) :: indent
    character(len=*), intent(in) :: line
    character(len=STR_LEN), intent(inout) :: sublist
    integer, intent(inout) :: current_objective
    integer(int32), intent(out) :: status
    character(len=STR_LEN) :: key
    character(len=STR_LEN) :: value
    logical :: has_value
    type(ObjectiveSpec) :: item

    status = GLAMIN_OK
    if (indent == 2 .and. starts_with(line, '- ')) then
      item = ObjectiveSpec()
      call append_objective(spec%objectives, item)
      current_objective = size(spec%objectives)
      sublist = ''
      call split_key_value(trim(line(3:)), key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_objective_property(spec%objectives(current_objective), key, value, has_value, sublist, status)
    else if (indent == 4 .and. current_objective > 0) then
      call split_key_value(line, key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_objective_property(spec%objectives(current_objective), key, value, has_value, sublist, status)
    else if (indent == 6 .and. current_objective > 0 .and. trim(sublist) == 'objective.target_mints') then
      call append_list_item(spec%objectives(current_objective)%target_mints, line, status)
    else
      status = GLAMIN_ERR_INVALID_ARG
    end if
  end subroutine parse_objectives_line

  subroutine parse_spaces_line(spec, indent, line, sublist, current_space, current_invariant, status)
    type(GeometrySpec), intent(inout) :: spec
    integer, intent(in) :: indent
    character(len=*), intent(in) :: line
    character(len=STR_LEN), intent(inout) :: sublist
    integer, intent(inout) :: current_space
    integer, intent(inout) :: current_invariant
    integer(int32), intent(out) :: status
    character(len=STR_LEN) :: key
    character(len=STR_LEN) :: value
    logical :: has_value
    type(SpaceSpec) :: item
    type(InvariantSpec) :: invariant

    status = GLAMIN_OK
    if (indent == 2 .and. starts_with(line, '- ')) then
      item = SpaceSpec()
      call append_space(spec%spaces, item)
      current_space = size(spec%spaces)
      current_invariant = 0
      sublist = ''
      call split_key_value(trim(line(3:)), key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_space_property(spec%spaces(current_space), key, value, has_value, sublist, status)
    else if (indent == 4 .and. current_space > 0) then
      current_invariant = 0
      call split_key_value(line, key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_space_property(spec%spaces(current_space), key, value, has_value, sublist, status)
    else if (indent == 6 .and. current_space > 0 .and. trim(sublist) == 'space.transform_chain') then
      call append_list_item(spec%spaces(current_space)%transform_chain, line, status)
    else if (indent == 6 .and. current_space > 0 .and. starts_with(line, '- ')) then
      invariant = InvariantSpec()
      call append_invariant(spec%spaces(current_space)%invariants, invariant)
      current_invariant = size(spec%spaces(current_space)%invariants)
      sublist = 'space.invariants'
      call split_key_value(trim(line(3:)), key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_invariant_property(spec%spaces(current_space)%invariants(current_invariant), key, value, &
        has_value, sublist, status)
    else if (indent == 8 .and. current_space > 0 .and. current_invariant > 0) then
      call split_key_value(line, key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_invariant_property(spec%spaces(current_space)%invariants(current_invariant), key, value, &
        has_value, sublist, status)
    else if (indent == 10 .and. current_space > 0 .and. current_invariant > 0 .and. &
        trim(sublist) == 'space.invariant.allowed') then
      call append_list_item(spec%spaces(current_space)%invariants(current_invariant)%allowed, line, status)
    else
      status = GLAMIN_ERR_INVALID_ARG
    end if
  end subroutine parse_spaces_line

  subroutine parse_mints_line(spec, indent, line, sublist, current_mint, status)
    type(GeometrySpec), intent(inout) :: spec
    integer, intent(in) :: indent
    character(len=*), intent(in) :: line
    character(len=STR_LEN), intent(inout) :: sublist
    integer, intent(inout) :: current_mint
    integer(int32), intent(out) :: status
    character(len=STR_LEN) :: key
    character(len=STR_LEN) :: value
    logical :: has_value
    type(MintSpec) :: item

    status = GLAMIN_OK
    if (indent == 2 .and. starts_with(line, '- ')) then
      item = MintSpec()
      call append_mint(spec%mints, item)
      current_mint = size(spec%mints)
      sublist = ''
      call split_key_value(trim(line(3:)), key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_mint_property(spec%mints(current_mint), key, value, has_value, sublist, status)
    else if (indent == 4 .and. current_mint > 0) then
      call split_key_value(line, key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_mint_property(spec%mints(current_mint), key, value, has_value, sublist, status)
    else if (indent == 6 .and. current_mint > 0 .and. trim(sublist) == 'mint.tags') then
      call append_list_item(spec%mints(current_mint)%tags, line, status)
    else
      status = GLAMIN_ERR_INVALID_ARG
    end if
  end subroutine parse_mints_line

  subroutine parse_corridors_line(spec, indent, line, current_corridor, status)
    type(GeometrySpec), intent(inout) :: spec
    integer, intent(in) :: indent
    character(len=*), intent(in) :: line
    integer, intent(inout) :: current_corridor
    integer(int32), intent(out) :: status
    character(len=STR_LEN) :: key
    character(len=STR_LEN) :: value
    logical :: has_value
    type(CorridorSpec) :: item

    status = GLAMIN_OK
    if (indent == 2 .and. starts_with(line, '- ')) then
      item = CorridorSpec()
      call append_corridor(spec%corridors, item)
      current_corridor = size(spec%corridors)
      call split_key_value(trim(line(3:)), key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_corridor_property(spec%corridors(current_corridor), key, value, has_value, status)
    else if (indent == 4 .and. current_corridor > 0) then
      call split_key_value(line, key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_corridor_property(spec%corridors(current_corridor), key, value, has_value, status)
    else
      status = GLAMIN_ERR_INVALID_ARG
    end if
  end subroutine parse_corridors_line

  subroutine parse_traces_line(spec, indent, line, sublist, current_trace, status)
    type(GeometrySpec), intent(inout) :: spec
    integer, intent(in) :: indent
    character(len=*), intent(in) :: line
    character(len=STR_LEN), intent(inout) :: sublist
    integer, intent(inout) :: current_trace
    integer(int32), intent(out) :: status
    character(len=STR_LEN) :: key
    character(len=STR_LEN) :: value
    logical :: has_value
    type(TraceSpec) :: item

    status = GLAMIN_OK
    if (indent == 2 .and. starts_with(line, '- ')) then
      item = TraceSpec()
      call append_trace(spec%traces, item)
      current_trace = size(spec%traces)
      sublist = ''
      call split_key_value(trim(line(3:)), key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_trace_property(spec%traces(current_trace), key, value, has_value, sublist, status)
    else if (indent == 4 .and. current_trace > 0) then
      call split_key_value(line, key, value, has_value, status)
      if (status /= GLAMIN_OK) return
      call assign_trace_property(spec%traces(current_trace), key, value, has_value, sublist, status)
    else if (indent == 6 .and. current_trace > 0) then
      select case (trim(sublist))
      case ('trace.steps')
        call append_list_item(spec%traces(current_trace)%steps, line, status)
      case ('trace.timestamps')
        call append_list_item(spec%traces(current_trace)%timestamps, line, status)
      case default
        status = GLAMIN_ERR_INVALID_ARG
      end select
    else
      status = GLAMIN_ERR_INVALID_ARG
    end if
  end subroutine parse_traces_line

  subroutine assign_objective_property(item, key, value, has_value, sublist, status)
    type(ObjectiveSpec), intent(inout) :: item
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    logical, intent(in) :: has_value
    character(len=STR_LEN), intent(inout) :: sublist
    integer(int32), intent(out) :: status

    status = GLAMIN_OK
    sublist = ''
    select case (trim(key))
    case ('objective_id')
      item%objective_id = trim(unquote(value))
    case ('description')
      item%description = trim(unquote(value))
    case ('target_mints')
      if (has_value) then
        call set_string_array(item%target_mints, value, status)
      else
        sublist = 'objective.target_mints'
      end if
    case ('priority')
      call parse_int32(value, item%priority, status)
      if (status == GLAMIN_OK) item%has_priority = .true.
    case default
      status = GLAMIN_ERR_INVALID_ARG
    end select
  end subroutine assign_objective_property

  subroutine assign_space_property(item, key, value, has_value, sublist, status)
    type(SpaceSpec), intent(inout) :: item
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    logical, intent(in) :: has_value
    character(len=STR_LEN), intent(inout) :: sublist
    integer(int32), intent(out) :: status

    status = GLAMIN_OK
    sublist = ''
    select case (trim(key))
    case ('space_id')
      item%space_id = trim(unquote(value))
    case ('dim')
      call parse_int32(value, item%dim, status)
    case ('metric')
      item%metric = trim(unquote(value))
    case ('normalization')
      item%normalization = trim(unquote(value))
    case ('transform_chain')
      if (has_value) then
        call set_string_array(item%transform_chain, value, status)
      else
        sublist = 'space.transform_chain'
      end if
    case ('invariants')
      if (has_value) then
        status = GLAMIN_ERR_INVALID_ARG
      else
        sublist = 'space.invariants'
      end if
    case default
      status = GLAMIN_ERR_INVALID_ARG
    end select
  end subroutine assign_space_property

  subroutine assign_invariant_property(item, key, value, has_value, sublist, status)
    type(InvariantSpec), intent(inout) :: item
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    logical, intent(in) :: has_value
    character(len=STR_LEN), intent(inout) :: sublist
    integer(int32), intent(out) :: status

    status = GLAMIN_OK
    sublist = 'space.invariants'
    select case (trim(key))
    case ('type')
      item%type_name = trim(unquote(value))
    case ('equals')
      item%equals = trim(unquote(value))
      item%has_equals = .true.
    case ('allowed')
      if (has_value) then
        call set_string_array(item%allowed, value, status)
        if (status == GLAMIN_OK) item%has_allowed = .true.
      else
        sublist = 'space.invariant.allowed'
      end if
    case ('min')
      call parse_real64(value, item%min_value, status)
      if (status == GLAMIN_OK) item%has_min = .true.
    case ('max')
      call parse_real64(value, item%max_value, status)
      if (status == GLAMIN_OK) item%has_max = .true.
    case default
      status = GLAMIN_ERR_INVALID_ARG
    end select
  end subroutine assign_invariant_property

  subroutine assign_mint_property(item, key, value, has_value, sublist, status)
    type(MintSpec), intent(inout) :: item
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    logical, intent(in) :: has_value
    character(len=STR_LEN), intent(inout) :: sublist
    integer(int32), intent(out) :: status

    status = GLAMIN_OK
    sublist = ''
    select case (trim(key))
    case ('mint_id')
      item%mint_id = trim(unquote(value))
    case ('space_id')
      item%space_id = trim(unquote(value))
    case ('text')
      item%text = trim(unquote(value))
    case ('tags')
      if (has_value) then
        call set_string_array(item%tags, value, status)
      else
        sublist = 'mint.tags'
      end if
    case default
      status = GLAMIN_ERR_INVALID_ARG
    end select
  end subroutine assign_mint_property

  subroutine assign_corridor_property(item, key, value, has_value, status)
    type(CorridorSpec), intent(inout) :: item
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    logical, intent(in) :: has_value
    integer(int32), intent(out) :: status

    status = GLAMIN_OK
    select case (trim(key))
    case ('corridor_id')
      item%corridor_id = trim(unquote(value))
    case ('space_id')
      item%space_id = trim(unquote(value))
    case ('between')
      if (.not. has_value) then
        status = GLAMIN_ERR_INVALID_ARG
      else
        call set_string_array(item%between, value, status)
      end if
    case ('width')
      call parse_real64(value, item%width, status)
    case ('risk_profile')
      item%risk_profile = trim(unquote(value))
    case ('notes')
      item%notes = trim(unquote(value))
    case default
      status = GLAMIN_ERR_INVALID_ARG
    end select
  end subroutine assign_corridor_property

  subroutine assign_trace_property(item, key, value, has_value, sublist, status)
    type(TraceSpec), intent(inout) :: item
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    logical, intent(in) :: has_value
    character(len=STR_LEN), intent(inout) :: sublist
    integer(int32), intent(out) :: status

    status = GLAMIN_OK
    sublist = ''
    select case (trim(key))
    case ('trace_id')
      item%trace_id = trim(unquote(value))
    case ('space_id')
      item%space_id = trim(unquote(value))
    case ('steps')
      if (has_value) then
        call set_string_array(item%steps, value, status)
      else
        sublist = 'trace.steps'
      end if
    case ('confidence')
      call parse_real64(value, item%confidence, status)
      if (status == GLAMIN_OK) item%has_confidence = .true.
    case ('timestamps')
      if (has_value) then
        call set_string_array(item%timestamps, value, status)
      else
        sublist = 'trace.timestamps'
      end if
    case ('notes')
      item%notes = trim(unquote(value))
    case default
      status = GLAMIN_ERR_INVALID_ARG
    end select
  end subroutine assign_trace_property

  subroutine validate_geometry_spec(spec, status)
    type(GeometrySpec), intent(in) :: spec
    integer(int32), intent(out) :: status
    integer :: idx

    status = GLAMIN_OK
    if (len_trim(spec%spec_version) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(spec%spec_id) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(spec%created_at) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(spec%owner) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(spec%embedder%embedder_id) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(spec%embedder%embedder_version) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(spec%embedder%input_schema) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(spec%embedder%model_hash) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(spec%embedder%config_hash) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (len_trim(spec%embedder%hardware_class) == 0) status = GLAMIN_ERR_INVALID_ARG
    if (.not. allocated(spec%embedder%preprocess_chain)) status = GLAMIN_ERR_INVALID_ARG
    if (.not. allocated(spec%spaces)) status = GLAMIN_ERR_INVALID_ARG
    if (.not. allocated(spec%mints)) status = GLAMIN_ERR_INVALID_ARG
    if (status /= GLAMIN_OK) then
      write(error_unit, '(A)') 'glamin_spec_tool: missing required geometry spec fields'
      return
    end if

    do idx = 1, size(spec%spaces)
      if (len_trim(spec%spaces(idx)%space_id) == 0 .or. spec%spaces(idx)%dim <= 0 .or. &
          len_trim(spec%spaces(idx)%metric) == 0 .or. len_trim(spec%spaces(idx)%normalization) == 0) then
        status = GLAMIN_ERR_INVALID_ARG
        write(error_unit, '(A)') 'glamin_spec_tool: invalid space definition'
        return
      end if
    end do

    do idx = 1, size(spec%mints)
      if (len_trim(spec%mints(idx)%mint_id) == 0 .or. len_trim(spec%mints(idx)%space_id) == 0 .or. &
          len_trim(spec%mints(idx)%text) == 0) then
        status = GLAMIN_ERR_INVALID_ARG
        write(error_unit, '(A)') 'glamin_spec_tool: invalid mint definition'
        return
      end if
      if (find_space_index(spec, spec%mints(idx)%space_id) == 0) then
        status = GLAMIN_ERR_INVALID_ARG
        write(error_unit, '(A)') 'glamin_spec_tool: mint references unknown space'
        return
      end if
    end do

    if (allocated(spec%entry_mints)) then
      do idx = 1, size(spec%entry_mints)
        if (find_mint_index(spec, spec%entry_mints(idx)) == 0) then
          status = GLAMIN_ERR_INVALID_ARG
          write(error_unit, '(A)') 'glamin_spec_tool: entry_mint references unknown mint'
          return
        end if
      end do
    end if

    if (allocated(spec%objectives)) then
      call validate_objectives(spec, status)
      if (status /= GLAMIN_OK) return
    end if

    if (allocated(spec%corridors)) then
      call validate_corridors(spec, status)
      if (status /= GLAMIN_OK) return
    end if

    if (allocated(spec%traces)) then
      call validate_traces(spec, status)
      if (status /= GLAMIN_OK) return
    end if
  end subroutine validate_geometry_spec

  subroutine validate_objectives(spec, status)
    type(GeometrySpec), intent(in) :: spec
    integer(int32), intent(out) :: status
    integer :: idx
    integer :: target_idx

    status = GLAMIN_OK
    do idx = 1, size(spec%objectives)
      if (len_trim(spec%objectives(idx)%objective_id) == 0) then
        status = GLAMIN_ERR_INVALID_ARG
        return
      end if
      if (.not. allocated(spec%objectives(idx)%target_mints)) then
        status = GLAMIN_ERR_INVALID_ARG
        return
      end if
      do target_idx = 1, size(spec%objectives(idx)%target_mints)
        if (find_mint_index(spec, spec%objectives(idx)%target_mints(target_idx)) == 0) then
          status = GLAMIN_ERR_INVALID_ARG
          return
        end if
      end do
    end do
  end subroutine validate_objectives

  subroutine validate_corridors(spec, status)
    type(GeometrySpec), intent(in) :: spec
    integer(int32), intent(out) :: status
    integer :: idx
    integer :: between_idx

    status = GLAMIN_OK
    do idx = 1, size(spec%corridors)
      if (len_trim(spec%corridors(idx)%corridor_id) == 0 .or. len_trim(spec%corridors(idx)%space_id) == 0) then
        status = GLAMIN_ERR_INVALID_ARG
        return
      end if
      if (find_space_index(spec, spec%corridors(idx)%space_id) == 0) then
        status = GLAMIN_ERR_INVALID_ARG
        return
      end if
      if (.not. allocated(spec%corridors(idx)%between)) then
        status = GLAMIN_ERR_INVALID_ARG
        return
      end if
      do between_idx = 1, size(spec%corridors(idx)%between)
        if (find_mint_index(spec, spec%corridors(idx)%between(between_idx)) == 0) then
          status = GLAMIN_ERR_INVALID_ARG
          return
        end if
      end do
    end do
  end subroutine validate_corridors

  subroutine validate_traces(spec, status)
    type(GeometrySpec), intent(in) :: spec
    integer(int32), intent(out) :: status
    integer :: idx
    integer :: step_idx

    status = GLAMIN_OK
    do idx = 1, size(spec%traces)
      if (len_trim(spec%traces(idx)%trace_id) == 0 .or. len_trim(spec%traces(idx)%space_id) == 0) then
        status = GLAMIN_ERR_INVALID_ARG
        return
      end if
      if (find_space_index(spec, spec%traces(idx)%space_id) == 0) then
        status = GLAMIN_ERR_INVALID_ARG
        return
      end if
      if (.not. allocated(spec%traces(idx)%steps)) then
        status = GLAMIN_ERR_INVALID_ARG
        return
      end if
      do step_idx = 1, size(spec%traces(idx)%steps)
        if (find_mint_index(spec, spec%traces(idx)%steps(step_idx)) == 0) then
          status = GLAMIN_ERR_INVALID_ARG
          return
        end if
      end do
      if (spec%traces(idx)%has_confidence) then
        if (spec%traces(idx)%confidence < 0.0_real64 .or. spec%traces(idx)%confidence > 1.0_real64) then
          status = GLAMIN_ERR_INVALID_ARG
          return
        end if
      end if
    end do
  end subroutine validate_traces

  function geometry_spec_to_json(spec) result(json)
    type(GeometrySpec), intent(in) :: spec
    character(len=:), allocatable :: json
    logical :: first

    json = '{'
    first = .true.
    call append_json_member(json, 'spec_version', json_quote(spec%spec_version), first)
    call append_json_member(json, 'spec_id', json_quote(spec%spec_id), first)
    if (len_trim(spec%title) > 0) call append_json_member(json, 'title', json_quote(spec%title), first)
    call append_json_member(json, 'created_at', json_quote(spec%created_at), first)
    call append_json_member(json, 'owner', json_quote(spec%owner), first)
    if (len_trim(spec%notes) > 0) call append_json_member(json, 'notes', json_quote(spec%notes), first)
    call append_json_member(json, 'embedder', embedder_to_json(spec%embedder), first)
    if (allocated(spec%entry_mints)) call append_json_member(json, 'entry_mints', string_array_to_json(spec%entry_mints), first)
    if (allocated(spec%objectives)) call append_json_member(json, 'objectives', objectives_to_json(spec%objectives), first)
    call append_json_member(json, 'spaces', spaces_to_json(spec%spaces), first)
    call append_json_member(json, 'mints', mints_to_json(spec%mints), first)
    if (allocated(spec%corridors)) call append_json_member(json, 'corridors', corridors_to_json(spec%corridors), first)
    if (allocated(spec%traces)) call append_json_member(json, 'traces', traces_to_json(spec%traces), first)
    json = json // '}'
  end function geometry_spec_to_json

  function contracts_to_json(spec) result(json)
    type(GeometrySpec), intent(in) :: spec
    character(len=:), allocatable :: json
    logical :: first
    integer :: idx
    character(len=:), allocatable :: embedder_spec_json
    character(len=:), allocatable :: space_spec_json

    embedder_spec_json = embedder_to_json(spec%embedder, include_signature=.false.)

    json = '{'
    first = .true.
    call append_json_member(json, 'spec_id', json_quote(spec%spec_id), first)
    call append_json_member(json, 'embedder', '{' // &
      json_pair('spec', embedder_spec_json) // ',' // &
      json_pair('contract_hash', json_quote(hash256_hex(embedder_spec_json))) // ',' // &
      json_pair('signature', json_quote(spec%embedder%signature)) // '}', first)

    json = json // ',"spaces":['
    do idx = 1, size(spec%spaces)
      if (idx > 1) json = json // ','
      space_spec_json = space_to_json(spec%spaces(idx))
      json = json // '{' // &
        json_pair('spec', space_spec_json) // ',' // &
        json_pair('contract_hash', json_quote(hash256_hex(space_spec_json))) // ',' // &
        json_pair('signature', json_quote('')) // '}'
    end do
    json = json // ']}'
  end function contracts_to_json

  function vector_layout_to_json(spec) result(json)
    type(GeometrySpec), intent(in) :: spec
    character(len=:), allocatable :: json
    integer(int64) :: offset_bytes
    integer(int64) :: total_vectors
    integer(int64) :: total_bytes
    integer :: space_idx
    integer :: mint_idx
    integer(int64) :: space_count
    integer(int64) :: local_index
    integer(int64) :: stride_bytes

    offset_bytes = 0_int64
    total_vectors = int(size(spec%mints), int64)

    json = '{'
    json = json // json_pair('dtype', json_quote('float32')) // ','
    json = json // json_pair('endianness', json_quote('little')) // ','
    json = json // json_pair('total_vectors', int_to_json(total_vectors)) // ','

    total_bytes = 0_int64
    do space_idx = 1, size(spec%spaces)
      space_count = count_space_mints(spec, spec%spaces(space_idx)%space_id)
      total_bytes = total_bytes + int(spec%spaces(space_idx)%dim, int64) * 4_int64 * space_count
    end do
    json = json // json_pair('total_bytes', int_to_json(total_bytes)) // ','
    json = json // '"spaces":['

    do space_idx = 1, size(spec%spaces)
      if (space_idx > 1) json = json // ','
      space_count = count_space_mints(spec, spec%spaces(space_idx)%space_id)
      stride_bytes = int(spec%spaces(space_idx)%dim, int64) * 4_int64
      json = json // '{'
      json = json // json_pair('space_id', json_quote(spec%spaces(space_idx)%space_id)) // ','
      json = json // json_pair('dim', int_to_json(int(spec%spaces(space_idx)%dim, int64))) // ','
      json = json // json_pair('count', int_to_json(space_count)) // ','
      json = json // json_pair('byte_stride', int_to_json(stride_bytes)) // ','
      json = json // json_pair('offset_bytes', int_to_json(offset_bytes)) // ','
      json = json // '"mints":['
      local_index = 0_int64
      do mint_idx = 1, size(spec%mints)
        if (trim(spec%mints(mint_idx)%space_id) /= trim(spec%spaces(space_idx)%space_id)) cycle
        if (local_index > 0_int64) json = json // ','
        json = json // '{' // &
          json_pair('mint_id', json_quote(spec%mints(mint_idx)%mint_id)) // ',' // &
          json_pair('index', int_to_json(local_index)) // ',' // &
          json_pair('offset_bytes', int_to_json(offset_bytes + local_index * stride_bytes)) // '}'
        local_index = local_index + 1_int64
      end do
      json = json // ']}'
      offset_bytes = offset_bytes + stride_bytes * space_count
    end do

    json = json // ']}'
  end function vector_layout_to_json

  function geometry_spec_to_dot(spec) result(dot)
    type(GeometrySpec), intent(in) :: spec
    character(len=:), allocatable :: dot
    integer :: idx
    integer :: step_idx

    dot = 'digraph Glamin {' // new_line('a')
    dot = dot // '  rankdir=LR;' // new_line('a')
    do idx = 1, size(spec%mints)
      dot = dot // '  "' // escape_json(trim(spec%mints(idx)%mint_id)) // '" [label=' // &
        json_quote(trim(spec%mints(idx)%mint_id)) // '];' // new_line('a')
    end do
    if (allocated(spec%corridors)) then
      do idx = 1, size(spec%corridors)
        if (.not. allocated(spec%corridors(idx)%between)) cycle
        if (size(spec%corridors(idx)%between) < 2) cycle
        dot = dot // '  "' // escape_json(trim(spec%corridors(idx)%between(1))) // '" -> "' // &
          escape_json(trim(spec%corridors(idx)%between(2))) // '" [style=dashed,label=' // &
          json_quote(trim(spec%corridors(idx)%corridor_id)) // '];' // new_line('a')
      end do
    end if
    if (allocated(spec%traces)) then
      do idx = 1, size(spec%traces)
        if (.not. allocated(spec%traces(idx)%steps)) cycle
        do step_idx = 1, size(spec%traces(idx)%steps) - 1
          dot = dot // '  "' // escape_json(trim(spec%traces(idx)%steps(step_idx))) // '" -> "' // &
            escape_json(trim(spec%traces(idx)%steps(step_idx + 1))) // '" [label=' // &
            json_quote(trim(spec%traces(idx)%trace_id)) // '];' // new_line('a')
        end do
      end do
    end if
    dot = dot // '}' // new_line('a')
  end function geometry_spec_to_dot

  subroutine embed_text(text, dim, normalization, vector)
    character(len=*), intent(in) :: text
    integer(int32), intent(in) :: dim
    character(len=*), intent(in) :: normalization
    real(real32), intent(out) :: vector(dim)
    character(len=STR_LEN) :: lowered
    character(len=STR_LEN) :: token
    integer :: idx
    integer :: token_len
    integer(int64) :: slot
    real(real64) :: norm

    vector = 0.0_real32
    lowered = lowercase(text)
    token = ''
    token_len = 0

    do idx = 1, len_trim(lowered) + 1
      if (idx <= len_trim(lowered) .and. is_token_char(lowered(idx:idx))) then
        if (token_len < STR_LEN) then
          token_len = token_len + 1
          token(token_len:token_len) = lowered(idx:idx)
        end if
      else if (token_len > 0) then
        slot = modulo(hash64(token(:token_len)), int(dim, int64))
        vector(int(slot, kind=4) + 1) = vector(int(slot, kind=4) + 1) + 1.0_real32
        token = ''
        token_len = 0
      end if
    end do

    if (trim(lowercase(normalization)) == 'l2') then
      norm = sqrt(sum(real(vector, real64) * real(vector, real64)))
      if (norm > 0.0_real64) vector = real(real(vector, real64) / norm, real32)
    end if
  end subroutine embed_text

  subroutine preprocess_line(raw_line, indent, line)
    character(len=*), intent(in) :: raw_line
    integer, intent(out) :: indent
    character(len=STR_LEN), intent(out) :: line
    character(len=2048) :: stripped
    integer :: idx
    integer :: mark

    stripped = raw_line
    mark = index(stripped, '#')
    if (mark > 0) stripped = stripped(:mark - 1)

    indent = 0
    do idx = 1, len(stripped)
      if (stripped(idx:idx) /= ' ') exit
      indent = indent + 1
    end do

    line = ''
    if (len_trim(stripped) > 0) line = trim(adjustl(stripped))
  end subroutine preprocess_line

  subroutine split_key_value(line, key, value, has_value, status)
    character(len=*), intent(in) :: line
    character(len=STR_LEN), intent(out) :: key
    character(len=STR_LEN), intent(out) :: value
    logical, intent(out) :: has_value
    integer(int32), intent(out) :: status
    integer :: colon_pos

    key = ''
    value = ''
    has_value = .false.
    status = GLAMIN_OK

    colon_pos = index(line, ':')
    if (colon_pos == 0) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    key = trim(adjustl(line(:colon_pos - 1)))
    if (colon_pos < len_trim(line)) then
      value = trim(adjustl(line(colon_pos + 1:)))
      has_value = len_trim(value) > 0
    end if
  end subroutine split_key_value

  subroutine require_value(has_value, status)
    logical, intent(in) :: has_value
    integer(int32), intent(out) :: status

    if (has_value) then
      status = GLAMIN_OK
    else
      status = GLAMIN_ERR_INVALID_ARG
    end if
  end subroutine require_value

  subroutine set_string_array(list, raw_value, status)
    character(len=STR_LEN), allocatable, intent(inout) :: list(:)
    character(len=*), intent(in) :: raw_value
    integer(int32), intent(out) :: status
    character(len=STR_LEN) :: inner
    character(len=STR_LEN) :: item
    integer :: idx
    integer :: start_pos
    integer :: comma_pos

    status = GLAMIN_OK
    if (allocated(list)) deallocate(list)

    inner = trim(adjustl(raw_value))
    if (.not. starts_with(inner, '[') .or. inner(len_trim(inner):len_trim(inner)) /= ']') then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (len_trim(inner) <= 2) then
      allocate(list(0))
      return
    end if

    inner = trim(adjustl(inner(2:len_trim(inner) - 1)))
    start_pos = 1
    do
      comma_pos = index(inner(start_pos:), ',')
      if (comma_pos == 0) then
        item = trim(adjustl(inner(start_pos:)))
        call append_string(list, unquote(item))
        exit
      else
        idx = start_pos + comma_pos - 2
        item = trim(adjustl(inner(start_pos:idx)))
        call append_string(list, unquote(item))
        start_pos = idx + 2
      end if
    end do
  end subroutine set_string_array

  subroutine append_list_item(list, line, status)
    character(len=STR_LEN), allocatable, intent(inout) :: list(:)
    character(len=*), intent(in) :: line
    integer(int32), intent(out) :: status
    character(len=STR_LEN) :: item

    status = GLAMIN_OK
    if (.not. starts_with(line, '- ')) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    item = trim(unquote(adjustl(line(3:))))
    call append_string(list, item)
  end subroutine append_list_item

  subroutine append_string(list, value)
    character(len=STR_LEN), allocatable, intent(inout) :: list(:)
    character(len=*), intent(in) :: value
    character(len=STR_LEN) :: item

    item = ''
    item = trim(value)
    if (.not. allocated(list)) then
      allocate(list(1))
      list(1) = item
    else
      list = [list, item]
    end if
  end subroutine append_string

  subroutine append_objective(list, value)
    type(ObjectiveSpec), allocatable, intent(inout) :: list(:)
    type(ObjectiveSpec), intent(in) :: value

    if (.not. allocated(list)) then
      allocate(list(1))
      list(1) = value
    else
      list = [list, value]
    end if
  end subroutine append_objective

  subroutine append_space(list, value)
    type(SpaceSpec), allocatable, intent(inout) :: list(:)
    type(SpaceSpec), intent(in) :: value

    if (.not. allocated(list)) then
      allocate(list(1))
      list(1) = value
    else
      list = [list, value]
    end if
  end subroutine append_space

  subroutine append_invariant(list, value)
    type(InvariantSpec), allocatable, intent(inout) :: list(:)
    type(InvariantSpec), intent(in) :: value

    if (.not. allocated(list)) then
      allocate(list(1))
      list(1) = value
    else
      list = [list, value]
    end if
  end subroutine append_invariant

  subroutine append_mint(list, value)
    type(MintSpec), allocatable, intent(inout) :: list(:)
    type(MintSpec), intent(in) :: value

    if (.not. allocated(list)) then
      allocate(list(1))
      list(1) = value
    else
      list = [list, value]
    end if
  end subroutine append_mint

  subroutine append_corridor(list, value)
    type(CorridorSpec), allocatable, intent(inout) :: list(:)
    type(CorridorSpec), intent(in) :: value

    if (.not. allocated(list)) then
      allocate(list(1))
      list(1) = value
    else
      list = [list, value]
    end if
  end subroutine append_corridor

  subroutine append_trace(list, value)
    type(TraceSpec), allocatable, intent(inout) :: list(:)
    type(TraceSpec), intent(in) :: value

    if (.not. allocated(list)) then
      allocate(list(1))
      list(1) = value
    else
      list = [list, value]
    end if
  end subroutine append_trace

  subroutine parse_int32(raw_value, value, status)
    character(len=*), intent(in) :: raw_value
    integer(int32), intent(out) :: value
    integer(int32), intent(out) :: status
    integer :: io_status
    character(len=STR_LEN) :: text

    text = trim(unquote(raw_value))
    read(text, *, iostat=io_status) value
    if (io_status /= 0) then
      status = GLAMIN_ERR_INVALID_ARG
    else
      status = GLAMIN_OK
    end if
  end subroutine parse_int32

  subroutine parse_real64(raw_value, value, status)
    character(len=*), intent(in) :: raw_value
    real(real64), intent(out) :: value
    integer(int32), intent(out) :: status
    integer :: io_status
    character(len=STR_LEN) :: text

    text = trim(unquote(raw_value))
    read(text, *, iostat=io_status) value
    if (io_status /= 0) then
      status = GLAMIN_ERR_INVALID_ARG
    else
      status = GLAMIN_OK
    end if
  end subroutine parse_real64

  function unquote(value) result(cleaned)
    character(len=*), intent(in) :: value
    character(len=STR_LEN) :: cleaned
    character(len=STR_LEN) :: trimmed
    integer :: n

    cleaned = ''
    trimmed = trim(adjustl(value))
    n = len_trim(trimmed)
    if (n >= 2) then
      if ((trimmed(1:1) == '"' .and. trimmed(n:n) == '"') .or. &
          (trimmed(1:1) == '''' .and. trimmed(n:n) == '''')) then
        cleaned = trimmed(2:n - 1)
        return
      end if
    end if
    cleaned = trimmed
  end function unquote

  function lowercase(value) result(lowered)
    character(len=*), intent(in) :: value
    character(len=len(value)) :: lowered
    integer :: idx
    integer :: code

    lowered = value
    do idx = 1, len(value)
      code = iachar(value(idx:idx))
      if (code >= iachar('A') .and. code <= iachar('Z')) then
        lowered(idx:idx) = achar(code + 32)
      end if
    end do
  end function lowercase

  pure logical function starts_with(value, prefix)
    character(len=*), intent(in) :: value
    character(len=*), intent(in) :: prefix

    starts_with = len_trim(value) >= len_trim(prefix) .and. value(:len_trim(prefix)) == prefix(:len_trim(prefix))
  end function starts_with

  pure logical function is_token_char(ch)
    character(len=1), intent(in) :: ch
    integer :: code

    code = iachar(ch)
    is_token_char = (code >= iachar('a') .and. code <= iachar('z')) .or. &
      (code >= iachar('A') .and. code <= iachar('Z')) .or. &
      (code >= iachar('0') .and. code <= iachar('9')) .or. ch == '_'
  end function is_token_char

  function string_array_to_json(values) result(json)
    character(len=STR_LEN), intent(in) :: values(:)
    character(len=:), allocatable :: json
    integer :: idx

    json = '['
    do idx = 1, size(values)
      if (idx > 1) json = json // ','
      json = json // json_quote(values(idx))
    end do
    json = json // ']'
  end function string_array_to_json

  function objectives_to_json(values) result(json)
    type(ObjectiveSpec), intent(in) :: values(:)
    character(len=:), allocatable :: json
    integer :: idx
    logical :: first

    json = '['
    do idx = 1, size(values)
      if (idx > 1) json = json // ','
      json = json // '{'
      first = .true.
      call append_json_member(json, 'objective_id', json_quote(values(idx)%objective_id), first)
      if (len_trim(values(idx)%description) > 0) call append_json_member(json, 'description', &
        json_quote(values(idx)%description), first)
      call append_json_member(json, 'target_mints', string_array_to_json(values(idx)%target_mints), first)
      if (values(idx)%has_priority) call append_json_member(json, 'priority', &
        int_to_json(int(values(idx)%priority, int64)), first)
      json = json // '}'
    end do
    json = json // ']'
  end function objectives_to_json

  function spaces_to_json(values) result(json)
    type(SpaceSpec), intent(in) :: values(:)
    character(len=:), allocatable :: json
    integer :: idx

    json = '['
    do idx = 1, size(values)
      if (idx > 1) json = json // ','
      json = json // space_to_json(values(idx))
    end do
    json = json // ']'
  end function spaces_to_json

  function space_to_json(value) result(json)
    type(SpaceSpec), intent(in) :: value
    character(len=:), allocatable :: json
    logical :: first

    json = '{'
    first = .true.
    call append_json_member(json, 'space_id', json_quote(value%space_id), first)
    call append_json_member(json, 'dim', int_to_json(int(value%dim, int64)), first)
    call append_json_member(json, 'metric', json_quote(value%metric), first)
    call append_json_member(json, 'normalization', json_quote(value%normalization), first)
    if (allocated(value%transform_chain)) call append_json_member(json, 'transform_chain', &
      string_array_to_json(value%transform_chain), first)
    if (allocated(value%invariants)) call append_json_member(json, 'invariants', &
      invariants_to_json(value%invariants), first)
    json = json // '}'
  end function space_to_json

  function invariants_to_json(values) result(json)
    type(InvariantSpec), intent(in) :: values(:)
    character(len=:), allocatable :: json
    integer :: idx
    logical :: first

    json = '['
    do idx = 1, size(values)
      if (idx > 1) json = json // ','
      json = json // '{'
      first = .true.
      call append_json_member(json, 'type', json_quote(values(idx)%type_name), first)
      if (values(idx)%has_equals) call append_json_member(json, 'equals', &
        json_number_or_string(values(idx)%equals), first)
      if (values(idx)%has_allowed) call append_json_member(json, 'allowed', &
        string_array_to_json(values(idx)%allowed), first)
      if (values(idx)%has_min) call append_json_member(json, 'min', real_to_json(values(idx)%min_value), first)
      if (values(idx)%has_max) call append_json_member(json, 'max', real_to_json(values(idx)%max_value), first)
      json = json // '}'
    end do
    json = json // ']'
  end function invariants_to_json

  function mints_to_json(values) result(json)
    type(MintSpec), intent(in) :: values(:)
    character(len=:), allocatable :: json
    integer :: idx
    logical :: first

    json = '['
    do idx = 1, size(values)
      if (idx > 1) json = json // ','
      json = json // '{'
      first = .true.
      call append_json_member(json, 'mint_id', json_quote(values(idx)%mint_id), first)
      call append_json_member(json, 'space_id', json_quote(values(idx)%space_id), first)
      call append_json_member(json, 'text', json_quote(values(idx)%text), first)
      if (allocated(values(idx)%tags)) call append_json_member(json, 'tags', &
        string_array_to_json(values(idx)%tags), first)
      json = json // '}'
    end do
    json = json // ']'
  end function mints_to_json

  function corridors_to_json(values) result(json)
    type(CorridorSpec), intent(in) :: values(:)
    character(len=:), allocatable :: json
    integer :: idx
    logical :: first

    json = '['
    do idx = 1, size(values)
      if (idx > 1) json = json // ','
      json = json // '{'
      first = .true.
      call append_json_member(json, 'corridor_id', json_quote(values(idx)%corridor_id), first)
      call append_json_member(json, 'space_id', json_quote(values(idx)%space_id), first)
      call append_json_member(json, 'between', string_array_to_json(values(idx)%between), first)
      call append_json_member(json, 'width', real_to_json(values(idx)%width), first)
      if (len_trim(values(idx)%risk_profile) > 0) call append_json_member(json, 'risk_profile', &
        json_quote(values(idx)%risk_profile), first)
      if (len_trim(values(idx)%notes) > 0) call append_json_member(json, 'notes', &
        json_quote(values(idx)%notes), first)
      json = json // '}'
    end do
    json = json // ']'
  end function corridors_to_json

  function traces_to_json(values) result(json)
    type(TraceSpec), intent(in) :: values(:)
    character(len=:), allocatable :: json
    integer :: idx
    logical :: first

    json = '['
    do idx = 1, size(values)
      if (idx > 1) json = json // ','
      json = json // '{'
      first = .true.
      call append_json_member(json, 'trace_id', json_quote(values(idx)%trace_id), first)
      call append_json_member(json, 'space_id', json_quote(values(idx)%space_id), first)
      call append_json_member(json, 'steps', string_array_to_json(values(idx)%steps), first)
      if (values(idx)%has_confidence) call append_json_member(json, 'confidence', &
        real_to_json(values(idx)%confidence), first)
      if (allocated(values(idx)%timestamps)) call append_json_member(json, 'timestamps', &
        string_array_to_json(values(idx)%timestamps), first)
      if (len_trim(values(idx)%notes) > 0) call append_json_member(json, 'notes', &
        json_quote(values(idx)%notes), first)
      json = json // '}'
    end do
    json = json // ']'
  end function traces_to_json

  function embedder_to_json(value, include_signature) result(json)
    type(EmbedderAuthoring), intent(in) :: value
    logical, intent(in), optional :: include_signature
    character(len=:), allocatable :: json
    logical :: first
    logical :: include_sig

    include_sig = .true.
    if (present(include_signature)) include_sig = include_signature

    json = '{'
    first = .true.
    call append_json_member(json, 'id', json_quote(value%embedder_id), first)
    call append_json_member(json, 'version', json_quote(value%embedder_version), first)
    call append_json_member(json, 'input_schema', json_quote(value%input_schema), first)
    call append_json_member(json, 'preprocess_chain', string_array_to_json(value%preprocess_chain), first)
    call append_json_member(json, 'model_hash', json_quote(value%model_hash), first)
    call append_json_member(json, 'config_hash', json_quote(value%config_hash), first)
    call append_json_member(json, 'hardware_class', json_quote(value%hardware_class), first)
    call append_json_member(json, 'min_ram_mb', int_to_json(int(value%min_ram_mb, int64)), first)
    call append_json_member(json, 'min_vram_mb', int_to_json(int(value%min_vram_mb, int64)), first)
    if (include_sig .and. len_trim(value%signature) > 0) call append_json_member(json, 'signature', &
      json_quote(value%signature), first)
    json = json // '}'
  end function embedder_to_json

  subroutine append_json_member(json, key, value, first)
    character(len=:), allocatable, intent(inout) :: json
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    logical, intent(inout) :: first

    if (.not. first) json = json // ','
    json = json // json_pair(key, value)
    first = .false.
  end subroutine append_json_member

  function json_pair(key, value) result(pair)
    character(len=*), intent(in) :: key
    character(len=*), intent(in) :: value
    character(len=:), allocatable :: pair

    pair = json_quote(key) // ': ' // value
  end function json_pair

  function json_quote(value) result(json)
    character(len=*), intent(in) :: value
    character(len=:), allocatable :: json

    json = '"' // escape_json(trim(value)) // '"'
  end function json_quote

  function json_number_or_string(value) result(json)
    character(len=*), intent(in) :: value
    character(len=:), allocatable :: json
    real(real64) :: parsed
    integer :: io_status
    character(len=STR_LEN) :: text

    text = trim(value)
    read(text, *, iostat=io_status) parsed
    if (io_status == 0) then
      json = trim(value)
    else
      json = json_quote(value)
    end if
  end function json_number_or_string

  function escape_json(value) result(escaped)
    character(len=*), intent(in) :: value
    character(len=:), allocatable :: escaped
    integer :: idx

    escaped = ''
    do idx = 1, len_trim(value)
      select case (value(idx:idx))
      case ('\')
        escaped = escaped // '\\'
      case ('"')
        escaped = escaped // '\"'
      case (new_line('a'))
        escaped = escaped // '\n'
      case default
        escaped = escaped // value(idx:idx)
      end select
    end do
  end function escape_json

  function int_to_json(value) result(json)
    integer(int64), intent(in) :: value
    character(len=:), allocatable :: json
    character(len=64) :: buffer

    write(buffer, '(I0)') value
    json = trim(buffer)
  end function int_to_json

  function real_to_json(value) result(json)
    real(real64), intent(in) :: value
    character(len=:), allocatable :: json
    character(len=64) :: buffer

    write(buffer, '(G0.12)') value
    json = trim(adjustl(buffer))
  end function real_to_json

  function join_path(root, child) result(path)
    character(len=*), intent(in) :: root
    character(len=*), intent(in) :: child
    character(len=:), allocatable :: path

    if (len_trim(root) == 0) then
      path = trim(child)
    else if (root(len_trim(root):len_trim(root)) == '/') then
      path = trim(root) // trim(child)
    else
      path = trim(root) // '/' // trim(child)
    end if
  end function join_path

  subroutine ensure_directory(path, status)
    character(len=*), intent(in) :: path
    integer(int32), intent(out) :: status
    integer :: cmd_status
    integer :: exit_status
    character(len=:), allocatable :: command

    if (len_trim(path) == 0) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    command = 'mkdir -p "' // trim(path) // '"'
    call execute_command_line(command, wait=.true., exitstat=exit_status, cmdstat=cmd_status)
    if (cmd_status /= 0 .or. exit_status /= 0) then
      status = GLAMIN_ERR_UNKNOWN
    else
      status = GLAMIN_OK
    end if
  end subroutine ensure_directory

  subroutine ensure_parent_directory(path, status)
    character(len=*), intent(in) :: path
    integer(int32), intent(out) :: status
    integer :: slash_pos

    slash_pos = scan(trim(path), '/', back=.true.)
    if (slash_pos <= 0) then
      status = GLAMIN_OK
      return
    end if

    call ensure_directory(path(:slash_pos - 1), status)
  end subroutine ensure_parent_directory

  subroutine write_text_file(path, content, status)
    character(len=*), intent(in) :: path
    character(len=*), intent(in) :: content
    integer(int32), intent(out) :: status
    integer :: unit_id
    integer :: io_status

    open(newunit=unit_id, file=trim(path), status='replace', action='write', iostat=io_status)
    if (io_status /= 0) then
      status = GLAMIN_ERR_UNKNOWN
      return
    end if
    write(unit_id, '(A)', iostat=io_status) content
    close(unit_id)
    if (io_status /= 0) then
      status = GLAMIN_ERR_UNKNOWN
    else
      status = GLAMIN_OK
    end if
  end subroutine write_text_file

  integer function find_space_index(spec, space_id) result(idx)
    type(GeometrySpec), intent(in) :: spec
    character(len=*), intent(in) :: space_id
    integer :: pos

    idx = 0
    if (.not. allocated(spec%spaces)) return
    do pos = 1, size(spec%spaces)
      if (trim(spec%spaces(pos)%space_id) == trim(space_id)) then
        idx = pos
        return
      end if
    end do
  end function find_space_index

  integer function find_mint_index(spec, mint_id) result(idx)
    type(GeometrySpec), intent(in) :: spec
    character(len=*), intent(in) :: mint_id
    integer :: pos

    idx = 0
    if (.not. allocated(spec%mints)) return
    do pos = 1, size(spec%mints)
      if (trim(spec%mints(pos)%mint_id) == trim(mint_id)) then
        idx = pos
        return
      end if
    end do
  end function find_mint_index

  integer(int64) function count_space_mints(spec, space_id) result(count)
    type(GeometrySpec), intent(in) :: spec
    character(len=*), intent(in) :: space_id
    integer :: idx

    count = 0_int64
    do idx = 1, size(spec%mints)
      if (trim(spec%mints(idx)%space_id) == trim(space_id)) count = count + 1_int64
    end do
  end function count_space_mints
end module glamin_geometry_spec_compiler
