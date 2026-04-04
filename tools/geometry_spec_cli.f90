program geometry_spec_cli
  use iso_fortran_env, only: error_unit, int32
  use glamin_errors, only: GLAMIN_OK
  use glamin_geometry_spec_compiler, only: canonicalize_geometry_spec_file, compile_geometry_spec, &
    embed_geometry_spec_file, validate_geometry_spec_file, visualize_geometry_spec_file
  implicit none

  integer(int32) :: status
  character(len=512) :: command
  character(len=512) :: spec_path
  character(len=512) :: output_path
  character(len=512) :: flag
  integer :: argc

  argc = command_argument_count()
  if (argc < 2) then
    call print_usage()
    stop 1
  end if

  call get_command_argument(1, command)
  call get_command_argument(2, spec_path)

  select case (trim(command))
  case ('validate')
    call validate_geometry_spec_file(trim(spec_path), status)
  case ('compile')
    output_path = 'build/specs'
    if (argc >= 4) then
      call get_command_argument(3, flag)
      call get_command_argument(4, output_path)
    end if
    call compile_geometry_spec(trim(spec_path), trim(output_path), status)
  case ('canonicalize')
    output_path = 'build/specs/spec.json'
    if (argc >= 4) then
      call get_command_argument(3, flag)
      call get_command_argument(4, output_path)
    end if
    call canonicalize_geometry_spec_file(trim(spec_path), trim(output_path), status)
  case ('embed')
    output_path = 'build/specs/vectors.bin'
    if (argc >= 4) then
      call get_command_argument(3, flag)
      call get_command_argument(4, output_path)
    end if
    call embed_geometry_spec_file(trim(spec_path), trim(output_path), status)
  case ('visualize')
    output_path = 'build/specs/spec.dot'
    if (argc >= 4) then
      call get_command_argument(3, flag)
      call get_command_argument(4, output_path)
    end if
    call visualize_geometry_spec_file(trim(spec_path), trim(output_path), status)
  case default
    call print_usage()
    stop 1
  end select

  if (status /= GLAMIN_OK) then
    write(error_unit, '(A,I0)') 'glamin_spec_tool failed with status ', status
    stop 1
  end if

contains
  subroutine print_usage()
    write(error_unit, '(A)') 'usage: glamin_spec_tool <validate|compile|canonicalize|embed|visualize> ' // &
      '<spec> [--out-dir|--output <path>]'
  end subroutine print_usage
end program geometry_spec_cli
