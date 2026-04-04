program spec_compiler_smoke
  use iso_fortran_env, only: int32
  use glamin_errors, only: GLAMIN_OK
  use glamin_geometry_spec_compiler, only: compile_geometry_spec
  implicit none

  character(len=*), parameter :: SPEC_PATH = 'tests/fixtures/geometry_spec_metadata.yaml'
  character(len=*), parameter :: OUT_DIR = 'build/spec_smoke'
  character(len=*), parameter :: MANIFEST_PATH = 'build/spec_smoke/manifest.json'

  integer(int32) :: status
  character(len=:), allocatable :: manifest

  call compile_geometry_spec(SPEC_PATH, OUT_DIR, status)
  if (status /= GLAMIN_OK) then
    error stop 'compile_geometry_spec failed'
  end if

  call read_text_file(MANIFEST_PATH, manifest, status)
  if (status /= GLAMIN_OK) then
    error stop 'failed to read emitted manifest'
  end if

  if (index(manifest, '"metadata": {"role": "allow","score": 7,"enabled": true,"flags": [true,false,"audit"],' // &
      '"policy": {"retries": 3,"mode": "strict","gates": {"alpha": true,"beta": false},' // &
      '"labels": {"primary": "native","secondary": "compiler"}}}') == 0) then
    error stop 'mint metadata missing from manifest'
  end if

  if (index(manifest, '"text": "Start metadata flow: #keep"') == 0) then
    error stop 'quoted hash/colon text missing from manifest'
  end if

  if (index(manifest, '"metadata": {"owner": "runtime","weight": 0.75,"tags": ["critical","native"],' // &
      '"routing": {"kind": "staged:canary #1","checks": {"lint": true,"perf": false}}}') == 0) then
    error stop 'trace metadata missing from manifest'
  end if

  if (index(manifest, '"notes": "Trace note: keep #literal"') == 0) then
    error stop 'quoted hash trace notes missing from manifest'
  end if

  write(*, '(A)') 'spec compiler smoke ok'

contains
  subroutine read_text_file(path, content, status)
    character(len=*), intent(in) :: path
    character(len=:), allocatable, intent(out) :: content
    integer(int32), intent(out) :: status
    integer :: unit_id
    integer :: io_status
    character(len=4096) :: line

    content = ''
    status = GLAMIN_OK

    open(newunit=unit_id, file=path, status='old', action='read', iostat=io_status)
    if (io_status /= 0) then
      status = 2_int32
      return
    end if

    do
      read(unit_id, '(A)', iostat=io_status) line
      if (io_status /= 0) exit
      content = content // trim(line)
    end do

    close(unit_id)
  end subroutine read_text_file
end program spec_compiler_smoke
