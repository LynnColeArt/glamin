module glamin_embedder
  use iso_fortran_env, only: int32, int64
  use glamin_metrics, only: METRIC_L2
  implicit none
  private

  public :: EmbedderSpec
  public :: EmbedderContract
  public :: HardwareProfile
  public :: EMBEDDER_ID_LEN
  public :: EMBEDDER_VERSION_LEN
  public :: EMBEDDER_SCHEMA_LEN
  public :: EMBEDDER_CHAIN_LEN
  public :: EMBEDDER_HASH_LEN
  public :: EMBEDDER_SIGNATURE_LEN
  public :: EMBEDDER_OWNER_LEN
  public :: EMBEDDER_HARDWARE_LEN
  public :: is_embedder_compatible
  public :: is_embedder_supported
  public :: has_embedder_signature

  integer, parameter :: EMBEDDER_ID_LEN = 64
  integer, parameter :: EMBEDDER_VERSION_LEN = 32
  integer, parameter :: EMBEDDER_SCHEMA_LEN = 128
  integer, parameter :: EMBEDDER_CHAIN_LEN = 256
  integer, parameter :: EMBEDDER_HASH_LEN = 128
  integer, parameter :: EMBEDDER_SIGNATURE_LEN = 256
  integer, parameter :: EMBEDDER_OWNER_LEN = 64
  integer, parameter :: EMBEDDER_HARDWARE_LEN = 16

  type :: EmbedderSpec
    character(len=EMBEDDER_ID_LEN) :: embedder_id = ''
    character(len=EMBEDDER_VERSION_LEN) :: embedder_version = ''
    character(len=EMBEDDER_SCHEMA_LEN) :: input_schema = ''
    character(len=EMBEDDER_CHAIN_LEN) :: preprocess_chain = ''
    character(len=EMBEDDER_HASH_LEN) :: model_hash = ''
    character(len=EMBEDDER_HASH_LEN) :: config_hash = ''
    character(len=EMBEDDER_HARDWARE_LEN) :: hardware_class = ''
    integer(int32) :: min_ram_mb = 0
    integer(int32) :: min_vram_mb = 0
    integer(int32) :: dim = 0
    integer(int32) :: metric = METRIC_L2
    integer(int32) :: normalization = 0
    integer(int64) :: created_at = 0
    character(len=EMBEDDER_OWNER_LEN) :: owner = ''
  end type EmbedderSpec

  type :: EmbedderContract
    type(EmbedderSpec) :: spec
    character(len=EMBEDDER_HASH_LEN) :: contract_hash = ''
    character(len=EMBEDDER_SIGNATURE_LEN) :: signature = ''
    character(len=EMBEDDER_OWNER_LEN) :: signer = ''
  end type EmbedderContract

  type :: HardwareProfile
    integer(int32) :: ram_mb = 0
    integer(int32) :: vram_mb = 0
    logical :: has_gpu = .false.
    logical :: has_npu = .false.
  end type HardwareProfile

contains
  pure logical function is_embedder_compatible(left, right)
    type(EmbedderSpec), intent(in) :: left
    type(EmbedderSpec), intent(in) :: right

    is_embedder_compatible = .false.
    if (trim(left%embedder_id) /= trim(right%embedder_id)) return
    if (trim(left%embedder_version) /= trim(right%embedder_version)) return
    if (left%dim /= right%dim) return
    if (left%metric /= right%metric) return
    if (left%normalization /= right%normalization) return
    if (trim(left%preprocess_chain) /= trim(right%preprocess_chain)) return
    if (trim(left%model_hash) /= trim(right%model_hash)) return
    if (trim(left%config_hash) /= trim(right%config_hash)) return
    is_embedder_compatible = .true.
  end function is_embedder_compatible

  pure logical function is_embedder_supported(spec, profile)
    type(EmbedderSpec), intent(in) :: spec
    type(HardwareProfile), intent(in) :: profile
    character(len=EMBEDDER_HARDWARE_LEN) :: required_class

    is_embedder_supported = .false.
    if (spec%min_ram_mb > profile%ram_mb) return
    if (spec%min_vram_mb > profile%vram_mb) return

    required_class = normalize_hardware_class(spec%hardware_class)
    if (len_trim(required_class) == 0) required_class = 'cpu'

    select case (trim(required_class))
    case ('any')
      is_embedder_supported = .true.
    case ('cpu')
      is_embedder_supported = .true.
    case ('gpu')
      is_embedder_supported = profile%has_gpu
    case ('npu')
      is_embedder_supported = profile%has_npu
    case ('hybrid')
      is_embedder_supported = profile%has_gpu .and. profile%has_npu
    case default
      is_embedder_supported = .false.
    end select
  end function is_embedder_supported

  pure logical function has_embedder_signature(contract)
    type(EmbedderContract), intent(in) :: contract

    has_embedder_signature = len_trim(contract%signature) > 0
  end function has_embedder_signature

  pure function normalize_hardware_class(value) result(normalized)
    character(len=*), intent(in) :: value
    character(len=EMBEDDER_HARDWARE_LEN) :: normalized
    integer :: idx
    integer :: code

    normalized = ''
    do idx = 1, min(len_trim(value), EMBEDDER_HARDWARE_LEN)
      code = iachar(value(idx:idx))
      if (code >= iachar('A') .and. code <= iachar('Z')) then
        normalized(idx:idx) = achar(code + 32)
      else
        normalized(idx:idx) = value(idx:idx)
      end if
    end do
    normalized = adjustl(normalized)
  end function normalize_hardware_class
end module glamin_embedder
