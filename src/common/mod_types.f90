module glamin_types
  use iso_fortran_env, only: int32, int64
  use iso_c_binding, only: c_ptr, c_null_ptr
  implicit none
  private

  public :: Request
  public :: VectorBlock
  public :: SearchPlan
  public :: IndexHandle
  public :: INDEX_KIND_UNKNOWN
  public :: INDEX_KIND_FLAT
  public :: INDEX_KIND_PQ
  public :: INDEX_KIND_IVF
  public :: INDEX_KIND_IVFPQ
  public :: INDEX_KIND_HNSW

  type :: Request
    integer(int64) :: id = 0
    integer(int32) :: status = 0
    integer(int32) :: error_code = 0
    type(c_ptr) :: payload = c_null_ptr
  end type Request

  type :: VectorBlock
    type(c_ptr) :: data = c_null_ptr
    integer(int64) :: length = 0
    integer(int32) :: dim = 0
    integer(int32) :: stride = 0
    integer(int32) :: elem_size = 0
    integer(int32) :: alignment = 0
  end type VectorBlock

  type :: SearchPlan
    integer(int32) :: k = 0
    integer(int32) :: nprobe = 0
    integer(int32) :: batch_size = 0
    integer(int32) :: metric = 0
  end type SearchPlan

  enum, bind(c)
    enumerator :: INDEX_KIND_UNKNOWN = 0
    enumerator :: INDEX_KIND_FLAT = 1
    enumerator :: INDEX_KIND_PQ = 2
    enumerator :: INDEX_KIND_IVF = 3
    enumerator :: INDEX_KIND_IVFPQ = 4
    enumerator :: INDEX_KIND_HNSW = 5
  end enum

  type :: IndexHandle
    type(c_ptr) :: impl = c_null_ptr
    integer(int32) :: kind = INDEX_KIND_UNKNOWN
  end type IndexHandle
end module glamin_types
