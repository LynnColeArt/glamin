module glamin_index_hnsw
  use iso_fortran_env, only: int32
  use iso_c_binding, only: c_ptr, c_null_ptr
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: HnswIndex
  public :: hnsw_create
  public :: hnsw_add
  public :: hnsw_search

  type :: HnswIndex
    integer(int32) :: dim = 0
    integer(int32) :: m = 0
    integer(int32) :: ef_construction = 0
    integer(int32) :: metric = 0
    type(c_ptr) :: graph = c_null_ptr
    type(VectorBlock) :: data
  end type HnswIndex

contains
  subroutine hnsw_create(index, dim, m, ef_construction, metric)
    type(HnswIndex), intent(out) :: index
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: ef_construction
    integer(int32), intent(in) :: metric
    error stop "hnsw_create not implemented"
  end subroutine hnsw_create

  subroutine hnsw_add(index, vectors)
    type(HnswIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    error stop "hnsw_add not implemented"
  end subroutine hnsw_add

  subroutine hnsw_search(index, queries, k, ef_search, distances, labels)
    type(HnswIndex), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    integer(int32), intent(in) :: ef_search
    type(VectorBlock), intent(out) :: distances
    type(VectorBlock), intent(out) :: labels
    error stop "hnsw_search not implemented"
  end subroutine hnsw_search
end module glamin_index_hnsw
