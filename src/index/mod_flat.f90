module glamin_index_flat
  use iso_fortran_env, only: int32
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: FlatIndex
  public :: flat_create
  public :: flat_add
  public :: flat_search

  type :: FlatIndex
    integer(int32) :: dim = 0
    integer(int32) :: metric = 0
    type(VectorBlock) :: data
  end type FlatIndex

contains
  subroutine flat_create(index, dim, metric)
    type(FlatIndex), intent(out) :: index
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: metric
    error stop "flat_create not implemented"
  end subroutine flat_create

  subroutine flat_add(index, vectors)
    type(FlatIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    error stop "flat_add not implemented"
  end subroutine flat_add

  subroutine flat_search(index, queries, k, distances, labels)
    type(FlatIndex), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    type(VectorBlock), intent(out) :: distances
    type(VectorBlock), intent(out) :: labels
    error stop "flat_search not implemented"
  end subroutine flat_search
end module glamin_index_flat
