module glamin_index_ivf
  use iso_fortran_env, only: int32
  use iso_c_binding, only: c_ptr, c_null_ptr
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: IvfIndex
  public :: ivf_create
  public :: ivf_train
  public :: ivf_add
  public :: ivf_search

  type :: IvfIndex
    integer(int32) :: dim = 0
    integer(int32) :: nlist = 0
    integer(int32) :: metric = 0
    type(VectorBlock) :: centroids
    type(c_ptr) :: lists = c_null_ptr
  end type IvfIndex

contains
  subroutine ivf_create(index, dim, nlist, metric)
    type(IvfIndex), intent(out) :: index
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: nlist
    integer(int32), intent(in) :: metric
    error stop "ivf_create not implemented"
  end subroutine ivf_create

  subroutine ivf_train(index, vectors)
    type(IvfIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    error stop "ivf_train not implemented"
  end subroutine ivf_train

  subroutine ivf_add(index, vectors)
    type(IvfIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    error stop "ivf_add not implemented"
  end subroutine ivf_add

  subroutine ivf_search(index, queries, k, nprobe, distances, labels)
    type(IvfIndex), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    integer(int32), intent(in) :: nprobe
    type(VectorBlock), intent(out) :: distances
    type(VectorBlock), intent(out) :: labels
    error stop "ivf_search not implemented"
  end subroutine ivf_search
end module glamin_index_ivf
