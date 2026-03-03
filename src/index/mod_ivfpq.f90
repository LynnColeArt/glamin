module glamin_index_ivfpq
  use iso_fortran_env, only: int32
  use iso_c_binding, only: c_ptr, c_null_ptr
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: IvfProductQuantizerIndex
  public :: ivfpq_create
  public :: ivfpq_train
  public :: ivfpq_add
  public :: ivfpq_search

  type :: IvfProductQuantizerIndex
    integer(int32) :: dim = 0
    integer(int32) :: nlist = 0
    integer(int32) :: m = 0
    integer(int32) :: ksub = 0
    integer(int32) :: metric = 0
    type(VectorBlock) :: centroids
    type(VectorBlock) :: codebooks
    type(c_ptr) :: lists = c_null_ptr
  end type IvfProductQuantizerIndex

contains
  subroutine ivfpq_create(index, dim, nlist, m, ksub, metric)
    type(IvfProductQuantizerIndex), intent(out) :: index
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: nlist
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: ksub
    integer(int32), intent(in) :: metric
    error stop "ivfpq_create not implemented"
  end subroutine ivfpq_create

  subroutine ivfpq_train(index, vectors)
    type(IvfProductQuantizerIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    error stop "ivfpq_train not implemented"
  end subroutine ivfpq_train

  subroutine ivfpq_add(index, vectors)
    type(IvfProductQuantizerIndex), intent(inout) :: index
    type(VectorBlock), intent(in) :: vectors
    error stop "ivfpq_add not implemented"
  end subroutine ivfpq_add

  subroutine ivfpq_search(index, queries, k, nprobe, distances, labels)
    type(IvfProductQuantizerIndex), intent(in) :: index
    type(VectorBlock), intent(in) :: queries
    integer(int32), intent(in) :: k
    integer(int32), intent(in) :: nprobe
    type(VectorBlock), intent(out) :: distances
    type(VectorBlock), intent(out) :: labels
    error stop "ivfpq_search not implemented"
  end subroutine ivfpq_search
end module glamin_index_ivfpq
