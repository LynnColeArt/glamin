module glamin_index_pq
  use iso_fortran_env, only: int32
  use glamin_types, only: VectorBlock
  implicit none
  private

  public :: ProductQuantizerCodebook
  public :: pq_create
  public :: pq_train
  public :: pq_encode
  public :: pq_decode

  type :: ProductQuantizerCodebook
    integer(int32) :: dim = 0
    integer(int32) :: m = 0
    integer(int32) :: ksub = 0
    type(VectorBlock) :: codebooks
  end type ProductQuantizerCodebook

contains
  subroutine pq_create(codebook, dim, m, ksub)
    type(ProductQuantizerCodebook), intent(out) :: codebook
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: ksub
    error stop "pq_create not implemented"
  end subroutine pq_create

  subroutine pq_train(codebook, vectors)
    type(ProductQuantizerCodebook), intent(inout) :: codebook
    type(VectorBlock), intent(in) :: vectors
    error stop "pq_train not implemented"
  end subroutine pq_train

  subroutine pq_encode(codebook, vectors, codes)
    type(ProductQuantizerCodebook), intent(in) :: codebook
    type(VectorBlock), intent(in) :: vectors
    type(VectorBlock), intent(out) :: codes
    error stop "pq_encode not implemented"
  end subroutine pq_encode

  subroutine pq_decode(codebook, codes, vectors)
    type(ProductQuantizerCodebook), intent(in) :: codebook
    type(VectorBlock), intent(in) :: codes
    type(VectorBlock), intent(out) :: vectors
    error stop "pq_decode not implemented"
  end subroutine pq_decode
end module glamin_index_pq
