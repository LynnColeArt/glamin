module glamin_index_pq
  use iso_fortran_env, only: int32, int64
  use iso_c_binding, only: c_associated, c_f_pointer, c_loc, c_null_ptr, c_ptr
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG, GLAMIN_ERR_OOM
  use glamin_memory, only: free_aligned
  use glamin_types, only: VectorBlock, IndexHandle, INDEX_KIND_PQ, INDEX_KIND_UNKNOWN
  implicit none
  private

  public :: ProductQuantizerCodebook
  public :: PQIndex
  public :: pq_create_handle
  public :: pq_destroy_handle
  public :: pq_handle
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

  type :: PQIndex
    integer(int32) :: dim = 0
    integer(int32) :: metric = 0
    integer(int32) :: m = 0
    integer(int32) :: nbits = 0
    integer(int32) :: code_size = 0
    integer(int64) :: ntotal = 0
    logical :: is_trained = .false.
    integer(int32) :: search_type = 0
    logical :: encode_signs = .false.
    integer(int32) :: polysemous_ht = 0
    type(VectorBlock) :: codebooks
    type(VectorBlock) :: codes
  end type PQIndex

contains
  subroutine pq_create_handle(handle, dim, m, nbits, metric, status)
    type(IndexHandle), intent(out) :: handle
    integer(int32), intent(in) :: dim
    integer(int32), intent(in) :: m
    integer(int32), intent(in) :: nbits
    integer(int32), intent(in) :: metric
    integer(int32), intent(out) :: status
    type(PQIndex), pointer :: pq_index
    integer(int32) :: alloc_status
    integer(int32) :: code_size

    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_PQ

    if (dim <= 0_int32 .or. m <= 0_int32 .or. nbits <= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (mod(dim, m) /= 0_int32) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    code_size = int((nbits * m + 7_int32) / 8_int32, int32)

    allocate(pq_index, stat=alloc_status)
    if (alloc_status /= 0_int32) then
      status = GLAMIN_ERR_OOM
      return
    end if

    pq_index%dim = dim
    pq_index%metric = metric
    pq_index%m = m
    pq_index%nbits = nbits
    pq_index%code_size = code_size
    pq_index%ntotal = 0_int64
    pq_index%is_trained = .false.
    pq_index%search_type = 0_int32
    pq_index%encode_signs = .false.
    pq_index%polysemous_ht = 0_int32
    pq_index%codebooks = VectorBlock()
    pq_index%codes = VectorBlock()

    handle%impl = c_loc(pq_index)
    handle%kind = INDEX_KIND_PQ
    status = GLAMIN_OK
  end subroutine pq_create_handle

  subroutine pq_destroy_handle(handle, status)
    type(IndexHandle), intent(inout) :: handle
    integer(int32), intent(out) :: status
    type(PQIndex), pointer :: pq_index
    integer(int32) :: free_status
    integer(int32) :: alloc_status

    call pq_handle(handle, pq_index)
    if (.not. associated(pq_index)) then
      status = GLAMIN_ERR_INVALID_ARG
      return
    end if

    if (c_associated(pq_index%codebooks%data)) then
      call free_aligned(pq_index%codebooks%data, free_status)
    end if
    if (c_associated(pq_index%codes%data)) then
      call free_aligned(pq_index%codes%data, free_status)
    end if

    deallocate(pq_index, stat=alloc_status)
    handle%impl = c_null_ptr
    handle%kind = INDEX_KIND_UNKNOWN
    status = GLAMIN_OK
  end subroutine pq_destroy_handle

  subroutine pq_handle(index_handle, pq_index)
    type(IndexHandle), intent(in) :: index_handle
    type(PQIndex), pointer :: pq_index

    pq_index => null()
    if (.not. c_associated(index_handle%impl)) then
      return
    end if

    if (index_handle%kind /= INDEX_KIND_PQ) then
      return
    end if

    call c_f_pointer(index_handle%impl, pq_index)
  end subroutine pq_handle

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
