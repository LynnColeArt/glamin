module glamin_faiss_io
  use glamin_types, only: IndexHandle
  use glamin_stream, only: IoStream
  implicit none
  private

  public :: load_faiss_index
  public :: save_faiss_index

contains
  subroutine load_faiss_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(out) :: index
    error stop "load_faiss_index not implemented"
  end subroutine load_faiss_index

  subroutine save_faiss_index(stream, index)
    type(IoStream), intent(inout) :: stream
    type(IndexHandle), intent(in) :: index
    error stop "save_faiss_index not implemented"
  end subroutine save_faiss_index
end module glamin_faiss_io
