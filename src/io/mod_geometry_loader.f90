module glamin_geometry_loader
  use iso_fortran_env, only: int32, int64
  use glamin_errors, only: GLAMIN_OK, GLAMIN_ERR_INVALID_ARG
  use glamin_contracts, only: load_embedder_contract, validate_embedder_contract
  use glamin_embedder, only: EmbedderContract
  use glamin_index_flat, only: flat_add, flat_create_handle
  use glamin_geometry_layout, only: load_vector_layout
  use glamin_types, only: IndexHandle, VectorBlock
  use glamin_vector_io, only: free_vector_block, load_vector_block_slice
  implicit none
  private

  public :: load_flat_from_layout

contains
  subroutine load_flat_from_layout(layout_path, vectors_path, space_id, metric, index, status, &
      contracts_path)
    character(len=*), intent(in) :: layout_path
    character(len=*), intent(in) :: vectors_path
    character(len=*), intent(in) :: space_id
    integer(int32), intent(in) :: metric
    type(IndexHandle), intent(out) :: index
    integer(int32), intent(out) :: status
    character(len=*), intent(in), optional :: contracts_path
    integer(int32) :: dim
    integer(int64) :: count
    integer(int64) :: offset_bytes
    type(VectorBlock) :: vectors
    integer(int32) :: free_status
    type(EmbedderContract) :: embedder_contract

    status = GLAMIN_OK
    vectors = VectorBlock()
    embedder_contract = EmbedderContract()

    if (present(contracts_path)) then
      if (len_trim(contracts_path) == 0) then
        status = GLAMIN_ERR_INVALID_ARG
        return
      end if

      call load_embedder_contract(contracts_path, embedder_contract, status)
      if (status /= GLAMIN_OK) then
        return
      end if

      call validate_embedder_contract(embedder_contract, status)
      if (status /= GLAMIN_OK) then
        return
      end if
    end if

    call load_vector_layout(layout_path, space_id, dim, count, offset_bytes, status)
    if (status /= GLAMIN_OK) then
      return
    end if

    call load_vector_block_slice(vectors_path, dim, count, offset_bytes, vectors, status)
    if (status /= GLAMIN_OK) then
      return
    end if

    call flat_create_handle(index, dim, metric, status)
    if (status /= GLAMIN_OK) then
      call free_vector_block(vectors, free_status)
      return
    end if

    call flat_add(index, vectors, status)
    call free_vector_block(vectors, free_status)
    if (status == GLAMIN_OK .and. free_status /= GLAMIN_OK) then
      status = free_status
    end if
  end subroutine load_flat_from_layout
end module glamin_geometry_loader
