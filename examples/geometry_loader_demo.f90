program geometry_loader_demo
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_loc, c_ptr
  use glamin_errors, only: GLAMIN_OK
  use glamin_geometry_layout, only: load_vector_layout
  use glamin_geometry_loader, only: load_flat_from_layout
  use glamin_metrics, only: METRIC_L2
  use glamin_types, only: IndexHandle, VectorBlock
  use glamin_index_flat, only: flat_search, flat_destroy_handle
  implicit none

  type(IndexHandle) :: index
  integer(int32) :: status
  type(VectorBlock) :: query_block
  type(VectorBlock) :: distances
  type(VectorBlock) :: labels
  real(real32), allocatable, target :: query(:)
  real(real32), pointer :: dist_ptr(:)
  integer(int32), pointer :: label_ptr(:)
  integer(int32) :: dim
  integer(int64) :: count
  integer(int64) :: offset_bytes

  call load_vector_layout("build/specs/vector_layout.json", "geometry.auth", dim, count, &
    offset_bytes, status)
  if (status /= GLAMIN_OK) then
    error stop "load_vector_layout failed"
  end if

  allocate(query(dim))
  query = 0.0_real32
  if (dim > 0) query(1) = 1.0_real32

  query_block%data = c_loc(query(1))
  query_block%length = 1
  query_block%dim = dim
  query_block%stride = dim
  query_block%elem_size = int(storage_size(0.0_real32) / 8, int32)
  query_block%alignment = 0

  call load_flat_from_layout("build/specs/vector_layout.json", "build/specs/vectors.bin", &
    "geometry.auth", METRIC_L2, index, status)
  if (status /= GLAMIN_OK) then
    error stop "load_flat_from_layout failed"
  end if

  distances = VectorBlock()
  labels = VectorBlock()
  call flat_search(index, query_block, 3_int32, distances, labels, status)
  if (status /= GLAMIN_OK) then
    call flat_destroy_handle(index, status)
    error stop "flat_search failed"
  end if

  call c_f_pointer(distances%data, dist_ptr, [3])
  call c_f_pointer(labels%data, label_ptr, [3])

  write(*, '(A, 3(F8.4,1X))') "Top distances: ", dist_ptr(1:3)
  write(*, '(A, 3(I8,1X))') "Top labels:    ", label_ptr(1:3)

  call flat_destroy_handle(index, status)
  deallocate(query)
end program geometry_loader_demo
