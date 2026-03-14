program glamin_distance_smoke
  use iso_fortran_env, only: int32, int64, real32
  use iso_c_binding, only: c_f_pointer, c_loc
  use glamin_distance, only: distance_ip_batch, distance_l2_batch
  use glamin_memory, only: free_aligned
  use glamin_types, only: VectorBlock
  implicit none

  real(real32), target :: vector_data(6)
  real(real32), target :: query_data(4)
  type(VectorBlock) :: vectors
  type(VectorBlock) :: queries
  type(VectorBlock) :: distances
  real(real32), pointer :: distance_ptr(:)
  integer(int32) :: elem_bytes
  integer(int32) :: status
  integer(int32) :: free_status

  elem_bytes = int(storage_size(0.0_real32) / 8, int32)

  vector_data = [0.0_real32, 0.0_real32, 1.0_real32, 0.0_real32, 0.0_real32, 1.0_real32]
  query_data = [0.0_real32, 0.0_real32, 1.0_real32, 1.0_real32]

  vectors = VectorBlock()
  vectors%data = c_loc(vector_data(1))
  vectors%length = 3_int64
  vectors%dim = 2
  vectors%stride = 2
  vectors%elem_size = elem_bytes

  queries = VectorBlock()
  queries%data = c_loc(query_data(1))
  queries%length = 2_int64
  queries%dim = 2
  queries%stride = 2
  queries%elem_size = elem_bytes

  distances = VectorBlock()
  call distance_l2_batch(queries, vectors, distances, status)
  if (status /= 0_int32) error stop "distance_l2_batch failed"

  call c_f_pointer(distances%data, distance_ptr, [6])
  if (any(abs(distance_ptr - [0.0_real32, 1.0_real32, 1.0_real32, &
      2.0_real32, 1.0_real32, 1.0_real32]) > 1.0e-6_real32)) then
    error stop "L2 distance mismatch"
  end if

  call free_aligned(distances%data, free_status)

  distances = VectorBlock()
  call distance_ip_batch(queries, vectors, distances, status)
  if (status /= 0_int32) error stop "distance_ip_batch failed"

  call c_f_pointer(distances%data, distance_ptr, [6])
  if (any(abs(distance_ptr - [0.0_real32, 0.0_real32, 0.0_real32, &
      0.0_real32, 1.0_real32, 1.0_real32]) > 1.0e-6_real32)) then
    error stop "IP distance mismatch"
  end if

  call free_aligned(distances%data, free_status)

  write (*, '(a)') 'distance smoke ok'
end program glamin_distance_smoke
