program native_hash_smoke
  use glamin_native_hash, only: hash256_hex
  implicit none

  if (trim(hash256_hex("abc")) /= &
      "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad") then
    error stop "native sha256 mismatch"
  end if

  write(*, '(A)') "native hash smoke ok"
end program native_hash_smoke
