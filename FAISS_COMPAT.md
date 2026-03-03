# FAISS Compatibility Targets

## Supported Index Types (Planned)
| Index | Training | Add | Search | Save/Load |
| --- | --- | --- | --- | --- |
| IndexFlatL2 | N/A | Yes | Yes | Yes |
| IndexFlatIP | N/A | Yes | Yes | Yes |
| IndexIVF | Yes | Yes | Yes | Yes |
| IndexPQ | Yes | Yes | Yes | Yes |
| IndexIVFPQ | Yes | Yes | Yes | Yes |
| IndexHNSW | Yes | Yes | Yes | Yes |

## Compatibility Notes
- Binary serialization targets FAISS file layouts for the indices above.
- Endianness conversion is handled at IO boundaries.
- Search output should match FAISS top-k results within a defined tolerance.

## Current Implementation
- IndexFlatL2 and IndexFlatIP load/save are implemented in `src/io/mod_faiss_io.f90`.

## Non-Goals (Initial)
- GPU index file compatibility beyond core metadata.
- Exotic composite indices not listed above.
