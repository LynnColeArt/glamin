# Changelog

All notable changes to this project will be documented in this file.

## 2026-08-02

### Added

- Added ABI version 4 with persistent flat artifact loading, space-contract
  compatibility checks, canonical contract-hash validation, and safe C ABI
  failures for missing vector files.
- Added ABI version 3 with immutable generation creation, activation,
  deactivation, stable pins, retirement, label retrieval, and pinned flat-index
  search.
- Added ABI version 2 with runtime-owned flat-index creation, synchronous
  float32 add/search, caller-owned results, and zero-based row labels.

### Fixed

- Corrected flat-index append behavior for input vectors whose stride is larger
  than their dimension.

## 2026-04-03

### Fixed

- Added explicit Fortran module build dependencies to the `Makefile` so clean and parallel builds no longer fail with missing `.mod` compiler errors during compilation.

### Changed

- Replaced the Python-based geometry spec tooling with a native Fortran compiler path, including validation, canonicalization, manifest/contracts/layout emission, CPU embedding, and DOT rendering.
