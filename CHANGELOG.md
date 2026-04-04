# Changelog

All notable changes to this project will be documented in this file.

## 2026-04-03

### Fixed

- Added explicit Fortran module build dependencies to the `Makefile` so clean and parallel builds no longer fail with missing `.mod` compiler errors during compilation.

### Changed

- Replaced the Python-based geometry spec tooling with a native Fortran compiler path, including validation, canonicalization, manifest/contracts/layout emission, CPU embedding, and DOT rendering.
