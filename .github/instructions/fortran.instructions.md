---
applyTo: "src/engines_gpl/**/*.{f,f90,F,F90},src/utils_gpl/**/*.{f,f90,F,F90},src/utils_lgpl/**/*.{f,f90,F,F90}"
---

# Fortran review guidelines (Delft3D kernels)

These guidelines apply to Fortran sources under `src/engines_gpl/`,
`src/utils_gpl/`, and `src/utils_lgpl/`. Focus code review comments on issues
that are likely to cause bugs, numerical problems, or long-term maintenance
pain.

## Correctness

- Flag `real`/`double precision` mismatches. Numeric kinds should use the
  project's `dp` or `sp` kind parameters from the `precision` module.
- Literal constants used in floating-point expressions must carry the kind
  suffix (e.g. `1.0_dp`, not `1.0` or `1.0d0`) to avoid silent precision loss.
- Check that `intent(in)`, `intent(out)`, `intent(inout)` is declared on every
  dummy argument, and that `intent(out)` arguments are actually assigned on
  every code path.
- Watch for uninitialized locals, especially arrays and derived-type
  components. Do not rely on compiler zero-initialization.

## Numerical

- Flag division without a guard against zero or near-zero denominators.
- Flag equality comparisons between floating-point values; use `compare_real`
  from the `precision_basics` module.
- Flag mixed-mode arithmetic (integer / real) that can truncate unexpectedly.
- Flag loss-of-significance patterns such as subtracting nearly equal
  quantities without reformulation.

## Modules

- Modules should use `implicit none(type, external)`. When this is present in the module, 
  discourage use of `implicit none` in functions and subroutines.
- Modules should use `private` by default, exposing only what is needed with `public ::`.

## Documentation

- Flag missing docstrings for *new* public functions, submodules, module variables, types,
  and interfaces. Use Fortran [Doxygen](https://www.doxygen.nl/manual/docblocks.html#fortranblocks)
  style docstrings. E.g. `!>` before procedures, and `!<` after dummy arguments.

## Backward compatibility

- Warn against public API breaking signature changes in `src/utils_gpl` or `src/utils_lgpl`. 
  This code is shared. If the source branch name does not start with the `all/` prefix, warn 
  the author to rename the branch to use an `all/` branch instead.
- Input file formats (`.mdu`, `.ext`, `.bc`, `.pol`, `.xyn`, DIMR XML, etc.)
  must remain readable. Warn against any keyword rename or default change that would
  break existing user input.

## What NOT to comment on
- Do not request docstring or comment additions on code that was not changed.
