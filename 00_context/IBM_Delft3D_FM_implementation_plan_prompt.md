You are a senior numerical-methods engineer and Delft3D Flexible Mesh (FM)
maintainer with strong experience in Fortran, finite-volume/finite-difference
shallow-water solvers, immersed-boundary methods, CMake, and scientific
verification.

## Objective

Create a concrete, repository-grounded implementation plan for a proof of
concept (POC) immersed boundary method (IBM) in Delft3D FM. The purpose is to
resolve velocity close to banks more accurately so that a future bank-erosion
model can use physically meaningful near-bank velocities.

Do not implement code in this task. Produce a plan that another engineer can
execute, including exact source files and symbols to change, data flow,
dependencies, tests, validation metrics, risks, and acceptance criteria.

## Problem context

- The reference is `00_context/Canestrelli16.md`, which summarizes Canestrelli
  et al. (2016), *A mass-conservative staggered immersed boundary model for
  solving the shallow water equations on complex geometries*.
- The reference method combines:
  - ghost cells for the momentum equations;
  - cut cells for the continuity equation;
  - a polygonal planar boundary embedded in a Cartesian grid;
  - interpolation from image points and enforcement of a free-slip boundary;
  - a staggered arrangement of water level and velocity variables;
  - an implicit/ADI-type solution procedure.
- The implementation described in the paper was made in a Delft3D 4 branch
  that is not available here. Treat the paper as a mathematical and numerical
  reference only; do not assume that its source code, data structures, or
  solver are present in this repository.
- IBM is expected to be meaningful primarily for Cartesian grids. For this
  POC, use the existing general unstructured FM solver and support IBM only
  when the supplied FM mesh can be proven to be Cartesian (within a clearly
  defined tolerance and topology/orientation criteria).
- The POC should use a static, planar, two-dimensional immersed bank boundary
  first. Do not implement bank erosion or moving-boundary coupling yet, but
  ensure the design does not prevent a future boundary update and morphology
  coupling.
- Existing simulations must retain their current behavior when IBM is not
  enabled. Non-Cartesian/unstructured meshes must not silently use Cartesian
  IBM assumptions.
- Prefer an opt-in feature with explicit input configuration, clear diagnostics,
  and a clear error or warning when the requested IBM is incompatible with the
  mesh or solver options.

## Repository investigation required before proposing the design

Inspect the repository rather than relying on generic Delft3D knowledge. At
minimum, trace and cite the relevant files and symbols in:

1. The FM kernel under
   `src/engines_gpl/dflowfm/packages/dflowfm_kernel/src`, including:
   - mesh and coordinate representations;
   - cell, edge/link, face, and boundary data;
   - locations of water levels, discharges, and velocity components;
   - continuity, momentum, advection, pressure/free-surface, viscosity,
     wetting/drying, and time-integration code;
   - the linear-system/solver path;
   - initialization, input parsing, restart, output, and diagnostics.
2. The FM library/kernel build wiring in
   `src/engines_gpl/dflowfm/packages/dflowfm_kernel/CMakeLists.txt` and any
   related CMake files that would need to include new Fortran sources or tests.
3. Existing boundary-condition and land/bank handling. Determine whether FM
   already has polygon, mask, inactive-cell, flow-link, cut-face, or geometry
   preprocessing facilities that can be reused.
4. Parallel execution and partition/halo exchange. Explain how embedded-boundary
   metadata and any ghost/image-point stencils would work across partitions.
5. Existing tests and test conventions, including:
   - `src/engines_gpl/dflowfm/tests`;
   - `test/deltares_testbench/TestBench.py`;
   - `test/deltares_testbench/data/cases`;
   - `test/deltares_testbench/configs/delft3dfm`;
   - `test/unit_test`;
   - `src/test`;
   - `doc/unit-testing.md`.
   The repository may use `test/` rather than a directory literally named
   `tests/`; identify the authoritative harness for each proposed test.
6. Existing user/configuration documentation and input-file conventions.

For every proposed change, name the current file, module/subroutine/type, and
the reason it is the correct integration point. If a symbol or path is not
found, say so explicitly and explain how the conclusion was reached. Do not
invent file names or APIs.

## Questions the plan must answer

### 1. Feasibility and architecture

- Is the requested POC feasible in the existing general unstructured FM
  formulation without first creating a separate Cartesian solver?
- How can a Cartesian mesh be represented and detected in FM? Define the
  geometric and topological checks, coordinate tolerance, orientation
  conventions, and behavior for skewed, rotated, mixed, or nearly Cartesian
  meshes.
- Where should the IBM opt-in be configured, and how should invalid combinations
  be reported? Consider `flow_run_single_timestep`, where already different solvers
  are called.
- Which parts of the existing unstructured solver can be reused unchanged,
  which require extension, and which paper assumptions do not map directly?
- Compare at least the following strategies and recommend one for the POC:
  1. adapting the existing unstructured flux/continuity formulation with
     embedded-boundary geometry;
  2. adding an IBM geometry/preprocessing layer while retaining the current
     solver;
  3. deferring to a future dedicated Cartesian solver.
  Explain the trade-offs in conservation, accuracy, stability, code size,
  performance, parallelism, backward compatibility, and testability.

### 2. Geometry and boundary representation

- Propose the input representation for the immersed bank (for example, an
  existing FM polygon/land representation or a new narrowly scoped input).
- Explain how to classify fluid, solid, cut, and ghost locations and how to
  compute boundary intersections, image points, normals, active areas, and
  active face/edge lengths.
- Define how degenerate cases are handled: a boundary through a node or face,
  very small active areas, corners, multiple intersections, narrow channels,
  insufficient interpolation stencils, and boundaries near partition edges.
- State whether the first POC supports only free-slip walls, and identify the
  design needed later for no-slip or rough-wall conditions.
- Explain whether the geometry is built once or can be rebuilt safely when the
  bank moves in a future erosion simulation.

### 3. Discrete equations and solver integration

- Map the paper’s hybrid cut-cell/ghost-cell idea onto FM’s actual discrete
  equations and variable locations. Do not assume that FM has the same
  staggered arrangement as Delft3D 4; verify it.
- Explain exactly how the immersed boundary changes:
  - continuity and volume/area fluxes;
  - momentum fluxes and velocity reconstruction;
  - free-surface gradients and pressure terms;
  - horizontal/vertical viscosity and turbulence terms;
  - advection and interpolation;
  - wetting/drying and partially wet cells;
  - boundary and initial conditions;
  - convergence, time-step, and linear-system behavior.
- State how impermeability and the chosen tangential condition are imposed.
- Explain how mass conservation is maintained and how the method behaves when
  very small cut areas are present. If FM’s time integration is explicit or
  otherwise incompatible with the paper’s stability argument, identify the
  consequence and propose the least risky POC treatment rather than assuming
  unconditional stability.
- Describe initialization order, per-step ordering, iteration/convergence
  criteria, and any required cached metadata.
- Include expected computational and memory costs, especially for MPI
  partitions and repeated geometry preprocessing.

### 4. Test and verification strategy

Design tests at more than one level:

- Unit tests for geometry classification, intersection/normal calculations,
  Cartesian-grid detection, stencil selection, interpolation, ghost values,
  cut-cell metrics, degeneracy handling, and conservation bookkeeping.
- A deterministic end-to-end FM regression test using the paper’s
  **uniform flow in a straight channel with banks oblique to the Cartesian
  grid** (Section 4.1 and Figures 4-8 of `Canestrelli16.md`) as the minimum
  paper-derived case. Use the existing FM test harness and input conventions,
  not an ad-hoc private script.
- If the repository and POC scope permit, plan a second paper-derived
  annular-channel case (Section 4.2) because it tests curved banks and
  near-bank velocity more strongly.
- Include a Cartesian grid aligned with the channel as a control, the same
  oblique channel without IBM as a regression comparison, and at least one
  non-Cartesian mesh case that verifies IBM is rejected or remains disabled.
- Prefer a 2-D test for the first POC. State separately what would be required
  for the paper’s 3-D/seven-layer verification and whether it belongs in the
  first implementation.

For each end-to-end case, specify:

- mesh, bank geometry, boundary conditions, bed slope, roughness/friction,
  discharge, depth, time step, run duration, and output variables;
- how the input data will be stored, generated, or versioned;
- whether the test is a unit test, CTest test, FM integration test, or
  TestBench case;
- exact comparison quantities and tolerances;
- how to measure mass conservation, normal velocity/leakage at the bank,
  near-bank tangential velocity, water-depth uniformity, steady-state
  convergence, and error against the analytical/reference solution;
- a mesh-refinement or resolution-sensitivity check where meaningful;
- expected runtime and whether the test is suitable for regular CI.

Do not claim second-order accuracy or machine-precision conservation merely
because the paper reports it. Define an achievable staged acceptance criterion
for this FM POC and explain how each threshold will be justified and calibrated.

### 5. Delivery, compatibility, and documentation

The plan must include:

- a sequenced implementation roadmap with dependencies and review/checkpoint
  boundaries;
- the smallest useful vertical slice for the first merge;
- required Fortran modules/types, configuration/input changes, CMake changes,
  diagnostics, output fields, and documentation;
- error handling and user-facing messages for unsupported meshes or invalid
  geometry;
- backward-compatibility and restart implications;
- serial and parallel validation;
- build, formatting, unit-test, integration-test, and regression-test commands
  based on repository documentation;
- performance instrumentation or diagnostics needed to investigate solver
  behavior near banks;
- a follow-up path for moving banks and future bank-erosion coupling, without
  expanding the current POC unnecessarily.

## Required plan format

Return the plan in this order:

1. **Executive summary and recommendation**
2. **Verified current architecture**
   - include a compact path/module/symbol/evidence table;
3. **Requirements, assumptions, and explicit non-goals**
4. **Alternatives considered and decision matrix**
5. **Detailed POC design**
   - geometry lifecycle and data structures;
   - equations/discretization;
   - solver call sequence;
   - configuration and diagnostics;
   - serial/MPI behavior;
6. **File- and symbol-level implementation work plan**
   - for each item: objective, exact files/symbols, dependencies, algorithm,
     compatibility impact, and completion criterion;
7. **Testing and verification matrix**
   - test level, case, metric, tolerance, harness, and CI suitability;
8. **Validation and acceptance criteria**
9. **Risks, unresolved technical questions, and fallback options**
   - distinguish decisions required before coding from decisions that can be
     deferred;
10. **Suggested implementation order and review checkpoints**
11. **Commands and evidence needed to validate the completed work**

Use equations or pseudocode where they remove ambiguity, but keep the plan
implementation-oriented. Clearly distinguish facts verified in the repository,
requirements taken from the paper, and recommendations or assumptions. Do not
silently fill gaps in the FM architecture, and do not propose unrelated
refactoring.
