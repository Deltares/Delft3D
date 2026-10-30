# Implementation plan — Immersed Boundary Method (IBM) POC in Delft3D FM

Plan produced from [IBM_Delft3D_FM_implementation_plan_prompt.md](./IBM_Delft3D_FM_implementation_plan_prompt.md).
Reference method: [Canestrelli16.md](./Canestrelli16.md) (Canestrelli et al., 2016).

Evidence labels used throughout:

- **[V]** verified in this repository (path:line cited, paths relative to repo root);
- **[P]** requirement/statement taken from the paper;
- **[R]** recommendation or design decision of this plan;
- **[A]** assumption not yet verified — each one is assigned to the spike in work item W0.

Abbreviation: `K = src/engines_gpl/dflowfm/packages/dflowfm_kernel/src`.

---

## 1. Executive summary and recommendation

**Feasible without a new solver.** FM's 2D discretisation on a Cartesian mesh is already a
staggered C‑grid [V]: water levels `s1` at cell centres, one face‑normal velocity `u1` per flow
link, and continuity as a finite‑volume balance `a1·(s1−s0)/Δt + Σ ±au·u = qin`. That is the same
variable arrangement as Delft3D 4, but without its ADI splitting.

The most important finding is that **FM already implements the cut‑cell half of the paper** [V].
Setting `[geometry] Cutcelllist` makes `cutcelwu(5)` do the following:

- shorten each cut edge's `wu` to its active (fluid) length;
- replace `ba` by the area of the active polygon;
- close fully solid links;
- optionally floor small cells (`Baorgfracmin`).

Continuity then stays exactly conservative with respect to the modified `ba`/`wu`, because
`s1ini`/`s1nod`/`volsur` only use those arrays. What FM does **not** have is the momentum side:

- the immersed segment is not a wall: `walls` is built only from closed net links;
- cut‑cell centre velocities (`ucx`, `ucy`) are reconstructed with ad‑hoc weights and no boundary
  constraint;
- there are no image‑point or ghost values for stencils that reach into the solid;
- there is no Cartesian‑mesh guard, no IBM diagnostics and no test coverage of FM cut cells
  (no FM `Cutcelllist` TestBench case was found).

**Recommendation [R]: Strategy 2.** Add an opt‑in IBM geometry/pre‑processing layer plus narrowly
scoped momentum hooks, and keep the existing FM solver (`step_reduce_hydro`) unchanged:

1. **Opt‑in and guard.** Add `[numerics] ImmersedBoundary = 1`. It requires `Cutcelllist` (reused
   as the bank‑polygon input in the POC), a proven Cartesian 2D mesh, `kmx = 0`, `jsferic = 0` and
   `FlowSolver = generic1d2d3d`. Anything else is a fatal `mess(LEVEL_ERROR, …)` naming the
   failed criterion.
2. **Geometry module.** A new module `m_immersed_boundary` builds its own geometry independently
   from the polygon and checks it against `cutcelwu`'s `ba`/`wu`:
   - cell classification (fluid / cut / solid / ghost);
   - boundary segments, normals and area/edge fractions;
   - boundary intercepts (BI), image points (IP) and bilinear stencils.

   This makes it the future source of truth for a moving bank.
3. **Momentum fix (first slice).** Enforce free slip in cut cells by correcting the reconstructed
   cell‑centre velocity: remove the normal component relative to the immersed segment, after every
   `setucxucyucxuucyunew` call. Ghost‑cell reflection (paper Eqs. 19–20) fills the stencil values
   that higher‑order advection reads from solid cells (second slice).
4. **Verification.** Use the paper's §4.1 oblique uniform‑channel case in four variants: aligned
   control, staircase, FM cut cells only, and IBM. Run it as TestBench cases for regression, plus a
   versioned analytical‑metrics script for acceptance. Fortran gtest (`f90tw`) unit tests cover
   the geometry.

Do **not** add a new `FLOW_SOLVER_*` branch in `flow_run_single_timestep` for the POC. IBM changes
geometry and explicit momentum terms, not the time‑integration driver. That select‑case remains the
right place for a later dedicated Cartesian solver (Strategy 3).

The FM scheme is θ‑implicit only in the water‑level gradient and explicit in advection and
viscosity, so the paper's unconditional‑stability argument for ADI **does not transfer**. The
time step is limited by `cflmx·vol1/Σq_out` per cell (`setdtorg`), so small cut cells reduce Δt.
The POC limits this with the existing `Baorgfracmin` floor and makes the resulting area error
visible in diagnostics. Accuracy targets are staged: first order near banks first; second order is
not claimed.

---

## 2. Verified current architecture

### 2.1 Variable locations and discrete equations [V]

On a Cartesian FM mesh:

- Cell centre `xz/yz` = circumcentre = centroid of each square.
- Link `L` connects `ln(1,L) → ln(2,L)`.
- `csu/snu` give the link normal in that positive direction (from `normalin`), so x‑faces have
  `|csu| = 1` and y‑faces `|snu| = 1`. These are the D3D4 u‑ and v‑points.
- There is no tangential velocity on links. Tangential information exists only through the
  cell‑centre reconstruction (`ucx`, `ucy`).

Momentum, per 2D link (`furu.f90:147‑204`):

```
cu = g·dxi(L)·teta(L)
du = u0/Δt − adve(L) + g·dxi(L)·slopec − (1−teta)·g·dxi·(s0(k2)−s0(k1))
fu = cu / (1/Δt + advi + frL),   ru = du / (1/Δt + advi + frL)
u1(L) = ru(L) − fu(L)·(s1(k2) − s1(k1))                        (u1q1.f90:80‑102)
```

Continuity, 2D (`s1ini.f90:333‑346`, `s1nod.f90:107‑171`, `volsur.f90:24‑25`):

```
bb(k) += teta·au·fu ;  ccr(Lv2(L)) −= teta·au·fu ;  dd(k) ∓= teta·au·ru + (1−teta)·au·u0
bbr(n) = bb(n) + a1(n)/Δt ;  ddr(n) = dd(n) + a1(n)/Δt·s0(n) ;  a1(n) = ba(n), vol1 = ba·h
```

Only links with `hu(L) > 0` enter the system, so a face that does not exist (or has `wu = 0`)
carries exactly zero flux.

### 2.2 Path / module / symbol / evidence table

| Area | Path:line | Symbol | Verified fact |
|---|---|---|---|
| Net (mesh) | `src/utils_lgpl/gridgeom/packages/gridgeom/src/network_data.f90:56` | `network_data` | `xk, yk, kn, netcell(n)%nod/%lin, lnn, lne, nump, numl`. |
| Flow geometry | `K/dflowfm_data/m_flowgeom.f90:104‑165,226` | `nd, kfs, bai, ln, lncn, kcu, dx, wu, acl, csu, xu, wcx1, walls` | Flow cells/links and metrics. `ln(2,lnx)`; `lncn` = net corner nodes of a link. |
| Cell centres/areas | `src/utils_lgpl/gridgeom/packages/gridgeom/src/generalmodules.f90:293‑297` | `xz, yz, ba` | Shared with gridgeom. |
| Flow state | `K/dflowfm_data/m_flow.f90:152‑157,183,265‑289,349‑350` | `s1, a1, vol1, ucx, u1, q1, au, adve, hu, suu, fu, ru` | Locations as in §2.1. |
| Geometry init | `K/dflowfm_kernel/prepost/flow_geominit.f90:274,277,292,297,403,975‑995,1119,1126,1165,1225‑1309,1311,1314` | `flow_geominit` | Order: dry points → `cutcell_list(6,1)` → thin dams → `cosphiunetcheck(1)` → `getcellsurface` → `wu` (+ `update_ghosts(ITYPE_U)`) → `cutcell_list(4,2)` (flag cut cells in `kfs`) → corner weights → `cutcell_list(5,3)` (`wu`/`ba` cut) → walls → `setwallorientations` → `setlinktocenterweights`. |
| Cut cells | `K/dflowfm_kernel/prepost/cutcell_list.f90` | `cutcell_list(n12,jamasks)` | Reads `md_cutcelllist` (a list of `.pol` files) with `oldfil`/`reapol`; k‑d tree on net nodes; stores per‑link intersection masks when `jamasks = 1`. |
| Cut cells | `K/dflowfm_kernel/prepost/cutcelwu.f90:300‑420` | `cutcelwu(n12,…)` | `KC(k) = 1` ⇔ net node inside polygon (**solid**). In mode 5, a cut edge gets `wu` = distance from intersection to the outside node; all‑inside edges get `wu = 0`; `ba = max(polygon area, Baorgfracmin·ba)`, then `max(ba, BAMIN)`; `nd(n)%x/y` is replaced by the active polygon. Mode 6 closes links (`lnn < 0`). `xz/yz` are **not** moved. |
| Cut‑cell weights | `K/dflowfm_kernel/prepost/setlinktocenterweights.f90:141‑160` | `wcx1/wcy1/…` | Cut cells (`kfs = 1`) get weights normalised by `wcxy`, not by `bai`. |
| Small cells | `K/dflowfm_data/m_flow.f90:336‑337`; `m_flowgeom.f90:310`; `unstruc_model.f90:1119‑1120` | `numlimdt_baorg`, `baorgfracmin` (default 0), `bamin = 1e‑6` | `[numerics] Numlimdt_baorg`, `Baorgfracmin`. |
| Walls | `flow_geominit.f90:1225‑1309`; `K/dflowfm_kernel/compute/setumod.f90:540‑667` | `walls(17,nw)`, `irov` | Walls exist **only** for net links with `lne2ln(L) < 0`. `irov`: 0 free slip (no stress), 1 partial slip (`wall_ks`), 2 no slip; stress added into `suu`, then `adve −= suu`. |
| Slip input | `K/dflowfm_data/unstruc_model.f90:1239‑1241,3454`; `m_flowparameters.f90:220,825` | `irov` (default 0), `wall_ks` | `[physics]` keywords. |
| Timestep | `K/dflowfm_kernel/main/flow_run_single_timestep.f90:68‑90` | `flow_run_single_timestep` | `select case (flow_solver)`: `FLOW_SOLVER_FM → step_reduce_hydro`; `FLOW_SOLVER_SRE → SOFLOW_wrap`. |
| Solver choice | `K/dflowfm_data/m_flowparameters.f90:412‑415`; `unstruc_model.f90:1140‑1142` | `flow_solver`, `[numerics] FlowSolver` | `generic1d2d3d` = FM. |
| Step | `K/dflowfm_kernel/compute/step_reduce_hydro.f90:112‑322` | `step_reduce_hydro` | `furu → volsur → s1ini → pack_matrix → s1nod → solve_matrix → update_ghosts(ITYPE_SALL,s1) → poshcheck → setkfs → volsur` (nonlinear loop). |
| Step init | `K/dflowfm_kernel/compute/flow_initimestep.f90:109‑240` | `flow_initimestep` | `setdt → … → setau → setumod → … → setcfuhi → … → advecdriver`. |
| Reconstruction | `setumod.f90:115,126`; `advecdriver.f90:82`; `prepost/flow_flowinit.f90:951` | `setucxucyucxuucyunew` | Perot reconstruction of `ucx/ucy` (and link advection velocities). Least‑squares alternative `reconst2nd` in `compute/setucxcuy_leastsquare.f90`. |
| Advection | `compute/advec.f90:52`; `compute/sethigherorderadvectionvelocities.f90:44`; `m_flowparameters.f90:678` | `advec`, `iadvec = 33` (default) | Explicit; θ‑blended in `advecdriver`. |
| θ | `m_flowparameters.f90:305,874`; `flow_geominit.f90:945‑953` | `teta0` = 0.55 | Semi‑implicit water‑level gradient only. |
| Linear solver | `K/dflowfm_utils/solve_guus.F90`; `solve_petsc.F90`; `unstruc_model.f90:967‑984` | `solve_matrix`, `Icgsolver` | Global unstructured system for `s1` (5‑point stencil on Cartesian meshes). |
| CFL | `compute/setdtorg.f90:129‑135` | `dtsc = cflmx·vol1/squ` | Excludes MPI ghost cells and dry cells. |
| Orthogonality | `prepost/cosphiunetcheck.f90:39`; `unstruc_model.f90:699` | `cosphiunetcheck`, `Cosphiutrsh` | Only orthogonality is checked; **no Cartesian/structured flag exists** (searched `isCartesian`, `cartesian`, curvilinear markers). |
| Polygons | `prepost/pol_to_cellmask.F90:31`; `prepost/m_cellmask_from_polygon_set.f90:31`; `src/utils_lgpl/deltares_common/packages/deltares_common/src/geometry_module.f90` | `pol_to_cellmask`, `dbpinpol`, `pinpok_*`, `dbdistance`, `normalin`, `dlinedis` | Reusable geometry utilities. |
| MDU read/write | `unstruc_model.f90:640‑663` (read), `2699‑2797` (write); `readMDUFile` at 467 | `prop_get/prop_set`, `md_cutcelllist` | Pattern for file and numeric keywords. Errors use `mess(LEVEL_ERROR,…)` (e.g. 1008‑1014); warnings `LEVEL_WARN` (1632‑1650). |
| Model init | `prepost/flow_modelinit.F90:216‑245,409‑410` | `flow_modelinit` | Serial: `flow_geominit(0)`. MPI: `flow_geominit(1)` → `partition_init_1D2D` → `update_geom(1)` → `flow_geominit(2)` → `update_geom(2)`. Then `flow_flowinit`. |
| MPI | `K/dflowfm_data/partition.F90:124‑140,447‑448,2323‑2363,3313` | `ITYPE_S/U/Sall`, `numlay_cellbased = 4`, `update_ghosts`, `reduce_sum` | Four cell‑based ghost layers. Ghost cells excluded from CFL and mass balance (`is_ghost_node`, `s1ini.f90:325`). |
| Output | `K/dflowfm_io/unstruc_netcdf.f90:3993‑4140,13132‑13162`; `unstruc_netcdf_data.f90:131‑475`; `unstruc_model.f90:2130‑2132` | `unc_def_var_map`, `t_unc_mapids`, `map_write_settings%…` | Optional map variables are switched with `[output] Wrimap_*`. |
| Restart | `K/dflowfm_io/wrirst.f90:69`; `read_restart_from_map.f90:44‑49` | `unc_write_rst_filepointer`, `unc_read_map_or_rst` | Restart holds state only; derived geometry is rebuilt at init. |
| Balance | `K/dflowfm_data/m_flow.f90:461‑663`; `fm_statistical_output.f90:779‑851` | `voltot`, `Wrihis_balance` | Water‑balance totals (including volume error) on the his file. |
| Build | `src/engines_gpl/dflowfm/packages/dflowfm_kernel/CMakeLists.txt:26‑64,284` | `get_fortran_source_files(...)` | Directory globbing per folder ⇒ new `.f90` in `prepost/`, `compute/` or `dflowfm_data/` is picked up without CMake edits. |
| Unit tests (kernel) | `src/engines_gpl/dflowfm/packages/dflowfm_kernel/test/CMakeLists.txt:14‑63`; `src/third_party_open/f90tw/f90tw-main/f90tw/CMakeLists.txt:95,166` | `f90twtest(test_dflowfm_kernel_gtest …)` | Fortran tests via f90tw/GoogleTest (`!$f90tw TESTCODE(TEST, suite, name, …)`, `use assertions_gtest`). Each file must be added to **both** `F90FILES` and `F2HFILES`; registered with `gtest_discover_tests` (CTest name `suite.name`). Geometry example: `test/src/test_pol_to_cellmask.f90`. |
| Unit tests (ftnunit) | `src/test/engines_gpl/dflowfm/packages/dflowfm_kernel/CMakeLists.txt`; `doc/unit-testing.md:1‑8,236‑350` | `dflowfm_kernel_test` | Older ftnunit target, run from the install folder. **Not** recommended for new tests. |
| C# tests | `src/engines_gpl/dflowfm/tests/DFlowFMTests/DFlowFmTests.cs` | NUnit | API tests only; not a harness for this work. |
| `test/unit_test` | `test/unit_test` | ftnunit | WAQ/utility tests only; no FM. |
| TestBench | `test/deltares_testbench/configs/dimr/dimr_dflowfm_lnx64.xml:202`; `configs/include/dimr_dflowfm_parallel.xml:488‑515`; `HOW_TO_DVC.md:61‑126` | `<testCase … ref="dimr">`, `<path version="DVC">` | Regression against reference NetCDF (`toleranceAbsolute`/`toleranceRelative`). Case data lives in DVC/MinIO; `data/cases` holds only `.dvc` files in git. |
| FM cut‑cell tests | `configs/include/delft3d4_sp_cases*.xml` | — | Only **Delft3D 4** cut‑cell cases exist (`e01_d3dflow/f15_cutcells`, `f52 … Cutcells_2DH`). **No FM `Cutcelllist` TestBench case found.** Closest FM template: `e02_dflowfm/f004_bottomfriction/c080_chezy_uniform`. |
| User documentation | repository‑wide search for `Cutcelllist`/`ThinDamFile` in `.md/.tex/.xml/.json` | — | Not found. MDU keyword documentation in‑repo is the description string passed to `prop_set` in `writeMDUFilepointer`. The user manual is an external PDF. |

Searched and not found: `isCartesian` or any structured‑origin flag; an FM periodic boundary
(not searched exhaustively — see W0); any existing IBM, ghost‑cell or image‑point code.

---

## 3. Requirements, assumptions, explicit non‑goals

**Functional requirements**

1. Opt‑in only: `ImmersedBoundary = 0` (the default) must give bit‑identical results to current
   code for every existing model, including models that use `Cutcelllist`.
2. A static, planar 2D polygonal bank (solid = inside polygon, same convention as `Cutcelllist`).
3. IBM is allowed only on a proven Cartesian mesh (§5.4). Other meshes get a fatal error that
   states the failed criterion; there is never a silent fallback.
4. Impermeability at the immersed boundary: no flux through the solid part of cut cells.
   Free‑slip tangential condition.
5. Mass conservation with respect to the discrete cell areas actually used (`ba`), with the
   floor‑induced area deviation reported.
6. Serial and MPI runs give consistent IBM metadata.
7. Diagnostics: class counts, minimum area fraction, floored cells, Cartesian‑check result, and
   per‑cell map fields.
8. Unit, regression and analytical‑verification tests (§7).

**Assumptions to verify in spike W0** [A]

- A1: Fully solid cells (all links closed by `cutcell_list(6,1)`) disappear as flow nodes, so no
  `s1`/`ucx` exists there. This decides whether ghost values need separate storage
  (the design assumes yes, §5.1).
- A2: In 2D, `iadvec = 33` reads only the cell‑centre velocities of `k1`/`k2` and of cells across
  *open* faces. `sethigherorderadvectionvelocities` may read second neighbours; establish which
  stencils reach solid cells.
- A3: `cutcell_list` produces identical `wu`/`ba` for MPI ghost cells and owned cells (it runs per
  partition on local coordinates).
- A4: The `flow_geominit(iphase)` semantics: the IBM build must run once, in the final phase.
- A5: Whether TestBench can assert an *expected failure* (non‑zero exit) of a model run.
- A6: Whether FM supports a periodic or "copy‑inflow" boundary (needed for the paper's 3D/annular
  periodic set‑ups).
- A7: The exact names of the his‑file water‑balance variables produced by `Wrihis_balance`.
- A8: The pattern for adding an optional map variable, including any generated source
  (`unc_put_var_map_output`, `CMakeLists.txt` comment at line 279).

**Explicit non‑goals (POC)**

- Bank erosion, a moving or time‑dependent boundary, morphology coupling.
- 3D (`kmx > 0`), spherical coordinates, 1D/1D2D networks, the SRE solver, `Nonlin2D`
  volume tables.
- Rotated, non‑uniform rectilinear or curvilinear grids (rejected; listed as extensions).
- No‑slip or rough immersed walls (design hook only).
- Second‑order accuracy (paper §3.2.2.2, Appendices A/B) — staged later.
- Cell merging for small cut cells.
- A dedicated Cartesian/ADI solver.
- Refactoring of `cutcelwu`/`cutcell_list` beyond what IBM needs.

---

## 4. Alternatives considered and decision matrix

| Criterion | S1: change unstructured flux/continuity formulation with embedded geometry | **S2: IBM geometry layer + narrow momentum hooks, current solver** | S3: dedicated Cartesian (ADI‑like) solver |
|---|---|---|---|
| Conservation | Exact if done right, but continuity code needs new logic | Exact w.r.t. `ba`/`wu` — reuses verified `cutcelwu` path and unchanged `s1ini`/`s1nod` | Exact (paper) |
| Near‑bank accuracy | Same potential as S2 | 1st order near banks (water level at full‑cell centre, like paper HCG1); 2nd order is a later slice | 2nd order (paper HCG2) |
| Stability | Explicit CFL with small `vol1` | Same; mitigated by `Baorgfracmin` floor (cost: area error) | ADI, Courant up to ~10 (paper) |
| Code size | Large (`s1ini`, `furu`, `volsur`, `setau` all touched) | Small/medium: 1 new module (~1.5–2.5 kLOC incl. tests), ~6 call sites | Very large (new solver, I/O, MPI, BCs) |
| Performance | Neutral | O(N_cut) per step, negligible | Potentially faster, but not reusable |
| MPI | Hard (matrix changes) | Geometry local + existing 4 ghost layers | New partitioning logic |
| Backward compatibility | High risk | Opt‑in; default path untouched | Isolated |
| Testability | Hard to isolate | Pure geometry routines unit‑testable; 4‑variant regression isolates the IBM effect | Hard, long lead time |
| Fit to future moving bank | Medium | Good: geometry rebuild is one routine | Good |

**Decision [R]:** S2 now. S3 stays the long‑term option, reached through the existing
`select case (flow_solver)` in `flow_run_single_timestep.f90:70`, if S2 accuracy or Δt limits prove
insufficient. S1 is rejected: the needed cut‑cell continuity already exists.

---

## 5. Detailed POC design

### 5.1 Geometry lifecycle and data structures

New file `K/dflowfm_kernel/prepost/m_immersed_boundary.f90` (picked up by globbing) [R]. It uses
`implicit none(type, external)`, is `private` by default, uses `dp`, has Doxygen comments, and
follows `.github/instructions/fortran.instructions.md`. Geometry kernels are **pure procedures on
explicit arrays** so they can be unit‑tested without initialising a model.

```fortran
type t_ibm_cartesian        ! result of the Cartesian check
   logical       :: is_cartesian
   real(dp)      :: x0, y0, dx, dy          ! lattice origin and spacing
   real(dp)      :: max_dev                 ! max relative deviation found
   character(256):: reason                  ! failed criterion, empty if OK
end type

type t_ibm_cut_cell         ! one per cut flow cell k
   integer  :: k                            ! flow node
   real(dp) :: area_frac                    ! geometric active area / full area (before floor)
   real(dp) :: ba_used                      ! ba after Baorgfracmin/BAMIN floor (= ba(k))
   real(dp) :: xbi(2), ybi(2)               ! segment end points (BI segment)
   real(dp) :: xc, yc                       ! centroid of active polygon
   real(dp) :: nx, ny                       ! unit normal of segment, pointing INTO fluid
   real(dp) :: seg_len
   integer  :: nseg                         ! >1 ⇒ multiple crossings ⇒ flagged degenerate
end type

type t_ibm_ghost            ! one per solid cell centre that some fluid stencil needs
   real(dp) :: xg, yg, xbi, ybi, xip, yip, nx, ny
   integer  :: stencil(4)                   ! flow nodes of bilinear IP stencil (0 = use BI constraint)
   real(dp) :: w(4)                         ! bilinear weights
   real(dp) :: ucx, ucy                     ! ghost values (own storage, see A1)
end type

type t_ibm_state
   logical :: active = .false.
   type(t_ibm_cartesian) :: grid
   integer, allocatable  :: cell_class(:)   ! (ndx) 0 fluid, 1 cut, 2 solid/absent, 3 ghost-referenced
   real(dp), allocatable :: edge_frac(:)    ! (lnx) wu_cut / wu_full
   real(dp), allocatable :: wu_full(:), ba_full(:)   ! kept for future rebuild (moving bank)
   type(t_ibm_cut_cell), allocatable :: cut(:)
   type(t_ibm_ghost),    allocatable :: ghost(:)
   ! diagnostics
   integer  :: ncut, nghost, nfloored, ndegenerate
   real(dp) :: min_area_frac, area_deficit   ! Σ(ba_used − frac·ba_full)
end type
```

**Lifecycle [R]**

1. **Read** (`readMDUFile`): `ImmersedBoundary`, `ImmersedBoundaryCartesianTol` and
   `ImmersedBoundaryMinAreaFrac`.
2. **Cartesian check** (`ibm_check_cartesian`), on the net, in `flow_geominit` immediately after
   `cosphiunetcheck(1)` (line 297). The net is final at that point. The lattice test still works
   after cells were deleted at lines 274/277.
3. **Save full metrics**: `wu_full = wu` (after line 995) and `ba_full = ba` (after line 403),
   before `cutcell_list(5,3)` at line 1165. These are needed for fractions, diagnostics and a
   future rebuild.
4. **Build** (`ibm_build_geometry`) after `setlinktocenterweights` (line 1314), in the final
   geominit phase only (A4):
   - recompute the polygon/net intersections independently;
   - classify cells;
   - compute fractions, segments, normals, image points and stencils;
   - cross‑check `ba(k)` and `wu(L)` from `cutcelwu`. Allow a relative difference of `1e‑10`, or
     the floor, otherwise raise an error. This catches divergence between the two geometry codes.
5. **Every time step**: velocity correction (§5.3). Static geometry, no rebuild.
6. **Output**: map fields once (class, fractions, normals) and every map step (normal‑velocity
   residual at segments).
7. **Future**: `ibm_rebuild(polygon)` = restore `wu_full`/`ba_full` → re‑cut → rebuild → remap
   state (§9). The POC keeps this as one entry point but does not call it after init.

**Classification [R]** (on the net, using the nodes `netcell(n)%nod` of each 2D cell):

- **solid**: all 4 corner nodes inside the polygon (`dbpinpol`) and no edge crossing;
- **fluid**: all nodes outside and no edge crossing (catches thin intrusions);
- **cut**: otherwise;
- **ghost**: a solid cell centre that appears in the bilinear stencil or advection stencil of a
  fluid/cut cell (A2).

**Intersections, normals, areas**

- The edge/segment crossing uses the same primitives as `crosslinkpoly`/`crosspoly`.
- The active polygon is obtained by clipping the square with the half‑planes of the polygon edges
  inside the cell (Sutherland–Hodgman on a convex square; the solid region may be non‑convex, but
  per cell it is handled edge by edge).
- `area_frac = A_active / ba_full`; `edge_frac = wu_cut / wu_full`.
- The normal comes from the segment joining the two edge intersections, oriented toward the active
  centroid.

**Image points [P→R]**

- BI = projection of the ghost centre onto the nearest polygon segment.
- IP = 2·BI − GP (so |GP−BI| = |IP−BI|).
- The stencil is the 4 cell centres of the lattice cell that contains the IP. On a Cartesian
  lattice this is an index computation: `i = floor((xip−x0)/dx − ½)`.
- If all 4 stencil cells are fluid or cut: bilinear weights.
- If some are solid: use the paper's BI‑constrained system (8×8 or 6×6, §3.1 "one/two points on
  ghost side") for the 2D velocity vector. Solve with a small dense LU (LAPACK `dgesv` if already
  linked, otherwise a local Gaussian elimination with pivoting) and check the condition number.

**Degenerate cases [R]**

| Case | Handling |
|---|---|
| Polygon vertex / segment through a net node | Snap: if the distance from the node to the polygon is < `tol_snap = 1e‑8·dx`, treat the node as **solid**. This makes the edge fraction exactly 0 or 1. Same rule in IBM and in the cross‑check. |
| Segment collinear with a cell face | Face fully open or fully closed after snapping; the cell is not cut (aligned control case tests this). |
| Very small active area (`area_frac < ImmersedBoundaryMinAreaFrac`, default 0.1) | Floored by the existing `Baorgfracmin` mechanism (IBM sets `Baorgfracmin = ImmersedBoundaryMinAreaFrac` if the user did not). Counted in `nfloored`; `area_deficit` reported. |
| Corner (polygon vertex inside a cell) / multiple crossings in one cell | Allowed if the active region is still one polygon. The normal is the area‑weighted mean of the sub‑segments. `nseg > 1` is flagged. If the active region splits into ≥2 parts (narrow solid spit): error in the POC. |
| Narrow channel (< 2 fluid cells between banks) | IP stencil falls on the opposite bank. Use BI‑constrained interpolation. If still singular, fall back to the nearest fluid cell value and count it in `ndegenerate`. Warn when >0. |
| Insufficient stencil (IP outside local domain) | MPI ghost layers (4) cover IP distances ≤ ~1.5 dx. If the stencil is not local: error naming the cell (do not silently degrade). |
| Boundary near partition edge | Geometry is computed for owned and ghost cells. Ghost‑value fill uses only local data. `ucx/ucy` are already exchanged with `update_ghosts(ITYPE_SALL)`. |
| Polygon crossing an open boundary | Error in the POC (inflow/outflow boundary links must be fully fluid). |

**Wall condition**: free slip only in the POC. The design keeps `nx, ny, seg_len` and the
wall‑distance per cut cell, which is exactly what the existing `walls(:,nw)` treatment needs
(`setumod.f90:540‑667`). Partial‑slip and no‑slip immersed walls are later implemented by
appending IBM segments as pseudo‑walls (fill `walls(1..17,·)` with `walls(4/5)` = the two
active links nearest to the segment ends). That reuses the existing `irov = 1|2` code instead of
writing a new one.

### 5.2 Equations / discretisation mapping

| Term | Paper (D3D4) | FM POC [R] | Change? |
|---|---|---|---|
| Continuity | Cut‑cell finite volume with active edge length and active area | Identical in structure: `a1 = ba_cut`, `au = wu_cut·hu` via `cutcelwu(5)`. The immersed segment has no link ⇒ zero flux, exact impermeability. | **No code change** (reuse). |
| Small cells | Weighted vertical‑velocity averaging (3D) for cells < 25 %; ADI handles 2D | `Baorgfracmin` floor (existing). The POC accepts the area error and reports it. Cell merging is deferred. | Config only. |
| Pressure gradient | Second‑order extrapolation of ζ from the cut‑cell centroid to the full‑cell centre (App. A) | `g·dxi·(s1(k2)−s1(k1))` with `xz` = full‑cell centre (unchanged) ⇒ first order near banks (paper HCG1 equivalent). | No (slice 3). |
| Velocity at the active part of the edge | Correction term `v − v_cut` (App. B) | Not done; `u1` stays the face‑average normal velocity (FM is a finite volume: `au·u1` *is* the flux). | No. |
| Bed/depth at the active edge | Interpolated to the active‑edge centre | `hu` from `bob` at the link ends (net‑node `zk`). The POC requires a bed defined smoothly through the solid region (test cases do this). | No (documented restriction). |
| Momentum advection | Ghost points at GPs for stencils | FM finite‑volume advection: fluxes through the closed segment are zero. Ghost values are only needed where stencils read solid‑cell `ucx/ucy` (A2). | Slice 2: ghost fill. |
| Cell‑centre velocity (used by advection, friction `v`, viscosity, output) | Not applicable (u, v are separate points) | Perot/`wcxy` normalised weights in cut cells (no boundary constraint). Add a **free‑slip projection**: `u_c ← u_c − (u_c·n_b)·n_b` in cut cells. Optional stage 2: constrained least squares with the active `u1` plus `u·n_b = 0`. | **Yes (slice 1)**. |
| Tangential condition | Neumann on `u_t` via ghost (Eq. 19–20) | Free slip = no wall stress (current `irov = 0` behaviour for walls). For immersed segments nothing is added to `suu`. | No. |
| Horizontal viscosity | Includes GPs | FM viscosity acts through links; there is no link across the segment ⇒ zero stress = free slip. Smagorinsky and `vicouv` near banks use `ucx/ucy`, so they benefit from the projection. | Indirect. |
| Wetting/drying | — | Unchanged (`epshu`, `hu`, `kfs`). A cut cell that dries behaves as an ordinary cell with smaller `ba`. Bank drying on a moving shoreline is out of scope. | No. |
| Boundary conditions | — | Open boundaries must lie on fully fluid links (checked). | Check only. |
| Linear system | Tridiagonal ADI lines; the paper iterates 6–10× | Unchanged global system. The matrix stays symmetric and diagonally dominant because `bbr = bb + a1/Δt`, with `a1 ≥ floor`. | No. |
| Time step | Courant up to ~10 | `setdtorg`: Δt ∝ `vol1/Σq_out`. With floor fraction φ the worst cut‑cell Δt ≈ φ·Δt_full. **Instrument** (§5.4); do not assume stability. | Diagnostic. |

Ghost reflection used in slice 2 (paper Eqs. 19–20), applied to the cell‑centre vector at a ghost
cell:

```
u_GP = −u_IP (nx² − ny²) − 2 v_IP nx ny
v_GP =  v_IP (nx² − ny²) − 2 u_IP nx ny     ⇒ (u_GP+u_IP)·n = 0,  u_t,GP = u_t,IP
```

FM has collocated `ucx`/`ucy` at cell centres, so the paper's u↔v interpolation step (§3.1
step 1) and the GP iteration (6–7 iterations) are **not needed**. One BI/IP per ghost cell is
enough. The only iteration left is when the IP stencil itself contains a ghost cell. That is
resolved by the BI‑constrained system, so no fixed‑point iteration is required.

### 5.3 Solver call sequence (IBM on) [R]

```
flow_modelinit
  readMDUFile                         → ibm_read_config + config‑level validation (LEVEL_ERROR)
  flow_geominit(...)
    delete_dry_points_and_areas; cutcell_list(6,1)        (existing)
    cosphiunetcheck(1)
    ► ibm_check_cartesian(xk,yk,netcell,…) → reduce over ranks → error if not Cartesian
    getcellsurface …            ► ibm_save_full_area (ba_full)
    wu …                        ► ibm_save_full_width (wu_full)
    cutcell_list(4,2); weights; cutcell_list(5,3)          (existing; Baorgfracmin set by IBM)
    walls; setwallorientations; setlinktocenterweights      (existing)
    ► ibm_build_geometry   (final phase only) → ibm_crosscheck_cutcelwu → ibm_report (dia)
  flow_flowinit
    setucxucyucxuucyunew (line 951) ► ibm_apply_velocity_bc(ucx,ucy)
per step:
  flow_initimestep
    setau; setumod
      setucxucyucxuucyunew (setumod.f90:115/126) ► ibm_apply_velocity_bc
    advecdriver
      [itstep==3 branch] setucxucyucxuucyunew (advecdriver.f90:82) ► ibm_apply_velocity_bc
      advec                                   (slice 2: reads ghost‑filled values)
  step_reduce_hydro                           (unchanged)
  flow_finalize_single_timestep               ► ibm_update_diagnostics (when map/his output due)
```

`ibm_apply_velocity_bc` does the following:

- slice 1: projection in cut cells;
- slice 2: ghost fill;
- it is guarded by `if (ibm%active)`, so `ImmersedBoundary = 0` executes no extra arithmetic and
  results stay bit‑identical;
- it operates on owned cells; `ucx/ucy` are then exchanged with `update_ghosts(ITYPE_SALL)` where
  the existing code already does this. W0 checks where that happens; if it does not, add a single
  exchange after the correction.

Iteration and convergence: no new iterations. The FM nonlinear `s1` loop
(`step_reduce_hydro.f90:200‑322`) is unaffected because IBM only changes geometry (`ba`, `wu`,
static) and explicit terms.

### 5.4 Configuration and diagnostics

**MDU keywords [R]** (read next to `Cutcelllist` in `unstruc_model.f90:646` and next to
`Baorgfracmin` at 1119; written next to 2796/3368 only when non‑default or `writeall`, so effective
MDU files of existing models do not change):

| Section | Key | Default | Meaning |
|---|---|---|---|
| `[numerics]` | `ImmersedBoundary` | `0` | 0 off; 1 = POC IBM (Cartesian 2D, free slip). |
| `[numerics]` | `ImmersedBoundaryMinAreaFrac` | `0.1` | Floor on cut‑cell area fraction (sets `Baorgfracmin` if that is 0). |
| `[numerics]` | `ImmersedBoundaryCartesianTol` | `1e‑6` | Relative tolerance of the Cartesian check. |
| `[geometry]` | `Cutcelllist` (existing) | — | Bank polygon(s). Required when `ImmersedBoundary = 1`. |

Reusing `Cutcelllist` avoids a second polygon reader and a second cut‑cell pipeline in the POC.
A dedicated `ImmersedBoundaryFile` can be added later without breaking input. **Decision D1
(before coding)** confirms this choice.

**Cartesian criteria [R]** — all must hold (computed on all 2D net cells local to the rank, then
`reduce` so every rank agrees):

1. `jsferic == 0`; no 1D net links (`numl1d == 0`); `kmx == 0`.
2. Every 2D net cell has exactly 4 nodes (no triangles or pentagons ⇒ mixed meshes rejected).
3. Global spacing: `dx` = median |Δx| over x‑aligned links, `dy` likewise (dx ≠ dy allowed).
4. Every 2D net link is axis‑aligned:
   - either `|Δy| ≤ ε·min(dx,dy)` and `| |Δx| − dx | ≤ ε·dx`,
   - or the same with x and y swapped.

   Rotated or skewed grids fail this test and are reported with the measured angle and skew.
5. Every net node lies on the lattice: `|(x−x0)/dx − nint(·)| ≤ ε` (and likewise for y), where
   `x0, y0` = the minimum node coordinates. Non‑uniform spacing fails here.
6. Uniform `dx`, `dy` across ranks (`reduce` min/max).
7. "Nearly Cartesian" = passes with ε (default `1e‑6`, user‑adjustable up to `1e‑3`). Above that:
   rejected. The message reports `max_dev`, the first offending node/link and its coordinates.

Example messages (`mess(LEVEL_ERROR, …)`):

- `ImmersedBoundary=1 requires a Cartesian 2D mesh: net link 1234 at (x,y) deviates 3.2e-3 (tol 1e-6) from axis alignment.`
- `ImmersedBoundary=1 requires Cutcelllist with at least one polygon.`
- `ImmersedBoundary=1 is not supported with kmx>0 / spherical coordinates / FlowSolver=<x> / Nonlin2D>0.`
- `ImmersedBoundary=1: active region of cell <k> is split by the immersed boundary; refine the grid or simplify the polygon.`

Warnings: `irov /= 0` ("applies to mesh walls only; immersed boundary uses free slip");
`ndegenerate > 0`; `nfloored > 0` with `area_deficit`.

**Diagnostics in the `.dia` file**: the Cartesian result (`dx, dy, x0, y0, max_dev`), `ncut`,
`nghost`, `nfloored`, `min_area_frac`, `area_deficit`, the cross‑check result and a timing line.

**Map output** (new `[output] Wrimap_immersed_boundary = 0|1`, pattern of
`map_write_settings%s1`, A8):

- `mesh2d_ibm_cell_class` (face, int);
- `mesh2d_ibm_area_fraction` (face);
- `mesh2d_ibm_edge_fraction` (edge);
- `mesh2d_ibm_normal_x/y` (face, fill value for non‑cut cells);
- `mesh2d_ibm_normal_velocity` (face, `u_c·n_b`, time‑dependent);
- `mesh2d_ibm_tangential_velocity` (face, time‑dependent).

These are the near‑bank quantities a future erosion model will consume.

**Performance instrumentation**: add `timstrt/timstop('IBM velocity BC', handle)`. Use the
existing `map_write_settings%dtcell` / `flowCourantNumber` (`setdtorg.f90:129‑135`) output to show
which cells limit Δt. Report the fraction of steps limited by cut cells in `ibm_report`.

### 5.5 Serial / MPI behaviour

- Geometry is built per rank on the local net (owned and ghost cells). It is deterministic, so
  owned and ghost copies agree (A3; verified by a test that compares `ba`/`edge_frac` after
  `update_ghosts`).
- The Cartesian check and diagnostics are reduced with the existing `reduce_*` helpers in
  `partition.F90`.
- The velocity BC is computed on owned cells. Ghost‑cell values come from the existing
  `ITYPE_SALL` exchange of `ucx/ucy`.
- Stencil radius: ≤ 2 cells; 4 cell‑based ghost layers (`partition.F90:447`) are sufficient.
- Partitioning (METIS or polygon) needs no change. A cut cell on a partition edge is valid.
- Memory: O(N_cut·~25 doubles + N_ghost·~20 doubles + 2·(ndx+lnx)) — negligible.
- CPU: geometry O(N_cut·N_polyseg_local) once, using the existing k‑d tree; per step O(N_cut).

---

## 6. File‑ and symbol‑level implementation work plan

**W0 — Spike (no merge)**

- Objective: close assumptions A1–A8.
- Files: read only, plus a scratch model built from `e02_dflowfm/f004_bottomfriction/c080_chezy_uniform`.
- Method:
  - run the oblique channel with `Cutcelllist` only;
  - dump `ndx`, `ba`, `wu`, `kfs` and stencil arrays;
  - instrument `sethigherorderadvectionvelocities`;
  - check `flow_geominit` phases;
  - test TestBench expected‑failure support;
  - search for a periodic boundary facility.
- Completion: a short note answering A1–A8, attached to the PR.

**W1 — Configuration**

- Objective: opt‑in keywords and config‑level validation.
- Files/symbols: new module variables in `m_immersed_boundary` (or `K/dflowfm_data/m_flowparameters.f90`
  for the integer flag next to `irov`, line 220). `readMDUFile` (`K/dflowfm_data/unstruc_model.f90`
  near 646 and 1119), `writeMDUFilepointer` (near 2796, 3368), defaults in the same reset routine as
  `irov` (`m_flowparameters.f90:825`).
- Depends on: none.
- Algorithm: `prop_get` plus checks against `kmx`, `flow_solver`, `nonlin2D`, `md_cutcelllist`.
- Compatibility: keywords written only when non‑default.
- Done when: the MDU round‑trip unit test passes and the error tests pass.

**W2 — Cartesian detection**

- Objective: `ibm_check_cartesian` (pure on arrays) and its wrapper using `network_data`.
- Files: `K/dflowfm_kernel/prepost/m_immersed_boundary.f90` (new). Call site:
  `K/dflowfm_kernel/prepost/flow_geominit.f90` after line 297.
- Depends on: W1.
- Algorithm: §5.4 criteria; MPI reduction via `partition.F90` helpers.
- Done when: unit tests U1–U8 pass, and an existing non‑IBM model shows unchanged results.

**W3 — IBM geometry build**

- Objective: classification, fractions, segments, normals, saved full metrics, cross‑check.
- Files: `m_immersed_boundary.f90`. Call sites in `flow_geominit.f90`:
  - save `ba_full` after line 403;
  - save `wu_full` after the `wu` block (975‑995);
  - set `Baorgfracmin` before line 1165;
  - build after line 1314.
- Depends on: W2.
- Algorithm: §5.1, reusing `dbpinpol`, `crosspoly`/`crosslinkpoly`, `dbdistance`, `normalin` and
  `dAREAN` (`cutcelwu.f90:383`).
- Compatibility: none when off.
- Done when: U9–U16 pass, and the cross‑check matches `cutcelwu` on the oblique‑channel mesh.

**W4 — Free‑slip velocity BC (slice 1)**

- Objective: `ibm_apply_velocity_bc(ucx, ucy)` projection in cut cells.
- Files: `m_immersed_boundary.f90`. Call sites: `K/dflowfm_kernel/compute/setumod.f90` after
  lines 115 and 126; `K/dflowfm_kernel/compute/advecdriver.f90` after 82;
  `K/dflowfm_kernel/prepost/flow_flowinit.f90` after 951.
- Depends on: W3.
- Algorithm: `un = ucx·nx + ucy·ny; ucx −= un·nx; ucy −= un·ny`. Also apply to `ucxq/ucyq` if
  `setucxucyucxuucyunew` produced them for Coriolis — check in W0.
- Compatibility: the guard keeps the default path bit‑identical.
- Done when: U17 passes, the oblique IBM case runs, and the normal‑velocity residual is ≈ 0 at
  machine precision for cut cells.

**W5 — Diagnostics and output**

- Objective: `.dia` report, map variables, timer.
- Files: `m_immersed_boundary.f90` (`ibm_report`); map definitions/writes in
  `K/dflowfm_io/unstruc_netcdf.f90` (pattern near 3993‑4140, `t_unc_mapids` in
  `unstruc_netcdf_data.f90`); `[output] Wrimap_immersed_boundary` in `unstruc_model.f90` near 2130.
- Depends on: W3, W4.
- Done when: the variables appear in the map with UGRID attributes, and the TestBench comparer
  reads them.

**W6 — Verification cases (slice 1)**

- Objective: §7 cases I1–I5 and the analytical script.
- Files:
  - new cases under `test/deltares_testbench/data/cases/e02_dflowfm/<new feature folder>/…`,
    tracked with DVC (`HOW_TO_DVC.md`);
  - `<testCase>` entries in a new include or in `configs/include/dimr_dflowfm_*.xml`, wired into
    `configs/dimr/dimr_dflowfm_lnx64.xml` and the win64 counterpart.
- Depends on: W4, W5.
- Done when: the acceptance criteria in §8, stage 1, are met.

**W7 — Ghost fill / image points (slice 2)**

- Objective: ghost list, IP stencils, BI‑constrained interpolation, reflection (Eqs. 19–20), and
  storage as decided by A1/A2.
- Files: `m_immersed_boundary.f90`; a hook inside or after `sethigherorderadvectionvelocities`
  only if A2 shows solid reads.
- Depends on: W4, W0.
- Done when: U18–U22 pass, and the near‑bank error does not get worse than slice 1 (it should
  improve).

**W8 — Annular case and refinement**

- Objective: §7 cases I6–I7.
- Depends on: W7.

**W9 — Documentation**

- Objective: MDU keyword descriptions (`prop_set` strings), a user‑facing note on restrictions, and
  a test‑case `doc` folder (DVC).
- Depends on: W1–W6.
- Note: the external User Manual is not in this repository; raise a docs ticket.

**Later (not POC)**:

- W10: second‑order corrections (paper App. A/B);
- W11: immersed walls as pseudo‑`walls` for `irov = 1|2`;
- W12: 3D (`kmx > 0`);
- W13: moving bank (§9).

No CMake changes are required for kernel sources. Test sources must be added to
`src/engines_gpl/dflowfm/packages/dflowfm_kernel/test/CMakeLists.txt` in both `F90FILES` and
`F2HFILES`.

---

## 7. Testing and verification matrix

### 7.1 Unit tests (f90tw/GoogleTest, target `test_dflowfm_kernel_gtest`)

New files `test/src/test_ibm_cartesian.f90` and `test/src/test_ibm_geometry.f90` (suite names
`test_ibm_cartesian` and `test_ibm_geometry`), patterned after `test_pol_to_cellmask.f90`. Each
test builds a small in‑memory lattice (e.g. 6×6, dx = 10 m), so no files are needed.

| ID | Test | Expected |
|---|---|---|
| U1 | Uniform square lattice | accepted; dx, dy, x0, y0 exact |
| U2 | dx ≠ dy rectilinear‑uniform | accepted |
| U3 | Whole grid rotated 10° | rejected, reason "axis alignment" |
| U4 | One node perturbed by 1e‑3·dx | rejected; reports node id |
| U5 | One node perturbed by 1e‑9·dx | accepted |
| U6 | One triangle cell | rejected "4 nodes" |
| U7 | Non‑uniform spacing in x | rejected "lattice" |
| U8 | Lattice with deleted (dry) cells | accepted |
| U9 | Half‑plane cut at 45° through cell centre | area_frac = 0.5, normal = (√½, √½) ± 1e‑14 |
| U10 | Half‑plane at angle atan(43/80), triangle and trapezoid cuts | analytic area and edge fractions ± 1e‑12 |
| U11 | Line through a net node (within snap tol) | node solid, edge fractions ∈ {0, 1}, no zero‑length segment |
| U12 | Line collinear with a face | no cut cells on that face; adjacent cells fluid/solid only |
| U13 | Polygon vertex (90° corner) inside a cell | nseg = 2, area exact, normal = normalised mean |
| U14 | One‑cell‑wide solid spit splitting a cell | error flagged |
| U15 | Conservation bookkeeping: Σ area_frac·ba_full vs analytic fluid area of a closed polygon | relative error ≤ 1e‑12 (no floor) |
| U16 | Floor accounting: `ba_used = max(frac, φ)·ba_full`; `area_deficit` | exact |
| U17 | Projection: random (u, v), any n | `u'·n = 0` ± 1e‑15; tangential unchanged |
| U18 | Image point: \|GP−BI\| = \|IP−BI\|; IP on the fluid side | ± 1e‑12 |
| U19 | Bilinear weights reproduce a linear field exactly | ± 1e‑13 |
| U20 | BI‑constrained system (one or two ghost points) reproduces a free‑slip‑consistent linear field | ± 1e‑10; condition number reported |
| U21 | Reflection Eqs. 19–20 | `(u_GP + u_IP)·n = 0`, `u_t` equal |
| U22 | Narrow channel fallback | flagged as degenerate, finite value |
| U23 | MDU round trip (extend `test_mdu_file_read_write.f90`) | new keys read/written; absent keys ⇒ defaults; effective MDU unchanged when IBM is off |
| U24 | Config validation (`kmx = 1`, `jsferic = 1`, empty `Cutcelllist`, SRE) | `LEVEL_ERROR` path taken (pattern of `test_unstruc_model.f90`) |

These run in CI with the existing kernel gtest target, in well under 1 s total.

### 7.2 End‑to‑end cases (TestBench, `ref="dimr"`, DVC data)

Paper §4.1 inputs [P]:

- tan β = 43/80, so β ≈ 28.26°. The paper text writes "β = tan(43/80)"; this plan reads it as an
  angle with tangent 43/80, consistent with its use as a grid‑slope ratio. Confirm as decision D2.
- Downstream depth h = 4 m; Chézy C = 65 m^½/s; q_s = 5 m²/s; u_s = 1.25 m/s.
- Width W = 500·cos β ≈ 440.4 m. The channel is therefore exactly 500 m wide measured along y,
  which suits boundaries on grid lines.
- S = (q_s / (C·h^{3/2}))² = (5/520)² ≈ 9.246·10⁻⁵.
- Free slip at the banks. The analytical solution is uniform: h = 4 m, |u| = 1.25 m/s, direction β.

The grid resolution of §4.1 is not stated in `Canestrelli16.md`.

Common set‑up [R]:

| Item | Value |
|---|---|
| Mesh | Cartesian, dx = dy = 25 m (≈ 17.6 cells across W). Refinement set: 50 / 25 / 12.5 m. |
| Domain | x ∈ [0, 3000] m. Channel centreline y = y₀ + x·tan β; banks at ±W/(2 cos β) = ±250 m in y. Bounding box ≈ 3000 × 2120 m. The solid region is removed by `Cutcelllist` mode 6. Active cells ≈ 2.4k at 25 m. |
| Bed | z_b(x, y) = z_ref − S·s, with s = x cos β + y sin β, prescribed at net nodes over the **whole** box (smooth through the solid). |
| Inflow | Discharge boundary on the x = 0 grid line between the banks (500 m), Q = q_s·W ≈ 2202 m³/s. |
| Outflow | Water‑level boundary on x = 3000. `.pli` with 2 points at the bank intersections, values ζ = z_b + 4 there. Linear along the line (exact for uniform flow). |
| Friction | `UnifFrictType = 0`, `UnifFrictCoef = 65`. |
| Viscosity | `Vicouv = 0`, Smagorinsky off (to match the inviscid analytical solution). |
| Other | `irov = 0`; `kmx = 0`; `teta0` default; `iadvec` default 33. |
| Initial | ζ = z_b + 4, u = 0. |
| Time | Automatic Δt (`CFLMax = 0.7`); duration 8 h. The friction timescale C²h/(g·u) ≈ 23 min and gravity‑wave transit ≈ 10 min, so 8 h is ample. Map output hourly, his every 10 min. |
| Observations | Centreline points every 250 m; cross‑section at mid‑channel; observation points in the first cut‑cell row on both banks. |
| Expected runtime | ≲ 1–2 min serial Release at 25 m (≈ 2.4k cells). 12.5 m (≈ 10k cells, ~4× more steps) ≲ 15 min ⇒ nightly only. |

| ID | Case | Purpose | Harness / CI |
|---|---|---|---|
| I1 | **Aligned control**: same physics, β = 0, banks on grid lines (W = 450 m = 18·dx), IBM **off** | FM's own accuracy floor for the metrics | TestBench, CI |
| I1b | Same as I1 with IBM **on** (polygon on grid lines) | Degenerate‑geometry path; must equal I1 within 1e‑10 (no cut cells) | TestBench, CI |
| I2 | **Oblique staircase**: β ≈ 28.26°, IBM off, banks as removed cells (`DryPointsFile` polygon, cell‑centre test) | Paper's "Cartesian solution" baseline | TestBench, CI |
| I3 | **Oblique FM cut cells**: `Cutcelllist` only, IBM off | Isolates what FM already gives (continuity cut cell only, paper Fig. 13 "cut‑cell") | TestBench, CI |
| I4 | **Oblique IBM**: `Cutcelllist` + `ImmersedBoundary = 1` | Target | TestBench, CI |
| I5 | I4 on 2 MPI partitions (`dimr_dflowfm_lnx64_parallel.xml` pattern, `-c 2`) | MPI consistency vs I4 | TestBench parallel config, CI |
| I6 | I4 at 50 / 12.5 m (with I2 and I3 at the same resolutions) | Resolution sensitivity / observed order | TestBench, **nightly** |
| I7 | **Annular channel** (§4.2) [slice 2] | Curved banks, near‑bank velocity | TestBench, nightly |
| N1 | **Non‑Cartesian rejection**: triangular mesh (or perturbed curvilinear) with IBM on | Must stop with the Cartesian error | gtest U3/U4/U6 (CI). TestBench expected‑failure only if A5 confirms support. |
| N2 | Non‑Cartesian mesh with IBM **off** and `Cutcelllist` | Existing behaviour unchanged (no new error) | Covered by the existing regression suite. Add a case if none exists. |

I7 parameters [P]:

- Width 20 m, inner radius 50 m; grid 2 m.
- Centreline slope −0.001, i.e. dz/dθ = −0.06 m/rad.
- C = 40, Q = 40 m³/s.
- Initial water level 1 m.
- Analytical solution: Eqs. 23–24, with D found from Q = ∫ v_θ h dr.

The paper uses periodic BCs. FM support for these is unverified (A6). Without them, use a 270°
open sector with Q inflow and the analytical ζ(r) imposed at the outflow (multi‑point `.pli`), and
evaluate only the middle 90°.

**Storage and versioning.** Each case gets a DVC‑tracked `input/`, `reference/` and `doc/`
(`HOW_TO_DVC.md`). Inputs are generated by a committed, deterministic Python generator stored in
the case `doc/` folder. It writes the `.nc` net, `.pol`, `.pli`, `.bc`, `.ext` and `.mdu`, and the
parameters above are its only inputs. The `<path version="DVC">` convention is used as in
`dimr_dflowfm_parallel.xml:489`.

**TestBench checks (regression).**

- `mesh2d_s1`, `mesh2d_ucx`, `mesh2d_ucy`, `mesh2d_ucmag`: `toleranceAbsolute = 1e‑5`.
- `mesh2d_ibm_*` (I4–I6): `1e‑10` for static fields.
- his `waterlevel`, `x_velocity`, `y_velocity`, `cross_section_discharge`: `1e‑5`.
- Cumulative discharge: `toleranceRelative = 1e‑7`.

Tolerances are tighter than the 1e‑4 of `c080_chezy_uniform` because the runs are short and
deterministic. Loosen them only if platform differences show up.

**Analytical acceptance metrics.** TestBench compares to references only. Computing analytical
errors in TestBench is not verified (A5). The metrics are therefore produced by a versioned script
`doc/verify_metrics.py` per case (xarray/netCDF4). The script is run during review and in a nightly
job, and its output table is stored in the case `doc/`. All metrics use the last map time, over the
evaluation region `R` = cells whose centre has 500 m ≤ x ≤ 2500 m (excluding inflow/outflow
transients):

| Metric | Definition |
|---|---|
| Depth error | `E_h = ‖h − 4‖₂,R / 4` and `‖·‖∞,R` (h = s1 − bl at active cells) |
| Depth uniformity | `CV_h = std_R(h) / mean_R(h)` |
| Centreline | Linear interpolation along y = y₀ + x tan β; `E_h,cl`, `E_u,cl` = relative L2/L∞ of h and \|u\| |
| Near‑bank tangential velocity | In the first cut/fluid cell row of each bank: `E_t = ‖u_c·t − 1.25‖₂ / 1.25`, t = (cos β, sin β) |
| Bank normal velocity (reconstructed) | `max \|u_c·n_b\| / 1.25` over cut cells (I3: diagnostic only; I4: should be ~1e‑14) |
| Bank leakage (flux) | `Σ_cut \|net continuity residual\|` checked from his volume error plus `Q_in − Q_out`. Structurally zero: no link exists through the segment. |
| Mass conservation | his water balance (A7): `\|ΔV − ∫(Q_in − Q_out)dt\| / V` at the final time |
| Steady state | `max_R \|s1(t_end) − s1(t_end − 1 h)\| / 3600 s` |
| Direction error | `mean_R \|atan2(ucy, ucx) − β\|` |
| Observed order (I6) | `p = log₂(E(2Δ) / E(Δ))` for E_h and E_t |
| Δt impact | Mean Δt of I4 vs I2/I3; % steps limited by cut cells (from `dtcell`) |
| MPI (I5 vs I4) | `max \|s1_par − s1_ser\|`, `max \|uc_par − uc_ser\|` |

---

## 8. Validation and acceptance criteria (staged)

The thresholds below are **initial targets**. They are calibrated in W6 as follows:

1. Run I1 and record FM's floor (`E_h^I1`, `E_t^I1`, mass error).
2. Set the absolute thresholds to `max(target, 3 × I1 value)`.
3. Freeze them in the case `doc/` together with the measured values.

Relative thresholds (IBM vs baselines) are not loosened without a review decision. Second‑order
accuracy and machine‑precision conservation are **not** claimed. Conservation is required only
relative to I1's solver tolerance.

**Stage 0 — no regression (gate for every PR)**

- The full existing FM TestBench suite and unit tests are unchanged. With IBM off, outputs are
  bit‑identical (verified on a sample of existing cases, including one `Cutcelllist`‑free and one
  MPI case).
- I1b = I1 within 1e‑10.

**Stage 1 — slice 1 (first merge: W1–W6)**

| Metric | Target |
|---|---|
| Runs to steady state | I4 completes without Δt collapse. Steady‑state drift < 1e‑7 m/s. Mean Δt(I4) ≥ 0.25 × mean Δt(I1) with φ = 0.1 (report if lower). |
| Mass | Relative water‑balance error of I4 ≤ max(1e‑9, 10 × I1). |
| Reconstructed normal velocity | max \|u_c·n_b\| / 1.25 ≤ 1e‑12 in I4 (by construction). Report I3 value. |
| Depth uniformity | CV_h(I4) ≤ 0.5 × CV_h(I2) **and** ≤ 1 %. |
| Depth error | E_h,L2(I4) ≤ 1 %, E_h,∞(I4) ≤ 3 %. |
| Centreline | E_u,cl,L2(I4) ≤ 2 %. |
| Near‑bank tangential | E_t(I4) ≤ 0.5 × E_t(I2) and ≤ E_t(I3). Absolute target ≤ 5 %. |
| Direction | mean error ≤ 1°. |
| MPI | I5 vs I4: ≤ 1e‑6 m and ≤ 1e‑6 m/s (solver‑tolerance level); geometry fields identical. |
| Non‑Cartesian | U3, U4, U6 and the config error tests pass; message names the criterion. |

**Stage 2 — slice 2 (W7–W8)**

- E_t(I4) does not get worse versus stage 1.
- I6: observed order p ≥ 0.8 for E_h and E_t (first‑order‑consistent), with the error decreasing
  monotonically.
- I7: E_h,L2 and E_vθ,L2 ≤ the staircase errors at the same resolution, with ζ(r) superelevation
  sign and magnitude within 20 % of Eqs. 23–24 in the evaluation sector.

**Stage 3 — later**: second‑order corrections, aiming at p ≈ 2 in the I7 L2 norm as in paper
Table I. This stage claims it only if measured.

**Ways to fail**: if I4 does not beat I3 by the stage‑1 relative margins, the momentum
reconstruction is not the bottleneck. In that case escalate to W10 (pressure‑gradient
extrapolation) before adding more ghost machinery (see §9).

---

## 9. Risks, unresolved questions, fallbacks

**Decisions required before coding**

| ID | Decision | Recommendation |
|---|---|---|
| D1 | Reuse `Cutcelllist` as the IBM polygon input vs a new `ImmersedBoundaryFile` | Reuse for the POC (no second geometry pipeline). Add the dedicated key when moving banks need an IBM‑owned cut. |
| D2 | Interpretation of "β = tan(43/80)" | tan β = 43/80 (β ≈ 28.26°). |
| D3 | Area floor φ default | 0.1, and calibrate in W6 (paper reports stable cells down to 1e‑5 with ADI; FM's explicit CFL cannot afford that). |
| D4 | Which Cartesian grids to accept | Axis‑aligned uniform (dx ≠ dy allowed). Reject rotated and non‑uniform for now. |
| D5 | Location of new test cases (`e02_dflowfm/<feature folder>`) and which config XML includes them | Ask the FM TestBench maintainers for the folder number. Do not reuse an existing family. |

**Can be deferred**: dedicated IBM polygon file; rotated grids (global rotation θ fitted from link
directions — the lattice test generalises with a rotation); non‑uniform rectilinear grids (bilinear
stencil generalises); immersed partial/no slip via pseudo‑walls; 3D; cell merging.

**Risks**

| Risk | Impact | Mitigation / fallback |
|---|---|---|
| Small cut cells limit Δt (explicit advection CFL `setdtorg`) | Slow runs or instability | φ floor (existing `Baorgfracmin`); report % limited steps. Fallback: cell merging (paper‑like area‑weighted coupling of a small cell with its largest neighbour); or exclude IBM cut cells from the CFL check if `ja_timestep_*`‑style options allow — needs a stability study first. |
| Floor alters the effective planform area | Small mass‑consistent bias (area error) | Report `area_deficit`; refinement study I6. |
| `cutcelwu` behaviour differs from the IBM geometry (snap rules, `BAMIN`, `ba` vs `numlimdt_baorg`) | Cross‑check failures | Align snap tolerance. If impossible, IBM overwrites `ba`/`wu` from its own geometry (one assignment loop) and the cross‑check becomes informational. |
| Pressure gradient uses full‑cell centres (`xz` unchanged) | First‑order error near banks may dominate | Stage 3 W10: least‑squares ζ extrapolation (paper App. A). The paper found the explicit variant unstable in the coupled step and used a partially implicit one; in FM it would enter `slopec`/`ds` in `furu`. Treat as a research item. |
| Cut‑cell Perot weights (`wcxy` normalisation) are inaccurate | Near‑bank velocity error even with projection | Stage 2: constrained least squares (reuse `reconst2nd` infrastructure in `setucxcuy_leastsquare.f90`). |
| Bed levels at cut links taken from solid net nodes | Wrong `hu` near banks | POC requires a smooth bed through the solid (documented). Later: interpolate the bed to the active edge centre (paper Fig. 3c). |
| A1/A2 wrong (solid cells are present / stencils read them) | Ghost storage design changes | W0 spike before W7; the module API hides the storage. |
| MPI geometry mismatch (A3) | Inconsistent `ba` across ranks | Test I5 plus an assertion in `ibm_crosscheck` comparing `ba` after `update_ghosts`. |
| Restart | IBM changes `ndx` (solid cells removed) — the same as current `Cutcelllist` | A restart is valid only with the same mesh and polygon. Detect `ndx`/`lnx` mismatch (existing restart reader behaviour) and document it. No new restart fields. |
| Analytical metrics outside TestBench | Risk of drift | Nightly job plus stored metric tables; promote to a TestBench check if a custom comparer is acceptable. |

**Moving‑bank follow‑up (design only)**:

1. `ibm_rebuild(polygon)`: restore `wu = wu_full` and `ba = ba_full` for affected cells, recut
   locally (k‑d tree query on the changed polygon bounding box), recompute fractions, weights for
   affected cells only (`setlinktocenterweights` is global today — needs a local variant) and
   ghosts.
2. Remap state: conservative volume redistribution for cells whose fraction changes. Newly wet
   cells take neighbour‑averaged `s1`; newly solid cells give their volume to neighbours. Keep
   `vol1` and the balance.
3. Bank erosion couples to `mesh2d_ibm_tangential_velocity`, normal and segment length per cut
   cell (shear via the near‑bank velocity and log law). The morphology interface is a separate plan.

The POC prepares for this by owning `wu_full`/`ba_full` and a single build entry point, and by not
storing geometry in restart files.

---

## 10. Suggested implementation order and review checkpoints

1. **W0 spike** → **Checkpoint A**: assumptions note, decisions D1–D5 confirmed.
2. **W1 + W2** (config + Cartesian check + U1–U8, U23, U24) → **Checkpoint B**: design review of
   the module API and messages. No behaviour change when off.
3. **W3** (geometry + U9–U16 + cross‑check) → **Checkpoint C**: geometry fields inspected on the
   oblique mesh (plots in the PR).
4. **W4 + W5 + W6** (projection, diagnostics, cases I1–I5) → **Checkpoint D = first merge
   (smallest useful vertical slice)**:
   - opt‑in IBM on Cartesian meshes;
   - cut‑cell continuity (existing) plus free‑slip reconstruction;
   - diagnostics and map fields;
   - unit tests plus 6 TestBench cases;
   - stage 0 and stage 1 criteria met.
5. **W7 + W8** (ghost fill, annular, refinement) → **Checkpoint E**: stage 2.
6. **W9** documentation, alongside each merge.
7. Later: W10–W13, each with its own plan and review.

---

## 11. Commands and evidence needed to validate the completed work

Commands come from `.github/skills/*/SKILL.md` and `doc/unit-testing.md`. On Linux the build
directory is `build_<config>_<build-type>`, per the build‑delft3d skill.

```bash
# Format changed Fortran (repo root; uses .fprettify.rc)
tools/fprettify/run_fprettify.sh src/engines_gpl/dflowfm/packages/dflowfm_kernel

# Build FM (Debug for development; Release/RelWithDebInfo for TestBench timing)
python build.py --config dflowfm --build-type Debug --build --keep-build

# Unit tests (f90tw/gtest names are <suite>.<test>)
ctest --test-dir build_dflowfm_debug --tests-regex 'test_ibm|test_mdu_file_read_write|test_unstruc_model' --output-on-failure
ctest --test-dir build_dflowfm_debug --output-on-failure        # full unit suite, stage 0

# TestBench (from test/deltares_testbench, inside its venv)
cd test/deltares_testbench
python TestBench.py --compare --config configs/dimr/dimr_dflowfm_lnx64.xml \
    --filter testcase=<I1,I1b,I2,I3,I4 names>
python TestBench.py --compare --config configs/dimr/dimr_dflowfm_lnx64_parallel.xml \
    --filter testcase=<I5 name>
# Stage 0 regression: run the full dflowfm configs (or TeamCity), compare against main.

# Analytical acceptance (per case, after a run)
python data/cases/e02_dflowfm/<feature>/<case>/doc/verify_metrics.py <work_dir>/dflowfmoutput
```

**Evidence to attach to the first‑merge PR**:

- W0 assumptions note (A1–A8).
- Unit test log (all `test_ibm_*` passing).
- `.dia` excerpts for I4: Cartesian check result, `ncut`, `nghost`, `nfloored`, `min_area_frac`,
  `area_deficit`, cross‑check OK, IBM timer.
- Metric table for I1/I2/I3/I4 (§7.2) against the §8 stage 1 targets, plus calibration values
  from I1.
- Plots: plan view of h and |u| for I2, I3 and I4 (like paper Fig. 4); centreline h and |u| (like
  Fig. 5); near‑bank tangential velocity profile.
- Δt statistics (I4 vs I1/I2) and % steps limited by cut cells.
- MPI comparison I5 vs I4.
- Proof of no regression with IBM off (TestBench run or TeamCity build link).
- Rejection messages for U3/U4/U6 and the config errors.
