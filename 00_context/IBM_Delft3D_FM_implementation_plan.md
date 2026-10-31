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
- all momentum terms on cut links still use uncut geometry: `xz`, `dx/dxi`, `acl`, and (with
  `BedlevType = 1`) cut‑cell bed levels sampled at `xz`, possibly inside the solid. So the pressure
  gradient uses wrong positions for `s1`;
- cut‑cell centre velocities (`ucx`, `ucy`) are reconstructed with ad‑hoc weights (uncut
  centre‑to‑face distance, normalised by the sum of weight magnitudes, not the wet area) and no
  boundary constraint;
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
3. **Momentum on cut cells (substantial — see §5.3).** Today, cut cells change only the
   continuity equation. Every momentum ingredient on and next to cut cells still uses the
   **uncut** geometry: cell centres `xz`, link distances `dx/dxi`, interpolation weights `acl`,
   Perot reconstruction weights, and cut‑cell bed levels sampled at `xz` (possibly inside the
   solid). Four changes are needed:
   - **M1 — pressure gradient.** Make `g·dxi·Δs1` consistent with `s1` being the mean over the
     *wet* polygon. Step 1 is implicit: a normal‑projected wet‑centroid distance. Step 2 is an
     explicit tangential (non‑orthogonality) correction, which needs a stability study. This is
     the dominant near‑bank error and maps to paper §3.2.1, §3.2.2.2 and App. A.
   - **M2 — cut‑link geometry.** Use wet‑geometry `acl`, and with `BedlevType = 1` (decided,
     required for morphodynamics) evaluate cut‑cell `bl` at the wet centroid.
   - **M3 — velocity reconstruction.** Perot reconstruction over the wet polygon, with the
     immersed segment as a zero‑flux face. It is exact for any constant velocity tangential to
     the wall, so free slip is built in.
   - **M4 — small cut edges.** Treat advection on edges with a small wet fraction (paper Fig. 3d).

   M1 step 1, M2 and M3 are in the **first** slice. Ghost‑cell reflection (paper Eqs. 19–20) is
   needed only for higher‑order advection stencils that reach into solid cells, and comes in the
   second slice.
4. **Verification.** Use the paper's §4.1 oblique uniform‑channel case in four variants: aligned
   control, staircase, FM cut cells only, and IBM. §4.1 is **insensitive** to the M1 error,
   because the surface gradient is parallel to the bank. So the first slice also includes:
   - a **closed oblique basin with wind set‑up** (zero‑velocity equilibrium under a gradient
     oblique to the bank);
   - the **annular channel** (§4.2), which has a gradient across the channel;
   - an along‑channel **momentum‑budget** metric.

   These run as TestBench cases for regression, plus a versioned analytical‑metrics script for
   acceptance. Fortran gtest (`f90tw`) unit tests cover geometry and the discrete operators.

Do **not** add a new `FLOW_SOLVER_*` branch in `flow_run_single_timestep` for the POC. IBM changes
geometry and explicit momentum terms, not the time‑integration driver. That select‑case remains the
right place for a later dedicated Cartesian solver (Strategy 3).

The FM scheme is θ‑implicit only in the water‑level gradient and explicit in advection and
viscosity, so the paper's unconditional‑stability argument for ADI **does not transfer**. The
time step is limited by `cflmx·vol1/Σq_out` per cell (`setdtorg`), so small cut cells reduce Δt.
The POC limits this with the existing `Baorgfracmin` floor and makes the resulting area error
visible in diagnostics. Accuracy targets are staged: first order near banks first; second order is
not claimed.

**Momentum conservation.** Neither the paper nor FM is discretely momentum‑conserving. The paper
claims mass conservation only and uses non‑conservative finite‑difference advection. FM's
`iadvec = 33` sums momentum fluxes per cell with telescoping face fluxes, then interpolates them
to each link. Physically, a bank also exerts a normal pressure force. The verifiable requirements
are therefore:

- zero momentum flux through the immersed segment (structural: no face exists there);
- zero tangential wall force (free slip);
- no spurious force from inconsistent cut‑cell geometry, which is the M1/M2 risk.

The last one is measured by the momentum‑budget and wind set‑up tests (§7–§8).

**Strategy checkpoint.** M1 step 2 may need a partially implicit correction, as the paper reports.
That would add couplings outside the 2‑point link stencil that `s1ini` assembles via `ccr(Lv2(L))`.
If it does, this is the evidence to move to Strategy 3, or to extend the matrix. That decision is
taken at Checkpoint E (§10).

### 1.1 Implementation status and review findings (review of 2026‑10‑31)

Branch `all/poc/UNST-10481_IBM_bank_erosion`, compared against its merge base `4877ed02e5`
(8 commits; 14 source files, about 3,100 added lines). The review traced every source change
against this plan, ran c05 and c02 (Debug build, focused targets `dflowfm-cli`, `dflowfm_dll`,
`dimr`), and made a short scratch run of the annulus cut‑cell case c08 with `Conveyance2D = -1`.

**Status per work package.**

| WP | Status | Notes |
|---|---|---|
| W0 | Not delivered as a note | Pre‑implementation trials exist (§7.2 items 6–7); A1–A11 and the M1 error measurement are still open. |
| W1 | Implemented | All six keywords read, validated and written only when non-default; U23 round trip passes. F4–F6 validation fixes are in place. |
| W2 | Implemented | `ibm_check_cartesian` + MPI verdict reduction; U1–U8 pass. Grid spacing/origin reductions are also in place; formal U36 owner/ghost assertions remain. |
| W3 | Implemented, **design deviation** | `ibm_build_geometry` measures the clipped polygons `nd(n)%x/y` produced by `cutcelwu` instead of recomputing the polygon/net intersections independently (§5.1 Lifecycle 4). The cross‑check therefore only checks self‑consistency of `ba`/`wu` with `nd` and cannot detect wrong cut polygons (F1, F3). `cutcelwu` got an IBM‑specific crossing path (k‑d tree crossing count per net link, interior‑vertex test). |
| W3b | Partially implemented | Integrated cut-cell safeguards and configuration fixes are in place; focused geometry/Cartesian tests pass (26/26). c06 now passes a two-rank 0.05-hour smoke run. c09/c14 still hit the intentional multiple-polygon guard; union clipping and formal U31–U36/geometry-verifier coverage remain. |
| W4 | Implemented (weights) | `ibm_set_perot_weights` overrides `wcx1/wcy1/wcx2/wcy2` on cut-cell sides with `±(x_f − c)·l_f/A_w`; sign convention matches `setlinktocenterweights`. `PerotWeightUpdate` rejected. Unsupported tangential-correction/projection settings are now rejected (F5); the algorithms are not implemented. Model-level U27 not done. |
| W4b | Implemented | `dxi_wet` used in `furu` `gdxi`; `acl_wet` in the `advec` `kmx = 0` `volu`/`advel` weighting; wet bed by LSQ fit from uncut wet neighbours within two link hops, applied before `setbobs` in `flow_geominit` and again after restart (F9). Hooks use per-link `allocated`/`size` checks instead of a precomputed override array (F11). |
| W5 | Not started | No `.dia` report (`ncut`, `nfloored`, `min_area_frac`, `area_deficit`, `ndegenerate` are computed but never written; only `nclipped_dn` is warned), no `Wrimap_immersed_boundary`, no timer, no `irov` warning. |
| W6 | Inputs generated, **not fully validated** | `f600_ibm/doc/generate_cases.py` produces c01–c33 (input + doc); DVC pointers staged locally, not pushed or committed. c06 has a direct MPI smoke run; `verify_metrics.py`, references and TestBench XML are still missing. |
| W7–W9 | Not started | As planned. |

**Progress update (2026-10-31):** The c06 two-rank 0.05-hour PETSc run completes after inactive
(`kfs = 0`) cells are assigned a unit diagonal while retaining their zero right-hand side. The
previous PETSc failure was caused by these all-zero rows, not by nonfinite active-cell diagonals.
The rebuilt run exited successfully with no divergence/error/nonfinite log markers; `mesh2d_s1`,
`mesh2d_ucx`, and `mesh2d_ucy` are finite in both rank-local and combined map outputs. This is a
direct smoke run, not the planned geometry verifier or formal U36 assertion.

Unit tests present: `test_ibm_cartesian` (U1–U8), `test_ibm_geometry` (U9–U13, U15, U16, U28,
U29, Cutcelllist validation), `test_ibm_momentum` (U25, U27) and the IBM MDU round trip (U23).
They exercise the pure routines of `m_immersed_boundary` on hand‑made polygons. **None of them
exercises the integrated `cutcelwu` → `ibm_build_geometry` path**, which is where F1–F3 occur.
Still missing: U14 as a `cutcelwu` case, U17–U22, U24, U26, U30, and the new U31–U36 (§7.1).

**Findings** (severity: B = blocks running the cases; H = high; M = medium; L = low).

| ID | Sev. | Finding | Evidence | Integrated in |
|---|---|---|---|---|
| F1 | B | **Polygon through a net node drops faces from cut polygons.** In the IBM branch of `cutcelwu` (`K/dflowfm_kernel/prepost/cutcelwu.f90`, IBM crossing block in the `n12 ≥ 4` loop) a k‑d tree crossing at a link *end point* sets `ja = 1`. When both link nodes are outside the polygon (`kc ≠ 1`), the `n12 = 5` branch adds no vertices, so the cut polygon of the cell loses that face while `wu` stays full. `ibm_build_geometry` then fails with "wet link face could not be recovered from the cut‑cell polygon". The legacy path only calls `crosslinkpoly` when a node has `kc = 1` and does not have this problem. | c05 fails at link 4960. All oblique channel layouts hit it: the banks `y = 60 + 0.5375 x` and `y = 560 + 0.5375 x` pass exactly through nodes (400, 275), (2400, 1350), (400, 775), (2400, 1850) at dx = 25 m; 2–6 node contacts in c05, c06, c18, c21. | §5.1 degenerate cases; W3b; U31; §7.2 item 5 |
| F2 | B | **Open‑boundary links next to cut cells are rejected.** `ibm_state%cell_class` is sized `ndx` and defaults to 2 (solid). For a boundary link `ln(1, L)` is the virtual boundary node, so `ibm_set_wet_link_metrics` raises "active IBM cut link is adjacent to a solid cell" for every wet boundary link of a cut cell. | Code trace (`m_immersed_boundary.f90`, `ibm_set_wet_link_metrics`; `ba(ln(1,L)) = ba(ln(2,L))` in `flow_geominit.f90` confirms the convention). c05 hits this as soon as F1 is fixed (banks cross both open boundaries). | §5.1; W3b; U32 |
| F3 | B | **Several polygons per solid give wrong geometry.** `cutcell_list` processes the polygons one at a time and `cutcelwu(5)` rebuilds each cut polygon from the full cell, so a cell cut by two polygons keeps only the last cut. Cells straddling an edge *shared* by two adjacent polygons are not fully inside either polygon and survive as active cut cells inside the solid. Overlapping polygons are safe away from the interface. | Scratch run of c08: **97 active cells lie entirely inside the solid** with area fraction 0.13–1.0 (along the shared inner‑disk / gap arc), and 112 cells have an area fraction more than 0.1 off the true wet fraction. Affects the annulus cases (c07–c09, c22–c27) and, by code reading, 6–8 corner cells of each basin case (c10–c15, c28–c33). | §5.1; W3b; U33; §7.2 items 5–6 |
| F4 | B | **`BedlevType = 1` requires `Conveyance2D ≤ 0`.** The FM default `Conveyance2D = 1` makes `flow_geominit` call `qnerror('bed-level type and conveyance type do not match')`. The generated MDUs do not set it. | c02 stops at initialisation with that message. | §5.5; W1; §7.2 item 4 |
| F5 | H | `ImmersedBoundaryTangentialCorrection = 1` and `ImmersedBoundaryProjection = 1` are accepted but not implemented; `ImmersedBoundarySmallEdgeFrac` only sets a flag. | c15 would run identically to c14 without any message. | §5.5; W1 |
| F6 | H | The `jsferic` check in `readMDUFile` runs before `loadNetwork`, so it always passes. A regular lat/lon mesh passes the lattice test. | `readMDUFile` is called before `loadNetwork` in `loadModel`. | §5.5; W2; U24 |
| F7 | M | Cell classification uses `area_tolerance = max(1e‑12, ImmersedBoundaryCartesianTol)`. Cells with `area_frac ≤ tol` become class 2 although `cutcelwu` keeps them active with a floored `ba`; their wet links then fail with "adjacent to a solid cell". Cells with `area_frac ≥ 1 − tol` keep the legacy cut‑cell weights. | Code trace (`ibm_build_geometry`). With `CartesianTol` up to 1e‑3 this is reachable. | §5.1; W3b; U34 |
| F8 | M | Cells that only touch the polygon at a node are flagged `kfs = 1` by `cutcelwu(4)` and get the legacy normalised `wcxy` weights, but are class 0, so the IBM Perot override never reaches them. | Code trace (`setlinktocenterweights.f90`, `kfs` branch). | §5.1; W3b |
| F9 | M | Map restart re‑applies the wet‑bed fit, which would overwrite morphologically evolved `bl` of cut cells (§5.3 M2 forbids this). Idempotent for pure hydrodynamics. | `load_restart_file` → `update_ibm_wet_bed_after_restart`. | §9 Restart row; W4b |
| F10 | M | The original MPI/ghost-layer risk is addressed in code: grid spacing/origin are reduced across ranks, wet-bed fitting is owner-only, and ghost geometry is synchronized. The two-rank c06 0.05-hour smoke run now passes with finite flow fields; formal U36 owner/ghost equivalence assertions remain. | `flow_geominit` MPI reductions and wet-bed synchronization; c06 direct run after the PETSc inactive-row fix. | §5.6; W3b; U36 |
| F11 | L | Hot‑loop hooks in `furu`/`advec` do `allocated`/`size`/index checks per link inside OpenMP loops. Results with IBM off are unchanged, but the overhead is unmeasured. | Diff of `furu.f90`, `advec.f90`. | W4b; Stage 0 |
| F12 | L | The IBM error message in `cutcelwu` says "a cut cell has no active grid node" but counts nodes with `kc = 1`, i.e. *inside* the polygon (solid). The `cycle` after `mess(LEVEL_ERROR)` is unreachable because `threshold_abort = LEVEL_ERROR` aborts. | Code trace. | W3b |
| F13 | L | `ibm_find_crossed_links` closes the polygon with `xpl(1)` and assumes one polygon per `.pol` file; `dmiss`‑separated multi‑polygon files are not handled. | Code trace. | §5.5; W3b |
| F14 | L | The full `fm-suite` build fails in the unrelated `preC-SUMO` test target (`undefined symbol …basic_stringstream…str`); D‑Flow FM targets build. | Build log, 2026‑10‑30. | §11 |

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
| Cut‑cell weights | `K/dflowfm_kernel/prepost/setlinktocenterweights.f90:64‑97,141‑168` | `wcx1/wcy1/…`, `wcxy` | Perot weight = `csu·acl·wu·dx` (uncut centre‑to‑face distance). Normal cells: × `bai`. Cut cells (`kfs = 1`): ÷ `wcxy` = Σ\|weights\|, not the wet area. |
| Link weights | `flow_geominit.f90:1057‑1065` | `acl(L)` | `acl = dxn1e/(dxn1e+dxn2e)` from distances of the **uncut** centres `xz` to the net link. Computed before `cutcell_list(5,3)`. |
| Bed levels | `K/dflowfm_kernel/prepost/setbobs.f90:39‑152`; MDU description of `BedlevType` (e.g. `tt3.mdu` in `e02_f004_c080`) | `setbobs`, `ibedlevtyp`, `bob`, `bl` | `BedlevType = 1` (`BEDLEV_TYPE_WATERLEVEL`): `bl` is delivered at cell centres (tiles at `xz,yz`, e.g. via an `[Initial] quantity = bedlevel` block); `setbobs` only fills missing values; `bob = max(bl(k1), bl(k2))`. Types 3–6 derive `bob`/`bl` from net‑node `zk` (including nodes inside the solid). Example type‑1 case: `e02_dflowfm/f040_subsidence/c01_uniform_ibed_1`. |
| Advection (default) | `K/dflowfm_kernel/compute/advec.f90:192‑203,397‑425` | `advec`, `uqcx/uqcy`, `sqa`, `volu` | `iadvec = 33`: per cell `uqc = Σ_faces qa·ucxu` (telescoping face fluxes), link term `(acl·qu1 + (1−acl)·qu2)/volu` with `volu = acl·vol1(k1) + (1−acl)·vol1(k2)`. No flux through a non‑existent face. |
| Pressure term | `K/dflowfm_kernel/compute/furu.f90:147‑157` | `gdxi = ag·dxi(L)` | Enters `fu` (implicit part, hence the matrix via `s1ini`) and `du` (explicit part). |
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
3. IBM is allowed only on a proven Cartesian mesh (§5.5). Other meshes get a fatal error that
   states the failed criterion; there is never a silent fallback.
4. Impermeability at the immersed boundary: no flux through the solid part of cut cells.
   Free‑slip tangential condition.
5. Momentum terms on and next to cut cells use wet geometry: a pressure gradient consistent with
   `s1` at wet centroids, wet bed levels, wet control volumes, and a wall‑consistent cell‑centre
   reconstruction. No spurious tangential force at the bank (§5.3).
6. Mass conservation with respect to the discrete cell areas actually used (`ba`), with the
   floor‑induced area deviation reported.
7. Serial and MPI runs give consistent IBM metadata.
8. Diagnostics: class counts, minimum area fraction, floored cells, Cartesian‑check result, and
   per‑cell map fields.
9. Unit, regression and analytical‑verification tests (§7).

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
- A9: Whether `setucxucyucxuucyunew` builds `ucx/ucy`, `ucxq/ucyq` and the advection velocities
  `ucxu/ucyu` from the same `wcx1…` weights, so that M3 propagates to all of them.
- A10: For `BedlevType = 1`, the initialisation order of the `bedlevel` initial field (interpolated
  at `xz`) relative to `setbobs` (`flow_flowinit.f90:934`), and all later `setbobs`/`bl` updates
  (morphology, `bathyupdate`). This places the M2 hook (re‑evaluate `bl` of cut cells at the
  wet centroid, before `bob = max(bl)` is formed).
- A11: Other users of `gdxi`/`dxi` in the momentum path that M1 must also cover. Known ones:
  `advecdriver.f90:76` (start‑up explicit step), `u1q1`, structures, fixed weirs, `iadvec`
  branches other than 33.

**Explicit non‑goals (POC)**

- Bank erosion, a moving or time‑dependent boundary, morphology coupling.
- 3D (`kmx > 0`), spherical coordinates, 1D/1D2D networks, the SRE solver, `Nonlin2D`
  volume tables.
- Rotated, non‑uniform rectilinear or curvilinear grids (rejected; listed as extensions).
- No‑slip or rough immersed walls (design hook only).
- Proven second‑order accuracy (paper HCG2). M1–M4 implement the consistency corrections that
  lead there, but a partially implicit pressure correction (if needed) is out of the POC scope (D8).
- Cell merging for small cut cells.
- A dedicated Cartesian/ADI solver.
- Refactoring of `cutcelwu`/`cutcell_list` beyond what IBM needs.

---

## 4. Alternatives considered and decision matrix

| Criterion | S1: change unstructured flux/continuity formulation with embedded geometry | **S2: IBM geometry layer + narrow momentum hooks, current solver** | S3: dedicated Cartesian (ADI‑like) solver |
|---|---|---|---|
| Conservation | Exact if done right, but continuity code needs new logic | Exact w.r.t. `ba`/`wu` — reuses verified `cutcelwu` path and unchanged `s1ini`/`s1nod` | Exact (paper) |
| Near‑bank accuracy | Same potential as S2 | With M1–M3: consistent normal pressure gradient and wall‑consistent reconstruction; tangential gradient correction (M1 step 2) is explicit and needs a stability study. First order is expected near banks until M1 step 2 is proven. | 2nd order (paper HCG2) |
| Stability | Explicit CFL with small `vol1` | Same; mitigated by `Baorgfracmin` floor (cost: area error) | ADI, Courant up to ~10 (paper) |
| Code size | Large (`s1ini`, `furu`, `volsur`, `setau` all touched) | Medium: 1 new module (~3–4 kLOC incl. tests), ~10 call sites (`flow_geominit`, `setbobs`, `setlinktocenterweights`, `furu`, `setumod`, `advecdriver`, `advec`, `flow_flowinit`). Continuity and matrix assembly untouched. | Very large (new solver, I/O, MPI, BCs) |
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

type t_ibm_cut_link         ! one per flow link with at least one cut neighbour cell (M1/M2/M4)
   integer  :: L
   real(dp) :: edge_frac                    ! wu_cut / wu_full (1 for uncut links next to cut cells)
   real(dp) :: xm, ym                       ! midpoint of the wet part of the face
   real(dp) :: dxi_wet                      ! 1 / ((c2 − c1)·n_L), c = wet centroids (M1 step 1)
   real(dp) :: tvec(2), dt12                ! face tangent and tangential offset (c2 − c1)·t (M1 step 2)
   real(dp) :: acl_wet                      ! wet-geometry left weight (M2)
   real(dp) :: bob_wet(2)                   ! bed levels at the wet-face end points (M2)
   logical  :: small_edge                   ! edge_frac < ImmersedBoundarySmallEdgeFrac (M4)
end type

type t_ibm_state
   logical :: active = .false.
   type(t_ibm_cartesian) :: grid
   integer, allocatable  :: cell_class(:)   ! (ndx) 0 fluid, 1 cut, 2 solid/absent, 3 ghost-referenced
   real(dp), allocatable :: edge_frac(:)    ! (lnx) wu_cut / wu_full
   real(dp), allocatable :: wu_full(:), ba_full(:)   ! kept for future rebuild (moving bank)
   real(dp), allocatable :: acl_full(:), dxi_full(:) ! uncut values, restored when IBM geometry is rebuilt
   type(t_ibm_cut_cell), allocatable :: cut(:)
   type(t_ibm_cut_link), allocatable :: clink(:)
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
     the floor, otherwise raise an error. This catches divergence between the two geometry codes;
   - compute the per‑link metadata `clink` (wet centroids, `dxi_wet`, `acl_wet`, tangential
     offsets, small‑edge flags);
   - compute wet‑polygon Perot weights for cut cells (M3) and **overwrite** `wcx1/wcy1/wcx2/wcy2`
     for the cut‑cell side of those links only.

   Global `dx`, `dxi` and `acl` are **not** overwritten. They are also used by transport,
   diffusion, walls and the Perot weights of neighbouring uncut cells. IBM values are used only at
   the IBM hooks (§5.4). The bed levels (`bob` of cut links, `bl` of cut cells) are overwritten
   **before** `bob` is formed in `setbobs` (`flow_flowinit.f90:934`), because `hu`/`hs` must see the
   wet bed (M2, `BedlevType = 1`).
5. **Every time step**: wet‑polygon reconstruction is already in the weights (M3). Added per step:
   - the optional free‑slip projection safeguard (default off);
   - the IBM pressure gradient in `furu` (M1);
   - the small‑edge advection treatment (M4);
   - ghost fill (slice 2).

   Static geometry, no rebuild (rebuild design: §9).
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
| Polygon vertex / segment through a net node | Snap: if the distance from the node to the polygon is < `tol_snap = 1e‑8·dx`, treat the node as **solid**. This makes the edge fraction exactly 0 or 1. Same rule in IBM and in the cross‑check. **The rule must be applied in `cutcelwu` itself (F1):** a crossing at a link end point is not a crossing, and `kc` of snapped nodes is set before the cut polygons are built. A cell that only touches the polygon at a node is not a cut cell: no `kfs = 1` flag, no legacy normalised weights (F8). |
| Segment collinear with a cell face | Face fully open or fully closed after snapping; the cell is not cut (aligned control case tests this). |
| Very small active area (`area_frac < ImmersedBoundaryMinAreaFrac`, default 0.1) | Floored by the existing `Baorgfracmin` mechanism (IBM sets `Baorgfracmin = ImmersedBoundaryMinAreaFrac` if the user did not). Counted in `nfloored`; `area_deficit` reported. Classification uses its own geometric tolerance (`1e‑12` relative), **not** `ImmersedBoundaryCartesianTol` (F7): every cell that `cutcelwu` keeps active with `0 < area_frac < 1` is class 1. |
| Corner (polygon vertex inside a cell) / multiple crossings in one cell | Allowed if the active region is still one polygon. The normal is the area‑weighted mean of the sub‑segments. `nseg > 1` is flagged. If the active region splits into ≥2 parts (narrow solid spit): error in the POC. |
| Cell cut by more than one polygon, or polygons sharing an edge | **Not supported by `cutcelwu`** (F3): polygons are cut one at a time and the last cut wins; cells straddling a shared edge stay active inside the solid. Either clip all polygons of the `Cutcelllist` together (classify a node as solid if it is inside *any* polygon; build each cut polygon once from the union), or detect it and stop with an error naming the cell. Until then, inputs must use one polygon per connected solid, or overlapping polygons whose overlaps lie entirely inside the solid. |
| Narrow channel (< 2 fluid cells between banks) | IP stencil falls on the opposite bank. Use BI‑constrained interpolation. If still singular, fall back to the nearest fluid cell value and count it in `ndegenerate`. Warn when >0. |
| Insufficient stencil (IP outside local domain) | MPI ghost layers (4) cover IP distances ≤ ~1.5 dx. If the stencil is not local: error naming the cell (do not silently degrade). |
| Boundary near partition edge | Geometry is computed for owned and ghost cells. Ghost‑value fill uses only local data. `ucx/ucy` are already exchanged with `update_ghosts(ITYPE_SALL)`. The wet‑bed fit of a cut cell at the outer edge of the ghost layer may lack neighbours (F10): fit owned cells only and take ghost `bl` from `update_geom(2)`. |
| Polygon crossing an open boundary | Allowed: cut boundary links keep their cut `wu`. Cells adjacent to open boundaries are excluded from ghost stencils (nearest‑fluid fallback). Virtual boundary nodes (`ln(1, L)`, `L > lnxi`) are **not** solid cells (F2): give them the class and wet centroid of the interior cell `ln(2, L)` (mirrored across the boundary face), or skip M1/M2 on boundary links and keep `dxi`/`acl`. |

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
| Pressure gradient | Second‑order extrapolation of ζ from the cut‑cell centroid to the full‑cell centre (App. A); explicit version unstable in the coupled step, so partially implicit | Today: `g·dxi·(s1(k2)−s1(k1))` with `dxi` between **uncut** centres, while `s1` of a cut cell is the wet‑polygon mean ⇒ local gradient error up to O(1) (§5.3). M1 step 1: implicit `dxi_wet` (normal distance of wet centroids). M1 step 2: explicit tangential correction, with a stability study. | **Yes — M1 (step 1 in slice 1, step 2 in slice 2).** |
| Velocity at the active part of the edge | Correction term `v − v_cut` (App. B) | `u1` is the average normal velocity over the wet part of the face (`au·u1` *is* the flux), so App. B has no direct analogue. Its momentum equation must be evaluated at the wet‑face midpoint `xm`, which is what M1/M2 do. | Via M1/M2. |
| Bed/depth at the active edge | Interpolated to the active‑edge centre (Fig. 3c) | `BedlevType = 1` (decided, D7): `bl` at cell centres, `bob = max(bl(k1), bl(k2))`. Today `bl` of a cut cell is sampled at the uncut centre `xz`, which may lie inside the solid (bank top). M2: `bl` of cut cells evaluated at the wet centroid; `bob` then follows from `max(bl)` without node values. | **Yes — M2 (slice 1).** |
| Momentum advection | Ghost points at GPs for stencils; extrapolation on edges < 50 % wet (Fig. 3d) | `iadvec = 33` face‑flux form: zero flux through the segment (no face). The link control volume `volu` and the `acl` weighting use uncut `acl` ⇒ M2 uses `acl_wet`. Small cut edges ⇒ M4 extrapolates `adve` from parallel wet links. Ghost values are needed only for stencils that read solid‑cell `ucx/ucy` (A2). | **Yes — M2/M4 (M4 slice 2); ghost fill slice 2.** |
| Cell‑centre velocity (used by advection, friction `v`, viscosity, output) | Not applicable (u, v are separate points) | Today: ad‑hoc `wcxy`‑normalised weights in cut cells. M3: exact Perot over the wet polygon about the wet centroid, with the segment as a zero‑flux face (§5.3). An optional projection `u_c ← u_c − (u_c·n_b)n_b` is available as a switch (default off). | **Yes — M3 (slice 1).** |
| Tangential condition | Neumann on `u_t` via ghost (Eq. 19–20) | Free slip = no wall stress (current `irov = 0` behaviour for walls). For immersed segments nothing is added to `suu`. | No. |
| Horizontal viscosity | Includes GPs | FM viscosity acts through links; there is no link across the segment ⇒ zero stress = free slip. Smagorinsky and `vicouv` near banks use `ucx/ucy`, so they benefit from M3. | Indirect. |
| Wetting/drying | — | Unchanged (`epshu`, `hu`, `kfs`). A cut cell that dries behaves as an ordinary cell with smaller `ba`. Bank drying on a moving shoreline is out of scope. | No. |
| Boundary conditions | — | Open boundaries must lie on fully fluid links (checked). | Check only. |
| Linear system | Tridiagonal ADI lines; the paper iterates 6–10× | Unchanged global system. The matrix stays symmetric and diagonally dominant because `bbr = bb + a1/Δt`, with `a1 ≥ floor`. | No. |
| Time step | Courant up to ~10 | `setdtorg`: Δt ∝ `vol1/Σq_out`. With floor fraction φ the worst cut‑cell Δt ≈ φ·Δt_full. **Instrument** (§5.5); do not assume stability. | Diagnostic. |

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

### 5.3 Momentum treatment on cut cells (M1–M4) and momentum conservation [R]

**Why the change looks smaller than in the paper.** Much of the paper's momentum work comes from
Delft3D 4 specifics that FM does not share [V]:

- separate u‑ and v‑points that need ghost values of both, plus u↔v interpolation;
- ghost values inside the implicit Red‑Black Jacobi solve of the uncoupled ADI stage;
- the wide cyclic finite‑difference advection stencil.

FM has one normal velocity per face and collocated `ucx/ucy`. Its advection and viscosity are
explicit. Its default advection (`advec.f90:192‑203`) is a face‑flux sum, so a non‑existent face
carries no momentum. What **does** carry over is the paper's consistency work: positions,
distances and weights on and around cut cells (§3.2.1, §3.2.2.2, App. A, Figs. 3a, 3c, 3d). In FM
those items are M1–M4.

**Notation.** For a link `L` with neighbour cells `k1`, `k2`:

- `c1`, `c2` are the wet centroids (equal to `xz` for uncut cells);
- `n` = (`csu`, `snu`) and `t` is the tangent;
- `xm` is the midpoint of the wet part of the face;
- `d_n = (c2 − c1)·n` and `d_t = (c2 − c1)·t`.

For a cut cell, `s1` is the volume mean over the wet polygon, so to second order it is ζ at `c`.

**M1 — pressure gradient (dominant error).** For a linear ζ:

```
ζ2 − ζ1 = ∂ζ/∂n · d_n + ∂ζ/∂t · d_t      ⇒      ∂ζ/∂n|face = (ζ2 − ζ1)/d_n − ∂ζ/∂t · d_t/d_n
```

FM today uses `(ζ2 − ζ1)·dxi`, with `dxi = 1/|xz2 − xz1|`. Next to a cut cell the centroid moves
by up to ~dx/2, so the local relative error in `∂ζ/∂n` is O(1) whenever ∇ζ has a component along
the centroid shift. In the §4.1 uniform channel, ∇ζ is parallel to the bank and the shift is
mostly normal to it, so the error is small there. In bends, set‑up or any flow with a gradient
across the channel it is large. This is the paper's "HCG1 → HCG2" gap, and it directly drives
near‑bank velocity through the friction balance.

- **Step 1 (slice 1, implicit, keeps the matrix structure).**
  - In `furu` (`furu.f90:147`), for IBM links use `gdxi = agp/d_n` instead of `agp·dxi(L)`. It
    then enters both `cu` (implicit part, therefore `fu` and the `s1ini` matrix) and the explicit
    `(1−teta)` part consistently.
  - The matrix stays a 2‑point link stencil, symmetric, and diagonally dominant (`d_n > 0` always,
    because `c1` and `c2` lie on opposite sides of the face).
  - Guard: `d_n ≥ d_min = 0.25·dx`. Clipped links are counted and reported.
  - Global `dxi` is untouched. Transport diffusion keeps the uncut distance; this is a documented
    limitation.
- **Step 2 (slice 2, explicit, stability study).**
  - The gradient correction is `−(∂ζ/∂t)·d_t/d_n` on `∂ζ/∂n`. Because momentum carries
    `−g·∂ζ/∂n`, this means adding `+g·(∂ζ/∂t)·d_t/d_n` to `du` in `furu`. `∂ζ/∂t` at the face comes from least‑squares
    gradients of `s0` in `k1`, `k2` (wet neighbours only, Cartesian lattice stencil).
  - `furu` is called once per step attempt, **before** the wet/dry loop
    (`step_reduce_hydro.f90:112‑114`), so the correction is lagged by one time step.
  - Mitigations: limit `|correction| ≤ α·|g·(ζ2−ζ1)/d_n|` (α = 1, calibrated); apply it only on
    links with `edge_frac ≥ f_se`.
  - The paper reports that its explicit equivalent was unstable in the coupled step. If ours is
    unstable too, the fallback is the paper's partially implicit form. That needs matrix couplings
    outside `ccr(Lv2(L))`, i.e. a change to `s1ini` assembly and `pack_matrix`. This is the
    **Strategy 2 → 3 decision point** (Checkpoint E).
- An equivalent alternative is to extrapolate ζ from `c` to `xz`, `ζ* = ζ_c + ∇ζ·(xz − c)`
  (paper App. A). It has the same order but no implicit part. Step 1 is preferred because it
  keeps the dominant normal part implicit.

**M2 — cut‑link geometry.**

- *Control volume and interpolation weights.* `acl_wet = ((xm − c1)·n)/d_n`. It is used through
  an IBM hook in `advec` for `volu = acl_wet·vol1(k1) + (1−acl_wet)·vol1(k2)` and for the
  `acl·qu1 + (1−acl)·qu2` weighting (`advec.f90:411‑425`). Global `acl` is unchanged because the
  Perot weights of uncut neighbours and `walls(10/11)` use it.
- *Bed levels* (paper Fig. 3c), for **`BedlevType = 1` only** (decision D7). Type 1 is the bed
  level type required for morphodynamics, which is where this work is heading.
  - In type 1, `bl(k)` is a cell‑centre (tile) value interpolated at `xz`, and
    `bob(·,L) = max(bl(k1), bl(k2))`. No net‑node `zk` is involved, so the only problem is that
    `xz` of a cut cell can lie **inside the solid** and pick up bank‑top bathymetry.
  - M2 re‑evaluates `bl(k)` of cut cells at the wet centroid `c`. The step runs after the
    `bedlevel` initial field is applied and before `bob` is formed in `setbobs` (A10).
  - POC default: least‑squares linear extrapolation from the `bl` of uncut wet neighbours to `c`.
    It does not depend on the input format and also works for samples, initial fields and
    restart.
  - Option (later): re‑interpolate the `bedlevel` initial field at `c` instead of `xz`, which is
    more faithful for real bathymetry.
  - `bob` then follows from the existing `max(bl)` rule, so cut links need no extra code.
  - Without M2, a bank top inside the solid raises `bl`/`bob` of near‑bank cells and links, and
    biases `hu` and friction.
  - Morphodynamics (future): in type 1 the bed evolves at cell centres, including cut cells,
    through the sediment balance over the wet faces. M2 is applied at initialisation only and must
    not overwrite morphological updates. `setbobs` after `bathyupdate` keeps `bob = max(bl)`.
  - Other `BedlevType` values are rejected when IBM is on.

**M3 — wet‑polygon Perot reconstruction (free slip built in).** For a polygon Ω with wet area
`A_w`, centroid `c`, wet faces `f` (length `l_f`, midpoint `x_f`, outward normal velocity `u_n,f`)
and immersed segment `b`:

```
u_c = (1/A_w) · Σ_{f wet} (x_f − c) · u_n,f · l_f          [segment term (x_b − c)·u_n,b·l_b omitted]
```

For any constant `u`, `Σ_all faces (x_f − c)(u·n_f) l_f = A_w·u` exactly. The midpoint rule is
exact for a linear integrand. So dropping the segment is exact whenever `u·n_b = 0`: uniform
flow along a planar wall is reconstructed exactly, and the omitted term is the physically zero
wall flux.

In FM weight form, for the cut‑cell side of link `L`:

- `wcx1(L) = (x_f − c1)_x · l_f / A_w` and `wcy1(L) = (x_f − c1)_y · l_f / A_w` for `k1`;
- `wcx2/wcy2` use `−(x_f − c2)`, because the outward normal velocity of `k2` is `−u1`.

`A_w` is the **geometric** wet area, not the floored `ba`. The current `wcxy` normalisation
(`setlinktocenterweights.f90:141‑168`) is replaced for IBM cut cells.

- `Perot_weight_update == PEROT_UPDATE` recomputes the weights each step
  (`flow_run_single_timestep.f90:98‑101`) and would overwrite this. It is therefore rejected
  together with IBM, or IBM re‑applies its weights after that call — decide in W4.
- The projection safeguard `u_c ← u_c − (u_c·n_b)·n_b` stays as an option (default off), because
  `u_n` at the centroid of a curved wall is not exactly 0. The diagnostic `u_c·n_b` is always
  reported.
- `ucxu/ucyu` (advection) and Coriolis `ucxq/ucyq` inherit the change automatically. W0 verifies
  that `setucxucyucxuucyunew` uses `wcx1…` for all of them.

**M4 — small cut edges** (paper Fig. 3d, slice 2). For IBM links with
`edge_frac < f_se` (default 0.5, as in the paper):

- replace `adve(L)` after `advec` (in `advecdriver`, before `setextforcechkadvec`) by a one‑sided
  linear extrapolation from the two nearest parallel wet links in the same lattice row, away from
  the bank (index arithmetic on the Cartesian lattice);
- fall back to the nearest wet link;
- links with `edge_frac < 1e‑3` are closed (`wu = 0`); the area change is reported.

**Momentum conservation.**

- *Paper:* claims mass conservation only; its advection is the non‑conservative cyclic
  finite‑difference scheme.
- *FM:* `iadvec = 33` accumulates `uqc = Σ qa·ucxu` per cell with telescoping face fluxes (cell
  level), then interpolates to the link equation with `acl`/`volu`. FM is therefore not discretely
  momentum‑conserving on any mesh, with or without IBM.
- *Physics:* with a bank, total momentum changes by the bank's normal pressure force. Free slip
  means only that the bank exerts **no tangential** force.

POC requirements and how each is checked:

| Requirement | How it is met | Check |
|---|---|---|
| No momentum flux through the bank | Structural: the segment has no face, `qa = 0` | U‑tests on `uqc` with a closed segment; budget metric |
| No tangential wall stress (free slip) | Nothing added to `suu` for immersed segments | Code review; budget metric |
| No spurious force from geometry inconsistency | M1, M2, M3 | Wind set‑up basin (I9): zero‑velocity equilibrium; along‑channel momentum budget (I4) |
| Mass conservation | Unchanged continuity (`cutcelwu`) | Water balance (I1–I9) |

### 5.4 Solver call sequence (IBM on) [R]

```
flow_modelinit
  readMDUFile                         → ibm_read_config + config‑level validation (LEVEL_ERROR)
  flow_geominit(...)
    delete_dry_points_and_areas; cutcell_list(6,1)        (existing)
    cosphiunetcheck(1)
    ► ibm_check_cartesian(xk,yk,netcell,…) → reduce over ranks → error if not Cartesian
    getcellsurface …            ► ibm_save_full_area (ba_full)
    wu …                        ► ibm_save_full_width (wu_full)
    acl/dxi …                   ► ibm_save_full_link_metrics (acl_full, dxi_full)
    cutcell_list(4,2); weights; cutcell_list(5,3)          (existing; Baorgfracmin set by IBM)
    walls; setwallorientations; setlinktocenterweights      (existing)
    ► ibm_build_geometry   (final phase only): classes, fractions, segments, normals,
        wet centroids, clink metadata (dxi_wet, acl_wet, d_t), image points
      → ibm_crosscheck_cutcelwu
      → ibm_set_perot_weights (M3: overwrite wcx*/wcy* on cut‑cell sides)
      → ibm_report (dia)
  flow_flowinit
    bedlevel initial field (bl at xz) ► ibm_set_wet_bed (M2: bl of cut cells at wet centroid)
    setbobs (line 934)          (existing; BedlevType 1 ⇒ bob = max(bl), so M2 propagates)
    setucxucyucxuucyunew (line 951) ► ibm_apply_velocity_bc (projection option, ghost fill)
per step:
  flow_run_single_timestep: if Perot_weight_update == PEROT_UPDATE → rejected with IBM (or re‑apply M3)
  flow_initimestep
    setau; setumod
      setucxucyucxuucyunew (setumod.f90:115/126) ► ibm_apply_velocity_bc
    advecdriver
      [itstep==3 branch] setucxucyucxuucyunew (advecdriver.f90:82) ► ibm_apply_velocity_bc
      advec               ► (M2) IBM volu/acl_wet for clink links (advec.f90:411‑425)
                          ► (slice 2) reads ghost‑filled values
      ► (M4, slice 2) ibm_small_edge_advection(adve)  — before setextforcechkadvec
  step_reduce_hydro
    furu (line 112)       ► (M1 step 1) gdxi = agp/d_n on clink links
                          ► (M1 step 2, slice 2) explicit tangential correction in du
    volsur; s1ini; s1nod; solve_matrix …                   (unchanged)
  flow_finalize_single_timestep               ► ibm_update_diagnostics (when map/his output due)
```

`ibm_apply_velocity_bc` does the following:

- optional projection in cut cells (default off; M3 already makes the reconstruction wall
  consistent);
- slice 2: ghost fill;
- it operates on owned cells; `ucx/ucy` are then exchanged with `update_ghosts(ITYPE_SALL)` where
  the existing code already does this. W0 checks where that happens; if it does not, add a single
  exchange after the correction.

Every IBM hook is guarded by `if (ibm%active)` (and, in `furu`/`advec`, by a per‑link flag looked
up through `clink`). So `ImmersedBoundary = 0` executes no extra arithmetic and results stay
bit‑identical. The `furu` and `advec` hooks are inside hot loops: implement them as a
precomputed per‑link override array (`gdxi_ibm(L)`, 0 = not IBM) to avoid branching cost, and
measure the overhead with IBM off (target < 1 %).

Iteration and convergence: no new iterations. The FM nonlinear `s1` loop
(`step_reduce_hydro.f90:200‑322`) is unaffected. IBM changes static geometry (`ba`, `wu`, `bob`,
weights), the coefficient `gdxi` inside `fu` (still a 2‑point link term), and explicit terms.
Only the fallback for M1 step 2 (partially implicit) would change the matrix structure.

### 5.5 Configuration and diagnostics

**MDU keywords [R]** (read next to `Cutcelllist` in `unstruc_model.f90:646` and next to
`Baorgfracmin` at 1119; written next to 2796/3368 only when non‑default or `writeall`, so effective
MDU files of existing models do not change):

| Section | Key | Default | Meaning |
|---|---|---|---|
| `[numerics]` | `ImmersedBoundary` | `0` | 0 off; 1 = POC IBM (Cartesian 2D, free slip). |
| `[numerics]` | `ImmersedBoundaryMinAreaFrac` | `0.1` | Floor on cut‑cell area fraction (sets `Baorgfracmin` if that is 0). |
| `[numerics]` | `ImmersedBoundaryCartesianTol` | `1e‑6` | Relative tolerance of the Cartesian check. |
| `[numerics]` | `ImmersedBoundaryTangentialCorrection` | `0` (slice 1) | M1 step 2 on/off. The default becomes 1 only if the stability study passes. |
| `[numerics]` | `ImmersedBoundarySmallEdgeFrac` | `0.5` | M4 threshold `f_se` for small‑edge advection treatment. |
| `[numerics]` | `ImmersedBoundaryProjection` | `0` | Optional free‑slip projection of `u_c` in cut cells (M3 safeguard). |
| `[geometry]` | `Cutcelllist` (existing) | — | Bank polygon(s). Required when `ImmersedBoundary = 1`. |

Reusing `Cutcelllist` avoids a second polygon reader and a second cut‑cell pipeline in the POC.
A dedicated `ImmersedBoundaryFile` can be added later without breaking input. **Decision D1
(before coding)** confirms this choice. Each `.pol` file may hold several `dmiss`‑separated
polygons; the IBM crossing search must close and search each one separately (F13).

**Additional configuration rules (review 2026‑10‑31).**

- `BedlevType = 1` needs `[geometry] Conveyance2D ≤ 0` (FM default is 1, which `flow_geominit`
  rejects for `BedlevType < 3`). With IBM on, check this in `readMDUFile` and stop with
  `ImmersedBoundary=1 requires Conveyance2D = -1 or 0 with BedlevType=1` (F4). The POC cases use
  `Conveyance2D = -1` (R = `hu`), the closest to the analytical Chézy solutions.
- Keywords whose feature is not implemented yet are **rejected**, not silently ignored (F5):
  `ImmersedBoundaryTangentialCorrection = 1` until W7, `ImmersedBoundaryProjection = 1` until
  `ibm_apply_velocity_bc` exists (W4), and any non‑default `ImmersedBoundarySmallEdgeFrac` until
  W8 (or warn that it only flags links).
- `jsferic` is known only after the net file is read. Check `jsferic == 0` in `flow_geominit`
  next to the Cartesian check, not in `readMDUFile` (F6). `readMDUFile` keeps the checks on keys
  it owns (`kmx`, `FlowSolver`, `Nonlin2D`, `AdvecType`, `BedlevType`, `Conveyance2D`,
  `PerotWeightUpdate`, `PerotType`).

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
- `ImmersedBoundary=1 is not supported with kmx>0 / spherical coordinates / FlowSolver=<x> / Nonlin2D>0 / Perot weight update / BedlevType=<x> (only BedlevType=1).`
- `ImmersedBoundary=1: active region of cell <k> is split by the immersed boundary; refine the grid or simplify the polygon.`

Warnings: `irov /= 0` ("applies to mesh walls only; immersed boundary uses free slip");
`ndegenerate > 0`; `nfloored > 0` with `area_deficit`; `nclipped_dn > 0` (M1 links with
`d_n` clipped to `d_min`); `nclosed_small_edges > 0` (M4).

**Diagnostics in the `.dia` file**:

- the Cartesian result (`dx, dy, x0, y0, max_dev`);
- `ncut`, `nghost`, `nfloored`, `min_area_frac`, `area_deficit`, the cross‑check result and a
  timing line;
- number of IBM links, min/max of `d_n/dx` and `acl_wet`, `nclipped_dn`, `nsmall_edges`;
- when M1 step 2 is on: per map interval, max \|correction\| / \|main term\| and the number of
  limited links.

**Map output** (new `[output] Wrimap_immersed_boundary = 0|1`, pattern of
`map_write_settings%s1`, A8):

- `mesh2d_ibm_cell_class` (face, int);
- `mesh2d_ibm_area_fraction` (face);
- `mesh2d_ibm_edge_fraction` (edge);
- `mesh2d_ibm_normal_x/y` (face, fill value for non‑cut cells);
- `mesh2d_ibm_normal_velocity` (face, `u_c·n_b`, time‑dependent);
- `mesh2d_ibm_tangential_velocity` (face, time‑dependent);
- `mesh2d_ibm_wet_centroid_x/y` (face) and `mesh2d_ibm_dn_ratio` (edge, `d_n/dx`) for checking
  M1/M2;
- `mesh2d_ibm_tangential_correction` (edge, time‑dependent, M1 step 2 only).

These are the near‑bank quantities a future erosion model will consume.

**Performance instrumentation**: add `timstrt/timstop('IBM velocity BC', handle)`. Use the
existing `map_write_settings%dtcell` / `flowCourantNumber` (`setdtorg.f90:129‑135`) output to show
which cells limit Δt. Report the fraction of steps limited by cut cells in `ibm_report`.

### 5.6 Serial / MPI behaviour

- Geometry is built per rank on the local net (owned and ghost cells). It is deterministic, so
  owned and ghost copies agree (A3; verified by a test that compares `ba`/`edge_frac` after
  `update_ghosts`).
- The Cartesian check and diagnostics are reduced with the existing `reduce_*` helpers in
  `partition.F90`. Reduce the verdict **and** `dx`, `dy` (min/max) and `x0`, `y0` (min), so all
  ranks use the same lattice (F10; today only the verdict is reduced).
- The velocity BC is computed on owned cells. Ghost‑cell values come from the existing
  `ITYPE_SALL` exchange of `ucx/ucy`.
- Stencil radius: ≤ 2 cells; 4 cell‑based ghost layers (`partition.F90:447`) are sufficient.
- Partitioning (METIS or polygon) needs no change. A cut cell on a partition edge is valid.
- Memory: O(N_cut·~25 doubles + N_ghost·~20 doubles + 2·(ndx+lnx)) — negligible.
- CPU: geometry O(N_cut·N_polyseg_local) once, using the existing k‑d tree; per step O(N_cut).

---

## 6. File‑ and symbol‑level implementation work plan

**W0 — Spike (no merge)**

- Objective: close assumptions A1–A11.
- Files: read only, plus a scratch model built from `e02_dflowfm/f004_bottomfriction/c080_chezy_uniform`.
- Method:
  - run the oblique channel with `Cutcelllist` only;
  - dump `ndx`, `ba`, `wu`, `kfs` and stencil arrays;
  - instrument `sethigherorderadvectionvelocities`;
  - check `flow_geominit` phases;
  - test TestBench expected‑failure support;
  - search for a periodic boundary facility;
  - trace the uses of the `wcx1…` weights, `setbobs` call sites and `dxi` in the momentum path
    (A9–A11);
  - measure, on the I3 set‑up, the size of the M1 error: compare `(ζ2−ζ1)·dxi` with the analytic
    `∂ζ/∂n` on links next to cut cells, for the oblique channel and the wind set‑up basin.
    This quantifies the problem before any fix;
  - reproduce the existing `Cutcelllist` SIGSEGV for polygons with > 100 vertices
    (`cutcelwu.f90:98‑100`, `split_pol`) in a Debug build. Decide between fixing it (separate PR)
    and limiting polygon size (§7.2 "Test‑case creation", item 6).
- Completion: a short note answering A1–A11, with the M1 error measurement, attached to the PR.

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
- Status (2026‑10‑31): implemented, U23 passes. Still to do: `Conveyance2D` check (F4), reject
  unimplemented keywords (F5), move the `jsferic` check to W2 (F6), U24.

**W2 — Cartesian detection**

- Objective: `ibm_check_cartesian` (pure on arrays) and its wrapper using `network_data`.
- Files: `K/dflowfm_kernel/prepost/m_immersed_boundary.f90` (new). Call site:
  `K/dflowfm_kernel/prepost/flow_geominit.f90` after line 297.
- Depends on: W1.
- Algorithm: §5.5 criteria; MPI reduction via `partition.F90` helpers.
- Done when: unit tests U1–U8 pass, and an existing non‑IBM model shows unchanged results.
- Status (2026‑10‑31): implemented, U1–U8 pass. Still to do: `jsferic == 0` check here (F6);
  reduce `dx`, `dy`, `x0`, `y0` across ranks (F10).

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
- Status (2026‑10‑31): implemented with a design deviation — the geometry is measured from the
  `cutcelwu` polygons `nd(n)%x/y` instead of an independent recomputation, so the cross‑check
  cannot detect wrong cut polygons. The oblique‑channel cross‑check fails (F1). Completion moves
  to W3b.

**W4 — Wet‑polygon Perot reconstruction, M3 (slice 1)**

- Objective: exact Perot weights over the wet polygon for cut cells; optional projection
  safeguard.
- Files: `m_immersed_boundary.f90` (`ibm_set_perot_weights`, `ibm_apply_velocity_bc`). Call
  sites:
  - `K/dflowfm_kernel/prepost/flow_geominit.f90` after line 1314 (weights);
  - `K/dflowfm_kernel/compute/setumod.f90` after lines 115 and 126,
    `K/dflowfm_kernel/compute/advecdriver.f90` after 82, and
    `K/dflowfm_kernel/prepost/flow_flowinit.f90` after 951 (optional projection, ghost fill later).
- Config validation: reject `Perot_weight_update == PEROT_UPDATE`, or re‑apply the weights after
  `flow_run_single_timestep.f90:98‑101`.
- Depends on: W3.
- Algorithm: §5.3 M3. Weights `±(x_f − c)·l_f/A_w` on the cut‑cell side of each link; `A_w`
  geometric.
- Compatibility: only weights of IBM cut cells change; the guard keeps the default path
  bit‑identical.
- Done when: U17 and U27 pass, and the oblique IBM case reconstructs uniform flow in cut cells to
  ≤ 1e‑10 relative in a frozen‑velocity test (U27 at model level).
- Status (2026‑10‑31): weights implemented, U27 passes, `PerotWeightUpdate` rejected.
  `ibm_apply_velocity_bc` (projection) and U17 not implemented; the keyword must be rejected
  meanwhile (F5). Model‑level U27 blocked by W3b. Cells flagged cut by `cutcelwu` but classed
  fluid keep legacy weights (F8, fixed in W3b).

**W4b — Consistent pressure gradient and cut‑link geometry, M1 step 1 + M2 (slice 1)**

- Objective: `gdxi = agp/d_n` on IBM links; `acl_wet` in the `advec` control volume; wet bed
  levels.
- Files/symbols:
  - `K/dflowfm_kernel/compute/furu.f90:147` (`gdxi`), using a precomputed override array;
  - `K/dflowfm_kernel/compute/advec.f90:411‑425` (`volu`, `acl` weighting, `iadvL == 33` branch;
    other `iadvec` values are rejected with IBM in the POC);
  - `bl` of cut cells at the wet centroid (`BedlevType = 1`), hook between the `bedlevel`
    initial‑field interpolation and `setbobs` (`flow_flowinit.f90:934`); exact location from W0
    (A10).
- Depends on: W3.
- Algorithm: §5.3 M1 step 1 and M2. Clip `d_n ≥ 0.25·dx` (counted).
- Compatibility: matrix structure unchanged (2‑point, symmetric); IBM off ⇒ override array
  unused.
- Done when: U25, U26, U28 and U29 pass, and I9 (wind set‑up) meets its stage‑1 criterion.
- Status (2026‑10‑31): implemented; U25, U28 and U29 pass, U26 belongs to W7. The wet bed is
  applied in `flow_geominit` just before `setbobs` (after `setbedlevelfromextfile`), and again
  after a restart (F9: skip it when `bl` comes from a map/rst file, or apply only to cells whose
  `bl` was never fitted). The hooks use per‑link `allocated`/`size` checks inside the OpenMP loops
  instead of the planned precomputed override arrays (`gdxi_ibm(L)`, `acl_ibm(L)`); replace them
  and measure the IBM‑off overhead (F11). Open‑boundary links are blocked by F2.

**W3b — Geometry robustness of the integrated cut‑cell path (slice 1, added after review)**

- Objective: make `cutcelwu` + `ibm_build_geometry` correct for the geometries of §7.2, and test
  the integrated path, not only the pure routines.
- Files: `K/dflowfm_kernel/prepost/cutcelwu.f90`, `cutcell_list.f90`, `m_immersed_boundary.f90`,
  `flow_geominit.f90`; new integrated unit tests in `test/src/test_ibm_geometry.f90` (or a new
  `test_ibm_cutcell.f90`, added to `F90FILES` and `F2HFILES`).
- Depends on: W3, W4, W4b. Blocks W5 and W6.
- Tasks:
  1. **Node snapping in `cutcelwu` (F1, F8).** Before building cut polygons, set `kc = 1`
     (solid) for every node within `tol_snap = 1e‑8·dx` of the polygon. Count a link as crossed
     only if its two nodes have different `kc`; ignore crossings at a link end point. A cell
     without a crossed link and without a polygon vertex strictly inside is not cut (`kfs`
     unchanged). Fix the misleading "no active grid node" message and the unreachable `cycle`
     (F12).
  2. **Open‑boundary links (F2).** Treat virtual boundary nodes as the mirror of their interior
     cell (class and wet centroid reflected across the boundary face), or exclude boundary links
     from the M1/M2 overrides and keep `dxi`/`acl`. Choose the option that keeps the open
     boundary flux consistent; document it in §5.1.
  3. **Multiple polygons (F3, F13).** Preferred: clip against all polygons of the
     `Cutcelllist` together (`kc = 1` if inside any polygon; one cut polygon per cell from the
     union, e.g. using the existing `jaalltogether`/`jamasks` storage of intersections in
     `cutcell_list`). Minimum: detect a cell cut by more than one polygon, or an active cell whose
     wet polygon lies inside another polygon, and stop with an error naming the cell. Close and
     search each `dmiss`‑separated polygon separately.
  4. **Classification tolerance (F7).** Use a geometric tolerance (`1e‑12` relative) for
     class 0/1/2; every active cell with `0 < area_frac < 1` is class 1.
  5. **MPI (F10).** Fit the wet bed on owned cells only; take ghost `bl` from `update_geom(2)`;
     do not raise an error for a ghost cell with too few neighbours.
  6. **Independent cross‑check (W3 intent).** At least for the cut cells, recompute the wet area
     from the polygon (half‑plane clipping of the square) and compare it with `ba` before the
     floor. This would have caught F1 and F3.
  7. **`>100` vertex limit.** Fix the `split_pol` path of `cutcelwu` (§7.2 item 6), so that a
     curved bank can be one polygon. This removes the need for sector polygons (F3).
- Done when: U31–U36 pass; c05, c09, c14 and the c06 partitioned run initialise without IBM
  errors; for c05, c09 and c14 no active cell lies entirely inside the solid and every cut cell's
  area fraction matches the analytic wet fraction to ≤ 1e‑10 (checked by
  `verify_metrics.py --geometry`).

**W5 — Diagnostics and output**

- Objective: `.dia` report, map variables, timer.
- Files: `m_immersed_boundary.f90` (`ibm_report`); map definitions/writes in
  `K/dflowfm_io/unstruc_netcdf.f90` (pattern near 3993‑4140, `t_unc_mapids` in
  `unstruc_netcdf_data.f90`); `[output] Wrimap_immersed_boundary` in `unstruc_model.f90` near 2130.
- Depends on: W3, W3b, W4, W4b.
- Done when: the variables appear in the map with UGRID attributes, and the TestBench comparer
  reads them.
- Status (2026‑10‑31): not started. `ncut`, `nfloored`, `min_area_frac`, `area_deficit` and
  `ndegenerate` are computed in `ibm_build_geometry` but not reported; only `nclipped_dn` is
  warned. Also add the `irov /= 0` warning of §5.5.

**W6 — Verification cases (slice 1)**

- Objective: §7 cases I1–I5, I7 (annular), I8 (lake at rest) and I9 (wind set‑up), plus the
  analytical and momentum‑budget script.
- Files:
  - cases under `test/deltares_testbench/data/cases/e02_dflowfm/f600_ibm/…` (D5), created as
    specified in §7.2 "Test‑case creation" (generator, inputs, docs, DVC);
    tracked with DVC (`HOW_TO_DVC.md`);
  - `<testCase>` entries in a new include or in `configs/include/dimr_dflowfm_*.xml`, wired into
    `configs/dimr/dimr_dflowfm_lnx64.xml` and the win64 counterpart.
- Depends on: W4, W4b, W3b, W5.
- Done when: the acceptance criteria in §8, stage 1, are met.
- Status (2026‑10‑31): generator and inputs for c01–c33 exist (§7.2 item 10); DVC pointers are
  staged locally, not pushed or committed. The generator must be updated (§7.2 items 4–5:
  `Conveyance2D = -1`, no bank through net nodes, single or overlapping polygons) and the cases
  regenerated before `dvc push`. No case has run to completion with IBM on.

**W7 — Tangential pressure correction, M1 step 2: stability study (slice 2)**

- Objective: explicit `+g·(∂ζ/∂t)·d_t/d_n` term in `du` (§5.3) with a limiter; decide whether it is stable.
- Files: `K/dflowfm_kernel/compute/furu.f90` (`du`, same override mechanism); LSQ gradient of `s0`
  in `m_immersed_boundary.f90`.
- Depends on: W4b, W6.
- Study:
  - Δt sweep up to the CFL limit; φ ∈ {0.05, 0.1, 0.25}; `teta0` ∈ {0.55, 1.0};
  - cases I4, I7, I9;
  - record growth of `max|u|` in I9 and the steps to steady state.
- Done when: either (a) stable for all sweep points and I7/I9 errors improve per §8 stage 2, so
  the default becomes 1; or (b) unstable, documented, and the decision at Checkpoint E is made
  (partially implicit matrix extension vs Strategy 3).

**W8 — Small cut edges M4 and ghost fill (slice 2)**

- Objective:
  - M4 `adve` extrapolation on links with `edge_frac < f_se`; closing links with
    `edge_frac < 1e‑3`;
  - ghost list, IP stencils, BI‑constrained interpolation, reflection (Eqs. 19–20), storage per
    A1/A2.
- Files: `m_immersed_boundary.f90`; hook in `K/dflowfm_kernel/compute/advecdriver.f90` before
  `setextforcechkadvec`; ghost hook inside or after `sethigherorderadvectionvelocities` only if A2
  shows reads from solid cells.
- Depends on: W4, W0.
- Done when: U18–U22 and U30 pass, and the near‑bank error does not get worse than slice 1 (it
  should improve).

**W8b — Refinement study**

- Objective: §7 case I6 (all of I2/I3/I4/I7 at three resolutions).
- Depends on: W7, W8.

**W9 — Documentation**

- Objective: MDU keyword descriptions (`prop_set` strings), a user‑facing note on restrictions, and
  a test‑case `doc` folder (DVC).
- Depends on: W1–W6.
- Note: the external User Manual is not in this repository; raise a docs ticket.

**Later (not POC)**:

- W10: partially implicit pressure correction (only if W7 outcome (b) and Strategy 2 is
  retained);
- W11: immersed walls as pseudo‑`walls` for `irov = 1|2`;
- W12: 3D (`kmx > 0`);
- W13: moving bank (§9);
- W14: wet‑geometry `dxi` for transport diffusion (consistency with M1).

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

Unit tests for the momentum operators (§5.3) live in a new file `test/src/test_ibm_momentum.f90`
(suite `test_ibm_momentum`):

| ID | Test | Expected |
|---|---|---|
| U25 | M1 step 1: linear ζ sampled at wet centroids of cut pairs (all cut types from U9–U13). `(ζ2−ζ1)/d_n` vs analytic `∂ζ/∂n` | Exact (± 1e‑12) when ∇ζ ⟂ `t`; otherwise the error equals `∂ζ/∂t·d_t/d_n` (verifies the decomposition). For comparison, the uncut `dxi` error is reported (should be O(1) for some pairs). |
| U26 | M1 step 2: LSQ gradient on cut stencils exact for linear ζ; corrected `∂ζ/∂n` exact for any linear ζ | ± 1e‑12 |
| U27 | M3: wet‑polygon Perot with constant `u` tangential to the segment, for triangle, trapezoid and pentagon cuts and the 90° corner of U13 (two segments) | `u_c = u` ± 1e‑13. For `u` with a normal component, the result equals the analytic formula (documented). |
| U28 | `acl_wet ∈ (0,1)`; equals `acl` for links not adjacent to cut cells; `d_n > 0`; clipping counted | exact |
| U29 | Wet bed (M2, `BedlevType = 1`): with `bl` = +10 m (bank top) sampled at uncut centres `xz` inside the solid, `bl` of cut cells is re‑evaluated at the wet centroid from wet neighbours, and `bob = max(bl)` of cut links follows | exact for a planar wet bed (± 1e‑12) |
| U30 | M4 extrapolation along a lattice row reproduces a linear `adve` field | ± 1e‑13 |

These run in CI with the existing kernel gtest target, in well under 1 s total.

**Integrated cut‑cell tests (W3b, added after review).** U1–U30 call the pure routines of
`m_immersed_boundary` on hand‑made polygons. The failures F1–F3 occur in the integrated path
`cutcell_list` → `cutcelwu(4/5)` → `ibm_build_geometry`, which no test exercised. These tests set
up a small in‑memory net (`network_data`) and polygon(s), run that path, and compare `ba`, `wu`,
`nd` and the IBM state with analytic values:

| ID | Test | Expected |
|---|---|---|
| U31 | Oblique bank passing exactly through a net node (slope 43/80 through a lattice node, as in c05) | No missing faces; every wet face recovered; edge fractions ∈ [0, 1]; touched‑only cells not flagged cut (F1, F8) |
| U32 | Bank crossing an open boundary (cut cell next to a boundary link) | IBM build succeeds; the boundary link has a valid `dxi_wet`/`acl_wet` or keeps `dxi`/`acl` as decided in W3b (F2) |
| U33 | Two polygons sharing an edge across the fluid region, and two half‑planes meeting at a corner inside a cell | Union clipping: exact wet area, no active cell inside the solid; or, minimum fix, an error naming the cell (F3) |
| U34 | Sliver cut with `area_frac` < `ImmersedBoundaryCartesianTol` and a near‑full cut with `area_frac` > 1 − tol | Both class 1, IBM weights applied (F7) |
| U35 | `.pol` file with two `dmiss`‑separated polygons | Both polygons cut; crossings counted per polygon (F13) |
| U36 | Partitioned run (2 ranks) of a small oblique case: `ba`, `wu`, `edge_frac`, `bl` on ghost cells after `update_ghosts` | Identical to owner values; no wet‑bed error on outer ghost cells (F10) |

U36 is an MPI test and may live in TestBench (I5) instead of gtest. Configuration error tests
(U24) are extended with `Conveyance2D = 1`, `ImmersedBoundaryTangentialCorrection = 1`,
`ImmersedBoundaryProjection = 1` (F4, F5) and a spherical net in `flow_geominit` (F6).

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
| Bed | `BedlevType = 1`. z_b(x, y) = z_ref − S·s, with s = x cos β + y sin β, given as samples at all cell centres of the box via an `[Initial] quantity = bedlevel` block in `ExtForceFileNew` (triangulation, which is exact at coincident points; `IniFieldFile` is deprecated). The bed is smooth through the solid in I1–I6, so M2 is not exercised there. |
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
| I6 | I4, I7 and I9 at three resolutions (50 / 25 / 12.5 m; annulus 4 / 2 / 1 m), with their staircase and cut‑cell‑only variants | Resolution sensitivity / observed order | TestBench, **nightly** |
| I7 | **Annular channel** (§4.2), staircase / cut‑cell‑only / IBM variants [**slice 1**] | Curved banks, superelevation (gradient across the channel) ⇒ sensitive to M1/M3. Near‑bank velocity. | TestBench, CI (≈ 2–5k cells) |
| I8 | **Lake at rest**: oblique closed basin, bed sloping in x and y, bed samples = +10 m (bank top) at cell centres inside the solid, flat ζ, no forcing, IBM on | Well‑balancedness; M2 bed handling; reconstruction noise | TestBench, CI (< 30 s) |
| I9 | **Wind set‑up in a closed oblique basin**: basin rotated by β, flat bed, uniform wind along +x (oblique to every bank). Variants: staircase, cut‑cell only, IBM step 1, IBM step 1+2. | Exact equilibrium has **u = 0** with a planar ζ oblique to the banks, so any velocity is a spurious force from the pressure gradient (M1). | TestBench, CI (< 1 min) |
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

I9 parameters [R]:

- Basin 2000 × 1000 m rotated by β ≈ 28.26° (all four sides closed and oblique to the grid);
  dx = 25 m.
- Depth h = 2 m (flat bed); Chézy C = 65 (for damping only).
- Wind U10 = 10 m/s along +x, constant `Cdbreakpoints` 0.0012; `rhomean`, `rhoair` set
  explicitly.
- Expected slope `dζ/dx = ρ_a·C_d·U²/(ρ·g·h)` ≈ 7.3e‑6, i.e. ≈ 1.5 cm set‑up. The exact steady
  state has u = 0, because the curl of τ/h is zero.
- 12 h run; spin‑up seiches damped by friction.
- Metrics: `max|u|` and `mean|u|` at the end, and the plane‑fit residual of ζ.

**Test‑case creation (W6; first generation done 2026‑10‑31, needs regeneration — see item 10).**

1. **Location (D5).** Put the cases under
   `test/deltares_testbench/data/cases/e02_dflowfm/f600_ibm/`. Follow `HOW_TO_DVC.md`:
   - each case `cNN_<name>/` gets `input/` and `doc/`; add `reference_lnx64/` and
     `reference_win64/` once results pass §8;
   - run `dvc add` per folder, `dvc push`, then `git add -f` the `.dvc` files;
   - add a family‑level `f600_ibm/doc/` with `functionality_report.tex` and
     `chapters/testcases.tex` (pattern of `f040_subsidence/doc`);
   - per‑case `doc/` contains `casedoc.tex` and `chapters/case_text.tex` (pattern of
     `f004_bottomfriction/c080_chezy_uniform/doc`).
2. **Case list.**

   | Case | Plan ID | Solid applied as | IBM |
   |---|---|---|---|
   | c01_channel_aligned | I1 | `DryPointsFile` polygon (z = 1), banks on grid lines | off |
   | c02_channel_aligned_ibm | I1b | `Cutcelllist`, banks on grid lines | on |
   | c03_channel_oblique_staircase | I2 | `DryPointsFile` | off |
   | c04_channel_oblique_cutcell | I3 | `Cutcelllist`, `Baorgfracmin = 0.1` | off |
   | c05_channel_oblique_ibm | I4 | `Cutcelllist` | on |
   | c06_channel_oblique_ibm_parallel | I5 | as c05; `dimr.xml` `<process>0 1</process>` + `DFM_COMM_DFMWORLD`; TestBench partitions with `--partition:ndomains=2:icgsolver=6` (pattern `e02_f014_c001`) | on |
   | c07 / c08 / c09_annulus_{staircase, cutcell, ibm} | I7 | `DryPointsFile` / `Cutcelllist` / `Cutcelllist` | off / off / on |
   | c10_lake_at_rest_ibm | I8 | `Cutcelllist` | on |
   | c11_wind_setup_aligned | I9 floor | `DryPointsFile`, basin edges on grid lines | off |
   | c12 / c13 / c14_wind_setup_{staircase, cutcell, ibm} | I9 | as above | off / off / on |
   | c15_wind_setup_ibm_tangential | I9 | `Cutcelllist`, `ImmersedBoundaryTangentialCorrection = 1` (runnable only after W7; rejected until then, F5) | on |
   | I6 refinement variants | I6 | same generator, other `dx` | nightly |

3. **Generator.** Write a deterministic Python generator `f600_ibm/doc/generate_cases.py`
   (numpy + netCDF4, both in the TestBench venv) that produces all inputs and case documents from
   the parameters in §7.2. It writes:
   - a UGRID‑1.0 net (`mesh2d_*`, 1‑based connectivity, counter‑clockwise faces, as in
     `f011_wind/c030` `windcase_net.nc`);
   - solid polygons (`.pol`, *inside = solid*), the `Cutcelllist` `.lst`, boundary `.pli` and
     `.bc` (constant time series per support point);
   - the new‑format `.ext`, bed and initial water‑level samples (`.xyz`) at all cell centres,
     `.xyn`/crs `.pli`, a compact `.mdu` and `dimr.xml`.

   Add a `README.md` mapping the plan IDs to the cases. Inputs are never edited by hand:
   regenerate, then `dvc commit`.
4. **Input conventions, checked against the current kernel.**
   - `[geometry] IniFieldFile` is **deprecated** (`fm_deprecated_keywords.f90:32`). Put the
     `bedlevel` and `initialWaterLevel` `[Initial]` blocks (`dataFileType = sample`,
     `interpolationMethod = triangulation`, `locationType = 2d`) in the `[external forcing]
     ExtForceFileNew` file. The old `waterlevel` quantity name is deprecated
     (`unstruc_inifields.f90:1025‑1028`).
   - `BedlevType = 1`; bed samples at **all** cell centres of the box, including the solid
     (smooth bed for I1–I7, I9; +10 m bank top inside the solid for I8).
   - `[geometry] Conveyance2D = -1` in **every** case. The FM default (1) is rejected with
     `BedlevType = 1` (F4: c02 stopped with "bed-level type and conveyance type do not match").
   - Uniform wind: `[Meteo] quantity = windx/windy`, `forcingFileType = uniform` (pattern
     `f011_wind/c030_comp_uniform_standard`); `[wind] ICdtyp = 1`, `Cdbreakpoints = 0.0012`,
     `Rhoair = 1.2`.
   - The IBM cases use the keywords of §5.5, which exist only after W1:
     - `ImmersedBoundary`, `ImmersedBoundaryMinAreaFrac`, `ImmersedBoundaryTangentialCorrection`;
     - `[output] Wrimap_immersed_boundary`.
5. **Geometry choices.**
   - *Open boundaries:* the boundary polylines span the whole domain edge, including the solid
     part. This replaces the §5.1 rule "polygon crossing an open boundary ⇒ error": cut boundary
     links are allowed, because continuity uses the cut `wu`. Cells next to open boundaries are
     excluded from ghost stencils.
   - *Oblique channel:* offset the centreline so that neither the bank/boundary intersections
     **nor any interior point of a bank** coincides with a net node. The first generation used
     y₀ = 310 m: the boundary intersections were clear, but the banks y = 60 + (43/80)·x and
     y = 560 + (43/80)·x pass exactly through 2, 4 and 6 nodes at dx = 50, 25 and 12.5 m, which
     triggers F1. **Use y₀ = 311 m** (banks at 61 and 561 m): verified node‑free at all three
     resolutions. The generator must assert that no lattice node lies within 1e‑6·dx of any
     non‑axis‑aligned solid edge. The exact‑node degeneracy stays covered by I1b, U11/U12 and
     U31.
   - *Annulus:* open the ring north of the centre by removing net cells, giving a discharge inflow
     on the east face and the analytical ζ(r) outflow on the west face, so no periodic boundary is
     needed (A6). The removed strip must be **wider than 2 cells**. A pre‑implementation trial with
     a 2‑cell strip (4 m at dx = 2 m) gave `Boundary link … already claimed` and **0** open
     outflow cells: the inflow polyline claimed the outflow links. Use ≥ 4 cells and verify
     `nr of open bndcells` for both boundaries in the `.dia` file.
     **The first generation deviated:** it closed the gap with two solid "gap" polygons and built
     the banks from 8 inner and 8 outer sector polygons (18 polygons) to stay below the 100‑vertex
     limit. The polygons share edges, which `cutcelwu` does not support (F3): the c08 scratch run
     had 97 active cells entirely inside the solid. Their straight radial edges also lie on the
     45°/135° diagonals through the lattice origin, so they pass through net nodes (F1). Until
     W3b tasks 3 or 7 are done, the annulus cases are **not valid**. After W3b: one polygon per
     bank (inner disk, outer region via union of overlapping pieces or one polygon once the vertex
     limit is fixed), and the gap made by removing net cells as specified above.
   - *Basin:* solid = four half‑plane quadrilaterals outside the rotated rectangle (no polygons
     with holes). The half‑planes overlap, which is safe away from the fluid, but each corner cell
     of the rectangle is cut by two polygons and keeps only the last cut (F3; 6–8 cells per case).
     Valid once W3b task 3 (union clipping, or detection with an error) is done. Moving the
     corners onto lattice nodes is not practical at tan β = 43/80: all four corners land on
     nodes only if both side lengths are multiples of 25·√(80² + 43²) ≈ 2270 m at dx = 25 m.
6. **Known limitation of the existing cut‑cell code (must be handled in W0/W3).** In a
   pre‑implementation trial with the current Release build, `Cutcelllist` crashed
   (SIGSEGV, `forrtl 174`) for a polygon with **more than 100 vertices**. That is the branch that
   calls `split_pol(2, 2, 100, 100)` in `cutcelwu.f90:98‑100`. A 90‑vertex disk ran. Options:
   - fix the `split_pol` path (an existing bug; separate small PR with a unit test); or
   - limit the generator polygons to ≤ 100 vertices per polygon and split curved banks into
     several polygons (the annulus outer bank as 8 sectors already does this; the inner disk needs
     ≥ 2 pieces).

   Decide in W0. The IBM geometry module itself must not have a vertex limit.
   **Review 2026‑10‑31:** splitting curved banks into several polygons is **not** a safe
   work‑around, because `cutcelwu` cuts one polygon at a time (F3). Fixing the `split_pol` path is
   now W3b task 7 and the preferred option.
7. **Pre‑implementation smoke‑run results** (non‑IBM variants only; Release build; for planning,
   not as references):

   | Case | 2D cells | Mean Δt | Wall time |
   |---|---|---|---|
   | c01 aligned (staircase) | 2160 | 11.7 s | 3.5 s |
   | c03 oblique staircase | 2400 | 7.4 s | 5.1 s |
   | c04 oblique cut cells (`Baorgfracmin = 0.1`) | 2582 | **1.0 s** | **32 s** |
   | c07 annulus staircase | 1904 | 7.6 s | 1.9 s |

   The existing cut cells reduce the mean Δt by ≈ 7× in the oblique channel. This confirms the
   small‑cell CFL risk (§9): report the IBM Δt against c03/c04, and calibrate D3 (φ) in W6. All
   cases are fast enough for CI.
8. **Metrics script.** Write `f600_ibm/doc/verify_metrics.py` for the §7.2 metrics. It needs only
   netCDF4 and numpy. Add a `--geometry` mode that reads the map file (`mesh2d_flowelem_ba`, face
   bounds) and the case metadata and reports active cells lying entirely in the solid and cut
   cells whose area fraction differs from the analytic wet fraction (the check used for F3).
9. **TestBench wiring.** Once references exist, add the `<testCase>` entries in a new include,
   wired into `configs/dimr/dimr_dflowfm_lnx64.xml` (and win64), plus c06 in the parallel
   config.
10. **First generation and smoke runs (2026‑10‑31, Debug build).**
    - `f600_ibm/doc/generate_cases.py` writes c01–c15 (I1–I9) and c16–c33 (I6 at dx/2 and 2·dx
      for the oblique channel, the annulus and the basin), with input and doc folders and the
      family doc. Structural checks pass (UGRID connectivity and orientation, sample counts,
      ≤ 100 vertices per polygon, MPI `dimr.xml`). DVC pointers are staged locally, not pushed.
    - c05 (I4) stops in IBM initialisation: "wet link face could not be recovered" (F1). After F1,
      F2 is expected on the open‑boundary links.
    - c02 (I1b) passes the IBM geometry build but stops on `Conveyance2D` (F4).
    - c08 (I7 cut cells, IBM off; scratch copy with `Conveyance2D = -1`, 3 min) runs, but has 97
      active cells entirely inside the solid (F3).
    - Before `dvc push`: regenerate with items 4–5 applied, rerun c01–c15, then follow item 1.

The `<path version="DVC">` convention is used as in `dimr_dflowfm_parallel.xml:489`.

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
| Bank normal velocity (reconstructed) | `max \|u_c·n_b\| / 1.25` over cut cells (I3 vs I4; ~1e‑14 only if the projection option is on) |
| Momentum budget (I4) | Over the evaluation region `R` between cross‑sections a and b: `F_drive = g·Σ_R A·h·(ζ̄_a − ζ̄_b)/L_s` (section‑averaged ζ, boundary form, avoids local gradients); `F_fric = g·Σ_R A·\|u\|·u_s/C²`; `ΔM` = momentum‑flux difference across a and b (from section discharge and velocities, ≈ 0 in uniform flow). Residual `R_mom = \|F_drive − F_fric − ΔM\| / F_drive` = spurious bank force fraction. |
| Pressure‑gradient consistency (I4, I7, I9) | On links next to cut cells: `E_grad = \|Δs1·gdxi_used/g − ∂ζ/∂n_analytic\| / \|∇ζ\|`, using the wet‑centroid and `d_n` map fields |
| Spurious velocity (I8, I9) | `max \|u_c\|`, `mean \|u_c\|` over the basin at the final time |
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

1. Run I1 and record FM's floor (`E_h^I1`, `E_t^I1`, mass error, `R_mom^I1`). For I9, the floor
   is the aligned (β = 0) basin with banks on grid lines, which must give `max|u|` at solver
   tolerance.
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
- IBM‑off runtime overhead of the `furu`/`advec` hooks < 1 % on a standard 2D case.

**Stage 1 — slice 1 (first merge: W1–W6, including M1 step 1, M2, M3)**

| Metric | Target |
|---|---|
| Cut geometry (W3b) | For every IBM and cut‑cell case: no active cell entirely inside the solid; every cut cell's geometric area fraction equals the analytic wet fraction to ≤ 1e‑10; all wet faces recovered (`verify_metrics.py --geometry`). |
| Runs to steady state | I4 completes without Δt collapse. Steady‑state drift < 1e‑7 m/s. Mean Δt(I4) ≥ 0.25 × mean Δt(I1) with φ = 0.1 (report if lower). |
| Mass | Relative water‑balance error of I4 ≤ max(1e‑9, 10 × I1). |
| Reconstruction (M3) | Frozen uniform tangential flow reconstructed in cut cells to ≤ 1e‑10 relative (model‑level U27). max \|u_c·n_b\| / 1.25 in I4 ≤ 0.1 × the I3 value (projection off). |
| Momentum budget | R_mom(I4) ≤ max(1 %, 3 × R_mom(I1)) and ≤ 0.5 × R_mom(I3). |
| Pressure‑gradient consistency | E_grad on links next to cut cells, in I9 and I7: ≤ 0.3 × the I3 value (M1 step 1 removes the normal part; the tangential remainder is accepted until stage 2). |
| Wind set‑up (I9) | max \|u\|(IBM step 1) ≤ 0.3 × max \|u\|(I3) and ≤ 0.3 × max \|u\|(staircase); ζ plane‑fit residual ≤ 5 % of the total set‑up. |
| Lake at rest (I8) | max \|u\| ≤ 1e‑8 m/s after 6 h; no cells dry because of bank‑top bed samples. |
| Annular (I7) | Superelevation ζ(r_outer) − ζ(r_inner) within 30 % of Eqs. 23–24 and closer than staircase and I3. E_vθ near both banks ≤ E_vθ(I3). |
| Depth uniformity | CV_h(I4) ≤ 0.5 × CV_h(I2) **and** ≤ 1 %. |
| Depth error | E_h,L2(I4) ≤ 1 %, E_h,∞(I4) ≤ 3 %. |
| Centreline | E_u,cl,L2(I4) ≤ 2 %. |
| Near‑bank tangential | E_t(I4) ≤ 0.5 × E_t(I2) and ≤ E_t(I3). Absolute target ≤ 5 %. |
| Direction | mean error ≤ 1°. |
| MPI | I5 vs I4: ≤ 1e‑6 m and ≤ 1e‑6 m/s (solver‑tolerance level); geometry fields identical. |
| Non‑Cartesian | U3, U4, U6 and the config error tests pass; message names the criterion. |

**Stage 2 — slice 2 (W7, W8, W8b)**

- M1 step 2 (if stable per W7): I9 max \|u\| ≤ 0.1 × I3; E_grad ≤ 0.1 × I3. If not stable, the
  stage‑2 targets are evaluated with step 1 only and Checkpoint E decides.
- E_t(I4) does not get worse versus stage 1.
- I6: observed order p ≥ 0.8 for E_h and E_t (first‑order‑consistent), with the error decreasing
  monotonically.
- I7: E_h,L2 and E_vθ,L2 ≤ the staircase and I3 errors at the same resolution, with ζ(r)
  superelevation sign and magnitude within 20 % of Eqs. 23–24 in the evaluation sector.
- The I6 observed order is reported for I4, I7 and I9. Lmax near banks must not drop below
  first order. The paper's HCG1 shows this drop; it is the signature of an uncorrected pressure
  gradient.

**Stage 3 — later**: partially implicit pressure correction or Strategy 3, aiming at p ≈ 2 in
the I7 L2 and Lmax norms as in paper Table I. This stage claims it only if measured.

**Ways to fail.**

- If I9 or I7 do not improve on I3 after M1 step 1, the remaining tangential term dominates. Then
  W7 (step 2) becomes the priority, ahead of the ghost fill.
- If W7 is unstable, go to Checkpoint E (§10).
- If I4 does not beat I3 but I9 does, check M2 (bed) and M3 (reconstruction) with the I8 and U27
  diagnostics before adding ghost machinery.

---

## 9. Risks, unresolved questions, fallbacks

**Decisions required before coding**

| ID | Decision | Recommendation |
|---|---|---|
| D1 | Reuse `Cutcelllist` as the IBM polygon input vs a new `ImmersedBoundaryFile` | Reuse for the POC (no second geometry pipeline). Add the dedicated key when moving banks need an IBM‑owned cut. |
| D2 | Interpretation of "β = tan(43/80)" | tan β = 43/80 (β ≈ 28.26°). |
| D3 | Area floor φ default | 0.1, and calibrate in W6 (paper reports stable cells down to 1e‑5 with ADI; FM's explicit CFL cannot afford that). |
| D4 | Which Cartesian grids to accept | Axis‑aligned uniform (dx ≠ dy allowed). Reject rotated and non‑uniform for now. |
| D5 | Location of new test cases | **Decided:** `test/deltares_testbench/data/cases/e02_dflowfm/f600_ibm`. Still open: which config XML includes them (add once references exist). |
| D6 | Scope of M1/M2 overrides: IBM‑local (`gdxi`/`acl_wet` only in momentum hooks) vs overwriting global `dxi`/`acl` | IBM‑local for the POC (no side effects on transport, walls or the weights of uncut cells). Revisit for W14. |
| D7 | Which `iadvec` and `BedlevType` values to support with IBM | **Decided:** `BedlevType = 1` (cell‑centre bed, required for morphodynamics) and `iadvec = 33`; reject others. |
| D8 | Acceptance of a partially implicit pressure correction that changes the `s1` matrix structure (if W7 fails) | Not part of the POC. Escalate at Checkpoint E with the W7 evidence. |
| D9 | M1/M2 on open-boundary links next to cut cells (F2) | **Chosen for the POC:** keep existing `dxi`/`acl` on boundary links; do not mirror virtual-cell centroids. Implemented; add/retain focused regression coverage. |
| D10 | Several `Cutcelllist` polygons cutting one cell (F3) | **Chosen for the current POC:** detect and stop with an explicit error. Union clipping is deferred. c14 reaches this guard; c09/annulus geometry remains unsupported, so this decision does not satisfy the original C3 case-pass criterion. |

**Can be deferred**: dedicated IBM polygon file; rotated grids (global rotation θ fitted from link
directions — the lattice test generalises with a rotation); non‑uniform rectilinear grids (bilinear
stencil generalises); immersed partial/no slip via pseudo‑walls; 3D; cell merging.

**Risks**

| Risk | Impact | Mitigation / fallback |
|---|---|---|
| Small cut cells limit Δt (explicit advection CFL `setdtorg`) | Slow runs or instability | φ floor (existing `Baorgfracmin`); report % limited steps. Fallback: cell merging (paper‑like area‑weighted coupling of a small cell with its largest neighbour); or exclude IBM cut cells from the CFL check if `ja_timestep_*`‑style options allow — needs a stability study first. |
| Floor alters the effective planform area | Small mass‑consistent bias (area error) | Report `area_deficit`; refinement study I6. |
| `cutcelwu` behaviour differs from the IBM geometry (snap rules, `BAMIN`, `ba` vs `numlimdt_baorg`) | Cross‑check failures | Align snap tolerance. If impossible, IBM overwrites `ba`/`wu` from its own geometry (one assignment loop) and the cross‑check becomes informational. |
| Pressure‑gradient inconsistency at cut cells (`s1` at wet centroid, `dxi` from uncut centres) | **Dominant** near‑bank error: spurious force along the bank. Hidden by the §4.1 test. | M1 step 1 in slice 1 (implicit, structure‑preserving). M1 step 2 explicit with a stability study (W7). Exposed by I7, I9 and `E_grad`. Fallback: partially implicit (D8) or Strategy 3. |
| M1 step 2 unstable (as the paper found for explicit corrections) | Tangential remainder of the gradient error stays | Limiter, Δt/θ sweep; if still unstable, Checkpoint E decision. The step‑1‑only result remains valid and is reported. |
| `d_n` very small for sliver cut pairs | Stiff link, large `fu` | Clip `d_n ≥ 0.25·dx`, plus the area floor; counted in diagnostics. |
| Wet‑polygon Perot weights interact with `Perot_weight_update` or other reconstruction options (`reconst2nd`, `Perot_type`) | IBM weights silently overwritten | Reject incompatible options with IBM (validated in W1); A9 confirms which arrays use the weights. |
| Global `dxi`/`acl` kept uncut for transport and diffusion | Scalar diffusion near banks is inconsistent with the momentum change | Documented limitation (D6); W14 later. |
| Cut‑cell bed sampled at the uncut centre `xz` inside the solid (`BedlevType = 1`) | Wrong `bl`/`bob` and `hu` near banks (bank top) | M2 in slice 1 (re‑evaluate at the wet centroid); I8 tests it with a +10 m bank top. |
| Existing `Cutcelllist` crashes for polygons with > 100 vertices (pre‑implementation trial; `split_pol` path in `cutcelwu.f90:98‑100`) | Curved or real banks with many vertices abort at initialisation | W0: reproduce in Debug, then fix (separate PR + unit test) or split polygons to ≤ 100 vertices. IBM geometry itself has no vertex limit. |
| A1/A2 wrong (solid cells are present / stencils read them) | Ghost storage design changes | W0 spike before W7; the module API hides the storage. |
| MPI geometry mismatch (A3) | Inconsistent `ba` across ranks | Test I5 plus an assertion in `ibm_crosscheck` comparing `ba` after `update_ghosts`. |
| Restart | IBM changes `ndx` (solid cells removed) — the same as current `Cutcelllist` | A restart is valid only with the same mesh and polygon. Detect `ndx`/`lnx` mismatch (existing restart reader behaviour) and document it. No new restart fields. The current implementation re‑applies the wet‑bed fit after reading the restart (F9); harmless for hydrodynamics, but it would overwrite morphological bed changes. Skip it when `bl` comes from the restart file. |
| IBM geometry trusts the `cutcelwu` polygons (W3 deviation) | Wrong cut polygons pass the cross‑check silently (F1, F3) | W3b task 6: independent recomputation of cut‑cell wet areas; integrated tests U31–U35; `verify_metrics.py --geometry` gate in stage 1. |
| Solid polygons that touch or share edges, or pass through net nodes (generated and real banks) | Missing faces, cells inside the solid left active, or initialisation errors | W3b tasks 1 and 3; the generator asserts node‑free banks and one polygon per solid until D10 is implemented. |
| Unimplemented keywords accepted | A case looks like it tests a feature that does nothing (c15) | Reject them until implemented (F5). |
| Unit tests cover only pure routines | Integrated failures reach the model runs first | Integrated tests U31–U36; smoke‑run c05, c09, c14 and c06 in every W3b/W4 review. |
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
4. **W4 + W4b** (M3 reconstruction; M1 step 1 + M2) with U25–U29 → **Checkpoint C2**:
   - review of the momentum hooks;
   - W0's M1 error measurement compared before and after, on the I3/I9 set‑ups;
   - confirmation that IBM off is bit‑identical and the hook overhead is < 1 %.

   *Status 2026‑10‑31:* code in place; Checkpoint C2 not passed — the review found F1–F14
   (§1.1), and no IBM case runs yet.
4b. **W3b** (integrated cut‑cell robustness: node snapping, open‑boundary links, multiple
   polygons, classification tolerance, MPI wet bed, independent area check, vertex limit) with
   U31–U36, plus the W1/W2 fixes F4–F6 and the generator fixes of §7.2 items 4–5 →
   **Checkpoint C3**:
   - c05, c09, c14 and c06 initialise with IBM on and pass `verify_metrics.py --geometry`;
     c06 now passes the two-rank 0.05-hour smoke run, but the verifier is not yet present and
     c09/c14 currently stop at the selected D10 guard;
   - D9 and D10 decided;
   - cases regenerated; only then `dvc push` and commit the DVC pointers.
5. **W5 + W6** (diagnostics, cases I1–I5, I7, I8, I9) → **Checkpoint D = first merge
   (smallest useful vertical slice)**:
   - opt‑in IBM on Cartesian meshes;
   - cut‑cell continuity (existing);
   - consistent normal pressure gradient, wet bed levels and wall‑consistent reconstruction;
   - diagnostics and map fields;
   - unit tests plus 9 TestBench cases (with variants);
   - stage 0 and stage 1 criteria met.
6. **W7** (M1 step 2 stability study) → **Checkpoint E: strategy decision**:
   - (a) stable ⇒ continue Strategy 2;
   - (b) unstable ⇒ choose between D8 (matrix extension, W10) and Strategy 3 (dedicated
     Cartesian solver via `select case (flow_solver)` in `flow_run_single_timestep.f90:70`),
     based on the measured I7/I9 errors with step 1 only.
7. **W8 + W8b** (M4, ghost fill, refinement) → **Checkpoint F**: stage 2.
8. **W9** documentation, alongside each merge.
9. Later: W10–W14, each with its own plan and review.

---

## 11. Commands and evidence needed to validate the completed work

Commands come from `.github/skills/*/SKILL.md` and `doc/unit-testing.md`. On Linux the build
directory is `build_<config>_<build-type>`, per the build‑delft3d skill.

```bash
# Format changed Fortran (repo root; uses .fprettify.rc)
tools/fprettify/run_fprettify.sh src/engines_gpl/dflowfm/packages/dflowfm_kernel

# Build FM (Debug for development; Release/RelWithDebInfo for TestBench timing)
python build.py --config dflowfm --build-type Debug --build --keep-build
# If the fm-suite build is used and stops in the unrelated preC-SUMO test target (F14),
# build only the D-Flow FM executables:
cmake --build build_fm-suite_debug --target dflowfm-cli dflowfm_dll dimr --parallel

# Direct smoke run of one case without TestBench (copy the input; never run in input/)
cp -a <case>/input <case>/input_work && cd <case>/input_work
OMP_NUM_THREADS=1 <build>/dflowfm_cli_exe/dflowfm --autostartstop <case>.mdu

# Unit tests (f90tw/gtest names are <suite>.<test>)
ctest --test-dir build_dflowfm_debug --tests-regex 'test_ibm|test_mdu_file_read_write|test_unstruc_model' --output-on-failure
ctest --test-dir build_dflowfm_debug --output-on-failure        # full unit suite, stage 0

# TestBench (from test/deltares_testbench, inside its venv)
cd test/deltares_testbench
python TestBench.py --compare --config configs/dimr/dimr_dflowfm_lnx64.xml \
    --filter testcase=<I1,I1b,I2,I3,I4,I7,I8,I9 names (with variants)>
python TestBench.py --compare --config configs/dimr/dimr_dflowfm_lnx64_parallel.xml \
    --filter testcase=<I5 name>
# Stage 0 regression: run the full dflowfm configs (or TeamCity), compare against main.

# Analytical acceptance (per case, after a run)
python data/cases/e02_dflowfm/f600_ibm/doc/verify_metrics.py <case> <work_dir>/dflowfmoutput
```

**Evidence to attach to the first‑merge PR**:

- Resolution of every finding F1–F14 of §1.1 (fix, test ID, or accepted limitation).
- `verify_metrics.py --geometry` output for all IBM and cut‑cell cases (stage 1 geometry row).
- W0 assumptions note (A1–A11), including the measured M1 error before the fix.
- Unit test log (all `test_ibm_*` passing).
- `.dia` excerpts for I4: Cartesian check result, `ncut`, `nghost`, `nfloored`, `min_area_frac`,
  `area_deficit`, cross‑check OK, IBM timer.
- Metric table for I1/I2/I3/I4, I7, I8 and I9 (§7.2) against the §8 stage 1 targets, plus
  calibration values from I1 and the aligned I9 basin. The table includes `R_mom`, `E_grad` and
  the I9 spurious velocities.
- Plots: plan view of h and |u| for I2, I3 and I4 (like paper Fig. 4); centreline h and |u| (like
  Fig. 5); near‑bank tangential velocity profile; I9 velocity magnitude maps for staircase / I3 /
  IBM (spurious‑current pattern along the banks); I7 ζ(r) and v_θ(r) against Eqs. 23–24.
- Δt statistics (I4 vs I1/I2) and % steps limited by cut cells.
- MPI comparison I5 vs I4.
- Proof of no regression with IBM off (TestBench run or TeamCity build link).
- Rejection messages for U3/U4/U6 and the config errors.
