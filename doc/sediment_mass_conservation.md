# Sediment mass conservation and bounded bedload transport

This change extracts the sediment corrections from `3dabsgen` commits
`19649bfc58` and `bb8bd94841` onto `origin/main`. It includes sediment
transport, bed exchange, morphology timestep restrictions and their
regression tests. The conservative bed update and face-flux divergence
retain their existing formulation.

## Settling and bed exchange use the same reference cell

For morphology sediment, `solve_vertical` now applies the same settling
cutoff as `comp_fluxver`: intercell settling is zero below the reference
cell selected by `kmxsed`. Exchange through the lower reference-cell face
is already represented by the bed-exchange source and sink. One effective
face settling velocity is used by both implicit settling and semi-implicit
advection. Passive tracers retain their own full-depth settling velocity.
The local velocity is OpenMP-private.

`fm_erosed` passes `ws(max(kmxsed - 1, kb))` to `soursin_3d`, rather than
the value at the upper reference-cell face. Settling and diffusion then
refer to the same lower interface. The bottom-reference fallback keeps
the existing bottom-cell velocity.

## Deposition follows the actual local transport interval

`comp_sinktot` now receives `nsubsteps`, `jaupdate` and `ndeltasteps` from
`update_constituents`. It accumulates deposition only for owned cells
updated by the transport solve, using the final concentration and the
same local interval, `dt_loc = dts * ndeltasteps(k)`. Without subcycling,
the ordinary `dts` applies. Bed and fluff deposition use the same rule;
the 3D active-cell selection matches the vertical solve.

For a unit-volume cell with initial concentration 1 and sink coefficient
1, an implicit update over two time units gives concentration 1/3. The
deposited mass must be 2/3. Previously, a skipped substep followed by that
update recorded `1 + 1/3 = 4/3`, counting stale concentration on the
skipped substep. The coupled regression checks that remaining suspended
mass plus deposited mass is exactly the initial mass within tolerance.

## Suspended flux units and interpolation

The Rouse-profile concentration integral in
`fm_suspended_sand_correction` already has units `kg/m^2`. Its retained
transport is therefore

```text
Q = velocity * integral(C dz) * face_width   [kg/s]
```

The extra multiplication by layer thickness is removed. The interpolation
weights `ac1/ac2` are real values, rather than integers that truncate
fractional weights. Reversing a geometric edge now preserves the physical
correction and changes only its oriented sign.

The same declaration correction in `fm_mor_maxtimestep` lets both adjacent
transport vectors contribute to the outgoing-flux estimate. This repairs
the existing bed-change timestep restriction; it does not change the
conservative bed update.

## Bounded wave/current bedload partition

In `bedtr1993`, the old partition divided the finite bedload potential
`qbt` by the magnitude of the actual combined direction vector:

```text
D = sqrt(1 + r^2 + 2*r*cos(phi))
qbc = qbt / D
qbw = r * qbc
```

For opposing components, `phi = pi` and `D = abs(1-r)`. Both component
magnitudes become unbounded as their ratio `r` approaches one. Independent
current and wave transport multipliers can then retain an enormous
component even when the other is disabled. In the traced T02 case with
`BedW=0`, this produced a roughly 3.35 m artificial bed excursion in one
0.08333 s morphology step. A smaller timestep cannot remove the closure
singularity.

The correction follows the perpendicular component partition in
[Van Rijn (2003), equations 5-7](https://www.leovanrijn-sediment.com/papers/P3-2003a.pdf),
then applies the actual current and wave directions:

```text
qbc = qbt / sqrt(1 + r^2)
qbw = r * qbc
qbc^2 + qbw^2 = qbt^2
```

Each component is bounded by `qbt`, and equal opposing components cancel
continuously. With independent multipliers `B=Bed` and `W=BedW`, the
resultant is bounded by `qbt*sqrt(B^2 + W^2)`. The existing no-wave and
extreme-ratio branches remain in use. At `r=1`, aligned components now
produce `sqrt(2)*qbt`: the correction intentionally changes transport
magnitudes away from perpendicular calibration and still needs physical
validation for the intended application.

This bedload closure belongs to sediment transport. No changes to wave
propagation, wave-current interaction or absorbing/generating boundaries
are included in this extraction.

## Regression coverage and validation limits

`test_sed_transport.f90` contains eleven production-routine tests for the
analytic concentration integral, fractional interpolation, edge reversal,
donor availability, morphology timestep bounds, opposing bedload near
`r=1`, angle changes, still-water limits and existing ratio cutoffs.
`test_sed_exchange.f90` adds eight tests for settling cutoffs, passive
tracers, unequal-volume conservation, semi-implicit transport, bottom
reference cells, local deposition intervals and coupled sink closure.
Both files are registered in the kernel's Fortran and f90tw source lists.

With a configured build, run:

```text
cmake --build build_fm-suite --config Release --target test_dflowfm_kernel_gtest test_bedload_balance test_morphology
ctest --test-dir build_fm-suite -C Release --output-on-failure -R "sediment_transport|sediment_exchange|mass_balance|test_morphology"
```

Extraction validation on 2026-11-02 passed all 228 registered kernel tests,
including the 19 sediment regressions, and all four shared morphology
tests. All seven changed production files and all branch kernel test
sources were freshly compiled with Intel Fortran 2024.2.1 into private
archives and a private test executable, reusing the existing configured
dependency objects. The shared morphology tests were relinked against
the freshly compiled bedload routine. The repository Fortran style check
and whitespace checks also passed. This was not a clean rebuild of all
dependencies or a new coupled T02 run.

Historical paired T02 runs on `3dabsgen`, with the same boundary correction
in both runs, reduced the post-spinup closed sediment-budget residual from
`+393.593539 kg` to `-0.050009 kg` at 21700 s. Those results establish the
benefit of the settling/deposition corrections in that configuration;
they are not an end-to-end validation of this extracted main-based branch.
The budget included actual boundary advection, suspended correction,
bedload and avalanche export, with inventories sampled after geometry
updates. The fixed-bed spinup introduced about `1502.54 kg`, so the
comparison started at 120 s. Conservation from initialization and the
remaining small post-spinup loss were not resolved. Dense suspension and
the morphodynamic water-volume budget also require separate assessment.

## Mass balance output with fixed composition and moving beds

Sediment balances include an explicit `Fixed bed exchange` term while bed
composition is held fixed, either before `CmpUpdStt` (which defaults to
`MorStt`) or with `CmpUpd = 0`. It records erosion minus actual accumulated
deposition from the transport solve, using the same morphology factor as
the suspended storage and boundary terms. Frozen fluff-layer exchange is
included before composition updates start. Potential bedload is excluded
from inventory transfers while composition is fixed.

Composition and bed-level updates are independent. Composition can change
bed inventory with `BedUpd = 0`; moving geometry can change water volume
while composition is fixed. The water balance includes a `Moving bed`
term measured from actual cell volumes before and after the geometry
update, including all vertical layers. This accounts for displaced water
without changing water levels, concentrations or sediment inventories.
Both diagnostics exclude non-owned cells, reduce contributions in MPI,
reset interval accumulators and retain whole-period totals.

Bedload balance fluxes are accumulated after `fm_bott3d`, which applies
availability and slope corrections, combines transport components and
applies morphology boundary conditions before updating bed composition.
Accumulating earlier reads the previous timestep's `e_sbn`, causing
interval errors and an incorrect spin-up transition. This ordering follows
composition updates independently of whether bed-level updating is enabled.

The separate `mass_balance_integration.bedload_call_order` regression runs
the full production timestep sequence on a generated three-cell channel
with two bedload fractions and a layered bed. It checks every output
interval and the whole-period budget across spin-up and composition
activation, and requires nonzero boundary transport and inventory change.

Bedload boundary names use `MbaLumpBoundaries`, matching the values. They
are independent of `MbaLumpFromToMba`. Unlumped external terms append a
new output entry, preserving the preceding term and its label.

`test_mass_balance.f90` exercises spin-up and activation, independent
composition and bed-level changes, frozen fluff exchange, 3D volumes,
ownership filtering, accumulator resets and boundary-label combinations.

Serial trench validation with `MorStt = 5`, `MorFac = 180` and a 1.2 s
balance interval reduced the whole-period sediment residual from
566.2093425 kg to 0.0185128 kg and the water residual from
-0.17944205 m^3 to -4.4e-14 m^3. Bed inventory, bed level, water level
and velocity outputs were identical; the largest suspended-concentration
difference was below 5.3e-39 kg/m^3. The nine added regressions and all
242 registered kernel tests passed with directly compiled Intel Fortran
units linked against the configured dependency libraries. This validation
was serial; MPI ownership filtering was exercised in the unit tests.

The bedload call-order regression failed against the old production library
and passed after moving the diagnostic call. All 242 existing kernel tests
also passed. A serial rerun of `c10_fixed_layer` reduced the whole-period
sediment residuals from -1.03374e-4 kg and -2.01535e-6 kg to -1.30e-13 kg
and 1.86e-21 kg, respectively. All time-dependent map values were identical.
The changed production routine and new regression were compiled directly
with Intel Fortran 2024.2.1 against the configured Release libraries because
the Visual Studio build driver reported invalid license data.
