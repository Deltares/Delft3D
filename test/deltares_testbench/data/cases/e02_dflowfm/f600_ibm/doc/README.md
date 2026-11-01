# D-Flow FM immersed-boundary verification cases

Inputs and case documentation are generated deterministically by `doc/generate_cases.py` from the parameters in §7.2 of the implementation plan. The generator creates c01–c15 for I1–I9 and c16–c33 for the I6 resolution study.

Regenerate with the TestBench virtual environment from this directory:

```bash
python generate_cases.py --force
```

With `--force`, the generator also removes obsolete numbered `solid_*.pol` files left by earlier polygon layouts;
unrelated files are preserved.

The generated inputs use `bedlevel` and `initialWaterLevel` sample blocks in the new-format `.ext` file. Cut-cell polygons are split into files with no more than 100 vertices each. DVC pointers cover each case's `input/` and `doc/` folders.

Reference outputs and TestBench config entries are intentionally not generated: they are added only after solver runs pass the acceptance criteria. `Wrimap_immersed_boundary` is likewise deferred until the W5 map diagnostics exist.

| Case | Plan ID | Grid | Variant | Purpose |
|---|---|---:|---|---|
| `c01_channel_aligned` | I1 | 25 m | drypoints | Aligned channel control |
| `c02_channel_aligned_ibm` | I1b | 25 m | ibm | Aligned channel with IBM enabled |
| `c03_channel_oblique_staircase` | I2 | 25 m | drypoints | Oblique channel with staircase banks |
| `c04_channel_oblique_cutcell` | I3 | 25 m | cutcell | Oblique channel with cut cells only |
| `c05_channel_oblique_ibm` | I4 | 25 m | ibm | Oblique channel with IBM |
| `c06_channel_oblique_ibm_parallel` | I5 | 25 m | ibm | Oblique channel with IBM on two MPI ranks |
| `c07_annulus_staircase` | I7 | 2 m | drypoints | Annular channel with staircase banks |
| `c08_annulus_cutcell` | I7 | 2 m | cutcell | Annular channel with cut cells only |
| `c09_annulus_ibm` | I7 | 2 m | ibm | Annular channel with IBM |
| `c10_lake_at_rest_ibm` | I8 | 25 m | ibm | Oblique lake at rest with IBM |
| `c11_wind_setup_aligned` | I9 | 25 m | drypoints | Aligned wind-setup floor |
| `c12_wind_setup_staircase` | I9 | 25 m | drypoints | Oblique wind setup with staircase banks |
| `c13_wind_setup_cutcell` | I9 | 25 m | cutcell | Oblique wind setup with cut cells only |
| `c14_wind_setup_ibm` | I9 | 25 m | ibm | Oblique wind setup with IBM |
| `c15_wind_setup_ibm_tangential` | I9 | 25 m | ibm | Oblique wind setup with tangential correction |
| `c16_channel_oblique_staircase_dx50p0` | I6 | 50 m | drypoints | Channel staircase refinement at 50 m |
| `c17_channel_oblique_cutcell_dx50p0` | I6 | 50 m | cutcell | Channel cutcell refinement at 50 m |
| `c18_channel_oblique_ibm_dx50p0` | I6 | 50 m | ibm | Channel ibm refinement at 50 m |
| `c19_channel_oblique_staircase_dx12p5` | I6 | 12.5 m | drypoints | Channel staircase refinement at 12.5 m |
| `c20_channel_oblique_cutcell_dx12p5` | I6 | 12.5 m | cutcell | Channel cutcell refinement at 12.5 m |
| `c21_channel_oblique_ibm_dx12p5` | I6 | 12.5 m | ibm | Channel ibm refinement at 12.5 m |
| `c22_annulus_staircase_dx4p0` | I6 | 4 m | drypoints | Annulus staircase refinement at 4 m |
| `c23_annulus_cutcell_dx4p0` | I6 | 4 m | cutcell | Annulus cutcell refinement at 4 m |
| `c24_annulus_ibm_dx4p0` | I6 | 4 m | ibm | Annulus ibm refinement at 4 m |
| `c25_annulus_staircase_dx1p0` | I6 | 1 m | drypoints | Annulus staircase refinement at 1 m |
| `c26_annulus_cutcell_dx1p0` | I6 | 1 m | cutcell | Annulus cutcell refinement at 1 m |
| `c27_annulus_ibm_dx1p0` | I6 | 1 m | ibm | Annulus ibm refinement at 1 m |
| `c28_wind_setup_staircase_dx50p0` | I6 | 50 m | drypoints | Wind staircase refinement at 50 m |
| `c29_wind_setup_cutcell_dx50p0` | I6 | 50 m | cutcell | Wind cutcell refinement at 50 m |
| `c30_wind_setup_ibm_dx50p0` | I6 | 50 m | ibm | Wind ibm refinement at 50 m |
| `c31_wind_setup_staircase_dx12p5` | I6 | 12.5 m | drypoints | Wind staircase refinement at 12.5 m |
| `c32_wind_setup_cutcell_dx12p5` | I6 | 12.5 m | cutcell | Wind cutcell refinement at 12.5 m |
| `c33_wind_setup_ibm_dx12p5` | I6 | 12.5 m | ibm | Wind ibm refinement at 12.5 m |
