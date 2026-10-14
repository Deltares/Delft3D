# Testcase e38_f01_c08_mod2_FM / "examples\dflowmf\12_dflowfm_pre_C_SUMO"
Group "e38_f02_precice" tests the connection of D-Flow FM to C-SUMO via preCICE. The user starts D-Flow FM, preC-SUMO (and C-SUMO).

## Purpose
Testcase "c01_minimal", copied to "examples\dflowmf\12_dflowfm_pre_C_SUMO" is based on "e38_f01_c08_mod2_FM" and is intended as the most simple connection. Thin dams around intake, source and sink locations make changes inside visible. The DIMR connection with C-SUMO is activated at t=120, 240, and 360 minutes, using a 7200-second coupling interval over the six-hour MDU time window. C-SUMO itself is not launched/used: instead, artificial prepared NF2FF files are provided in the expected folder. The responses at 240 and 360 minutes repeat the fixed response at 120 minutes.

## Model description
Simple rectangular basin. Five constructed, artificial NF2FF files:
\begin{enumerate}
1. NF2FF__FlowFM_SubMod001_120.000_i0si2so2.xml   
    No intakes, 2 sinks, 2 sources. No Desa spreading because more than 1 source: both entrainment and discharge is spread over the 2 sources, dummy intake.
2. NF2FF__FlowFM_SubMod001_120.000_i0si2so1.xml   
    No intakes, 2 sinks, 1 source. Desa spreading width is 15 m, resulting in all entrainments and discharges being lumped in one cell, dummy intake.
3. NF2FF__FlowFM_SubMod001_120.000_i1si2so2.xml   
    1 intake, 2 sinks, 2 sources. No Desa spreading because more than 1 source: both entrainment and discharge is spread over the 2 sources, real intake.
4. NF2FF__FlowFM_SubMod001_120.000_i10si2so1.xml   
    10 intakes, 2 sinks, 1 source. Desa spreading width is 15 m, resulting in all entrainments and discharges being lumped in one cell, 10 vertically spread intakes.
5. NF2FF__FlowFM_SubMod001_120.000_i1si42so1.xml   
    1 intake, 42 sinks, 1 source. Desa spreading width is 150 m, resulting in all entrainments and discharges being lumped in 4 cells, 1 intake.

The resulting sources and sinks on the D-Flow FM side are added as comments at the end of each NF2FF file.

By enabling line "ExtForceFileNew = sources.ext" in the mdu file, the nearfield sources and sinks are mimicked using traditional sources and sinks.
