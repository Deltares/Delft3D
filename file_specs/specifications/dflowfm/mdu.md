**File description**: The master definition file of D-Flow FM

**File version**: 2.0.0

## Table of Contents
- [Supported keywords and options](#supported-keywords-and-options)
  - [[general]](#supported-keywords-and-options-general)
  - [[model] ⚠️](#supported-keywords-and-options-model)
  - [[geometry]](#supported-keywords-and-options-geometry)
  - [[volumeTables]](#supported-keywords-and-options-volumetables)
  - [[numerics]](#supported-keywords-and-options-numerics)
  - [[physics]](#supported-keywords-and-options-physics)
  - [[sediment]](#supported-keywords-and-options-sediment)
  - [[sedtrails]](#supported-keywords-and-options-sedtrails)
  - [[wind]](#supported-keywords-and-options-wind)
  - [[waves]](#supported-keywords-and-options-waves)
  - [[grw]](#supported-keywords-and-options-grw)
  - [[hydrology]](#supported-keywords-and-options-hydrology)
  - [[time]](#supported-keywords-and-options-time)
  - [[restart]](#supported-keywords-and-options-restart)
  - [[external forcing]](#supported-keywords-and-options-external-forcing)
  - [[trachytopes]](#supported-keywords-and-options-trachytopes)
  - [[output]](#supported-keywords-and-options-output)
  - [[calibration]](#supported-keywords-and-options-calibration)
  - [[processes]](#supported-keywords-and-options-processes)
  - [[particles]](#supported-keywords-and-options-particles)
  - [[veg]](#supported-keywords-and-options-veg)
  - [[ice]](#supported-keywords-and-options-ice)
- [Research keywords and options](#research-keywords-and-options)
- [Deprecated keywords and options](#deprecated-keywords-and-options)
- [Obsolete keywords and options](#obsolete-keywords-and-options)

<a id="supported-keywords-and-options"></a>

# Supported keywords and options

**Legend**

- 🔬: Indicates keywords or options that are still experimental.

- ⚠️: Indicates keywords or options that are deprecated and will be removed in future release versions.


<a id="supported-keywords-and-options-general"></a>
## [general]

This section contains the program name and its version.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `program` | Program. | `D-Flow FM` |  |  |  |
| `version` | Version number of computational kernel. | `1.2.60.64623M` |  |  |  |
| `fileType` | File type. Do not edit this. | `modelDef` | <ul><li>**`modelDef`**: model definition file</li></ul> |  |  |
| `fileVersion` | File version. Do not edit this. | `1.09` |  |  |  |
| `guiVersion` | Version number of GUI. | `<empty>` |  |  |  |
| `autoStart` ⚠️ | Autostart simulation after loading MDU or not. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: autostart</li><li>**`2`**: autostartstop</li></ul> |  |  |
| `pathsRelativeToParent` | Resolve file names (e.g. inside the \*.ext file) relative to their direct parent, instead of to the top-level MDU working dir. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `modelSpecific` 🔬 | Optional 'model specific ID', to enable certain custom runtime function calls (instead of via MDU name). | `<empty>` |  |  |  |
| `inputSpecific` 🔬 | Use of hardcoded specific inputs, shall not be used by users. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `convertLongCulverts` ⚠️ | Convert long culvert input to 1D2D long culverts. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |


<a id="supported-keywords-and-options-model"></a>
## [model] ⚠️

This section contains the program name and its version.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `program` | Program. | `D-Flow FM` |  |  |  |
| `version` | Version number of computational kernel. | `1.2.60.64623M` |  |  |  |
| `guiVersion` | Version number of GUI. | `<empty>` |  |  |  |
| `autoStart` | Autostart simulation after loading MDU or not. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: autostart</li><li>**`2`**: autostartstop</li></ul> |  |  |
| `pathsRelativeToParent` | Resolve file names (e.g. inside the \*.ext file) relative to their direct parent, instead of to the top-level MDU working dir. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `modelSpecific` | Optional 'model specific ID', to enable certain custom runtime function calls (instead of via MDU name). | `<empty>` |  |  |  |
| `inputSpecific` | Use of hardcoded specific inputs, shall not be used by users. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `convertLongCulverts` | Convert long culvert input to 1D2D long culverts. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |


<a id="supported-keywords-and-options-geometry"></a>
## [geometry]

In this section, the main entry comprises the specification of the grid (i.e. the netCDF network file). In addition, thin dams and thin dykes can be specified.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `netFile` | Net file (\*_net.nc) containing mesh information. | `<empty>` |  |  |  |
| `dryPointsFile` | Dry points file (\*.xyz), third column dummy z values, or polygon file (\*.pol). | `<empty>` |  |  |  |
| `gridEnclosureFile` | Enclosure file (\*.pol) to clip outer parts from the grid. | `<empty>` |  |  |  |
| `structureFile` | File (\*.ini) containing list of hydraulic structures. Supports multiple filenames separated by spaces. Filenames containing spaces must be placed inside double quotes. | `<empty>` |  |  |  |
| `gulliesFile` | Polyline file (\*_gul.pliz), containing lowest bed level along talweg x, y, z level. | `<empty>` |  |  |  |
| `roofsFile` | Polyline file (\*_roof.pliz), containing roofgutter heights x, y, z level. | `<empty>` |  |  |  |
| `iniFieldFile` ⚠️ | Initial and parameter field file (\*.ini). | `<empty>` |  |  |  |
| `waterLevIniFile` | Initial water levels sample file (\*.xyz). | `<empty>` |  |  |  |
| `landBoundaryFile` | Land boundary file (\*.ldb), only used for plotting. | `<empty>` |  |  |  |
| `thinDamFile` | Polyline file (\*_thd.pli), containing polyline(s) for tracing thin dams. | `<empty>` |  |  |  |
| `fixedWeirFile` | Polyline file (\*_fxw.pliz), containing polyline(s) with x, y, z where z = fixed weir top levels (formerly fixed weir). | `<empty>` |  |  |  |
| `pillarFile` | Polyline file (\*_pillar.pliz), containing four columns with x, y, diameter and Cd coefficient for bridge pillars. | `<empty>` |  |  |  |
| `useCaching` | Use caching for geometrical/network-related items. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `vertPlizFile` 🔬 | Polyline file (\*_vlay.pliz), containing x, y, Z where first Z = nr of layers, second Z = laytyp. For `layerType` = 3 (mixed layering in polygon regions). | `<empty>` |  |  |  |
| `frictFile` | Files with roughness data for 1D (space separated). | `<empty>` |  |  |  |
| `crossDefFile` | File containing the cross section definitions for all cross section shapes. | `<empty>` |  |  |  |
| `crossLocFile` | File containing the location definitions of the cross sections on a 1D network. | `<empty>` |  |  |  |
| `storageNodeFile` | File containing the specification of storage nodes and/or manholes to add extra storage to 1D models. | `<empty>` |  |  |  |
| `1D2DLinkFile` | File containing the custom parameterization of 1D-2D links. | `<empty>` |  |  |  |
| `allowBndAtBifurcation` | Allow 1D boundary node when connecting branch leads to bifurcation. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `profLocFile` | (\*_proflocation.xyz) x, y, z, z = profile refnumber. | `<empty>` |  |  |  |
| `profDefFile` | (\*_profdefinition.def) definition for all profile nrs. | `<empty>` |  |  |  |
| `profDefXyzFile` | (\*_profdefinition.def) definition for all profile nrs. | `<empty>` |  |  |  |
| `partitionFile` | (\*_part.pol), polyline(s) x, y. | `<empty>` |  |  |  |
| `dxWuiMin2D` | Smallest fraction dx/wu , set dx > `dxWuiMin2D`\*wu. | `0.0` |  |  |  |
| `waterLevIni` | Initial water level. | `0.0` |  | [m AD] |  |
| `bedLevUni` | Uniform bed level (only if `bedLevType`>=3), used at missing z-values in `netFile`. | `-5.0` |  | [m AD] |  |
| `bedSlope` | Bed slope inclination, sets zk = `bedLevUni` + x\*`bedSlope` and sets zbndz = xbndz\*`bedSlope`. | `0.0` |  | [-] |  |
| `bedLevType` | Bed level definition type. | `3` | <ul><li>**`1`**: at cell center (tiles xz, yz, bl, bob=max(bl))</li><li>**`2`**: at face (tiles xu, yu, blu, bob=blu)</li><li>**`3`**: at face (using mean node values)</li><li>**`4`**: at face (using min node values)</li><li>**`5`**: at face (using max node values)</li><li>**`6`**: with bl based on node values</li></ul> |  |  |
| `blMeanBelow` | If not -999.0, below this level the cell centre bedlevel is the mean of surrounding netnodes. | `-999.0` |  | [m] |  |
| `blMinAbove` | If not -999.0, above this level the cell centre bedlevel is the min of surrounding netnodes. | `-999.0` |  | [m] |  |
| `angLat` | Angle of latitude S-N, 0=on Equator (and thus no Coriolis force). Only required for Coriolis on Cartesian grids and for heat flux modelling. | `0.0` |  | [deg] |  |
| `angLon` | Angle of longitude E-W, 0=Greenwich Mean Time. Only required for heat flux modelling. | `0.0` |  | [deg] |  |
| `conveyance2D` | 2D analytic conveyance description. | `-1` | <ul><li>**`-1`**: R=HU</li><li>**`0`**: R=H</li><li>**`1`**: R=A/P</li><li>**`2`**: K=analytic-1D conv</li><li>**`3`**: K=analytic-2D conv</li></ul> |  |  |
| `nonLin1D` | Non-linear 1D volumes, applicable for models with closed cross sections. | `0` | <ul><li>**`0`**: none</li><li>**`1`**: treat closed sections as partially open by using a Preissmann slot</li><li>**`2`**: Nested Newton approach</li><li>**`3`**: partial Nested Newton approach</li></ul> |  |  |
| `nonLin2D` | Non-linear 2D volumes, only i.c.m. `bedLevType`=3 and `conveyance2D`>=1. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `slotw1D` | Minimum slotwidth 1D. | `0.001` |  | [m] |  |
| `slotw2D` | Minimum slotwidth 2D. | `0.001` |  | [m] |  |
| `uniformWidth1D` | Uniform width for 1D profiles and 1D2D internal links. | `2.0` |  | [m] |  |
| `uniformHeight1D` | Uniform height for 1D profiles and 1D2D internal links. | `3.0` |  | [m] |  |
| `uniformWidth1DStreetInlets` | Uniform width for street inlets. | `0.2` |  | [m] |  |
| `uniformHeight1DStreetInlets` | Uniform height for street inlets. | `0.1` |  | [m] |  |
| `uniformTyp1DStreetInlets` | Uniform cross section type for street inlets. | `-2` | <ul><li>**`1`**: circle</li><li>**`2`**: rectangle</li><li>**`-2`**: closed rectangle</li></ul> |  |  |
| `uniformWidth1DRoofGutterPipes` | Uniform width for roof gutter pipes. | `0.1` |  | [m] |  |
| `uniformHeight1DRoofGutterPipes` | Uniform height for roof gutter pipes. | `0.1` |  | [m] |  |
| `uniformTyp1DRoofGutterPipes` | Uniform cross section type for type roof gutter pipes. | `-2` | <ul><li>**`1`**: circle</li><li>**`2`**: rectangle</li><li>**`-2`**: closed rectangle</li></ul> | [m] |  |
| `sillHeightMin` | Fixed weir only active if both ground heights are larger than this value. | `0.0` |  | [m AD] |  |
| `makeOrthoCenters` | Switch from circumcentres to orthocenters in geominit. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `dCenterInside` | Limit cell center; 1.0:in cell <-> 0.0:on c/g. | `1.0` | `[0.0, 1.0]` | [-] |  |
| `circumcenterMethod` ⚠️ | Circumcenter computation method. | `allNetlinksLoop` | <ul><li>**`internalNetlinksEdge`**: iterate each edge</li><li>**`internalNetlinksLoop`**: iterate each loop</li><li>**`allNetlinksLoop`**: iterate each edge and each loop</li></ul> | [-] |  |
| `circumcenterTolerance` ⚠️ | Tolerance for convergence of circumcenter method. | `1e-15` |  | [m] |  |
| `baMin` | Minimum grid cell area, i.c.m. cutcells. | `1e-06` |  | [m²] |  |
| `openBoundaryTolerance` | Search tolerance factor between boundary polyline and grid cells. | `3.0` |  | [cell size] |  |
| `renumberFlowNodes` | Renumber the flow nodes. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `kmx` | Number of vertical layers. NB. If keyword `zLayerGrowthFactor` is used, then number of layers is determined by D-Flow FM. | `0` |  | [-] |  |
| `layerType` | Vertical layer type. | `1` | <ul><li>**`1`**: sigma-layers</li><li>**`2`**: z- or z-sigma-layers</li><li>**`3`** 🔬: mixed layering in polygon regions</li><li>**`4`** 🔬: density controlled sigma-layers</li></ul> |  |  |
| `numTopSig` | Number of sigma-layers on top of z-layers in case of z-sigma-layers. | `0` |  |  |  |
| `sigmaGrowthFactor` ⚠️ | Growth factor of z-Layer thickness starting below the level specified by `dzTopUniAboveZ` till the bed. | `1.0` |  | [-] |  |
| `zLayerGrowthFactor` | Growth factor of z-Layer thickness starting below the level specified by `dzTopUniAboveZ` till the bed. | `1.0` |  | [-] |  |
| `floorLevTopLay` | The floor level of the top layer. | `-999.0` |  | [m] |  |
| `dzTop` | z-layer thickness of layers above level `dzTopUniAboveZ`. | `-999.0` |  | [m] |  |
| `dzTopUniAboveZ` | The level above which the layers will have uniform thickness of `dzTop`. | `-999.0` |  | [m] |  |
| `numTopSigUniform` | The number of sigma-layers in a z-sigma-model is constant or decreasing (depending on local depth). | `1` | <ul><li>**`0`**: decreasing</li><li>**`1`**: constant</li></ul> |  |  |
| `zLayBot` | If specified, first z-layer starts from `zLayBot`, if not, it starts from the lowest bed point. | `-999.0` |  | [m] |  |
| `zLayTop` | If specified, highest z-layer starts from `zLayTop`, if not, it ends at the initial water level. | `-999.0` |  | [m] |  |
| `stretchType` | Stretching type for non-uniform layers. | `-1` | <ul><li>**`-1`**: uniform</li><li>**`0`**: uniform</li><li>**`1`**: user defined</li><li>**`2`**: exponential</li></ul> |  |  |
| `stretchCoef` | Coefficients for sigma layer. For `stretchType`=1: percentages of the layers, user defined, laycof(`kmx`). For `stretchType`=2: Stretching level, and two coefficients for layer growth, laycof(3). | `<empty>` |  | [-] |  |
| `dxMin1D` | Minimum 1D link length. | `0.001` |  | [m] |  |
| `dxDoubleAt1DEndNodes` | Extend a 1D grid cell at the end of a network with 0.5∆x. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `changeVelocityAtStructures` | Ignore structure dimensions for the velocity at hydraulic structures, when calculating the surrounding cell centered flow velocities. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `changeStructureDimensions` | Change the structure dimensions in case these are inconsistent with the channel dimensions. ⋄ weirs, orifices, general structures: 1. In case the crest width exceeds the surface width, the crest width is set to the surface width; 2. In case the crest level is lower than the bed level, the crest level is set to the bed level. ⋄ bridges: 1. In case the crest width exceeds the surface width, the crest width is set to the surface width; 2. In case the flow area of the bridge exceeds the upstream flow area the flow area of the bridge is set to the upstream flow area. ⋄ universal weirs: only the crest level is checked and changed. NOTE: It is strongly advised not to change this parameter (true). Since turning this option off can lead to instabilities and unrealistic results. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `calculateBedLevelOverNonActiveLinks` 🔬 | Specifies whether the bed levels must be computed over all links of a cell including the closed boundaries, thin dams and dry points (= 0), or only the flow links. (= 1) | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `stripMesh` 🔬 | Strip unused nodes and links from the mesh after clipping. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `topLayMinThick` 🔬 | Minimum top layer thickness, only for Z-layers. | `0.01` |  | [m] |  |
| `helmert` 🔬 | Use Helmert. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `waterDepthIni1D` 🔬 | Initial waterdepth in 1D. | `-999.0` |  |  |  |
| `zLayerAtuByBob` 🔬 | Lowest connected cells governed by bob instead of by bL L/R. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `shipDefFile` 🔬 | File \*.shd containing ship definitions. | `<empty>` |  |  |  |
| `bedWaveLength` 🔬 | Bed testcases. | `0.0` |  |  |  |
| `removeSmallLinksTrsh` 🔬 | Remove small links. | `0.1` |  |  |  |
| `createLinks1D2D` 🔬 | Rashly create links between 1D nodes and 2D cells when initializing model. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `bedWaveAmplitude` 🔬 | Bed testcases. | `0.0` |  |  |  |
| `uniformHu` 🔬 | Waterdepth in rigid-lid-like solution. | `-999.0` |  |  |  |
| `tSigma` 🔬 | Sigma adaptation period for `layerType` = 4 (density controlled sigma-layers). | `100.0` |  | [s] |  |
| `dpuopt` 🔬 | Bed level interpolation at velocity point in case of tile approach bed level. | `1` | <ul><li>**`1`**: max</li><li>**`2`**: mean</li></ul> |  |  |
| `keepZLayeringAtBed` | Z-layering at bed. | `2` | <ul><li>**`0`**: original bed level</li><li>**`1`**: adapted bed level</li><li>**`2`**: Ztbml approach of Delft3D-FLOW</li></ul> |  |  |
| `ihuzcSig` 🔬 | If `keepZLayeringAtBed`>=2. | `3` | <ul><li>**`1`**: average of Leftsig and Rightsig</li><li>**`2`**: max of Leftsig and Rightsig</li><li>**`3`**: min of Leftsig and Rightsig</li><li>**`4`**: uniform</li></ul> |  |  |
| `ihuz` 🔬 | TODO | `4` |  |  |  |
| `cosphiutrsh` 🔬 | 1.0=no bad orthos. | `0.5` | `[0.0, 1.0]` |  |  |
| `cutCellList` 🔬 | File with names of cutcell polygons, e.g. cutcellpolygons.lst. | `<empty>` |  |  |  |
| `uniformTyp1D` 🔬 | Uniform type for channel profiles not specified by profloc. | `3` |  |  |  |
| `1D2DInternalLinkType` 🔬 | Link treatment method for type-3 internal links. | `1` |  |  |  |
| `pipeFile` 🔬 | File \*.pliz containing pipe-based 'culverts'. | `<empty>` |  |  |  |
| `groundLayerThickness` 🔬 | Only in pipes: groundlayer thickness. | `-999.0` |  | [m] |  |
| `extrBl` 🔬 | Extrapolate bed level at boundaries according to the slope. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `keepZLay1BedVol` 🔬 | Correct volumes when `keepZLayeringAtBed`=1. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `orgFloorLevTopLayDef` ⚠️ | Backward compatibility option: floorlevtoplay is dztop below specified level. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `bedLevMode` | Bed level mode. | `1` |  |  |  |


<a id="supported-keywords-and-options-volumetables"></a>
## [volumeTables]

This section contains the settings for the volume tables used in 1D grid cells.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `useVolumeTables` | Use volume tables for 1D grid cells. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `increment` | The height increment for the volume tables. | `0.1` |  | [m] |  |
| `useVolumeTableFile` | Read and write the volume table from/to file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |


<a id="supported-keywords-and-options-numerics"></a>
## [numerics]

This section contains the settings of specific parts of the flow solver, such as limiters and the iterative solver type.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `cflMax` | Maximum Courant nr. | `0.7` |  | [-] |  |
| `advecType` | Advection type. | `33` |  |  |  |
| `advecCorrection1D2D` | Advection correction of 1D2D link volume. | `0` | <ul><li>**`0`**: regular advection</li><li>**`1`**: link volume au*dx</li><li>**`2`**: advection on 1D2D switched off</li></ul> |  |  |
| `timeStepType` | Type of time stepping. | `2` | <ul><li>**`0`**: only transport</li><li>**`1`**: transport + velocity update</li><li>**`2`**: full implicit step-reduce</li><li>**`3`**: step-Jacobi</li><li>**`4`**: explicit</li></ul> |  |  |
| `maxNonLinearIterations` | Maximal iterations in non-linear iteration loop before a time step reduction is applied. | `100` |  |  |  |
| `setHorizontalBobsFor1D2D` | Bobs are set to 2D bedlevel, to prevent incorrect storage in sewer system. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `limTypHu` | Limiter type for waterdepth in continuity eq. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: minmod</li><li>**`2`**: vanLeer</li><li>**`3`**: Koren</li><li>**`4`**: Monotone Central</li></ul> |  |  |
| `limTypMom` | Limiter type for cell center advection velocity. | `4` | <ul><li>**`0`**: no</li><li>**`1`**: minmod</li><li>**`2`**: vanLeer</li><li>**`4`**: Monotone Central</li></ul> |  |  |
| `limTypSa` | Limiter type for salinity transport. | `4` | <ul><li>**`0`**: no</li><li>**`1`**: minmod</li><li>**`2`**: vanLeer</li><li>**`4`**: Monotone Central</li></ul> |  |  |
| `pure1D` | Purely 1D advection. | `0` | <ul><li>**`0`**: original advection using velocity vector</li><li>**`1`**: pure 1D using flow volume vol1_f</li><li>**`2`**: pure 1D using volume vol1</li></ul> |  |  |
| `junction1D` | Advection at 1D junctions. | `0` | <ul><li>**`0`**: original 1D advection using velocity vector</li><li>**`1`**: same as along 1D channels using `pure1D`=1</li></ul> |  |  |
| `icgSolver` | Solver type. | `4` | <ul><li>**`1`**: sobekGS_OMP</li><li>**`2`**: sobekGS_OMPthreadsafe</li><li>**`3`**: sobekGS</li><li>**`4`**: sobekGS + Saad-ILUD (default sequential)</li><li>**`5`**: parallel/global Saad</li><li>**`6`**: PETSc (default parallel)</li><li>**`7`**: CG+MILU (parallel)</li></ul> |  |  |
| `logSolverConvergence` | Print time step, number of solver iterations and solver residual to diagnostic output. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `maxDegree` | Maximum degree in Gauss elimination. | `6` |  |  |  |
| `fixedWeirScheme` | Fixed weir scheme. | `9` | <ul><li>**`0`**: none</li><li>**`1`**: TODO</li><li>**`2`**: TODO</li><li>**`3`**: TODO</li><li>**`4`**: TODO</li><li>**`5`**: TODO</li><li>**`6`**: semi-subgrid scheme</li><li>**`7`**: TODO</li><li>**`8`**: Tabellenboek</li><li>**`9`**: Villemonte</li></ul> |  |  |
| `fixedWeirContraction` | flow width = flow width\*`fixedWeirContraction`. | `1.0` |  | [-] |  |
| `fixedWeirTopWidth` | Uniform width of the groyne part of fixed weirs. | `3.0` |  | [m] |  |
| `fixedWeirTalud` | Uniform talud slope of fixed weirs. | `4.0` |  |  |  |
| `fixedWeirTopFrictCoef` | Uniform friction coefficient of the groyne part of fixed weirs. | `-999.0` |  | [depends on frictiontype] |  |
| `fixedWeirRelaxationCoef` | Fixed weir relaxation coefficient for computation of energy loss. | `0.6` |  |  |  |
| `fixedWeirScheme1D2D` | Fixed weir scheme for 1D2D links. | `0` | <ul><li>**`0`**: same as `fixedWeirScheme`</li><li>**`1`**: lateral iterative fixed weir scheme</li></ul> |  |  |
| `fixedWeir1D2D_dx` | Extra delta x for lateral 1D2D fixed weirs. | `50.0` |  |  |  |
| `izBndPos` | Position of z boundary. | `0` | <ul><li>**`0`**: mirroring of closest cell (as in Delft3D-FLOW)</li><li>**`1`**: on net boundary</li></ul> |  |  |
| `tlfSmo` | Fourier smoothing time on water level boundaries. | `0.0` |  | [s] |  |
| `slopeDrop2D` | Droplosses are applied if local bottom slope > `slopeDrop2D`, <=0 = no droplosses. | `0.0` |  | [m] |  |
| `drop1D` | Limit the downstream water level in the momentum equation to the downstream invert level, BOBdown (ζ ∗ down = max (BOBdown, ζdown)). | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `chkAdvd` | Check advection terms if depth < `chkAdvd`. | `0.1` |  | [m] |  |
| `teta0` | Theta (implicitness) of time integration. | `0.55` | `(0.5, 1.0)` | [-] |  |
| `cstBnd` | Delft3D-FLOW type velocity treatment near boundaries for small coastal models or not. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `maxItVerticalForesterSal` ⚠️ | Forester iterations for salinity (0: no vertical filter for salinity, > 0: max nr of iterations). | `0` | `≥0` |  |  |
| `maxItVerticalForester` | Forester iterations for all constituents (0: no vertical filter, > 0: max nr of iterations). | `0` | `≥0` |  |  |
| `maxItVerticalForesterTem` ⚠️ | Forester iterations for temperature (0: no vertical filter for temperature, > 0: max nr of iterations). | `0` | `≥0` |  |  |
| `transportAutoTimeStepDiff` | Auto timestepdiff in transport. | `0` | <ul><li>**`0`**: lim diff, no lim Dt_tr</li><li>**`1`**: no lim diff, lim Dt_tr</li><li>**`2`**: no lim diff, no lim Dt_tr</li><li>**`3`**: implicit (only 2D)</li></ul> |  |  |
| `implicitDiffusion2D` | Implicit diffusion 2D. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `turbulenceModel` | Turbulence model. | `3` | <ul><li>**`0`**: no</li><li>**`1`**: constant</li><li>**`2`**: algebraic</li><li>**`3`**: k-epsilon</li><li>**`4`**: k-tau</li></ul> |  |  |
| `c1e` | c1 coefficient in turbulence model. | `1.489` |  |  |  |
| `c3eStable` | c3e coefficient (for stable stratification) in k-eps turbulence model. | `0.0` |  |  |  |
| `c3eUnstable` | c3e coefficient (for unstable stratification) in k-eps turbulence model. | `1.489` |  |  |  |
| `antiCreep` | Include anti-creep to suppress artificial vertical diffusion. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `barocPOnBnd` | Include baroclinic pressure on open boundaries. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `maxItPresDens` ⚠️ | Max number of iterations in pressure-density coupling, only if `thermobaricity`=true. | `1` |  |  |  |
| `diagnosticTransport` | No update of transport quantities, also known as diagnostic transport. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `maxWaterLevelDiff` | Upper bound on water level changes, (<= 0: no bounds). Run will abort when violated. | `0.0` |  | [m] |  |
| `maxVelocityDiff` | Upper bound on velocity changes, (<= 0: no bounds). Run will abort when violated. | `0.0` |  | [m/s] |  |
| `maxVelocity` | Upper bound on velocity (<= 0: no bounds). Run will abort when violated. | `0.0` |  | [m/s] |  |
| `waterLevelWarn` | Warning level on water level (<= 0: no check). | `0.0` |  | [m AD] |  |
| `velocityWarn` | Warning level on velocity (<= 0: no check). | `0.0` |  | [m/s] |  |
| `velMagnWarn` | Warning level on velocity magnitude (<= 0: no check). | `0.0` |  | [m/s] |  |
| `minTimeStepBreak` 🔬 | Smallest allowed timestep, checked on a sliding average of several timesteps. Run will abort when violated. | `0.001` |  | [s] |  |
| `epshu` | Threshold water depth for wetting and drying. | `0.0001` |  | [m] |  |
| `epsMaxLev` | Stop criterion for non linear iteration. | `1e-08` |  |  |  |
| `epsMaxLevM` | Stop criterion for Nested Newton loop in non-linear iteration. | `1e-08` |  |  |  |
| `flowSolver` | Flow solver. | `generic1d2d3d` | <ul><li>**`generic1d2d3d`**: generic solver</li><li>**`implicit1d`**: implicit 1D solver</li></ul> |  |  |
| `lateral_fixedweir_umin` | Minimal velocity threshold for weir losses in iterative lateral 1D2D weir coupling. | `0.0` |  | [m/s] |  |
| `jasfer3D` | Corrections for spherical coordinates. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `cfFacVer` 🔬 | Factor for including (1-CFL) in HO term vertical. | `0.0` | `[0.0, 1.0]` |  |  |
| `eddyViscosityBedFacmax` 🔬 | Limit eddy viscosity at bed by factor of first layer above. | `0.0` | `[0.0, 1.0]` |  |  |
| `lateral_fixedweir_umin_method` 🔬 | Method for minimal velocity threshold for weir losses in iterative lateral 1D2D weir coupling. | `0` |  |  |  |
| `lateral_fixedweir_minimal_1d2d_embankment` 🔬 | Minimal crest height of 1D2D SOBEK-DFM embankments. | `0.0` |  |  |  |
| `testFixedWeirs` 🔬 | Test for fixed weir algorithms. | `0` | <ul><li>**`0`**: original Villemonte (Sieben2010) approach</li><li>**`1`**: Sieben2007</li></ul> |  |  |
| `jposhchk` 🔬 | Check for positive waterdepth. | `2` | <ul><li>**`0`**: no</li><li>**`1`**: 0.7dts, just redo</li><li>**`2`**: 1.0dts, close all links</li><li>**`3`**: 0.7dts, close all links</li><li>**`4`**: 1.0dts, reduce au</li><li>**`5`**: 0.7dts, reduce au</li><li>**`6`**: 1.0dts, close outflowing links</li><li>**`7`**: 0.7dts, close outflowing links</li></ul> |  |  |
| `cfConHorMom` 🔬 | Constant for including (1-CFL) in HO term horizontal momentum. | `0.0` |  |  |  |
| `cfFacHorMom` 🔬 | Factor for including (1-CFL) in HO term horizontal momentum. | `1.0` | `[0.0, 1.0]` |  |  |
| `trsh_u1lb` 🔬 | 2D bedfriction in 3D below this threshold. | `0.0` | `[0.0, 1.0]` | [m] |  |
| `jaupwindsrc` 🔬 | Upwind advection discretization at sources/sinks. | `1` | <ul><li>**`0`**: no (higher-order)</li><li>**`1`**: yes (first-order)</li></ul> |  |  |
| `corioAdamsBashfordFac` 🔬 | Only when `newCorio`=1, Adams-Bashford factor in Coriolis term. | `0.5` |  |  |  |
| `corioConstant` 🔬 | Coriolis constant. | `0` | <ul><li>**`0`**: no change in Coriolis</li><li>**`1`**: Coriolis constant in Spherical models</li><li>**`2`**: beta plane approach both in Cartesian and Spherical coordinates</li></ul> |  |  |
| `drop3D` 🔬 | Drop losses in 3D are applied if z upwind is below bob + 2/3 hu\*`drop3D`. | `1.0` |  |  |  |
| `zLayerCenterBedVel` 🔬 | Reconstruction of center velocity at half closed bed cells. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `horAdvTypZLayer` 🔬 | Horizontal advection treatment of z-layers for dambreaks. | `0` | <ul><li>**`0`**: default</li><li>**`2`**: sigma-like</li></ul> |  |  |
| `iCoriolisType` 🔬 | Coriolis type. | `5` | <ul><li>**`0`**: no Coriolis force</li><li>**`5`**: default approach for Coriolis (depth dependent and similar to Delft3D-FLOW)</li></ul> |  |  |
| `zwsbTol` 🔬 | Tolerance for zws(kb-1) at bed. | `0.0` |  |  |  |
| `cfExpHu` 🔬 | Exponent for including (1-CFL) in sethu. | `1.0` |  |  |  |
| `jbasqbnddownwindhs` 🔬 | Water depth scheme at discharge boundaries. | `0` | <ul><li>**`0`**: original hu</li><li>**`1`**: downwind hs</li></ul> |  |  |
| `filterOrder` 🔬 | First-order or second order filter to suppress checkerboarding. | `2` | <ul><li>**`1`**: first order</li><li>**`2`**: second order</li></ul> |  |  |
| `keepSTBndOnOutflow` 🔬 | Keep salinity and temperature signals on boundary cells at outflow. | `1` | <ul><li>**`0`**: no (like Thatcher-Harleman)</li><li>**`1`**: yes (use prescribed boundary conditions)</li></ul> |  |  |
| `keepZLayeringAtBed` ⚠️ | Z-layering at bed. | `2` | <ul><li>**`0`**: original bed level</li><li>**`1`**: adapted bed level</li><li>**`2`**: Ztbml approach of Delft3D-FLOW</li></ul> |  |  |
| `logProfAtUBndIn` 🔬 | ubnds inflow. | `1` | <ul><li>**`0`**: uniform U1</li><li>**`1`**: log U1</li><li>**`2`**: log U1 and k-eps accordingly</li></ul> |  |  |
| `logProfKepsBndIn` 🔬 | 3D profile at open boundaries. | `0` | <ul><li>**`0`**: k-eps</li><li>**`1`**: log k-eps inflow</li><li>**`2`**: log k-eps in- and outflow</li></ul> |  |  |
| `epshstem` 🔬 | Only compute heat flux + evaporation if depth > `epshstem`. | `0.001` |  |  |  |
| `diffusionOnBnd` 🔬 | Horizontal diffusion on open boundaries. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `newCorio` 🔬 | New standard way of Coriolis term calculation. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `barrierAdvection` 🔬 | Advection modelling at barriers. | `1` | <ul><li>**`1`**: no correction</li><li>**`2`**: advection correction</li></ul> |  |  |
| `rhoInterfaces` 🔬 | Estimate rho at 3D layer interfaces for baroclinic pressure gradient method. | `-1` | <ul><li>**`-1`**: Original linear interpolation</li><li>**`0`**: Improved linear interpolation</li><li>**`1`**: Recompute from salinity and temperature</li><li>**`2`**: Use cell density</li></ul> |  |  |
| `chkdifd` 🔬 | Check diffusion terms if depth < `chkdifd`, only if `transportAutoTimeStepDiff`=1. | `0.01` |  |  |  |
| `fixedWeirFrictScheme` 🔬 | Fixed weir friction scheme. | `0` | <ul><li>**`0`**: friction based on hu</li><li>**`1`**: friction based on subgrid weir friction scheme</li><li>**`2`**: without weir (like WAQUA)</li><li>**`3`**: full undisturbed velocity reconstruction</li><li>**`4`**: full undisturbed velocity reconstruction</li></ul> |  |  |
| `testDryingFlooding` 🔬 | Drying flooding algorithm. | `0` | <ul><li>**`0`**: D-Flow FM</li><li>**`1`**: Delft3D-FLOW</li><li>**`2`**: similar to `0`, and volume limitation in the transport solver based on `epshu`</li></ul> |  |  |
| `turbulenceAdvection` 🔬 | Turbulence advection. | `3` | <ul><li>**`0`**: none</li><li>**`1`**: upwind explicit</li><li>**`2`**: central explicit</li><li>**`3`**: horizontally explicit and vertically implicit</li><li>**`4`**: central implicit</li></ul> |  |  |
| `horizontalMomentumfilter` | Filter for reduction of checkerboarding. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `checkerboardMonitor` | Flag for checkerboarding output on history file (only for sigma layers yet). | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `tSpinUpTurbLogProf` | Spin up time when starting with a parabolic viscosity profile in whole model domain. | `0.0` |  | [s] |  |
| `vertAdvTypMom` | Vertical advection type in momentum equation. | `6` | <ul><li>**`3`**: upwind implicit</li><li>**`6`**: centerbased upwind explicit</li></ul> |  |  |
| `verticalAdvectionType` | Vertical advection type for salinity. Note that `verticalAdvectionType`=`centralImplicit` leads to less numerical dissipation than `verticalAdvectionType`=`higherOrderUpwindExplicit`. | `higherOrderUpwindExplicit` | <ul><li>**`centralImplicit`**: theta implicit</li><li>**`higherOrderUpwindExplicit`**: higher order explicit, no Forester filter</li></ul> |  |  |
| `vertAdvTypSal` ⚠️ | Vertical advection type for salinity. Note that `vertAdvTypSal`=4 leads to less numerical dissipation than `vertAdvTypSal`=6. | `6` | <ul><li>**`0`**: none</li><li>**`4`**: theta implicit</li><li>**`5`**: legacy mode for backwards compatibility</li><li>**`6`**: higher order explicit, no Forester filter</li></ul> |  |  |
| `vertAdvTypTem` ⚠️ | Vertical advection type for temperature. Note that `vertAdvTypTem`=4 leads to less numerical dissipation than `vertAdvTypTem`=6. | `6` | <ul><li>**`0`**: none</li><li>**`4`**: theta implicit</li><li>**`6`**: higher order explicit, no Forester filter</li></ul> |  |  |
| `zeroZBndInflowAdvection` | Switch for advection at open boundary. | `0` | <ul><li>**`0`**: Neumann</li><li>**`1`**: zero at inflow</li><li>**`2`**: zero at inflow and outflow</li></ul> |  |  |
| `turbulenceTimeIntegrationFactor` | 0.0=Tur0 from links, 1.0=Tur0 maximal mix of values from links with nodes | `0.0` | `[0.0, 1.0]` | [-] |  |
| `turbulenceTimeIntegrationMethod` | Where to apply `turbulenceTimeIntegrationFactor` | `2` | <ul><li>**`1`**: apply to all cells</li><li>**`2`**: apply only when vertical layers are horizontally connected</li></ul> |  |  |
| `locSaltMin` 🔬 | Minimum salinity for case of lock exchange. | `5.0` |  |  |  |
| `locSaltMax` 🔬 | Maximum salinity for case of lock exchange. | `10.0` |  |  |  |
| `locSaltLev` 🔬 | Salinity level for case of lock exchange. | `1.0` |  |  |  |
| `linContin` 🔬 | Linear continuity. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `cfExpHorMom` 🔬 | Exponent for including (1-CFL) in HO term horizontal momentum. | `1.0` |  |  |  |
| `coriohhtrsh` 🔬 | Only when `newCorio`=1, 0.0=no safety in hu/hus weightings. | `0.0` | `≥0.0` |  |  |
| `limTypW` 🔬 | Limiter type for wave action transport. | `4` | <ul><li>**`0`**: no</li><li>**`1`**: minmod</li><li>**`2`**: vanLeer</li><li>**`3`**: Koren</li><li>**`4`**: Monotone Central</li></ul> |  |  |
| `huWeirRegular` 🔬 | For Villemonte and Tabellenboek, regular hu below `huWeirRegular`. | `0.0` |  |  |  |
| `structureLayersActive` 🔬 | For Villemonte and Tabellenboek, regular hu below `huWeirRegular`. | `1` | <ul><li>**`0`**: structure flow through all layers</li><li>**`1`**: structure flow only through open layers</li></ul> |  |  |
| `baOrgFracMin` 🔬 | Cell area = max(orgcellarea\*`baOrgFracMin`, cut cell area). | `0.0` |  |  |  |
| `subsuplupdates1` 🔬 | Update water levels (s1) due to subsidence/uplift. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `linkDriedMx` 🔬 | Maximum number of Au growth steps after having dried. | `0` |  |  |  |
| `lateral_fixedweir_relax` 🔬 | Relaxation factor for iterative lateral 1D2D weir coupling algorithm. | `0.1` |  |  |  |
| `numlimdt_baorg` 🔬 | If previous numlimdt > `numlimdt_baorg` keep original cell area ba in cut cell. | `0` |  |  |  |
| `cfFacHu` 🔬 | Factor for including (1-CFL) in sethu. | `1.0` |  |  |  |
| `vertAdvTypMom3OnBnd` 🔬 | Vertical advection type u1 bnd UpwimpL. | `0` | <ul><li>**`0`**: follow javau</li><li>**`1`**: on bnd</li><li>**`2`**: on and near bnd</li></ul> |  |  |
| `noDerivedTypes` 🔬 | Use derived types. | `5` | <ul><li>**`0`**: use derived types</li><li>**`1`**: less</li><li>**`2`**: lesser</li><li>**`5`**: also deallocate derived types</li></ul> |  |  |
| `jarhoxu` 🔬 | Include density gradient in advection term. | `0` | <ul><li>**`0`**: no (strongly advised)</li><li>**`1`**: yes</li><li>**`2`**: also in barotropic and baroclinic pressure term</li><li>**`3`**: also in vertical advection</li><li>**`4`**: also in vertical advection</li></ul> |  |  |
| `ilutype` 🔬 | TODO. | `0` |  |  |  |
| `nlevel` 🔬 | TODO. | `0` |  |  |  |
| `dtol` 🔬 | TODO. | `0.0` |  |  |  |
| `pillarFarFieldVelocity` 🔬 | Use far-field velocity for pillars. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `minWaterlevelChangeBreak` | Stop the simulation when the rolling mean of the maximum water level change is below this value (considered when larger than 0.0). | `0.0` |  | [m] |  |
| `maxSSC` | Upper limit of cell centre SSC concentration after transport timestep. Default 1e6 (effectively switched off) | `1000000.0` |  |  |  |
| `perotWeightUpdate` | Perot weight update option. | `0` |  |  |  |
| `perotType` | Perot discretization type. | `1` |  |  |  |


<a id="supported-keywords-and-options-physics"></a>
## [physics]

In this field, physical model parameters can be inserted, for instance related to friction modelling and turbulence modelling.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `unifFrictCoef` | Uniform friction coefficient. | `0.023` | `≥0.0` | [depends on `unifFrictType`] |  |
| `unifFrictType` | Uniform friction type. | `1` | <ul><li>**`-1`**: hydraulically smooth</li><li>**`0`**: Chezy</li><li>**`1`**: Manning</li><li>**`2`**: White-Colebrook</li><li>**`3`**: White-Colebrook in WAQUA</li></ul> |  |  |
| `unifFrictCoef1D` | Uniform friction coefficient in 1D links. | `0.023` | `≥0.0` | [depends on `unifFrictType`] |  |
| `unifFrictCoef1D2D` 🔬 | Uniform friction coefficient in 1D2D links. | `0.023` | `≥0.0` | [depends on `unifFrictType`] |  |
| `unifFrictCoefLin` | Uniform linear friction coefficient. | `0.0` | `≥0.0` | [m/s] |  |
| `vicouv` | Uniform horizontal eddy viscosity. | `0.1` |  | [m²/s] |  |
| `dicouv` | Uniform horizontal eddy diffusivity. | `0.1` |  | [m²/s] |  |
| `vicoww` | Background vertical eddy viscosity. | `1e-06` |  | [m²/s] |  |
| `dicoww` | Background vertical eddy diffusivity. | `1e-06` |  | [m²/s] |  |
| `vicwminb` | Minimum viscosity in production and buoyancy term. | `0.0` |  | [m²/s] |  |
| `xlOzmidov` | Ozmidov length scale, 0.0=no contribution of internal waves to vertical diffusion. | `0.0` |  | [m] |  |
| `TKEMin` 🔬 | Minimum turbulence kinetic energy (TKE) value in k-eps turbulence model. | `1e-32` |  | [m²/s²] |  |
| `EPSMin` 🔬 | Minimum turbulent dissipation rate (EPS) value in k-eps turbulence model. | `1e-32` |  | [m²/s²] |  |
| `TAUMin` 🔬 | Minimum turbulent time scale (TAU) value in k-tau turbulence model. | `1e-32` |  | [m²/s²] |  |
| `Smagorinsky` | Add Smagorinsky horizontal turbulence: vicu = vicu + ((`Smagorinsky`\*dx)^2)\*S. | `0.2` |  | [-] |  |
| `Elder` | Add Elder contribution: vicu = vicu + (`Elder`\*kappa\*ustar\*H/6); e.g. 1.0. | `0.0` |  | [-] |  |
| `irov` | Wall friction. | `0` | <ul><li>**`0`**: free slip</li><li>**`1`**: partial slip using `wall_ks`</li><li>**`2`**: no slip</li></ul> |  |  |
| `wall_ks` | Nikuradse roughness for side walls, wall_z0=`wall_ks`/30. | `0.0` |  | [m] |  |
| `rhoMean` | Average water density. | `1000.0` |  | [kg/m³] |  |
| `iDensForm` | Density calculation. | `2` | <ul><li>**`0`**: uniform</li><li>**`1`**: Eckart</li><li>**`2`**: UNESCO</li><li>**`3`**: UNESCO83</li><li>**`13`**: `3`+pressure</li></ul> |  |  |
| `thermobaricity` | Include pressure effects on water density. Only works for `iDensForm`=3 (UNESCO83). | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `ag` | Gravitational acceleration. | `9.81` |  | [m/s²] |  |
| `tidalForcing` | Tidal forcing, if jserfic=1. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `itcap` | Upper limit on internal tides dissipation. | `0.0` |  | [W/m²] |  |
| `doodsonStart` | Doodson start time for tidal forcing. | `55.565` |  | [s] |  |
| `doodsonStop` | Doodson stop time for tidal forcing. | `375.575` |  | [s] |  |
| `doodsonEps` | Doodson tolerance level for tidal forcing. | `0.0` |  | [s] |  |
| `villemonteCD1` | Calibration coefficient for Villemonte. | `1.0` |  | [-] |  |
| `villemonteCD2` | Calibration coefficient for Villemonte. | `10.0` |  | [-] |  |
| `salinity` | Include salinity. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `initialSalinity` | Initial salinity concentration. | `0.0` |  | [ppt] |  |
| `sal0AboveZLev` | Salinity 0 above level. | `-999.0` |  | [m] |  |
| `deltaSalinity` | Uniform initial salinity. | `-999.0` |  | [ppt] |  |
| `backgroundSalinity` | Background salinity for eqn. of state if salinity not computed. | `30.0` |  | [ppt] |  |
| `temperature` | Include temperature. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: only transport</li><li>**`3`**: excess model of D3D</li><li>**`5`**: composite (ocean) model</li></ul> |  |  |
| `initialTemperature` | Initial temperature. | `6.0` |  | [°C] |  |
| `backgroundWaterTemperature` | Background water temperature for eqn. of state if temperature not computed. | `20.0` |  | [°C] |  |
| `SecchiDepth` | Water clarity parameter. | `1.0` |  | [m] |  |
| `SecchiDepth2` ⚠️ | Water clarity parameter for non-penetrative radiation. | `0.0` |  | [m] |  |
| `SecchiDepthNonPenetrative` 🔬 | Water clarity parameter for non-penetrative radiation. | `0.0` |  | [m] |  |
| `SecchiDepth2Fraction` ⚠️ | Fraction of solar radiation that falls in non-penetrative spectrum. | `0.0` |  | [-] |  |
| `SecchiDepthNonPenetrativeFraction` 🔬 | Fraction of solar radiation that falls in non-penetrative spectrum. | `0.0` |  | [-] |  |
| `stanton` | Coefficient for convective heat flux, if negative, then Cd wind is used. | `0.0013` |  | [-] |  |
| `dalton` | Coefficient for evaporative heat flux, if negative, then Cd wind is used. | `0.0013` |  | [-] |  |
| `albedo` | Albedo coefficient. Fraction of solar radiation reflected by the water surface. | `0.06` |  | [-] |  |
| `tempMax` | Limit the temperature to max value. | `-999.0` |  | [°C] |  |
| `tempMin` | Limit the temperature to min value. | `0.0` |  | [°C] |  |
| `saliMax` | Limit for salinity to max value. | `-999.0` |  | [ppt] |  |
| `saliMin` | Limit for salinity to min value. | `0.0` |  | [ppt] |  |
| `heat_eachStep` | Switch for heat each time step or each user time step. | `0` | <ul><li>**`0`**: each user time step</li><li>**`1`**: each time step</li></ul> |  |  |
| `nudgeTimeUni` | Uniform nudge relaxation time. | `3600.0` |  | [s] |  |
| `iniWithNudge` | Initialize salinity and temperature with nudge variables. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li><li>**`2`**: only initialize, no nudging</li></ul> |  |  |
| `secondaryFlow` | Secondary flow. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `betaSpiral` | Weight factor of the spiral flow intensity on flow dispersion stresses. 0.0=disabled. | `0.0` |  | [-] |  |
| `breachGrowth` | Method for distributing dam breach width over dam break flow links. | `symmetric-asymmetric` | <ul><li>**`symmetric`**: symmetrical growth</li><li>**`proportional`**: proportional growth relative to available width</li><li>**`symmetric-asymmetric`**: symmetrical growth until end reached on one side</li></ul> |  |  |
| `thermobaricityInPressureGradient` 🔬 | Apply thermobaricity in computing the baroclinic pressure gradient. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `surfTempSmoFac` 🔬 | Horizontal smoothing factor for surface water in heatflux computations. | `0.0` | `[0.0, 1.0]` |  |  |
| `selfAttractionLoading_correct_wl_with_ini` 🔬 | Correct water level with initial water level in self attraction and loading. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `nfEntrainmentMomentum` 🔬 | Use momentum transfer in NearField related entrainment. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `equili` 🔬 | Equilibrium spiral flow intensity. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `soilTempThick` 🔬 | Use soil temperature buffer if >0.0 | `0.0` |  | [m] |  |
| `selfAttractionLoading` 🔬 | Use self attraction and loading. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li><li>**`2`**: only self attraction</li></ul> |  |  |
| `prandtlNumberTemperature` 🔬 | Turbulent Prandtl number for temperature. | `0.7` |  |  |  |
| `schmidtNumberSalinity` 🔬 | Turbulent Schmidt number for salinity. | `0.7` |  |  |  |
| `schmidtNumberTracer` 🔬 | Turbulent Schmidt number for tracer(s). | `1.0` |  |  |  |
| `jadelvappos` ⚠️ | Only positive forced evaporation fluxes. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `freeConvectionCoefficient` 🔬 | Free convection turbulence coefficient. | `0.14` |  |  |  |
| `uniffrictcoef1dgrlay` 🔬 | Uniform ground layer friction coefficient for ocean models. | `0.05` |  | [m/s] |  |
| `salinityDependentFreezingPoint` | Enable computation of negative temperature values by adjusting freezing point based on salinity levels. `tempMin` should be below 0 degrees Celsius. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `dynRoughVeg` 🔬 | Switch for dynamic vegetation roughness (only in combination with friction type Manning). | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `dRoot` 🔬 | Root depth. | `0.5` |  | [m] |  |
| `dStem` 🔬 | Stem height. | `0.5` |  | [m] |  |
| `unifFrictCoefNoVeg` 🔬 | Uniform friction (Manning) coefficient without vegetation. | `0.023` |  | [s/m^{1/3}] |  |
| `dynVegPol` 🔬 | Polygon file (\*.pol) marking the area within which to apply dynamic vegetation roughness. | `<empty>` |  |  |  |


<a id="supported-keywords-and-options-sediment"></a>
## [sediment]

This section contains the setting for sediment transport and morphology.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `sedimentModelNr` | Sediment model number. | `2` | <ul><li>**`0`**: no</li><li>**`1`**: Krone</li><li>**`2`**: SvR2007</li><li>**`3`**: E-H</li><li>**`4`**: MorphologyModule</li></ul> |  |  |
| `morFile` | Morphology settings file (\*.mor). | `<empty>` |  |  |  |
| `sedFile` | Sediment characteristics file (\*.sed). | `<empty>` |  |  |  |
| `jaceneqtr` | TODO | `2` |  |  |  |
| `mxgrkrone` 🔬 | Highest fraction index treated by Krone. | `-1` |  |  |  |
| `sedDensCoupling` 🔬 | Sed rho coupling. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `implicitFallVelocity` 🔬 | Implicit or explicit fall velocity | `1` | <ul><li>**`0`**: implicit</li><li>**`1`**: explicit</li></ul> |  |  |
| `nr_of_sedfractions` 🔬 | Number of sediment fractions. | `0` |  |  |  |
| `dredgeFile` | Dredging/dumping settings file (\*.dad) | `<empty>` |  |  |  |
| `bndTreatment` | Separate treatment boundary links in upwinding transports. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `sourSink` | Switch off source or sink terms for sed advection. | `1` | <ul><li>**`0`**: off</li><li>**`1`**: source+sink</li><li>**`2`**: source</li><li>**`3`**: sink for sediment</li></ul> |  |  |
| `cRefCav` | Calibration par only in jased==3, default=20.0 ( )  | `20.0` |  |  |  |
| `d50` | Mean Sandgrain diameter (m), e.g. 0.0001 | `0.0002` |  |  |  |
| `morFac` | Morphological acceleration factor (), bottom updates active for morfac > 0, 1.0=realtime, etc | `1.0` |  |  |  |
| `rhoSed` | Mean Sandgrain rho (kg/m3) , e.g. 2650 | `2650.0` |  |  |  |
| `numIntVerticalEinstein` | Number of vertical intervals in Einstein integrals ( )  | `10` |  |  |  |
| `alfaBed` | Calibration par bed      load, default=1.0 ( )  | `1.0` |  |  |  |
| `tMorfSpinup` | Spin up time for morphological adaptations (s) | `0.0` |  |  |  |
| `initialSedimentConcentration` | Initial Sediment Concentration in jased==3 (kg/m3)   | `0.0` |  |  |  |
| `uniformErodableThickness` | Uniform erodable layer thickness (m) | `1.0` |  |  |  |
| `alfaSus` | Calibration par suspended load, default=1.0 ( )  | `1.0` |  |  |  |
| `morCFL` | Use CFL-like condition for morphologic updating (0=no, 1=yes) (default yes) | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `DzbDtMax` | Maximum bed level change (m) per time step for the case MorCFL=1 (default=0.1 m) | `0.1` |  |  |  |
| `morphoPol` | Only apply bed updating within specified polygon (\*.pol) | `<empty>` |  |  |  |
| `ws` | Fall velocity (m/s), e.g. 0.0005 m/s | `0.0003` |  |  |  |
| `erosionPar` | Krone Partheniades erosion parameter, e.g. 0.0001  (kg/(m2s) | `0.0001` |  |  |  |
| `tauCre` | Critical shear stress for erosion    (N/m2), e.g. 0.3 | `0.3` |  |  |  |
| `inMorphoPol` | Value of the update inside MorphoPol (0=inside polygon no update, 1=inside polygon yes update) | `1` | <ul><li>**`0`**: inside polygon no update</li><li>**`1`**: inside polygon yes update</li></ul> |  |  |
| `mormergeDtUser` | Mormerge operation | `0` | <ul><li>**`0`**: at dts</li><li>**`1`**: at dtuser timesteps</li></ul> |  |  |
| `upperLimitSSC` | Upper limit of cell centre SSC concentration after transport timestep. | `1e-06` |  |  |  |


<a id="supported-keywords-and-options-sedtrails"></a>
## [sedtrails]

Section for configuring the Sedtrails output.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `sedTrailsGrid` 🔬 | Grid file for sedtrails output locations on corners. | `<empty>` |  |  |  |
| `sedtrailsAnalysis` 🔬 | Option for writing sedtrails to the output file. | `all` | <ul><li>**`all`**: write all quantities</li><li>**`transport`**: write bedload and suspended-load transport vectors per sediment fraction</li><li>**`flowvelocity`**: write depth-averaged velocity components at grid corners</li><li>**`soulsby`**: write quantities for the Soulsby sediment mobility method</li></ul> |  |  |
| `sedtrailsInterval` 🔬 | Sedtrails output, given as 'interval' 'start period' 'end period'. | `0.0` |  |  |  |
| `sedtrailsOutputFile` 🔬 | Sedtrails time-averaged output file. | `<empty>` |  |  |  |


<a id="supported-keywords-and-options-wind"></a>
## [wind]

The wind section prescribes the dependency of the wind drag coefficient to the wind velocity through 2 or 3 breakpoints. This field also contains pressure information.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `icdTyp` | Wind drag coefficient type. | `2` | <ul><li>**`0`**: None</li><li>**`1`**: Constant</li><li>**`2`**: Smith&Banke (2 pts)</li><li>**`3`**: Smith&Banke (3 pts)</li><li>**`4`**: Charnock 1955</li><li>**`5`**: Hwang 2005</li><li>**`6`**: Wuest 2005</li><li>**`7`**: Hersbach 2011 (2 pts)</li><li>**`8`**: Charnock+viscous</li><li>**`9`**: Garratt 1977</li></ul> |  |  |
| `cdBreakPoints` | Wind drag breakpoints. | `0.00063 0.00723` |  | [-] |  |
| `windSpeedBreakpoints` | Wind speed breakpoints. | `0.0 100.0` |  | [m/s] |  |
| `rhoAir` | Air density. | `1.2` |  | [kg/m³] |  |
| `computedAirDensity` | Compute air density. Requires quantities airpressure, airtemperature and dewpoint in ext-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `stressToWind` | Switch between wind speed and wind stress approach for wind forcing. | `0.0` | `[0.0, 1.0]` |  |  |
| `relativeWind` | Wind speed factor relative to top-layer water speed\*`relativeWind` (0.0=no relative wind, 1.0=using full top layer speed). | `0.0` | `[0.0, 1.0]` | [kg/m³] |  |
| `windPartialDry` | Reduce windstress on water if link partially dry, only for `bedLevType`=3. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `pavBnd` | Average air pressure on open boundaries, only applied if value > 0. | `0.0` |  | [N/m²] |  |
| `pavIni` | Initial air pressure, only applied if value > 0. | `0.0` |  | [N/m²] |  |
| `windHuOrZwsBased` 🔬 | Wind drag hu or zws based. | `0` | <ul><li>**`0`**: hu</li><li>**`1`**: zws</li></ul> |  |  |
| `varyingAirDensity` 🔬 | Compute air density. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wind_eachstep` 🔬 | Switch for wind and air pressure each time step or each user time step. | `0` | <ul><li>**`0`**: each user time step</li><li>**`1`**: each time step</li></ul> |  |  |
| `rhoWaterInWindStress` | Water density used in computation of wind stress. | `0` | <ul><li>**`0`**: Rhomean</li><li>**`1`**: local (surface) density of model</li></ul> |  |  |


<a id="supported-keywords-and-options-waves"></a>
## [waves]

The wind section prescribes the dependency of the wind drag coefficient to the wind velocity through 2 or 3 breakpoints. This field also contains pressure information.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `waveModelNr` | Wave model number. | `0` | <ul><li>**`0`**: none</li><li>**`1`**: fetch/depth limited Hurdle-Stive</li><li>**`2`**: fetch/depth limited Young-Verhagen</li><li>**`3`**: online wave coupling with D-Waves</li><li>**`5`**: uniform</li><li>**`7`**: offline wave coupling</li></ul> |  |  |
| `waveForcing` | Wave forcing mode (only supported for `waveModelNr` = 7). | `0` | <ul><li>**`0`**: none</li><li>**`1`**: radiation stresses</li><li>**`2`**: dissipation total</li><li>**`3`**: dissipation 3d</li></ul> |  |  |
| `3dWaveBreakerTurbulence` | Add wave-induced production terms in turbulence modelling | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `rouWav` | Friction model for wave induced shear stress. | `FR84` | <ul><li>**`none`**: none</li><li>**`FR84`**: Fredsøe (1984)</li><li>**`MS90`**: Myrhaug and Slaattelid (1990)</li><li>**`HT91`**: Huynh-Thanh and Temperville (1991)</li><li>**`GM79`**: Grant and Madsen (1979)</li><li>**`DS88`**: Davies et al. (1988)</li><li>**`BK67`**: Bijker (1967)</li><li>**`CJ85`**: Christoffersen and Jonsson (1985)</li><li>**`OY88`**: O' Connor and Yoo (1988)</li><li>**`VR04`**: Van Rijn et al. (2004)</li></ul> |  |  |
| `gammaX` | Maximum wave height/water depth ratio. | `0.5` |  |  |  |
| `flowWithoutWaves` | Exclude Wave data in the flow computations, passing it directly to D-WAQ. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `surfBeatInput` | File with surf beat input conditions. | `<empty>` |  |  |  |
| `waveSwartDelwaq` 🔬 | If `waveSwartDelwaq`=1 and tiWaq> 0, then increase tauwave to Delwaq with 0.5rhofwuorbuorb. | `0` | <ul><li>**`0`**: Communicate only the current-related shear stress (taucur) to Delwaq.</li><li>**`1`**: Communicate the linear sum taucur + tauwave to Delwaq.</li><li>**`2`**: Communicate the maximum shear stress (taubxu) to Delwaq.</li></ul> |  |  |
| `hwavuni` 🔬 | Root mean square wave height. | `0.0` |  | [m] |  |
| `tiFetchComp` 🔬 | Time interval fetch comp if `waveModelNr`=1,2. | `0.0` |  | [s] |  |
| `phiwavuni` 🔬 | Root mean square wave direction, math convention. | `0.0` |  | [deg] |  |
| `3DWaveStreaming` 🔬 | Influence of wave streaming. 0: no, 1: added to adve. | `0` |  |  |  |
| `3DWaveBoundaryLayer` 🔬 | Boundary layer formulation. | `1` | <ul><li>**`0`**: none</li><li>**`1`**: Sana</li></ul> |  |  |
| `twavuni` 🔬 | Root mean square wave period. | `0.0` |  | [s] |  |
| `uorbfac` 🔬 | Orbital velocities. | `0` | <ul><li>**`0`**: D3D style</li><li>**`1`**: Guza style</li></ul> |  |  |
| `fwfac` 🔬 | Factor for adjusting wave boundary layer streaming. | `1.0` | `≥0.0` | [-] |  |
| `ftauw` 🔬 | Factor for adjusting wave-related bottom shear stress. | `1.0` | `≥0.0` | [-] |  |
| `fbreak` 🔬 | Factor for adjusting wave breaking contribution to TKE. | `1.0` | `≥0.0` | [-] |  |
| `3Dwaveturbpendepth` 🔬 | Layer thickness as a proportion of hrms over which wave breaking adds to TKE source. | `1.5` | `≥0.0` | [-] |  |
| `fforc` 🔬 | Factor for adjusting wave forces in momentum equation. | `1.0` | `≥0.0` | [-] |  |
| `streamLayerFac` 🔬 | Factor for adjusting streaming layer thickness in momentum equation. | `3.0` | `>0.0` | [-] |  |
| `3DStokesProfile` 🔬 | Stokes profile. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: uniform over depth</li><li>**`2`**: second-order Stokes theory</li><li>**`3`**: `2` with vertical stokes gradient in adve.</li></ul> |  |  |
| `jamapsigwav` 🔬 | Wave height on map output | `0` | <ul><li>**`0`**: hrms wave height</li><li>**`1`**: sign wave height</li></ul> |  |  |
| `hminlw` 🔬 | Cut-off depth for application of wave forces in momentum balance. | `0.2` |  |  |  |
| `jahissigwav` 🔬 | Wave height on his output | `1` | <ul><li>**`0`**: hrms wave height</li><li>**`1`**: sign wave height</li></ul> |  |  |


<a id="supported-keywords-and-options-grw"></a>
## [grw]

This section contains the settings for ground water flow.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `groundWater` | Use (horizontal) ground water flow. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `infiltrationModel` | Infiltration method. | `0` | <ul><li>**`0`**: no infiltration</li><li>**`1`**: interception layer</li><li>**`2`**: constant infiltration capacity</li><li>**`3`**: model unsaturated/saturated (with grw)</li><li>**`4`**: Horton</li></ul> |  |  |
| `hInterceptionLayer` | Intercept this amount of rain. | `0.0` |  | [m] |  |
| `unifInfiltrationCapacity` | Uniform maximum infiltration capacity. | `0.0` |  | [m/s] |  |
| `conductivity` | Non-dimensionless K conductivity saturated, Q = K\*A\*i (m³/s). | `0.0` |  | [m/s] |  |
| `h_aquiferuni` | Level of impervious layer is bgrw = bl - `h_aquiferuni`, if negative, bgrw = `bgrwuni`. | `20.0` |  | [m] |  |
| `bgrwuni` | Uniform level of impervious layer, only used if `h_aquiferuni` is negative. | `-999.0` |  | [m] |  |
| `h_unsatini` | Initial level ground water is bedlevel - `h_unsatini`, if negative, sgrw = `sgrwini`. | `0.2` |  | [m] |  |
| `sgrwini` | Initial ground water level, if `h_unsatini` < 0. | `-999.0` |  | [m] |  |


<a id="supported-keywords-and-options-hydrology"></a>
## [hydrology]

This section contains the settings for hydrology.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `interceptionModel` | Interception model. | `0` | <ul><li>**`0`**: none</li><li>**`1`**: on, via layer thickness</li></ul> |  |  |


<a id="supported-keywords-and-options-time"></a>
## [time]

This section contains the time settings for the model, such as start and stop time of the simulation.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `refDate` | Reference date. By default midnight is taken (00h00m00s). | `20010101` |  |  | `yyyymmdd` |
| `tZone` | Data Sources in GMT are interrogated with time in minutes since `refDate`-`tZone`\*60. | `0.0` |  | [min] |  |
| `tUnit` | Time units in MDU. | `S` | <ul><li>**`D`**: days</li><li>**`H`**: hours</li><li>**`M`**: minutes</li><li>**`S`**: seconds</li></ul> |  |  |
| `dtUser` | User timestep in seconds (interval for external forcing update & his/map output). | `300.0` |  | [s] |  |
| `dtNodal` | Time interval for updating nodal factors in astronomical boundary conditions. | `21600.0` |  | [s] |  |
| `dtMax` | Maximum timestep in seconds. | `30.0` |  | [s] |  |
| `dtInit` | Initial timestep in seconds. | `1.0` |  | [s] |  |
| `tStart` | Start time w.r.t. `refDate`. | `0.0` |  | [tUnit] |  |
| `tStop` | Stop time w.r.t. `refDate`. | `86400.0` |  | [tUnit] |  |
| `startDateTime` | Computation start datetime, when specified, overrides `tStart`. | `<empty>` |  |  | `yyyymmddhhmmss` |
| `stopDateTime` | Computation stop datetime, when specified, overrides `tStop`. | `<empty>` |  |  | `yyyymmddhhmmss` |
| `updateRoughnessInterval` | Update interval for time dependent roughness parameters. | `86400.0` |  | [s] |  |
| `tStartTlfsmo` | Start time w.r.t. `refDate` of Fourier smoothing time on water level boundaries. | `0.0` |  | [tUnit] |  |
| `startDateTimeTlfsmo` | Computation start datetime w.r.t. `refDate` of Fourier smoothing time on water level boundaries, when specified, overrides `tStartTlfsmo`. | `<empty>` |  |  | `yyyymmddhhmmss` |
| `autoTimestep` | Automatic timestepping limited by the CFL condition. Several options are available controlling which flows are used in the CFL limit. Options 1 and 5 are the default for 2D and 3D models, respectively. | `1` | <ul><li>**`0`**: Use no CFL timestep limit (use constant timestepping)</li><li>**`1`**: For 2D-models; use outflows in CFL timestep limit</li><li>**`3`**: For 3D-models; use horizontal outflows (per cell-layer) in CFL timestep limit</li><li>**`4`**: For 3D-models; use horizontal in- and outflows (per cell-layer) in CFL timestep limit</li><li>**`5`**: For 3D-models; use in- or outflows (per cell-column) in CFL timestep limit</li></ul> |  |  |
| `autoTimestepNoStruct` | Exclude structure links (and neighbours) from time step limitation. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `autoTimestepNoQOut` | Exclude negative qin terms from time step limitation. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `dtFacMax` | Max timestep increase factor in successive time steps. | `1.1` |  |  |  |
| `timeStepAnalysis` 🔬 | Write time steps analysis file \*.steps. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `autoTimeStepVisc` 🔬 | Use time step limitation based on explicit diffusive term. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |


<a id="supported-keywords-and-options-restart"></a>
## [restart]

This section contains the settings for restarting from a previous simulation.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `restartFile` | Restart file, only from NetCDF-file, hence: either \*_rst.nc or \*_map.nc. | `<empty>` |  |  |  |
| `restartDateTime` | Restart time, only relevant but obligatory in case of restart from \*_map.nc. | `<empty>` |  |  | `yyyymmddhhmmss` |
| `rstIgnoreBl` 🔬 | Ignore bed level from restart. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |


<a id="supported-keywords-and-options-external-forcing"></a>
## [external forcing]

This section contains the settings for external forcings.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `extForceFile` ⚠️ | Old format for external forcings file \*.ext, link with tim/cmp-format boundary conditions specification. | `<empty>` |  |  |  |
| `extForceFileNew` | New format for external forcings file \*.ext, link with bc-format boundary conditions specification. Supports multiple filenames separated by spaces. Filenames containing spaces must be placed inside double quotes. | `<empty>` |  |  |  |
| `rainfall` | Include rainfall. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `qExt` | Include user Qin/out, externally provided. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `evaporation` | Include evaporation in water balance. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `windExt` | Include wind, externally provided. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: reserved for EC</li><li>**`2`**: yes</li></ul> |  |  |


<a id="supported-keywords-and-options-trachytopes"></a>
## [trachytopes]

This section contains the settings for trachytopes allowing the usage of different types of roughness formulations at different locations.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `trtRou` | Flag for trachytopes. | `N` | <ul><li>**`Y`**: on</li><li>**`N`**: off</li></ul> |  |  |
| `trtDef` | File (\*.ttd) including trachytope definitions. | `<empty>` |  |  |  |
| `trtL` | File (\*.arl) including distribution of trachytope definitions. | `<empty>` |  |  |  |
| `dtTrt` | Interval for updating of bottom roughness due to trachytopes in seconds. | `60.0` |  | [s] |  |
| `trtMxR` | Maximum recursion level for composite trachytope definitions. | `8` |  |  |  |
| `trtMth` 🔬 | Area averaging method. | `1` | <ul><li>**`1`**: Nikuradse k based</li><li>**`2`**: Chezy C based (parallel and serial)</li></ul> |  |  |
| `trtMnh` 🔬 | Minimum water depth for roughness computations. | `0.1` |  |  |  |
| `trtCll` 🔬 | Calibration factor file for roughness from trachytopes. | `<empty>` |  |  |  |


<a id="supported-keywords-and-options-output"></a>
## [output]

This section contains the settings for various output files.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `wrishp_crs` | Writing cross sections to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_dambreak` | Writing dambreaks to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_dryarea` | Writing dry areas to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_emb` | Writing embankments to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_fxw` | Writing fixed weirs to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_gate` | Writing gates to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_genstruc` | Writing general structures to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_obs` | Writing observation points to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_pump` | Writing pumps to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_src` | Writing sources and sinks to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_thd` | Writing thin dams to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrishp_weir` | Writing weirs to shape file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `writeSurfaceDataToMapFile` | Only write surface data to map file for 3D quantities. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `outputDir` | Output directory of map-, his-, rst-, dat- and timins files, default: DFM_OUTPUT_<modelname>. Set to . for no dir/current dir. | `<empty>` |  |  |  |
| `waqOutputDir` | Output directory of Water Quality files. | `<empty>` |  |  |  |
| `flowGeomFile` | \*_flowgeom.nc Flow geometry file in NetCDF format. | `<empty>` |  |  |  |
| `mapOutputPolygonFile` | Space-separated list of output polygon files to restrict map output to (e.g., \*_out.pol) | `<empty>` |  |  |  |
| `obsFile` | Space separated list of files, containing information about observation points. | `<empty>` |  |  |  |
| `deleteObsPointsOutsideGrid` | Delete observation points outside the grid. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `crsFile` | Space separated list of files, containing information about observation cross sections. | `<empty>` |  |  |  |
| `fouFile` | Name of attribute file that defines the \*_fou.nc Fourier output file in NetCDF format. | `<empty>` |  |  |  |
| `fouUpdateStep` | Fourier output type. | `0` | <ul><li>**`0`**: every user step</li><li>**`1`**: every computational step</li><li>**`2`**: equal to his output</li></ul> |  |  |
| `hisFile` | \*_his.nc History file in NetCDF format. | `<empty>` |  |  |  |
| `hisInterval` | History output, given as 'interval' 'start period' 'end period'. | `0.0` |  | [s] |  |
| `xlsInterval` | Interval between XLS history. | `0.0` |  | [s] |  |
| `mapFile` | \*_map.nc Map file in NetCDF format. | `<empty>` |  |  |  |
| `mapInterval` | Map file output, given as 'interval' 'start period' 'end period'. | `0.0` |  | [s] |  |
| `rstInterval` | Restart file output, given as 'interval' 'start period' 'end period'. | `0.0` |  | [s] |  |
| `comInterval` | Comfile write times, given as 'interval' 'start period' 'end period' w.r.t. `refDate`. | `300.0` |  | [s] |  |
| `mapFormat` | Map file format. | `4` | <ul><li>**`1`**: NetCDF</li><li>**`4`**: NetCDF UGRID</li></ul> |  |  |
| `ncFormat` | Format for all NetCDF output files. | `3` | <ul><li>**`3`**: classic</li><li>**`4`**: NetCDF4+HDF5</li></ul> |  |  |
| `ncMapDataPrecision` | Precision for NetCDF data in map files (double or single). | `single` | <ul><li>**`double`**: double precision</li><li>**`single`**: single precision</li></ul> |  |  |
| `ncHisDataPrecision` | Precision for NetCDF data in his files (double or single). | `single` | <ul><li>**`double`**: double precision</li><li>**`single`**: single precision</li></ul> |  |  |
| `ncCompression` | Apply compression to NetCDF output files. Only works when `ncFormat`=4. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `ncNoUnlimited` | Write full-length time-dimension instead of unlimited dimension. Might require `ncFormat`=4. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `ncNoForcedFlush` | Do not force flushing of map-like files every output timestep. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `ncWriteLatLon` | Write extra lat-lon coordinates for all projected coordinate variables in each NetCDF file (for CF-compliancy). | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_balance` | Write mass balance totals to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_gen` | Write general structure parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_dam` | Write dam parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_pump` | Write pump parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_gate` | Write gate parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_weir` | Write weir parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_orifice` | Write orifice parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_bridge` | Write bridge parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_culvert` | Write culvert parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_longculvert` | Write long culvert parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_damBreak` | Write dam break parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_uniWeir` | Write universal weir parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_structure_compound` | Write compound structure parameters to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_lateral` | Write lateral data. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_velocity` | Write velocity magnitude in observation point to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_discharge` | Write discharge magnitude in observation point to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_sourcesink` | Write sources-sinks data to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_bubblescreens` | Write bubblescreens data to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_turbulence` | Write k, eps and vicww to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_windstress` | Write wind stress to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_wind` | Write wind velocities to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_rain` | Write precipitation to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_airdensity` | Write air density to his file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_infiltration` | Write infiltration to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_temperature` | Write temperature to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_waves` | Write wave data to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_heat_fluxes` | Write heat fluxes to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_salinity` | Write salinity to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_density` | Write density to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_waterlevel_s1` | Write water level to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_bedlevel` | Write bed level to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_waterdepth` | Write water depth to his file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_velocity_vector` | Write velocity vectors to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_upward_velocity_component` | Write upward velocity to his file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_sediment` | Write sediment transport to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_constituents` | Write tracers to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_zcor` | Write vertical coordinates to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_taucurrent` | Write mean bed shear stress to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_wqBot` | Write water quality bottom variables to his file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_wqBot3d` | Write 3D water quality bottom variables to his file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_waterlevel_s0` | Write water levels at old time level to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_waterlevel_s1` | Write water levels at new time level to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_evaporation` | Write evaporation to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_velocity_component_u0` | Write velocities at old time level to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_velocity_component_u1` | Write velocities at new time level to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_velocity_vector` | Write cell-center velocity vectors to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_upward_velocity_component` | Write upward velocity component to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_density_rho` | Write density to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_horizontal_viscosity_viu` | Write horizontal viscosity to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_horizontal_diffusivity_diu` | Write horizontal diffusivity to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_flow_flux_q1` | Write fluxes to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_spiral_flow` | Write spiral flow to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_numlimdt` | Write numlimdt to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriXyz_numlimdt` | Write numlimdt to xyz file This option is useful when a map file is not written. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_taucurrent` | Write bottom friction to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_chezy` | Write chezy roughness in flow elements to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_chezy_on_flow_links` | Write chezy roughness on flow links to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `writePart_domain` | Write interpreted network file (DFM_interpreted_idomain_\*.nc) containing domain partition info. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `velocityDirectionClassesInterval` | Class map's step size of class values for velocity direction. | `0.0` |  |  |  |
| `velocityMagnitudeClasses` | Class map's list of class values for velocity magnitudes. | `<empty>` |  |  |  |
| `wriMap_input_roughness` | Write chezy input roughness on flow links to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_turbulence` | Write turbulence to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_rain` | Write rainfall rate to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_wind` | Write winds to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_airdensity` | Write air density to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_calibration` | Write roughness calibration factors to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_salinity` | Write salinity to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_temperature` | Write temperature to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_constituents` | Write tracers and others constituents to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_sediment` | Write sediment transport to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_waves` | Write wave variables to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_z0` | Write current-related roughness height to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_trachytopes` | Write roughness from trachytopes to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_nudging` | Write nudging to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriTek_cdWind` | Write wind friction coefficients to tek file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_heat_fluxes` | Write heat fluxes to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_fixed_weir_energy_loss` | Write energy losses of fixed weirs to map file. `wriMap_waterdepth_on_ground` and `wriMap_volume_on_ground`. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_wet_waterdepth_threshold` | Waterdepth threshold above which a grid point counts as 'wet'. Defaults to 0.2\*`epshu`. It is used for `wriMap_time_water_on_ground`, `wriMap_waterdepth_on_ground` and `wriMap_volume_on_ground`. | `2e-05` |  |  |  |
| `wriMap_time_water_on_ground` | Write cumulative time when water is above ground level (only for 1D nodes) to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_freeboard` | Write freeboard (only for 1D nodes) to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_waterdepth_on_ground` | Write waterdepth that is above ground level to map file (only for 1D nodes). | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_volume_on_ground` | Write volume that is above ground level to map file (only for 1D nodes). | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_total_net_inflow_1d2d` | Write current total 1D2D net inflow (discharge) and cumulative total 1D2D net inflow (volume) to map file (only for 1D nodes). | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_total_net_inflow_lateral` | Write current total lateral net inflow (discharge) and cumulative total lateral net inflow (volume) to map file (only for 1D nodes). | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_water_level_gradient` | Write water level gradient to map file (only for 1D links). | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_tidal_potential` | Write tidal potential to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_sal_potential` | Write self attraction and loading potential to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_internal_tides_dissipation` | Write internal tides dissipation to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_flow_analysis` | Write flow analysis data to the map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_volume1` | Write volumes to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_waterdepth` | Write water depths to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_waterdepth_hu` | Write water depths on u-points to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_ancillary_variables` | Write ancillary variables attributes to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_flowarea_au` | Write low areas au to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_velocity_magnitude` | Write cell-center velocity vector magnitude to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_velocity_vectorq` | Write cell-center velocity vectors (discharge-based) to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_flow_flux_q1_main` | Write flow flux in main channel to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_interception` | Write interception to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_windstress` | Write wind stress to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_cdWind` | Write wind friction coeffs to map file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_bnd` | Write boundary points to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_Qin` | Write sum of all influxes to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_dtCell` | Write time step per cell based on CFL. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_wqBot3d` | Write 3D water quality bottom variables to map file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriMap_every_dt` | Write output to map file every computational timestep, between start and stop time from `mapInterval`. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `mapOutputTimeVector` | File (.mpt) containing fixed map output times (s) w.r.t. `refDate`. | `<empty>` |  |  |  |
| `comOutputTimeVector` | File (.ctv) containing fixed comfile write times (s) w.r.t. `refDate`. | `<empty>` |  |  |  |
| `fullGridOutput` | Full grid output mode for layer positions. | `0` | <ul><li>**`0`**: compact</li><li>**`1`**: full time-varying grid layer data</li></ul> |  |  |
| `eulerVelocities` | Write Eulerian velocities. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `avgWaveOutputInterval` | Average wave output, given as 'interval' 'start period' 'end period'. | `0.0` |  | [s] |  |
| `classMapFile` | Name of class map file. | `<empty>` |  |  |  |
| `waterLevelClasses` | Series of values between which water level classes are computed. | `<empty>` |  |  |  |
| `waterDepthClasses` | Series of values between which water depth classes are computed. | `<empty>` |  |  |  |
| `classMapInterval` | Interval between class map file outputs, given as 'interval' 'start period' 'end period'. | `0.0` |  | [s] |  |
| `waqInterval` | Interval between DELWAQ file outputs, given as 'interval' 'start period' 'end period'. | `0.0` |  | [s] |  |
| `statsInterval` | Interval between screen step outputs in seconds simulation time, if negative in seconds wall clock time. | `-60.0` |  | [s] |  |
| `timingsInterval` | Timings output interval. | `0.0` |  | [s] |  |
| `richardsonOnOutput` | Write Richardson number. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `mbaLumpSourceSinks` 🔬 | Lump MBA source/sink mass balance terms. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrimap_nearfield` 🔬 | Write NearField parameters. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `writeDfmInterpretedValues` 🔬 | Write DFM interpreted values. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `mbaLumpBoundaries` 🔬 | Lump MBA boundary mass balance terms. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `waqHorAggr` 🔬 | DELWAQ output horizontal aggregation file (\*.dwq). | `<empty>` |  |  |  |
| `writeDetailedTimers` 🔬 | Write detailed timers output file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `metadataFile` 🔬 | Metadata NetCDF file with user-defined global dataset attributes (\*_meta.nc). | `<empty>` |  |  |  |
| `mbaFile` | Space-separated input mass balance area file(s) (\*_mba.ini). | `<empty>` |  |  |  |
| `mbaInterval` 🔬 | Mass balance area output interval. | `0.0` |  | [s] |  |
| `wrirst_bnd` 🔬 | Write water level. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `generateUuid` 🔬 | Generate UUID as unique dataset identifier and include in output NetCDF files. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `timeSplitInterval` 🔬 | Time splitting interval after which a new output file is started. Format: value+unit, e.g. '1M'. | `<empty>` |  | [Y, M, D, h, m, s] |  |
| `rugFile` 🔬 | Polyline file \*_rug.pli defining runup gauges. | `<empty>` |  |  |  |
| `mbaWriteCsv` 🔬 | Write mass balance area output to a CSV file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `mbaLumpFromToMba` 🔬 | Lump MBA from/to other areas mass balance terms. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `mbaLumpProcesses` 🔬 | Lump MBA processes mass balance terms. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `waqVertAggr` 🔬 | DELWAQ output vertical aggregation file (\*.vag). | `<empty>` |  |  |  |
| `mbaWriteNetcdf` 🔬 | Write mass balance area output to a NetCDF file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `mbaWriteTxt` 🔬 | Write mass balance area output to a TXT file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wrimap_ice` 🔬 | Write output to map file for ice cover. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `enableDebugArrays` | Enable debug arrays in output. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_crs_flow` | Write cross-section flow output to history file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_crs_constituents` | Write cross-section constituents output to history file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_water_quality_output` | Write water quality output to history file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_runupgauge` | Write runup gauge output to history file. | `1` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |


<a id="supported-keywords-and-options-calibration"></a>
## [calibration]

This section contains roughness calibration settings.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `useCalibration` 🔬 | Activate calibration factor friction multiplier. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `definitionFile` 🔬 | File (\*.cld) containing calibration definitions. | `<empty>` |  |  |  |
| `areaFile` 🔬 | File (\*.cll) containing area distribution of calibration definitions. | `<empty>` |  |  |  |


<a id="supported-keywords-and-options-processes"></a>
## [processes]

This section settings for online water quality processes.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `substanceFile` 🔬 | Substance file name. | `<empty>` |  |  |  |
| `substanceDensityCoupling` 🔬 | Substance rho coupling. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `additionalHistoryOutputFile` 🔬 | Extra history output filename. | `<empty>` |  |  |  |
| `statisticsFile` 🔬 | Statistics definition file. | `<empty>` |  |  |  |
| `thetaVertical` ⚠️ | Theta value for vertical transport of water quality substances. | `0.0` |  | [-] |  |
| `dtProcesses` 🔬 | Waq processes time step. Must be a multiple of `dtUser`. If `dtProcesses` is negative, water quality processes are calculated with every hydrodynamic time step. | `0.0` |  | [s] |  |
| `volumeDryThreshold` 🔬 | Volume below which segments are marked as dry. | `0.001` |  | [m³] |  |
| `depthDryThreshold` 🔬 | Water depth below which segments are marked as dry. | `0.001` |  | [m] |  |
| `dtMassBalance` ⚠️ | Mass balance area output interval | `0.0` |  | [s] |  |


<a id="supported-keywords-and-options-particles"></a>
## [particles]

This section contains settings for particle tracking.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `particlesFile` | Initial particle locations file (\*.xyz). | `<empty>` |  |  |  |
| `particlesReleaseFile` | Particles release file (\*.tim, 4 column). | `<empty>` |  |  |  |
| `addTracer` | Add tracer or not. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `startTime` | Start time (if > 0). | `0.0` |  | [s] |  |
| `timeStep` | Time step (if > 0) or every computational time step. | `0.0` |  | [s] |  |
| `3DType` | 3D velocity type. | `0` | <ul><li>**`0`**: depth averaged velocities</li><li>**`1`**: free surface/top layer velocities</li></ul> |  |  |


<a id="supported-keywords-and-options-veg"></a>
## [veg]

This section contains setting for a dynamic vegetation model.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `vegetationModelNr` 🔬 | Vegetation model nr. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: Baptist DFM</li></ul> |  |  |
| `clVeg` 🔬 | Stem distance factor. | `0.8` |  | [-] |  |
| `cdVeg` 🔬 | Stem Cd coefficient. | `0.7` |  | [-] |  |
| `cbVeg` 🔬 | Stem stiffness coefficient. | `0.0` |  | [-] |  |
| `rhoVeg` 🔬 | Stem Rho, if > 0, buoyant stick procedure. | `0.0` |  | [kg/m³] |  |
| `stemHeightStd` 🔬 | Stem height standard deviation fraction, e.g. 0.1. | `0.0` |  | [-] |  |
| `stemHeightConvention` 🔬 | Stem height convention. | `upward_from_bed` | <ul><li>**`upward_from_bed`**: Upward from bed</li><li>**`downward_from_surface`**: Downward from surface</li></ul> | [-] |  |
| `densVegMinBap` 🔬 | Minimum vegetation density in Baptist formula. Only in 2D. | `0.0` |  | [1/m²] |  |
| `expChiStem` 🔬 | TODO. | `0.0` |  | [-] |  |
| `expChiLeaf` 🔬 | TODO. | `0.0` |  | [-] |  |
| `uChiStem` 🔬 | TODO. | `0.0` |  | [m/s] |  |
| `uChiLeaf` 🔬 | TODO. | `0.0` |  | [m/s] |  |
| `areaLeaf` 🔬 | TODO. | `0.0` |  | [m²] |  |
| `cdLeaf` 🔬 | TODO. | `0.0` |  | [-] |  |


<a id="supported-keywords-and-options-ice"></a>
## [ice]

This section contains setting for the ice module.

| Keyword | Description | Default | Supported values | Unit | Format |
| ------- | ----------- | ------- | ---------------- | ---- | ------ |
| `wriHis_ice_default` | Default flag for writing ice cover quantities to his-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_ice_open_water_level` | Write water level of open water to his-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_ice_lower_surface_height` | Write lower surface height of ice cover to his-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_ice_surface_height` | Write upper surface height of ice cover to his-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_ice_area_fraction` | Write area fraction covered by ice to his-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_ice_thickness` | Write ice thickness to his-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_ice_pressure` | Write ice pressure to his-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_ice_temperature` | Write ice temperature to his-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_snow_thickness` | Write snow thickness to his-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |
| `wriHis_snow_temperature` | Write snow temperature to his-file. | `0` | <ul><li>**`0`**: no</li><li>**`1`**: yes</li></ul> |  |  |

<a id="research-keywords-and-options"></a>
# Research keywords and options

The keyword and options listed below are available for research purposes.


| Keyword/value | Notes |
| ------------- | ----- |
| `[calibration] areaFile` |  |
| `[calibration] definitionFile` |  |
| `[calibration] useCalibration` |  |
| `[general] inputSpecific` |  |
| `[general] modelSpecific` |  |
| `[geometry] 1D2DInternalLinkType` |  |
| `[geometry] bedWaveAmplitude` |  |
| `[geometry] bedWaveLength` |  |
| `[geometry] calculateBedLevelOverNonActiveLinks` |  |
| `[geometry] cosphiutrsh` |  |
| `[geometry] createLinks1D2D` |  |
| `[geometry] cutCellList` |  |
| `[geometry] dpuopt` |  |
| `[geometry] extrBl` |  |
| `[geometry] groundLayerThickness` |  |
| `[geometry] helmert` |  |
| `[geometry] ihuz` |  |
| `[geometry] ihuzcSig` |  |
| `[geometry] keepZLay1BedVol` |  |
| `[geometry] layerType=3` |  |
| `[geometry] layerType=4` |  |
| `[geometry] pipeFile` |  |
| `[geometry] removeSmallLinksTrsh` |  |
| `[geometry] shipDefFile` |  |
| `[geometry] stripMesh` |  |
| `[geometry] topLayMinThick` |  |
| `[geometry] tSigma` |  |
| `[geometry] uniformHu` |  |
| `[geometry] uniformTyp1D` |  |
| `[geometry] vertPlizFile` |  |
| `[geometry] waterDepthIni1D` |  |
| `[geometry] zLayerAtuByBob` |  |
| `[numerics] baOrgFracMin` |  |
| `[numerics] barrierAdvection` |  |
| `[numerics] cfConHorMom` |  |
| `[numerics] cfExpHorMom` |  |
| `[numerics] cfExpHu` |  |
| `[numerics] cfFacHorMom` |  |
| `[numerics] cfFacHu` |  |
| `[numerics] cfFacVer` |  |
| `[numerics] chkdifd` |  |
| `[numerics] corioAdamsBashfordFac` |  |
| `[numerics] corioConstant` |  |
| `[numerics] coriohhtrsh` |  |
| `[numerics] diffusionOnBnd` |  |
| `[numerics] drop3D` |  |
| `[numerics] dtol` |  |
| `[numerics] eddyViscosityBedFacmax` |  |
| `[numerics] epshstem` |  |
| `[numerics] filterOrder` |  |
| `[numerics] fixedWeirFrictScheme` |  |
| `[numerics] horAdvTypZLayer` |  |
| `[numerics] huWeirRegular` |  |
| `[numerics] iCoriolisType` |  |
| `[numerics] ilutype` |  |
| `[numerics] jarhoxu` |  |
| `[numerics] jaupwindsrc` |  |
| `[numerics] jbasqbnddownwindhs` |  |
| `[numerics] jposhchk` |  |
| `[numerics] keepSTBndOnOutflow` |  |
| `[numerics] lateral_fixedweir_minimal_1d2d_embankment` |  |
| `[numerics] lateral_fixedweir_relax` |  |
| `[numerics] lateral_fixedweir_umin_method` |  |
| `[numerics] limTypW` |  |
| `[numerics] linContin` |  |
| `[numerics] linkDriedMx` |  |
| `[numerics] locSaltLev` |  |
| `[numerics] locSaltMax` |  |
| `[numerics] locSaltMin` |  |
| `[numerics] logProfAtUBndIn` |  |
| `[numerics] logProfKepsBndIn` |  |
| `[numerics] minTimeStepBreak` |  |
| `[numerics] newCorio` |  |
| `[numerics] nlevel` |  |
| `[numerics] noDerivedTypes` |  |
| `[numerics] numlimdt_baorg` |  |
| `[numerics] pillarFarFieldVelocity` |  |
| `[numerics] rhoInterfaces` |  |
| `[numerics] structureLayersActive` |  |
| `[numerics] subsuplupdates1` |  |
| `[numerics] testDryingFlooding` |  |
| `[numerics] testFixedWeirs` |  |
| `[numerics] trsh_u1lb` |  |
| `[numerics] turbulenceAdvection` |  |
| `[numerics] vertAdvTypMom3OnBnd` |  |
| `[numerics] zLayerCenterBedVel` |  |
| `[numerics] zwsbTol` |  |
| `[output] generateUuid` |  |
| `[output] mbaInterval` |  |
| `[output] mbaLumpBoundaries` |  |
| `[output] mbaLumpFromToMba` |  |
| `[output] mbaLumpProcesses` |  |
| `[output] mbaLumpSourceSinks` |  |
| `[output] mbaWriteCsv` |  |
| `[output] mbaWriteNetcdf` |  |
| `[output] mbaWriteTxt` |  |
| `[output] metadataFile` |  |
| `[output] rugFile` |  |
| `[output] timeSplitInterval` |  |
| `[output] waqHorAggr` |  |
| `[output] waqVertAggr` |  |
| `[output] wrimap_ice` |  |
| `[output] wrimap_nearfield` |  |
| `[output] wrirst_bnd` |  |
| `[output] writeDetailedTimers` |  |
| `[output] writeDfmInterpretedValues` |  |
| `[physics] dRoot` |  |
| `[physics] dStem` |  |
| `[physics] dynRoughVeg` |  |
| `[physics] dynVegPol` |  |
| `[physics] EPSMin` |  |
| `[physics] equili` |  |
| `[physics] freeConvectionCoefficient` |  |
| `[physics] nfEntrainmentMomentum` |  |
| `[physics] prandtlNumberTemperature` |  |
| `[physics] schmidtNumberSalinity` |  |
| `[physics] schmidtNumberTracer` |  |
| `[physics] SecchiDepthNonPenetrative` |  |
| `[physics] SecchiDepthNonPenetrativeFraction` |  |
| `[physics] selfAttractionLoading` |  |
| `[physics] selfAttractionLoading_correct_wl_with_ini` |  |
| `[physics] soilTempThick` |  |
| `[physics] surfTempSmoFac` |  |
| `[physics] TAUMin` |  |
| `[physics] thermobaricityInPressureGradient` |  |
| `[physics] TKEMin` |  |
| `[physics] unifFrictCoef1D2D` |  |
| `[physics] uniffrictcoef1dgrlay` |  |
| `[physics] unifFrictCoefNoVeg` |  |
| `[processes] additionalHistoryOutputFile` |  |
| `[processes] depthDryThreshold` |  |
| `[processes] dtProcesses` |  |
| `[processes] statisticsFile` |  |
| `[processes] substanceDensityCoupling` |  |
| `[processes] substanceFile` |  |
| `[processes] volumeDryThreshold` |  |
| `[restart] rstIgnoreBl` |  |
| `[sediment] implicitFallVelocity` |  |
| `[sediment] mxgrkrone` |  |
| `[sediment] nr_of_sedfractions` |  |
| `[sediment] sedDensCoupling` |  |
| `[sedtrails] sedtrailsAnalysis` |  |
| `[sedtrails] sedTrailsGrid` |  |
| `[sedtrails] sedtrailsInterval` |  |
| `[sedtrails] sedtrailsOutputFile` |  |
| `[time] autoTimeStepVisc` |  |
| `[time] timeStepAnalysis` |  |
| `[trachytopes] trtCll` |  |
| `[trachytopes] trtMnh` |  |
| `[trachytopes] trtMth` |  |
| `[veg] areaLeaf` |  |
| `[veg] cbVeg` |  |
| `[veg] cdLeaf` |  |
| `[veg] cdVeg` |  |
| `[veg] clVeg` |  |
| `[veg] densVegMinBap` |  |
| `[veg] expChiLeaf` |  |
| `[veg] expChiStem` |  |
| `[veg] rhoVeg` |  |
| `[veg] stemHeightConvention` |  |
| `[veg] stemHeightStd` |  |
| `[veg] uChiLeaf` |  |
| `[veg] uChiStem` |  |
| `[veg] vegetationModelNr` |  |
| `[waves] 3DStokesProfile` |  |
| `[waves] 3DWaveBoundaryLayer` |  |
| `[waves] 3DWaveStreaming` |  |
| `[waves] 3Dwaveturbpendepth` |  |
| `[waves] fbreak` |  |
| `[waves] fforc` |  |
| `[waves] ftauw` |  |
| `[waves] fwfac` |  |
| `[waves] hminlw` |  |
| `[waves] hwavuni` |  |
| `[waves] jahissigwav` |  |
| `[waves] jamapsigwav` |  |
| `[waves] phiwavuni` |  |
| `[waves] streamLayerFac` |  |
| `[waves] tiFetchComp` |  |
| `[waves] twavuni` |  |
| `[waves] uorbfac` |  |
| `[waves] waveSwartDelwaq` |  |
| `[wind] varyingAirDensity` |  |
| `[wind] wind_eachstep` |  |
| `[wind] windHuOrZwsBased` |  |

<a id="deprecated-keywords-and-options"></a>
# Deprecated keywords and options

The keywords and options listed below are deprecated and will be removed in a future release. If your model still uses one of them, please update it as soon as possible by following the notes or user manual.


| Keyword/value | Notes |
| ------------- | ----- |
| `[external forcing] extForceFile` | Use `[external forcing] extForceFileNew` instead. Note: you should update some keywords and values in the file contents to the new format. See the User Manual for details. |
| `[general] autoStart` | Keyword will be removed in a future release. |
| `[general] convertLongCulverts` | Unconverted long culverts (convertLongCulverts=0) will be removed in a future release. |
| `[geometry] circumcenterMethod` | Once the keyword is removed/becomes obsolete, the `allNetlinksLoop` method will be used. |
| `[geometry] circumcenterTolerance` | Once the keyword is removed/becomes obsolete, a fixed tolerance will be used. |
| `[geometry] iniFieldFile` | Use `[external forcing] extForceFileNew` instead. Note: you should update some keywords and values in the file contents to the new format. See the User Manual for details. |
| `[geometry] orgFloorLevTopLayDef` | Use `[geometry] floorLevTopLay` instead. |
| `[geometry] sigmaGrowthFactor` | Use `[geometry] zLayerGrowthFactor` instead. |
| `[model]` | Section has been replaced with `[general]` section. |
| `[numerics] keepZLayeringAtBed` | Use `[geometry] keepZLayeringAtBed` instead. |
| `[numerics] maxItPresDens` | Keyword will be removed in a future release. |
| `[numerics] maxItVerticalForesterSal` | Use `[numerics] maxItVerticalForester` instead. |
| `[numerics] maxItVerticalForesterTem` | Use `[numerics] maxItVerticalForester` instead. |
| `[numerics] vertAdvTypSal` | Use `[numerics] verticalAdvectionType` instead. |
| `[numerics] vertAdvTypTem` | Use `[numerics] verticalAdvectionType` instead. |
| `[physics] jadelvappos` | Keyword will be removed in a future release. |
| `[physics] SecchiDepth2` | Use `[physics] SecchiDepthNonPenetrative` instead. |
| `[physics] SecchiDepth2Fraction` | Use `[physics] SecchiDepthNonPenetrativeFraction` instead. |
| `[processes] dtMassBalance` | Use `[output] MbaInterval` instead. |
| `[processes] thetaVertical` | Use `[numerics] verticalAdvectionType` instead. |

<a id="obsolete-keywords-and-options"></a>
# Obsolete keywords and options

The keywords and options listed below are no longer supported and will cause an error if used in D-Flow FM. They are grouped by the release in which they became obsolete, most recent first. If your model still uses one of them, please update it by following the notes or user manual.

<a id="release-2027.01"></a>
## Release 2027.01


| Keyword/value | Notes |
| ------------- | ----- |
| `[model] mduFormatVersion` | Use `[general] fileVersion` instead. |
| `[physics] umodLin` | Keyword no longer supported. |
| `[sediment] masBalMinDep` | Keyword no longer supported. |

<a id="release-2026.02"></a>
## Release 2026.02


| Keyword/value | Notes |
| ------------- | ----- |
| `[geometry] circumcenter` | Use `[geometry] circumcenterMethod` instead. |

<a id="release-2026.01"></a>
## Release 2026.01


| Keyword/value | Notes |
| ------------- | ----- |
| `[numerics] barocTerm` | Keyword no longer supported. |
| `[numerics] barocTimeInt` | Keyword no longer supported. |
| `[numerics] barocZLayBed` | Keyword no longer supported. |
| `[numerics] epsEPS` | Use `[physics] EPSMin` (k-epsilon turbulence model) or `[physics] TAUMin` (k-tau turbulence model) instead. |
| `[numerics] epsTKE` | Use `[physics] TKEMin` instead. |
| `[numerics] facLaxTurb` | Use `[numerics] turbulenceTimeIntegrationFactor` instead. |
| `[numerics] facLaxTurbHor` | Use `[numerics] turbulenceTimeIntegrationMethod` instead. |
| `[numerics] facLaxTurbVer` | Keyword no longer supported. |
| `[numerics] jaDrhoDz` | Keyword no longer supported. |
| `[numerics] orgBarocKeywords` | Keyword no longer supported. |
| `[output] mapFormat=2` | Option no longer supported. |
| `[output] mapFormat=3` | Option no longer supported. |
| `[physics] allowCoolingBelowZero` | Use `[physics] salinityDependentFreezingPoint` to allow cooling below zero degrees Celsius. |
| `[physics] rhoAirRhoWater` | Use `[wind] rhoWaterInWindStress` instead. |

<a id="release-2025.01"></a>
## Release 2025.01


| Keyword/value | Notes |
| ------------- | ----- |
| `[processes] wriWaqBot3dOutput` | Use `[output] wriHis_wqBot3d` and/or `[output] wriMap_wqBot3d` instead. |

<a id="release-2023.01"></a>
## Release 2023.01


| Keyword/value | Notes |
| ------------- | ----- |
| `[numerics] transportMethod` | Value 1 is the default and only available option, no further input needed. Value 2 for diagnostic transport is now under its own keyword. Use `[numerics] diagnosticTransport` keyword instead. |
| `[numerics] transportTimeStepping` | Keyword no longer supported. |

<a id="release-2022.02"></a>
## Release 2022.02


| Keyword/value | Notes |
| ------------- | ----- |
| `[geometry] 1dNetworkFile` | Use individual keywords such as `[geometry] structureFile`, `crossDefFile`, etc. instead. |
| `[geometry] bathymetryFile` | Use `[geometry] iniFieldFile` instead. That keyword should point to a file which contains a reference to the bed level samples file, which was originally referenced by this keyword. See manual for more information. |
| `[geometry] bedLevelFile` | Use `[geometry] iniFieldFile` instead. That keyword should point to a file which contains a reference to the bed level samples file, which was originally referenced by this keyword. See manual for more information. |
| `[geometry] botLevType` | Use `[geometry] bedLevType` instead. |
| `[geometry] botLevUni` | Use `[geometry] bedLevUni` instead. |
| `[geometry] Grdang` | Keyword no longer supported. |
| `[geometry] iThinDykeScheme` | Use `[numerics] fixedWeirScheme` instead. |
| `[geometry] manholeFile` | Use `[geometry] storageNodeFile` instead. |
| `[geometry] nodeFile` | Use `[geometry] storageNodeFile` instead. |
| `[geometry] noOptimizedPolygon` | Keyword no longer supported. |
| `[geometry] roughnessFiles` | Use `[geometry] frictFile` instead. |
| `[geometry] thinDykeFile` | Use `[geometry] fixedWeirFile` instead. |
| `[numerics] CFLWaveFrac` | Keyword no longer supported. |
| `[numerics] filter` | Option no longer supported. |
| `[numerics] hkad` | Keyword no longer supported. |
| `[numerics] iThinDykeScheme` | Use `[numerics] fixedWeirScheme` instead. |
| `[numerics] jaEmbed1d` | Keyword no longer supported. |
| `[numerics] jaOrgSethu` | Keyword no longer supported. |
| `[numerics] qhRelax` | Option no longer needed due to improved semi-implicit algorithm. |
| `[numerics] relax` | Use `[numerics] lateral_fixedweir_relax` instead. |
| `[numerics] sobekdfm_minimal_1d2d_embankment` | Use `[numerics] lateral_fixedweir_minimal_1d2d_embankment` instead. |
| `[numerics] sobekdfm_relax` | Use `[numerics] lateral_fixedweir_relax` instead. |
| `[numerics] sobekdfm_umin` | Use `[numerics] lateral_fixedweir_umin` instead. |
| `[numerics] sobekdfm_umin_method` | Use `[numerics] lateral_fixedweir_umin_method` instead. |
| `[numerics] thinDykeContraction` | Use `[numerics] fixedWeirContraction` instead. |
| `[numerics] wridia_viscosity_diffusivity_limit` | Keyword no longer supported. |
| `[output] heatFluxesOnOutput` | Use `[output] wriHis_heat_fluxes` and `[output] wriMap_heat_fluxes` instead. |
| `[output] s1incInterval` | Use `[output] classMapFile` instead. |
| `[output] snapshotdir` | Keyword no longer supported. |
| `[output] waqFileBase` | Keyword no longer supported. |
| `[output] wrihis_heatflux` | Renamed to `[output] wriHis_heat_fluxes`. |
| `[output] wrimap_input_dt` | Keyword no longer supported. |
| `[output] wrishp_enc` | Use `[output] wrishp_dryarea` instead. |
| `[output] writeBalanceFile` | Keyword no longer supported. Superseded by default information included in the history file. |
| `[physics] effectSpiral` | Use `Espir` contained in `MorFile` instead. |
| `[physics] stericCorrection` | Keyword no longer supported. |
| `[processes] processFluxIntegration` | Process fluxes integration is always done by WAQ. |
| `[time] autoTimeStepDiff` | Use `[time] autoTimeStepVisc` instead. |
| `[trachytopes] trtdt` | Use `[trachytopes] dtTrt` instead. |
| `[waves] waveNikuradse` | Keyword no longer supported. |
| `[wind] gapres` | Keyword no longer supported. |