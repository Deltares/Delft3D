#include <dflowfm_io/MduSchema.h>

// This file is generated from mdu.json. Manual edits will be lost.

namespace dflowfm_io
{

    const MduSchema MDU_SCHEMA {
        .description = "The master definition file of D-Flow FM",
        .sections = {
            SectionSchema {
                .name        = "general",
                .required    = true,
                .description = "This section contains the program name and its version.",
                .properties  = {
                    PropertySchema {
                        .key           = "program",
                        .required      = false,
                        .value_type    = ValueType::String,
                        .default_value = "D-Flow FM",
                        .description   = "Program."
                    },
                    PropertySchema {
                        .key           = "version",
                        .required      = false,
                        .value_type    = ValueType::String,
                        .default_value = "1.2.60.64623M",
                        .description   = "Version number of computational kernel."
                    },
                    PropertySchema {
                        .key           = "fileType",
                        .required      = true,
                        .value_type    = ValueType::Enum,
                        .default_value = "modelDef",
                        .enum_values   = {
                            {0, "modelDef"}
                        },
                        .description   = "File type. Do not edit this."
                    },
                    PropertySchema {
                        .key           = "fileVersion",
                        .required      = true,
                        .value_type    = ValueType::String,
                        .default_value = "1.09",
                        .description   = "File version. Do not edit this."
                    },
                    PropertySchema {
                        .key           = "guiVersion",
                        .required      = false,
                        .value_type    = ValueType::String,
                        .description   = "Version number of GUI."
                    },
                    PropertySchema {
                        .key           = "autoStart",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "no"},
                            {1, "autostart"},
                            {2, "autostartstop"}
                        },
                        .description   = "Autostart simulation after loading MDU or not."
                    },
                    PropertySchema {
                        .key           = "pathsRelativeToParent",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Resolve file names (e.g. inside the *.ext file) relative to their direct parent, instead of to the toplevel MDU working dir."
                    },
                    PropertySchema {
                        .key           = "modelSpecific",
                        .required      = false,
                        .value_type    = ValueType::String,
                        .description   = "Optional 'model specific ID', to enable certain custom runtime function calls (instead of via MDU name)."
                    },
                    PropertySchema {
                        .key           = "inputSpecific",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Use of hardcoded specific inputs, shall not be used by users."
                    }
                }
            },
            SectionSchema {
                .name        = "geometry",
                .required    = true,
                .description = "In this section, the main entry comprises the specification of the grid (i.e. the netCDF network file). In addition, thin dams and thin dykes can be specified.",
                .properties  = {
                    PropertySchema {
                        .key           = "netFile",
                        .required      = true,
                        .value_type    = ValueType::Path,
                        .description   = "Net file (*_net.nc) containing mesh information."
                    },
                    PropertySchema {
                        .key           = "dryPointsFile",
                        .required      = false,
                        .value_type    = ValueType::PathList,
                        .description   = "Dry points file (*.xyz), third column dummy z values, or polygon file (*.pol)."
                    },
                    PropertySchema {
                        .key           = "gridEnclosureFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Enclosure file (*.pol) to clip outer parts from the grid."
                    },
                    PropertySchema {
                        .key           = "structureFile",
                        .required      = false,
                        .value_type    = ValueType::PathList,
                        .description   = "File (*.ini) containing list of hydraulic structures. Supports multiple filenames separated by spaces. Filenames containing spaces must be placed inside double quotes."
                    },
                    PropertySchema {
                        .key           = "gulliesFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Polyline file (*_gul.pliz), containing lowest bed level along talweg x, y, z level."
                    },
                    PropertySchema {
                        .key           = "roofsFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Polyline file (*_roof.pliz), containing roofgutter heights x, y, z level."
                    },
                    PropertySchema {
                        .key           = "iniFieldFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Initial and parameter field file (*.ini)."
                    },
                    PropertySchema {
                        .key           = "waterLevIniFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Initial water levels sample file (*.xyz)."
                    },
                    PropertySchema {
                        .key           = "landBoundaryFile",
                        .required      = false,
                        .value_type    = ValueType::PathList,
                        .description   = "Land boundary file (*.ldb), only used for plotting."
                    },
                    PropertySchema {
                        .key           = "thinDamFile",
                        .required      = false,
                        .value_type    = ValueType::PathList,
                        .description   = "Polyline file (*_thd.pli), containing polyline(s) for tracing thin dams."
                    },
                    PropertySchema {
                        .key           = "fixedWeirFile",
                        .required      = false,
                        .value_type    = ValueType::PathList,
                        .description   = "Polyline file (*_fxw.pliz), containing polyline(s) with x, y, z where z = fixed weir top levels (formerly fixed weir)."
                    },
                    PropertySchema {
                        .key           = "pillarFile",
                        .required      = false,
                        .value_type    = ValueType::PathList,
                        .description   = "Polyline file (*_pillar.pliz), containing four colums with x, y, diameter and Cd coefficient for bridge pillars."
                    },
                    PropertySchema {
                        .key           = "useCaching",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Use caching for geometrical/network-related items."
                    },
                    PropertySchema {
                        .key           = "vertPlizFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Polyline file (*_vlay.pliz), containing x, y, Z where first Z = nr of layers, second Z = laytyp. For `layerType` = 3 (mixed layering in polygon regions)."
                    },
                    PropertySchema {
                        .key           = "frictFile",
                        .required      = false,
                        .value_type    = ValueType::PathList,
                        .description   = "Files with roughness data for 1D (space separated)."
                    },
                    PropertySchema {
                        .key           = "crossDefFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File containing the cross section definitions for all cross section shapes."
                    },
                    PropertySchema {
                        .key           = "crossLocFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File containing the location definitions of the cross sections on a 1D network."
                    },
                    PropertySchema {
                        .key           = "storageNodeFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File containing the specification of storage nodes and/or manholes to add extra storage to 1D models."
                    },
                    PropertySchema {
                        .key           = "1D2DLinkFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File containing the custom parameterization of 1D-2D links."
                    },
                    PropertySchema {
                        .key           = "allowBndAtBifurcation",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Allow 1D boundary node when connecting branch leads to bifurcation."
                    },
                    PropertySchema {
                        .key           = "profLocFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "(*_proflocation.xyz) x, y, z, z = profile refnumber."
                    },
                    PropertySchema {
                        .key           = "profDefFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "(*_profdefinition.def) definition for all profile nrs."
                    },
                    PropertySchema {
                        .key           = "profDefXyzFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "(*_profdefinition.def) definition for all profile nrs."
                    },
                    PropertySchema {
                        .key           = "partitionFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "(*_part.pol), polyline(s) x, y."
                    },
                    PropertySchema {
                        .key           = "dxWuiMin2D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Smallest fraction dx/wu , set dx > `dxWuiMin2D`*wu."
                    },
                    PropertySchema {
                        .key           = "waterLevIni",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Initial water level."
                    },
                    PropertySchema {
                        .key           = "bedLevUni",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-5.0",
                        .description   = "Uniform bed level (only if `bedLevType`>=3), used at missing z-values in `netFile`."
                    },
                    PropertySchema {
                        .key           = "bedSlope",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Bed slope inclination, sets zk = `bedLevUni` + x*`bedSlope` and sets zbndz = xbndz*`bedSlope`."
                    },
                    PropertySchema {
                        .key           = "bedLevType",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "3",
                        .enum_values   = {
                            {1, "at cell center (tiles xz, yz, bl, bob=max(bl))"},
                            {2, "at face (tiles xu, yu, blu, bob=blu)"},
                            {3, "at face (using mean node values)"},
                            {4, "at face (using min node values)"},
                            {5, "at face (using max node values)"},
                            {6, "with bl based on node values"}
                        },
                        .description   = "Bed level definition type."
                    },
                    PropertySchema {
                        .key           = "blMeanBelow",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "If not -999.0, below this level the cell centre bedlevel is the mean of surrouding netnodes."
                    },
                    PropertySchema {
                        .key           = "blMinAbove",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "If not -999.0, above this level the cell centre bedlevel is the min of surrouding netnodes."
                    },
                    PropertySchema {
                        .key           = "angLat",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Angle of latitude S-N, 0=on Equator (and thus no Coriolis force). Only required for Coriolis on Cartesian grids and for heat flux modelling."
                    },
                    PropertySchema {
                        .key           = "angLon",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Angle of longitude E-W, 0=Greenwich Mean Time. Only required for heat flux modelling."
                    },
                    PropertySchema {
                        .key           = "conveyance2D",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "-1",
                        .enum_values   = {
                            {-1, "R=HU"},
                            {0, "R=H"},
                            {1, "R=A/P"},
                            {2, "K=analytic-1D conv"},
                            {3, "K=analytic-2D conv"}
                        },
                        .description   = "2D analytic conveyance description."
                    },
                    PropertySchema {
                        .key           = "nonLin1D",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {1, "treat closed sections as partially open by using a Preissmann slot"},
                            {2, "Nested Newton approach"},
                            {3, "partial Nested Newton approach"}
                        },
                        .description   = "Non-linear 1D volumes, applicable for models with closed cross sections."
                    },
                    PropertySchema {
                        .key           = "nonLin2D",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Non-linear 2D volumes, only i.c.m. `bedLevType`=3 and `conveyance2D`>=1."
                    },
                    PropertySchema {
                        .key           = "slotw1D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.001",
                        .description   = "Minimum slotwidth 1D."
                    },
                    PropertySchema {
                        .key           = "slotw2D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.001",
                        .description   = "Minimum slotwidth 2D."
                    },
                    PropertySchema {
                        .key           = "uniformWidth1D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "2.0",
                        .description   = "Uniform width for 1D profiles and 1D2D internal links."
                    },
                    PropertySchema {
                        .key           = "uniformHeight1D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "3.0",
                        .description   = "Uniform height for 1D profiles and 1D2D internal links."
                    },
                    PropertySchema {
                        .key           = "uniformWidth1DStreetInlets",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.2",
                        .description   = "Uniform width for street inlets."
                    },
                    PropertySchema {
                        .key           = "uniformHeight1DStreetInlets",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.1",
                        .description   = "Uniform height for street inlets."
                    },
                    PropertySchema {
                        .key           = "uniformTyp1DStreetInlets",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "-2",
                        .enum_values   = {
                            {1, "circle"},
                            {2, "rectangle"},
                            {-2, "closed rectangle"}
                        },
                        .description   = "Uniform cross section type for street inlets."
                    },
                    PropertySchema {
                        .key           = "uniformWidth1DRoofGutterPipes",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.1",
                        .description   = "Uniform width for roof gutter pipes."
                    },
                    PropertySchema {
                        .key           = "uniformHeight1DRoofGutterPipes",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.1",
                        .description   = "Uniform height for roof gutter pipes."
                    },
                    PropertySchema {
                        .key           = "uniformTyp1DRoofGutterPipes",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "-2",
                        .enum_values   = {
                            {1, "circle"},
                            {2, "rectangle"},
                            {-2, "closed rectangle"}
                        },
                        .description   = "Uniform cross section type for type roof gutter pipes."
                    },
                    PropertySchema {
                        .key           = "sillHeightMin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Fixed weir only active if both ground heights are larger than this value."
                    },
                    PropertySchema {
                        .key           = "makeOrthoCenters",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Switch from circumcentres to orthocenters in geominit."
                    },
                    PropertySchema {
                        .key           = "dCenterInside",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.0",
                        .description   = "Limit cell center; 1.0:in cell <-> 0.0:on c/g."
                    },
                    PropertySchema {
                        .key           = "circumcenterMethod",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .default_value = "internalNetlinksEdge",
                        .enum_values   = {
                            {0, "internalNetlinksEdge"},
                            {1, "internalNetlinksLoop"},
                            {2, "allNetlinksLoop"}
                        },
                        .description   = "Circumcenter computation method."
                    },
                    PropertySchema {
                        .key           = "circumcenterTolerance",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.001",
                        .description   = "Tolerance for convergence of circumcenter method."
                    },
                    PropertySchema {
                        .key           = "baMin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1e-06",
                        .description   = "Minimum grid cell area, i.c.m. cutcells."
                    },
                    PropertySchema {
                        .key           = "openBoundaryTolerance",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "3.0",
                        .description   = "Search tolerance factor between boundary polyline and grid cells."
                    },
                    PropertySchema {
                        .key           = "renumberFlowNodes",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Renumber the flow nodes."
                    },
                    PropertySchema {
                        .key           = "kmx",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "0",
                        .description   = "Number of vertical layers. NB. If keyword `zLayerGrowthFactor` is used, then number of layers is determined by D-Flow FM."
                    },
                    PropertySchema {
                        .key           = "layerType",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {1, "sigma-layers"},
                            {2, "z- or z-sigma-layers"}
                        },
                        .description   = "Vertical layer type."
                    },
                    PropertySchema {
                        .key           = "numTopSig",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "0",
                        .description   = "Number of sigma-layers on top of z-layers in case of z-sigma-layers."
                    },
                    PropertySchema {
                        .key           = "sigmaGrowthFactor",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.0",
                        .description   = "Growth factor of z-Layer thickness starting below the level specified by `dzTopUniAboveZ` till the bed."
                    },
                    PropertySchema {
                        .key           = "zLayerGrowthFactor",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.0",
                        .description   = "Growth factor of z-Layer thickness starting below the level specified by `dzTopUniAboveZ` till the bed."
                    },
                    PropertySchema {
                        .key           = "floorLevTopLay",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "The floor level of the top layer."
                    },
                    PropertySchema {
                        .key           = "dzTop",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "z-layer thickness of layers above level `dzTopUniAboveZ`."
                    },
                    PropertySchema {
                        .key           = "dzTopUniAboveZ",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "The level above which the layers will have uniform thickness of `dzTop`."
                    },
                    PropertySchema {
                        .key           = "numTopSigUniform",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {0, "decreasing"},
                            {1, "constant"}
                        },
                        .description   = "The number of sigma-layers in a z-sigma-model is constant or decreasing (depending on local depth)."
                    },
                    PropertySchema {
                        .key           = "zLayBot",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "-999",
                        .description   = "If specified, first z-layer starts from `zLayBot`, if not, it starts from the lowest bed point."
                    },
                    PropertySchema {
                        .key           = "zLayTop",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "-999",
                        .description   = "If specified, highest z-layer starts from `zLayTop`, if not, it ends at the initial water level."
                    },
                    PropertySchema {
                        .key           = "stretchType",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "-1",
                        .enum_values   = {
                            {-1, "uniform"},
                            {1, "user defined"},
                            {2, "exponential"}
                        },
                        .description   = "Stretching type for non-uniform layers."
                    },
                    PropertySchema {
                        .key           = "stretchCoef",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .description   = "Coefficients for sigma layer. For `stretchType`=1: percentages of the layers, user defined, laycof(`kmx`). For `stretchType`=2: Stretching level, and two coefficients for layer growth, laycof(3)."
                    },
                    PropertySchema {
                        .key           = "dxMin1D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.001",
                        .description   = "Minimum 1D link length."
                    },
                    PropertySchema {
                        .key           = "dxDoubleAt1DEndNodes",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Extend a 1D grid cell at the end of a network with 0.5∆x."
                    },
                    PropertySchema {
                        .key           = "changeVelocityAtStructures",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Ignore structure dimensions for the velocity at hydraulic structures, when calculating the surrounding cell centered flow velocities."
                    },
                    PropertySchema {
                        .key           = "changeStructureDimensions",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Change the structure dimensions in case these are inconsistent with the channel dimensions. ⋄ weirs, orifices, general structures: 1. In case the crest width exceeds the surface width, the crest width is set to the surface width; 2. In case the crest level is lower than the bed level, the crest level is set to the bed level. ⋄ bridges: 1. In case the crest width exceeds the surface width, the crest width is set to the surface width; 2. In case the flow area of the bridge exceeds the upstream flow area the flow area of the bridge is set to the upstream flow area. ⋄ universal weirs: only the crest level is checked and changed. NOTE: It is strongly advised not to change this parameter (true). Since turning this option off can lead to instabilities and unrealistic results."
                    },
                    PropertySchema {
                        .key           = "calculateBedLevelOverNonActiveLinks",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Specifies whether the bed levels must be computed over all links of a cell including the closed boundaries, thin dams and dry points (= 0), or only the flow links. (= 1)"
                    },
                    PropertySchema {
                        .key           = "stripMesh",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Strip unused nodes and links from the mesh after clipping."
                    },
                    PropertySchema {
                        .key           = "topLayMinThick",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Minimum top layer thickness, only for Z-layers."
                    },
                    PropertySchema {
                        .key           = "helmert",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Use Helmert."
                    },
                    PropertySchema {
                        .key           = "waterDepthIni1D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Initial waterdepth in 1D."
                    },
                    PropertySchema {
                        .key           = "zLayerAtuByBob",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Lowest connected cells governed by bob instead of by bL L/R."
                    },
                    PropertySchema {
                        .key           = "shipDefFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File *.shd containing ship definitions."
                    },
                    PropertySchema {
                        .key           = "bedWaveLength",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Bed testcases."
                    },
                    PropertySchema {
                        .key           = "removeSmallLinksTrsh",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Remove small links."
                    },
                    PropertySchema {
                        .key           = "createLinks1D2D",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Rashly create links between 1D nodes and 2D cells when initializing model."
                    },
                    PropertySchema {
                        .key           = "bedWaveAmplitude",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Bed testcases."
                    },
                    PropertySchema {
                        .key           = "uniformHu",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Waterdepth in rigid-lid-like solution."
                    },
                    PropertySchema {
                        .key           = "tSigma",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Sigma adaptation period for `layerType` = 4 (density controlled sigma-layers)."
                    },
                    PropertySchema {
                        .key           = "dpuopt",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {1, "max"},
                            {2, "mean"}
                        },
                        .description   = "Bed level interpolation at velocity point in case of tile approach bed level."
                    },
                    PropertySchema {
                        .key           = "ihuzcSig",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {1, "average of Leftsig and Rightsig"},
                            {2, "max of Leftsig and Rightsig"},
                            {3, "min of Leftsig and Rightsig"},
                            {4, "uniform"}
                        },
                        .description   = "If `keepZLayeringAtBed`>=2."
                    },
                    PropertySchema {
                        .key           = "ihuz",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .description   = "TODO"
                    },
                    PropertySchema {
                        .key           = "cosphiutrsh",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "1.0=no bad orthos."
                    },
                    PropertySchema {
                        .key           = "cutCellList",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File with names of cutcell polygons, e.g. cutcellpolygons.lst."
                    },
                    PropertySchema {
                        .key           = "uniformTyp1D",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .description   = "Uniform type for channel profiles not specified by profloc."
                    },
                    PropertySchema {
                        .key           = "1D2DInternalLinkType",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .description   = "Link treatment method for type-3 internal links."
                    },
                    PropertySchema {
                        .key           = "pipeFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File *.pliz containing pipe-based 'culverts'."
                    },
                    PropertySchema {
                        .key           = "groundLayerThickness",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Only in pipes: groundlayer thickness."
                    },
                    PropertySchema {
                        .key           = "extrBl",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Extrapolate bed level at boundaries according to the slope."
                    },
                    PropertySchema {
                        .key           = "keepZLay1BedVol",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Correct volumes when `keepZLayeringAtBed`=1."
                    }
                }
            },
            SectionSchema {
                .name        = "volumeTables",
                .required    = false,
                .description = "This section contains the settings for the volume tables used in 1D grid cells.",
                .properties  = {
                    PropertySchema {
                        .key           = "useVolumeTables",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Use volume tables for 1D grid cells."
                    },
                    PropertySchema {
                        .key           = "increment",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.1",
                        .description   = "The height increment for the volume tables."
                    },
                    PropertySchema {
                        .key           = "useVolumeTableFile",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Read and write the volume table from/to file."
                    }
                }
            },
            SectionSchema {
                .name        = "numerics",
                .required    = false,
                .description = "This section contains the settings of specific parts of the flow solver, such as limiters and the iterative solver type.",
                .properties  = {
                    PropertySchema {
                        .key           = "cflMax",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.7",
                        .description   = "Maximum Courant nr."
                    },
                    PropertySchema {
                        .key           = "advecType",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "33",
                        .enum_values   = {
                            {0, "no"},
                            {3, "Perot q(uiou)"},
                            {33, "Perot q(uio-u) fast"}
                        },
                        .description   = "Advection type."
                    },
                    PropertySchema {
                        .key           = "advecCorrection1D2D",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "regular advection"},
                            {1, "link volume au*dx"},
                            {2, "advection on 1D2D switched off"}
                        },
                        .description   = "Advection correction of 1D2D link volume."
                    },
                    PropertySchema {
                        .key           = "timeStepType",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "2",
                        .enum_values   = {
                            {0, "only transport"},
                            {1, "transport + velocity update"},
                            {2, "full implicit step_reduce"},
                            {3, "step_jacobi"},
                            {4, "explicit"}
                        },
                        .description   = "Type of time stepping."
                    },
                    PropertySchema {
                        .key           = "maxNonLinearIterations",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "100",
                        .description   = "Maximal iterations in non-linear iteration loop before a time step reduction is applied."
                    },
                    PropertySchema {
                        .key           = "setHorizontalBobsFor1D2D",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Bobs are set to 2D bedlevel, to prevent incorrect storage in sewer system."
                    },
                    PropertySchema {
                        .key           = "limTypHu",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "no"},
                            {1, "minmod"},
                            {2, "vanLeer"},
                            {3, "Koren"},
                            {4, "Monotone Central"}
                        },
                        .description   = "Limiter type for waterdepth in continuity eq."
                    },
                    PropertySchema {
                        .key           = "limTypMom",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "4",
                        .enum_values   = {
                            {0, "no"},
                            {1, "minmod"},
                            {2, "vanLeer"},
                            {4, "Monotone Central"}
                        },
                        .description   = "Limiter type for cell center advection velocity."
                    },
                    PropertySchema {
                        .key           = "limTypSa",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "4",
                        .enum_values   = {
                            {0, "no"},
                            {1, "minmod"},
                            {2, "vanLeer"},
                            {4, "Monotone Central"}
                        },
                        .description   = "Limiter type for salinity transport."
                    },
                    PropertySchema {
                        .key           = "pure1D",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "original advection using velocity vector"},
                            {1, "pure 1D using flow volume vol1_f"},
                            {2, "pure 1D using volume vol1"}
                        },
                        .description   = "Purely 1D advection."
                    },
                    PropertySchema {
                        .key           = "junction1D",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "original 1D advection using velocity vector"},
                            {1, "same as along 1D channels using `pure1D`=1"}
                        },
                        .description   = "Advection at 1D junctions."
                    },
                    PropertySchema {
                        .key           = "icgSolver",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "4",
                        .enum_values   = {
                            {4, "sobekGS + Saad-ILUD (default sequential)"},
                            {6, "PETSc (default parallel)"},
                            {7, "CG+MILU (parallel)"}
                        },
                        .description   = "Solver type."
                    },
                    PropertySchema {
                        .key           = "logSolverConvergence",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Print time step, number of solver iterations and solver residual to diagnostic output."
                    },
                    PropertySchema {
                        .key           = "maxDegree",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "6",
                        .description   = "Maximum degree in Gauss elimination."
                    },
                    PropertySchema {
                        .key           = "fixedWeirScheme",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "9",
                        .enum_values   = {
                            {6, "semi-subgrid scheme"},
                            {8, "Tabellenboek"},
                            {9, "Villemonte"}
                        },
                        .description   = "Fixed weir scheme."
                    },
                    PropertySchema {
                        .key           = "fixedWeirContraction",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.0",
                        .description   = "flow width = flow width*`fixedWeirContraction`."
                    },
                    PropertySchema {
                        .key           = "fixedWeirTopWidth",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "3.0",
                        .description   = "Uniform width of the groyne part of fixed weirs."
                    },
                    PropertySchema {
                        .key           = "fixedWeirTalud",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "4.0",
                        .description   = "Uniform talud slope of fixed weirs."
                    },
                    PropertySchema {
                        .key           = "fixedWeirTopFrictCoef",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Uniform friction coefficient of the groyne part of fixed weirs."
                    },
                    PropertySchema {
                        .key           = "fixedWeirRelaxationCoef",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.6",
                        .description   = "Fixed weir relaxation coefficient for computation of energy loss."
                    },
                    PropertySchema {
                        .key           = "fixedWeirScheme1D2D",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "same as `fixedWeirScheme`"},
                            {1, "lateral iterative fixed weir scheme"}
                        },
                        .description   = "Fixed weir scheme for 1D2D links."
                    },
                    PropertySchema {
                        .key           = "fixedWeir1D2D_dx",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "50.0",
                        .description   = "Extra delta x for lateral 1D2D fixed weirs."
                    },
                    PropertySchema {
                        .key           = "izBndPos",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "mirroring of closest cell (as in Delft3D-FLOW)"},
                            {1, "on net boundary"}
                        },
                        .description   = "Position of z boundary."
                    },
                    PropertySchema {
                        .key           = "tlfSmo",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Fourier smoothing time on water level boundaries."
                    },
                    PropertySchema {
                        .key           = "slopeDrop2D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Droplosses are applied if local bottom slope > `slopeDrop2D`, <=0 = no droplosses."
                    },
                    PropertySchema {
                        .key           = "drop1D",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Limit the downstream water level in the momentum equation to the downstream invert level, BOBdown (ζ ∗ down = max (BOBdown, ζdown))."
                    },
                    PropertySchema {
                        .key           = "chkAdvd",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.1",
                        .description   = "Check advection terms if depth < `chkAdvd`."
                    },
                    PropertySchema {
                        .key           = "teta0",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.55",
                        .description   = "Theta (implicitness) of time integration."
                    },
                    PropertySchema {
                        .key           = "qhRelax",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.01",
                        .description   = "Relaxation on Q-h open boundaries."
                    },
                    PropertySchema {
                        .key           = "cstBnd",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Delft3D-FLOW type velocity treatment near boundaries for small coastal models or not."
                    },
                    PropertySchema {
                        .key           = "maxItVerticalForesterSal",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "0",
                        .description   = "Forester iterations for salinity (0: no vertical filter for salinity, > 0: max nr of iterations)."
                    },
                    PropertySchema {
                        .key           = "maxItVerticalForester",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "0",
                        .description   = "Forester iterations for salinity (0: no vertical filter for salinity, > 0: max nr of iterations)."
                    },
                    PropertySchema {
                        .key           = "maxItVerticalForesterTem",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "0",
                        .description   = "Forester iterations for temperature (0: no vertical filter for temperature, > 0: max nr of iterations)."
                    },
                    PropertySchema {
                        .key           = "transportAutoTimeStepDiff",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "lim diff, no lim Dt_tr"},
                            {1, "no lim diff, lim Dt_tr"},
                            {2, "no lim diff, no lim Dt_tr"},
                            {3, "implicit (only 2D)"}
                        },
                        .description   = "Auto timestepdiff in transport."
                    },
                    PropertySchema {
                        .key           = "implicitDiffusion2D",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Implicit diffusion 2D."
                    },
                    PropertySchema {
                        .key           = "turbulenceModel",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "3",
                        .enum_values   = {
                            {0, "no"},
                            {1, "constant"},
                            {2, "algebraic"},
                            {3, "k-epsilon"},
                            {4, "k-tau"}
                        },
                        .description   = "Turbulence model."
                    },
                    PropertySchema {
                        .key           = "c1e",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.489",
                        .description   = "c1 coefficient in turbulence model."
                    },
                    PropertySchema {
                        .key           = "c3eStable",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "c3e coefficient (for stable stratification) in k-eps turbulance model."
                    },
                    PropertySchema {
                        .key           = "c3eUnstable",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.489",
                        .description   = "c3e coefficient (for unstable stratification) in k-eps turbulance model."
                    },
                    PropertySchema {
                        .key           = "antiCreep",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Include anti-creep to suppress artificial vertical diffusion."
                    },
                    PropertySchema {
                        .key           = "barocPOnBnd",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Include baroclinic pressure on open boundaries."
                    },
                    PropertySchema {
                        .key           = "maxItPresDens",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "1",
                        .description   = "Max number of iterations in pressure-density coupling, only if `thermobaricity`=true."
                    },
                    PropertySchema {
                        .key           = "diagnosticTransport",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "No update of transport quantities, also known as diagnostic transport."
                    },
                    PropertySchema {
                        .key           = "maxWaterLevelDiff",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Upper bound on water level changes, (<= 0: no bounds). Run will abort when violated."
                    },
                    PropertySchema {
                        .key           = "maxVelocityDiff",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Upper bound on velocity changes, (<= 0: no bounds). Run will abort when violated."
                    },
                    PropertySchema {
                        .key           = "maxVelocity",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Upper bound on velocity (<= 0: no bounds). Run will abort when violated."
                    },
                    PropertySchema {
                        .key           = "waterLevelWarn",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Warning level on water level (<= 0: no check)."
                    },
                    PropertySchema {
                        .key           = "velocityWarn",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Warning level on velocity (<= 0: no check)."
                    },
                    PropertySchema {
                        .key           = "velMagnWarn",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Warning level on velocity magnitude (<= 0: no check)."
                    },
                    PropertySchema {
                        .key           = "minTimeStepBreak",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Smallest allowed timestep, checked on a sliding average of several timesteps. Run will abort when violated."
                    },
                    PropertySchema {
                        .key           = "epshu",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0001",
                        .description   = "Threshold water depth for wetting and drying."
                    },
                    PropertySchema {
                        .key           = "epsMaxLev",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1e-08",
                        .description   = "Stop criterium for non linear iteration."
                    },
                    PropertySchema {
                        .key           = "epsMaxLevM",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1e-08",
                        .description   = "Stop criterium for Nested Newton loop in non-linear iteration."
                    },
                    PropertySchema {
                        .key           = "flowSolver",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .default_value = "generic1d2d3d",
                        .enum_values   = {
                            {0, "generic1d2d3d"},
                            {1, "implicit1d"}
                        },
                        .description   = "Flow solver."
                    },
                    PropertySchema {
                        .key           = "lateral_fixedweir_umin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Minimal velocity threshold for weir losses in iterative lateral 1D2D weir coupling."
                    },
                    PropertySchema {
                        .key           = "jasfer3D",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Corrections for spherical coordinates."
                    },
                    PropertySchema {
                        .key           = "cfFacVer",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Factor for including (1-CFL) in HO term vertical."
                    },
                    PropertySchema {
                        .key           = "eddyViscosityBedFacmax",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Limit eddy viscosity at bed by factor of first layer above."
                    },
                    PropertySchema {
                        .key           = "lateral_fixedweir_umin_method",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .description   = "Method for minimal velocity treshold for weir losses in iterative lateral 1D2D weir coupling."
                    },
                    PropertySchema {
                        .key           = "lateral_fixedweir_minimal_1d2d_embankment",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Minimal crest height of 1D2D SOBEK-DFM embankments."
                    },
                    PropertySchema {
                        .key           = "testFixedWeirs",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {0, "original Villemonte (Sieben2010) approach"},
                            {1, "Sieben2007"}
                        },
                        .description   = "Test for fixed weir algorithms."
                    },
                    PropertySchema {
                        .key           = "jposhchk",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "2",
                        .enum_values   = {
                            {0, "no"},
                            {1, "0.7dts, just redo"},
                            {2, "1.0dts, close all links"},
                            {3, "0.7dts, close all links"},
                            {4, "1.0dts, reduce au"},
                            {5, "0.7dts, reduce au"},
                            {6, "1.0dts, close outflowing links"},
                            {7, "0.7dts, close outflowing links"}
                        },
                        .description   = "Check for positive waterdepth."
                    },
                    PropertySchema {
                        .key           = "cfConHorMom",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Constant for including (1-CFL) in HO term horizontal momentum."
                    },
                    PropertySchema {
                        .key           = "cfFacHorMom",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Factor for including (1-CFL) in HO term horizontal momentum."
                    },
                    PropertySchema {
                        .key           = "trsh_u1lb",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "2D bedfriction in 3D below this threshold."
                    },
                    PropertySchema {
                        .key           = "jaupwindsrc",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {0, "no (higher-order)"},
                            {1, "yes (first-order)"}
                        },
                        .description   = "Upwind advection discretization at sources/sinks."
                    },
                    PropertySchema {
                        .key           = "corioAdamsBashfordFac",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.5",
                        .description   = "Only when `newCorio`=1, Adams-Bashford factor in Coriolis term."
                    },
                    PropertySchema {
                        .key           = "corioConstant",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "no change in Coriolis"},
                            {1, "Coriolis constant in Spherical models"},
                            {2, "beta plane approach both in Cartesian and Spherical coordinates"}
                        },
                        .description   = "Coriolis constant."
                    },
                    PropertySchema {
                        .key           = "drop3D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.0",
                        .description   = "Drop losses in 3D are applied if z upwind is below bob + 2/3 hu*`drop3D`."
                    },
                    PropertySchema {
                        .key           = "zLayerCenterBedVel",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Reconstruction of center velocity at half closed bed cells."
                    },
                    PropertySchema {
                        .key           = "horAdvTypZLayer",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "default"},
                            {2, "sigma-like"}
                        },
                        .description   = "Horizontal advection treatment of z-layers for dambreaks."
                    },
                    PropertySchema {
                        .key           = "iCoriolisType",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "5",
                        .enum_values   = {
                            {0, "no Coriolis force"},
                            {5, "default approach for Coriolis (depth dependent and similar to Delft3D-FLOW)"}
                        },
                        .description   = "Coriolis type."
                    },
                    PropertySchema {
                        .key           = "zwsbTol",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Tolerance for zws(kb-1) at bed."
                    },
                    PropertySchema {
                        .key           = "cfExpHu",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Exponent for including (1-CFL) in sethu."
                    },
                    PropertySchema {
                        .key           = "jbasqbnddownwindhs",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "original hu"},
                            {1, "downwind hs"}
                        },
                        .description   = "Water depth scheme at discharge boundaries."
                    },
                    PropertySchema {
                        .key           = "filterOrder",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "2",
                        .enum_values   = {
                            {1, "first order"},
                            {2, "second order"}
                        },
                        .description   = "First-order or second order filter to suppress checkerboarding."
                    },
                    PropertySchema {
                        .key           = "keepSTBndOnOutflow",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {0, "no (like Thatcher-Harleman)"},
                            {1, "yes (use prescribed boundary conditions)"}
                        },
                        .description   = "Keep salinity and temperature signals on boundary cells at outflow."
                    },
                    PropertySchema {
                        .key           = "keepZLayeringAtBed",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "2",
                        .enum_values   = {
                            {0, "original bed level"},
                            {1, "adapted bed level"},
                            {2, "Ztbml approach of Delft3D-FLOW"}
                        },
                        .description   = "Z-layering at bed."
                    },
                    PropertySchema {
                        .key           = "logProfAtUBndIn",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {0, "uniform U1"},
                            {1, "log U1"},
                            {2, "log U1 and k-eps accordingly"}
                        },
                        .description   = "ubnds inflow."
                    },
                    PropertySchema {
                        .key           = "logProfKepsBndIn",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "k-eps"},
                            {1, "log k-eps inflow"},
                            {2, "log k-eps in- and outflow"}
                        },
                        .description   = "3D profile at open boundaries."
                    },
                    PropertySchema {
                        .key           = "epshstem",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.001",
                        .description   = "Only compute heat flux + evaporation if depth > `epshstem`."
                    },
                    PropertySchema {
                        .key           = "diffusionOnBnd",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Horizontal diffusion on open boundaries."
                    },
                    PropertySchema {
                        .key           = "newCorio",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "New standard way of Coriolis term calculation."
                    },
                    PropertySchema {
                        .key           = "barrierAdvection",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {1, "no correction"},
                            {2, "advection correction"}
                        },
                        .description   = "Advection modelling at barriers."
                    },
                    PropertySchema {
                        .key           = "rhoInterfaces",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "-1",
                        .enum_values   = {
                            {-1, "Original linear interpolation"},
                            {0, "Improved linear interpolation"},
                            {1, "Recompute from salinity and temperature"},
                            {2, "Use cell density"}
                        },
                        .description   = "Estimate rho at 3D layer interfaces for baroclinic pressure gradient method."
                    },
                    PropertySchema {
                        .key           = "chkdifd",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Check diffusion terms if depth < `chkdifd`, only if `transportAutoTimeStepDiff`=1."
                    },
                    PropertySchema {
                        .key           = "fixedWeirFrictScheme",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "friction based on hu"},
                            {1, "friction based on subgrid weir friction scheme"},
                            {2, "without weir (like WAQUA)"},
                            {3, "full undisturbed velocity reconstruction"},
                            {4, "full undisturbed velocity reconstruction"}
                        },
                        .description   = "Fixed weir friction scheme."
                    },
                    PropertySchema {
                        .key           = "testDryingFlooding",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "D-Flow FM"},
                            {1, "Delft3D-FLOW"},
                            {2, "similar to `0`, and volume limitation in the transport solver based on `epshu`"}
                        },
                        .description   = "Drying flooding algorithm."
                    },
                    PropertySchema {
                        .key           = "turbulenceAdvection",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "3",
                        .enum_values   = {
                            {0, "none"},
                            {1, "upwind explicit"},
                            {2, "central explicit"},
                            {3, "horizontally explicit and vertically implicit"},
                            {4, "central implicit"}
                        },
                        .description   = "Turbulance advection."
                    },
                    PropertySchema {
                        .key           = "horizontalMomentumfilter",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Filter for reduction of checkerboarding."
                    },
                    PropertySchema {
                        .key           = "checkerboardMonitor",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Flag for checkerboarding output on history file (only for sigma layers yet)."
                    },
                    PropertySchema {
                        .key           = "tSpinUpTurbLogProf",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Spin up time when starting with a parabolic viscosity profile in whole model domain."
                    },
                    PropertySchema {
                        .key           = "vertAdvTypMom",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "6",
                        .enum_values   = {
                            {3, "upwind implicit"},
                            {6, "centerbased upwind explicit"}
                        },
                        .description   = "Vertical advection type in momentum equation."
                    },
                    PropertySchema {
                        .key           = "verticalAdvectionType",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .default_value = "higherOrderUpwindExplicit",
                        .enum_values   = {
                            {0, "centralImplicit"},
                            {1, "higherOrderUpwindExplicit"}
                        },
                        .description   = "Vertical advection type for salinity. Note that `verticalAdvectionType`=`centralImplicit` leads to less numerical dissipation than `verticalAdvectionType`=`higherOrderUpwindExplicit`."
                    },
                    PropertySchema {
                        .key           = "vertAdvTypSal",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "6",
                        .enum_values   = {
                            {0, "none"},
                            {4, "theta implicit"},
                            {6, "higher order explicit, no Forester filter"}
                        },
                        .description   = "Vertical advection type for salinity. Note that `vertAdvTypSal`=4 leads to less numerical dissipation than `vertAdvTypSal`=6."
                    },
                    PropertySchema {
                        .key           = "vertAdvTypTem",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "6",
                        .enum_values   = {
                            {0, "none"},
                            {4, "theta implicit"},
                            {6, "higher order explicit, no Forester filter"}
                        },
                        .description   = "Vertical advection type for temperature. Note that `vertAdvTypTem`=4 leads to less numerical dissipation than `vertAdvTypTem`=6."
                    },
                    PropertySchema {
                        .key           = "zeroZBndInflowAdvection",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "Neumann"},
                            {1, "zero at inflow"},
                            {2, "zero at inflow and outflow"}
                        },
                        .description   = "Switch for advection at open boundary."
                    },
                    PropertySchema {
                        .key           = "turbulenceTimeIntegrationFactor",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "0.0=Tur0 from links, 1.0=Tur0 maximal mix of values from links with nodes"
                    },
                    PropertySchema {
                        .key           = "turbulenceTimeIntegrationMethod",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "2",
                        .enum_values   = {
                            {1, "apply to all cells"},
                            {2, "apply only when vertical layers are horizontally connected"}
                        },
                        .description   = "Where to apply `turbulenceTimeIntegrationFactor`"
                    },
                    PropertySchema {
                        .key           = "locSaltMin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "5.0",
                        .description   = "Minimum salinity for case of lock exchange."
                    },
                    PropertySchema {
                        .key           = "locSaltMax",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "10.0",
                        .description   = "Maximum salinity for case of lock exchange."
                    },
                    PropertySchema {
                        .key           = "locSaltLev",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.0",
                        .description   = "Salinity level for case of lock exchange."
                    },
                    PropertySchema {
                        .key           = "linContin",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Linear continuity."
                    },
                    PropertySchema {
                        .key           = "cfExpHorMom",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Exponent for including (1-CFL) in HO term horizontal momentum."
                    },
                    PropertySchema {
                        .key           = "coriohhtrsh",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Only when `newCorio`=1, 0.0=no safety in hu/hus weightings."
                    },
                    PropertySchema {
                        .key           = "limTypW",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {0, "no"},
                            {1, "minmod"},
                            {2, "vanLeer"},
                            {3, "Koren"},
                            {4, "Monotone Central"}
                        },
                        .description   = "Limiter type for wave action transport."
                    },
                    PropertySchema {
                        .key           = "huWeirRegular",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "For Villemonte and Tabellenboek, regular hu below `huWeirRegular`."
                    },
                    PropertySchema {
                        .key           = "structureLayersActive",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {0, "structure flow through all layers"},
                            {1, "structure flow only through open layers"}
                        },
                        .description   = "For Villemonte and Tabellenboek, regular hu below `huWeirRegular`."
                    },
                    PropertySchema {
                        .key           = "baOrgFracMin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Cell area = max(orgcellarea*`baOrgFracMin`, cut cell area)."
                    },
                    PropertySchema {
                        .key           = "subsuplupdates1",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Update water levels (s1) due to subsidence/uplift."
                    },
                    PropertySchema {
                        .key           = "linkDriedMx",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .description   = "Maximum numberr of Au growth steps after having dried."
                    },
                    PropertySchema {
                        .key           = "lateral_fixedweir_relax",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Relaxation factor for iterative lateral 1D2D weir coupling algorithm."
                    },
                    PropertySchema {
                        .key           = "numlimdt_baorg",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "If previous numlimdt > `numlimdt_baorg` keep original cell area ba in cut cell."
                    },
                    PropertySchema {
                        .key           = "cfFacHu",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Factor for including (1-CFL) in sethu."
                    },
                    PropertySchema {
                        .key           = "vertAdvTypMom3OnBnd",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "follow javau"},
                            {1, "on bnd"},
                            {2, "on and near bnd"}
                        },
                        .description   = "Vertical advection type u1 bnd UpwimpL."
                    },
                    PropertySchema {
                        .key           = "noDerivedTypes",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {0, "use derived types"},
                            {1, "less"},
                            {2, "lesser"},
                            {5, "also deallocate derived types"}
                        },
                        .description   = "Use derived types."
                    },
                    PropertySchema {
                        .key           = "jarhoxu",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "no (strongly advised)"},
                            {1, "yes"},
                            {2, "also in barotropic and baroclinic pressure term"},
                            {3, "also in vertical advection"},
                            {4, "also in vertical advection"}
                        },
                        .description   = "Include density gradient in advection term."
                    },
                    PropertySchema {
                        .key           = "ilutype",
                        .required      = false,
                        .value_type    = ValueType::String,
                        .description   = "TODO."
                    },
                    PropertySchema {
                        .key           = "nlevel",
                        .required      = false,
                        .value_type    = ValueType::String,
                        .description   = "TODO."
                    },
                    PropertySchema {
                        .key           = "dtol",
                        .required      = false,
                        .value_type    = ValueType::String,
                        .description   = "TODO."
                    },
                    PropertySchema {
                        .key           = "pillarFarFieldVelocity",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Use far-field velocity for pillars."
                    },
                    PropertySchema {
                        .key           = "minWaterlevelChangeBreak",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Stop the simulation when the rolling mean of the maximum water level change is below this value (considered when larger than 0.0)."
                    }
                }
            },
            SectionSchema {
                .name        = "physics",
                .required    = false,
                .description = "In this field, physical model parameters can be inserted, for instance related to friction modelling and turbulence modelling.",
                .properties  = {
                    PropertySchema {
                        .key           = "unifFrictCoef",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.023",
                        .description   = "Uniform friction coefficient."
                    },
                    PropertySchema {
                        .key           = "unifFrictType",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {0, "Chezy"},
                            {1, "Manning"},
                            {2, "White-Colebrook"},
                            {3, "White-Colebrook in WAQUA"}
                        },
                        .description   = "Uniform friction type."
                    },
                    PropertySchema {
                        .key           = "unifFrictCoef1D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.023",
                        .description   = "Uniform friction coefficient in 1D links."
                    },
                    PropertySchema {
                        .key           = "unifFrictCoef1D2D",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.023",
                        .description   = "Uniform friction coefficient in 1D2D links."
                    },
                    PropertySchema {
                        .key           = "unifFrictCoefLin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Uniform linear friction coefficient."
                    },
                    PropertySchema {
                        .key           = "vicouv",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.1",
                        .description   = "Uniform horizontal eddy viscosity."
                    },
                    PropertySchema {
                        .key           = "dicouv",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.1",
                        .description   = "Uniform horizontal eddy diffusivity."
                    },
                    PropertySchema {
                        .key           = "vicoww",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1e-06",
                        .description   = "Background vertical eddy viscosity."
                    },
                    PropertySchema {
                        .key           = "dicoww",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1e-06",
                        .description   = "Background vertical eddy diffusivity."
                    },
                    PropertySchema {
                        .key           = "vicwminb",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Minimum viscosity in production and buoyancy term."
                    },
                    PropertySchema {
                        .key           = "xlOzmidov",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Ozmidov length scale, 0.0=no contribution of internal waves to vertical diffusion."
                    },
                    PropertySchema {
                        .key           = "TKEMin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1e-32",
                        .description   = "Minimum turbulence kinetic energy (TKE) value in k-eps turbulence model."
                    },
                    PropertySchema {
                        .key           = "EPSMin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1e-32",
                        .description   = "Minimum turbulent dissipation rate (EPS) value in k-eps turbulence model."
                    },
                    PropertySchema {
                        .key           = "TAUMin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1e-32",
                        .description   = "Minimum turbulent time scale (TAU) value in k-tau turbulence model."
                    },
                    PropertySchema {
                        .key           = "Smagorinsky",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.2",
                        .description   = "Add Smagorinsky horizontal turbulence: vicu = vicu + ((`Smagorinsky`*dx)^2)*S."
                    },
                    PropertySchema {
                        .key           = "Elder",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Add Elder contribution: vicu = vicu + (`Elder`*kappa*ustar*H/6); e.g. 1.0."
                    },
                    PropertySchema {
                        .key           = "irov",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "free slip"},
                            {1, "partial slip using `wall_ks`"}
                        },
                        .description   = "Wall friction."
                    },
                    PropertySchema {
                        .key           = "wall_ks",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Nikuradse roughness for side walls, wall_z0=`wall_ks`/30."
                    },
                    PropertySchema {
                        .key           = "rhoMean",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1000.0",
                        .description   = "Average water density."
                    },
                    PropertySchema {
                        .key           = "iDensForm",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "2",
                        .enum_values   = {
                            {0, "uniform"},
                            {1, "Eckart"},
                            {2, "UNESCO"},
                            {3, "UNESCO83"},
                            {13, "`3`+pressure"}
                        },
                        .description   = "Density calculation."
                    },
                    PropertySchema {
                        .key           = "thermobaricity",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Include pressure effects on water density. Only works for `iDensForm`=3 (UNESCO83)."
                    },
                    PropertySchema {
                        .key           = "ag",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "9.81",
                        .description   = "Gravitational acceleration."
                    },
                    PropertySchema {
                        .key           = "tidalForcing",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Tidal forcing, if jserfic=1."
                    },
                    PropertySchema {
                        .key           = "itcap",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Upper limit on internal tides dissipation."
                    },
                    PropertySchema {
                        .key           = "doodsonStart",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "55.565",
                        .description   = "Doodson start time for tidal forcing."
                    },
                    PropertySchema {
                        .key           = "doodsonStop",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "375.575",
                        .description   = "Doodson stop time for tidal forcing."
                    },
                    PropertySchema {
                        .key           = "doodsonEps",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Doodson tolerance level for tidal forcing."
                    },
                    PropertySchema {
                        .key           = "villemonteCD1",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.0",
                        .description   = "Calibration coefficient for Villemonte."
                    },
                    PropertySchema {
                        .key           = "villemonteCD2",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "10.0",
                        .description   = "Calibration coefficient for Villemonte."
                    },
                    PropertySchema {
                        .key           = "salinity",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Include salinity."
                    },
                    PropertySchema {
                        .key           = "initialSalinity",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Initial salinity concentration."
                    },
                    PropertySchema {
                        .key           = "sal0AboveZLev",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "Salinity 0 above level."
                    },
                    PropertySchema {
                        .key           = "deltaSalinity",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "Uniform initial salinity."
                    },
                    PropertySchema {
                        .key           = "backgroundSalinity",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "30.0",
                        .description   = "Background salinity for eqn. of state if salinity not computed."
                    },
                    PropertySchema {
                        .key           = "temperature",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "no"},
                            {1, "only transport"},
                            {3, "excess model of D3D"},
                            {5, "composite (ocean) model"}
                        },
                        .description   = "Include temperature."
                    },
                    PropertySchema {
                        .key           = "initialTemperature",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "6.0",
                        .description   = "Initial temperature."
                    },
                    PropertySchema {
                        .key           = "backgroundWaterTemperature",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "20.0",
                        .description   = "Background water temperature for eqn. of state if temperature not computed."
                    },
                    PropertySchema {
                        .key           = "secchiDepth",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "2.0",
                        .description   = "Water clarity parameter."
                    },
                    PropertySchema {
                        .key           = "stanton",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0013",
                        .description   = "Coefficient for convective heat flux, if negative, then Cd wind is used."
                    },
                    PropertySchema {
                        .key           = "dalton",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0013",
                        .description   = "Coefficient for evaporative heat flux, if negative, then Cd wind is used."
                    },
                    PropertySchema {
                        .key           = "albedo",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.06",
                        .description   = "Albedo coefficient. Fraction of solar radiation reflected by the water surface."
                    },
                    PropertySchema {
                        .key           = "tempMax",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "Limit the temperature to max value."
                    },
                    PropertySchema {
                        .key           = "tempMin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Limit the temperature to min value."
                    },
                    PropertySchema {
                        .key           = "saliMax",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "Limit for salinity to max value."
                    },
                    PropertySchema {
                        .key           = "saliMin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Limit for salinity to min value."
                    },
                    PropertySchema {
                        .key           = "heat_eachStep",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "each user time step"},
                            {1, "each time step"}
                        },
                        .description   = "Switch for heat each time step or each user time step."
                    },
                    PropertySchema {
                        .key           = "nudgeTimeUni",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "3600.0",
                        .description   = "Uniform nudge relaxation time."
                    },
                    PropertySchema {
                        .key           = "iniWithNudge",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "no"},
                            {1, "yes"},
                            {2, "only initialize, no nudging"}
                        },
                        .description   = "Initialize salinity and temperature with nudge variables."
                    },
                    PropertySchema {
                        .key           = "secondaryFlow",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Secondary flow."
                    },
                    PropertySchema {
                        .key           = "betaSpiral",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Weight factor of the spiral flow intensity on flow dispersion stresses. 0.0=disabled."
                    },
                    PropertySchema {
                        .key           = "breachGrowth",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .default_value = "symmetric-asymmetric",
                        .enum_values   = {
                            {0, "symmetric"},
                            {1, "proportional"},
                            {2, "symmetric-asymmetric"}
                        },
                        .description   = "Method for distributing dam breach width over dam break flow links."
                    },
                    PropertySchema {
                        .key           = "thermobaricityInPressureGradient",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Apply thermobaricity in computing the baroclinic pressure gradient."
                    },
                    PropertySchema {
                        .key           = "surfTempSmoFac",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Horizontal smoothing factor for surface water in heatflux computations."
                    },
                    PropertySchema {
                        .key           = "selfAttractionLoading_correct_wl_with_ini",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Correct water level with initial water level in self attraction and loading."
                    },
                    PropertySchema {
                        .key           = "nfEntrainmentMomentum",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Use momentum transfer in NearField related entrainment."
                    },
                    PropertySchema {
                        .key           = "equili",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Equilibrium spiral flow intensity."
                    },
                    PropertySchema {
                        .key           = "soilTempThick",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Use soil temperature buffer if >0.0"
                    },
                    PropertySchema {
                        .key           = "selfAttractionLoading",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "no"},
                            {1, "yes"},
                            {2, "only self attraction"}
                        },
                        .description   = "Use self attraction and loading."
                    },
                    PropertySchema {
                        .key           = "prandtlNumberTemperature",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Turbulent Prandtl number for temperature."
                    },
                    PropertySchema {
                        .key           = "schmidtNumberSalinity",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Turbulent Schmidt number for salinity."
                    },
                    PropertySchema {
                        .key           = "schmidtNumberTracer",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Turbulent Schmidt number for tracer(s)."
                    },
                    PropertySchema {
                        .key           = "umodLin",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Linear friction umod."
                    },
                    PropertySchema {
                        .key           = "jadelvappos",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Only positive forced evaporation fluxes."
                    },
                    PropertySchema {
                        .key           = "freeConvectionCoefficient",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.14",
                        .description   = "Free convection turbulence coefficient."
                    },
                    PropertySchema {
                        .key           = "uniffrictcoef1dgrlay",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Uniform ground layer friction coefficient for ocean models."
                    },
                    PropertySchema {
                        .key           = "salinityDependentFreezingPoint",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Enable computation of negative temperature values by adjusting freezing point based on salinity levels. `tempMin` should be below 0 degrees Celsius."
                    }
                }
            },
            SectionSchema {
                .name        = "sediment",
                .required    = false,
                .description = "This section contains the setting for sediment transport and morphology.",
                .properties  = {
                    PropertySchema {
                        .key           = "sedimentModelNr",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "2",
                        .enum_values   = {
                            {0, "no"},
                            {1, "Krone"},
                            {2, "SvR2007"},
                            {3, "E-H"},
                            {4, "MorphologyModule"}
                        },
                        .description   = "Sediment model number."
                    },
                    PropertySchema {
                        .key           = "morFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Morphology settings file (*.mor)."
                    },
                    PropertySchema {
                        .key           = "sedFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Sediment characteristics file (*.sed)."
                    },
                    PropertySchema {
                        .key           = "jaceneqtr",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .description   = "TODO"
                    },
                    PropertySchema {
                        .key           = "mxgrkrone",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .description   = "Highest fraction index treated by Krone."
                    },
                    PropertySchema {
                        .key           = "sedDensCoupling",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Sed rho coupling."
                    },
                    PropertySchema {
                        .key           = "implicitFallVelocity",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {0, "implicit"},
                            {1, "explicit"}
                        },
                        .description   = "Implicit or explicit fall velocity"
                    },
                    PropertySchema {
                        .key           = "nr_of_sedfractions",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .description   = "Number of sediment fractions."
                    }
                }
            },
            SectionSchema {
                .name        = "sedtrails",
                .required    = false,
                .description = "",
                .properties  = {
                    PropertySchema {
                        .key           = "sedTrailsGrid",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Grid file for sedtrails output locations on corners."
                    },
                    PropertySchema {
                        .key           = "sedtrailsAnalysis",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .enum_values   = {
                            {0, "all"},
                            {1, "transport"},
                            {2, "flowvelocity"},
                            {3, "soulsby"}
                        },
                        .description   = "Sedtrails analysis."
                    },
                    PropertySchema {
                        .key           = "sedtrailsInterval",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .description   = "Sedtrails output, given as 'interval' 'start period' 'end period'"
                    },
                    PropertySchema {
                        .key           = "sedtrailsOutputFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Sedtrails time-averaged output file."
                    }
                }
            },
            SectionSchema {
                .name        = "wind",
                .required    = false,
                .description = "The wind section prescribes the dependency of the wind drag coefficient to the wind velocity through 2 or 3 breakpoints. This field also contains pressure information",
                .properties  = {
                    PropertySchema {
                        .key           = "icdTyp",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "2",
                        .enum_values   = {
                            {1, "Constant"},
                            {2, "Smith&Banke (2 pts)"},
                            {3, "Smith&Banke (3 pts)"},
                            {4, "Charnock 1955"},
                            {5, "Hwang 2005"},
                            {6, "Wuest 2005"},
                            {7, "Hersbach 2011 (2 pts)"},
                            {8, "`4`+viscous"}
                        },
                        .description   = "Wind drag coefficient type."
                    },
                    PropertySchema {
                        .key           = "cdBreakPoints",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "0.00063 0.00723",
                        .description   = "Wind drag breakpoints."
                    },
                    PropertySchema {
                        .key           = "windSpeedBreakpoints",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "0.0 100.0",
                        .description   = "Wind speed breakpoints."
                    },
                    PropertySchema {
                        .key           = "rhoAir",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.2",
                        .description   = "Air density."
                    },
                    PropertySchema {
                        .key           = "computedAirDensity",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Compute air density. Requires quantities airpressure, airtemperature and dewpoint in ext-file."
                    },
                    PropertySchema {
                        .key           = "stressToWind",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Switch between wind speed and wind stress approach for wind forcing."
                    },
                    PropertySchema {
                        .key           = "relativeWind",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Wind speed factor relative to top-layer water speed*`relativeWind` (0.0=no relative wind, 1.0=using full top layer speed)."
                    },
                    PropertySchema {
                        .key           = "windPartialDry",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Reduce windstress on water if link partially dry, only for `bedLevType`=3."
                    },
                    PropertySchema {
                        .key           = "pavBnd",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Average air pressure on open boundaries, only applied if value > 0."
                    },
                    PropertySchema {
                        .key           = "pavIni",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Initial air pressure, only applied if value > 0."
                    },
                    PropertySchema {
                        .key           = "windHuOrZwsBased",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "hu"},
                            {1, "zws"}
                        },
                        .description   = "Wind drag hu or zws based."
                    },
                    PropertySchema {
                        .key           = "varyingAirDensity",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Compute air density."
                    },
                    PropertySchema {
                        .key           = "wind_eachstep",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "0"},
                            {1, "1"}
                        },
                        .description   = "Switch for wind and air pressure each time step or each user time step."
                    },
                    PropertySchema {
                        .key           = "rhoWaterInWindStress",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "Rhomean"},
                            {1, "local (surface) density of model"}
                        },
                        .description   = "Water density used in computation of wind stress."
                    }
                }
            },
            SectionSchema {
                .name        = "waves",
                .required    = false,
                .description = "The wind section prescribes the dependency of the wind drag coefficient to the wind velocity through 2 or 3 breakpoints. This field also contains pressure information",
                .properties  = {
                    PropertySchema {
                        .key           = "waveModelNr",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "3",
                        .enum_values   = {
                            {0, "none"},
                            {1, "fetch/depth limited hurdlestive"},
                            {2, "Young-Verhagen"},
                            {3, "SWAN"},
                            {4, "wave group forcing"},
                            {5, "uniform"},
                            {6, "SWAN-NetCDF"}
                        },
                        .description   = "Wave model nr."
                    },
                    PropertySchema {
                        .key           = "rouWav",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .default_value = "FR84",
                        .enum_values   = {
                            {0, "FR84"},
                            {1, "MS90"},
                            {2, "HT91"},
                            {3, "GM79"},
                            {4, "DS88"},
                            {5, "BK67"},
                            {6, "CJ85"},
                            {7, "OY88"},
                            {8, "VR04"}
                        },
                        .description   = "Friction model for wave induced shear stress."
                    },
                    PropertySchema {
                        .key           = "gammaX",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.5",
                        .description   = "Maximum wave height/water depth ratio."
                    },
                    PropertySchema {
                        .key           = "flowWithoutWaves",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Exclude Wave data in the flow computations, passing it directly to D-WAQ."
                    },
                    PropertySchema {
                        .key           = "surfBeatInput",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File with surf beat input conditions."
                    },
                    PropertySchema {
                        .key           = "waveSwartDelwaq",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "If `waveSwartDelwaq`=1 and tiWaq> 0, then increase tauwave to Delwaq with 0.5rhofwuorbuorb."
                    },
                    PropertySchema {
                        .key           = "hwavuni",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Root mean square wave height."
                    },
                    PropertySchema {
                        .key           = "tiFetchComp",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Time interval fetch comp if `waveModelNr`=1,2."
                    },
                    PropertySchema {
                        .key           = "phiwavuni",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Root mean square wave direction, math convention."
                    },
                    PropertySchema {
                        .key           = "3DWaveStreaming",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .description   = "Influence of wave streaming. 0: no, 1: added to adve."
                    },
                    PropertySchema {
                        .key           = "3DWaveBoundaryLayer",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {1, "Sana"}
                        },
                        .description   = "Boundary layer formulation."
                    },
                    PropertySchema {
                        .key           = "twavuni",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Root mean square wave period."
                    },
                    PropertySchema {
                        .key           = "uorbfac",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {0, "D3D style"},
                            {1, "Guza style"}
                        },
                        .description   = "Orbital velocities."
                    },
                    PropertySchema {
                        .key           = "3DStokesProfile",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {0, "no"},
                            {1, "uniform over depth"},
                            {2, "second-order Stokes theory"},
                            {3, "`2` with vertical stokes gradient in adve."}
                        },
                        .description   = "Stokes profile."
                    },
                    PropertySchema {
                        .key           = "jamapsigwav",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "hrms wave height"},
                            {1, "sign wave height"}
                        },
                        .description   = "Wave height on map output"
                    },
                    PropertySchema {
                        .key           = "hminlw",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Cut-off depth for application of wave forces in momentum balance."
                    },
                    PropertySchema {
                        .key           = "jahissigwav",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {0, "hrms wave height"},
                            {1, "sign wave height"}
                        },
                        .description   = "Wave height on his output"
                    }
                }
            },
            SectionSchema {
                .name        = "grw",
                .required    = false,
                .description = "This section contains the settings for ground water flow.",
                .properties  = {
                    PropertySchema {
                        .key           = "groundWater",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Use (horizontal) ground water flow."
                    },
                    PropertySchema {
                        .key           = "infiltrationModel",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "no infiltration"},
                            {1, "interception layer"},
                            {2, "constant infiltration capacity"},
                            {3, "model unsaturated/saturated (with grw)"},
                            {4, "Horton"}
                        },
                        .description   = "Infiltration method."
                    },
                    PropertySchema {
                        .key           = "hInterceptionLayer",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Intercept this amount of rain."
                    },
                    PropertySchema {
                        .key           = "unifInfiltrationCapacity",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Uniform maximum infiltration capacity."
                    },
                    PropertySchema {
                        .key           = "conductivity",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Non-dimensionless K conductivity saturated, Q = K*A*i (m³/s)."
                    },
                    PropertySchema {
                        .key           = "h_aquiferuni",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "20.0",
                        .description   = "Level of impervious layer is bgrw = bl - `h_aquiferuni`, if negative, bgrw = `bgrwuni`."
                    },
                    PropertySchema {
                        .key           = "bgrwuni",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "Uniform level of impervious layer, only used if `h_aquiferuni` is negative."
                    },
                    PropertySchema {
                        .key           = "h_unsatini",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.2",
                        .description   = "Initial level ground water is bedlevel - `h_unsatini`, if negative, sgrw = `sgrwini`."
                    },
                    PropertySchema {
                        .key           = "sgrwini",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-999.0",
                        .description   = "Initial ground water level, if `h_unsatini` < 0."
                    }
                }
            },
            SectionSchema {
                .name        = "hydrology",
                .required    = false,
                .description = "This section contains the settings for hydrology.",
                .properties  = {
                    PropertySchema {
                        .key           = "interceptionModel",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "none"},
                            {1, "on, via layer thickness"}
                        },
                        .description   = "Interception model."
                    }
                }
            },
            SectionSchema {
                .name        = "time",
                .required    = false,
                .description = "This section contains the time settings for the model, such as start and stop time of the simulation.",
                .properties  = {
                    PropertySchema {
                        .key           = "refDate",
                        .required      = false,
                        .value_type    = ValueType::DateTime,
                        .default_value = "20010101",
                        .description   = "Reference date. By default midnight is taken (00h00m00s)."
                    },
                    PropertySchema {
                        .key           = "tZone",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Data Sources in GMT are interrogated with time in minutes since `refDate`-`tZone`*60."
                    },
                    PropertySchema {
                        .key           = "tUnit",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .default_value = "S",
                        .enum_values   = {
                            {0, "D"},
                            {1, "H"},
                            {2, "M"},
                            {3, "S"}
                        },
                        .description   = "Time units in MDU."
                    },
                    PropertySchema {
                        .key           = "dtUser",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "300.0",
                        .description   = "User timestep in seconds (interval for external forcing update & his/map output)."
                    },
                    PropertySchema {
                        .key           = "dtNodal",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "21600.0",
                        .description   = "Time interval for updating nodal factors in astronomical boundary conditions."
                    },
                    PropertySchema {
                        .key           = "dtMax",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "30.0",
                        .description   = "Maximum timestep in seconds."
                    },
                    PropertySchema {
                        .key           = "dtInit",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.0",
                        .description   = "Initial timestep in seconds."
                    },
                    PropertySchema {
                        .key           = "tStart",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Start time w.r.t. `refDate`."
                    },
                    PropertySchema {
                        .key           = "tStop",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "86400.0",
                        .description   = "Stop time w.r.t. `refDate`."
                    },
                    PropertySchema {
                        .key           = "startDateTime",
                        .required      = false,
                        .nullable      = true,
                        .value_type    = ValueType::DateTime,
                        .description   = "Computation start datetime, when specified, overrides `tStart`."
                    },
                    PropertySchema {
                        .key           = "stopDateTime",
                        .required      = false,
                        .nullable      = true,
                        .value_type    = ValueType::DateTime,
                        .description   = "Computation stop datetime, when specified, overrides `tStop`."
                    },
                    PropertySchema {
                        .key           = "updateRoughnessInterval",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "86400.0",
                        .description   = "Update interval for time dependent roughness parameters."
                    },
                    PropertySchema {
                        .key           = "tStartTlfsmo",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Start time w.r.t. `refDate` of Fourier smoothing time on water level boundaries."
                    },
                    PropertySchema {
                        .key           = "startDateTimeTlfsmo",
                        .required      = false,
                        .value_type    = ValueType::DateTime,
                        .description   = "Computation start datetime w.r.t. `refDate` of Fourier smoothing time on water level boundaries, when specified, overrides `tStartTlfsmo`."
                    },
                    PropertySchema {
                        .key           = "autoTimestep",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {0, "Use no CFL timestep limit (use constant timestepping)"},
                            {1, "For 2D-models; use outflows in CFL timestep limit"},
                            {3, "For 3D-models; use horizontal outflows (per cell-layer) in CFL timestep limit"},
                            {4, "For 3D-models; use horizontal in- and outflows (per cell-layer) in CFL timestep limit"},
                            {5, "For 3D-models; use in- or outflows (per cell-column) in CFL timestep limit"}
                        },
                        .description   = "Automatic timestepping limited by the CFL condition. Several options are available controlling which flows are used in the CFL limit. Options 1 and 5 are the default for 2D and 3D models, respectively."
                    },
                    PropertySchema {
                        .key           = "autoTimestepNoStruct",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Exclude structure links (and neighbours) from time step limitation."
                    },
                    PropertySchema {
                        .key           = "autoTimestepNoQOut",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Exclude negative qin terms from time step limitation."
                    },
                    PropertySchema {
                        .key           = "dtFacMax",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "1.1",
                        .description   = "Max timestep increase factor in successive time steps."
                    },
                    PropertySchema {
                        .key           = "timeStepAnalysis",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write time steps analysis file *.steps."
                    },
                    PropertySchema {
                        .key           = "autoTimeStepVisc",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Use time step limitation based on explicit diffusive term."
                    }
                }
            },
            SectionSchema {
                .name        = "restart",
                .required    = false,
                .description = "This section contains the settings for restarting from a previous simulation.",
                .properties  = {
                    PropertySchema {
                        .key           = "restartFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Restart file, only from NetCDF-file, hence: either *_rst.nc or *_map.nc."
                    },
                    PropertySchema {
                        .key           = "restartDateTime",
                        .required      = false,
                        .value_type    = ValueType::DateTime,
                        .description   = "Restart time, only relevant but obligatory in case of restart from *_map.nc."
                    },
                    PropertySchema {
                        .key           = "rstIgnoreBl",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Ignore bed level from restart."
                    }
                }
            },
            SectionSchema {
                .name        = "external forcing",
                .required    = false,
                .description = "This section contains the settings for external forcings.",
                .properties  = {
                    PropertySchema {
                        .key           = "extForceFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Old format for external forcings file *.ext, link with tim/cmp-format boundary conditions specification."
                    },
                    PropertySchema {
                        .key           = "extForceFileNew",
                        .required      = false,
                        .value_type    = ValueType::PathList,
                        .description   = "New format for external forcings file *.ext, link with bc-format boundary conditions specification. Supports multiple filenames separated by spaces. Filenames containing spaces must be placed inside double quotes."
                    },
                    PropertySchema {
                        .key           = "rainfall",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Include rainfall."
                    },
                    PropertySchema {
                        .key           = "qExt",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Include user Qin/out, externally provided."
                    },
                    PropertySchema {
                        .key           = "evaporation",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Include evaporation in water balance."
                    },
                    PropertySchema {
                        .key           = "windExt",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {0, "no"},
                            {1, "reserved for EC"},
                            {2, "yes"}
                        },
                        .description   = "Include wind, externally provided."
                    }
                }
            },
            SectionSchema {
                .name        = "trachytopes",
                .required    = false,
                .description = "This section contains the settings for trachytopes allowing the usage of different types of roughness formulations at different locations.",
                .properties  = {
                    PropertySchema {
                        .key           = "trtRou",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .default_value = "N",
                        .enum_values   = {
                            {0, "Y"},
                            {1, "N"}
                        },
                        .description   = "Flag for trachytopes."
                    },
                    PropertySchema {
                        .key           = "trtDef",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File (*.ttd) including trachytope definitions."
                    },
                    PropertySchema {
                        .key           = "trtL",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File (*.arl) including distribution of trachytope definitions."
                    },
                    PropertySchema {
                        .key           = "dtTrt",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "60.0",
                        .description   = "Interval for updating of bottom roughness due to trachytopes in seconds."
                    },
                    PropertySchema {
                        .key           = "trtMxR",
                        .required      = false,
                        .value_type    = ValueType::Int,
                        .default_value = "8",
                        .description   = "Maximum recursion level for composite trachytope definitions."
                    },
                    PropertySchema {
                        .key           = "trtMth",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .enum_values   = {
                            {1, "Nikuradse k based"},
                            {2, "Chezy C based (parallel and serial)"}
                        },
                        .description   = "Area averaging method."
                    },
                    PropertySchema {
                        .key           = "trtMnh",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Minimum water depth for roughness computations."
                    },
                    PropertySchema {
                        .key           = "trtCll",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Calibration factor file for roughness from trachytopes."
                    }
                }
            },
            SectionSchema {
                .name        = "output",
                .required    = false,
                .description = "This section contains the settings for various output files.",
                .properties  = {
                    PropertySchema {
                        .key           = "wrishp_crs",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing cross sections to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_dambreak",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing dambreaks to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_dryarea",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing dry areas to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_enc",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing enclosures to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_emb",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing embankments to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_fxw",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing fixed weirs to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_gate",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing gates to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_genstruc",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing general structures to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_obs",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing observation points to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_pump",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing pumps to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_src",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing sources and sinks to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_thd",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing thin dams to shape file."
                    },
                    PropertySchema {
                        .key           = "wrishp_weir",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Writing weirs to shape file."
                    },
                    PropertySchema {
                        .key           = "outputDir",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Output directory of map-, his-, rst-, dat- and timins files, default: DFM_OUTPUT_<modelname>. Set to . for no dir/current dir."
                    },
                    PropertySchema {
                        .key           = "waqOutputDir",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Output directory of Water Quality files."
                    },
                    PropertySchema {
                        .key           = "flowGeomFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "*_flowgeom.nc Flow geometry file in NetCDF format."
                    },
                    PropertySchema {
                        .key           = "obsFile",
                        .required      = false,
                        .value_type    = ValueType::PathList,
                        .description   = "Space separated list of files, containing information about observation points."
                    },
                    PropertySchema {
                        .key           = "deleteObsPointsOutsideGrid",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Delete observation points outside the grid."
                    },
                    PropertySchema {
                        .key           = "crsFile",
                        .required      = false,
                        .value_type    = ValueType::PathList,
                        .description   = "Space separated list of files, containing information about observation cross sections."
                    },
                    PropertySchema {
                        .key           = "fouFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Name of attribute file that defines the *_fou.nc Fourier output file in NetCDF format."
                    },
                    PropertySchema {
                        .key           = "fouUpdateStep",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "every user step"},
                            {1, "every computational step"},
                            {2, "equal to his output"}
                        },
                        .description   = "Fourier output type."
                    },
                    PropertySchema {
                        .key           = "hisFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "*_his.nc History file in NetCDF format."
                    },
                    PropertySchema {
                        .key           = "hisInterval",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "300.0",
                        .description   = "History output, given as 'interval' 'start period' 'end period'."
                    },
                    PropertySchema {
                        .key           = "xlsInterval",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "0.0",
                        .description   = "Interval between XLS history."
                    },
                    PropertySchema {
                        .key           = "mapFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "*_map.nc Map file in NetCDF format."
                    },
                    PropertySchema {
                        .key           = "mapInterval",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "1200.0",
                        .description   = "Map file output, given as 'interval' 'start period' 'end period'."
                    },
                    PropertySchema {
                        .key           = "rstInterval",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "0.0",
                        .description   = "Restart file output, given as 'interval' 'start period' 'end period'."
                    },
                    PropertySchema {
                        .key           = "comInterval",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "300.0",
                        .description   = "Comfile write times, given as 'interval' 'start period' 'end period' w.r.t. `refDate`."
                    },
                    PropertySchema {
                        .key           = "mapFormat",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "4",
                        .enum_values   = {
                            {1, "NetCDF"},
                            {4, "NetCDF UGRID"}
                        },
                        .description   = "Map file format."
                    },
                    PropertySchema {
                        .key           = "ncFormat",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "3",
                        .enum_values   = {
                            {3, "classic"},
                            {4, "NetCDF4+HDF5"}
                        },
                        .description   = "Format for all NetCDF output files."
                    },
                    PropertySchema {
                        .key           = "ncMapDataPrecision",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .default_value = "single",
                        .enum_values   = {
                            {0, "double"},
                            {1, "single"}
                        },
                        .description   = "Precision for NetCDF data in map files (double or single)."
                    },
                    PropertySchema {
                        .key           = "ncHisDataPrecision",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .default_value = "single",
                        .enum_values   = {
                            {0, "double"},
                            {1, "single"}
                        },
                        .description   = "Precision for NetCDF data in his files (double or single)."
                    },
                    PropertySchema {
                        .key           = "ncCompression",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Apply compression to NetCDF output files. Only works when `ncFormat`=4."
                    },
                    PropertySchema {
                        .key           = "ncNoUnlimited",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write full-length time-dimension instead of unlimited dimension. Might require `ncFormat`=4."
                    },
                    PropertySchema {
                        .key           = "ncNoForcedFlush",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Do not force flushing of map-like files every output timestep."
                    },
                    PropertySchema {
                        .key           = "ncWriteLatLon",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write extra lat-lon coordinates for all projected coordinate variables in each NetCDF file (for CF-compliancy)."
                    },
                    PropertySchema {
                        .key           = "wriHis_balance",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write mass balance totals to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_gen",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write general structure parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_dam",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write dam parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_pump",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write pump parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_gate",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write gate parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_weir",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write weir parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_orifice",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write orifice parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_bridge",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write bridge parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_culvert",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write culvert parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_longculvert",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write long culvert parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_damBreak",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write dam break parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_uniWeir",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write universal weir parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_structure_compound",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write compound structure parameters to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_lateral",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write lateral data."
                    },
                    PropertySchema {
                        .key           = "wriHis_velocity",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write velocity magnitude in observation point to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_discharge",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write discharge magnitude in observation point to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_sourcesink",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write sources-sinks statistics to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_turbulence",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write k, eps and vicww to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_wind",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write wind velocities to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_rain",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write precipitation to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_airdensity",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write air density to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_infiltration",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write infiltration to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_temperature",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write temperature to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_waves",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write wave data to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_heat_fluxes",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write heat fluxes to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_salinity",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write salinity to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_density",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write density to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_waterlevel_s1",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write water level to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_bedlevel",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write bed level to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_waterdepth",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write water depth to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_velocity_vector",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write velocity vectors to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_upward_velocity_component",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write upward velocity to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_sediment",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write sediment transport to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_constituents",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write tracers to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_zcor",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write vertical coordinates to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_taucurrent",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write mean bed shear stress to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_wqBot",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write water quality bottom variables to his file."
                    },
                    PropertySchema {
                        .key           = "wriHis_wqBot3d",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write 3D water quality bottom variables to his file."
                    },
                    PropertySchema {
                        .key           = "wriMap_waterlevel_s0",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write water levels at old time level to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_waterlevel_s1",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write water levels at new time level to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_evaporation",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write evaporation to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_velocity_component_u0",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write velocities at old time level to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_velocity_component_u1",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write velocities at new time level to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_velocity_vector",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write cell-center velocity vectors to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_upward_velocity_component",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write upward velocity component to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_density_rho",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write density to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_horizontal_viscosity_viu",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write horizontal viscosity to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_horizontal_diffusivity_diu",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write horizontal diffusivity to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_flow_flux_q1",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write fluxes to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_spiral_flow",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write spiral flow to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_numlimdt",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write numlimdt to map file."
                    },
                    PropertySchema {
                        .key           = "wriXyz_numlimdt",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write numlimdt to xyz file This option is useful when a map file is not written."
                    },
                    PropertySchema {
                        .key           = "wriMap_taucurrent",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write bottom friction to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_chezy",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write chezy roughness in flow elements to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_chezy_on_flow_links",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write chezy roughness on flow links to map file."
                    },
                    PropertySchema {
                        .key           = "writePart_domain",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write partition domain info, for postprocessing."
                    },
                    PropertySchema {
                        .key           = "velocityDirectionClassesInterval",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Class map's step size of class values for velocity direction."
                    },
                    PropertySchema {
                        .key           = "velocityMagnitudeClasses",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "0.0",
                        .description   = "Class map's list of class values for velocity magnitudes."
                    },
                    PropertySchema {
                        .key           = "wriMap_input_roughness",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write chezy input roughness on flow links to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_turbulence",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write turbulence to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_rain",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write rainfall rate to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_wind",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write winds to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_airdensity",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write air density to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_calibration",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write roughness calibration factors to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_salinity",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write salinity to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_temperature",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write temperature to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_constituents",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write tracers and others constituents to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_sediment",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write sediment transport to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_waves",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write wave variables to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_z0",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write current-related roughness height to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_trachytopes",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write roughness from trachytopes to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_nudging",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write nudging to map file."
                    },
                    PropertySchema {
                        .key           = "wriTek_cdWind",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write wind friction coefficients to tek file."
                    },
                    PropertySchema {
                        .key           = "wriMap_heat_fluxes",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write heat fluxes to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_fixed_weir_energy_loss",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write energy losses of fixed weirs to map file. `wriMap_waterdepth_on_ground` and `wriMap_volume_on_ground`."
                    },
                    PropertySchema {
                        .key           = "wriMap_wet_waterdepth_threshold",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "2e-05",
                        .description   = "Waterdepth threshold above which a grid point counts as 'wet'. Defaults to 0.2*`epshu`. It is used for `wriMap_time_water_on_ground`, `wriMap_waterdepth_on_ground` and `wriMap_volume_on_ground`."
                    },
                    PropertySchema {
                        .key           = "wriMap_time_water_on_ground",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write cumulative time when water is above ground level (only for 1D nodes) to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_freeboard",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write freeboard (only for 1D nodes) to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_waterdepth_on_ground",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write waterdepth that is above ground level to map file (only for 1D nodes)."
                    },
                    PropertySchema {
                        .key           = "wriMap_volume_on_ground",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write volume that is above ground level to map file (only for 1D nodes)."
                    },
                    PropertySchema {
                        .key           = "wriMap_total_net_inflow_1d2d",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write current total 1D2D net inflow (discharge) and cumulative total 1D2D net inflow (volume) to map file (only for 1D nodes)."
                    },
                    PropertySchema {
                        .key           = "wriMap_total_net_inflow_lateral",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write current total lateral net inflow (discharge) and cumulative total lateral net inflow (volume) to map file (only for 1D nodes)."
                    },
                    PropertySchema {
                        .key           = "wriMap_water_level_gradient",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write water level gradient to map file (only for 1D links)."
                    },
                    PropertySchema {
                        .key           = "wriMap_tidal_potential",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write tidal potential to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_sal_potential",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write self attraction and loading potential to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_internal_tides_dissipation",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write internal tides dissipation to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_flow_analysis",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write flow analysis data to the map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_volume1",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write volumes to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_waterdepth",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write water depths to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_waterdepth_hu",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write water depths on u-points to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_ancillary_variables",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write ancillary variables attributes to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_flowarea_au",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write low areas au to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_velocity_magnitude",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write cell-center velocity vector magnitude to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_velocity_vectorq",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write cell-center velocity vectors (discharge-based) to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_flow_flux_q1_main",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write flow flux in main channel to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_interception",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write interception to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_windstress",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write wind stress to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_cdWind",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Write wind friction coeffs to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_bnd",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write boundary points to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_Qin",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write sum of all influxes to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_dtCell",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write time step per cell based on CFL."
                    },
                    PropertySchema {
                        .key           = "wriMap_wqBot3d",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write 3D water quality bottom variables to map file."
                    },
                    PropertySchema {
                        .key           = "wriMap_every_dt",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write output to map file every computational timestep, between start and stop time from `mapInterval`."
                    },
                    PropertySchema {
                        .key           = "mapOutputTimeVector",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File (.mpt) containing fixed map output times (s) w.r.t. `refDate`."
                    },
                    PropertySchema {
                        .key           = "comOutputTimeVector",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File (.ctv) containing fixed comfile write times (s) w.r.t. `refDate`."
                    },
                    PropertySchema {
                        .key           = "fullGridOutput",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "compact"},
                            {1, "full time-varying grid layer data"}
                        },
                        .description   = "Full grid output mode for layer positions."
                    },
                    PropertySchema {
                        .key           = "eulerVelocities",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write Eulerian velocities."
                    },
                    PropertySchema {
                        .key           = "classMapFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Name of class map file."
                    },
                    PropertySchema {
                        .key           = "waterLevelClasses",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "0.0",
                        .description   = "Series of values between which water level classes are computed."
                    },
                    PropertySchema {
                        .key           = "waterDepthClasses",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "0.0",
                        .description   = "Series of values between which water depth classes are computed."
                    },
                    PropertySchema {
                        .key           = "classMapInterval",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "0.0",
                        .description   = "Interval between class map file outputs."
                    },
                    PropertySchema {
                        .key           = "waqInterval",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "0.0",
                        .description   = "Interval between DELWAQ file outputs."
                    },
                    PropertySchema {
                        .key           = "statsInterval",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "-60.0",
                        .description   = "Interval between screen step outputs in seconds simulation time, if negative in seconds wall clock time."
                    },
                    PropertySchema {
                        .key           = "timingsInterval",
                        .required      = false,
                        .value_type    = ValueType::FloatList,
                        .default_value = "0.0",
                        .description   = "Timings output interval."
                    },
                    PropertySchema {
                        .key           = "richardsonOnOutput",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Write Richardson number."
                    },
                    PropertySchema {
                        .key           = "mbaLumpSourceSinks",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Lump MBA source/sink mass balance terms."
                    },
                    PropertySchema {
                        .key           = "wrimap_nearfield",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Write NearField parameters."
                    },
                    PropertySchema {
                        .key           = "writeDfmInterpretedValues",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Write DFM interpreted values."
                    },
                    PropertySchema {
                        .key           = "mbaLumpBoundaries",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Lump MBA boundary mass balance terms."
                    },
                    PropertySchema {
                        .key           = "waqHorAggr",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "DELWAQ output horizontal aggregation file (*.dwq)."
                    },
                    PropertySchema {
                        .key           = "writeDetailedTimers",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Write detailed timers output file."
                    },
                    PropertySchema {
                        .key           = "metadataFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Metadata NetCDF file with user-defined global dataset attributes (*_meta.nc)."
                    },
                    PropertySchema {
                        .key           = "mbaInterval",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .description   = "Mass balance area output interval."
                    },
                    PropertySchema {
                        .key           = "wrirst_bnd",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Write water level."
                    },
                    PropertySchema {
                        .key           = "generateUuid",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Generate UUID as unique dataset identifier and include in output NetCDF files."
                    },
                    PropertySchema {
                        .key           = "timeSplitInterval",
                        .required      = false,
                        .value_type    = ValueType::String,
                        .description   = "Time splitting interval after which a new output file is started. Format: value+unit, e.g. '1M'."
                    },
                    PropertySchema {
                        .key           = "rugFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Polyline file *_rug.pli defining runup gauges."
                    },
                    PropertySchema {
                        .key           = "mbaWriteCsv",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Write mass balance area output to a CSV file."
                    },
                    PropertySchema {
                        .key           = "mbaLumpFromToMba",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Lump MBA from/to other areas mass balance terms."
                    },
                    PropertySchema {
                        .key           = "mbaLumpProcesses",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Lump MBA processes mass balance terms."
                    },
                    PropertySchema {
                        .key           = "waqVertAggr",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "DELWAQ output vertical aggregation file (*.vag)."
                    },
                    PropertySchema {
                        .key           = "mbaWriteNetcdf",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Write mass balance area output to a NetCDF file."
                    },
                    PropertySchema {
                        .key           = "mbaWriteTxt",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Write mass balance area output to a TXT file."
                    },
                    PropertySchema {
                        .key           = "wrimap_ice",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Write output to map file for ice cover."
                    }
                }
            },
            SectionSchema {
                .name        = "calibration",
                .required    = false,
                .description = "This section contains roughness calibration settings.",
                .properties  = {
                    PropertySchema {
                        .key           = "useCalibration",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Activate calibration factor friction multiplier."
                    },
                    PropertySchema {
                        .key           = "definitionFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File (*.cld) containing calibration definitions."
                    },
                    PropertySchema {
                        .key           = "areaFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "File (*.cll) containing area distribution of calibration definitions."
                    }
                }
            },
            SectionSchema {
                .name        = "processes",
                .required    = false,
                .description = "This section settings for online water quality processes.",
                .properties  = {
                    PropertySchema {
                        .key           = "substanceFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Substance file name."
                    },
                    PropertySchema {
                        .key           = "substanceDensityCoupling",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .description   = "Substance rho coupling."
                    },
                    PropertySchema {
                        .key           = "additionalHistoryOutputFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Extra history output filename."
                    },
                    PropertySchema {
                        .key           = "statisticsFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Statistics definition file."
                    },
                    PropertySchema {
                        .key           = "thetaVertical",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Theta value for vertical transport of water quality substances."
                    },
                    PropertySchema {
                        .key           = "dtProcesses",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Waq processes time step. Must be a multiple of `dtUser`. If `dtProcesses` is negative, water quality processes are calculated with every hydrodynamic time step."
                    },
                    PropertySchema {
                        .key           = "processFluxIntegration",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "1",
                        .enum_values   = {
                            {1, "WAQ"},
                            {2, "D-Flow FM"}
                        },
                        .description   = "Process fluxes integration option."
                    },
                    PropertySchema {
                        .key           = "volumeDryThreshold",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.001",
                        .description   = "Volume below which segments are marked as dry."
                    },
                    PropertySchema {
                        .key           = "depthDryThreshold",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.001",
                        .description   = "Water depth below which segments are marked as dry."
                    }
                }
            },
            SectionSchema {
                .name        = "particles",
                .required    = false,
                .description = "This section contains settings for particle tracking.",
                .properties  = {
                    PropertySchema {
                        .key           = "particlesFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Initial particle locations file (*.xyz)."
                    },
                    PropertySchema {
                        .key           = "particlesReleaseFile",
                        .required      = false,
                        .value_type    = ValueType::Path,
                        .description   = "Particles release file (*.tim, 4 column)."
                    },
                    PropertySchema {
                        .key           = "addTracer",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "0",
                        .description   = "Add tracer or not."
                    },
                    PropertySchema {
                        .key           = "startTime",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Start time (if > 0)."
                    },
                    PropertySchema {
                        .key           = "timeStep",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Time step (if > 0) or every computational time step."
                    },
                    PropertySchema {
                        .key           = "3DType",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "depth averaged velocities"},
                            {1, "free surface/top layer velocities"}
                        },
                        .description   = "3D velocity type."
                    }
                }
            },
            SectionSchema {
                .name        = "veg",
                .required    = false,
                .description = "This section contains setting for a dynamic vegetation model",
                .properties  = {
                    PropertySchema {
                        .key           = "vegetationModelNr",
                        .required      = false,
                        .value_type    = ValueType::IntEnum,
                        .default_value = "0",
                        .enum_values   = {
                            {0, "no"},
                            {1, "Baptist DFM"}
                        },
                        .description   = "Vegetation model nr."
                    },
                    PropertySchema {
                        .key           = "clVeg",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.8",
                        .description   = "Stem distance factor."
                    },
                    PropertySchema {
                        .key           = "cdVeg",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.7",
                        .description   = "Stem Cd coefficient."
                    },
                    PropertySchema {
                        .key           = "cbVeg",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Stem stiffness coefficient."
                    },
                    PropertySchema {
                        .key           = "rhoVeg",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Stem Rho, if > 0, bouyant stick procedure."
                    },
                    PropertySchema {
                        .key           = "stemHeightStd",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Stem height standard deviation fraction, e.g. 0.1."
                    },
                    PropertySchema {
                        .key           = "stemHeightConvention",
                        .required      = false,
                        .value_type    = ValueType::Enum,
                        .default_value = "upward_from_bed",
                        .enum_values   = {
                            {0, "upward_from_bed"},
                            {1, "downward_from_surface"}
                        },
                        .description   = "Stem height convention."
                    },
                    PropertySchema {
                        .key           = "densVegMinBap",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "Minimum vegeation density in Baptist formula. Only in 2D."
                    },
                    PropertySchema {
                        .key           = "expChiStem",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "TODO."
                    },
                    PropertySchema {
                        .key           = "expChiLeaf",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "TODO."
                    },
                    PropertySchema {
                        .key           = "uChiStem",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "TODO."
                    },
                    PropertySchema {
                        .key           = "uChiLeaf",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "TODO."
                    },
                    PropertySchema {
                        .key           = "areaLeaf",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "TODO."
                    },
                    PropertySchema {
                        .key           = "cdLeaf",
                        .required      = false,
                        .value_type    = ValueType::Float,
                        .default_value = "0.0",
                        .description   = "TODO."
                    }
                }
            }
        }
    };

} // namespace dflowfm_io
