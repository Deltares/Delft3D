#include <dflowfm_io/MduSchemaData.h>

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
                        .key         = "fileVersion",
                        .required    = true,
                        .value_type  = ValueType::String,
                        .description = "File version. Do not edit this."
                    },
                }
            },
            SectionSchema {
                .name        = "geometry",
                .required    = true,
                .description = "In this section, the main entry comprises the specification of the grid (i.e. the netCDF network file). In addition, thin dams and thin dykes can be specified.",
                .properties  = {
                    PropertySchema {
                        .key         = "netFile",
                        .required    = true,
                        .value_type  = ValueType::Path,
                        .description = "Net file (*_net.nc) containing mesh information."
                    },
                    PropertySchema {
                        .key           = "useCaching",
                        .required      = false,
                        .value_type    = ValueType::IntBool,
                        .default_value = "1",
                        .description   = "Use caching for geometrical/network-related items."
                    },
                    PropertySchema {
                        .key           = "kmx",
                        .required      = false,
                        .value_type    = ValueType::Integer,
                        .default_value = "0",
                        .description   = "Number of vertical layers. NB. If keyword `zLayerGrowthFactor` is used, then number of layers is determined by D-Flow FM."
                    },
                    PropertySchema {
                        .key           = "waterLevIni",
                        .required      = false,
                        .value_type    = ValueType::FloatingPoint,
                        .default_value = "0.0",
                        .description   = "Initial water levels sample file (*.xyz)."
                    },
                    PropertySchema {
                        .key         = "dryPointsFile",
                        .required    = false,
                        .value_type  = ValueType::PathList,
                        .description = "Dry points file (*.xyz), third column dummy z values, or polygon file (*.pol)."
                    },
                }
            },
            SectionSchema {
                .name        = "numerics",
                .required    = true,
                .description = "This section contains the settings of specific parts of the flow solver, such as limiters and the iterative solver type.",
                .properties  = {
                    PropertySchema {
                        .key           = "cflMax",
                        .required      = false,
                        .value_type    = ValueType::FloatingPoint,
                        .default_value = "0.7",
                        .description   = "Maximum Courant nr."
                    },
                }
            },
            SectionSchema {
                .name        = "wind",
                .required    = false,
                .description = "The wind section prescribes the dependency of the wind drag coefficient to the wind velocity through 2 or 3 breakpoints. This field also contains pressure information",
                .properties  = {
                    PropertySchema {
                        .key           = "cdBreakPoints",
                        .required      = false,
                        .value_type    = ValueType::FloatingPointList,
                        .default_value = "0.00063, 0.00723",
                        .description   = "Wind drag breakpoints."
                    },
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
                }
            },
        }
    };

} // namespace dflowfm_io