#pragma once
typedef struct netcdf_references netcdf_references;
struct netcdf_references
{
	int   strlenDim;
	int   timeDim;
	int   timeVar;
	int * item_values;
	int * item_variables;
};
