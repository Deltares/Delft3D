#pragma once
enum {
	CT_START = 1, // Used to identify the type of a ControlBlock
	CT_STARTGROUP = 2,
	CT_SEQUENTIAL = 3,
	CT_PARALLEL = 4,
	CT_COUPLER = 5
};

enum {
	COMP_TYPE_DEFAULT_BMI = 0,
	COMP_TYPE_FM = 1, // Used to identify the type of a Component
	COMP_TYPE_RTC = 2,
	COMP_TYPE_WAVE = 3,
	COMP_TYPE_FLOW1D = 4,
	COMP_TYPE_WANDA = 5,
	COMP_TYPE_FLOW2D3D = 6,
	COMP_TYPE_FLOW1D2D = 7,
	COMP_TYPE_DELWAQ = 8,
	COMP_TYPE_RR = 9,
	COMP_TYPE_COSUMO_BMI = 10,
	COMP_TYPE_ZSF = 11,
	COMP_TYPE_TEST = 12
};

enum {
	ITEM_TYPE_SCALAR = 0,
	ITEM_TYPE_PTR = 1
};

enum {
	GLOBAL_PHASE_INIT = 1, // Init   of first control block
	GLOBAL_PHASE_UPDATE = 2, // Update of first control block
	GLOBAL_PHASE_FINISH = 3  // Finish of first control block and init+update+finish of all other control blocks
};

enum {
	MAXSTRING = 1000    // max string length in bytes
};

// Store the exact name of the entry points in the dlls
const char BmiDimrSetLogger[] = "set_dimr_logger";
const char BmiInitializeEntryPoint[] = "initialize";
const char BmiUpdateEntryPoint[] = "update";
const char BmiFinalizeEntryPoint[] = "finalize";
const char BmiGetStartTimeEntryPoint[] = "get_start_time";
const char BmiGetEndTimeEntryPoint[] = "get_end_time";
const char BmiGetTimeStepEntryPoint[] = "get_time_step";
const char BmiGetCurrentTimeEntryPoint[] = "get_current_time";
const char BmiGetVarEntryPoint[] = "get_var";
const char BmiSetVarEntryPoint[] = "set_var";
const char BmiSetLogger[] = "set_logger";
const char BmiSetLogger2[] = "set_logger_c_callback";
const char BmiGetAttributeEntryPoint[] = "get_attribute";
const char BmiGetVarShapeEntryPoint[] = "get_var_shape";
