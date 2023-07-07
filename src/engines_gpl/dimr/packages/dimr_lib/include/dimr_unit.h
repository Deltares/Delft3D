#pragma once
#include "dimr_component.h"
#include "dimr_coupler.h"
// OR the component pointer is defined (coupler pointer is NULL) OR the other way around
typedef struct dimr_unit dimr_unit;
struct dimr_unit {
	dimr_component * component;
	dimr_coupler   * coupler;
};
