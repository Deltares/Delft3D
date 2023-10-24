#pragma once
#include "dimr_couple_item.h"
#include "dimr_logger.h"
// A coupler defines the communication between two components, one coupler for each direction
// Corresponds with a coupler block in config.xml
typedef struct dimr_coupler dimr_coupler;
struct dimr_coupler {
	const char       *   name;                // Coupler name: must be unique in the config.xml file (e.g. rtc2flow)
	char             *   sourceComponentName; // Name of the component providing data to be communicated by the coupler
	char             *   targetComponentName; // Name of the component receiving data to be communicated by the coupler
	dimr_component   *   sourceComponent;     // Pointer to the related component
	dimr_component   *   targetComponent;     // idem
	unsigned int         numItems;
	dimr_couple_item *   items;               // Array of items defining this coupler
	unsigned int     *   itemTypes;           // Array of type of each item
	dimr_logger      *   logger;
};
