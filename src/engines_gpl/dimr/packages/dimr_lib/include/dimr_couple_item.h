#pragma once
// Each item to be communicated between components
// Corresponds with an item in config.xml
typedef struct dimr_couple_item dimr_couple_item;
struct dimr_couple_item {
	const char * sourceName;         // as written in config.xml
	const char * targetName;         // idem
	int          sourceProcess;      // id of Process that can deliver this item; should be exactly one
	int          targetProcess;      // id of first Process that can accept this item; can be more than one
	double     * sourceVarPtr;       // Pointer to the related variable inside the component instance (result of getVar)
	double     * targetVarPtr;       // idem
};
