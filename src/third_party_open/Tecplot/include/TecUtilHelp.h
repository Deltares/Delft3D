#pragma once

#define ENGINE_INTERNAL_EXPORT // force LINKTOADDON to export insead of import
#include "TECADDON.h"
#include "tp360addonuiaction_Exports.h"

#if defined LINKTOADDON
#undef LINKTOADDON
#endif

#define LINKTOADDON extern "C" tp360addonuiaction_API

LINKTOADDON void STDCALL TecUtilHelp(
	const char * helpFileOrURL,
	Boolean_t goToID,
	int helpID);
