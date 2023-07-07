#define ENGINE_INTERNAL_EXPORT // force LINKTOADDON to export insead of import
#include "TECADDON.h"
#include "tp360addonuiaction_Exports.h"

#if defined LINKTOADDON
#undef LINKTOADDON
#endif

#define LINKTOADDON extern "C" tp360addonuiaction_API

LINKTOADDON Boolean_t STDCALL TecUtilMenuAddOption(
    const char *MenuPath,
    const char            *MenuLabel,
    char                   Mnemonic,
    DynamicMenuCallback_pf MenuOptionCallback);

LINKTOADDON Boolean_t STDCALL TecUtilMenuAddSeparator(const char *MenuPath);

LINKTOADDON void STDCALL TecUtilMenuSetSensitivity(
    const char *MenuPath,
    const char *MenuLabel,
    Boolean_t   IsSensitive);
