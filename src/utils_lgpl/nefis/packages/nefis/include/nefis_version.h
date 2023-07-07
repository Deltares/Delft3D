#ifndef NEFIS_VERSION
#define NEFIS_VERSION

#define NEFIS_MAJOR "5"
#define NEFIS_MINOR "09"
#define NEFIS_REVISION "00"
#define COMPONENT_NAME "nefis"

#include "version_definition.h"
/*=================================================== DO NOT MAKE CHANGES BELOW THIS LINE ===================================================================== */


extern "C" {
    extern char * getfullversionstring_nefis(void);
    extern char * getfileversionstring_nefis(void);
    extern char * getcompanystring_nefis(void);
}

#endif /* NEFIS_VERSION */
