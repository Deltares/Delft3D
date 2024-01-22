#include "version_definition.h"

#define MAJOR 4
#define MINOR 6

#define MAJOR_STR "4"
#define MINOR_STR "06"

#define VERSION_FULL MAJOR_STR "." MINOR_STR "-" BUILD_NR

#define PRODUCT_NAME "ESMFSM"

#define MOD_NAME       ESMFSM          /* Will be added to the function names */
#if HAVE_CONFIG_H
#   define F90_MOD_NAME   FC_FUNC(esmfsm, ESMFSM)
#else
#   define F90_MOD_NAME   MOD_NAME
#endif



