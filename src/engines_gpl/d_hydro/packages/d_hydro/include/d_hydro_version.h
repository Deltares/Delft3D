#ifndef D_HYDRO_EXE_VERSION
#define D_HYDRO_EXE_VERSION

#define CAT(a, b) a ## b
#define FUNC_CAT(a, b) CAT(a, b)

#define MOD_NAME         D_HYDRO_EXE
#define modname_program  "D_HYDRO_EXE"
#if HAVE_CONFIG_H
#   define F90_MOD_NAME   FC_FUNC(d_hydro, D_HYDRO)
#else
#   define F90_MOD_NAME   MOD_NAME
#endif

#include "version_definition.h"

#define modname_major "3"
#define modname_minor "02"
#define modname_build BUILD_NR

#define modname_company COMPANY_NAME
#define modname_company_url COMPANY_URL

#define modname_sourcecode_url "@(#) BRANCH"

#define modname_version modname_major "." modname_minor "." modname_build
#define modname_version_short modname_major "." modname_minor
#define modname_version_full  modname_company ", " modname_program " Version " modname_major "." modname_minor "." modname_build ", " __DATE__ ", " __TIME__ ""
#define modname_url modname_sourcecode_url

/*=================================================== DO NOT MAKE CHANGES BELOW THIS LINE ===================================================================== */

char * getversionstring_d_hydro(void);
char * getfullversionstring_d_hydro(void);
char * getshortversionstring_d_hydro(void);
char * geturlstring_d_hydro(void);
char * getversionidstring_d_hydro(void);

#endif /* DIMR_EXE_VERSION */

