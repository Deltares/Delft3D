#include "version_definition.h"

#ifndef RTCTOOLS_VERSION
#define RTCTOOLS_VERSION

#if defined _WIN32
#define dllexp extern "C" __declspec(dllexport)
#else
#define dllexp extern "C"
#endif

#define STRINGIFY(x) #x
#define TOSTRING(x) STRINGIFY(x)

#define OSS_Version  "1.6.1"
#define OSS_Revision BUILD_NR
#define PRODUCT_NAME "FBC-Tools"
#define VERSION_FULL OSS_Version "." OSS_Revision

#if defined _WIN64
static char rtctools_version [] = {OSS_Version "." OSS_Revision " (Win64)"};
static char rtctools_version_id [] = {"@(#)Deltares, " PRODUCT_NAME" Version " OSS_Version "." OSS_Revision " (Win64), " __DATE__", " __TIME__""};
dllexp extern void get_attribute(const char *, char *);
dllexp extern char * get_rtctools_version(void);
dllexp extern char * get_rtctools_version_id(void);
#elif defined _WIN32
static char rtctools_version [] = {OSS_Version "." OSS_Revision " (Win32)"};
static char rtctools_version_id [] = {"@(#)Deltares, " PRODUCT_NAME" Version " OSS_Version "." OSS_Revision " (Win32), " __DATE__", " __TIME__""};
dllexp extern void get_attribute(const char *, char *);
dllexp extern char * get_rtctools_version(void);
dllexp extern char * get_rtctools_version_id(void);
#else
static char rtctools_version [] = {OSS_Version "." OSS_Revision " (Linux64)"};
static char rtctools_version_id [] = {"@(#)Deltares, " PRODUCT_NAME" Version " OSS_Version "." OSS_Revision " (Linux64), " __DATE__", " __TIME__""};
extern "C" {
extern void get_attribute(const char *, char *);
extern char * get_rtctools_version(void);
extern char * get_rtctools_version_id(void);
}
#endif

#endif
