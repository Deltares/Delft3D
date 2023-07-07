#pragma once

/* CORE SOURCE CODE REMOVED */

#define Tec360EditionName   "2014"      /* ""     for beta releases, or the year for production release */
#define Tec360ReleaseName   "Release 2" /* "Beta" for beta releases, or "Release #" for production releases*/
#define Tec360ReleaseAbbr   "R2"        /* "Beta" for beta releases, or "R#" for production releases */

/*
 * Some consumers of this version information need integers representing the
 * individual parts of the version string. Other consumers want a single string
 * with everything already in it. To support both of these use cases, we define
 * each version component individually then use a preprocessor trick to combine
 * those individual components into a single version string. See
 * http://stackoverflow.com/questions/240353/convert-a-preprocessor-token-to-a-string
 * for details on the preprocessor trick.
 */
#define TP_STRINGIZE2(s) #s
#define TP_STRINGIZE(s) TP_STRINGIZE2(s)

#define TecplotMajorVer            14
#define TecplotMinorVer            2
#define TecplotMajorRev            0
#define TecplotReleaseYear         2014
#define TecplotReleaseMonth        09   // Must include a leading zero for single-digit values (see request #29649)
#include "TecplotMinorRev.h"

#define TecplotAPIVersion          141    /* ...updated when the macro or TecUtil language changes or introduces new functionality and is most often a conjunction of TecplotMajorVer and TecplotMinorVer numbers but may vary */

#define TecplotMajorVerStr         TP_STRINGIZE(TecplotMajorVer)
#define TecplotMinorVerStr         TP_STRINGIZE(TecplotMinorVer)
#define TecplotMajorRevStr         TP_STRINGIZE(TecplotMajorRev)
#define TecplotMinorRevStr         TP_STRINGIZE(TecplotMinorRev)

#define TecplotLicenseVersionStr   TP_STRINGIZE(TecplotReleaseYear) "." TP_STRINGIZE(TecplotReleaseMonth)
#define TecplotAPIVersionStr       TP_STRINGIZE(TecplotAPIVersion)

#define TecVersionId TecplotMajorVerStr "." TecplotMinorVerStr "." TecplotMajorRevStr "." TecplotMinorRevStr
#define TecCopyright "Copyright (c) 1988-2014 Tecplot, Inc."
#define TecCurrentYear "2014"

// TODO (SMO) M 2012/8/9: this will need to change when we enable the tecplot focus flavor.
#define TecplotProductName "Tecplot 360 EX"
