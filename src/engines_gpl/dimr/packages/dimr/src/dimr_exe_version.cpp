//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: dimr_exe_version.cpp 932 2011-10-25 09:41:59Z mourits $
// $HeadURL: $
#include <stdio.h>
#include "dimr_exe_version.h"

#ifndef min
#  define min(a,b) (a)<(b) ? (a) : (b)
#  define max(a,b) (a)>(b) ? (a) : (b)
#endif

#if defined(_WIN64)
static char modname_version_id [] = {"@(#)Deltares, " modname_program " Version " modname_major "." modname_minor "." modname_build " (Win64), " __DATE__ ", " __TIME__ ""};
#elif defined(LINUX64)
static char modname_version_id [] = {"@(#)Deltares, " modname_program " Version " modname_major "." modname_minor "." modname_build " (Linux64), " __DATE__ ", " __TIME__ ""};
#else
static char modname_version_id[] = { "@(#)Deltares, " modname_program " Version " modname_major "." modname_minor "." modname_build " (Unknown), " __DATE__ ", " __TIME__ "" };
#endif


char * getversionstring_dimr_exe(void)
{
    return modname_version;
}

char * getfullversionstring_dimr_exe(void)
{
    return modname_version_full;
}

char * getshortversionstring_dimr_exe(void)
{
    return modname_version_short;
}

char * geturlstring_dimr_exe(void)
{
    return modname_url;
}

char * getversionidstring_dimr_exe(void)
{
	return modname_version_id;
}
