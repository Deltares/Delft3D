//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2025.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
// $Id: dimr_lib_version.cpp 933 2011-10-25 10:01:26Z mourits $
// $HeadURL: $
//------------------------------------------------------------------------------

#include <stdio.h>
#include "dimr_lib_version.h"

#if defined(_WIN64)
static char modname_version_id[] = { "@(#)Deltares, " modname_program " Version " modname_major "." modname_minor "." modname_build " (Win64), " __DATE__ ", " __TIME__ "" };
#elif defined(LINUX64)
static char modname_version_id[] = { "@(#)Deltares, " modname_program " Version " modname_major "." modname_minor "." modname_build " (Linux64), " __DATE__ ", " __TIME__ "" };
#else
static char modname_version_id[] = { "@(#)Deltares, " modname_program " Version " modname_major "." modname_minor "." modname_build " (Unknown), " __DATE__ ", " __TIME__ "" };
#endif


char * getversionstring_dimr_lib(void)
{
	return modname_version;
}

char * getfullversionstring_dimr_lib(void)
{
	return modname_version_full;
}

char * getshortversionstring_dimr_lib(void)
{
	return modname_version_short;
}

char * geturlstring_dimr_lib(void)
{
	return modname_url;
}

char * getversionidstring_dimr_lib(void)
{
	return modname_version_id;
}
