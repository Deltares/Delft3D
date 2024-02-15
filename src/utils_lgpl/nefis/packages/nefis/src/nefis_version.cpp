//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2024.
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
// $Id$
// $HeadURL$
//#include <stdio.h>
#include "nefis_version.h"

#if defined(W32)
static char libnefis_version_id [] = {"@(#)Deltares, " COMPONENT_NAME " Version " NEFIS_MAJOR "." NEFIS_MINOR "." NEFIS_REVISION "." BUILD_NR " (Win32), " __DATE__ ", " __TIME__ ""};
#elif defined(W64)
static char libnefis_version_id [] = {"@(#)Deltares, " COMPONENT_NAME " Version " NEFIS_MAJOR "." NEFIS_MINOR "." NEFIS_REVISION "." BUILD_NR " (Win64), " __DATE__ ", " __TIME__ ""};
#elif defined(LINUX64)
static char libnefis_version_id [] = {"@(#)Deltares, " COMPONENT_NAME " Version " NEFIS_MAJOR "." NEFIS_MINOR "." NEFIS_REVISION "." BUILD_NR " (Linux64), " __DATE__ ", " __TIME__ ""};
#else
static char libnefis_version_id [] = {"@(#)Deltares, " COMPONENT_NAME " Version " NEFIS_MAJOR "." NEFIS_MINOR "." NEFIS_REVISION "." BUILD_NR " (Unknown), " __DATE__ ", " __TIME__ ""};
#endif
static char nefis_file_version [] = {"5.00.00"};
static char nefis_company_name [] = {"Deltares"};

extern "C" {
    char * getfullversionstring_nefis(void)
    {
        return libnefis_version_id;
    };
    char * getfileversionstring_nefis(void)
    {
        return nefis_file_version;
    };
    char * getcompanystring_nefis(void)
    {
        return nefis_company_name;
    };
}
