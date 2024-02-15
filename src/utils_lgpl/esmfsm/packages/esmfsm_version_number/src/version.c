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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "version_number.h"
#define CAT(a, b) a ## b
#define FUNC_CAT(a, b) CAT(a, b)

#if defined WIN32
#define strdup _strdup
#endif

static char ident   [] = {"@(#)"COMPANY_NAME", "PRODUCT_NAME" Version "VERSION_FULL", "__DATE__", "__TIME__""};
static char company [] = {COMPANY_NAME};
static char file_version [] = {VERSION_FULL};
static char version [] = {VERSION_FULL};
static char program_name [] = {PRODUCT_NAME};
static char short_program_name [] = {PRODUCT_NAME};
static char git_revision [] = {BUILD_NR};

/*==========================================================================*/
const char * FUNC_CAT( version_getFileVersionString_, MOD_NAME)()
{
    return file_version;
}
/*==========================================================================*/
const char * FUNC_CAT( version_getFullVersionString_, MOD_NAME)()
{
    return ident;
}

/*==========================================================================*/
const char * FUNC_CAT( version_getCompanyString_, MOD_NAME)()
{
    return company;
}

/*==========================================================================*/
const char * FUNC_CAT( version_getVersionNumberString_, MOD_NAME)()
{
    return version;
}
/*==========================================================================*/
const char * FUNC_CAT( version_getProgramNameString_, MOD_NAME)()
{
    return program_name;
}
/*==========================================================================*/
const char * FUNC_CAT( version_getShortProgramNameString_, MOD_NAME)()
{
    return short_program_name;
}
/*==========================================================================*/
const char * FUNC_CAT( version_getSvnRevisionString_, MOD_NAME)()
{
    return git_revision;
}
/*==========================================================================*/
const char * FUNC_CAT( version_getFeatureNumberString_, MOD_NAME)()
{
    char * feature_nr;
    long i, j, len;
    feature_nr = strdup(version);
    len = (long) strlen(feature_nr);
    j = 0;
    for (i=0; i<len; i++)
    {
        if (feature_nr[i]=='.') {j = j+1;};
        if (j==2 || feature_nr[i]=='\0')
        {
            feature_nr[i]= '\0';
            break;
        }
    }
    return feature_nr;
}
