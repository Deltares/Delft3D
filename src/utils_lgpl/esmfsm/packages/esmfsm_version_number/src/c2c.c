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
// $Id$
// $HeadURL$
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if defined(WIN32) || defined (WIN64)
#  include <io.h>
#  include <wtypes.h>
#endif

#include "version_number.h"
#define CAT(a, b) a ## b
#define FUNC_CAT(a, b) CAT(a, b)

extern  char * FUNC_CAT( version_getFileVersionString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getFullVersionString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getCompanyString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getVersionNumberString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getProgramNameString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getShortProgramNameString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getSvnRevisionString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getFeatureNumberString_, MOD_NAME)(void);

/*==========================================================================*/
void FUNC_CAT( getFileVersionString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getFileVersionString_, MOD_NAME)();
  strcpy(str, str1);
}

/*==========================================================================*/
void FUNC_CAT( getFullVersionString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getFullVersionString_, MOD_NAME)();
  strcpy(str, str1);
}

/*==========================================================================*/
void FUNC_CAT( getCompanyString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getCompanyString_, MOD_NAME)();
  strcpy(str, str1);
}

/*==========================================================================*/
void FUNC_CAT( getVersionNumberString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getVersionNumberString_, MOD_NAME)();
  strcpy(str, str1);
}
/*==========================================================================*/
void FUNC_CAT( getProgramNameString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getProgramNameString_, MOD_NAME)();
  strcpy(str, str1);
}
/*==========================================================================*/
void FUNC_CAT( getShortProgramNameString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getShortProgramNameString_, MOD_NAME)();
  strcpy(str, str1);
}

/*==========================================================================*/
void FUNC_CAT( getSvnRevisionString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getSvnRevisionString_, MOD_NAME)();
  strcpy(str, str1);
}
/*==========================================================================*/
void FUNC_CAT( getFeatureNumberString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getFeatureNumberString_, MOD_NAME)();
  strcpy(str, str1);
}
