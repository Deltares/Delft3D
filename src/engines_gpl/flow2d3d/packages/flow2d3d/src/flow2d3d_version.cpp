//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2016.
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
// $Id: flow2d3d_version.cpp 904 2011-10-20 14:08:52Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20110420_OnlineVisualisation/src/engines_gpl/flow2d3d/packages/flow2d3d/src/flow2d3d_version.cpp $
#include <stdio.h>
#include <string.h>

#if defined(WIN32) || defined (WIN64)
#  include <io.h>
#  include <wtypes.h>
#elif defined (salford32)
#  include <io.h>
#  include <windows.h>
#endif

#if HAVE_CONFIG_H
#   include "config.h"
#endif
#include "flow2d3d_version.h"

#ifndef min
#  define min(a,b) (a)<(b) ? (a) : (b)
#  define max(a,b) (a)>(b) ? (a) : (b)
#endif

static char modname_version_id [] = {"@(#)Deltares, "modname_program" Version "modname_major"."modname_minor"."modname_revision"."modname_build", "__DATE__", "__TIME__""};

void STDCALL GETSHORTVERSIONSTRING( char * str, int length_str )
{
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  i  = min((int) length_str, (int) strlen(modname_version_short));
  strncpy(str, modname_version_short, i);
}

void STDCALL GETFULLVERSIONSTRING( char * str, int length_str )
{
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  i  = min((int) length_str, (int) strlen(modname_version_full));
  strncpy(str, modname_version_full, i);
}

void STDCALL GETCOMFILEVERSIONSTRING( char * str, int length_str )
{
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  i  = min((int) length_str, (int) strlen(com_file_version));
  strncpy(str, com_file_version, i);
}

void STDCALL GETDROFILEVERSIONSTRING( char * str, int length_str )
{
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  i  = min((int) length_str, (int) strlen(dro_file_version));
  strncpy(str, dro_file_version, i);
}

void STDCALL GETHISFILEVERSIONSTRING( char * str, int length_str )
{
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  i  = min((int) length_str, (int) strlen(his_file_version));
  strncpy(str, his_file_version, i);
}

void STDCALL GETMAPFILEVERSIONSTRING( char * str, int length_str )
{
  int i;
  for (i=0; i<length_str; i++) {str[i] = ' ';}
  i  = min((int) length_str, (int) strlen(map_file_version));
  strncpy(str, map_file_version, i);
}
