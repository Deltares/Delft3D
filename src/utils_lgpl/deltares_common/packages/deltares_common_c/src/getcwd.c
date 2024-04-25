// ---- - LGPL--------------------------------------------------------------------
//
// Copyright(C)  Stichting Deltares, 2011-2024 - 2021-2024.
//
// This library is free software; you can redistribute itand /or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the GNU
// Lesser General Public License for more details.
// 
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see < http://www.gnu.org/licenses/>. 
//
// contact : delft3d.support@deltares.nl
// Stichting Deltares
// P.O.Box 177
// 2600 MH Delft, The Netherlands
//
// All indicationsand logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares.All rights reserved.
//
// ------------------------------------------------------------------------------ -
// $Id$
// $HeadURL : https ://svn.oss.deltares.nl/repos/delft3d/trunk/src/utils_lgpl/deltares_common/packages/deltares_common/src/combinepaths.f90 $
// --description---------------------------------------------------------------- -
// This routine obtains the current
// working directorty(cwd).Code based on : https://stackoverflow.com/questions/30279228/is-there-an-alternative-to-getcwd-in-fortran-2003-2008.
//------------------------------------------------------------------------------ -


#ifdef _WIN32
#include <direct.h>
#define GETCWD _getcwd
#else
#include <unistd.h>
#define GETCWD getcwd
#endif

/* Return 0 on success, 1 on error. */
int getCWDHelper(char *str, int len)
{
    return GETCWD(str, len) == NULL;
}