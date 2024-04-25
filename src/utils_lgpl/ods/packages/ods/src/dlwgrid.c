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
/*
 *  See c2for.txt for details
 */

#include "dlwgrid.h"


TVoid ODS_DELWAQ_UNF_lgrid(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4    * indloc,
    TInt4    * indx,
    TInt4    * nocell,
    TInt4    * igisty,
    TInt4    * ierror
    )
{

    ODS_DELWAQ_UNF_LGA(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
      indloc,
      indx,
      nocell,
      igisty,
      ierror
#if ! defined(IN_BETWEEN)
     ,lfname
#endif
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
}


TVoid ODS_DELWAQ_UNF_telmac(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4      ipcode,
    TReal8   * time,
    TInt4    * indloc,
    TReal4     valmis,
    TInt4      maxdim,
    TReal4   * data,
    TInt4    * ierror
    )
{

    ODS_DELWAQ_UNF_CCO(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
     &ipcode,
      time,
      indloc,
     &valmis,
     &maxdim,
      data,
      ierror
#if ! defined(IN_BETWEEN)
     ,lfname
#endif
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
}


