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

#include "phidi2c.h"


TVoid ODSGetDimPhiMap(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    dimtyp,
    TInt4      pardep,
    TInt4      timdep,
    TInt4      locdep,
    TInt4    * ndim,
    TInt4    * ierror,
    TString    option
    )
{
    TInt4     l_dimtyp = strlen (dimtyp);
    TInt4     l_option = strlen (option);


    PHI_DIM(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
      dimtyp,
#if defined(IN_BETWEEN)
      l_dimtyp,
#endif
     &pardep,
     &timdep,
     &locdep,
      ndim,
      ierror,
      option,
#if ! defined(IN_BETWEEN)
      lfname,
      l_dimtyp,
#endif
      l_option
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
}


TVoid ODSGetDimPhiSpec(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    dimtyp,
    TInt4      pardep,
    TInt4      timdep,
    TInt4      locdep,
    TInt4    * ndim,
    TInt4    * ierror,
    TString    option
    )
{
    TInt4     l_dimtyp = strlen (dimtyp);
    TInt4     l_option = strlen (option);


    PHSPDIM(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
      dimtyp,
#if defined(IN_BETWEEN)
      l_dimtyp,
#endif
     &pardep,
     &timdep,
     &locdep,
      ndim,
      ierror,
      option,
#if ! defined(IN_BETWEEN)
      lfname,
      l_dimtyp,
#endif
      l_option
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
}


TVoid ODSGetParPhiMap(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    pardef,
    TInt4      lpardef,
    TInt4      npardef,
    TInt4      maxdef,
    TInt4      timdep,
    TInt4      locdep,
    TInt4      maxlst,
    TInt4      lang,
    TString    parlst,
    TInt4      lparlst,
    TInt4      nparlst,
    TString    paruni,
    TInt4      lparuni,
    TInt4      nparuni,
    TInt4    * partyp,
    TInt4    * parcod,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    )
{
    TInt4     l_option = strlen (option);


    PHI_PAR(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
      pardef,
#if defined(IN_BETWEEN)
      lpardef,
#endif
     &maxdef,
     &timdep,
     &locdep,
     &maxlst,
     &lang,
      parlst,
#if defined(IN_BETWEEN)
      lparlst,
#endif
      paruni,
#if defined(IN_BETWEEN)
      lparuni,
#endif
      partyp,
      parcod,
      nrlst,
      ierror,
      option,
#if ! defined(IN_BETWEEN)
      lfname,
      lpardef,
      lparlst,
      lparuni,
#endif
      l_option
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
/* Input - do not clean up
    {
        TInt4 i,j;
        for (j=0, i=lpardef-1 ; j < npardef; j++, i+= lpardef) {
            pardef[i] = '\0';
        }
    }
*/
    {
        TInt4 i,j;
        for (j=0, i=lparlst-1 ; j < nparlst; j++, i+= lparlst) {
            parlst[i] = '\0';
        }
    }
    {
        TInt4 i,j;
        for (j=0, i=lparuni-1 ; j < nparuni; j++, i+= lparuni) {
            paruni[i] = '\0';
        }
    }
}


TVoid ODSGetParPhiSpec(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    pardef,
    TInt4      lpardef,
    TInt4      npardef,
    TInt4      maxdef,
    TInt4      timdep,
    TInt4      locdep,
    TInt4      maxlst,
    TInt4      lang,
    TString    parlst,
    TInt4      lparlst,
    TInt4      nparlst,
    TString    paruni,
    TInt4      lparuni,
    TInt4      nparuni,
    TInt4    * partyp,
    TInt4    * parcod,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    )
{
    TInt4     l_option = strlen (option);


    PHSPPAR(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
      pardef,
#if defined(IN_BETWEEN)
      lpardef,
#endif
     &maxdef,
     &timdep,
     &locdep,
     &maxlst,
     &lang,
      parlst,
#if defined(IN_BETWEEN)
      lparlst,
#endif
      paruni,
#if defined(IN_BETWEEN)
      lparuni,
#endif
      partyp,
      parcod,
      nrlst,
      ierror,
      option,
#if ! defined(IN_BETWEEN)
      lfname,
      lpardef,
      lparlst,
      lparuni,
#endif
      l_option
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
/* Input - do not clean up
    {
        TInt4 i,j;
        for (j=0, i=lpardef-1 ; j < npardef; j++, i+= lpardef) {
            pardef[i] = '\0';
        }
    }
*/
    {
        TInt4 i,j;
        for (j=0, i=lparlst-1 ; j < nparlst; j++, i+= lparlst) {
            parlst[i] = '\0';
        }
    }
    {
        TInt4 i,j;
        for (j=0, i=lparuni-1 ; j < nparuni; j++, i+= lparuni) {
            paruni[i] = '\0';
        }
    }
}


TVoid ODSGetLocPhiSpec(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    locdef,
    TInt4      llocdef,
    TInt4      nlocdef,
    TInt4      maxdef,
    TInt4      pardep,
    TInt4      timdep,
    TString    loclst,
    TInt4      lloclst,
    TInt4      nloclst,
    TInt4    * loctyp,
    TInt4    * locnr,
    TInt4      maxlst,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    )
{
    TInt4     l_option = strlen (option);


    PHSPLOC(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
      locdef,
#if defined(IN_BETWEEN)
      llocdef,
#endif
     &maxdef,
     &pardep,
     &timdep,
      loclst,
#if defined(IN_BETWEEN)
      lloclst,
#endif
      loctyp,
      locnr,
     &maxlst,
      nrlst,
      ierror,
      option,
#if ! defined(IN_BETWEEN)
      lfname,
      llocdef,
      lloclst,
#endif
      l_option
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
/* Input - do not clean up
    {
        TInt4 i,j;
        for (j=0, i=llocdef-1 ; j < nlocdef; j++, i+= llocdef) {
            locdef[i] = '\0';
        }
    }
*/
    {
        TInt4 i,j;
        for (j=0, i=lloclst-1 ; j < nloclst; j++, i+= lloclst) {
            loclst[i] = '\0';
        }
    }
}


TVoid ODSGetTmePhiMap(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TReal8   * timdef,
    TInt4      maxdef,
    TInt4      pardep,
    TInt4      locdep,
    TInt4      maxlst,
    TReal8   * timlst,
    TInt4    * timtyp,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    )
{
    TInt4     l_option = strlen (option);


    PHI_TME(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
      timdef,
     &maxdef,
     &pardep,
     &locdep,
     &maxlst,
      timlst,
      timtyp,
      nrlst,
      ierror,
      option,
#if ! defined(IN_BETWEEN)
      lfname,
#endif
      l_option
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
}


TVoid ODSGetTmePhiSpec(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TReal8   * timdef,
    TInt4      maxdef,
    TInt4      pardep,
    TInt4      locdep,
    TInt4      maxlst,
    TReal8   * timlst,
    TInt4    * timtyp,
    TInt4    * nrlst,
    TInt4    * ierror,
    TString    option
    )
{
    TInt4     l_option = strlen (option);


    PHSPTME(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
      timdef,
     &maxdef,
     &pardep,
     &locdep,
     &maxlst,
      timlst,
      timtyp,
      nrlst,
      ierror,
      option,
#if ! defined(IN_BETWEEN)
      lfname,
#endif
      l_option
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
}


TVoid ODSGetMatPhiMap(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4      parcod,
    TInt4    * loc,
    TReal8   * tim,
    TReal4     misval,
    TInt4      i3gl,
    TInt4      maxdim,
    TReal4   * xdata,
    TInt4    * ierror,
    TString    option,
    TInt4    * ibuffs,
    TReal4   * rbuffs
    )
{
    TInt4     l_option = strlen (option);

    PHI_MAT(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
     &parcod,
      loc,
      tim,
     &misval,
     &i3gl,
     &maxdim,
      xdata,
      ierror,
      option,
#if defined(IN_BETWEEN)
      l_option,
#endif
      ibuffs,
      rbuffs
#if ! defined(IN_BETWEEN)
     ,lfname
     ,l_option
#endif
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
}


TVoid ODSGetMatPhiSpec(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TInt4      parcod,
    TInt4    * loc,
    TReal8   * tim,
    TReal4     misval,
    TInt4      i3gl,
    TInt4      maxdim,
    TReal4   * xdata,
    TInt4    * ierror,
    TString    option,
    TInt4    * ibuffs,
    TReal4   * rbuffs
    )
{
    TInt4     l_option = strlen (option);

    PHSPMAT(
      fname,
#if defined(IN_BETWEEN)
     lfname,
#endif
     &itype,
     &parcod,
      loc,
      tim,
     &misval,
     &i3gl,
     &maxdim,
      xdata,
      ierror,
      option,
#if defined(IN_BETWEEN)
      l_option,
#endif
      ibuffs,
      rbuffs
#if ! defined(IN_BETWEEN)
     ,lfname
     ,l_option
#endif
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
}


TVoid ODSGetLocPhiHis(
    TString    fname,
    TInt4      lfname,
    TInt4      nfname,
    TInt4      itype,
    TString    locdef,
    TInt4      llocdef,
    TInt4      nlocdef,
    TInt4      maxdef,
    TInt4      pardep,
    TInt4      timdep,
    TInt4      maxlst,
    TString    loclst,
    TInt4      lloclst,
    TInt4      nloclst,
    TInt4    * loctyp,
    TInt4    * nrlst,
    TInt4    * locnr,
    TInt4    * ierror,
    TString    zbuffs,
    TInt4      lzbuffs,
    TInt4      nzbuffs,
    TString    option
    )
{
    TInt4     l_option = strlen (option);


    PHI_LOC(
      fname,
#if defined(IN_BETWEEN)
      lfname,
#endif
     &itype,
      locdef,
#if defined(IN_BETWEEN)
      llocdef,
#endif
     &maxdef,
     &pardep,
     &timdep,
     &maxlst,
      loclst,
#if defined(IN_BETWEEN)
      lloclst,
#endif
      loctyp,
      nrlst,
      locnr,
      ierror,
      zbuffs,
#if defined(IN_BETWEEN)
      lzbuffs,
#endif
      option,
#if ! defined(IN_BETWEEN)
      lfname,
      llocdef,
      lloclst,
      lzbuffs,
#endif
      l_option
      );
    {
        TInt4 i,j;
        for (j=0, i=lfname-1 ; j < nfname; j++, i+= lfname) {
            fname[i] = '\0';
        }
    }
/* Input - do not clean up
    {
        TInt4 i,j;
        for (j=0, i=llocdef-1 ; j < nlocdef; j++, i+= llocdef) {
            locdef[i] = '\0';
        }
    }
*/
    {
        TInt4 i,j;
        for (j=0, i=lloclst-1 ; j < nloclst; j++, i+= lloclst) {
            loclst[i] = '\0';
        }
    }
    {
        TInt4 i,j;
        for (j=0, i=lzbuffs-1 ; j < nzbuffs; j++, i+= lzbuffs) {
            zbuffs[i] = '\0';
        }
    }
}


