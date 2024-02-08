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
 *  pharmap.c -  ODS functions for PHAROS nefis files (map functions)
 *
 *   Peter van den Bosch
 */

/*
 *  $Author: Markus $
 *  $Date: 28-06-04 9:19 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/pharmap.c,v $
*/
/*
 *
 *
 */



/*   Date:       8 Febr 1995                                          */
/*   Time:       10:55                                                */
/*   Date:       9 Dec 1994                                           */
/*   Program:    PHARMAP.C                                            */
/*   Version:    1.00                                                 */
/*   Programmer: Peter van den Bosch                                  */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   Project:     PHAROS koppeling GPP                                */
/*   Module:      GetParameter/GetDimensions                          */
/*   Subroutines: ODSGetDimPharMap Get dimensions of PHAROS maps      */
/*                ODSGetDimPharAmp Get dimensions of PHAROS seiches-  */
/*                                 locations parameters               */
/*                ODSGetParPharMap Get parameters of PHAROS map file  */
/*                ODSGetParPharAmp Get parameters of PHAROS seiches-  */
/*                                 locations parameters               */
/*                ODSGetLocPharAmp Get locations of PHAROS seiches-   */
/*                                 locations                          */
/*                ODSGetTmePharAmp Get times of PHAROS seiches-       */
/*                                 locations                          */
/*                ODSGetMatPharMap Get functions of PHAROS map file   */
/*                ODSGetGrdPharMap Get topography of PHAROS map file  */
/*                ODSGetGrdPharAmp Get dummy topography for FREQUENCY */
/*                                 axis                               */
/*   Function:                                                        */
/*   Comment:     General version                                     */
/*   Reference:                                                       */
/*   Review:                                                          */
/*                                                                    */
/**********************************************************************/


#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "portable.h"
#include "ods.h"
#include "utils.h"
#include "itrans.h"
#include "nefis.h"
#include "opnclose.h"
#include "equal.h"

#ifndef FALSE
#    define FALSE   0
#    define TRUE    1
#endif

#define PI 3.1415926


#ifdef USE_VS2005
// define some memory management functions, the whole dm is not required here
void DM_Free( void* memory )
{
       free(memory);
}
void* DM_Malloc( TInt4 size )  /* I Size of memory block */
                                  /* Returns pointer */
{
    if (size < 16)
    {
   size = 16;
    }
    return malloc( (size_t) size );
}
#endif

TVoid ODSGetBasicPar( TInt4 * infadm, TString parlst, TString paruni,
                      TInt4   maxlst, TInt4 * nrlst, TInt4 * partyp,
                      TInt4 * parcod, TInt4 * ierror ) ;
TVoid ODSGetHierarchPar( TInt4 *  infadm, TReal4 * parinf,
                         TString  parlst, TString  paruni, TInt4    maxlst,
                         TInt4    parid, TInt4 *  nrlst, TInt4 *  partyp,
                         TInt4 *  parcod, TInt4 *  ierror ) ;

/*************************************************************************/
/*    SUBROUTINE Get Dimension from PHAROS map file                     */
/*************************************************************************/
/* @@------------------------------------------
    Function: ODSGetDimPharAmp()
    Author:   Peter van den Bosch
    Purpose:  Get dimensions from PHAROS map (nefis admin) file
    Context:  Get dimensions of parameters, locations or times
    Pseudo Code:
              - read information vector from the file
              - find dimensions depending on the current information
  ------------------------------------------------*/

TVoid ODSGetDimPharAmp( TString fname,
                        TInt4   *ftype,
                        TString dim,
                        TInt4   pardep,
                        TInt4   timdep,
                        TInt4   locdep,
                        TInt4   *ndim,
                        TInt4   *ierror,
                        TString option)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       dim     -          I   String indicating dimension type         */
/*       fname   3          I   Full filename, including extension.      */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       locdep  -          I   Location dependency (ignored)            */
/*       option  -          I   Option (ignored)                         */
/*       pardep  -          I   Parameter dependency (ignored)           */
/*       timdep  -          I   Time dependency (ignored)                */
/*                                                                       */
/*************************************************************************/
{
   TInt4   datfds[ 999],
           deffds[2997] ;
   TInt4 * infadm     ;
   TInt4   uindex[1][3],
           usrord ;
   TInt4   ind21,
           ind,
           buflen ;
   TChar   grpnam[20],
           elmnam[20],
           celnam[16] ;
   TInt4   grp_ndim,
           grp_dims,
           grp_ord,
           ldum ;

   *ierror = IEOK ;

   /* Read nr of locations from the infadm vector */

   OPNNEF( fname, ftype, datfds, deffds, ierror) ;
   if ( *ierror != IEOK )
   {
      return ;
   }

   /* get informationvector INFADM from file */
   /* start with index 0; so the indices should be corrected with one */
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   usrord = 1;
   buflen = 200 * sizeof(TInt4) ;
   infadm = (TInt4 *) DM_Malloc( buflen ) ;
   strcpy( grpnam, "INFO" ) ;
   strcpy( elmnam, "INFADM" ) ;
   *ierror = Getelt( deffds, grpnam, elmnam, uindex,
                     &usrord, &buflen, infadm ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      return ;
   }

   if ( infadm[14-1] == 0 || infadm[19-1] == 0 )
   {
      *ierror = IETYPE ;
      CLOSFL( fname, &ldum  ) ;
      return;
   }

   if ( equal( dim , "loc" ) )
   {
      ndim[0] = 4;
      ndim[1] = infadm[33-1];
      ndim[2] = 1;
      ndim[3] = infadm[19-1];
      ndim[4] = ndim[3];
   }


/*  Which dimension?
    The maps don't depend on time, so ...
*/
   if ( equal( dim , "tim" ) )
   {
      ndim[0] = 1;
      ndim[1] = 1;
   }

/*  The number of parameters depends on the run parameters specified in
    the information vector - all parameters required
*/
   if ( equal( dim , "par" ) && pardep == 0 )
   {
      grp_ndim = 5 ;
      *ierror = Inqgrp( deffds, "SEICH_loc", celnam, &grp_ndim,
                        &grp_dims, &grp_ord );

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         return ;
      }

      ndim[0] = 1 ;
      ndim[1] = 6 ;
   }
   CLOSFL( fname, ierror ) ;
   DM_Free( infadm );
}

/* @@------------------------------------------
    Function: ODSGetDimPharMap()
    Author:   Peter van den Bosch
    Purpose:  Get dimensions from PHAROS map (nefis admin) file
    Context:  Get dimensions of parameters, locations or times
    Pseudo Code:
            - read information vector from the file
            - find dimensions depending on the current information
  ------------------------------------------------*/

TVoid ODSGetDimPharMap( TString fname,
                        TInt4   *ftype,
                        TString dim,
                        TInt4   pardep,
                        TInt4   timdep,
                        TInt4   locdep,
                        TInt4   *ndim,
                        TInt4   *ierror,
                        TString option)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       dim     -          I   String indicating dimension type         */
/*       fname   3          I   Full filename, including extension.      */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       locdep  -          I   Location dependency (ignored)            */
/*       option  -          I   Option (ignored)                         */
/*       pardep  -          I   Parameter dependency (ignored)           */
/*       timdep  -          I   Time dependency (ignored)                */
/*                                                                       */
/*************************************************************************/
{
   TInt4   datfds[ 999],
           deffds[2997] ;
   TInt4 * infadm     ;
   TInt4   uindex[1][3],
           usrord ;
   TInt4   ind21,
           ind,
           buflen,
           ldum ;
   TChar   grpnam[20],
           elmnam[20] ;

   *ierror = IEOK ;

   OPNNEF( fname, ftype, datfds, deffds, ierror) ;
   if ( *ierror != IEOK )
   {
      return ;
   }

   /* get informationvector INFADM from file */
   /* start with index 0; so the indices should be corrected with one */
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   usrord = 1;
   buflen = 200 * sizeof(TInt4) ;
   infadm = (TInt4 *) DM_Malloc( buflen ) ;
   strcpy( grpnam, "INFO" ) ;
   strcpy( elmnam, "INFADM" ) ;
   *ierror = Getelt( deffds, grpnam, elmnam, uindex,
             &usrord, &buflen, infadm ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      return ;
   }


/*  Which dimension?
    The maps don't depend on time, so ...
*/
   if ( equal( dim , "tim" ) )
   {
      ndim[0] = 1 ;
      ndim[1] = 0 ;
      return ;
   }

/*  The number of parameters depends on the run parameters specified in
    the information vector - all parameters required
*/
   if ( equal( dim , "par" ) && pardep == 0 )
   {
      ndim[0] = 1 ;
      ndim[1] = 3 ;
      if ( infadm[18-1] == 1 )
      {
         /* computation with current */
         ndim[1] = ndim[1]+2 ;
      }

      if ( infadm[11-1] == 1 )
      {
         /* computation with breaking */
         ndim[1] = ndim[1]+1 ;
      }

      if ( infadm[13-1] == 0 && infadm[25-1] == 1 )
      {
         /* no seiches - computational results available */
         if ( infadm[17-1] > 0 )
         {
            /* directional spreaded results */
            ndim[1] = ndim[1]+1 ;
         }
         else
         {
            /* uni-directional results */
            ndim[1] = ndim[1]+3 ;
         }
      }

      if ( infadm[13-1] == 1 && infadm[25-1] == 1)
      {
         /* seiches - computational results available */
         ndim[1] = ndim[1]+6 ;
      }
   }

/* The number of parameters depends on the number of sub-parameters
   of an hierarchical parameter
 */
   if ( equal( dim , "par" ) && pardep != 0 )
   {
      ndim[0] = 1 ;
      if ( pardep == 7 )
      {
         /* directional spreaded results - ndir main wave directions */
         ndim[1] = infadm[17-1] ;
      }
      if ( pardep == 8 || pardep == 9 || pardep == 10 )
      {
         /* uni-directional results - nhr wave directions*/
         ndim[1] = infadm[16-1] ;
      }
      if ( pardep > 10 && pardep <= 16 )
      {
         /* seches - ntper number of computed frequencies */
         ndim[1] = infadm[19-1] ;
      }
   }


/* The size of the grid - note: four dimensions (!) are returned
   - the fourth being the size of the administration array
   Note:
   - number of coordinates: number of nodes (NKN)
   - size of the administration array: 3*number of elements (NELTS)
*/
   if ( equal( dim , "loc" ) )
   {
      if ( pardep > 16 && pardep <= 21 )
      {
         /* location dependend parameters - size is # of locations */
         ndim[0] = 1;
         ndim[1] = infadm[33-1];
      }
      else
      {
         /* map parameters */
         ndim[0] = 4 ;
         ndim[1] = infadm[1-1] ;
         ndim[2] = 1 ;
         ndim[3] = 1 ;
         ndim[4] = 3 * (infadm[2-1]-infadm[4-1]-infadm[5-1]-infadm[6-1]) ;
      }
   }

/* Free buffers */
   DM_Free( infadm) ;

/* Close file */
   CLOSFL( fname, ierror ) ;

   return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Parameter list from SHYFEM grid file                */
/*************************************************************************/
/* @@------------------------------------------
    Function: ODSGetParPharAmp()
    Author:   Peter van den Bosch
    Purpose:  Get parameters from PHAROS admin file (amplifications)
    Context:  Get list of available parameters
    Pseudo Code:
              - read information vector from the file
              - fill parameter list depending on the current information
  ------------------------------------------------*/

TVoid ODSGetParPharAmp ( TString fname,
                         TInt4   *ftype,
                         TString pardef,
                         TInt4   maxdef,
                         TString parlst,
                         TString paruni,
                         TInt4   maxlst,
                         TInt4   *nrlst,
                         TInt4   *partyp,
                         TInt4   *parcod,
                         TInt4   *ierror)
/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for                 */
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*       parcod  maxdef     I   List of codes (indices) of parameters    */
/*                              found.                                   */
/*       pardef  maxdef     I   List of parameters wanted.               */
/*       parlst  maxlst     O   List of parameters found.                */
/*       partyp  maxlst     O   List of types of parameters found.       */
/*       paruni  maxlst     O   List of units of parameters found.       */
/*                                                                       */
/*************************************************************************/
{
   TInt4    datfds[ 999],
            deffds[2997] ;
   TInt4 *  infadm ,
         *  irl ;
   TInt4    uindex[1][3],
            usrord[1] ;
   TInt4    ind21,
            ind,
            buflen ,
            i,
            nfreq,
            nhr,
            ncom,
            lrec,
            parid,
            ldum ;
   TReal4   raddeg = 57.29578 ;  /* faktor for radians to degrees */
   TReal4 * parinf ;
   TReal8 * qh,
          * freq ;

   *ierror = IEOK ;

   OPNNEF( fname, ftype, datfds, deffds, ierror) ;
   if ( *ierror != IEOK )
   {
      return ;
   }

   /* get informationvector INFADM from file */
   /* start with index 0; so the indices should be corrected with one */
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   usrord[0] = 1;
   buflen = 200 * sizeof(TInt4) ;
   infadm = (TInt4 *) DM_Malloc( buflen) ;
   *ierror = Getelt( deffds, "INFO", "INFADM", uindex,
                     usrord, &buflen, infadm ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      return ;
   }

   *nrlst = 0;
   ind    = *nrlst;
   ind21  = *nrlst * 21;


/*  Put the names in the arrays
 *
 *  Next codes are used for parcod
 *    Amplification at loc    17
 *    Phase at location       18
 *    Umax  at location       19
 *    Umin  at location       20
 *    Udir  at location       21
 */

   if ( strcmp( pardef, "*" ) == 0 )
   {
      if ( infadm[13-1] == 1 && infadm[25-1] == 1)
      {
         /* seiches - computational results available */
         if ( infadm[33-1] > 0 )
         {
            /* seiches computation and output locations defined */
            *nrlst = *nrlst + 6;
            if ( maxlst < *nrlst )
            {
                *ierror = IEPMNY ;
                CLOSFL( fname, &ldum  ) ;
                return ;
            }
            strcpy( parlst + ind21      , "Amplification at loc" ) ;
            strcpy( parlst + ind21 + 21 , "Phase at location"    ) ;
            strcpy( parlst + ind21 + 42 , "Umax  at location"    ) ;
            strcpy( parlst + ind21 + 63 , "Umin  at location"    ) ;
            strcpy( parlst + ind21 + 84 , "Udir  at location"    ) ;
            strcpy( parlst + ind21 +105 , "FREQUENCY"            ) ;
            strcpy( paruni + ind21      , "m" ) ;
            strcpy( paruni + ind21 + 21 , ""  ) ;
            strcpy( paruni + ind21 + 42 , "m/s" ) ;
            strcpy( paruni + ind21 + 63 , "m/s" ) ;
            strcpy( paruni + ind21 + 84 , "rad" ) ;
            strcpy( paruni + ind21 +105 , "Hz"  ) ;

            partyp[ind]   = ODS_PT_LOC_LIST + ODS_PT_LOC_DEP ;
            partyp[ind+1] = ODS_PT_LOC_LIST + ODS_PT_LOC_DEP;
            partyp[ind+2] = ODS_PT_LOC_LIST + ODS_PT_LOC_DEP;
            partyp[ind+3] = ODS_PT_LOC_LIST + ODS_PT_LOC_DEP ;
            partyp[ind+4] = ODS_PT_LOC_LIST + ODS_PT_LOC_DEP ;
            partyp[ind+5] = ODS_PT_LOC_MNK ;
            parcod[ind]   = 17             ;
            parcod[ind+1] = 18             ;
            parcod[ind+2] = 19             ;
            parcod[ind+3] = 20             ;
            parcod[ind+4] = 21             ;
            parcod[ind+5] = 22             ;

            ind   = *nrlst ;
            ind21 = *nrlst * 21;
         }
      }
   }
   CLOSFL( fname, ierror ) ;
}

/* @@------------------------------------------
    Function: ODSGetParPharMap()
    Author:   Peter van den Bosch
    Purpose:  Get parameters from PHAROS map (nefis admin) file
    Context:  Get list of available parameters
    Pseudo Code:
            - read information vector from the file
            - fill parameter list depending on the current information
  ------------------------------------------------*/


TVoid ODSGetParPharMap ( TString fname,
                         TInt4   *ftype,
                         TString pardef,
                         TInt4   maxdef,
                         TString parlst,
                         TString paruni,
                         TInt4   maxlst,
                         TInt4   *nrlst,
                         TInt4   *partyp,
                         TInt4   *parcod,
                         TInt4   *ierror)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*       parcod  maxdef     I   List of codes (indices) of parameters    */
/*                              found.                                   */
/*       pardef  maxdef     I   List of parameters wanted.               */
/*       parlst  maxlst     O   List of parameters found.                */
/*       partyp  maxlst     O   List of types of parameters found.       */
/*       paruni  maxlst     O   List of units of parameters found.       */
/*                                                                       */
/*************************************************************************/
{
   TInt4    datfds[ 999],
            deffds[2997] ;
   TInt4 *  infadm ,
         *  irl ;
   TInt4    uindex[1][3],
            usrord[1] ;
   TInt4    ind21,
            ind,
            buflen ,
            i,
            nfreq,
            nhr,
            ncom,
            lrec,
            parid,
            ldum ;
   TReal4   raddeg = 57.29578 ;  /* faktor for radians to degrees */
   TReal4 * parinf ;
   TReal8 * qh,
          * freq ;

   *ierror = IEOK ;

   OPNNEF( fname, ftype, datfds, deffds, ierror) ;
   if ( *ierror != IEOK )
   {
       return ;
   }

   /* get informationvector INFADM from file */
   /* start with index 0; so the indices should be corrected with one */
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   usrord[0] = 1;
   buflen = 200 * sizeof(TInt4) ;
   infadm = (TInt4 *) DM_Malloc( buflen) ;
   *ierror = Getelt( deffds, "INFO", "INFADM", uindex,
             usrord, &buflen, infadm ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      return ;
   }


/*  Put the names in the arrays
 *
 *  Next codes are used for parcod
 *    X_coor                   1
 *    Y_coor                   2
 *    Waterdepth               3
 *    U_current                4
 *    V_current                5
 *    Break coeff Gamma        6
 *    Wave height dir. spr     7
 *    Wave height              8
 *    Wave phase               9
 *    Wave image              10
 *    Wave height (seiches)   11
 *    Wave phase  (seiches)   12
 *    Wave image  (seiches)   13
 *    Umax        (seiches)   14
 *    Umin        (seiches)   15
 *    Udir        (seiches)   16
 */

   if ( strcmp( pardef, "*" ) == 0 )
   {
      /* Basic parameters required */
      ODSGetBasicPar( infadm, parlst, paruni, maxlst, nrlst, partyp,
                      parcod, ierror ) ;
      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         return ;
      }
   }
   else
   {
      /* Sub-parameters of an heirarchical parameter required */

      parid = 0;
      if ( strcmp( pardef, "Wave height dir. spr" ) == 0 )   parid = 7000 ;
      if ( strcmp( pardef, "Wave height" ) == 0 && infadm[13-1] == 0 )
         parid =  8000 ;
      if ( strcmp( pardef, "Wave phase"  ) == 0 && infadm[13-1] == 0 )
         parid =  9000 ;
      if ( strcmp( pardef, "Wave image"  ) == 0 && infadm[13-1] == 0 )
         parid = 10000 ;
      if ( strcmp( pardef, "Wave height" ) == 0 && infadm[13-1] == 1 )
         parid = 11000 ;
      if ( strcmp( pardef, "Wave phase" ) == 0 && infadm[13-1] == 1 )
         parid = 12000 ;
      if ( strcmp( pardef, "Wave image" ) == 0 && infadm[13-1] == 1 )
         parid = 13000 ;
      if ( strcmp( pardef, "Umax" ) == 0 ) parid = 14000 ;
      if ( strcmp( pardef, "Umin" ) == 0 ) parid = 15000 ;
      if ( strcmp( pardef, "Udir" ) == 0 ) parid = 16000 ;

      if ( parid == 0 )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm ) ;
         return ;
      }

      if ( infadm[13-1] == 0 && infadm[25-1] == 1 )
      {
         /* no seiches - computational results available */
         if ( infadm[17-1] > 0 )
         {
            if ( parid != 7000 )
            {
               *ierror = IEOTHR ;
               DM_Free( infadm ) ;
               return ;
            }
            /* directional spreaded results - get main wave directions */
            ncom = infadm[17-1] ;
            uindex[0][0] = 1;
            uindex[0][1] = ncom;
            uindex[0][2] = 1;
            usrord[0] = 1;
            buflen = 2 * ncom * sizeof(TReal8) ;
            qh = (TReal8 *) DM_Malloc( buflen) ;
            *ierror = Getelt( deffds, "HS_dir", "QH_inc",
                              uindex, usrord, &buflen, qh ) ;

            if ( *ierror != IEOK )
            {
               *ierror = IEINFO ;
               CLOSFL( fname, &ldum  ) ;
               DM_Free( infadm ) ;
               DM_Free( qh ) ;
               return ;
            }

            /* reserve space for vector with wave directions */
            buflen = nhr * sizeof( TReal4 );
            parinf = (TReal4 *) DM_Malloc( buflen ) ;

            for ( i = 0; i < ncom; i++ )
            {
               parinf[i] = (TReal4) qh[2*i]*raddeg ;
            }

            DM_Free( qh ) ;
         }
         else
         {
            /* Not sure if this is correct now! AM */
            if ( infadm[16-1] > 0 )
            {
               if ( parid != 8000 && parid != 9000 && parid != 10000 )
               {
                  *ierror = IEOTHR ;
                  CLOSFL( fname, &ldum  ) ;
                  DM_Free( infadm ) ;
                  return ;
               }

               /* uni-directional results - get main wave directions */
               nhr = infadm[16-1] ;
               uindex[0][0] = 1;
               uindex[0][1] = 1;
               uindex[0][2] = 1;
               usrord[0] = 1;
               buflen = 2 * nhr * sizeof(TReal8) ;
               qh = (TReal8 *) DM_Malloc( buflen) ;
               *ierror = Getelt( deffds, "GRID_adm", "QH_incident",
                                 uindex, usrord, &buflen, qh ) ;

               if ( *ierror != IEOK )
               {
                  *ierror = IEINFO ;
                  CLOSFL( fname, &ldum  ) ;
                  DM_Free( infadm ) ;
                  DM_Free( qh ) ;
                  return ;
               }

               /* reserve space for vector with wave directions */
               buflen = nhr * sizeof( TReal4 );
               parinf = (TReal4 *) DM_Malloc( buflen ) ;

               for ( i = 0; i < nhr; i++ )
               {
                  parinf[i] = (TReal4) qh[2*i]*raddeg ;
               }

               DM_Free( qh ) ;
            }
         }
      }
      else if (infadm[13-1] == 1 && infadm[25-1] == 1 )
      {
         if ( parid < 11000 || parid > 16000 )
         {
             *ierror = IEOTHR ;
               CLOSFL( fname, &ldum  ) ;
             DM_Free( infadm ) ;
             return ;
         }

         /* secihes computation - get computed frequencies */
         nfreq = infadm[19-1] ;
         lrec  = infadm[30-1] ;
         uindex[0][0] = 1;
         uindex[0][1] = 1;
         uindex[0][2] = 1;
         usrord[0] = 1;
         buflen = lrec * sizeof(TReal8) ;
         freq = (TReal8 *) DM_Malloc( buflen) ;
         *ierror = Getelt( deffds, "SEICH_def", "FREQ",
                   uindex, usrord, &buflen, freq ) ;
         buflen = lrec * sizeof(TInt4) ;
         irl = (TInt4 *) DM_Malloc( buflen) ;
         *ierror = Getelt( deffds, "SEICH_def", "IRL",
                   uindex, usrord, &buflen, irl ) ;

         if ( *ierror != IEOK )
         {
            *ierror = IEINFO ;
            CLOSFL( fname, &ldum  ) ;
            DM_Free( infadm ) ;
            DM_Free( freq ) ;
            DM_Free( irl ) ;
            return ;
         }

         /* reserve space for vector with computed frequencies */
         buflen = nfreq * sizeof( TReal4 );
         parinf = (TReal4 *) DM_Malloc( buflen ) ;

         ind = 0;
         for ( i = 0; i < lrec; i++ )
         {
            if ( ind > nfreq )
            {
               *ierror = IEOTHR ;
               CLOSFL( fname, &ldum  ) ;
               DM_Free( infadm ) ;
               DM_Free( freq ) ;
               DM_Free( irl ) ;
               DM_Free( parinf ) ;
               return ;
            }
            if ( irl[i] != 0 )
            {
               parinf[ind] = (TReal4) freq[i] ;
               parcod[ind] = (TReal4) irl[i] ;
               ind ++;
            }
         }
         DM_Free( freq );
         DM_Free( irl  );
      }

      ODSGetHierarchPar( infadm, parinf, parlst, paruni,
                         maxlst, parid, nrlst, partyp, parcod, ierror ) ;

      DM_Free( parinf );
   }

   /* Free buffers */
   DM_Free( infadm) ;

   /* Close file */
   CLOSFL( fname, ierror ) ;

   return ;
}

/* @@------------------------------------------
    Function: ODSGetBasicPar()
    Author:   Peter van den Bosch
    Purpose:  Get basic parameters from PHAROS (nefis admin) file
    Context:  Get list of available parameters
    Pseudo Code:
            - fill parameter list depending on the information vector
  ------------------------------------------------*/

TVoid ODSGetBasicPar(
    TInt4 * infadm,
    TString parlst,
    TString paruni,
    TInt4   maxlst,
    TInt4 * nrlst,
    TInt4 * partyp,
    TInt4 * parcod,
    TInt4 * ierror )
{
    TInt4  ind,
         ind21 ;

/*  Put the names in the arrays
 *
 *  Next codes are used for parcod
 *    X_coor                   1
 *    Y_coor                   2
 *    Waterdepth               3
 *    U_current                4
 *    V_current                5
 *    Break coeff Gamma        6
 *    Wave height dir. spr     7
 *    Wave height              8
 *    Wave phase               9
 *    Wave image              10
 *    Wave height (seiches)   11
 *    Wave phase  (seiches)   12
 *    Wave image  (seiches)   13
 *    Umax        (seiches)   14
 *    Umin        (seiches)   15
 *    Udir        (seiches)   16
 *    Amplification at loc    17
 *    Phase at location       18
 *    Umax  at location       19
 *    Umin  at location       20
 *    Udir  at location       21
 */

/*  if list too short to hold the three parameters, an error
*/
    if ( maxlst < 3 )
    {
       *ierror = IEPMNY ;
       return ;
    }

    *nrlst = 3 ;
    strcpy( parlst      , "X_coor" ) ;
    strcpy( parlst + 21 , "Y_coor" ) ;
    strcpy( parlst + 42 , "Waterdepth" ) ;
    strcpy( paruni      , "m" ) ;
    strcpy( paruni + 21 , "m" ) ;
    strcpy( paruni + 42 , "m" ) ;

    partyp[0] = ODS_PT_LOC_MNK ;
    partyp[1] = ODS_PT_LOC_MNK ;
    partyp[2] = ODS_PT_LOC_MNK ;
    parcod[0] = 1              ;
    parcod[1] = 2              ;
    parcod[2] = 3              ;

    ind   = *nrlst ;
    ind21 = *nrlst * 21 ;

    if ( infadm[18-1] == 1 )
    {
      /* computation with current - add velocity components */
        *nrlst = *nrlst + 2 ;
        if ( maxlst < *nrlst )
        {
            *ierror = IEPMNY ;
            return ;
        }
        strcpy( parlst + ind21      , "U_current" ) ;
        strcpy( parlst + ind21 + 21 , "V_current" ) ;
        strcpy( paruni + ind21      , "m/s" ) ;
        strcpy( paruni + ind21 + 21 , "m/s" ) ;

        partyp[ind]   = ODS_PT_LOC_MNK ;
        partyp[ind+1] = ODS_PT_LOC_MNK ;
        parcod[ind]   = 4              ;
        parcod[ind+1] = 5              ;

        ind   = *nrlst ;
        ind21 = *nrlst * 21 ;
    }

    if ( infadm[11-1] == 1 )
    {
      /* computation with wave breaking - add breaking coefficient */
        *nrlst = *nrlst + 1 ;
        if ( maxlst < *nrlst )
        {
            *ierror = IEPMNY ;
            return ;
        }
        strcpy( parlst + ind21      , "Break coeff Gamma" ) ;
        strcpy( paruni + ind21      , "" ) ;

        partyp[ind]   = ODS_PT_LOC_MNK ;
        parcod[ind]   = 6              ;

        ind   = *nrlst ;
        ind21 = *nrlst * 21 ;
    }

    if ( infadm[13-1] == 0 && infadm[25-1] == 1 )
    {
      /* no seiches - computational results available */
      if ( infadm[17-1] > 0 )
      {
          /* directional spreaded results */
            *nrlst = *nrlst + 1 ;
            if ( maxlst < *nrlst )
            {
                *ierror = IEPMNY ;
                return ;
            }
            strcpy( parlst + ind21      , "Wave height dir. spr" ) ;
            strcpy( paruni + ind21      , "m" ) ;

          if (infadm[17-1] == 1)
          {
              partyp[ind]   = ODS_PT_LOC_MNK ;
          }
          else
          {
        partyp[ind]   = ODS_PT_HIERACHY ;
          }
            parcod[ind]   = 7              ;

          ind   = *nrlst ;
          ind21 = *nrlst * 21 ;
      }
      else
      {
          /* uni-directional results */
            *nrlst = *nrlst + 3 ;
            if ( maxlst < *nrlst )
            {
                *ierror = IEPMNY ;
                return ;
            }
            strcpy( parlst + ind21      , "Wave height" ) ;
            strcpy( parlst + ind21 + 21 , "Wave phase"  ) ;
            strcpy( parlst + ind21 + 42 , "Wave image"  ) ;
            strcpy( paruni + ind21      , "m" ) ;
            strcpy( paruni + ind21 + 21 , ""  ) ;
            strcpy( paruni + ind21 + 42 , "m" ) ;

          if ( infadm[16-1] == 1 )
          {
              partyp[ind]   = ODS_PT_LOC_MNK ;
              partyp[ind+1] = ODS_PT_LOC_MNK ;
              partyp[ind+2] = ODS_PT_LOC_MNK ;
          }
          else
            {
              partyp[ind]   = ODS_PT_HIERACHY ;
              partyp[ind+1] = ODS_PT_HIERACHY ;
              partyp[ind+2] = ODS_PT_HIERACHY ;
          }
            parcod[ind]   = 8              ;
            parcod[ind+1] = 9              ;
            parcod[ind+2] = 10             ;

          ind   = *nrlst ;
          ind21 = *nrlst * 21 ;
      }
    }

    if ( infadm[13-1] == 1 && infadm[25-1] == 1)
    {
        /* seiches - computational results available */
        *nrlst = *nrlst + 6 ;
        if ( maxlst < *nrlst )
        {
            *ierror = IEPMNY ;
            return ;
        }
        strcpy( parlst + ind21      , "Wave height" ) ;
        strcpy( parlst + ind21 + 21 , "Wave phase"  ) ;
        strcpy( parlst + ind21 + 42 , "Wave image"  ) ;
        strcpy( parlst + ind21 + 63 , "Umax"        ) ;
        strcpy( parlst + ind21 + 84 , "Umin"        ) ;
        strcpy( parlst + ind21 + 105, "Udir"        ) ;
        strcpy( paruni + ind21      , "m" ) ;
        strcpy( paruni + ind21 + 21 , ""  ) ;
        strcpy( paruni + ind21 + 42 , "m" ) ;
        strcpy( paruni + ind21 + 63 , "m/s" ) ;
        strcpy( paruni + ind21 + 84 , "m/s" ) ;
        strcpy( paruni + ind21 + 105, "rad" ) ;

        partyp[ind]   = ODS_PT_HIERACHY ;
        partyp[ind+1] = ODS_PT_HIERACHY ;
        partyp[ind+2] = ODS_PT_HIERACHY ;
        partyp[ind+3] = ODS_PT_HIERACHY ;
        partyp[ind+4] = ODS_PT_HIERACHY ;
        partyp[ind+5] = ODS_PT_HIERACHY ;
        parcod[ind]   = 11             ;
        parcod[ind+1] = 12             ;
        parcod[ind+2] = 13             ;
        parcod[ind+3] = 14             ;
        parcod[ind+4] = 15             ;
        parcod[ind+5] = 16             ;

        ind   = *nrlst ;
        ind21 = *nrlst * 21 ;
    }
    return ;
}
/* @@------------------------------------------
    Function: ODSGetHierarchPar()
    Author:   Peter van den Bosch
    Purpose:  Get sub-parameters of a main parameter from PHAROS
            (nefis admin) file
    Context:  Get list of available parameters
    Pseudo Code:
            - fill parameter list depending on main parameter
        and available directions or frequencies
  ------------------------------------------------*/

TVoid ODSGetHierarchPar(
   TInt4 *  infadm,  /* Pharos information vector      */
   TReal4 * parinf,  /* information about sub-parameters (directions or freq)*/
   TString  parlst,
   TString  paruni,
   TInt4    maxlst,
   TInt4    parid,   /* Basic parcode, depending on pardep */
   TInt4 *  nrlst,
   TInt4 *  partyp,
   TInt4 *  parcod,
   TInt4 *  ierror )
{
   TInt4  i ;
   TReal4 dir,
          freq ;
   TChar  hlpstr[21] ;

/*  Put the names in the arrays
 *
 *  Next basic codes are used for parcod
 *    Wave height dir. spr     7000
 *    Wave height              8000
 *    Wave phase               9000
 *    Wave image              10000
 *    Wave height (seiches)   11000
 *    Wave phase  (seiches)   12000
 *    Wave image  (seiches)   13000
 *    Umax        (seiches)   14000
 *    Umin        (seiches)   15000
 *    Udir        (seiches)   16000
 *
 *    These codes are increased with the particular sequence number of
 *    the sub-parameter
 */

   if ( parid == 8000 || parid == 9000 || parid == 10000 )
   {
      /* parameters uni-directional computation - add wave directions */

      *nrlst = infadm[16-1] ;
      if ( maxlst < *nrlst )
      {
         *ierror = IEPMNY ;
         return ;
      }
      for ( i = 0; i < infadm[16-1]; i++ )
      {
         dir = parinf[i] ;
         sprintf( hlpstr, "%5.1f deg", dir ) ;
         strcpy( parlst + 21*i, hlpstr );
         strcpy( paruni + 21*i, "m"    ) ;

         partyp[i] = ODS_PT_LOC_MNK ;
         parcod[i] = parid + i + 1   ;
      }
   }
   if ( parid == 7000 )
   {
      /* parameters directional spreaded computation - add wave directions */

      *nrlst = infadm[17-1] ;
      if ( maxlst < *nrlst )
      {
         *ierror = IEPMNY ;
         return ;
      }
      for ( i = 0; i < infadm[17-1]; i++ )
      {
         dir = parinf[i] ;
         sprintf( hlpstr, "%5.1f deg", dir ) ;
         strcpy( parlst + 21*i, hlpstr );
         strcpy( paruni + 21*i, "m"    ) ;

         partyp[i] = ODS_PT_LOC_MNK ;
         parcod[i] = parid + i + 1   ;
      }
   }

   if ( parid >= 11000 && parid <= 16000 )
   {
      /* parameters seiches computation - add frequencies */

      *nrlst = infadm[19-1] ;
      if ( maxlst < *nrlst )
      {
         *ierror = IEPMNY ;
         return ;
      }
      for ( i = 0; i < infadm[19-1]; i++ )
      {
         freq = parinf[i] ;
         sprintf( hlpstr, "%8.6f Hz", freq ) ;
         strcpy( parlst + 21*i, hlpstr );
         strcpy( paruni + 21*i, "Hz"    ) ;

         partyp[i] = ODS_PT_LOC_MNK    ;
         parcod[i] = parcod[i] + parid ;
      }
   }
   return ;
}

/* @@------------------------------------------
    Function: ODSGetLocPharAmp()
    Author:   Peter van den Bosch
    Purpose:  Get locations of seiches computation from PHAROS admin file
    Context:  ODS function GETLOC
    Pseudo Code:
  ------------------------------------------------*/
void ODSGetLocPharAmp(
   TString   fname ,
   TInt4   * ftype,
   TString   locdef,
   TInt4     maxdef,
   TInt4     pardep,
   TInt4     timdep,
   TString   loclst,
   TInt4   * loctyp,
   TInt4   * locnr,
   TInt4     maxlst,
   TInt4   * nrlst,
   TInt4   * ierror,
   TString   option )
/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       maxdef  -          I   Max. nr of locations wanted.             */
/*       maxlst  -          I   Max. nr of locations to return.          */
/*       nrlst   -          O   Nr of locations returned.                */
/*       locdef  maxdef     I   List of locations wanted.                */
/*       loclst  maxlst     O   List of locations found.                 */
/*       loctyp  maxlst     O   List of types of locations found.        */
/*                                                                       */
/*************************************************************************/

{
   TInt4    datfds[ 999],
            deffds[2997] ;
   TInt4 *  infadm ,
         *  irl ;
   TInt4    uindex[1][3],
            usrord[1] ;
   TInt4    ind21,
            ind,
            buflen ,
            i,
            ldum ;
   TChar    buffer[80];

   *ierror = IEOK ;

   if ( pardep <= 16 || pardep > 21 )
   {
      *nrlst = 0;
      return ;
   }

   OPNNEF( fname, ftype, datfds, deffds, ierror) ;
   if ( *ierror != IEOK )
   {
      return ;
   }

   /* get informationvector INFADM from file */
   /* start with index 0; so the indices should be corrected with one */
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   usrord[0] = 1;
   buflen = 200 * sizeof(TInt4) ;
   infadm = (TInt4 *) DM_Malloc( buflen) ;
   *ierror = Getelt( deffds, "INFO", "INFADM", uindex,
                     usrord, &buflen, infadm ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      DM_Free( infadm );
      return ;
   }

   *nrlst = infadm[33-1];

   if ( maxlst < *nrlst )
   {
      *ierror = IEPMNY ;
      CLOSFL( fname, &ldum  ) ;
      DM_Free( infadm );
      return ;
   }
   buflen = 80;
   ind21  = 0;
   for ( i = 0; i < *nrlst; i++ )
   {
      uindex[0][0] = i+1;
      uindex[0][1] = i+1;
      uindex[0][2] = 1;
      usrord[0] = 1;
      *ierror = Getelt( deffds, "SEICH_loc", "DESCR", uindex,
                        usrord, &buflen, buffer ) ;

      strncpy( loclst + ind21, buffer, 20 );
      locnr[i]  = i ;
      loctyp[i] = 0 ; /* AM: value 1 triggers an error in evaddset.c */
      *(loclst+ind21+20) = '\0';
      ind21 = ind21 + 21;
   }

   CLOSFL( fname, ierror ) ;
   DM_Free( infadm );
}

/*************************************************************************/
/*    SUBROUTINE Get Time list from PHAROS admin file (map file)         */
/*************************************************************************/
/* @@------------------------------------------
    Function: ODSGetTmePharAmp()
    Author:   Peter van den Bosch
    Purpose:  Get time levels from PHAROS map (nefis admin) file
    Context:  ODS function GETTME
    Pseudo Code:
              - information is independent of times, so return dummy
  ------------------------------------------------*/

TVoid ODSGetTmePharAmp ( TString fname,
                         TInt4   *ftype,
                         TReal8 *timdef,
                         TInt4   maxdef,
                         TInt4   pardef,
                         TInt4   locdep,
                         TReal8  *timlst,
                         TInt4   maxlst,
                         TInt4   *nrlst,
                         TInt4   *ierror,
                         TString option)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       locdep  -          I   Location dependency (ignored)            */
/*       maxdef  -          I   Number of values for time intervals      */
/*       maxlst  -          I   Max. nr of times to return.              */
/*       nrlst   -          O   Nr of times returned.                    */
/*       option  -          I   Option (future use; ignored)             */
/*       parcod  maxdef     I   List of codes (indices) of parameters    */
/*                              found.                                   */
/*       pardep  -          I   Parameter dependency (ignored)           */
/*       parlst  maxlst     O   List of parameters found.                */
/*       partyp  maxlst     O   List of types of parameters found.       */
/*       timdef  maxdef     I   Period of interest (ignored)             */
/*       timlst  maxlst     O   TImes present in file (list is empty)    */
/*                                                                       */
/*************************************************************************/
{
   TInt4 iyear0 , imonth0 , iday0 , ihour0 , imin0 , isec0 ;

/* No need to scan the file: we know what times it contains, if it
   exists ...
*/
   *ierror = IEOK ;

   *nrlst  = 1    ;

   iyear0  = 1900 ;
   imonth0 =    1 ;
   iday0   =    1 ;
   ihour0  =    0 ;
   imin0   =    0 ;
   isec0   =    0 ;
   julian ( (TInt4 *) &iyear0, (TInt4 *) &imonth0, (TInt4 *) &iday0,
            (TInt4 *) &ihour0, (TInt4 *) &imin0,   (TInt4 *) &isec0,
            (TReal8 *) &timlst[0] ) ;

   return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Values (in matrix form) from PHAROS admin file      */
/*************************************************************************/
/* @@------------------------------------------
    Function: ODSGetMatPharMap()
    Author:   Peter van den Bosch
    Purpose:  Get data from PHAROS map (nefis admin) file
    Context:  ODS function GETMATs
    Pseudo Code:
              - read information vector from the file
              - get data corresponding to desired parameter specified in
                parcod
  ------------------------------------------------*/

TVoid ODSGetMatPharMap(  TString fname,
                         TInt4   itype,
                         TInt4   parcod ,
                         TReal8  *tim ,
                         TInt4   *loc ,
                         TReal4  misval ,
                         TInt4   maxdim ,
                         TReal4  *data ,
                         TInt4   *ierror )
{

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       itype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       parcod  -          I   Parameter code (which parameter to return*/
/*       loc     -          I   Locations (ignored for parcod<17)        */
/*       misval  -          I   Missing value                            */
/*       maxdim  -          I   Maximum number of values (should be enough) */
/*       data    -          O   Values returned                          */
/*       tim     3          I   Return which times (ignored)             */
/*                                                                       */
/*************************************************************************/

   TInt4    datfds[ 999],
            deffds[2997] ;
   TInt4 *  infadm,
         *  indbuf;
   TInt4    uindex[1][3],
            usrord[1] ;
   TInt4    ind21,
            ind,
            buflen ;
   TInt4    nnod,
            lparm,
            seq,
            np,
            lrec,
            i_rec,
            i_data,
            i,
            ldum ;
   TReal4   arg ,
            hmmax,
            fsmax,
            dd,
            pi2,
            pi23;
   TReal4 * ampl,
          * fase ;
   TReal8 * buffer,
          * bufr,
          * bufi ;
   TChar    grpnam[20],
            elmrnm[20],
            elminm[20];

   *ierror = IEOK ;
   seq = 1 ;

   /* define parameter code and sequence number */
   if ( parcod >= 1000 )
   {
      lparm = parcod/1000 ;
      seq   = parcod - lparm * 1000 ;
   }
   else
   {
      lparm = parcod ;
   }

   /* open the nefis file */
   OPNNEF( fname, &itype, datfds, deffds, ierror) ;
   if ( *ierror != IEOK )
   {
      return ;
   }

   /* get informationvector INFADM from file */
   /* start with index 0; so the indices should be corrected with one */
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   usrord[0] = 1;
   buflen = 200 * sizeof(TInt4) ;
   infadm = (TInt4 *) DM_Malloc( buflen) ;
   *ierror = Getelt( deffds, "INFO", "INFADM", uindex,
                     usrord, &buflen, infadm ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      DM_Free( infadm );
      return ;
   }

   nnod = infadm[0] ;

   /* AM: this can be made quite a lot shorter and clearer! */

   if ( lparm == 1 )
   {
      /* parcod = 1    -  get the X_coor */
      uindex[0][0] = 1;
      uindex[0][1] = nnod;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      buffer = (TReal8 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "GRID_coor", "X_coor", uindex,
                        usrord, &buflen, buffer ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( buffer );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         data[i] = (TReal4) buffer[i] ;
      }

      DM_Free( buffer ) ;
   }

   if ( lparm == 2 )
   {
      /* parcod = 1    -  get Y_coor */
      uindex[0][0] = 1;
      uindex[0][1] = nnod;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      buffer = (TReal8 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "GRID_coor", "Y_coor", uindex,
                        usrord, &buflen, buffer ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( buffer );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         data[i] = (TReal4) buffer[i] ;
      }

      DM_Free( buffer ) ;
   }

   if ( lparm == 3 )
   {
      /* parcod = 3    -  get Waterdepth */
      uindex[0][0] = 1;
      uindex[0][1] = 1;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      buffer = (TReal8 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "GRID", "H_depth", uindex,
                        usrord, &buflen, buffer ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( buffer );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         data[i] = (TReal4) buffer[i] ;
      }

      DM_Free( buffer ) ;
   }

   if ( lparm == 4 )
   {
      /* parcod = 4    -  get Ux velocity */
      uindex[0][0] = 1;
      uindex[0][1] = 1;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      buffer = (TReal8 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "CURRENT", "Ux_veloc", uindex,
                        usrord, &buflen, buffer ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( buffer );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         data[i] = (TReal4) buffer[i] ;
      }

      DM_Free( buffer ) ;
   }

   if ( lparm == 5 )
   {
      /* parcod = 5    -  get Uy velocity */
      uindex[0][0] = 1;
      uindex[0][1] = 1;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      buffer = (TReal8 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "CURRENT", "Uy_veloc", uindex,
                        usrord, &buflen, buffer ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( buffer );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         data[i] = (TReal4) buffer[i] ;
      }

      DM_Free( buffer ) ;
   }

   if ( lparm == 6 )
   {
      /* parcod = 6    -  get breaking index gamma-b */
      uindex[0][0] = 1;
      uindex[0][1] = 1;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      buffer = (TReal8 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "BREAKING", "Gamma_b", uindex,
                        usrord, &buflen, buffer ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( buffer );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         data[i] = (TReal4) buffer[i] ;
      }

      DM_Free( buffer ) ;
   }

   if ( lparm == 7 )
   {
      /* parcod = 7    -  get directional spreaded wave height */
      uindex[0][0] = seq;
      uindex[0][1] = seq;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      buffer = (TReal8 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "HS_dir", "HS_directional", uindex,
                        usrord, &buflen, buffer ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( buffer );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         data[i] = (TReal4) buffer[i] ;
      }

      DM_Free( buffer ) ;
   }

   if ( lparm == 8 || lparm == 11 )
   {
      /* parcod = 8    -  uni-directional wave height
         parcod = 11   -  wave height seiches
         get Potential and compute wave height
      */
      uindex[0][0] = seq;
      uindex[0][1] = seq;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      bufr   = (TReal8 *) DM_Malloc( buflen) ;
      bufi   = (TReal8 *) DM_Malloc( buflen) ;

      if ( lparm == 8 )
      {
         strcpy( grpnam, "POTENTIALS" );
         strcpy( elmrnm, "PHI_r" );
         strcpy( elminm, "PHI_i" );
      }
      else if ( lparm == 11 )
      {
         strcpy( grpnam, "SEICH_res" );
         strcpy( elmrnm, "PHIs_r" );
         strcpy( elminm, "PHIs_i" );
      }

      *ierror = Getelt( deffds, grpnam, elmrnm, uindex,
                        usrord, &buflen, bufr   ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( bufr );
         DM_Free( bufi );
         return ;
      }

      *ierror = Getelt( deffds, grpnam, elminm, uindex,
                        usrord, &buflen, bufi   ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( bufr );
         DM_Free( bufi );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         arg = (TReal4) ( bufr[i]*bufr[i] + bufi[i]*bufi[i] );
         data[i] = 2.0 * sqrt( arg ) ;
      }

      DM_Free( bufr ) ;
      DM_Free( bufi ) ;
   }

   if ( lparm == 9 || lparm == 12 )
   {
      /* parcod = 9    -  uni-directional wave phase
       parcod = 12   -  wave phase seiches
         get Potential and compute wave phase
       */
      uindex[0][0] = seq;
      uindex[0][1] = seq;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      bufr   = (TReal8 *) DM_Malloc( buflen) ;
      bufi   = (TReal8 *) DM_Malloc( buflen) ;

      if ( lparm == 9 )
      {
         strcpy( grpnam, "POTENTIALS" );
         strcpy( elmrnm, "PHI_r" );
         strcpy( elminm, "PHI_i" );
      }
      else if ( lparm == 12 )
      {
         strcpy( grpnam, "SEICH_res" );
         strcpy( elmrnm, "PHIs_r" );
         strcpy( elminm, "PHIs_i" );
      }
      *ierror = Getelt( deffds, grpnam, elmrnm, uindex,
                        usrord, &buflen, bufr   ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( bufr );
         DM_Free( bufi );
         return ;
      }

      *ierror = Getelt( deffds, grpnam, elminm, uindex,
                        usrord, &buflen, bufi   ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( bufr );
         DM_Free( bufi );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
          data[i] = atan2( bufi[i], bufr[i] ) ;
      }

      DM_Free( bufr ) ;
      DM_Free( bufi ) ;
   }

   if ( lparm == 10 || lparm == 13 )
   {
      /* parcod = 10   -  uni-directional wave image
         parcod = 13   -  wave image seiches
         get Potential and compute wave image
      */
      uindex[0][0] = seq;
      uindex[0][1] = seq;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      bufr   = (TReal8 *) DM_Malloc( buflen) ;
      bufi   = (TReal8 *) DM_Malloc( buflen) ;
      ampl   = (TReal4 *) DM_Malloc( buflen) ;
      fase   = (TReal4 *) DM_Malloc( buflen) ;

      if ( lparm == 10 )
      {
         strcpy( grpnam, "POTENTIALS" );
         strcpy( elmrnm, "PHI_r" );
         strcpy( elminm, "PHI_i" );
      }
      else if ( lparm == 13 )
      {
         strcpy( grpnam, "SEICH_res" );
         strcpy( elmrnm, "PHIs_r" );
         strcpy( elminm, "PHIs_i" );
      }
      *ierror = Getelt( deffds, grpnam, elmrnm, uindex,
                        usrord, &buflen, bufr   ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( bufr );
         DM_Free( bufi );
         DM_Free( ampl );
         DM_Free( fase );
         return ;
      }

      *ierror = Getelt( deffds, grpnam, elminm, uindex,
                        usrord, &buflen, bufi   ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( bufr );
         DM_Free( bufi );
         DM_Free( ampl );
         DM_Free( fase );
         return ;
      }

      /* compute wave height and phase */
      hmmax = 0.0 ;
      for ( i = 0; i < nnod ; i++ )
      {
         arg = (TReal4) ( bufr[i]*bufr[i] + bufi[i]*bufi[i] );
         ampl[i] = sqrt( arg ) ;
         fase[i] = atan2( bufi[i], bufr[i] ) ;
         if ( ampl[i] > hmmax )
         {
            hmmax = ampl[i] ;
            fsmax = fase[i] ;
         }
      }

      /* compute wave image and copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         fase[i] = fase[i]-fsmax ;
         data[i] = ampl[i] * cos( fase[i] ) ;
      }

      DM_Free( bufr ) ;
      DM_Free( bufi ) ;
      DM_Free( ampl ) ;
      DM_Free( fase ) ;
   }

   if ( lparm == 14 )
   {
      /* parcod = 14    -  get maximum u-velocities Umax */
      uindex[0][0] = seq;
      uindex[0][1] = seq;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      buffer = (TReal8 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "SEICH_res", "UMAX", uindex,
                        usrord, &buflen, buffer ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( buffer );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         data[i] = (TReal4) buffer[i] ;
      }

      DM_Free( buffer ) ;
   }

   if ( lparm == 15 )
   {
      /* parcod = 15    -  get minimum u-velocities Umin */
      uindex[0][0] = seq;
      uindex[0][1] = seq;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      buffer = (TReal8 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "SEICH_res", "UMIN", uindex,
                        usrord, &buflen, buffer ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( buffer );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         data[i] = (TReal4) buffer[i] ;
      }

      DM_Free( buffer ) ;
   }

   if ( lparm == 16 )
   {
      /* parcod = 16    -  get directions of maximum velocities  UDIR */
      uindex[0][0] = seq;
      uindex[0][1] = seq;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = nnod * sizeof(TReal8) ;
      buffer = (TReal8 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "SEICH_res", "UDIR", uindex,
                        usrord, &buflen, buffer ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( buffer );
         return ;
      }

      /* copy result to output array */
      for ( i = 0; i < nnod ; i++ )
      {
         data[i] = (TReal4) buffer[i] ;
      }

      DM_Free( buffer ) ;
   }

   if ( lparm > 16 && lparm < 22 )
   {
      /* parcod > 16 en < 22    -  first get nodal point nr of location */
      uindex[0][0] = loc[0]+1;
      uindex[0][1] = loc[0]+1;
      uindex[0][2] = 1;
      usrord[0] = 1;
      buflen = 4;
      *ierror = Getelt( deffds, "SEICH_loc", "Point_nr", uindex,
                        usrord, &buflen, &np     ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         return ;
      }

      /* get index nrs of computed frequencies */
      uindex[0][0] = 1;
      uindex[0][1] = 1;
      uindex[0][2] = 1;
      usrord[0] = 1;
      lrec = infadm[30-1];
      buflen = lrec * sizeof(TInt4) ;
      indbuf = (TInt4 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "SEICH_def", "IRL", uindex,
                        usrord, &buflen, indbuf ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( indbuf );
         return ;
      }

      i_data = 0;
      for ( i_rec = 0; i_rec < lrec; i_rec++ )
      {
         if ( indbuf[i_rec] == 0 )
         {
             continue;
         }

         if ( lparm == 17 || lparm == 18 )
         {
            /* parcod = 17  - wave amplification
               parcod = 18  - phase at location
            */
            uindex[0][0] = indbuf[i_rec];
            uindex[0][1] = indbuf[i_rec];
            uindex[0][2] = 1;
            usrord[0] = 1;
            buflen = nnod * sizeof(TReal8) ;
            bufr   = (TReal8 *) DM_Malloc( buflen) ;
            bufi   = (TReal8 *) DM_Malloc( buflen) ;

            *ierror = Getelt( deffds, "SEICH_res", "PHIs_r", uindex,
                              usrord, &buflen, bufr   ) ;

            if ( *ierror != IEOK )
            {
               *ierror = IEINFO ;
               CLOSFL( fname, &ldum  ) ;
               DM_Free( infadm );
               DM_Free( indbuf );
               DM_Free( bufr  );
               DM_Free( bufi  );
               return ;
            }

            *ierror = Getelt( deffds, "SEICH_res", "PHIs_i", uindex,
                              usrord, &buflen, bufi   ) ;

            if ( *ierror != IEOK )
            {
               *ierror = IEINFO ;
               CLOSFL( fname, &ldum  ) ;
               DM_Free( infadm );
               DM_Free( indbuf );
               DM_Free( bufr  );
               DM_Free( bufi  );
               return ;
            }

            /* compute wave height and phase */
            arg = (TReal4) ( bufr[np]*bufr[np] + bufi[np]*bufi[np] );
            if ( lparm == 17 )
            {
               data[i_data] = 2.0 * sqrt( arg );
            }
            if ( lparm == 18 )
            {
               data[i_data] = atan2( bufi[np], bufr[np] );
            }
            i_data++;
            DM_Free( bufi );
            DM_Free( bufr );
         } /* end of parameters wave amplification and phase */

        if ( lparm == 19 || lparm == 20 || lparm == 21 )
        {
            /* parcod = 19  - Umax - max velocity
               parcod = 20  - Umin - min velocity
               parcod = 21  - Udir - direction of velocity
            */
            uindex[0][0] = indbuf[i_rec];
            uindex[0][1] = indbuf[i_rec];
            uindex[0][2] = 1;
            usrord[0] = 1;
            buflen = nnod * sizeof(TReal8) ;
            bufr   = (TReal8 *) DM_Malloc( buflen) ;

            if ( lparm == 19 )
            {
               strcpy( elmrnm, "UMAX" );
            }
            if ( lparm == 20 )
            {
               strcpy( elmrnm, "UMIN" );
            }
            if ( lparm == 21 )
            {
               strcpy( elmrnm, "UDIR" );
            }
            *ierror = Getelt( deffds, "SEICH_res", elmrnm, uindex,
                              usrord, &buflen, bufr   ) ;

            if ( *ierror != IEOK )
            {
               *ierror = IEINFO ;
               CLOSFL( fname, &ldum  ) ;
               DM_Free( infadm );
               DM_Free( indbuf );
               DM_Free( bufr  );
               return ;
            }

            data[i_data] = bufr[np];
            i_data++;
            DM_Free( bufr );
        } /* end of velocities at locations */
     } /* end of for loop for frequencies */

     if ( lparm == 21 )
     {
        /* smooth directions to avoid discontinuty's */
        pi2  = 0.5 * PI;
        pi23 = 1.5 * PI;
        if ( data[0] < pi2 ) data[0] = data[0] + PI;
        if ( data[0] > pi23) data[0] = data[0] - PI;
        for ( i = 1; i < i_data; i++ )
        {
           dd = data[i] - data[i-1];
           if ( dd > pi2 ) data[i] = data[i] - PI;
           if ( dd < pi2 ) data[i] = data[i] + PI;
        }
     }

     DM_Free( indbuf );
   }
   if ( lparm == 22 )
   {
      /* parcod = 22  - FREQ - computed frequencies
      */
      /* get index nrs of computed frequencies */
      uindex[0][0] = 1;
      uindex[0][1] = 1;
      uindex[0][2] = 1;
      usrord[0] = 1;
      lrec = infadm[30-1];
      buflen = lrec * sizeof(TInt4) ;
      indbuf = (TInt4 *) DM_Malloc( buflen) ;
      *ierror = Getelt( deffds, "SEICH_def", "IRL", uindex,
                        usrord, &buflen, indbuf ) ;

      if ( *ierror != IEOK )
      {
         *ierror = IEINFO ;
         CLOSFL( fname, &ldum  ) ;
         DM_Free( infadm );
         DM_Free( indbuf );
         return ;
     }
     uindex[0][0] = 1;
     uindex[0][1] = 1;
     uindex[0][2] = 1;
     usrord[0] = 1;
     buflen = lrec * sizeof(TReal8) ;
     bufr   = (TReal8 *) DM_Malloc( buflen) ;
     *ierror = Getelt( deffds, "SEICH_def", "FREQ", uindex,
                       usrord, &buflen, bufr   ) ;

     if ( *ierror != IEOK )
     {
        *ierror = IEINFO ;
        CLOSFL( fname, &ldum  ) ;
        DM_Free( infadm );
        DM_Free( indbuf );
        DM_Free( bufr   );
        return ;
     }
     i_data = 0;
     for ( i_rec = 0; i_rec < lrec; i_rec++ )
     {
        if ( indbuf[i_rec] == 0 )
        {
           continue;
        }
        data[i_data] = bufr[i_rec];
        i_data++;
     }

     DM_Free( bufr );
     DM_Free( indbuf );
   }


   /* Free buffers */
   DM_Free( infadm) ;

   /* Close file */
   CLOSFL( fname, ierror ) ;

   return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Administration array from SHYFEM grid file          */
/*************************************************************************/

TVoid ODSGetGrdPharAmp(  TString fname,
                         TInt4   itype,
                         TInt4   *indloc ,
                         TInt4   *indx ,
                         TInt4   *nocell ,
                         TInt4   *igisty ,
                         TInt4   *ierror )
{
/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       itype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       indloc  -          I   Locations indices that are wanted (ignored) */
/*       indx    -          O   Administration array                     */
/*       nocell  -          O   Number of cells associated with grid     */
/*       igisty  -          O   Type of grid/GIS-information             */
/*                                                                       */
/*************************************************************************/

   TInt4    datfds[ 999],
            deffds[2997] ;
   TInt4 *  infadm ;
   TInt4    uindex[1][3],
            usrord[1] ;
   TInt4    ind,
            buflen ;
   TInt4    i,
            ntper,
            ldum;

   *ierror = IEOK ;
   *igisty = IGX1D ;

   OPNNEF( fname, &itype, datfds, deffds, ierror) ;
   if ( *ierror != IEOK )
   {
      return ;
   }

   /* get informationvector INFADM from file */
   /* start with index 0; so the indices should be corrected with one */
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   usrord[0] = 1;
   buflen = 200 * sizeof(TInt4) ;
   infadm = (TInt4 *) DM_Malloc( buflen) ;
   *ierror = Getelt( deffds, "INFO", "INFADM", uindex,
                     usrord, &buflen, infadm ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      DM_Free( infadm );
      return ;
   }

   ntper = infadm[19-1] ;
   for ( i = 0; i < ntper; i++ )
   {
      indx[i] = i;
   }

   *nocell = ntper;

   DM_Free( infadm );
   return;
}

TVoid ODSGetGrdPharMap(  TString fname,
                         TInt4   itype,
                         TInt4   *indloc ,
                         TInt4   *indx ,
                         TInt4   *nocell ,
                         TInt4   *igisty ,
                         TInt4   *ierror )
{
/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       itype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       indloc  -          I   Locations indices that are wanted (ignored) */
/*       indx    -          O   Administration array                     */
/*       nocell  -          O   Number of cells associated with grid     */
/*       igisty  -          O   Type of grid/GIS-information             */
/*                                                                       */
/*************************************************************************/

   TInt4    datfds[ 999],
            deffds[2997] ;
   TInt4 *  infadm ;
   TInt4    uindex[1][3],
            usrord[1] ;
   TInt4    ind,
            buflen ;
   TInt4    nelgrp,
            lengkc,
            i,
            ldum ;
   TInt4 *  inpelm,
         *  inelem,
         *  kmeshc ;

   *ierror = IEOK ;
   *igisty = IGFEM3P ;

   OPNNEF( fname, &itype, datfds, deffds, ierror) ;
   if ( *ierror != IEOK )
   {
      return ;
   }

   /* get informationvector INFADM from file */
   /* start with index 0; so the indices should be corrected with one */
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   usrord[0] = 1;
   buflen = 200 * sizeof(TInt4) ;
   infadm = (TInt4 *) DM_Malloc( buflen) ;
   *ierror = Getelt( deffds, "INFO", "INFADM", uindex,
                     usrord, &buflen, infadm ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      DM_Free( infadm );
      return ;
   }

   nelgrp = infadm[3-1] ;

   /* get information about elementgroups (nr of nodal points in an
      element and nr of elements in a group)
   */
   uindex[0][0] = 1;
   uindex[0][1] = nelgrp;
   uindex[0][2] = 1;
   usrord[0] = 1;
   buflen = nelgrp * sizeof(TInt4) ;
   inpelm = (TInt4 *) DM_Malloc( buflen) ;
   inelem = (TInt4 *) DM_Malloc( buflen) ;

   *ierror = Getelt( deffds, "MESH_1", "INPELM", uindex,
                     usrord, &buflen, inpelm ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      DM_Free( infadm );
      DM_Free( inpelm );
      DM_Free( inelem );
      return ;
   }
   *ierror = Getelt( deffds, "MESH_1", "INELEM", uindex,
                     usrord, &buflen, inelem ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      DM_Free( infadm );
      DM_Free( inpelm );
      DM_Free( inelem );
      return ;
   }

   /* compute index in connectivity array for the triangles */
   ind = 0 ;
   for ( i = 0; i < nelgrp-1; i++ )
   {
      ind = ind + inpelm[i]*inelem[i] ;
   }

   /* get complete mesh connectivity array */
   lengkc = infadm[22-1];
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   usrord[0] = 1;
   buflen = lengkc * sizeof(TInt4) ;
   kmeshc = (TInt4 *) DM_Malloc( buflen) ;

   *ierror = Getelt( deffds, "MESH_2", "KMESHC", uindex,
                     usrord, &buflen, kmeshc ) ;

   if ( *ierror != IEOK )
   {
      *ierror = IEINFO ;
      CLOSFL( fname, &ldum  ) ;
      DM_Free( infadm );
      DM_Free( inpelm );
      DM_Free( inelem );
      DM_Free( kmeshc );
      return ;
   }

   /* copy mesh connectivity for triangles to the index array */
   *nocell = 3*inelem[nelgrp-1];

   for ( i = 0; i < *nocell; i++ )
   {
      indx[i] = kmeshc[ind+i] ;
   }

   /* Free buffers */
   DM_Free( infadm) ;
   DM_Free( inpelm) ;
   DM_Free( inelem) ;
   DM_Free( kmeshc) ;

   /* Close file */
   CLOSFL( fname, ierror ) ;

   return ;
}
