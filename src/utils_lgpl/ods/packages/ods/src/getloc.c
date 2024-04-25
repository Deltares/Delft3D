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
 *  getloc.c  -  ODS Get list of locations from file
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 5-04-04 9:43 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/getloc.c,v $
 */


/*   Date:       28 Feb 1994                                          */
/*   Time:       14:50                                                */
/*   Program:    GETLOC.C                                             */
/*   Version:    1.03                                                 */
/*   Programmer: Andre Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.02 -- 6 Dec 1993 -- 13:00 -- Operating System: DOS             */
/*   1.01 -- 5 Aug 1993 -- 16:31 -- Operating System: DOS             */
/*   1.00 -- 5 Aug 1993 -- 16:21 -- Operating System: DOS             */
/*   0.00 -- 5 Aug 1993 -- 12:31 -- Operating System: DOS             */
/*   Project:    Open Data Structuur                                  */
/*   Module:     GetLocation                                          */
/*   Function:                                                        */
/*   Comment:    Added WAS and WS1 files                              */
/*   Reference:                                                       */
/*   Review:                                                          */

#include "portable.h"
#include "ods.h"

#include <stdio.h>

#ifndef NOPROT

/* function prototypes */

#include "odsmodel.h"

TInt4 iftype ( char *fname) ;

#endif


/* the function to get locations from 'any' file:   */

void DllExport getloc ( char  *fname,  TInt4 *itype,  char *locdef,
                        TInt4 *maxdef, TInt4 *pardep, TInt4 *timdep,
                        TInt4 *maxlst, char  *loclst, TInt4 *loctyp,
                        TInt4 *locnr , TInt4 *nrlst,  TInt4 *ierror,
                        char  *option)

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    Size      I/O  Description                             */
/*        ------  --------  ---  ------------------------------------    */
/*        fname   -          I   Full filename, including extension      */
/*        ierror  -          O   Errorcode, see errors.inc for definitio */
/*        itype   -         I/O  Filetype, see types.inc for definitions */
/*        len.    -          I   UNIX only: length of CHARACTER variable */
/*        nrlst   -          O   Nr of locations returned.               */
/*        maxdef  -          I   Max. nr of wanted locations.            */
/*        maxlst  -          I   Max. nr of locations to return.         */
/*        locdef  maxdef     I   List of wanted locations.               */
/*        loclst  maxlst     O   List of locations found.                */
/*        loctyp  maxlst     O   list of location types                  */
/*        locnr   maxlst     O   List of index numbers of locations found*/
/*        option  -         I/O  option (reserved for future extensions) */
/*        pardep  -          I   parameter dependency of locations       */
/*        timdep  -          I   time dependency of locations            */
/*                                                                       */
/*************************************************************************/

   {
   int i , j , inum , length ;
   char  * zbuffs;
   char *fn;

   *ierror = IEOK;

   /* Make sure the list of parameter names is clean
      - this helps with possible trailing blanks
   */
   memset( loclst, '\0', (PARLEN+1)*(*maxlst) ) ;


   /* strip trailing blanks and add terminating 0 to all strings,
      just to make sure:
   */

   for (inum = 0 ; inum < ODS_FNAME_DIM ; inum++)
   {
       fn = &fname[inum*ODS_FILNAMLEN] ;

       fn[ODS_FILNAMLEN-1] = '\0' ;
       length = strlen( fn ) ;
       for ( i = length-1 ; i >= 0 ; i -- )
       {
          if ( fn[i] == ' ')
          {
             fn[i] = '\0' ;
          }
          else
          {
             break ;
          }
       }
   }

   *nrlst = 0 ;

   /*  Get filetype, if we don't know, first check extension:          */

   if ( *itype == ITUNDE)
      *itype = iftype ( fname) ;

   /*  Now proceed:                                                    */

   switch ( *itype)
      {
      case ITUNDE :

         /* Unable to determine filetype                               */

         *ierror = IEUNDE ;
         break ;

      case ODS_DELWAQ_MAP_NEFIS :
      case ODS_DELWAQ_HIS_NEFIS :
      case ODS_DELPAR_MAP_NEFIS :
      case ODS_DELPAR_HIS_NEFIS :
      case ODS_DELPAR_PLO_NEFIS :

         /* DELWAQ/DELPAR NEFIS filetype */

         DlwGetNefisLoc ( fname, itype, locdef, maxdef, loclst,
                          maxlst, nrlst , locnr, ierror, option );

         break ;


#if 0
      case ITDBF3 :

         /* DBASE 3 filetype                                           */

         ODSGetLocDbf3 ( fname, locdef, maxdef, loclst,
                  maxlst, nrlst , ierror) ;
         break ;

      case ITDBF4 :

         /* DBASE 4 filetype                                           */

         ODSGetLocDbf4 ( fname , locdef, maxdef, loclst,
                  maxlst, nrlst , ierror) ;
         break ;
#endif

      case ITDLWM :
      case ITDLWH :
      case ITDLWB :
      case ITDLPP :

         /* DELWAQ 4 MAP/HIS file, DELPAR PLO file                     */

         ODSGetLocDlwq ( fname , itype, locdef, *maxdef, loclst,
                         *maxlst, nrlst , locnr, ierror) ;
         break ;

      case ITWAS  :

         /* WASPRO WAS file                                            */

         ODSGetLocWas  ( fname , locdef, *maxdef, loclst,
                         *maxlst, nrlst , ierror) ;
         break ;

      case ITWS1  :

         /* WASPRO WS1 file                                            */

         ODSGetLocWs1  ( fname , locdef, *maxdef, loclst,
                  *maxlst, nrlst , ierror) ;
         break ;

      case ITMPX  :

         /* MAPPIX MPX file                                            */

         ODSGetLocMPX  ( fname , locdef, *maxdef, loclst,
                         *maxlst, nrlst , ierror) ;

         break ;

      case ODS_TRISULA_HIS_NEFIS  :

         /* Trisula Nefis history file                                 */
         GNF_GetLoc( fname, itype, locdef, maxdef, pardep, timdep,
            maxlst, loclst, loctyp, locnr, nrlst, ierror, option ) ;

/*
         zbuffs = (char *) malloc ( *maxlst * 21 * sizeof(char) );
         if (zbuffs == NULL)
             *ierror = IEDISK;

         ODS_TRISULA_NEFIS_hisloc( fname, 256, 3, *itype,
                                   locdef, 21, *maxdef,
                                   *maxdef, *pardep,
                                   *timdep, *maxlst,
                                   loclst, 21, *maxlst,
                                   (TInt4 *) loctyp, (TInt4 *) nrlst,
                                   (TInt4 *) locnr,
                                   (TInt4 *) ierror,
                                   zbuffs, 21, *maxlst,
                                   option );
         free( zbuffs );
*/
         break ;

      case ODS_TRISULA_MAP_NEFIS  :

         /* Trisula Nefis map file                                     */
         GNF_GetLoc( fname, itype, locdef, maxdef, pardep, timdep,
            maxlst, loclst, loctyp, locnr, nrlst, ierror, option ) ;

/*
         zbuffs = (char *) malloc ( *maxlst * 21 * sizeof(char) );
         if (zbuffs == NULL)
             *ierror = IEDISK;

         ODS_TRISULA_NEFIS_MAPLOC( fname, 256, 3, *itype,
                                   locdef, 21, *maxdef,
                                   *maxdef, *pardep,
                                   *timdep, *maxlst,
                                   loclst, 21, *maxlst,
                                   (TInt4 *) loctyp, (TInt4 *) nrlst,
                                   (TInt4 *) locnr,
                                   (TInt4 *) ierror,
                                   zbuffs, 21, *maxlst,
                                   option );
         free( zbuffs ) ;
*/
         break ;

      case ODS_TRISULA_DRO_NEFIS  :
      case ODS_DELPAR_TRK_NEFIS   :

         /* Trisula Nefis drogues file                                 */

         TriDroGetLoc( fname,  itype,  locdef, maxdef, pardep, timdep,
                       maxlst, loclst, loctyp, locnr,  nrlst,  ierror,
                       option                                        ) ;

         break ;

      case ODS_SAMPLES_2D    :
      case ODS_SAMPLES_TABLE :
      case ODS_SAMPLES_TIME  :

         /* Samples file (2D or table)                                 */

         SamplesGetLoc( fname,  itype,  locdef, maxdef, pardep, timdep,
                        maxlst, loclst, loctyp, locnr,  nrlst,  ierror,
                        option                                        ) ;

         break ;

      case ODS_MORSYS_HIS_NEFIS  :
         /* Morsysa Nefis history files                                */
/* HACK  not yet implemented
 *       zbuffs = (char *) malloc ( *maxlst * 21 * sizeof(char) );
 *       if (zbuffs == NULL)
 *           *ierror = iedisk;

 *       ODS_MORSYS_NEFIS_hisloc( fname, 256, 3, *itype,
 *                                 locdef, 21, *maxdef,
 *                                 *maxdef, *pardep,
 *                                 *timdep, *maxlst,
 *                                 loclst, 21, *maxlst,
 *                                 (TInt4 *) loctyp, (TInt4 *) nrlst,
 *                                 (TInt4 *) locnr,
 *                                 (TInt4 *) ierror,
 *                                 zbuffs, 21, *maxlst,
 *                                 option );
 *       free( zbuffs );
 */
         break ;

      case ODS_MORSYS_TRAH_NEFIS  :

         /* Morsys Nefis history file                                 */
         GNF_GetLoc( fname, itype, locdef, maxdef, pardep, timdep,
            maxlst, loclst, loctyp, locnr, nrlst, ierror, option ) ;

         break ;

      case ODS_MORSYS_BAGR_NEFIS  :

         /* Morsys Nefis BAGR files (dredging)                      */

         MorOdsGetLoc ( fname, itype, locdef, maxdef, loclst,
                        maxlst, nrlst , locnr, ierror, option );

         break ;


      case ODS_PHAROS_AMP_NEFIS:

               /* PHAROS NEFIS file                                         */
         ODSGetLocPharAmp(
                          (TString) fname , (TInt4 *) itype, locdef, *maxdef,
                          *pardep, *timdep, loclst, (TInt4 *) loctyp,
                          (TInt4 *) locnr, *maxlst, (TInt4 *) nrlst,
                          (TInt4 *) ierror, (TString) option ) ;

         break;

      case ODS_PHIDIAS_HISTORY  :

         /* Phidias Nefis history file                                 */
         zbuffs = (char *) malloc ( *maxlst * 21 * sizeof(char) );
         if (zbuffs == NULL)
             *ierror = IEDISK;

         ODSGetLocPhiHis( fname, 256, 3, *itype,
                                   locdef, 21, *maxdef,
                                   *maxdef, *pardep,
                                   *timdep, *maxlst,
                                   loclst, 21, *maxlst,
                                   (TInt4 *) loctyp, (TInt4 *) nrlst,
                                   (TInt4 *) locnr,
                                   (TInt4 *) ierror,
                                   zbuffs, 21, *maxlst,
                                   option );
         free( zbuffs );
         break ;

      case ODS_PHIDIAS_SPECTRAL:
               /* PHIDIAS spectral NEFIS file      */

         ODSGetLocPhiSpec( fname, 256, 3, *itype, locdef, 21, *maxdef,
                           *maxdef, *pardep, *timdep,
                           loclst, 21, *maxlst, (TInt4 *) loctyp,
                           (TInt4 *) locnr, *maxlst, (TInt4 *) nrlst,
                           (TInt4 *) ierror, option );
         break ;

      case ODS_ANY_TEKAL_ASCII_1D :
      case ODS_ANY_TEKAL_ASCII_1DE :

    /* TEKAL ASCII 1D files                                       */

    ODSGetLocAnyTekAscHis      (
           fname , itype, locdef, *maxdef, *pardep,
                          *timdep, loclst, loctyp, locnr, *maxlst,
           nrlst, ierror, option ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_2D :
      case ODS_ANY_TEKAL_ASCII_VEC :

    /* TEKAL ASCII 2D files  ( scalar and vector )                */

         ODSGetLocAnyTekAscMap      (
            fname , itype, locdef, *maxdef, *pardep,
                          *timdep, loclst, loctyp, locnr, *maxlst,
            nrlst, ierror, option ) ;

         break ;

      case ODS_ANY_TEKAL_ASCII_ANNO :

    /* TEKAL ASCII Annotation files (text at specified location)          */

         ODSGetLocTekAnno (
            fname , itype, locdef, *maxdef, *pardep,
                          *timdep, loclst, loctyp, locnr, *maxlst,
            nrlst, ierror, option ) ;

         break ;

      case ODS_ANY_JSPOST :

    /* JSPost files                                               */
    ODSGetLocJSP( fname, locdef, *maxdef, loclst, loctyp, *maxlst,
             nrlst, locnr, ierror, option ) ;
    break ;

      case ODS_GEOGR_BNA:
      case ODS_GEOGR_DXF:
      case ODS_GEOGR_TEKAL:

         /* BNA, DXF and TEKAL landboundary files                     */
         ODS_BNA_getloc( fname,  itype,  locdef, maxdef, pardep, timdep,
                         maxlst, loclst, locnr,  nrlst,  ierror, option) ;
         break ;

      default :

         /* Filetype not implemented:                                  */

         *ierror = IEUNKN ;
      }
   if ( *ierror == IEPMNY) *ierror = IELMNY ;

/* A patch/hack: make sure that no trailing blanks occur in the names
   Note: unfortunately the current routines work with a string length
   of PARLEN+1, instead of PARLEN.
   Note:
   Avoid strlen(), as this will not work if nulls have not been
   added properly. With the construct below some superfluous work
   is done, but it works.
*/
   for ( i = 0 ; i < *nrlst ; i ++ )
   {
      for ( j = PARLEN-1 ; j >= 0 ; j -- )
      {
         if ( loclst[j+(PARLEN+1)*i] == ' ' ||
              loclst[j+(PARLEN+1)*i] == '\0'   )
         {
            loclst[j+(PARLEN+1)*i] = '\0' ;
         }
         else
         {
            break ;
         }
      }
   }

   return ;
}
