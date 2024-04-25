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
 *  shyfem.c  -  ODS functions for SHYFEM grid files
 *
 *   Arjen Markus
 */

/*
 *  $Author: Markus $
 *  $Date: 11/15/00 3:52p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/shyfem.c,v $
*/
/*
 *
 */

/*   Date:       9 Dec 1994                                           */
/*   Time:       10:55                                                */
/*   Program:    SHYFEM.C                                             */
/*   Version:    1.00                                                 */
/*   Programmer: Arjen Markus                                         */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   Project:     Open Data Structuur                                 */
/*   Module:      GetParameter/GetDimensions                          */
/*   Subroutines: ODSGetDimShyf Get dimensions from SHYFEM grid file  */
/*                ODSGetParShyf Get parameters from SHYFEM grid file  */
/*   Function:                                                        */
/*   Comment:     General version                                     */
/*   Reference:                                                       */
/*   Review:                                                          */
/*                                                                    */
/**********************************************************************/


#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "portable.h"
#include "ods.h"
#include "itrans.h"
#include "nefis.h"
#include "opnclose.h"
#include "equal.h"
#include "shyfem.h"

#ifndef FALSE
#    define FALSE   0
#    define TRUE    1
#endif

/* Nice patch for MS-Windows DLL's ! */
#ifdef MSWINDOWS
#   define sprintf wsprintf
     _far __cdecl wsprintf(char _far* lpszOut, const char _far* lpszFmt, ...);
#endif

/*************************************************************************/
/*    SUBROUTINE Get Dimension from SHYFEM Grid file                     */
/*************************************************************************/

TVoid ODSGetDimShyf( TString fname,
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
   TInt4 dummy , indx[10] , indloc[9] ;
   int len_fname ;
   FILE *infile ;

   *ierror = IEOK ;

/* Which dimension?
   The grid does not depend on time, so ...
*/
   if ( equal( dim , "tim" ) )
   {
      ndim[0] = 1 ;
      ndim[1] = 0 ;
      return ;
   }

/* The grid consists of two parameters that can be read via GETMAT/GETPAR
*/
   if ( equal( dim , "par" ) )
   {
      ndim[0] = 1 ;
      ndim[1] = 2 ;
   }

/* The size of the grid - note: four dimensions (!) are returned
   - the fourth being the size of the administration array
   Note:
   - number of coordinates: number of nodes (NKN)
   - size of the administration array: 3*number of elements (NELTS)
*/
   if ( equal( dim , "loc" ) )
   {
      infile = fopen( fname , "r" ) ;
      fscanf( infile , "%ld %ld %ld %ld" , &ndim[1] , &dummy ,
              &ndim[4] , &dummy ) ;
      ndim[0] = 4 ;
      ndim[2] = 1 ;
      ndim[3] = 1 ;
      ndim[4] = 3 * ndim[4] ;
      fclose( infile ) ;
   }

   return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Parameter list from SHYFEM grid file                */
/*************************************************************************/

TVoid ODSGetParShyf ( TString fname,
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

/* No need to scan the file: we know what parameters it contains, if it
   exists ...
*/
   *ierror = IEOK ;

/* if list too short to hold the two parameters, an error
*/
   if ( maxlst < 2 )
   {
      *ierror = IEPMNY ;
      return ;
   }

/* Put the names in the arrays
*/
   *nrlst = 2 ;
   strcpy( parlst      , "XCOORD" ) ;
   strcpy( parlst + 21 , "YCOORD" ) ;
   strcpy( paruni      , "m" ) ;
   strcpy( paruni + 21 , "m" ) ;
   partyp[0] = ODS_PT_LOC_MNK ;
   partyp[1] = ODS_PT_LOC_MNK ;
   parcod[0] = 1              ;
   parcod[1] = 2              ;

   return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Time list from SHYFEM grid file                     */
/*************************************************************************/

TVoid ODSGetTmeShyf ( TString fname,
                      TInt4   *ftype,
                      TInt4   timdef,
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

/* No need to scan the file: we know what times it contains, if it
   exists ...
*/
   *ierror = IEOK ;

/* The number of times is zero!
*/
   *nrlst = 0 ;

   return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Values (in matrix form) from SHYFEM grid file       */
/*************************************************************************/

TVoid ODSGetMatShyf(  TString fname,
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
/*       loc     -          I   Locations (ignored)                      */
/*       misval  -          I   Missing value                            */
/*       maxdim  -          I   Maximum number of values (should be enough) */
/*       data    -          O   Values returned                          */
/*       tim     3          I   Return which times (ignored)             */
/*                                                                       */
/*************************************************************************/

   TInt4 i , j , dummy , nkn , nel , nlinks ;
   float fdummy , xval , yval ;
   FILE *infile ;

/* File was closed, so reopen it
*/
   infile = fopen( fname , "r" ) ;

/* Read the header - just dummies
*/
   fscanf( infile , "%ld %ld %ld %ld" , &nkn , &dummy , &nel , &dummy ) ;

/* Skip the element adminstration
*/
   for ( i = 0 ; i < nel ; i ++ )
   {
      fscanf( infile , "%ld %ld %ld" , &dummy , &dummy , &dummy ) ;
   }

/* Skip some rubbish - links between nodes
*/
   for ( i = 0 ; i < nkn ; i ++ )
   {
      fscanf( infile , "%ld" , &nlinks ) ;
      for ( j = 0 ; j <= nlinks ; j ++ )
      {
         fscanf( infile , "%ld" , &dummy ) ;
      }
   }

/* Now, finally the data we want!
*/
   for ( i = 0 ; i < nkn ; i ++ )
   {
      fscanf( infile , "%ld %f %f %f %f" , &j , &xval ,
          &yval , &fdummy , &fdummy ) ;
      if ( parcod == 1 )
      {
         data[j-1] = xval ;
      }
      else
      {
         data[j-1] = yval ;
      }
   }

/* Close the file
*/
   fclose( infile ) ;
   return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Administration array from SHYFEM grid file          */
/*************************************************************************/

TVoid ODSGetGrdShyf(  TString fname,
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

   TInt4 i , j , dummy , nkn , nel , nlinks ;
   float fdummy , xval , yval ;
   FILE *infile ;

/* File was closed, so reopen it
*/
   infile = fopen( fname , "r" ) ;

/* Read the header - just dummies
*/
   fscanf( infile , "%ld %ld %ld %ld" , &nkn , &dummy , &nel , &dummy ) ;

/* Skip the element adminstration
*/
   j = 0 ;
   for ( i = 0 ; i < nel ; i ++ )
   {
      fscanf( infile , "%ld %ld %ld" , &indx[j] ,  &indx[j+1] ,  &indx[j+2] ) ;
      j += 3 ;
   }

/* We have read everything, so ...
*/
   fclose( infile ) ;
   return ;
}
