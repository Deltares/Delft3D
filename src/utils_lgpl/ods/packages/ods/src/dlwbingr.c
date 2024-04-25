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
 *  dlwbingr.c  -  ODS functions for standard Delwaq/Delpar grid files
 *                 In addition: it will read TELEMAC grid files that
 *                 used by the TELEMAC-DELWAQ coupling.
 *
 *   Arjen Markus
 */

/*
 *  $Author: Markus $
 *  $Date: 6/09/06 16:13 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/dlwbingr.c,v $
*/
/*
 *
 */

/*   Date:       15 Nov 1994                                          */
/*   Time:       11:30                                                */
/*   Program:    DLWBINGR.C                                           */
/*   Version:    1.00                                                 */
/*   Programmer: Arjen Markus                                         */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   Project:     Open Data Structuur                                 */
/*   Module:      GetParameter/GetDimensions                          */
/*   Subroutines: ODSGetDimDlwg Get dimensions from Delwaq grid file  */
/*                ODSGetParDlwg Get parameters from Delwaq grid file  */
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
#include "dlwbin.h"
#include "dlwgrid.h"

#ifndef FALSE
#    define FALSE   0
#    define TRUE    1
#endif

#define MAXIBU 512


/* Nice patch for MS-Windows DLL's ! */
#ifdef MSWINDOWS
#   define sprintf wsprintf
     _far __cdecl wsprintf(char _far* lpszOut, const char _far* lpszFmt, ...);
#endif

/*************************************************************************/
/*    SUBROUTINE Get Dimension from DeLWaq Grid file                     */
/*************************************************************************/

TVoid ODSGetDimDlwg( TString fname,
                     TInt4   *ftype,
                     TString dim,
                     TInt4   pardep,
                     TInt4   timdep,
                     TInt4   locdep,
                     TInt4   *ndim,
                     TInt4   *ierror,
                     char    *option)
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
   TInt4    dummy , indx[10] , indloc[9] ;

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
   Make sure the routine GETLGA will return the dimensions only (set indloc[0]
   to -1
*/
   if ( equal( dim , "loc" ) )
   {
      indloc[0] = -1 ;

      if ( *ftype == ODS_DELWAQ_GRID_UNF )
      {
         ODS_DELWAQ_UNF_lgrid( fname , 256 , 3 , *ftype , indloc , indx ,
                               &dummy , &dummy , (TInt4 *) ierror ) ;
      }
      if ( *ftype == ODS_DELWAQ_TELEMAC )
      {
         ODS_Delwaq_Telemac_Grid( fname, *ftype, indloc, indx, &dummy, &dummy, ierror ) ;
      }
      ndim[0] = 4 ;
      ndim[1] = indx[0] ;
      ndim[2] = indx[1] ;
      ndim[3] = indx[2] ;
      ndim[4] = indx[3] ;
   }

   return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Parameter list from DELWAQ/DELPAR grid file         */
/*************************************************************************/

TVoid ODSGetParDlwg ( TString fname,
                      TInt4   *ftype,
                      TString pardef,
                      TInt4   maxdef,
                      TString parlst,
                      TString paruni,
                      TInt4   maxlst,
                      TInt4   *nrlst,
                      TInt4   *partyp,
                      TInt4   *parcod,
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
/*    SUBROUTINE Get Time list from DELWAQ/DELPAR grid file              */
/*************************************************************************/

TVoid ODSGetTmeDlwg ( TString fname,
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

/* Following functions taken from "gridread.c" in WAQ-GUI
*/

/* @@------------------------------------------
    Function: ReadInteger()
    Author:   Arjen Markus
    Purpose:  Read a (big-endian) integer
    Context:  Used by routines here
    Pseudo Code:
              Read four bytes. Turn the bytes
              around. Return the integer value
 ------------------------------------------------*/
static TInt4 ReadInteger(
                FILE *infile )  /* Input file */
                                /* Returns integer */
{
   char buffer[4] ;
   union _v {
      char buffer2[4] ;
      int  intvalue   ;
   } v ;
   fread( buffer, 4, sizeof(char), infile ) ;

   /* Big endian file! */
   v.buffer2[0] = buffer[3] ;
   v.buffer2[1] = buffer[2] ;
   v.buffer2[2] = buffer[1] ;
   v.buffer2[3] = buffer[0] ;
   return v.intvalue ;
}

/* @@------------------------------------------
    Function: ReadReal()
    Author:   Arjen Markus
    Purpose:  Read a (big-endian) real
    Context:  Used by routines here
    Pseudo Code:
              Read four bytes. Turn the bytes
              around. Return the integer value
 ------------------------------------------------*/
static TReal4 ReadReal(
                FILE *infile )  /* Input file */
                                /* Returns integer */
{
   char buffer[4] ;
   union _v {
      char   buffer2[4] ;
      TReal4 rvalue     ;
   } v ;
   fread( buffer, 4, sizeof(char), infile ) ;

   /* Big endian file! */
   v.buffer2[0] = buffer[3] ;
   v.buffer2[1] = buffer[2] ;
   v.buffer2[2] = buffer[1] ;
   v.buffer2[3] = buffer[0] ;
   return v.rvalue ;
}

/* @@------------------------------------------
    Function: SkipRecord()
    Author:   Arjen Markus
    Purpose:  Skip a full record
    Context:  Used by WQ_ImportGridFEM()
    Pseudo Code:
              Read the four bytes that constitute
              the record length. Then skip to the
              start of the next record
 ------------------------------------------------*/
static TVoid SkipRecord(
                FILE *infile )  /* Input file */
{
   TInt4    reclen ;

   reclen = ReadInteger(infile) ;
   fseek( infile, reclen+4, SEEK_CUR  ) ;
}

/* @@------------------------------------------
    Function: ReadRecordIntegers()
    Author:   Arjen Markus
    Purpose:  Read integers from a record
    Context:  Used by WQ_ImportGridFEM()
    Pseudo Code:
              Successively read the bytes,
              skip to the next record when done
 ------------------------------------------------*/
static TVoid ReadRecordIntegers(
                FILE  *infile,   /* Input file */
                TInt4 *array,    /* Array to be filled */
                TInt4  maxnum )  /* Maximum number */
{
   TInt4    i      ;
   TInt4    reclen ;

   reclen = ReadInteger(infile) ;

   for ( i = 0 ; i < reclen/4 && i < maxnum ; i ++ )
   {
      array[i] = ReadInteger(infile) ;
   }

   if ( maxnum < reclen/4 )
   {
      fseek( infile, reclen-4*maxnum, SEEK_CUR ) ;
   }
   fseek( infile, 4, SEEK_CUR ) ;
}

/* @@------------------------------------------
    Function: ReadRecordReals()
    Author:   Arjen Markus
    Purpose:  Read reals from a record
    Context:  Used by WQ_ImportGridFEM()
    Pseudo Code:
              Successively read the bytes,
              skip to the next record when done
 ------------------------------------------------*/
static TVoid ReadRecordReals(
                FILE   *infile,   /* Input file */
                TReal4 *array,    /* Array to be filled */
                TInt4   maxnum )  /* Maximum number */
{
   TInt4    i      ;
   TInt4    reclen ;

   reclen = ReadInteger(infile) ;

   for ( i = 0 ; i < reclen/4 && i < maxnum ; i ++ )
   {
      array[i] = ReadReal(infile) ;
   }

   if ( maxnum < reclen/4 )
   {
      fseek( infile, reclen-4*maxnum, SEEK_CUR ) ;
   }
   fseek( infile, 4, SEEK_CUR ) ;
}

/* @@------------------------------------------
    Function: ReadRecordString()
    Author:   Leo Postma
    Purpose:  Reads a string record
    Context:  Used by WQ_ImportGridFEM()
    Pseudo Code: just read the string and cut the Fortrans
 ------------------------------------------------*/
static TVoid ReadRecordString(
                FILE   *infile,   /* Input file */
                char*   string,   /* Array to be filled */
                TInt4   maxnum )  /* Maximum number */
{  TInt4    reclen ;

   reclen = ReadInteger(infile) ;

   fread ( string, (int)min(reclen,maxnum), 1, infile ) ;
   if ( reclen <  maxnum ) string[reclen]   = '\0' ;
   if ( reclen == maxnum ) string[reclen-1] = '\0' ;
   if ( reclen >  maxnum )
   {   string[maxnum-1] = '\0' ;
       fseek ( infile, reclen-maxnum, SEEK_CUR ) ;
   }
   fseek( infile, 4, SEEK_CUR ) ;

}

/* Following function derived from "gridread.c" in WAQ-GUI
*/

TVoid
   ODS_Delwaq_Telemac_Grid( TString  fname,
                            TInt4    ftype,
                            TInt4   *indloc,
                            TInt4   *indx,
                            TInt4   *nocell,
                            TInt4   *igisty,
                            TInt4   *ierror )

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       igisty  -          O   Type of topgraphic data                  */
/*       indloc  -         I/O  Indicator for index array dimensions     */
/*       indx    -          I   Index array itself                       */
/*       nocell  -          O   Number of cells                          */
/*                                                                       */
/*************************************************************************/
{
   FILE         *infile                        ;
   TInt4         i                             ;
   TInt4         values[10]                    ;
   TInt4         no_elems                      ;
   TInt4         no_nodes                      ;
   TInt4         no_layers                     ;
   char          title[80]                     ;
   TInt4         nosys                         ;
   TInt4        *indxf                         ;

   *ierror = IEOK ;
   infile = fopen( fname, "rb" ) ;

   if ( infile == NULL )
   {
      *ierror = IENOFI ;
      return ;
   }

   ReadRecordString  ( infile, title , 80 ) ;
   ReadRecordIntegers( infile, values,  2 ) ;
   nosys  = values[0] ;
   for ( i = 0 ; i < nosys ; i ++ )
   {   ReadRecordString ( infile, title, 32 ) ;
   }
   ReadRecordIntegers( infile, values, 10 ) ;
   ReadRecordIntegers( infile, values,  4 ) ;
   no_elems  = values[0] ;
   no_nodes  = values[1] ;
   no_layers = values[6] ;
   if ( no_layers == 0 )
   {
      no_layers = 1 ;
   }

   /* Inquiry about dimensions?
   */
   if ( indloc[0] == -1 )
   {
      indx[0] = no_nodes/no_layers       ;
      indx[1] = 1                        ;
      indx[2] = 1                        ;
      if ( no_layers > 1 )
      {
         indx[3] = 3*no_elems/(no_layers-1) ;
      }
      else
      {
         indx[3] = 3*no_elems ;
      }
      return ;
   }
   else
   {
      /* Get the administration
      */
      *nocell = no_nodes/no_layers ;
      *igisty = IGFEM3P  ;

      if ( no_layers == 1 )
      {
         ReadRecordIntegers( infile, indx, 3*no_elems ) ;
      }
      else
      {
         indxf = (TInt4 *) malloc( 6*no_elems*sizeof(TInt4) ) ;
         ReadRecordIntegers( infile, indxf, 6*no_elems ) ;
         for ( i = 0 ; i < no_elems/(no_layers-1) ; i ++ )
         {
            indx[3*i+0] = indxf[6*i+0] ;
            indx[3*i+1] = indxf[6*i+1] ;
            indx[3*i+2] = indxf[6*i+2] ;
         }
         free( indxf ) ;
      }
   }
   fclose( infile ) ;

   return ;
}

TVoid
   ODS_Delwaq_Telemac_Coords( TString  fname,
                              TInt4    ftype,
                              TInt4    parcod,
                              TInt4   *loc,
                              TReal8  *tim,
                              TReal4   misval,
                              TInt4    i3gl,
                              TInt4    maxdim,
                              TReal4  *data,
                              TInt4   *ierror,
                              TString  option )

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       data    -          O   Array of data                            */
/*       fname   -          I   Full filename, including extension       */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Error code                               */
/*       i3gl    -          I   Programming language type (ignored)      */
/*       loc     -          I   Matrix describing desired locations      */
/*       maxdim  -          I   Maximum number of data to be used        */
/*       misval  -          I   Value reserved for missing data          */
/*       option  -          I   Option - unused                          */
/*       parcod  -          I   Which parameter                          */
/*       tim     -          I   Which time(s)                            */
/*                                                                       */
/*************************************************************************/
{
   FILE         *infile                        ;
   TInt4         i                             ;
   TInt4         values[10]                    ;
   TInt4         no_elems                      ;
   TInt4         no_nodes                      ;
   TInt4         no_layers                     ;
   char          title[80]                     ;
   TInt4         nosys                         ;

   *ierror = IEOK ;
   infile = fopen( fname, "rb" ) ;

   if ( infile == NULL )
   {
      *ierror = IENOFI ;
      return ;
   }

   ReadRecordString  ( infile, title , 80 ) ;
   ReadRecordIntegers( infile, values,  2 ) ;
   nosys  = values[0] ;
   for ( i = 0 ; i < nosys ; i ++ )
   {   ReadRecordString ( infile, title, 32 ) ;
   }
   ReadRecordIntegers( infile, values, 10 ) ;
   ReadRecordIntegers( infile, values,  4 ) ;
   no_elems = values[0] ;
   no_nodes = values[1] ;
   no_layers = values[6] ;
   if ( no_layers == 0 )
   {
      no_layers = 1 ;
   }
   no_nodes = no_nodes / no_layers ;

   /* Skip connectivity table */
   SkipRecord( infile ) ;

   /* Skip open boundary table */
   SkipRecord( infile ) ;

   /* Get the coordinates */
   if ( parcod == 1 )
   {
      ReadRecordReals( infile, data, no_nodes ) ;
   }
   else
   {
      SkipRecord( infile ) ; /* skip x-coordinates */
      ReadRecordReals( infile, data, no_nodes ) ;
   }

   fclose( infile ) ;

   return ;
}
