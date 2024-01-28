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
 *  samples.c  -  ODS: Samples files and simple tables
 *
 *  Arjen Markus
 *
 *  Note:
 *  These files have a very simple structure:
 *  - the lines should not be longer than 1024 columns
 *  - a line with a * or a # in the first column is comment, and
 *    therefore ignored
 *  - a line with words enclosed in " defines the names of the columns
 *    (only one such line is allowed). It need not be present though.
 *  - if a line contains a single number, it will be regarded as
 *    a scale factor for  the third parameter (if the type is
 *    ODS_SAMPLES_2D. Any number of such lines may be present,
 *    but only the last one is used. The scale factor is ONLY
 *    applied to the third parameter (the first one visible to the user)
 *    Note:
 *    This option has been included for SHIPMA only.
 *  - any other line is supposed to contain the same number of values
 *  - the first or the first two columns are used as x- and
 *    x,y-coordinates or as date and time, depending on the file type.
 *    The first or the first two columns have fixed names:
 *    "XCOORD", "YCOORD", "DATE" and "TIME"
 *  - if the type is chosen as ODS_SAMPLES_TIME, then the date
 *    and time are given in the format "yyyymmd hhmmss" or as
 *    "yyyy/mm/dd hh:mm:ss". Note the spaces between the date and time
 *  - there is currently no time information for the ODS_SAMPLES_2D
 *    type
 *  - the values may be separated by spaces, tabs and commas, but
 *    if a missing value is intended, a dot (.) or a question mark (?)
 *    should be used.
 *
 *  Example:
 *  * ----------------
 *  * Example of a "samples" file
 *  * ----------------
 *   "X"     "Y"    "salinity"  "temperature"
 *   10000.  20000.   30.0          12.0
 *   11000.  21000.   31.0          12.3
 *   15000.  21200.   29.0            ?
 *    5043.  11200.   29.1          11.32
 *   ...
 *
 *   Or (ODS_SAMPLES_TIME, two possible date/time formats):
 *
 *   "DATE"      "TIME"    "salinity"  "temperature"
 *   19990101    200000     30.0          12.0
 *   20010201    210000     31.0          12.3
 *   1991/03/02  21:20:00   29.0            ?
 *   1998/02/01  11:12:11   29.1          11.32
 *   ...
 *
 *
 */

/*
 *  $Author: Markus $
 *  $Date: 28-06-04 9:19 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/samples.c,v $
 *
 */

#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include "portable.h"
#include "ods.h"
#include "odsmodel.h"

static TInt4 SamplesGetData( TString buffer, TInt4 ftype, TInt4 idx,
                             TReal8  *value ) ;

#define MAX_LINE_LENGTH 1024

/*----------------------------------------------------------------------
 * Read a line from a samples file
 *----------------------------------------------------------------------
 */
static void
SamplesGetLine( FILE *infile , TInt4 ftype, TString buffer , TInt4 *err )
{
   TChar   buff2[10] ;
   TString pchr      ;

/* Get the line
*/
   *err = 0 ;
   fgets( buffer , MAX_LINE_LENGTH , infile ) ;

/* Check for error conditions
*/
   if ( feof( infile ) || ferror( infile ) )
   {
      *err = 1 ;
      return   ;
   }

/* Check for end-of-line
*/
   pchr = strchr( buffer , '\n' ) ;
   if ( pchr == NULL )
   {
      while ( *err != 0 )
      {
          fgets( buff2 , 10 , infile ) ;

          if ( feof( infile ) || ferror( infile ) )
          {
             *err = 1 ;
          }
          if ( strchr( buff2 , '\n' ) != NULL )
          {
             break ;
          }
      }
   }
   else
   {
      *pchr = '\0' ;
   }

   return ;
}

/*----------------------------------------------------------------------
 * Read the header from a samples file
 * The header may consist of:
 * - a number of comment lines
 * - a number of lines with a single number
 * - a line with the names of the columns
 *
 * The routine returns the scale factor and the line of column names,
 * if present. Defaults:
 * - scale factor = 1.0
 * - line of column names: empty
 *----------------------------------------------------------------------
 */
static void
SamplesGetHeader(
   FILE *infile,  TInt4 ftype, TString buffer,
   TReal4 *scale, TInt4 *error )
{
   TChar   buff2[10] ;
   TString pchr      ;
   TReal8  value     ;
   TChar   names[MAX_LINE_LENGTH] ;
   TInt4   err       ;
   long    pos       ;

   names[0] = '\0' ;
   *scale   = 1.0  ;

/* Skip the comment lines (may hold time information some day)
   and analyse the lines that may hold relevant information
   Also: the header may be that of a TEKAL file. So ignore that
   information (if present)
*/
   err =  0 ;
   pos = -1 ;
   while ( err == 0 )
   {
      SamplesGetLine( infile, ftype, buffer, &err ) ;
      pos ++ ;
      if ( buffer[0] != '*' && buffer[0] != '#' )
      {
         /* For TEKAL files, the first character on the line must be a
            letter - the block name. If so, skip the next line as well.
         */
         if ( isalpha( buffer[0] ) )
         {
            SamplesGetLine( infile, ftype, buffer, &err ) ;
            pos ++ ;
            continue ;
         }

         /* A line with column names (detect first!)
            - this is not fool-proof, though
         */
         if ( strchr( buffer, '"' ) != NULL )
         {
            strcpy( names, buffer ) ;
            continue ;
         }

         /* The lines with scale factors
         */
         if ( SamplesGetData( buffer, ftype, -1, &value ) == 1 )
         {
            (void) SamplesGetData( buffer, ftype, 0, &value ) ;
            *scale = (TReal4) value ;
         }
         else
         {
            /* Probably a line with values - break from this loop!
            */
            break ;
         }
      }
   }

/* The names of the columns
*/
   strcpy( buffer, names ) ;

/* Position the file pointer to the first line containing proper data
   (Note: we can not use ftell() for this, because on Windows ftell()
   returns the wrong value for UNIX-like text files!)
*/
   fseek( infile, 0, SEEK_SET ) ;
   while ( pos > 0 )
   {
      SamplesGetLine( infile, ftype, buffer, &err ) ;
      pos -- ;
   }
   *error = err ;
   return ;
}


/*----------------------------------------------------------------------
 * Get the n-th value from a line
 * Returns 0 upon success, otherwise 1 to indicate an error.
 * If the value is "." or "?", the value is not set.
 * If the index idx is -1, return the number of values on the line
 *----------------------------------------------------------------------
 */
static TInt4
SamplesGetData( TString buffer, TInt4 ftype, TInt4 idx, TReal8 *value )
{
   TString ptoken     ;
   TInt4   i , number ;
   TInt4   jjjjmmdd   ;
   TInt4   hhmmss     ;
   TInt4   year       ;
   TInt4   month      ;
   TInt4   day        ;
   TInt4   hour       ;
   TInt4   min        ;
   TInt4   sec        ;

/* Let strtok() do the work
*/
   ptoken = strtok( buffer , " ,\t" ) ;

   if ( ptoken == NULL )
   {
      return 1 ; /* Empty line? */
   }

   if ( idx != -1 )
   {
      if ( ftype == ODS_SAMPLES_TIME && idx == 0 )
      {
         if ( strchr( ptoken, '/' ) != NULL )
         {
            sscanf( ptoken, "%d%*c%d%*c%d", &year, &month, &day ) ;
            ptoken = strtok( NULL , " ,\t" ) ;
            sscanf( ptoken, "%d%*c%d%*c%d", &hour, &min, &sec ) ;
         }
         else
         {
            sscanf( ptoken, "%d", &jjjjmmdd ) ;
            ptoken = strtok( NULL , " ,\t"  ) ;
            sscanf( ptoken, "%d", &hhmmss   ) ;
            year  =  jjjjmmdd                           / 10000 ;
            month = (jjjjmmdd - year*10000            ) /   100 ;
            day   = (jjjjmmdd - year*10000 - month*100)         ;
            hour  =  hhmmss                             / 10000 ;
            min   = (hhmmss   - hour*10000            ) /   100 ;
            sec   = (hhmmss   - hour*10000 - min*100  )         ;
         }
         julian( &year, &month, &day, &hour, &min, &sec, value );
         return 0 ;
      }

      /* First value has already been found. Now the next */
      for ( i = 1 ; i <= idx ; i ++ )
      {
         ptoken = strtok( NULL , " ,\t" ) ;
      }

      /* Check if it is a missing value */
      if ( ptoken == NULL )
      {
         return 0 ;
      }
      if ( ptoken[0] == '.' && strchr( ". ,\t" , ptoken[1] ) != NULL )
      {
         return 0 ;
      }
      if ( ptoken[0] == '?' )
      {
         return 0 ;
      }

      /* It seems a valid value
      */
      *value = (TReal4) atof( ptoken ) ;
   }
   else
   {
      number = 0 ;
      while ( ptoken != NULL )
      {
         ptoken = strtok( NULL , " ,\t" ) ;
         number ++ ;
      }
      return number ;
   }

   return 0 ;
}

/*----------------------------------------------------------------------
 * Open Samples file (skips comments in the process)
 *----------------------------------------------------------------------
 */
static FILE *
SamplesOpenFile ( TString fname   , TInt4 ftype   , TReal8 *t0 ,
                  TReal4  *scale  , TInt4 *ierror              )
{
   TChar  buffer[MAX_LINE_LENGTH] ;
   FILE  *infile                  ;
   TInt4  pos                     ;
   TInt4  err                     ;
   TInt4  iyear0                  ;
   TInt4  imonth0                 ;
   TInt4  iday0                   ;
   TInt4  ihour0                  ;
   TInt4  imin0                   ;
   TInt4  isec0                   ;

   *ierror = IEOK;

   if ( ftype != ODS_SAMPLES_2D   && ftype != ODS_SAMPLES_TABLE &&
        ftype != ODS_SAMPLES_TIME )
   {
      *ierror = IETYPE ;
      return NULL      ;
   }

/* Open the file and read the header
*/
   infile = fopen( fname , "r" ) ;
   if ( infile == NULL )
   {
      *ierror = IENOFI ;
      return NULL      ;
   }

   SamplesGetHeader( infile, ftype, buffer, scale, &err ) ;
   if ( err != 0 )
   {
      *ierror = IEUNKN ;
      return NULL      ;
   }

/* Now calculate the julian date and time: 1 jan 1900
*/
   iyear0  = 1900 ;
   imonth0 =    1 ;
   iday0   =    1 ;
   ihour0  =    0 ;
   imin0   =    0 ;
   isec0   =    0 ;
   julian( &iyear0 , &imonth0 , &iday0 , &ihour0 , &imin0 , &isec0 ,
           t0 );

/* Return the file pointer - if no error occurred
*/
   if ( err == 0 )
   {
      return infile ;
   }
   else
   {
      fclose( infile ) ;
      *ierror = IEUEOF ;
      return NULL      ;
   }
}

/*---------------------------------------------------------------------
 * Function to get a list of times from a samples file
 *---------------------------------------------------------------------
 */
#ifndef NOPROT
   void SamplesGetTme ( TString fname,  TInt4 *ftype,
                        TReal8 *timdef, TInt4 *maxdef, TInt4 *pardep,
                        TInt4  *locdep, TInt4 *maxlst, TReal8 *timlst,
                        TInt4  *timtyp, TInt4 *nrtim,  TInt4 *ierror,
                        TString option)
#else
   void SamplesGetTme ( fname,     ftype,      timdef,     maxdef,
                        timlst,    maxlst,     nrtim,      ierror,
                        option)

   TString fname, option;
   TInt4   *maxdef, *maxlst, *nrtim, *ierror, *ftype;
   TReal8  *timdef, *timlst;
#endif

/*************************************************************************/
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   3          I   Full filename, including extension.      */
/*       ftype   -          I   Filetype. See ods.h for definition.      */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       maxdef  -          I   Nr. of time intervals in input.          */
/*       maxlst  -          I   Max. nr. of time intervals in output.    */
/*       nrtim   -          O   Nr. of time intervals in output.         */
/*       option  -         I/O  option (reserved for future extensions)  */
/*       timlst  maxlst     O   Timesteps found.                         */
/*       timdef  maxdef     I   Requested timesteps.                     */
/*                                                                       */
/*************************************************************************/

{
   FILE   *infile  ;
   TReal8  t0      ;
   TReal8  juldat  ;
   TInt4   offst   ;
   TInt4   itim    ;
   TReal4  scale   ;
   TChar   buffer[MAX_LINE_LENGTH] ;
   TInt4   err     ;
   TInt4   valid   ;

/* Do some initialisation
*/
   *ierror = IEOK ;

/* Open the file
*/
   infile = SamplesOpenFile( fname , *ftype , &t0 , &scale , ierror) ;

   if ( *ierror != IEOK )
   {
       return ;
   }

   if ( *ftype == ODS_SAMPLES_TIME )
   {
      SamplesGetLine( infile, *ftype, buffer, &err ) ;
      if ( strchr( buffer, '"' ) != NULL )
      {
         SamplesGetLine( infile, *ftype, buffer, &err ) ;
      }

      offst = 0 ;
      for ( itim = 0 ; itim < *maxlst ; itim ++ )
      {
         if ( err != 0 )
         {
            break ;
         }
         timlst[offst] = t0 ;
         if ( buffer[0] != '*' && buffer[0] != '#' )
         {
            valid = SamplesGetData( buffer, *ftype, 0, &juldat ) ;

            /* Check if all steps are wanted */
            if ( timdef[0] <= 0.0 && timdef[1] >= ALLSTEPS )
            {
               /* Just copy step to outlist */
               timlst[offst] = juldat;
            }
            else
            {
               /* Compare against wanted list
                  HACK: ignore maxdef */
               if ( juldat >= timdef[0] && juldat <= timdef[1] )
               {
                  timlst[offst] = juldat;
               }
            }
         }
         offst ++ ;
         SamplesGetLine( infile, *ftype, buffer, &err ) ;
      }

      *nrtim = offst;
      fclose( infile ) ;
      return ;
   }


/* Return date and time (for the other cases):
   Compare times to wanted list and copy wanted times
   - mainly for future use!
*/
   offst  = 0 ;
   juldat = t0 ;
   for ( itim = 0 ; itim < 1 ; itim ++ )
   {
      /* Check if all steps are wanted */
      if ( timdef[0] <= 0.0 && timdef[1] >= ALLSTEPS )
      {
         /* Just copy step to outlist */
         timlst[offst] = juldat;
      }
      else
      {
         /* Compare against wanted list
            HACK: ignore maxdef */
         if ( juldat >= timdef[0] && juldat <= timdef[1] )
         {
            timlst[offst] = juldat;
         }
      }
      offst ++ ;
      if ( offst >= *maxlst )
      {
         break ;
      }
   }
   *nrtim = offst;

   fclose( infile ) ;
   return;
}

/*--------------------------------------------------------------------
* Function to get a matrix from a samples file
*---------------------------------------------------------------------
*/

#ifndef NOPROT
   void SamplesGetMat ( TString fname,   TInt4  *ftype,  TInt4  *parcod,
                        TInt4   *loc,    TReal8 *tim,    TReal4 *misval,
                        TInt4   *i3gl,   TInt4  *maxdim, TReal4 *values,
                        TInt4   *ierror, TString option)
#else
   void SamplesGetMat ( fname,     ftype,      parcod,   tim,
                        loc,       misval,     maxdim,   values,
                        ierror,    option)

   TString fname, option;
   TInt4 *ftype, *parcod, *loc, *maxdim, *ierror;
   TReal8 *tim;
   TReal4 *misval, *values;
#endif

/*************************************************************************/
/*     Arguments:                                                        */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   -          I   Full filename, including extension       */
/*       ftype   -          I   Filetype. See ods.h for definition.      */
/*       ierror  -          O   Errorcode, see ods.h for definition.     */
/*       loc     -          I   List of indices of locations wanted.     */
/*       maxdim  -          I   dimensions of data array.                */
/*       misval  -          I   Real to use if no data found.            */
/*       option  -         I/O  option (reserved for future extensions). */
/*       parcod  -          I   Parameter identifying data wanted.       */
/*       tim     -          I   List of times (Julian dates).            */
/*       values  maxdim     O   Retrieved data.                          */
/*************************************************************************/

{
   FILE   *infile        ;
   TReal8  t0            ;
   TReal8  value         ;
   TReal4 *fdata         ;
   TReal4  scale         ;
   TInt4   valid         ;
   TInt4   offst         ;
   TInt4   idx           ;
   TInt4   iloc          ;
   TInt4   err           ;
   TInt4   start_loc     ;
   TInt4   end_loc       ;
   TInt4   step_loc      ;
   TChar   buffer[MAX_LINE_LENGTH] ;

/* Do some initialisation
*/
   *ierror = IEOK;

   infile = SamplesOpenFile( fname , *ftype , &t0 , &scale , ierror ) ;
   if ( *ierror != IEOK )
   {
       return;
   }

/* Set output array elements to misval
*/
   for (fdata = values; fdata < (values + *maxdim); fdata++)
   {
        *fdata = *misval;
   }

/* The dataset is always a simple series of data. The time can be ignored
   - at the moment: there is only one timestep
*/
   start_loc  = loc[0] ;
   end_loc    = loc[1] ;
   step_loc   = loc[2] ; /* Assumed 1! */

   if ( *ftype == ODS_SAMPLES_TIME )
   {
      step_loc = 0 ;
   }

/* Current timestep matches, get the data (for future use)
   Note:
   The first line that is being read may contain the names of the
   parameters. If so, it contains double quotes
   Note:
   The procedure assumes there is no comment on lines containing data
*/
   idx   = *parcod ;
   offst = 0       ;

   SamplesGetLine( infile, *ftype, buffer, &err ) ;

   if ( strchr( buffer , '"' ) != NULL )
   {
      SamplesGetLine( infile, *ftype, buffer, &err ) ;
   }

   for ( iloc  = start_loc ; (iloc <= end_loc) && (offst < *maxdim) ;
         iloc += step_loc  )
   {
      if ( err != 0 )
      {
         break ;
      }

      /* Analyse the line only if it does not start with a "*" or "#"
      */
      while ( buffer[0] == '*' || buffer[0] == '#' )
      {
         SamplesGetLine( infile, *ftype, buffer, &err ) ;
      }

      valid = SamplesGetData( buffer, *ftype, idx, &value ) ;
      values[offst] = (TReal4) value ;

      /* Apply the scaling factor (for the third column only)
      */
      if ( idx == 2 && *ftype == ODS_SAMPLES_2D )
      {
         values[offst] *= scale ;
      }

      /* Read the next line
      */
      SamplesGetLine( infile, *ftype, buffer, &err ) ;

      offst ++ ;
   }

   fclose( infile ) ;

   return;
}

/*--------------------------------------------------------------------
* Function to get a list of locations from a samples file
*---------------------------------------------------------------------
*/
#ifndef NOPROT
   void SamplesGetLoc ( TString fname,  TInt4 *ftype,    TString locdef,
                        TInt4 *maxdef,  TInt4 *pardep,   TInt4  *timdep,
                        TInt4 *maxlst,  TString loclst,  TInt4  *loctyp,
                        TInt4 *locnr,   TInt4 *nrlst,    TInt4  *ierror,
                        TString option)
#else
   void SamplesGetLoc ( fname,     ftype,      locdef,      maxdef,
                        loclst,    maxlst,     nrlst,       locnr,
                        ierror,    option)

   TString fname,  locdef, loclst, option;
   TInt4   *ftype,  *maxdef, *maxlst, *nrlst, *locnr,  *ierror;
#endif

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    Size      I/O  Description                             */
/*        ------  --------  ---  ------------------------------------    */
/*       fname   -          I   Full filename, including extension.      */
/*       ftype   -          I   Filetype. See ods.h for definition.      */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       locdef  maxdef     I   List of locations wanted.                */
/*       loclst  maxlst     O   List of locations found.                 */
/*       locnr   maxlst     O   List of index numbers of locations found */
/*       maxdef  -          I   Max. nr of locations wanted.             */
/*       maxlst  -          I   Max. nr of locations to return.          */
/*       nrlst   -          O   Nr of locations returned.                */
/*       option  -         I/O  option (reserved for future extensions)  */
/*                                                                       */
/*************************************************************************/

{
   FILE   *infile  ;
   TReal8  t0      ;
   TReal4  scale   ;

   *ierror = IEOK;

   infile = SamplesOpenFile( fname , *ftype , &t0 , &scale , ierror ) ;
   if ( *ierror != IEOK )
   {
       return;
   }

/* Get location names: one or none!
*/
   *nrlst = 0 ;

   if ( *ftype == ODS_SAMPLES_TIME )
   {
      *nrlst = 1 ;
      strcpy( loclst, "Single table" ) ;
      locnr[0] = 0 ;
   }

   fclose( infile ) ;
   return ;
}

/*--------------------------------------------------------------------
* Function to get a list of parameters from a samples file
*---------------------------------------------------------------------
*/
#ifndef NOPROT
   void SamplesGetPar ( TString fname,   TInt4 *ftype,    TString pardef,
                        TInt4   *maxdef, TInt4 *timdep,   TInt4   *locdep,
                        TInt4   *maxlst, TInt4 *lang,     TString parlst,
                        TString paruni,  TInt4 *partyp,   TInt4   *parcod,
                        TInt4   *nrlst,  TInt4 *ierror,   TString option)
#else
   void SamplesGetPar ( fname,     ftype,      pardef,
                        maxdef,    timedep,    locdep,
                        maxlst,    lang,       parlst,
                        paruni,    partyp,     parcod,
                        nrlst,     ierror,     option)

   TString fname, pardef, parlst, paruni, option;
   TInt4 *ftype, *maxdef, *maxlst, *partyp, *parcod, *nrlst, *ierror;
#endif

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*        Name    Size      I/O  Description                             */
/*        ------  --------  ---  ------------------------------------    */
/*       fname   -          I   Full filename, including extension       */
/*       ftype   -          I   Filetype. See ods.h for definition.      */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*       option  -         I/O  option (reserved for future extensions)  */
/*       pardef  maxdef     I   List of parameters wanted.               */
/*       parlst  maxlst     O   List of parameters found.                */
/*       partyp  maxlst     O   List of types of parameters found.       */
/*       paruni  maxlst     O   List of units of parameters found.       */
/*       parcod  maxlst     O   List of index nrs of parameters found.   */
/*                                                                       */
/*************************************************************************/
{
   FILE    *infile                  ;
   TReal8   t0                      ;
   TReal4   scale                   ;
   TChar    buffer[MAX_LINE_LENGTH] ;
   TInt4    err                     ;
   TInt4    par                     ;
   TInt4    par2                    ;
   TInt4    parskip                 ;
   TString  pstr                    ;
   TString  pchr                    ;
   TString  pchr2                   ;

   *ierror = IEOK;

   infile = SamplesOpenFile( fname , *ftype , &t0 , &scale , ierror ) ;
   if ( *ierror != IEOK )
   {
       return;
   }

/* First set the default names and so on
*/
   for ( par = 0 ; par < *maxlst; par ++ )
   {
      if ( *ftype == ODS_SAMPLES_TIME )
      {
         parcod[par] = par + 2                          ;
         partyp[par] = ODS_PT_LOC_DEP                   ;
      }
      else
      {
         parcod[par] = par                              ;
         partyp[par] = ODS_PT_TIME_DEP | ODS_PT_LOC_MNK ;
      }

      par2 = par ;
      if ( *ftype == ODS_SAMPLES_2D    ||
           *ftype == ODS_SAMPLES_TIME     ) par2 = par - 1 ;

      sprintf( parlst+par*(PARLEN+1) , "Parameter %d" , (int) par2 ) ;
      strcpy(  paruni+par*(PARLEN+1) , "[-]"                       ) ;
   }

/* If the file contains a line of names, then it should be the first
   line not a comment line

/* Get the line that might contain the names - via the header
*/
   fseek( infile, 0L, SEEK_SET ) ;
   SamplesGetHeader( infile, *ftype, buffer, &scale, &err ) ;

/* Look for quotation marks
*/
   par     = 0          ;
   parskip = 0          ;
   if ( *ftype == ODS_SAMPLES_TIME )
   {
      parskip = 2 ;
   }
   pstr  = &buffer[0] ;
   pchr  = &buffer[0] ;
   pchr2 = NULL       ;
   while ( pchr != NULL )
   {
      pchr = strchr( pstr , '"' ) ;
      if ( pchr == NULL )
      {
         continue ; /* Past the last " */
      }
      pstr  = pchr + 1             ;
      pchr2 = strchr( pstr , '"' ) ;
      if ( pchr2 != NULL )
      {
         *pchr2 = '\0' ;
      }
      if ( parskip > 0 )
      {
         parskip -- ;
      }
      else
      {
         strcpy( parlst + par * ( PARLEN + 1 ) , pstr ) ;
         par ++ ;
      }
      pstr = pstr + strlen( pstr ) + 1 ;
   }

/* Anyway, the first one or two names are fixed
*/
   if ( *ftype != ODS_SAMPLES_TIME )
   {
      parcod[0] = 0              ;
      partyp[0] = ODS_PT_LOC_DEP ;
      strcpy( parlst          , "XCOORD" ) ;
      strcpy( paruni          , "[m]"    ) ;
      if ( *ftype == ODS_SAMPLES_2D )
      {
         par       = 1 ;
         parcod[1] = 1 ;
         strcpy( parlst+PARLEN+1 , "YCOORD" ) ;
         strcpy( paruni+PARLEN+1 , "[m]"    ) ;
         partyp[1] = ODS_PT_LOC_DEP ;
      }
   }

/* The number of parameters
*/
   *nrlst = *maxlst ;

   fclose( infile ) ;

   return;
}

/*--------------------------------------------------------------------
* Function to get the dimensions from a samples file
*---------------------------------------------------------------------
*/

#ifndef NOPROT
   void SamplesGetDim ( TString fname,   TInt4 *ftype,   TString dim,
                        TInt4 *pardep,  TInt4 *timdep,  TInt4 *locdep,
                        TInt4 *ndim,    TInt4 *ierror,  TString option)
#else
   void SamplesGetDim ( fname,      ftype,   dim,     pardep,   timdep,
                        locdep,     ndim,    ierror,  option)

   TString fname, dim, option;
   TInt4 *ftype, *pardep, *timdep, *locdep, *ndim, *ierror;
#endif

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       dim     -          I   Dimension required.                      */
/*       fname   3          I   Full filename, including extension.      */
/*       ftype   -          I   Filetype. See ods.h for definition.      */
/*       ierror  -          O   Errorcode, see ods.h for definition.     */
/*       locdep  -          I   location dependency of dimension.        */
/*       ndim    -          O   dimension.                               */
/*       option  -         I/O  option (reserved for future extensions). */
/*       pardep  -          I   parameter dependency of dimension.       */
/*       timdep  -          I   time dependency of dimension.            */
/*                                                                       */
/*************************************************************************/

{
   FILE   *infile                 ;
   TReal8 t0                      ;
   TReal8 value                   ;
   TReal4 scale                   ;
   TInt4  err                     ;
   TInt4  idx                     ;
   TChar  buffer[MAX_LINE_LENGTH] ;

/* Initialisation
*/
   *ierror = IEOK;

   infile = SamplesOpenFile( fname , *ftype , &t0 , &scale , ierror ) ;
   if ( *ierror != IEOK )
   {
      return;
   }

/* Get the various dimensions
*/
   switch (dim[0])
   {

      case 'p':  /* par */
      case 'P':
         ndim[0] = 1 ;
         ndim[1] = 0 ;
         /* Number of parameters is number of values */
         SamplesGetLine( infile, *ftype, buffer, &err   ) ;
         SamplesGetLine( infile, *ftype, buffer, &err   ) ;
         idx     = -1 ; /* Trick: number of data returned */
         ndim[1] = SamplesGetData( buffer, *ftype, idx, &value ) ;
         if ( *ftype == ODS_SAMPLES_TIME )
         {
            ndim[1] = ndim[1] - 2 ;
         }
         break;

      case 't':  /* tim */
      case 'T':
      case 'l':  /* loc */
      case 'L':
         if ( *ftype != ODS_SAMPLES_TIME &&
              (dim[0] == 't' || dim[0] == 'T') )
         {
            ndim[0] = 1L ;
            ndim[1] = 1L ; /* At the moment: only one timestep! */
            return ;
         }
         if ( *ftype == ODS_SAMPLES_TIME &&
              (dim[0] == 'l' || dim[0] == 'L') )
         {
            ndim[0] = 1L ;
            ndim[1] = 1L ; /* At the moment: only one location! */
            return ;
         }

         /* The remaining two cases
         */
         ndim[0] = 1 ;
         ndim[1] = 0 ;

         /* Number of times/locations is almost number of lines */
         SamplesGetLine( infile, *ftype, buffer, &err ) ;
         if ( strchr( buffer , '"' ) == NULL )
         {
            ndim[1] = 1 ;
         }
         while ( err == 0 )
         {
            SamplesGetLine( infile, *ftype, buffer, &err ) ;
            if ( err != 0 )
            {
               continue ;
            }

            /* Accept the line only if it does not start with a "*" or "#"
            */
            if ( buffer[0] != '*' && buffer[0] != '#' )
            {
               ndim[1] ++ ;
            }
         }

         break;

      default:
         *ierror = IEOTHR;
         break;
   } /* switch dim */

   fclose( infile ) ;

   return;
}

/*--------------------------------------------------------------------
* Function to get the grid from a samples file (quasi grid!)
*---------------------------------------------------------------------
*/

void SamplesGetGrd ( TString   fname,  TInt4 * ftype,   TInt4 * indloc,
                     TInt4   * indx,   TInt4 * nocell,  TInt4 * igisty,
                     TInt4   * ierror )

/*************************************************************************/
/*                                                                       */
/*     Arguments:                                                        */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   3          I   Full filename, including extension.      */
/*       ftype   -          I   Filetype. See ods.h for definition.      */
/*       ierror  -          O   Errorcode, see ods.h for definition.     */
/*       indloc  -          I   location dependency (ignored)            */
/*       indx    -          O   administration array                     */
/*       igisty  -          O   type of grid                             */
/*       nocell  -          O   number of data                           */
/*                                                                       */
/*************************************************************************/

{
   TInt4  locdep                  ;
   TInt4  timdep                  ;
   TInt4  pardep                  ;
   TInt4  i                       ;
   TChar *option                  ;
   TChar  dim[4]                  ;
   TInt4  ndim[4]                 ;

/* Initialisation
*/
   *ierror = IEOK;
   *igisty = IGCURV ; /* For lack of something better */

/* Get the number of data
*/
   strcpy( dim, "LOC" ) ;
   locdep = 0 ;
   timdep = 0 ;
   pardep = 0 ;
   SamplesGetDim ( fname,  ftype,  dim,    &pardep,  &timdep,
                   &locdep, ndim, ierror, option) ;

   *nocell = ndim[1] ;
   for ( i = 0 ; i < *nocell ; i ++ )
   {
      indx[i] = i + 1 ;
   }

   return;
}
