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
/* @begin@ */
/*
 *  ods_bna.c -  ODS routines to read BNA-, DXF and related TEKAL files
 *
 *  Arjen Markus
 */

/*
 *  General information:
 *  This file contains the following functions:
 *  - ODS_BNA_getdim():    Get the dimensions for the dataset
 *  - ODS_BNA_getpar():    Get the parameters for the dataset
 *  - ODS_BNA_getloc():    Get the locations for the dataset
 *  - ODS_BNA_gettme():    Get the times for the dataset
 *  - ODS_BNA_getmat():    Get the values for the dataset
 *  - ODS_BNA_getgrd():    Get the adminstration array for the dataset
 *  - ODS_BNA_readfile():  Actually read the file
 */

/*
 *  $Author: Markus $
 *  $Date: 11-02-05 10:04 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/ods_bna.c,v $
*/
/*
 */

/* @end@ */

/*
 * Include files and definitions
 */
#include "portable.h"

static void ODS_BNA_readfile( char *fname  ,TInt4  itype   ,TInt4  action ,
                             TInt4 *size   , float *data   ,TInt4 *iadmin ,
                             TInt4 *nocell ,TInt4  *ierror                ) ;

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "ods.h"

/*
 * Macros - none
 */


/* -------------------------------------------------------------------
   Function: ODS_BNA_getdim()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3    I/O  Full filename, including extension
   long    *itype      1    I/O  Filetype, see types.inc for definitions
   char    *dim        1     I   Dimension required.
   long    *pardep     1     I   For which parameter (ignored)
   long    *timdep     1     I   For which time (ignored)
   long    *locdep     1     I   For which location (ignored)
   long    *nodim      5     O   Dimensions returned
   long    *ierror     1     O   Error code
   char    *option     *     I   Dummy argument

--------------------------------------------------------------------- */

void ODS_BNA_getdim( char *fname  ,TInt4 *itype  , char *dim    ,
                    TInt4 *pardep ,TInt4 *timdep ,TInt4 *locdep ,
                    TInt4 *nodim  ,TInt4 *ierror , char *option )
                                      /* Returns nothing */
{
   *ierror = IEOK ;

/* This routine may need to actually read the file:
   for the locations and the grid (administration array)
*/
   if ( strncmp( dim , "TIM" , 3 ) == 0 ||
        strncmp( dim , "tim" , 3 ) == 0   )
   {
      nodim[0] = 1 ;
      nodim[1] = 1 ; /* There are no times in the file */
      return ;
   }
   if ( strncmp( dim , "PAR" , 3 ) == 0 ||
        strncmp( dim , "par" , 3 ) == 0   )
   {
      nodim[0] = 1 ;
      nodim[1] = 2 ; /* There are always two parameters in the file */
      return ;
   }
   if ( strncmp( dim , "LOC" , 3 ) == 0 ||
        strncmp( dim , "loc" , 3 ) == 0 )
   {
      nodim[0] = 4 ;
      nodim[1] = 0 ;
      nodim[2] = 0 ;
      nodim[3] = 0 ;
      nodim[4] = 0 ;
      ODS_BNA_readfile( fname , *itype , -1 , &nodim[1] , NULL , NULL ,
                        NULL  , ierror ) ;

   }
/* Is this really distinguished?
   if ( strncmp( dim , "GRD" , 3 ) == 0 ||
        strncmp( dim , "grd" , 3 ) == 0 )
   {
      ODS_BNA_readfile( fname , *itype , -2 , &nodim[4] , NULL , NULL ,
                        NULL  , ierror ) ;
      nodim[0] = 1 ;
      nodim[1] = 2 ; *//* There are always two parameters in the file *//*
      return ;
   }
*/
/* This is it, return
*/
   return ;
}

/* -------------------------------------------------------------------
   Function: ODS_BNA_getpar()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3    I/O  Full filename, including extension
   long    *itype      1    I/O  Filetype, see types.inc for definitions
   char    *pardef     1     I   Which parameter masks
   long    *maxdef     1     I   Number of masks
   long    *timdep     1     I   For which time (ignored)
   long    *locdep     1     I   For which location (ignored)
   long    *maxlst     1     I   Maximum number of parameters
   long    *lang       1     I   Progrma language (ignored)
   char    *parlst     1     O   List of parameters found
   char    *paruni     1     O   List of parameter units
   long    *partyp     1     O   List of parameter types
   long    *parcod     1     O   List of parameter codes
   long    *nrlst      1     O   Number of parameters found
   long    *ierror     1     O   Error code
   char    *option     1    I/O  Option (ignored)

--------------------------------------------------------------------- */

void ODS_BNA_getpar( char *fname  ,TInt4 *itype  , char *pardef ,
                    TInt4 *maxdef ,TInt4 *timdep ,TInt4 *locdep ,
                    TInt4 *maxlst ,TInt4 *lang   , char *parlst ,
                     char *paruni ,TInt4 *partyp ,TInt4 *parcod ,
                    TInt4 *nrlst  ,TInt4 *ierror , char *option )
                                      /* Returns nothing */
{
   *ierror = IEOK ;

/* This routine is a dummy: simply return up to two parameters:
   x-coordinate and y-coordinate.
*/
   *nrlst = 0 ;
   if ( *maxlst <= 0 )
   {
      *ierror = IEPMNY ;
   }
   if ( *maxlst >= 1 )
   {
      (*nrlst) ++ ;
      strcpy( parlst+0 , "x-coordinate" ) ;
      strcpy( paruni+0 , "m" ) ;
      partyp[0] = 0 ;
      parcod[0] = 1 ;
   }
   if ( *maxlst >= 2 )
   {
      (*nrlst) ++ ;
      strcpy( parlst+21 , "y-coordinate" ) ;
      strcpy( paruni+21 , "m" ) ;
      partyp[1] = 0 ;
      parcod[1] = 2 ;
   }

/* This is it, return
*/
   return ;
}

/* -------------------------------------------------------------------
   Function: ODS_BNA_getloc()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3    I/O  Full filename, including extension
   long    *itype      1    I/O  Filetype, see types.inc for definitions
   char    *locdef     1     I   Which location masks
   long    *maxdef     1     I   Number of masks
   long    *pardep     1     I   For which parameter (ignored)
   long    *timdep     1     I   For which time (ignored)
   long    *maxlst     1     I   Maximum number of parameters
   char    *loclst     1     O   List of locations found
   long    *locnr      1     O   List of location codes
   long    *loctyp     1     O   List of location types
   long    *nrlst      1     O   Number of locations found
   long    *ierror     1     O   Error code
   char    *option     1    I/O  Option (ignored)

--------------------------------------------------------------------- */

void ODS_BNA_getloc( char *fname  ,TInt4 *itype  , char *locdef ,
                    TInt4 *maxdef ,TInt4 *pardep ,TInt4 *timdep ,
                    TInt4 *maxlst , char *loclst ,TInt4 *locnr  ,
                    TInt4 *nrlst  ,TInt4 *ierror , char *option )
                                      /* Returns nothing */
{
   *ierror = IEOK ;

/* This routine is a dummy: simply return zero locations, as none
   has a name - currently.
*/
   *nrlst = 0 ;

/* This is it, return
*/
   return ;
}

/* -------------------------------------------------------------------
   Function: ODS_BNA_gettme()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3    I/O  Full filename, including extension
   long    *itype      1    I/O  Filetype, see types.inc for definitions
   double  *timdef    2*(*)  I   Which time intervals
   long    *maxdef     1     I   Number of intervals
   long    *pardep     1     I   For which parameter (ignored)
   long    *locdep     1     I   For which location (ignored)
   long    *maxlst     1     I   Maximum number of times
   double  *timlst     1     O   List of times found
   long    *timtyp     1     O   List of time types
   long    *nrlst      1     O   Number of times found
   long    *ierror     1     O   Error code
   char    *option     1    I/O  Option (ignored)

--------------------------------------------------------------------- */

void ODS_BNA_gettme( char *fname  ,TInt4   *itype  , double *timdef ,
                    TInt4 *maxdef ,TInt4   *pardep ,TInt4   *locdep ,
                    TInt4 *maxlst , double *timlst ,TInt4   *timtyp ,
                    TInt4 *nrlst  ,TInt4   *ierror , char   *option )
                                      /* Returns nothing */
{
  TInt4 year , month , day , hour , minute , secnd ;

   *ierror = IEOK ;

/* This routine is a dummy: simply return one time, as the grid is
   constant.
*/
   *nrlst = 1 ;
   year   = 1900 ;
   month  =    1 ;
   day    =    1 ;
   hour   =    0 ;
   minute =    0 ;
   secnd  =    0 ;

   julian( &year , &month , &day , &hour , &minute , &secnd ,
           &timlst[0] ) ;
   timtyp[0] = 0 ;

/* This is it, return
*/
   return ;
}

/* -------------------------------------------------------------------
   Function: ODS_BNA_getmat()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3    I/O  Full filename, including extension
   long    *itype      1    I/O  Filetype, see types.inc for definitions
   long    *parcod     1     I   Which parameter
   long    *loc       3*3    I   Which location(s)
   long    *tim        3     I   Which time(s) (ignored)
   float   *misval     1     I   Missing value
   long    *i3gl       1     I   Which program language (ignored)
   long    *maxdim     1     I   Maximum number of data
   float   *data     maxdim  O   Retrieved data
   long    *ierror     1     O   Error code
   char    *option     1    I/O  Option (ignored)

--------------------------------------------------------------------- */

void ODS_BNA_getmat( char *fname  ,TInt4 *itype  ,TInt4  *parcod ,
                    TInt4 *loc    ,TInt4 *tim    , float *misval ,
                    TInt4 *i3gl   ,TInt4 *maxdim , float *data   ,
                    TInt4 *ierror , char *option                 )
                                      /* Returns nothing */
{

   *ierror = IEOK ;

/* This routine simply calls ODS_BNA_readfile()
*/
   ODS_BNA_readfile( fname , *itype , *parcod , maxdim , data , NULL ,
                     NULL  , ierror ) ;

/* This is it, return
*/
   return ;
}

/* -------------------------------------------------------------------
   Function: ODS_BNA_getgrd()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3    I/O  Full filename, including extension
   long    *itype      1    I/O  Filetype, see types.inc for definitions
   long    *loc       3*3    I   Which location(s)
   long    *iadmin    3*(*)  O   Administration array
   long    *nocell     1     O   Number of grid cells
   long    *igisty     1     O   Type of geographical dataset
   long    *ierror     1     O   Error code

--------------------------------------------------------------------- */

void ODS_BNA_getgrd( char *fname  ,TInt4 *itype  ,TInt4  *loc    ,
                    TInt4 *iadmin ,TInt4 *nocell ,TInt4  *igisty ,
                    TInt4 *ierror                                )
                                      /* Returns nothing */
{
  TInt4 nodim ;

   *ierror = IEOK ;

/* This routine simply calls ODS_BNA_readfile()
*/
   nodim   = loc[1] ;
   *igisty = IGLAND ;
   ODS_BNA_readfile( fname  , *itype , 0 , &nodim , NULL , iadmin ,
                     nocell , ierror ) ;

/* This is it, return
*/
   return ;
}

/* -------------------------------------------------------------------
   Function: ODS_BNA_readfile()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3    I/O  Full filename, including extension
   long     itype      1     I   Filetype, see types.inc for definitions
   long     action     1     I   Type of action:
                                 -1: get the number of coordinate pairs
                                 -2: get the size of the adminstration array
                                  0: get the adminstration array
                                  1: get the x-coordinate
                                  2: get the y-coordinate
   long    *size       4    I/O  Size of the arrays
   float   *data       *     O   Array of coordinate values (1 or 2)
   long    *iadmin    3*(*)  O   Administration array
   long    *nocell     1     O   Number of grid cells
   long    *ierror     1     O   Error code

--------------------------------------------------------------------- */

static void ODS_BNA_readfile( char *fname  ,TInt4  itype   ,TInt4  action ,
                             TInt4 *size   , float *data   ,TInt4 *iadmin ,
                             TInt4 *nocell ,TInt4  *ierror                )
                                      /* Returns nothing */
{
   FILE     *infile        ;
   char     line[1000]     ; /* This should be long enough for most applications! */
   char     *pstr          ;
  TInt4 code   , icrd  , iline  , idadm  , count  , closed ;
  TInt4 noitem , nocrd , maxseg , maxcrd , nocrd2 , i      ;
  TInt4 item_type , no_info ;
   float    xcrd , ycrd    , xbgn   , ybgn   ;

   static TInt4 item_info[7] = { 1 , 2 , 2 , 3 , 0 , 0 , 1 } ;

   *ierror = IEOK ;
   closed = 0 ;
   noitem = 0 ;
   nocrd  = 0 ;
   maxseg = 0 ;
   maxcrd = 0 ;
   icrd   = 0 ;
   idadm  = 0 ;

/* Open the file
*/
   infile = fopen( fname , "r" ) ;
   if ( infile == NULL )
   {
      *ierror = IENOFI ;
      return ;
   }

/* Skip any preliminaries:
   - for a BNA file there are none
   - for a TEKAL file you might have comments (which are ignored)
   - a DXF file starts with a header section that can not be
     interpreted correctly, nor is it meaningful to us (so ignore it)
   To make sure all files behave in the same way, read the first line.
   We try to make sure that the file is of the correct type from this
   first line:
   - BNA: the first character will be a double quote (")
   - TEKAL: the first character will be an asterisk or an alphabetical
     character
   - DXF: the first line is "0  ", the second line is "SECTION"

   Note:
   Do not use sscanf() because MS Windows DLL's do not support that
   function. For the same reason, avoid strtok().
*/
   readLine( line , sizeof( line ) , infile ) ;
   switch( itype )
   {
      case ODS_GEOGR_BNA :
         if ( line[0] != '"' )
         {
            *ierror = IETYPE ;
            break ;
         }
         break ; /* Nothing to do */

      case ODS_GEOGR_TEKAL :
         if ( !isalnum( line[0] ) && line[0] != '*' )
         {
            *ierror = IETYPE ;
            break ;
         }
         while ( line[0] == '*' )
         {
            readLine( line , sizeof( line ) , infile ) ;
         }
         break ;

      case ODS_GEOGR_DXF :
         if ( strncmp( line , "0"   , 1 ) != 0 &&
              strncmp( line , " 0"  , 2 ) != 0 &&
              strncmp( line , "  0" , 3 ) != 0     ) *ierror = IETYPE ;
         readLine( line , sizeof( line ) , infile ) ;
         if ( strncmp( line , "SECTION" , 7 ) != 0 ) *ierror = IETYPE ;
         if ( *ierror != IEOK )
         {
            break ;
         }
/* Find the beginning of the information we want: a group with a name
   like "POINT" or "LINE"
*/
         while( !ferror( infile ) && !feof( infile ) )
         {
            readLine( line , sizeof( line ) , infile ) ;
            code = atol( line ) ;
            if ( code < 0 || code > 9 )
            {
               readLine( line , sizeof( line ) , infile ) ;
               continue ;
            }
            else
            {
               readLine( line , sizeof( line ) , infile ) ;
               item_type = -1 ;
               if ( strncmp( line , "POINT"   , 5 ) == 0 ) item_type = 0 ;
               if ( strncmp( line , "LINE"    , 4 ) == 0 ) item_type = 1 ;
/*             if ( strncmp( line , "ARC"     , 3 ) == 0 ) item_type = 2 ; */
/*             if ( strncmp( line , "CIRCLE"  , 6 ) == 0 ) item_type = 3 ; */
/* Use the keyword VERTEX instead:
               if ( strncmp( line , "POLYLINE", 8 ) == 0 ) item_type = 4 ; */
               if ( strncmp( line , "VERTEX"  , 6 ) == 0 ) item_type = 4 ;
/*             if ( strncmp( line , "TEXT"    , 4 ) == 0 ) item_type = 5 ; */
               if ( item_type != -1 )
               {
                  break ; /* We have found the beginning */
               }
            }
         }
         break ;

      default:
         *ierror = IETYPE ;
         break ;
   }
   if ( *ierror != IEOK )
   {
      fclose( infile ) ;
      return ;
   }

/* These were the preliminaries: now the real work.
   - For BNA files: we have a line of the form
        "<string1>","<string2>",<number>
     or:
        "<string1>","<string2>",1,<xcrd>,<ycrd>
     The number is the number of coordinate pairs
   - For TEKAL files: we have a line containing the name of the block.
     The next line contains the number of rows and columns
   - For DXF files: we simply have the beginning of a list of coordinate
     pairs. We stop with the current item, when the code is no longer
     within the range.
*/
   noitem = 0 ;
   while( !ferror( infile ) && !feof( infile ) )
   {
      if ( action == 0 )
      {
         iadmin[idadm]   = 0          ;
         iadmin[idadm+1] = maxcrd     ;
         iadmin[idadm+2] = maxcrd - 1 ;
      }

      switch( itype )
      {
/* BNA: Analyse the line and read the coordinates
   Is the first name perhaps a formatted number? (Then it is the segment
   number)
*/
         case ODS_GEOGR_BNA :
            pstr   = strchr( line   , '"' ) ;
            code   = atol( pstr+1 ) ;
            closed = 0 ;

            if ( code != 0 )
            {
               maxseg = ( maxseg > code ? maxseg : code ) ;
               if ( action == 0 )
               {
                  iadmin[idadm] = code ;
               }
            }
            pstr   = strchr( line   , ',' ) ;
            pstr   = strchr( pstr+1 , ',' ) ;
            nocrd  = atol( pstr+1 ) ; /* Get the number of coordinates */
            nocrd2 = ( nocrd >= 0 ? nocrd : -nocrd ) ;
            if ( nocrd < 0 )
            {
               closed = 1 ;
            }
            maxcrd = maxcrd + nocrd2 ;
            iline  = 0 ;
            if ( nocrd2 > 1 )
            {
               readLine( line , sizeof( line ) , infile ) ;
               pstr  = line ;
            }
            else
            {
               pstr  = strchr( pstr+1 , ',' ) ;
            }

            while( iline < nocrd2 )
            {
               /* Not very robust
               xcrd = atof( pstr+1 ) ;
               pstr = strchr( pstr+1 , ',' ) ;
               ycrd = atof( pstr+1 ) ;
               */
               sscanf( pstr , "%g%*[ ,\t]%g" , &xcrd , &ycrd ) ;

               if ( iline == 0 )
               {
                  xbgn = xcrd ;
                  ybgn = ycrd ;
               }

               if ( action == 1 ) data[icrd] = xcrd ;
               if ( action == 2 ) data[icrd] = ycrd ;
               icrd ++ ;

               iline ++ ;
               readLine( line , sizeof( line ) , infile ) ;
               pstr = line ;
               if ( ferror( infile ) )
               {
                  *ierror = IEUEOF ;
                  break ; /* Break from this while-loop */
               }
               if ( feof( infile ) )
               {
                  break ; /* Break from this while-loop */
               }
            }

            break ; /* BNA files */

/* TEKAL-files are somewhat easier to handle (assume that there are at
   least two columns: the x-coordinate and the y-coordinate)
*/
         case ODS_GEOGR_TEKAL :
            closed = 0 ; /* No indication for open/closed provided */

            readLine( line , sizeof( line ) , infile ) ;
            nocrd  = atol( line ) ;
            nocrd2 = nocrd        ;
            maxcrd = maxcrd + nocrd ;
            for ( i = 0 ; i < nocrd ; i ++ )
            {
               readLine( line , sizeof( line ) , infile ) ;
               if ( ferror( infile ) )
               {
                  *ierror = IEUEOF ;
                  break ; /* Break from this while-loop */
               }
               if ( action > 0 )
               {
                  /* TODO: use sscanf() to read the numbers! */

                  if ( action == 1 )
                  {
                     data[icrd] = atof( line ) ;
                  }
                  else
                  {
                     pstr  = line ;
                     count = 0    ;
                     while ( *pstr != '\n' && *pstr != '\0' )
                     {
                        if ( ( *pstr      != ' ' && *pstr     != ',' ) &&
                             (  *(pstr+1) == ' ' || *(pstr+1) == ',' ) )
                        {
                           data[icrd] = atof( pstr+2 ) ;
                           break ;
                        }
                        pstr ++ ;
                     }
                  }
                  icrd ++ ;
               }

               if ( feof( infile ) )
               {
                  break ; /* Break from this while-loop */
               }
            }
/* Skip the name and comments in the next block
*/
            while( !feof( infile ) && !ferror( infile ) )
            {
               readLine( line , sizeof( line ) , infile ) ;
               if ( line[0] != '*' ) break ;
            }
            break ; /* TEKAL files */

/* DXF-files, finally, contain various items, points, lines, polygons
   etc. The information on each item starts with a code and a name,
   then groups of codes and values.
   For points, lines, arcs, cicrles and texts:
   1. code = 10: starting point, x value - all
   2. code = 20: starting point, y value - all
   3. code = 11: starting point, x value - line only
   4. code = 21: starting point, y value - line only
   5. code = 40: radius - circle, arc
   6. code = 50: starting angle - arc
   7. code = 51: ending angle - arc
   8. code =  1 or 8: text string - text
   9. code = 62: colour (has a default: zero, needs not be
      present)
   An item ends when all information (typified by the number of data,
   not their character) is read. From the above we ignore circles, arcs
   and texts.

   For polygons, the most interesting item for maps, we need a slightly
   different procedure:
   The vertices are indicated with the name 'VERTEX'. The codes that
   are recognised are:
   1. code = 10: starting point, x value - vertex
   2. code = 20: starting point, y value - vertex
   3. code = 70: polyline flag (closed or not) - polyline only
   4. code = 62: colour (has a default: zero, needs not be
                 present) - is ignored
   The polyline ends with 'SEQEND'
*/
         case ODS_GEOGR_DXF :
            no_info = 0 ;
            while( code >= 0 )
            {
               readLine( line , sizeof( line ) , infile ) ;
               code = atol( line ) ;
               readLine( line , sizeof( line ) , infile ) ;
               switch( code )
               {
                  case 10 :
                  case 11 :
                     xcrd = atof( line ) ;
                     if ( action == 1 ) data[icrd] = xcrd ;
                     if ( icrd   == 0 ) xbgn       = xcrd ;
                     no_info ++ ;
                     break ;
                  case 20 :
                  case 21 :
                     ycrd = atof( line ) ;
                     if ( action == 2 ) data[icrd] = ycrd ;
                     if ( icrd   == 0 ) ybgn       = ycrd ;
                     icrd    ++ ; /* Only here! Assume that x comes before y */
                     no_info ++ ;
                     break ;
                  case 70 :
                     closed = atol( line ) % 2 ; /* Closed or not? */
                     break ;
                  default :
                     break ;
               }
               if ( strncmp( line , "SEQEND" , 6 ) == 0 )
               {
                  code = -1 ;
               }
               if ( no_info >= item_info[item_type] && item_type != 4 )
               {
                  code    = -1 ;
                  no_info =  0 ;
               }
            }

            while( !feof( infile ) && !ferror( infile ) )
            {
               readLine( line , sizeof( line ) , infile ) ;
               code = atol( line ) ;
               readLine( line , sizeof( line ) , infile ) ;
               item_type = -1 ;
               if ( strncmp( line , "POINT"   , 5 ) == 0 ) item_type = 0 ;
               if ( strncmp( line , "LINE"    , 4 ) == 0 ) item_type = 1 ;
/*             if ( strncmp( line , "ARC"     , 3 ) == 0 ) item_type = 2 ; */
/*             if ( strncmp( line , "CIRCLE"  , 6 ) == 0 ) item_type = 3 ; */
/* use vertex  if ( strncmp( line , "POLYLINE", 8 ) == 0 ) item_type = 4 ; */
               if ( strncmp( line , "VERTEX"  , 6 ) == 0 ) item_type = 4 ;
/*             if ( strncmp( line , "TEXT"    , 4 ) == 0 ) item_type = 5 ; */
               if ( item_type >= 0 )
               {
                  break ;
               }
            }

            nocrd2 = icrd - maxcrd ;
            maxcrd = icrd ;
            break ; /* DXF files */

         default:
            break ;
      }

/* Finish the administration: closed polyline (polygon)?
*/
      if ( closed == 1 )
      {
         if ( xbgn != xcrd || ybgn != ycrd )
         {
            nocrd2 ++ ;
            maxcrd ++ ;

            if ( action == 1 ) data[icrd] = xbgn ;
            if ( action == 2 ) data[icrd] = ybgn ;
         }
      }
      if ( action == 0 )
      {
         iadmin[idadm+2] = iadmin[idadm+2] + nocrd2 ;
      }
      idadm  += 3 ;
      noitem += 1 ;
   }

/* This is it, return
*/
   if ( action == -2 )
   {
      *nocell = maxseg ;
   }
   if ( action == 0 )
   {
      *nocell = maxseg ;
   }
   if ( action == -1 )
   {
      size[0] = maxcrd     ;
      size[1] = 1          ;
      size[2] = 1          ;
      size[3] = 3 * noitem ;
   }
   fclose( infile ) ;
   return ;
}

