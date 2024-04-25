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
 *  morbagr.c  -  ODS: MORSYS "bagger" files (functions)
 *
 *  Derived from code by Cor ten Napel (mor_sys.f)
 *  Arjen Markus
 */

/*   Date:       17 June 1997                                         */
/*   Time:       14:30                                                */
/*   Program:    morbagr.c                                            */
/*   Version:    1.00                                                 */
/*   Programmer: Arjen Markus                                         */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   Project:    Open Data Structuur                                  */
/*   Module:                                                          */
/*   Function:                                                        */
/*   Comment:                                                         */
/*   Reference:                                                       */
/*   Review:                                                          */
/*
 *  $Author: Markus $
 *  $Date: 1-04-03 10:52 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/morbagr.c,v $
*/
/*
 *
 */

#include <string.h>
#include <stdio.h>

#include "portable.h"
#include "ods.h"
#include "odsmodel.h"

#ifndef NOPROT

/* function prototypes */

TVoid MorNefisGetT0( TInt4  *deffds , TInt4  *datfds ,
                     TReal8 *t0     , TReal8 *dt0    ) ;

#else

static void MorNefisGetT0( ) ;

#endif

/*                                1         2 */
/*                       12345678901234567890 */
#define BLANK20         "                    "
#define SUBSNAMELEN     20

#define max(a,b)        ( a < b ? b : a )

/*----------------------------------------------------------------------
 * The routine strrstr() is not part of the ANSI standard, so use
 * one of our own instead. This may be slower, but it is portable.
 *---------------------------------------------------------------------- */
static TString ReverseFindString( TString string , TString substr )
{
   TString pstr1 , pstr2 ;

   pstr1 = string ;
   pstr2 = strstr( pstr1 , substr ) ;

/* The case that the substring is not present, is special
*/
   if ( pstr2 == NULL ) /* Not found? */
   {
      return NULL ;
   }

/* It was found, now search the last occurrence
*/
   while( pstr2 != NULL )
   {
      pstr1 = pstr2 + 1 ; /* possibly: "\0" */
      pstr2 = strstr( pstr1 , substr ) ;
   }

   return ( pstr1 - 1 ) ;
}


/* -------------------------------------------------------------------
   Function: MorOpenNefisFiles()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   TString  fname      3    I/O  Full filename, including extension
   long     ftype      1     I   Filetype, see types.inc for definitions
   BInt4   *datfds     1    I/O  Array for data file
   BInt4   *deffds     1    I/O  Array for definition file
   long    *ierror     1     O   Error code

--------------------------------------------------------------------- */

#ifndef NOPROT
   void MorOpenNefisFiles ( TString fname,   TInt4 ftype,
                            BInt4 *datfds,   BInt4 *deffds,
                            TInt4 *ierror)
#else
   void MorOpenNefisFiles ( fname,      ftype,      datfds,      deffds,
                            ierror)

   BInt4 *datfds, *deffds;
   TInt4 *ierror, ftype;
#endif
{
   TInt4 i , maxdim   ;
   TInt4 dims[5] , usrord[5] ;
   TString pstr        ;
   TInt4   length       ;
   BInt4   err_nef      ;
   TChar   celnam[20]   ;
   TChar   datext_up[5] , datext_lw[5] ;
   TChar   defext[5]    ;

   static TChar grp_nam[4][10] =
   {
      "GRID" , "TEMPOUT" , "MAPBGREF" , "MAPTBAG"
   } ;

   *ierror = IEOK;

/* Construct the appropriate second filename
   - ignore the possibility that it is already given
*/
/* Was:
   length = strlen( fname+ODS_FILNAMLEN ) ;
   if ( length == 0 )
   {
*/
   strcpy( fname+ODS_FILNAMLEN , fname ) ;
   switch ( ftype )
   {
      case ODS_MORSYS_BAGR_NEFIS :
         strcpy( datext_up , ".DAT" ) ;
         strcpy( datext_lw , ".dat" ) ;
         strcpy( defext    , ".def" ) ;
         break ;
   }
   pstr = ReverseFindString( fname+ODS_FILNAMLEN , datext_up ) ;
   if ( pstr == NULL )
   {
      pstr = ReverseFindString( fname+ODS_FILNAMLEN , datext_lw ) ;
      if ( pstr == NULL )
      {
         *ierror = IETYPE ;   /* A hack: but I can not determine the
                                 proper extensions */
         return ;
      }
   }
   strcpy( pstr , defext ) ;

/* Ending brace removed  - see above! */

/* Open files
*/
   OPNNEF(fname, &ftype, datfds, deffds, ierror);
   if (*ierror != 0 )
       return;

/* Check whether the groups GRID, TEMPOUT, MAPBGREF and MAPTBAG
   are present
*/
   for ( i = 0 ; i < 4 ; i ++ )
   {
      maxdim  = 5 ;
      err_nef = Inqgrp( deffds , &grp_nam[i][0] , celnam , &maxdim ,
                        dims   , usrord                            ) ;
      if ( err_nef != 0 )
      {
         CloseNefisFiles( fname , datfds , deffds , ierror ) ;
         *ierror = IEINFO ;
         return ;
      }
   }

/* We are ready
*/
   return ;
}

/* -------------------------------------------------------------------
   Function: MorGetParNames()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   BInt4   *deffds     1     I   Array for definition file
   TString  parnam     *     O   Names of parameters
   long    *parcod     *     O   Indices of parameters
   long    *nopar      1     O   Number of parameters (in: maximum)

--------------------------------------------------------------------- */

#ifndef NOPROT
static void MorGetParNames( BInt4 *deffds , TString parnam ,
                            TInt4 *parcod , TInt4   *nopar )
#else
static void MorGetParNames( deffds , parnam ,
                            parcod , nopar  )
   BInt4   *deffds ;
   TString parnam ;
   TInt4   *parcod ;
   TInt4   * nopar ;
#endif
{
   TInt4 i , nocomp ;
   TInt4 newpar     ;
   BInt4 err_nef    ;
   TInt4 nbytsg     ;
   TInt4 elmdim     ;
   TInt4 dims[5]    ;
   TChar elmtyp[20] , elmqty[20] , elmunt[20] ;
   TChar elmdsc[80] ;

   static TChar elmnam[6][20] =
   {
      "XCOR" , "YCOR" , "DPBREF" , "DDPBAG" , "DP" , "DPnodredg"
   } ;
   static elmcod[6] =
   {
         -1  ,    -2  ,      -3  ,       1  ,   2 ,          3
   } ;

/* Loop over the element names that are expected
*/
   nocomp = 0 ;
   newpar = 0 ;

   for ( i = 0 ; i < 5 ; i ++ )
   {
      elmdim  = 5 ;
      err_nef = Inqelm( deffds , &elmnam[i][0]   , elmtyp  , &nbytsg ,
                        elmqty , elmunt , elmdsc , &elmdim , dims    ) ;
      if ( err_nef == 0 )
      {
         strcpy( &parnam[newpar*21] , &elmnam[i][0] ) ;
         parcod[newpar] = elmcod[i] ;
         newpar         ++          ;
         if ( i >= 3 )
         {
            nocomp ++ ;
         }
      }
      if ( newpar > (*nopar)-1 )
      {
         break ; /* Too many names! */
      }
   }

/* Do we have the data for the combined parameter?
*/
   if ( nocomp >= 2 )
   {
      strcpy( &parnam[(newpar)*21] , &elmnam[5][0] ) ;
      parcod[newpar] = elmcod[5] ;
      newpar         ++          ;
   }

/* Set the number of parameters
*/
   *nopar = newpar ;

   return ;
}

/* -------------------------------------------------------------------
   Function: MorGetT0()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   BInt4   *deffds     *     I   Array for definition file
   BInt4   *datfds     *     I   Array for data file
   TReal8  *t0         1     O   Reference time
   TReal8  *dt0        1     O   Timestep in days

--------------------------------------------------------------------- */

#ifndef NOPROT
static void MorGetT0( BInt4  *deffds , BInt4  *datfds ,
                      TReal8 *t0     , TReal8 *dt0    )
#else
static void MorGetT0( deffds , datfds ,
                      t0     , dt0    )
   BInt4  *deffds ;
   BInt4  *datfds ;
   TReal8 *t0     ;
   TReal8 *dt0    ;
#endif
{
   BInt4  err_nef      ;
   TInt4  ito1 , ito2  ;
   TReal4 tscale       ;
   TInt4  uindex[5][3] ;
   TInt4  usrord[5]    ;
   TInt4  buflen       ;
   TInt4  iday  , imonth , iyear ;
   TInt4  ihour , imin   , isec  ;
   TChar  grpnam[20] ;
   TChar  elmnam[20] ;

   *dt0 = 1.0 ;
   *t0  = 1.0 ;

/* We need the elements ITO1, ITO2 and TSCALE from group MAPBGREF
   Assume no error can or will occur. Makes life a lot easier and
   if an error occurs, it is most probably catched before this
   routine.
*/
   strcpy( grpnam , "MAPBGREF" ) ;
   uindex[0][0] = 1 ; uindex[0][1] = 1; uindex[0][2] = 1;
   usrord[0]    = 1 ;
   buflen = sizeof( TInt4 );

   strcpy( elmnam , "ITO1" ) ;
   err_nef = Getelt( deffds , grpnam , elmnam , uindex ,
                     usrord , &buflen , &ito1                    ) ;

   strcpy( elmnam , "ITO2" ) ;
   err_nef = Getelt( deffds , grpnam , elmnam , uindex ,
                     usrord , &buflen , &ito2                    ) ;

   buflen = sizeof( TReal4 );
   strcpy( elmnam , "TSCALE" ) ;
   err_nef = Getelt( deffds , grpnam  , elmnam , uindex ,
                     usrord , &buflen , &tscale                   ) ;

/* Turn ITO1 and ITO2 into a julian date and time
*/
   iyear  = ito1 / 10000 ;
   imonth = ( ito1 - iyear * 10000 ) / 100 ;
   iday   = ito1 % 100 ;

   ihour  = ito2 / 10000 ;
   imin   = ( ito2 - ihour * 10000 ) / 100 ;
   isec   = ito2 % 100 ;

   julian( &iyear , &imonth , &iday , &ihour , &imin , &isec , t0 ) ;

/* TSCALE is the timestep in seconds
   Note:
   We ignore for the moment the possibility of other units
*/
   *dt0 = tscale / 86400.0 ;

   return ;
}

/* -------------------------------------------------------------------
   Function: MorOdsGetDim()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3    I/O  Full filename, including extension
   long    *ftype      1    I/O  Filetype, see types.inc for definitions
   char    *dim        1     I   Dimension required.
   long    *pardep     1     I   For which parameter (ignored)
   long    *timdep     1     I   For which time (ignored)
   long    *locdep     1     I   For which location (ignored)
   long    *nodim      5     O   Dimensions returned
   long    *ierror     1     O   Error code
   char    *option     *     I   Dummy argument

--------------------------------------------------------------------- */

#ifndef NOPROT
void MorOdsGetDim( TString fname   , TInt4 *ftype  , TString dim     ,
                   TInt4   *pardep , TInt4 *timdep , TInt4   *locdep ,
                   TInt4   *nodim  , TInt4 *ierror , TString option  )
                                      /* Returns nothing */
#else
void MorOdsGetDim( fname  , ftype  , dim    ,
                   pardep , timdep , locdep ,
                   nodim  , ierror , option )
     TString fname   ;
     TInt4   *ftype  ;
     TString dim     ;
     TInt4   *pardep ;
     TInt4   *timdep ;
     TInt4   *locdep ;
     TInt4   *nodim  ;
     TInt4   *ierror ;
     TString option  ;
#endif
{
   TInt4 dims[5]      , uindex[5][3], usrord[5]  ;
   TInt4 elmdim       , nbytsg      , nopar      ;
   TInt4 maxdim       , dim1        , buflen     ;
   TInt4 parcod[10]                              ;
   TChar celnam[20]   , parnam[10][21]           ;
   TChar elmnam[20]   , elmtyp[20]  , elmqty[20] , elmunt[20] , elmdsc[80] ;
   TChar grpnam[20]                              ;
   BInt4 deffds[2997] , datfds[999]              ;
   BInt4 err_nef                                 ;

   *ierror = IEOK ;

/* Check whether the file is of the right type. Then the
   groups GRID, TEMPOUT and MAPBREF must be present.
*/
   MorOpenNefisFiles( fname , *ftype , datfds , deffds , ierror ) ;
   if ( *ierror != IEOK )
       return;

/* Which type of dimension should be returned?
   - If time, then the parameter may be of importance:
     - the grid parameters (XCOR and YCOR) have no time whatsoever
     - the parameters DP and DDPBAG (and the derived DP-DDPBAG) do
     To distinguish them, positive indices indicate time-dependent
     parameters
*/
   if ( strncmp( dim , "TIM" , 3 ) == 0 ||
        strncmp( dim , "tim" , 3 ) == 0   )
   {
      nodim[0] = 1 ;
      if ( *pardep < 0 )
      {
         nodim[1] = 0 ; /* No times in the file for these parameters */
      }
      else
      {
/* Get the number of timesteps
*/
         maxdim = 5 ;
         err_nef = Inqgrp( deffds , "MAPTBAG" , celnam , &maxdim ,
                           dims   , usrord                       ) ;
         if ( err_nef != 0 )
         {
            CloseNefisFiles ( fname, datfds, deffds, ierror);
            *ierror = IEINFO ;
            return ;
         }
         if (dims[0] == 0)
         {
             err_nef = Inqmxi( deffds , "MAPTBAG" , &dim1 ) ;
             if ( err_nef != 0 )
             {
                CloseNefisFiles( fname , datfds , deffds , ierror ) ;
                *ierror = IEINFO ;
                return ;
             }
             dims[0] = dim1 ;
         }
         nodim[1] = dims[0] ;
      }
   }

/* The number of parameters. We may return the following:
   - XCOR      (-1), YCOR (-2)
   - DPBREF    (-3)
   - DP        (1)
   - DDPBAG    (2)
   - DP-DDPBAG (3)
   But for safety: check their presence
*/
   if ( strncmp( dim , "PAR" , 3 ) == 0 ||
        strncmp( dim , "par" , 3 ) == 0   )
   {
      nopar    =    10 ; /* No more than 10 parameters at the moment */
      MorGetParNames( deffds , &parnam[0][0] , parcod , &nopar ) ;
      nodim[0] =     1 ;
      nodim[1] = nopar ;
   }

/* The number of locations. We have a simple grid: MMAX by NMAX
*/
   if ( strncmp( dim , "LOC" , 3 ) == 0 ||
        strncmp( dim , "loc" , 3 ) == 0 )
   {
      nodim[0] = 2 ;
      nodim[1] = 0 ;
      nodim[2] = 0 ;
      strcpy( grpnam , "GRID" ) ;
      strcpy( elmnam , "MMAX" ) ;

      uindex[0][0] = 1 ; uindex[0][1] = 1; uindex[0][2] = 1;
      usrord[0]    = 1 ;
      buflen = sizeof( TInt4 );
      err_nef = Getelt( deffds , grpnam    , elmnam , uindex ,
                        usrord , &buflen , &nodim[1]                   ) ;

      strcpy( elmnam , "NMAX" ) ;
      err_nef = Getelt( deffds , grpnam    , elmnam , uindex ,
                        usrord , &buflen , &nodim[2]                   ) ;
   }

   CloseNefisFiles( fname , datfds , deffds , ierror ) ;
   return ;
}

/* -------------------------------------------------------------------
   Function: MorOdsGetPar()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   TString  fname      3    I/O  Full filename, including extension
   TInt4   *ftype      1    I/O  Filetype, see types.inc for definitions
   TString  pardef     1     I   For which parameter strings (ignored)
   TInt4   *maxdef     1     I   Number of parameter strings
   TString  parlst   maxlst  O   List of parameters found.
   TString  paruni   maxlst  O   Units of parameters
   TInt4   *maxlst     1     I   Max. nr of parameters to return
   TInt4   *nrlst      1     O   Number of parameters returned
   TInt4   *partyp     1     O   Type of parameter
   TInt4   *parcod     1     O   Code of parameter
   long    *ierror     1     O   Error code
   TString  option     *     I   Dummy argument

--------------------------------------------------------------------- */
#ifndef NOPROT
   void MorOdsGetPar( TString fname   , TInt4   *ftype  , TString pardef  ,
                      TInt4   *maxdef , TString parlst  , TString paruni  ,
                      TInt4   *maxlst , TInt4   *nrlst  , TInt4   *partyp ,
                      TInt4   *parcod , TInt4   *ierror , TString option  )
#else
   void MorOdsGetPar( fname  , ftype  , pardef , maxdef ,
                      parlst , paruni , maxlst , nrlst  ,
                      partyp , parcod , ierror , option )

   TString fname  , pardef  , parlst  , paruni  , option  ;
   TInt4   *ftype , *maxdef , *maxlst , *partyp , *parcod ,
           *nrlst , *ierror ;
#endif
{
   TInt4   i         ;
   BInt4   deffds[2997], datfds[999] ;

   *ierror = IEOK ;

/* Check whether the file is of the right type. Then the
   groups GRID, TEMPOUT and MAPBREF must be present.
*/
   MorOpenNefisFiles( fname , *ftype , datfds , deffds , ierror ) ;
   if ( *ierror != IEOK )
       return;

/* Get the parameter names
*/
   *nrlst = *maxlst ;
   MorGetParNames( deffds , parlst , parcod , nrlst ) ;

/* Fill in some missing information
*/
   for ( i = 0 ; i < *nrlst ; i ++ )
   {
      strcpy( &paruni[i*21] , "[m]" ) ;
      partyp[i] = ODS_PT_LOC_MNK | ODS_PT_TIME_DEP ;
   }

   CloseNefisFiles( fname , datfds , deffds , ierror ) ;
   return ;
}

/* -------------------------------------------------------------------
   Function: MorOdsGetLoc()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3    I/O  Full filename, including extension
   long    *ftype      1    I/O  Filetype, see types.inc for definitions
   char    *locdef     1     I   For which location strings (ignored)
   long    *maxdef     1     I   Number of location strings
   char    *loclst   maxlst  O   List of locations found.
   long    *maxlst     1     I   Max. nr of locations to return.
   long    *nrlst      1     O   Nr of locations returned.
   long    *locnr   maxlst   O   List of index numbers of locations found
   long    *ierror     1     O   Error code
   char    *option     *     I   Dummy argument

--------------------------------------------------------------------- */

#ifndef NOPROT
   void MorOdsGetLoc ( TString fname   , TInt4   *ftype , TString locdef  ,
                       TInt4   *maxdef , TString loclst , TInt4   *maxlst ,
                       TInt4   *nrlst  , TInt4   *locnr , TInt4   *ierror ,
                       TString option                                     )
#else
   void MorOdsGetLoc ( fname  , ftype  , locdef , maxdef ,
                       loclst , maxlst , nrlst  , locnr  ,
                       ierror , option                   )

   TString fname  , locdef  , loclst  , option ;
   TInt4   *ftype , *maxdef , *maxlst , *nrlst ,
           *locnr , *ierror                    ;
#endif

{

/* Dummy routine: there are no names to return!
*/
   *nrlst = 0 ;
   return ;
}

/* -------------------------------------------------------------------
   Function: MorOdsGetTme()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3    I/O  Full filename, including extension
   long    *ftype      1    I/O  Filetype, see types.inc for definitions
   TReal8  *timdef   maxdef  I   For which times (ignored)
   long    *maxdef     1     I   Number of times requested
   TReal8  *timlst   maxlst  O   List of times found.
   long    *maxlst     1     I   Max. nr of times to return.
   long    *nrtim      1     O   Nr of times returned.
   long    *ierror     1     O   Error code
   char    *option     *     I   Dummy argument

--------------------------------------------------------------------- */

#ifndef NOPROT
   void MorOdsGetTme ( TString fname   , TInt4 *ftype  ,
                       TReal8  *timdef , TInt4 *maxdef , TReal8 *timlst ,
                       TInt4   *maxlst , TInt4 *nrtim  , TInt4  *ierror ,
                       TString option                                   )
#else
   void MorOdsGetTme ( fname  , ftype  , timdef , maxdef ,
                       timlst , maxlst , nrtim  , ierror ,
                       option                            )

   TString fname   , option  ;
   TInt4   *maxdef , *maxlst , *nrtim , *ierror , *ftype ;
   TReal8  *timdef , *timlst ;
#endif

{
   char    celnam[20];
   char    elmnam[20];
   char   *deffn;
   int     stap;
   long    offst     ;
   TInt4  *tbuff     ;
   TInt4   i , ldum  ;
   BInt4   err_nef ;
   BInt4   elmndm, elmdms[5];
   BInt4   maxdim, nbytsg, buflen, dim1;
   BInt4   uindex[1][3], deffds[2997], datfds[999], dims[5], usrord[5];
   TReal8  juldat , dt0     ;
   TReal8  t0     , newtime ;

   *ierror = IEOK ;
   t0      = 0.0  ;

/* Check whether the file is of the right type. Then the
   groups GRID, TEMPOUT and MAPBREF must be present.
*/
   MorOpenNefisFiles( fname , *ftype , datfds , deffds , ierror ) ;
   if ( *ierror != IEOK )
       return;

/* Get the number of timesteps
*/
   maxdim = 5 ;
   err_nef = Inqgrp( deffds , "MAPTBAG" , celnam , &maxdim ,
                     dims   , usrord                       ) ;
   if ( err_nef != 0 )
   {
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *ierror = IEINFO ;
      return;
   }
   if ( dims[0] == 0 )
   {
       err_nef = Inqmxi( deffds , "MAPTBAG" , &dim1 ) ;
       if ( err_nef != 0 )
       {
          CloseNefisFiles( fname , datfds , deffds , ierror ) ;
          *ierror = IEINFO ;
          return ;
       }
       dims[0] = dim1 ;
   }

/* Get the times
*/
   strcpy( elmnam , "T-BAG" ) ;
   usrord[0]    = 1;
   uindex[0][0] = 1; /* start,stop,step */
   uindex[0][1] = dims[0];
   uindex[0][2] = 1;
   buflen  = dims[0] * sizeof( TInt4 ) ;
   tbuff   = (TInt4 *) malloc( buflen ) ;
   err_nef = Getelt( deffds , "MAPTBAG" , elmnam ,
                     uindex , usrord , &buflen   , tbuff  ) ;
   if ( err_nef != 0 )
   {
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *ierror = IEINFO ;
      return;
   }

/* Get reference date and time
*/
   MorGetT0( deffds , datfds , &t0 , &dt0 ) ;

/* Compare times to wanted list and copy wanted times
*/
   for ( stap=0, offst=0;
         (stap < dims[0]) && (offst < *maxlst) ;
          stap++)
   {
/* Convert times to julian days first
*/
      newtime = (double) tbuff[stap] * dt0 ;
      juldat = t0 + newtime;

/* Check if all steps are wanted
*/
       if ( timdef[0] <= 0.0 && timdef[1] >= ALLSTEPS )
       { /* just copy step to outlist */
           timlst[offst] = juldat;
           offst++ ;
           continue;
       }

/* Compare against wanted list
   HACK: ignore maxdef */
/*
       for ( i = 0 ; i < 1 ; i ++ )
       {
*/
       if ( juldat >= timdef[0] && juldat <= timdef[1] )
       {
          timlst[offst] = juldat;
          offst++ ;
          /* break; */
       }
       /* assuming climbing wanted list entries */
       /*  if (*curdef > juldat) */
       /*      break; */
/*     } */
   }
   *nrtim = offst ;

   free( tbuff ) ;

   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return;
}

/* -------------------------------------------------------------------
   Function: MorOdsGetMat()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3     I   Full filename, including extension
   long    *ftype      1     I   Filetype, see types.inc for definitions
   long    *parcod     1     I   Parameter code
   TReal8  *tim        3     I   For which times
   long    *loc       3*3    I   For which locations (matrix)
   TReal4  *misval     1     I   Missing value
   long    *maxdim     1     I   Maximum number of vaues to be stored
   TReal4  *values   maxdim  O   List of values
   long    *ierror     1     O   Error code
   char    *option     *     I   Dummy argument

--------------------------------------------------------------------- */

#ifndef NOPROT
   void MorOdsGetMat ( TString fname   , TInt4  *ftype  , TInt4  *parcod ,
                       TReal8  *tim    , TInt4  *loc    , TReal4 *misval ,
                       TInt4   *maxdim , TReal4 *values , TInt4  *ierror ,
                       TString option                                    )
#else
   void MorOdsGetMat ( fname  , ftype  , parcod , tim    ,
                       loc    , misval , maxdim , values ,
                       ierror , option                   )

   TString fname   , option  ;
   TInt4   *ftype  , *parcod , *loc , *maxdim , *ierror ;
   TReal8  *tim    ;
   TReal4  *misval , *values ;
#endif

{
   char    grpnam[20];
   char    celnam[20];
   char    elmnam[20];
   char   *deffn     ;
   long    stap      ;
   long    cur_step  ;
   long    offst     ;
   TInt4  *tbuff     ;
   TReal4 *rbuff1    ;
   TReal4 *rbuff2    ;
   TInt4   get_data  ;
   TInt4   i , j     ;
   TInt4   n1 , n2   ;
   TInt4   ndims   , nmax , mmax ;
   BInt4   err_nef ;
   BInt4   elmndm, elmdms[5];
   BInt4   nbytsg, buflen, dim1;
   BInt4   uindex[1][3], deffds[2997], datfds[999], dims[5], usrord[5];
   TReal8  curtim , juldat  , dt0 ;
   TReal8  t0     , newtime       ;

/* Open the file
*/
   *ierror = IEOK;

   MorOpenNefisFiles (fname, *ftype, datfds, deffds, ierror ) ;
   if ( *ierror != IEOK )
       return;

/* Get the number of timesteps
*/
   ndims = 5 ;
   err_nef = Inqgrp( deffds , "MAPTBAG" , celnam , &ndims ,
                     dims   , usrord                      ) ;
   if ( err_nef != 0 )
   {
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *ierror = IEINFO ;
      return;
   }
   if ( dims[0] == 0 )
   {
       err_nef = Inqmxi( deffds , "MAPTBAG" , &dim1 ) ;
       if ( err_nef != 0 )
       {
          CloseNefisFiles( fname , datfds , deffds , ierror ) ;
          *ierror = IEINFO ;
          return ;
       }
       dims[0] = dim1 ;
   }

/* Get the times
*/
   strcpy( elmnam , "T-BAG" ) ;
   usrord[0]    = 1;
   uindex[0][0] = 1; /* start,stop,step */
   uindex[0][1] = dims[0];
   uindex[0][2] = 1;
   buflen = dims[0] * sizeof( TInt4  ) ;
   tbuff  = (TInt4 *) malloc( buflen ) ;
   err_nef = Getelt( deffds , "MAPTBAG" , elmnam ,
                     uindex , usrord , &buflen   , tbuff  ) ;
   if ( err_nef != 0 )
   {
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *ierror = IEINFO ;
      return;
   }

/* Get array dimensions
*/
   uindex[0][0] = 1;   /* start */
   uindex[0][1] = 1;   /* stop */
   uindex[0][2] = 1;   /* step */
   usrord[0]    = 1;
   buflen       = sizeof (TInt4);
   strcpy( grpnam, "GRID" );
   strcpy( elmnam, "NMAX" );
   *ierror = Getelt( deffds , grpnam  , elmnam ,
                     uindex , usrord , &buflen , &nmax  ) ;
   if ( *ierror != IEOK )
   {
       /* close NEFIS files */
       CloseNefisFiles( fname , datfds , deffds , ierror ) ;
       return;
   }

   strcpy (elmnam, "MMAX");
   *ierror = Getelt( deffds , grpnam  , elmnam ,
                     uindex , usrord , &buflen , &mmax  ) ;
   if ( *ierror != IEOK )
   {
       /* close NEFIS files */
       CloseNefisFiles( fname , datfds , deffds , ierror ) ;
      return;
   }

/* Allocate the buffers
*/
   buflen =  nmax * mmax * sizeof( TReal4 ) ;
   rbuff1 = (TReal4 *) malloc( buflen );

/* Combined parameter
*/
   rbuff2 = NULL ;
   if ( *parcod == 3 )
   {
      rbuff2 = (TReal4 *) malloc( buflen );
   }

/* Get reference date and time
*/
   MorGetT0( deffds , datfds , &t0 , &dt0 ) ;

/* If the paramaeter code is negative, we do not have to bother
   with time. Simulate this situation.
*/
   if ( *parcod < 0 )
   {
      dims[0]  = 1      ;
      t0       = tim[0] ;
      tbuff[0] = 0.0    ;
   }

/* Compare times to wanted list and copy wanted times
*/
   cur_step = 0      ;
   curtim   = tim[0] ;
   offst    = 0      ;

   for ( stap=0 ;
         (stap < dims[0]) && (offst < *maxdim) ;
          stap++)
   {
/* Convert times to julian days first
*/
      newtime  = (double) tbuff[stap] * dt0 ;
      juldat   = t0 + newtime               ;
      get_data = 0                          ;

/* Check if all steps are wanted
*/
      if ( tim[0] <= 0.0 && tim[1] >= ALLSTEPS )
      {
         /* We need this time! */
         get_data = 1 ;
      }
      else
      {

/* Variable curtim is as close to the time in the file as possible
*/
         while ( curtim < juldat && curtim < tim[1] )
         {
            cur_step ++ ;
            curtim   = tim[0] + cur_step * tim[2] ;
         }

         if ( juldat >= curtim-1.0e-6 && juldat <= tim[1]+1.0e-6 )
         {
            get_data = 1 ;
         }
      }

/* Get the data - if wanted
*/
      if ( get_data == 0 )
      {
         continue ;
      }

      switch ( *parcod )
      {
         case (-1) : /* X-coordinate */
            strcpy( grpnam , "GRID" ) ;
            strcpy( elmnam , "XCOR" ) ;
            break ;

         case (-2) : /* Y-coordinate */
            strcpy( grpnam , "GRID" ) ;
            strcpy( elmnam , "YCOR" ) ;
            break ;

         case (-3) : /* DPBREF - reference dredging depth */
            strcpy( grpnam , "MAPBGREF" ) ;
            strcpy( elmnam , "DPBREF"  ) ;
            break ;

         case   1  : /* DDPBAG - amount dregded (m) */
            strcpy( grpnam , "MAPTBAG" ) ;
            strcpy( elmnam , "DDPBAG"  ) ;
            break ;

         case   2  : /* DP     - total depth (after dredging) */
            strcpy( grpnam , "MAPTBAG" ) ;
            strcpy( elmnam , "DP"      ) ;
            break ;

         case   3  : /* DP-DDPBAG - depth without dredging) */
            strcpy( grpnam , "MAPTBAG" ) ;
            strcpy( elmnam , "DDPBAG"  ) ;
            uindex[0][0] = stap + 1 ;
            uindex[0][1] = stap + 1 ;
            uindex[0][2] = 1        ;
            usrord[0]    = 1        ;
            err_nef = Getelt( deffds , grpnam , elmnam , uindex ,
                              usrord , &buflen , rbuff2                   ) ;
            strcpy( elmnam , "DP"  ) ;
            break ;
      }

/* Get the data and put them in the array
*/
      uindex[0][0] = stap + 1 ;
      uindex[0][1] = stap + 1 ;
      uindex[0][2] = 1        ;
      usrord[0]    = 1        ;
      err_nef = Getelt( deffds , grpnam , elmnam , uindex ,
                        usrord , &buflen , rbuff1                   ) ;

      for ( j = 0 ; j < nmax ; j ++ )
      {
         for ( i = 0 ; i < mmax ; i ++ )
         {
            n1 = j * mmax + i             ;
            n2 = i * nmax + j             ;
            values[offst+n1] = rbuff1[n2] ;
         }
      }

      if ( *parcod == 3 )
      {
         for ( j = 0 ; j < nmax ; j ++ )
         {
            for ( i = 0 ; i < mmax ; i ++ )
            {
               n1 = j * mmax + i ;
               n2 = i * nmax + j ;
               values[offst+n1] = values[offst+n1] - rbuff2[n2] ;
            }
         }
      }

/* Increase the offset in the values array for the next timestep
*/
      offst = offst + nmax * mmax ;
   }

/* Free the allocated space
*/
   free( tbuff  ) ;
   free( rbuff1 ) ;
   if ( rbuff2 != NULL ) free( rbuff2 ) ;

   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return;
}

/* -------------------------------------------------------------------
   Function: MorOdsGetGrd()

   Arguments:
   Type     Name      Size  I/O  Description
   ------   --------  ---   ---  ------------------------------------
   char    *fname      3     I   Full filename, including extension
   long    *ftype      1     I   Filetype, see types.inc for definitions
   long    *indloc    3*3    I   Location array (ignored!)
   long    *indx       *     O   Index array
   long    *nocell    3*3    O   For which locations (matrix)
   long    *igisty     1     O   Type of geometric information
   long    *ierror     1     O   Error code

--------------------------------------------------------------------- */

void MorOdsGetGrd( char    *fname,
                   TInt4   *ftype,
                   TInt4   *indloc,
                   TInt4   *indx,
                   TInt4   *nocell,
                   TInt4   *igisty,
                   TInt4   *ierror)

{
   TInt4   i, j, l ;
   TInt4   *ibuffs ;

   BInt4   deffds[2997], datfds[999], uindex[1][3],elmdms[5];
   BInt4   usrord, buflen, nbytsg, elmndm;
   TInt4   nmax, mmax ;
   TInt4   ibuffersize ;
   char    grp_par_nam[17], elmnam[17];
   char    elmtyp[10], elmqty[16], elmunt[16], elmdes[64];
   TInt4   dummy;

   *ierror = IEOK ;
   *igisty = IGCURV ;
   *nocell = 0 ;

   MorOpenNefisFiles( fname , *ftype , datfds , deffds , ierror ) ;
   if ( *ierror != IEOK )
       return;

/* Get array dimensions
*/
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "GRID" );
    strcpy( elmnam, "NMAX" );
    *ierror = Getelt( deffds, grp_par_nam, elmnam,
                      uindex, &usrord, &buflen, &nmax );
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    strcpy (elmnam, "MMAX");
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
                   uindex, &usrord, &buflen, &mmax );
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

/* Calculate buffer size ibuffs
*/
    ibuffersize =  nmax * mmax ;
    if (ibuffersize > 0)
    {
       ibuffs = (TInt4 *) malloc( ibuffersize * sizeof(TInt4) );
       if ( ibuffs == NULL )
       {
            *ierror = IEDISK;
       }
    }
    else
    {
        *ierror = IEINFO;
    }

/* Get mask array for bottom points (!) as all parameters
   are defined on bottom points:
      CODW = 0 inactive point
           = 1 active point
           = 2 open boundary point
*/
    strcpy (elmnam, "CODW");
    strcpy( grp_par_nam, "TEMPOUT" );
    buflen = sizeof (TInt4) * mmax * nmax ;
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
                      uindex, &usrord, &buflen, ibuffs) ;
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    *nocell = mmax * nmax ;

    for ( i=0 ; i<nmax ; i++ )
    {
       for ( j=0 ; j<mmax ; j++ )
       {
           l =  i * mmax + j ;
           switch ( ibuffs[j*nmax+i] )
           {
           case -1L :
               indx[l] = 0 ; /* inactive point */
               break;
           case 0L :
               indx[l] = 0 ; /* -(l+1) */ ; /* inactive point */

               break ;

           case 1L :
                 /* Note: counting starts at 1 as in FORTRAN */
               indx[l] = l + 1 ; /* active computational point */

               break ;

           case 2L :
               indx[l] = -1 ; /* open boundary point */

               break ;

           default :
               indx[l] = 0 ; /* inactive point */

               break ;

           }
       } /* end for j */
    } /* end for i */

    free (ibuffs) ;

    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );

    return;
}
