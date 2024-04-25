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
 *  tridro.c  -  ODS: TRISULA drogues files (functions)
 *
 *  Derived from code by Eric Verschuur
 *  Arjen Markus
 */

/*
 *  $Author: Markus $
 *  $Date: 28-06-04 9:19 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/tridro.c,v $
*/
/*
 *
 */

#include <string.h>
#include <stdio.h>
#include <float.h>
#include <math.h>

#include "portable.h"
#include "ods.h"
#include "odsmodel.h"

#ifndef NOPROT
   static TVoid TriDroGetRefDate( TInt4  ftype   , TInt4  *deffds   ,
                                  TInt4  *datfds , TReal8 *ref_date ,
                                  TReal8 *dt0                       ) ;
#else
   static TVoid TriDroGetRefDate( ftype   , deffds   ,
                                  datfds  , ref_date ,
                                  dt0                )
#endif

/*----------------------------------------------------------------------
 * The routine strrstr() is not part of the ANSI standard, so use
 * one of our own instead. This may be slower, but it is portable.
 *---------------------------------------------------------------------- */
static TString ReverseFindString( char *string , char *substr )
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

/*----------------------------------------------------------------------
 * Open TRISULA drogues (NEFIS) files and check content
 *----------------------------------------------------------------------
 */
#ifndef NOPROT
   static void TriDroOpenFiles ( TString fname,   TInt4 ftype,
                                 BInt4 *datfds,   BInt4 *deffds,
                                 TInt4 *ierror)
#else
   static void TriDroOpenFiles ( fname,      ftype,      datfds,      deffds,
                                 ierror)

   BInt4 *datfds, *deffds;
   TInt4 *ierror, ftype;
#endif
{
   char  *pstr ;
   char  grp_par_def[17];
   char  grp_res_def[17];
   char  celnam[17];
   int   length ;
   BInt4 err_nef ;
   char  datext_up[5] , datext_lw[5] ;
   char  defext[5] ;
   TInt4  maxdim , dims[5] , ord ;

   *ierror = IEOK;

   /* construct the appropriate second filename -
      assumption: it is never given
   */
   strcpy( fname+ODS_FILNAMLEN , fname ) ;
   switch ( ftype )
   {
      case ITNFTD : /* Fall through */
      case ITDPNT :
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
      }
   }
   strcpy( pstr , defext ) ;

   /* open files */
   OPNNEF( fname, &ftype, datfds, deffds, ierror ) ;
   if ( *ierror != 0 )
   {
      return ;
   }
   else
   {
      /* check the existence of the groups "dro-const" and "dro-series" */
      if ( ftype == ITNFTD )
      {
         strcpy( grp_par_def , "dro-const" ) ;
         strcpy( grp_res_def , "dro-series" ) ;
      }
      else
      {
         strcpy( grp_par_def , "trk-const" ) ;
         strcpy( grp_res_def , "trk-series" ) ;
      }
      maxdim  = 5 ;
      err_nef = Inqgrp (deffds, grp_par_def, celnam, &maxdim,
                        dims, &ord);
      if ( err_nef == 0 )
      {
         maxdim  = 5 ;
         err_nef = Inqgrp (deffds, grp_res_def, celnam, &maxdim,
                           dims, &ord);
      }
      /* Close the files if an error occurred */
      if ( err_nef != 0 )
      {
         *ierror = IEUNKN ;
         CloseNefisFiles ( fname, datfds, deffds, ierror);
      }
   }

   return;
}


/*---------------------------------------------------------------------
 * Function to get a list of times from a TRISULA drogues file
 *---------------------------------------------------------------------
 */
#ifndef NOPROT
   void TriDroGetTme ( TString fname,   TInt4  *ftype,  TReal8 *timdef,
                       TInt4   *maxdef, TInt4  *pardep, TInt4  *locdep,
                       TInt4   *maxlst, TReal8 *timlst, TInt4  *timtyp,
                       TInt4   *nrtim,  TInt4 *ierror,  TString option)
#else
   void TriDroGetTme ( fname,  ftype,  timdef, maxdef, pardep, locdep,
                       maxlst, timlst, timtyp, nrtim,  ierror, option)

   TString fname,   option;
   TInt4   *ftype,  *maxdef, *pardep, *locdep, *maxlst, *timtyp,
           *nrtim,  *ierror;
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
   char   grp_res_nam[17];
   char   elmnam[17];
   char   celnam[17];
   long  *buffer, offst;
   TInt4  itim ;
   BInt4  err_nef ;
   BInt4  maxdim, usrord, buflen;
   BInt4  uindex[1][3], deffds[2997], datfds[999], dims[5], ord[5];
   double juldat , dt0 ;
   double ref_date, newtime;

   *ierror = IEOK;
   ref_date = 0.0;

   TriDroOpenFiles (fname, *ftype, datfds, deffds, ierror);
   if ( *ierror != IEOK )
       return;

   /* Get reference date and time
   */
   TriDroGetRefDate( *ftype , deffds , datfds , &ref_date , &dt0 ) ;


   /* Get number of timesteps */
   dims[0] = 0 ;
   if ( *ftype == ITNFTD )
   {
      strcpy( grp_res_nam , "dro-info-series" ) ;
   }
   else
   {
      strcpy( grp_res_nam , "trk-info-series" ) ;
   }
   maxdim  = 5 ;
   err_nef = Inqgrp( deffds, grp_res_nam, celnam, &maxdim,
                     dims, ord ) ;
   if ( dims[0] == 0 )
   {
      err_nef = Inqmxi( deffds, grp_res_nam, &dims[0] ) ;
   }
   if ( err_nef != 0 )
   {
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *ierror = IEINFO ;
      return;
   }

   /* Get times */
   if ( *ftype == ITNFTD )
   {
      strcpy (elmnam, "ITDROC");
   }
   else
   {
      strcpy (elmnam, "ITTRKC");
   }
   usrord = 1;
   uindex[0][0] = 1; /* start,stop,step */
   uindex[0][1] = dims[0];
   uindex[0][2] = 1;
   buflen = dims[0] * sizeof(long);
   buffer = (long *) malloc(buflen);
   err_nef = Getelt (deffds, grp_res_nam, elmnam,
                     uindex, &usrord, &buflen, buffer );
   if ( err_nef != 0 )
   {
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *ierror = IEINFO ;
      return;
   }

   /* Compare times to wanted list and copy wanted times */
   offst = 0 ;
   for ( itim = 0 ; itim < dims[0] ; itim ++ )
   {
      /* Convert times to julian days first */
      newtime = (double) buffer[itim] * dt0 ;
      juldat = ref_date + newtime;

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

   free (buffer);

   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return;
}

/*--------------------------------------------------------------------
* Function to get a matrix from a TRISULA drogues file
*---------------------------------------------------------------------
*/

#ifndef NOPROT
   void TriDroGetMat ( TString fname,   TInt4  *ftype,   TInt4  *parcod,
                       TInt4   *loc,    TReal8 *tim,     TReal4 *misval,
                       TInt4   *i3gl,   TInt4  *maxdim,  TReal4 *values,
                       TInt4   *ierror, TString option)
#else
   void TriDroGetMat ( fname,  ftype,   parcod,   loc,      tim,
                       misval,  i3gl,   maxdim,   values,   ierror
                       option)

   TString fname,   option;
   TInt4   *ftype,  *parcod, *loc, *i3gl, *maxdim, *ierror;
   TReal8  *tim;
   TReal4  *misval, *values;
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
   char    grp_par_nam[17];
   char    grp_res_nam[17];
   char    celnam[17];
   char    elmnam[17];
   TInt4   itim , iloc2 ;
   TInt4   no_loc , no_times ;
   TInt4   *start_stop ;
   TInt4   *times;
   TInt4   offst, nefstart, nefstep , nefstop , i ;
   TInt4   start_loc, end_loc, step_loc, iloc ;
   TInt4   dro_start, dro_stop, dro_step ;
   TInt4   no_dim ;
   TInt4   ldum;
   BInt4   err_nef ;
   BInt4   maxd, usrord, buflen;
   BInt4   uindex[1][3], deffds[2997], datfds[999], dims[5], ord[5];
   float   *fdata , *coord ;
   double  juldat, julstep, outstep;
   double  ref_date, newtime , dt0 ;

   /* do some initialisation */
   *ierror = IEOK;
   if ( *ftype == ITNFTD )
   {
      strcpy( grp_par_nam , "dro-const" ) ;
      strcpy( grp_res_nam , "dro-info-series" ) ;
   }
   else
   {
      strcpy( grp_par_nam , "trk-const" ) ;
      strcpy( grp_res_nam , "trk-info-series" ) ;
   }

   TriDroOpenFiles (fname, *ftype, datfds, deffds, ierror);
   if ( *ierror != IEOK )
       return;

   /* set output array elements to misval */
   for (fdata = values; fdata < (values + *maxdim); fdata++)
        *fdata = *misval;

/* The dataset is always a simple timeseries. So no fuss about layers
   or anything. But: the actual data need not be defined over the
   whole interval. This is determined by the start and stop time for
   the drogues.
*/
   start_loc  = loc[0] ;
   end_loc    = loc[1] ;
   step_loc   = loc[2] ;

   outstep = tim[2];

   maxd = 0;

   /* Get nr of timesteps */
   dims[0] = 0 ;
   err_nef = Inqgrp (deffds, grp_res_nam, celnam, &maxd, dims, ord);
   if (dims[0] == 0)
   {
      err_nef = Inqmxi (deffds, grp_res_nam, &dims[0]);
   }
   if ( err_nef != 0 )
   {
      *ierror = IEINFO ;
      CloseNefisFiles ( fname, datfds, deffds, &ldum);
      return;
   }
   no_times = dims[0] ;

   /* Get the number of drogues (locations) */
   if ( *ftype == ITNFTD )
   {
      strcpy( elmnam, "NDRO" ) ;
   }
   else
   {
      strcpy( elmnam, "NTRK" ) ;
   }
   usrord = 1;
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   buflen  = sizeof (TInt4);
   err_nef = Getelt (deffds, grp_par_nam, elmnam,
                     uindex, &usrord, &buflen, &no_loc);
   if ( err_nef != 0 )
   {
       CloseNefisFiles ( fname, datfds, deffds, &ldum);
       *ierror = IEINFO ;
       return;
   }

   /* Get reference date and time */
   TriDroGetRefDate( *ftype , deffds , datfds , &ref_date , &dt0 ) ;

   julstep = dt0 ;

   /* if time stepsize == 0, set stepsize to stepsize in file */
   if ( fabs(outstep) <= DBL_EPSILON )
   {
      outstep = dt0 ;
   }

   /* Get the start and stop times per drogue */
   /* AM: for particle tracks this is dubious info ...*/
   if ( *ftype == ITNFTD )
   {
      strcpy( elmnam      , "NTDRO"     ) ;
   }
   else
   {
      strcpy( elmnam      , "NTTRK"     ) ;
   }

   usrord       = 1 ;
   uindex[0][0] = 1 ;
   uindex[0][1] = 1 ;
   uindex[0][2] = 1 ;
   buflen = 2 * no_loc * sizeof( TInt4 );
   start_stop = ( TInt4 *) malloc(buflen);
   err_nef = Getelt (deffds, grp_par_nam, elmnam,
               uindex, &usrord, &buflen, start_stop );
   if ( err_nef != 0 )
   {
       CloseNefisFiles ( fname, datfds, deffds, &ldum);
       *ierror = IEINFO ;
       free (start_stop);
       return;
   }

   /* For particle tracks start/stop times seem not relevant
   */
   if ( *ftype != ITNFTD )
   {
      for ( i = 0 ; i < no_loc ; i ++ )
      {
         start_stop[2*i]   =         -1 ;
         start_stop[2*i+1] = 1000000000 ;
      }
   }


   /* Get the timesteps */
   if ( *ftype == ITNFTD )
   {
      strcpy( elmnam      , "ITDROC"    ) ;
   }
   else
   {
      strcpy( elmnam      , "ITTRKC"    ) ;
   }
   usrord = 1 ;
   uindex[0][0] = 1       ; /* start */
   uindex[0][1] = dims[0] ; /* stop */
   uindex[0][2] = 1       ; /* step */
   buflen = dims[0] * sizeof( TInt4 );
   times = (TInt4 *) malloc(buflen);
   err_nef = Getelt( deffds, grp_res_nam, elmnam,
               uindex, &usrord, &buflen, times ) ;
   if ( err_nef != 0 )
   {
      CloseNefisFiles( fname, datfds, deffds, &ldum ) ;
      *ierror = IEINFO ;
      free( start_stop ) ;
      free( times ) ;
      return ;
   }

   /* Create a buffer for storing the coordinate information */
   /* Note: for particle track files, 3 coordinates are needed */
   if ( *ftype == ITNFTD )
   {
      no_dim = 2 ;
   }
   else
   {
      no_dim = 3 ;
   }
   buflen = no_dim * no_loc * sizeof( TReal4 ) ;
   coord  = (TReal4 *) malloc( buflen ) ;

   /* Find the required timesteps */
   nefstart = -1 ;
   nefstop  = -1 ;
   dro_start = (long) ( ( tim[0] - ref_date + 0.5 * dt0 ) / dt0 ) ;
   dro_stop  = (long) ( ( tim[1] - ref_date + 0.5 * dt0 ) / dt0 ) ;
   dro_step  = (long) ( tim[2] / dt0 ) ;

   for ( i = 0 ; i < no_times ; i ++ )
   {
      if ( times[i] >= dro_start && nefstart == -1 )
      {
         nefstart = i + 1 ;
      }
      if ( times[i] >= dro_stop  && nefstop  == -1 )
      {
         nefstop  = i + 1 ;
      }
   }
   if ( nefstart == -1 || nefstop == -1 )
   {
      *ierror = IEINFO ;
      return ;
   }
   nefstep = 1 ; /* (AM) HACK: always all data */

   /* We know which drogues (locations), we know the start and stop
      timesteps. Now get the actual data
   */
   offst = 0 ;
   if ( *ftype == ITNFTD )
   {
      strcpy( grp_res_nam , "dro-series" ) ;
      strcpy( elmnam      , "XYDRO"      ) ;
   }
   else
   {
      strcpy( grp_res_nam , "trk-series" ) ;
      strcpy( elmnam      , "XYZTRK"     ) ;
   }

   for ( itim = nefstart ; itim <= nefstop ; itim += nefstep )
   {
      uindex[0][0] = itim ;    /* start */
      uindex[0][1] = itim ;    /* stop */
      uindex[0][2] = 1    ;    /* step */
      err_nef = Getelt (deffds, grp_res_nam, elmnam,
                        uindex, &usrord, &buflen, coord );
      if ( err_nef != 0 )
      {
         CloseNefisFiles ( fname, datfds, deffds, &ldum);
         *ierror = IEINFO ;
         free (start_stop);
         free (times);
         free (coord);
         return;
      }

      /* Convert times to julian days first */
      newtime = (double) times[itim-1] * dt0 ;
      juldat  = ref_date + newtime ;

      /* if timestep is within wanted range copy data */
      if ( juldat >= tim[0]-1.0e-6 && juldat <= tim[1]+1.0e-6 )
      {
         /* Current timestep matches, get data for the "active"
            drogues
         */
         for ( iloc = start_loc ; (iloc <= end_loc) && (offst < *maxdim) ;
                iloc += step_loc )
         {
             iloc2 = no_dim * iloc ;
             if ( itim >= start_stop[2*iloc]   &&
                  itim <= start_stop[2*iloc+1]    )
             {
                values[offst] = coord[iloc2+(*parcod)];
             }
             offst++;
         }
      }
   }
   free (start_stop);
   free (times);
   free (coord);

   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return;
}

/*--------------------------------------------------------------------
* Function to get a list of locations from a TRISULA drogues file
*---------------------------------------------------------------------
*/
#ifndef NOPROT
   void TriDroGetLoc ( TString fname,   TInt4 *ftype,   TString locdef,
                       TInt4 *maxdef,   TInt4 *pardep,  TInt4   *timdep,
                       TInt4 *maxlst,   TString loclst, TInt4   *loctyp,
                       TInt4 *locnr,    TInt4 *nrlst,   TInt4   *ierror,
                       TString option)
#else
   void TriDroGetLoc ( fname,     ftype,      locdef,      maxdef,
                       pardep,    timdep,     maxlst,      loclst,
                       loctyp,    locnr,      nrlst,       ierror,
                       option)

   TString fname,  locdef,  loclst,  option;
   TInt4   *ftype, *maxdef, *pardep, *timdep, *maxlst, *loctyp,
           *locnr, *nrloc,  *ierror;
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
   char  grp_par_nam[17];
   char  elmnam[17];
   TInt4 i , offst;
   TInt4 loctel;
   TInt4 iloc , no_loc ;
   char  *cbuffer;
   BInt4 err_nef ;
   BInt4 usrord, buflen;
   BInt4 deffds[2997], datfds[999], uindex[1][3];

   *ierror = IEOK;

   TriDroOpenFiles (fname, *ftype, datfds, deffds, ierror);
   if ( *ierror != IEOK )
       return;

   /* Get number of locations */
   if ( *ftype == ITNFTD )
   {
      strcpy (grp_par_nam, "dro-const" );
      strcpy (elmnam, "NDRO");
   }
   else
   {
      strcpy (grp_par_nam, "trk-const" );
      strcpy (elmnam, "NTRK");
   }
   uindex[0][0] = 1; uindex[0][1] = 1; uindex[0][2] = 1; /* start,stop,step */
   usrord = 1;
   buflen = sizeof (long);
   err_nef = Getelt (deffds, grp_par_nam, elmnam,
              uindex, &usrord, &buflen, &no_loc );
   if ( err_nef != IEOK )
   {
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *ierror = IEINFO ;
      return;
   }

   /* Get location names */
   if ( *ftype == ITNFTD )
   {
      strcpy (elmnam, "NAMDRO");
   }
   else
   {
      strcpy (elmnam, "NAMTRK");
   }
   usrord = 1;
   uindex[0][0] = 1; /* start, stop, step */
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   buflen = no_loc * (PARLEN+1);
   cbuffer = (char *) malloc(buflen);
   err_nef = Getelt (deffds, grp_par_nam, elmnam,
              uindex, &usrord, &buflen, cbuffer );
   if ( err_nef != 0 )
   {
      free (cbuffer);
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *nrlst  = 0      ;
      *ierror = IEINFO ;
      return;
   }

   /* Split string buffer into location names */
   offst  = 0 ;
   loctel = 0 ;
   for ( iloc = 0 ; iloc < no_loc ; iloc ++ )
   {

/* Strings in file are 20 characters long and have trailing blanks.
   Remove these. Actual length of strings returned: PARLEN+1
*/
      strncpy ( loclst+offst , cbuffer + 20 * iloc , 20 ) ;
      loclst[offst+PARLEN] = '\0' ;

      for ( i = 19 ; i > 0 ; i -- )
      {
         if ( loclst[offst+i] == ' ' || loclst[offst+i] == '\t' )
         {
            loclst[offst+i] = '\0' ;
         }
         else
         {
            break ;
         }
      }
      locnr[iloc] = iloc ;
      offst += ( PARLEN + 1 ) ;
      if ( iloc >= *maxlst-1 )
      {
         break ;
      }
   }

   *nrlst = iloc + 1 ;

   free (cbuffer);

   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return;
}

/*--------------------------------------------------------------------
* Function to get a list of parameters from a TRISULA drogues file
*---------------------------------------------------------------------
*/
#ifndef NOPROT
   void TriDroGetPar ( TString fname,   TInt4 *ftype,   TString pardef,
                       TInt4   *maxdef, TInt4 *timdep,  TInt4   *locdep,
                       TInt4   *maxlst, TInt4 *lang,    TString parlst,
                       TString paruni,  TInt4 *partyp,  TInt4   *parcod,
                       TInt4   *nrlst,  TInt4 *ierror,  TString option)
#else
   void TriDroGetPar ( fname,     ftype,      pardef,      maxdef,
                       maxdef,    timdep,     locdep,      maxlst,
                       lang,      parlst,     paruni,      partyp,
                       parcod,    nrlst,      ierror,      option)

   TString fname, pardef, parlst, paruni, option;
   TInt4 *ftype, *maxdef, *maxlst, *lang, *partyp, *parcod, *nrlst, *ierror;
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
   BInt4 deffds[2997], datfds[999];

   *ierror = IEOK;

   TriDroOpenFiles (fname, *ftype, datfds, deffds, ierror);
   if ( *ierror != IEOK )
       return;

   /* Get number and names of substances - actually: fixed */
   strcpy( parlst          , "XCOORD" ) ;
   strcpy( parlst+PARLEN+1 , "YCOORD" ) ;
   strcpy( paruni          , "[m]"    ) ;
   strcpy( paruni+PARLEN+1 , "[m]"    ) ;
   parcod[0] = 0 ;
   parcod[1] = 1 ;
   partyp[0] = ODS_PT_LOC_DEP ;
   partyp[1] = ODS_PT_LOC_DEP ;
   *nrlst = 2 ;

   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return;
}

/*--------------------------------------------------------------------
* Function to get the dimensions from a TRISULA drogues file
*---------------------------------------------------------------------
*/

#ifndef NOPROT
   void TriDroGetDim ( TString fname,    TInt4 *ftype,   TString dim,
                       TInt4   *pardep,  TInt4 *timdep,  TInt4   *locdep,
                       TInt4   *ndim,    TInt4 *ierror,  TString option)
#else
   void TriDrogetDim ( fname,      ftype,   dim,     pardep,   timdep,
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
   char   grp_par_nam[17];
   char   grp_res_nam[17];
   char   elmnam[16];
   char   celnam[17];
   BInt4  maxdim, dims[5], ord[5] ;
   long  *buffer ;
   TInt4  dummy ;
   BInt4  err_nef ;
   BInt4  usrord, buflen;
   BInt4  uindex[1][3], deffds[2997], datfds[999];

   *ierror = IEOK;
   err_nef = 0 ;

   TriDroOpenFiles (fname, *ftype, datfds, deffds, ierror);
   if ( *ierror != IEOK )
       return;

   /* Get the various dimensions */
   switch (dim[0])
   {
      case 't':  /* tim */
      case 'T':
         if ( *ftype == ITNFTD )
         {
            strcpy( grp_res_nam , "dro-info-series" ) ;
            strcpy( celnam      , "ITDROC"          ) ;
         }
         else
         {
            strcpy( grp_res_nam , "trk-info-series" ) ;
            strcpy( celnam      , "ITTRKC"          ) ;
         }
         dims[0] = 0 ;
         maxdim  = 5 ;
         err_nef = Inqgrp (deffds, grp_res_nam, celnam, &maxdim,
                           dims, ord);
         if (dims[0] == 0)
         {
             err_nef = Inqmxi (deffds, grp_res_nam, &dims[0]);
         }
         ndim[0] = 1L;
         ndim[1] = dims[0];
         break;

      case 'p':  /* par */
      case 'P':
         ndim[0] = 1 ;
         ndim[1] = 2 ;
         break;

      case 'l':  /* loc */
      case 'L':
         if ( *ftype == ITNFTD )
         {
            strcpy( grp_res_nam , "dro-const" ) ;
            strcpy (elmnam, "NDRO");
         }
         else
         {
            strcpy( grp_res_nam , "trk-const" ) ;
            strcpy (elmnam, "NTRK");
         }
         uindex[0][0] = 1; uindex[0][1] = 1; uindex[0][2] = 1;
         usrord = 1;
         buflen = sizeof (long);
         err_nef = Getelt (deffds, grp_res_nam, elmnam, uindex,
                           &usrord, &buflen, &ndim[1]);
         ndim[0] = 1 ;
         break;

      default:
         *ierror = IEOTHR;
         break;
   } /* switch dim */

   if ( err_nef != 0 )
   {
      ndim[1] = 0 ;
      *ierror = IEINFO ;
   }

   CloseNefisFiles ( fname, datfds, deffds, &dummy);
   return;
}

/*--------------------------------------------------------------------
* Get reference date and time
*---------------------------------------------------------------------
*/
#ifndef NOPROT
    static TVoid TriDroGetRefDate( TInt4  ftype   , TInt4  *deffds   ,
                                   TInt4  *datfds , TReal8 *ref_date ,
                                   TReal8 *dt0                       )
#else
    static TVoid TriDroGetRefDate( ftype   , deffds   ,
                                   datfds  , ref_date ,
                                   dt0                )
    TInt4  ftype     ;
    TInt4  *deffds   ;
    TInt4  *datfds   ;
    TReal8 *ref_date ;
    TReal8 *dt0      ;
#endif
{
   TChar  grp_par_nam[17] ;
   TChar  elmnam[17]      ;
   TInt4  itdate[2] ;
   BInt4  usrord , uindex[5][3] , buflen , err_nef ;
   TInt4  iyear0 , imonth0 , iday0 , ihour0 , imin0 , isec0 , idt0 ;
   TReal4 deltt  , tunit   ;

/* Get ITDATE: date and time in the form of YYYYMMDD and HHMMSS
*/
   if ( ftype == ITNFTD )
   {
      strcpy( grp_par_nam , "dro-const" ) ;
      strcpy( elmnam      , "ITDATE"    ) ;
   }
   else
   {
      strcpy( grp_par_nam , "trk-const" ) ;
      strcpy( elmnam      , "ITDATE"    ) ;
   }

   usrord = 1;
   uindex[0][0] = 1; /* start,stop,step */
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   buflen = 2 * sizeof( TInt4 ) ;
   err_nef = Getelt (deffds, grp_par_nam, elmnam,
                     uindex, &usrord, &buflen, itdate );

   if ( err_nef == 0 )
   {
      strcpy( elmnam , "TUNIT" ) ;
      buflen = sizeof( TReal4 ) ;
      err_nef = Getelt (deffds, grp_par_nam, elmnam,
                        uindex, &usrord, &buflen, &tunit );

      if ( err_nef == 0 )
      {
         strcpy( elmnam , "DT" ) ;
         buflen = sizeof( TReal4 ) ;
         err_nef = Getelt (deffds, grp_par_nam, elmnam,
                           uindex, &usrord, &buflen, &deltt );
      }
   }

   if ( err_nef != 0 )
   {
      itdate[0] = 19000101 ; /* Default: 1 jan 1900 */
      itdate[1] =        0 ;
      tunit     =     60.0 ; /* From minutes to seconds */
      deltt     =      1.0 ; /* Time step by default 1 minute */
   }

/* Variable "dt0" is used to express the values in the file as
   julian time. The values in the file are timestep numbers.
*/
   *dt0      = tunit * deltt / 86400.0 ;

/* The reference date and time
*/
   iyear0  =   itdate[0] / 10000 ;
   imonth0 = ( itdate[0] - iyear0 * 10000 ) / 100 ;
   iday0   = ( itdate[0] - iyear0 * 10000 - imonth0 * 100 ) ;
   ihour0  =   itdate[1] / 10000 ;
   imin0   = ( itdate[1] - ihour0 * 10000 ) / 100 ;
   isec0   = ( itdate[1] - ihour0 * 10000 - imin0   * 100 ) ;
   idt0    =    1 ;

/* Now calculate the julian date and time - double check!
*/
   julian( &iyear0 , &imonth0 , &iday0 , &ihour0 , &imin0 , &isec0 ,
           ref_date );

    if ( *ref_date == -1.0 )
    {
       iyear0  = 1900 ;
       imonth0 =    1 ;
       iday0   =    1 ;
       ihour0  =    0 ;
       imin0   =    0 ;
       isec0   =    0 ;
       idt0    =    1 ;
       julian( &iyear0 , &imonth0 , &iday0 , &ihour0 , &imin0 , &isec0 ,
               ref_date );
    }

    return;
}
