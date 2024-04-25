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
 *  dlwnef.c  -  ODS: DELWAQ/DELPAR NEFIS files (functions)
 *
 *  Derived from code by Eric Verschuur
 *  Arjen Markus
 */

/*   Date:       29 Juni 1994                                         */
/*   Time:       15:55                                                */
/*   Program:    ODS2NEF.C                                            */
/*   Version:    1.00                                                 */
/*   Programmer: Eric Verschuur                                       */
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
 *  $Date: 5-04-04 9:43 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/dlwnef.c,v $
 */

#include <string.h>
#include <stdio.h>
#include <float.h>
#include <math.h>

#include "ods.h"
#include "odsmodel.h"

/* function prototypes */

static void GetDelparPloXYNef( TString fname   ,
                               TInt4   *datfds , TInt4 *deffds ,
                               TInt4   *loc    , TInt4 parcod  ,
                               TReal4  *values , TInt4 *ierror ) ;

TVoid DlwNefisGetT0( TInt4   ftype , TInt4 *deffds , TInt4  *datfds   ,
                     TString grp_par_nam           , TReal8 *dlw_date ,
                     TReal8 *dt0                                      );

long iftype ( char *fname) ;

#ifdef WIN32
#define cldfnf CLDFNF
#define cldtnf CLDTNF
TInt4 STDCALL cldfnf( BInt4 *deffds ) ;
TInt4 STDCALL cldtnf( BInt4 *datfds ) ;
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
 * Routine to identify the type of DELPAR file - old or new (this
 * has to do with the organisation of the data - 3D or not)
 * Currently:
 * The existence of an element "FILE-VERSION" is enough to identify
 * this.
 *---------------------------------------------------------------------- */
static TInt4 DelparNewFileType( BInt4 *deffds )
{
   BInt4  err_nef     ;
   char   elmtyp[17]  ;
   char   elmqty[17]  ;
   char   elmunit[17] ;
   char   elmdesc[65] ;
   BInt4  nbytsg      ;
   BInt4  elmndim     ;
   BInt4  elmdims[5]  ;

   err_nef = Inqelm( deffds,  "FILE-VERSION", elmtyp,
                     &nbytsg, elmqty, elmunit,
                     elmdesc, &elmndim, elmdims  ) ;

   return (err_nef == 0) ;
}

/*----------------------------------------------------------------------
 * open nefis files
 * Note (AM, september 2002):
 * In NEFIS 4, there is only one single integer that identifies both
 * the data file and the definition file. To keep the changes minimal,
 * I store this integer in the first element of datfds and deffds
 *----------------------------------------------------------------------
 */
#ifndef NOPROT
      void OpenNefisDefDat ( TString fname,  BInt4 *datfds,  BInt4 *deffds,
                             TInt4 *ierror)
#else
      void OpenNefisDefDat ( fname,      datfds,      deffds,
                             ierror)
      TString fname;
      BInt4 *datfds, *deffds;
      TInt4 *ierror;
#endif
{
   FILE *tfp;
   char  coding;
   char *deffn;
   int   baslen;
   char *ext;

   *ierror = IEOK;
   coding  = 'R' ; /* Read-only */

   /* first check if a def file name is supplied, if not check default name */
   deffn = &fname[ODS_FILNAMLEN];
   if (strlen (deffn) == 0)
   { /* no second file, assume .def file has datafile name + .def */
       ext = strrchr(fname, '.') ;
       if (ext == NULL)
           ext = strrchr(fname, '.') ;
       baslen = ext-fname;
       strncpy (deffn, fname, baslen);
       deffn[baslen] = 0 ;
       strcat (deffn, ".def");
   }
   else
   {
       if ( ReverseFindString(deffn, ".def") != NULL ||
            ReverseFindString(deffn,".DEF") != NULL )
       { /* it has a .def extension all right */
           ;
       }
       /* else it might still be a NEFIS def file, how to check ?? */
   }

   /* test if files exist */
   if (( tfp = fopen (deffn, "r")) != NULL )
      fclose (tfp);
   else
   {
      *ierror = IENOFI;
      return ;
   }

   if (( tfp = fopen (fname, "r")) != NULL )
      fclose (tfp);
   else
   {
      *ierror = IENOFI;
      return ;
   }

   /* open both data and definition files */
   *ierror = Crenef( (BInt4 *)deffds, fname, deffn, 'N', 'R' ) ;
   datfds[0] = deffds[0] ;

   return ;
}

/*----------------------------------------------------------------------
 * open nefis file and get groupnames
 *----------------------------------------------------------------------
 */
#ifndef NOPROT
   void DlwOpenNefisFiles ( TString fname,   TInt4 ftype,
                            BInt4 *datfds,   BInt4 *deffds,
                            TString grp_par, TString grp_res,
                            TInt4 *ierror)
#else
   void DlwOpenNefisFiles ( fname,      ftype,      datfds,      deffds,
                            grp_par,    grp_res,    ierror)

   TString fname, grp_par, grp_res;
   BInt4 *datfds, *deffds;
   TInt4 *ierror, ftype;
#endif
{
   char  *pstr ;
   char  grp_nam[17]    ;
   char  grp_nam_def[17];
   int   length ;
   BInt4 err_nef ;
   char  datext_up[5] , datext_lw[5] ;
   char  defext[5] ;

   *ierror = IEOK;

   /* construct the appropriate second filename - if it is not given */
   length = strlen( fname+ODS_FILNAMLEN ) ;
   if ( length == 0 )
   {
      strcpy( fname+ODS_FILNAMLEN , fname ) ;
      switch ( ftype )
      {
         case ITDWNM :
            strcpy( datext_up , ".ADA" ) ;
            strcpy( datext_lw , ".ada" ) ;
            strcpy( defext    , ".adf" ) ;
            break ;
         case ITDWNH :
            strcpy( datext_up , ".HDA" ) ;
            strcpy( datext_lw , ".hda" ) ;
            strcpy( defext    , ".hdf" ) ;
            break ;
         case ITDPNM :
         case ITDPNH :
         case ITDPNP :
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
   }

   /* open files */
   OPNNEF(fname, &ftype, datfds, deffds, ierror);
   if (*ierror != 0 )
       return;

   /* get group name of various groups in file "*_PARAMS" and "*_RESULTS" */

   strcpy( grp_par, "" ) ;
   strcpy( grp_res, "" ) ;

   err_nef = Inqfst((BInt4 *)datfds, grp_nam, grp_nam_def) ;

   while ( err_nef == 0 )
   {
       if ( strncmp( &grp_nam[6], "_PARAMS", 7 ) == 0 )
       {
          strcpy( grp_par, grp_nam ) ;
       }
       if ( strncmp( &grp_nam[6], "_RESULTS", 8 ) == 0 )
       {
          strcpy( grp_res, grp_nam ) ;
       }
       err_nef = Inqnxt((BInt4 *)datfds, grp_nam, grp_nam_def);
   }

   if ( strcmp( grp_par, "" ) == 0 || strcmp( grp_res, "" ) == 0 )
   {
      *ierror = IEINFO ;
       return;
   }
   return;
}

/*----------------------------------------------------------------------
 * close nefis file
 *----------------------------------------------------------------------
 */
#ifndef NOPROT
   void CloseNefisFiles ( TString fname,   BInt4 *datfds,  BInt4 *deffds,
                          TInt4 *ierror)
#else
   void CloseNefisFiles ( fname,      datfds,      deffds,      ierror)

   TString fname;
   BInt4 *datfds, *deffds;
   TInt4 *ierror;
#endif
{
   CLOSFL(fname, ierror);
   return;
}

/*----------------------------------------------------------------------
 * close nefis file
 *----------------------------------------------------------------------
 */
#ifndef NOPROT
   void CloseNefisDefDat ( TString fname,   BInt4 *datfds,  BInt4 *deffds,
                           TInt4 *ierror)
#else
   void CloseNefisDefDat ( fname,      datfds,      deffds,      ierror)

   TString fname;
   BInt4 *datfds, *deffds;
   TInt4 *ierror;
#endif
{
/* HACK: use the new close routines directly
*/
#if defined(USE_SUNOS) || defined(USE_IRIX) || defined(USE_LINUX)
#define cldtnf cldtnf_
#define cldfnf cldfnf_
#endif
   *ierror = cldtnf(datfds);
   *ierror = cldfnf(deffds);
   return;
}

/*---------------------------------------------------------------------
 * function to get a list of times from a DELWAQ/DELPAR Nefis file
 *---------------------------------------------------------------------
 */
#ifndef NOPROT
   void DlwGetNefisTme ( TString fname,  TInt4 *ftype,
                         TReal8 *timdef, TInt4 *maxdef, TReal8 *timlst,
                         TInt4 *maxlst,  TInt4 *nrtim,  TInt4 *ierror,
                         TString option)
#else
   void DlwGetNefisTme ( fname,     ftype,      timdef,     maxdef,
                         timlst,    maxlst,     nrtim,      ierror,
                         option)

   TString fname, option;
   TInt4 *maxdef, *maxlst, *nrtim, *ierror, *ftype;
   TReal8 *timdef, *timlst;
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
   char   grp_par_nam[17];
   char   grp_res_nam[17];
   char   elmnam[17];
   char   celnam[17];
   int    stap;
   long  *buffer, offst;
   BInt4  err_nef ;
   BInt4  maxdim, usrord, buflen, dim1;
   BInt4  uindex[1][3], deffds[2997], datfds[999], dims[5], ord[5];
   double juldat   , dt0     ;
   double dlw_date , newtime ;

   *ierror = IEOK;
   dlw_date = 0.0;

   DlwOpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam,
                      grp_res_nam, ierror);
   if ( *ierror != IEOK )
       return;

   /* DELWAQ/DELPAR .MAP or .HIS file type require these groups */
   if ( strncmp(grp_par_nam, "DELWAQ_PARAMS",13) != 0 &&
        strncmp(grp_par_nam, "DELPAR_PARAMS",13) != 0   )
   {
      *ierror = IEUNKN ;
      CloseNefisFiles ( fname, datfds, deffds, ierror);
   }

   /* get nr of timesteps */
   maxdim  = 5 ;
   err_nef = Inqgrp (deffds, grp_res_nam, celnam, &maxdim,
                     dims, ord);
   if ( err_nef != 0 )
   {
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *ierror = IEINFO ;
      return;
   }
   if (dims[0] == 0)
   {
       err_nef = Inqmxi (deffds, grp_res_nam, &dim1);
       if ( err_nef != 0 )
       {
          CloseNefisFiles ( fname, datfds, deffds, ierror);
          *ierror = IEINFO ;
          return;
       }
       dims[0] = dim1;
   }

   /* get times */
   strcpy (elmnam, "TIME");
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

   /* Get reference date and time
   */
   DlwNefisGetT0( *ftype , deffds , datfds , grp_par_nam ,
                  &dlw_date , &dt0 ) ;

   /* compare times to wanted list and copy wanted times */
   for (stap=0, offst=0;
        (stap < dims[0]) && (offst < *maxlst) ;
          stap++)
   {
      /* convert times to julian days first */
       newtime = (double) buffer[stap] * dt0 / 86400.0 ;
       juldat = dlw_date + newtime;

       /* check if all steps are wanted */
       if ( timdef[0] <= 0.0 && timdef[1] >= ALLSTEPS )
       { /* just copy step to outlist */
           timlst[offst] = juldat;
           offst++ ;
           continue;
       }

      /* compare against wanted list
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
   *nrtim = offst;

   free (buffer);

   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return;
}

/*--------------------------------------------------------------------
* function to get a matrix from a DELWAQ/DELPAR Nefis file
*---------------------------------------------------------------------
*/

#ifndef NOPROT
   void DlwGetNefisMat ( TString fname,   TInt4 *ftype,   TInt4 *parcod,
                         TReal8 *tim,   TInt4 *loc,     TReal4 *misval,
                         TInt4 *maxdim,  TReal4 *values, TInt4 *ierror,
                         TString option)
#else
   void DlwGetNefisMat ( fname,     ftype,      parcod,   tim,
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
   char    grp_par_nam[17];
   char    grp_res_nam[17];
   char    celnam[17];
   char    elmnam[17];
   int     stap;
   long    sizes[6] , noloc ;
   long   *times ;
   long    offst, nefstart, nefstep , nefstop , i ;
   long    start_loc, end_loc, step_loc, iloc, no_loc_lay ;
   long    dlw_start, dlw_stop, dlw_step ;
   TInt4   ldum;
   BInt4   err_nef ;
   BInt4   maxd, usrord, buflen, dim1;
   BInt4   uindex[1][3], deffds[2997], datfds[999], dims[5], ord[5];
   float  *fdata;
   double  juldat   , julstep , outstep ;
   double  dlw_date , newtime , dt0     ;

   /* do some initialisation */
   *ierror = IEOK;

   DlwOpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam,
                      grp_res_nam, ierror);
   if ( *ierror != IEOK )
       return;

   /* DELWAQ/DELPAR .MAP or .HIS file type require these groups */
   if ( strncmp(grp_par_nam, "DELWAQ_PARAMS",13) != 0 &&
        strncmp(grp_par_nam, "DELPAR_PARAMS",13) != 0   )
   {
      *ierror = IEUNKN ;
      CloseNefisFiles ( fname, datfds, deffds, ierror);
   }

   /* set output array elements to misval */
   for (fdata = values; fdata < (values + *maxdim); fdata++)
        *fdata = *misval;

   /* take care of the special parameters for DELPAR PLO files */
   if ( *ftype == ITDPNP && *parcod < 0 )
   {
      GetDelparPloXYNef( fname  , datfds , deffds , loc , *parcod ,
                         values , ierror ) ;
      CloseNefisFiles ( fname, datfds, deffds, &ldum );
      return ;
   }

 /* Was:
   start_loc = loc[0]-1;
   end_loc = loc[3]-1;
   step_loc = (loc[6] > 0 ? loc[6] : 1) ;
*/
/* The dataset can have layers, but second dimension is always 1
   Further assumption: all data in each required layer
*/
   no_loc_lay = loc[1] - loc[0] + 1 ;
   start_loc  = loc[0] + no_loc_lay * loc[6] ;
   end_loc    = loc[1] + no_loc_lay * loc[7] ;
   step_loc   = loc[2] ;

   outstep = tim[2];

   /* get nr of timesteps */
   maxd = 5 ;
   *ierror = Inqgrp (deffds, grp_res_nam, celnam, &maxd, dims, ord);
   if (dims[0] == 0)
   {
      *ierror = Inqmxi (deffds, grp_res_nam, &dim1);
      dims[0] = dim1;
   }
   if ( *ierror != IEOK )
   {
       CloseNefisFiles ( fname, datfds, deffds, &ldum);
       return;
   }

   /* get nr of locations */
   strcpy (elmnam, "SIZES");
   usrord = 1;
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   buflen = 6 * sizeof (long);
   err_nef = Getelt (deffds, grp_par_nam, elmnam,
               uindex, &usrord, &buflen, sizes);
   if ( err_nef != 0 )
   {
       CloseNefisFiles ( fname, datfds, deffds, &ldum);
       *ierror = IEINFO ;
       return;
   }

   /* Get reference date and time
   */
   DlwNefisGetT0( *ftype , deffds , datfds , grp_par_nam ,
                  &dlw_date , &dt0 ) ;

   julstep = 1.0 / 86400.0 ;

   /* if time stepsize == 0, set stepsize to stepsize in file */
   if (fabs(outstep) <= DBL_EPSILON)
       outstep = julstep;

   /* get times */
   /*
   ** start =  1 + (tim[0] - startdate - timoff[3]) / timoff[5]
   ** step = tim[2] / timoff[5];
   ** tim[0] == (first wanted timestep, julian),
   ** tim[2] == (interval between wanted steps, julian)
   ** timoff[3] == (1st timestep in delwaq file, seconds),
   ** timoff[5] == (stepsize in delwaq file, seconds)
   */
   /* (AM) I do not agree with this:
      nefstart = 1.1 + (tim[0] - dlw_date - (timoff[3]/86400.0))/julstep;
      nefstep = outstep / julstep;
      if (nefstep == 0L) nefstep = 1;
   */
   /* (AM) instead something simpler
   */
   nefstart = 1 ;
   nefstep  = 1 ;

   strcpy (elmnam, "TIME");
   usrord = 1;
   uindex[0][0] = nefstart; /* start */
   uindex[0][1] = dims[0];  /* stop */
   uindex[0][2] = nefstep;  /* step */
   buflen = dims[0] * sizeof(long);
   times = (long *) malloc(buflen);
   err_nef = Getelt (deffds, grp_res_nam, elmnam,
               uindex, &usrord, &buflen, times );
   if ( err_nef != 0 )
   {
       CloseNefisFiles ( fname, datfds, deffds, &ldum);
       *ierror = IEINFO ;
       free (times);
       return;
   }

   /* (AM) */
   nefstart = -1 ;
   nefstop  = -1 ;
   dlw_start = (long) ( ( tim[0] - dlw_date ) * 86400.0 + 0.5 ) ;
   dlw_stop  = (long) ( ( tim[1] - dlw_date ) * 86400.0 + 0.5 ) ;
   dlw_step  = (long) ( tim[2] * 86400.0 ) ;

   /* (AM) Find the required timesteps */
   for ( i = 0 ; i < dims[0] ; i ++ )
   {
      if ( times[i] >= dlw_start && nefstart == -1 )
      {
         nefstart = i + 1 ;
      }
      if ( times[i] >= dlw_stop  && nefstop  == -1 )
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

   /* get data */
   /* create substance name */
   sprintf (elmnam, "SUBST_%03ld", *parcod );
   usrord = 1;
   noloc  = max( sizes[1] , sizes[2] ) ;
   if ( *ftype == ITDPNP )
   {
      noloc = sizes[4] * sizes[5] * max( sizes[3], 1 ) ;
   }
   buflen = noloc * sizeof(float);
   fdata = (float *) malloc (buflen);

   /* compare times to wanted list and copy wanted data */
   offst = 0 ;
   for ( stap = nefstart ; stap <= nefstop  ; stap += nefstep )
   {
      uindex[0][0] = stap ;    /* start */
      uindex[0][1] = stap ;    /* stop */
      uindex[0][2] = 1    ;    /* step */
      err_nef = Getelt (deffds, grp_res_nam, elmnam,
                        uindex, &usrord, &buflen, fdata );
      if ( err_nef != 0 )
      {
         CloseNefisFiles ( fname, datfds, deffds, &ldum);
         *ierror = IEINFO ;
         free (times);
         free (fdata);
         return;
      }

      /* convert times to julian days first */
      newtime = (double) times[stap-1] * dt0 / 86400.0 ;
      juldat  = dlw_date + newtime;

      /* if timestep is within wanted range copy data */
#if 0
      if ( juldat >= tim[0] && juldat <= tim[1] )
      {
#else
      if ( 1 )
      {
#endif
         /* current timestep matches, get data */
         for ( iloc = start_loc ; (iloc <= end_loc) && (offst < *maxdim) ;
                iloc += step_loc )
         {
             values[offst] = fdata[iloc];
             offst++;
         }
      }
   }

/* For DELPAR PLO files: a zero means missing value
   Not anymore: ARS 6554
*/
#if 0
    if ( *ftype == ITDPNP )
    {
       for ( iloc = 0 ; iloc < *maxdim ; iloc ++ )
       {
          if ( values[iloc] == 0.0 )
          {
             values[iloc] = *misval ;
          }
       }
    }
#endif

   free (times);
   free (fdata);

   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return;
}

/*--------------------------------------------------------------------
* function to get a list of locations from a Nefis file
*---------------------------------------------------------------------
*/


#ifndef NOPROT
   void DlwGetNefisLoc ( TString fname,   TInt4 *ftype,   TString locdef,
                         TInt4 *maxdef,  TString loclst,  TInt4 *maxlst,
                         TInt4 *nrlst,   TInt4 *locnr,   TInt4 *ierror,
                         TString option)
#else
   void DlwGetNefisLoc ( fname,     ftype,      locdef,      maxdef,
                         loclst,    maxlst,     nrlst,       locnr,
                         ierror,    option)

   TString fname,  locdef, loclst, option;
   TInt4 *ftype,  *maxdef, *maxlst, *nrlst, *locnr,  *ierror;
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
   char  grp_res_nam[17];
   char  elmnam[17];
   char  *locn, *locd;
   char  location[PARLEN+1];
   int   offst;
   long  loctel;
   long *lbuffer;
   char *cbuffer;
   BInt4 err_nef ;
   BInt4 usrord, buflen;
   BInt4 deffds[2997], datfds[999], uindex[1][3];

   *ierror = IEOK;

   DlwOpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam,
                      grp_res_nam, ierror);
   if ( *ierror != IEOK )
       return;

   /* DELWAQ/DELPAR .MAP or .HIS file type require these groups */
   if ( strncmp(grp_par_nam, "DELWAQ_PARAMS",13) != 0 &&
        strncmp(grp_par_nam, "DELPAR_PARAMS",13) != 0   )
   {
      *ierror = IEUNKN ;
      CloseNefisFiles ( fname, datfds, deffds, ierror);
   }

   /* get number of locations */
   strcpy (elmnam, "SIZES");
   uindex[0][0] = 1; uindex[0][1] = 1; uindex[0][2] = 1; /* start,stop,step */
   usrord = 1;
   buflen = 6 * sizeof (long);
   lbuffer = (long *) malloc(buflen);
   err_nef = Getelt (deffds, grp_par_nam, elmnam,
              uindex, &usrord, &buflen, lbuffer );
   if ( err_nef != IEOK )
   {
      free (lbuffer);
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *ierror = IEINFO ;
      return;
   }

   /* get location names */
   strcpy (elmnam, "LOCATION_NAMES");
   usrord = 1;
   uindex[0][0] = 1; /* start, stop, step */
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   buflen = lbuffer[2] * (PARLEN+1);
   free (lbuffer);
   cbuffer = (char *) malloc(buflen);
   err_nef = Getelt (deffds, grp_par_nam, elmnam,
              uindex, &usrord, &buflen, cbuffer );
   if ( err_nef != 0 )
   {
      free (cbuffer);
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      *nrlst  = 0      ;
      *ierror = IEINFO ;
      if ( *ftype == ITDWNM || *ftype == ITDPNM )
      {
         *ierror = IEOK   ;
      }
      return;
   }

   offst = 0;
   /* split strings into location names */
   for (locn = cbuffer, loctel = 0 ;
       (locn < cbuffer+buflen) && (offst < *maxlst);
       locn +=PARLEN, loctel++ )
   {
      strncpy ( location, locn, (size_t) PARLEN);
      *(location + PARLEN) = '\0' ;

      /* remove any trailing blanks from substance name */
      /* for ( locd = location+PARLEN-1; */
      /*                locd>location && *locd==' '; locd--) */
      /*     *locd = '\0'; */

      /* check if substance name is on the wanted list,
      ** if so add it to parlst
      */
      for ( locd=locdef; locd<(locdef+(*maxdef*PARLEN)); locd+=PARLEN+1)
      {
         /* next test is true if first non-blank characters of location
         ** match against wanted list, or first wanted list entry is "*"
         ** and location is meaningful (first character is not hex 0 or FF)
         */
         if ((!strcmp(locd,"*") ||
                !strncmp(locd, location, strcspn(location," "))) &&
                   (location[0] != '\377' && location[0] != '\0') )
         {
            strcpy(loclst +( offst * (PARLEN+1)), location);
            locnr[offst] = loctel;
            offst++;
            break;
         }
      }
   }

   *nrlst = offst;

   free (cbuffer);

   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return;
}

/*--------------------------------------------------------------------
* function to get a list of parameters from a Nefis file
*---------------------------------------------------------------------
*/

#ifndef NOPROT
   void DlwGetNefisPar ( TString fname,   TInt4 *ftype,   TString pardef,
                         TInt4 *maxdef,  TString parlst,  TString paruni,
                         TInt4 *maxlst,  TInt4 *nrlst,   TInt4 *partyp,
                         TInt4 *parcod,  TInt4 *ierror,  TString option)
#else
   void DlwGetNefisPar ( fname,     ftype,      pardef,      maxdef,
                         parlst,    paruni,     maxlst,      nrlst,
                         partyp,    parcod,     ierror,      option)

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
   char  grp_par_nam[17];
   char  grp_res_nam[17];
   char  elmnam[17];
   char  *subnam, *unit, *subdef, *ip1, *ip2;
   char  substance[PARLEN+1], eenh[PARLEN+1];
   int   i, offst;
   long  subnr;
   long  nsub , nsub2 ;
   long *lbuffer;
   char *cbuffer;
   BInt4 err_nef ;
   BInt4 usrord, buflen;
   BInt4 deffds[2997], datfds[999], uindex[1][3];

   *ierror = IEOK;

   DlwOpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam,
                      grp_res_nam, ierror);
   if ( *ierror != IEOK )
       return;

   /* DELWAQ/DELPAR .MAP or .HIS file type require these groups */
   if ( strncmp(grp_par_nam, "DELWAQ_PARAMS",13) != 0 &&
        strncmp(grp_par_nam, "DELPAR_PARAMS",13) != 0   )
   {
      *ierror = IEUNKN ;
      CloseNefisFiles ( fname, datfds, deffds, ierror);
   }

    /* get number of substances */
    strcpy (elmnam, "SIZES");
    uindex[0][0] = 1; /* start */
    uindex[0][1] = 1; /* stop */
    uindex[0][2] = 1; /* step */
    usrord = 1;
    buflen = 6 * sizeof (long);
    lbuffer = (long *) malloc(buflen);
    err_nef = Getelt (deffds, grp_par_nam, elmnam,
               uindex, &usrord, &buflen, lbuffer );
    if ( err_nef != 0 )
    {
       CloseNefisFiles ( fname, datfds, deffds, ierror);
       free (lbuffer);
       *ierror = IEINFO ;
       return;
    }

    /* get substance names
       Two extra parameters for DELPAR PLO files
    */
    strcpy (elmnam, "SUBST_NAMES");
    uindex[0][0] = 1; uindex[0][1] = 1; uindex[0][2] = 1;
    nsub  = lbuffer[0] ;
    nsub2 = nsub       ;
    if ( *ftype == ITDPNP )
    {
       nsub2 = nsub + 2 ;
    }
    buflen = nsub2 * (PARLEN+1);
    free (lbuffer);
    cbuffer = (char *) malloc(buflen);
    err_nef = Getelt (deffds, grp_par_nam, elmnam,
               uindex, &usrord, &buflen, cbuffer );
    if ( err_nef != IEOK )
    {
       CloseNefisFiles ( fname, datfds, deffds, ierror);
       *ierror = IEINFO ;
       free (cbuffer);
       return;
    }

    if ( *ftype == ITDPNP )
    {
       strcpy( cbuffer +  nsub    * PARLEN , "XCOORD" ) ;
       strcpy( cbuffer + (nsub+1) * PARLEN , "YCOORD" ) ;
    }

   offst = 0;
   /* split strings into substance names and units */
   for (subnam = cbuffer, unit = paruni, subnr = 1 ;
        (subnam < cbuffer+buflen) && (offst < *maxlst);
        subnam +=PARLEN, unit +=PARLEN, subnr++ )
   {
       /* Assume DELWAQ/DELPAR names can have a unit between [] / * or ()  */

       ip1 = strchr ( subnam, (int) '[') ;
/*       if (ip1 == NULL) */
/*       { */
/*           ip1 = strchr ( subnam, (int) '(') ; */
/*           ip2 = strchr ( subnam, (int) ')') ; */
/*       } */
/*       else */
           ip2 = strchr ( subnam, (int) ']') ;

       if ( ( ip2 > ip1) && ( ip1 != NULL) && ( ip2 != NULL) &&
           (ip2 - subnam < PARLEN) )
       {
          strncpy ( substance, subnam, (size_t) (ip1-subnam)) ;
          strncpy ( eenh, ip1 + 1, (size_t) (ip2-ip1-1)) ;
          *(substance + (ip1-subnam)) = '\0' ;
          *(eenh + (ip2-ip1-1))  = '\0' ;
       }
       else
       {
          strncpy ( substance, subnam, (size_t) PARLEN);
          strncpy ( eenh, BLANK20, (size_t) PARLEN) ;
          *(substance + PARLEN) = '\0' ;
          *(eenh + PARLEN) = '\0' ;
       }
       /* remove any trailing blanks from substance name */
       /* for ( subdef = substance+PARLEN-1; */
       /*                subdef>substance && *subdef==' '; subdef--) */
       /*     *subdef = '\0'; */

       /* check if substance name is on the wanted list, */
       /* if so add it to parlst */
       for ( subdef=pardef; subdef<(pardef+(*maxdef*PARLEN)); subdef+=PARLEN+1)
       {
           if ( (!strcmp(subdef,"*") ||
                 !strncmp(subdef, substance, strcspn(substance," "))) &&
                     (substance[0] != '\377' && substance[0] != '\0') )
           {
              strcpy(parlst +( offst * (PARLEN+1)), substance);
              strcpy(paruni +(offst * (PARLEN+1)), eenh);
              parcod[offst] = subnr;
              offst++;
              break;
           }
       }
   }

   *nrlst = offst;

   /* for now return hard-wired parameter type codes */
   if ( *ftype == ITDWNH || *ftype == ITDPNH )
   {
      for (i=0; i<*nrlst; i++)
      {
          partyp[i] = ODS_PT_LOC_DEP ;
      }
   }
   else
   {
      for (i=0; i<*nrlst; i++)
      {
         if ( i < nsub )
         {
            partyp[i] = ODS_PT_TIME_DEP | ODS_PT_LOC_MNK ;
         }
         else
         {
            /* The extra parameters: special codes -2, -3 */
            partyp[i] = ODS_PT_LOC_MNK ;
            parcod[i] = nsub - i - 2 ;
         }
      }
   }

   free (cbuffer);
   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return;
}

/*--------------------------------------------------------------------
* get dimensions from a Nefis file
*---------------------------------------------------------------------
*/

#ifndef NOPROT
   void DlwGetNefisDim ( TString fname,   TInt4 *ftype,   TString dim,
                         TInt4 *pardep,  TInt4 *timdep,  TInt4 *locdep,
                         TInt4 *ndim,    TInt4 *ierror,  TString option)
#else
   void DlwGetNefisDim ( fname,      ftype,   dim,     pardep,   timdep,
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
   char   grp_par_nam[17]; /* "DELWAQ_PARAMS" bij delwaq .HIS of .MAP */
   char   grp_res_nam[17]; /* "DELWAQ_RESULTS" bij delwaq .HIS of .MAP */
   char   elmnam[16];
   char   celnam[17];
   BInt4  maxdim, dim1, dims[5], ord[5] ;
   long  *buffer;
   TInt4  dummy ;
   BInt4  err_nef ;
   BInt4  usrord, buflen;
   BInt4  uindex[1][3], deffds[2997], datfds[999];

   *ierror = IEOK;

   DlwOpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam,
                      grp_res_nam, ierror);
   if ( *ierror != IEOK )
       return;

   /* DELWAQ/DELPAR .MAP or .HIS file type require these groups */
   if ( strncmp(grp_par_nam, "DELWAQ_PARAMS",13) != 0 &&
        strncmp(grp_par_nam, "DELPAR_PARAMS",13) != 0   )
   {
      *ierror = IEUNKN ;
      CloseNefisFiles ( fname, datfds, deffds, ierror);
   }

   /* Get the various dimensions */
   switch (dim[0])
   {
      case 't':  /* tim */
      case 'T':
         maxdim = 5 ;
         err_nef = Inqgrp (deffds, grp_res_nam, celnam, &maxdim,
                           dims, ord);
         ndim[0] = 1L;
         ndim[1] = dims[0];
         if (dims[0] == 0)
         {
             err_nef = Inqmxi (deffds, grp_res_nam, &dim1);
             ndim[1] = dim1;
         }
         break;

      case 'p':  /* par */
      case 'P':
         strcpy (elmnam, "SIZES");
         uindex[0][0] = 1; uindex[0][1] = 1; uindex[0][2] = 1;
         usrord = 1;
         buflen = 6 * sizeof (long);
         buffer = (long *)malloc (buflen);
         err_nef = Getelt (deffds, grp_par_nam, elmnam, uindex,
                           &usrord, &buflen, buffer);
         ndim[0] = 1L ;
         ndim[1] = buffer[0]; /* nr of substances */
         if ( *ftype == ITDPNP )
         {
            ndim[1] = ndim[1] + 2 ;
         }
         free (buffer);
         break;

      case 'l':  /* loc */
      case 'L':
         strcpy (elmnam, "SIZES");
         uindex[0][0] = 1; uindex[0][1] = 1; uindex[0][2] = 1;
         usrord = 1;
         buflen = 6 * sizeof (long);
         buffer = (long *)malloc (buflen);
         err_nef = Getelt (deffds, grp_par_nam, elmnam, uindex,
                           &usrord, &buflen, buffer);
         ndim[0] = 1L ;
         if ( *ftype == ITDWNH || *ftype == ITDPNH )
         {
            ndim[1] = buffer[2]; /* nr of locations */
         }
         else
         {
            ndim[1] = buffer[1]; /* nr of segments */
         }
         /* Special handling for DELPAR PLO files
         */
         if ( *ftype == ITDPNP )
         {
            if ( *pardep >= 0 ) /* Ordinary parameters */
            {
               ndim[0] = 1 ;
               ndim[1] = buffer[4] * buffer[5] ;
               if ( DelparNewFileType( deffds ) )
               {
                  ndim[0] = 3 ;
                  ndim[1] = buffer[4] * buffer[5] ;
                  ndim[2] = 1 ;
                  ndim[3] = buffer[3] ;
               }

            }
            /* Note array elements are intentionally exchanged */
            if ( *pardep <= -2 ) /* x/y coordinates */
            {
               ndim[0] = 2 ;
               ndim[1] = buffer[5] + 2 ; /* x-direction */
               ndim[2] = buffer[4] + 2 ; /* y-direction */
            }
            if ( *pardep == -1 ) /* admin-array */
            {
               ndim[0] = 4 ;
               ndim[1] = buffer[5] + 2 ; /* x-direction */
               ndim[2] = buffer[4] + 2 ; /* y-direction */
               ndim[3] = 1             ; /* z-direction */
               ndim[4] = ndim[1]*ndim[2] ; /* size of admin array */
            }
         }
         free (buffer);
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
* translate Nefis errors into ODS errors
*---------------------------------------------------------------------
*/
#ifndef NOPROT
    void Nef2odsErr(TInt4 *ierror)
#else
    void Nef2odsErr(ierror)
    TInt4 *ierror;
#endif
{
    long error;

    error = -(*ierror);
    switch (error)
    {
        case 9051:
        case 25051:
        case 27012:
        case 28012:
            *ierror = IEBUFF;
            break;

        default:
            /* generalized messages */
            switch (error % 1000)
            {
                case 11:
                case 21:
                    *ierror = IEINFO;
                    break;

                case 31:
                case 41:
                default:
                    *ierror = IEOTHR;
                    break;
            }
            break;
    }
    return;
}

/*--------------------------------------------------------------------
* get time offset from run identification
*---------------------------------------------------------------------
*/
#ifndef NOPROT
    TVoid DlwNefisGetT0( TInt4   ftype , TInt4 *deffds , TInt4  *datfds   ,
                         TString grp_par_nam           , TReal8 *dlw_date ,
                         TReal8 *dt0                                      )
#else
    TVoid DlwNefisGetT0( ftype    , deffds , datfds , grp_par_nam ,
                         dlw_date , dt0                           )
    TInt4   ftype       ;
    TInt4   *deffds     ;
    TInt4   *datfds     ;
    TString grp_par_nam ;
    TReal8  *dlw_date   ;
    TReal8  *dt0        ;
#endif
{
    char  *runid ;
    char  elmnam[17] , title[161] ;
    BInt4 timoff[7] , usrord , uindex[5][3] , buflen , err_nef ;
    TInt4 iyear0 , imonth0 , iday0 , ihour0 , imin0 , isec0 , idt0 ;

/* Decide how to get the reference date and time:
   - DELPAR uses it explicitly, but consistently (ARS 3500)
   - DELWAQ only via the T0 convention (get the run ID)
   Always use T0!
*/
    *dlw_date = 0.0 ;
    *dt0      = 1.0 ;

    iyear0  = 1900 ;
    imonth0 =    1 ;
    iday0   =    1 ;
    ihour0  =    0 ;
    imin0   =    0 ;
    isec0   =    0 ;
    idt0    =    1 ;

#if 0
    /* Because of ARS 3500 */
    if ( ftype == ITDPNM || ftype == ITDPNH || ftype == ITDPNP )
    {
       strcpy (elmnam, "TIME_OFFSET");
       usrord = 1;
       uindex[0][0] = 1; /* start,stop,step */
       uindex[0][1] = 1;
       uindex[0][2] = 1;
       buflen = sizeof( timoff ) ;
       err_nef = Getelt (deffds, grp_par_nam, elmnam,
                         uindex, &usrord, &buflen, timoff );

       if ( err_nef == 0 && timoff[0] != 0L )
       {
          iyear0  = timoff[0]         ;
          imonth0 = timoff[1]         ;
          iday0   = timoff[2] / 86400 ;
          ihour0  = 0                 ;
          imin0   = 0                 ;
          isec0   = 0                 ;
       }
    }
#endif

/* Use a different method
*/
#if 0
    /* Because of ARS 3500 */
    if ( ftype == ITDWNM || ftype == ITDWNH )
    {
#endif
       strcpy (elmnam, "TITLE");
       usrord = 1;
       uindex[0][0] = 1; /* start,stop,step */
       uindex[0][1] = 1;
       uindex[0][2] = 1;
       buflen = sizeof( title ) ;
       err_nef = Getelt (deffds, grp_par_nam, elmnam,
                         uindex, &usrord, &buflen, title );
       title[160] = '\0' ;
       if ( err_nef != 0 )
       {
          strcpy( &title[3*40] ,
                    "                                       " ) ;
       }

/* Read DELWAQ header hoping to find a start time
   t0= 1993.12.31 12:59:59 (scu =12345678 )
   01234567890123456789012345678901234567890
             1         2         3         4
*/
       runid       = title + 3*40 ;
       runid [  8] = '\0' ;
       runid [ 11] = '\0' ;
       runid [ 14] = '\0' ;
       runid [ 17] = '\0' ;
       runid [ 20] = '\0' ;
       runid [ 23] = '\0' ;
       runid [ 38] = '\0' ;
       iyear0  = atol ( runid + 4) ;
       imonth0 = atol ( runid + 9) ;
       iday0   = atol ( runid +12) ;
       ihour0  = atol ( runid +15) ;
       imin0   = atol ( runid +18) ;
       isec0   = atol ( runid +21) ;
       idt0    = atol ( runid +30) ;
#if 0
    }
#endif

/* Now calculate the julian date and time - double check!
*/
    julian( &iyear0 , &imonth0 , &iday0 , &ihour0 , &imin0 , &isec0 ,
            dlw_date );

    if ( *dlw_date == -1.0 )
    {
       iyear0  = 1900 ;
       imonth0 =    1 ;
       iday0   =    1 ;
       ihour0  =    0 ;
       imin0   =    0 ;
       isec0   =    0 ;
       idt0    =    1 ;
       julian( &iyear0 , &imonth0 , &iday0 , &ihour0 , &imin0 , &isec0 ,
               dlw_date );
    }

/* Incomplete T0-strings will probably give a time scale factor of 0
*/
    if ( idt0 <= 0 )
    {
       idt0 = 1 ;
    }

/* Return the values
*/
    *dt0 = (double) idt0 ;

    return;
}

/*------------------------------------------------------------------------
 * subroutine for generating the coordinates for DELPAR PLO files
 *------------------------------------------------------------------------
 */
static void GetDelparPloXYNef( TString fname ,
                               TInt4   *datfds,
                               TInt4   *deffds,
                               TInt4   *loc,
                               TInt4   parcod ,
                               TReal4  *values ,
                               TInt4   *ierror )
{
    TReal4  xywin[4] ;
    TInt4   sizes[6] , i , j  , ij  , dummy , buflen ,
            usrord , uindex[1][3] , nseg[3] ;
    TInt4   err_nef ;

    *ierror = IEOK;

/* Get the sizes of the plot grid
*/
   usrord = 1;
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   buflen = 6 * sizeof ( TInt4 );
   err_nef = Getelt (deffds, "DELPAR_PARAMS" , "SIZES" ,
               uindex, &usrord, &buflen, sizes );
   if ( err_nef != 0 )
   {
       CloseNefisFiles ( fname, datfds, deffds, &dummy );
       *ierror = IEINFO ;
       return;
   }
   nseg[1] = sizes[5] ; /* Order is intentionally reversed */
   nseg[2] = sizes[4] ;

/* Get the extremes of the plot window
*/
   usrord = 1;
   uindex[0][0] = 1;
   uindex[0][1] = 1;
   uindex[0][2] = 1;
   buflen = 4 * sizeof ( TReal4 );
   err_nef = Getelt (deffds, "DELPAR_PARAMS" , "PLOT_WINDOW" ,
               uindex, &usrord, &buflen, xywin );
   if ( err_nef != 0 )
   {
       CloseNefisFiles ( fname, datfds, deffds, &dummy );
       *ierror = IEINFO ;
       return;
   }
/* Now calculate the coordinates

   Note (AM, dd 1 april 1999):
   Robert Vos noted that the coordinates were not properly
   calculated. There was an offset of -1.
*/
    xywin[1] = ( xywin[1] - xywin[0] ) / nseg[1] ;
    xywin[3] = ( xywin[3] - xywin[2] ) / nseg[2] ;
    ij = 0 ;
    for ( j = 0 ; j < nseg[2]+2 ; j ++ )
    {
       for ( i = 0 ; i < nseg[1]+2 ; i ++ )
       {
          if ( parcod == -2 ) /* x-coordinate */
          {
             values[ij] = xywin[0] + i * xywin[1] ;
          }
          else                /* y-coordinate */
          {
             values[ij] = xywin[2] + j * xywin[3] ;
          }
          ij ++ ;
       }
    }

    return ;
}


/*------------------------------------------------------------------------ */
/*    SUBROUTINE Get Grid from DeLPar Nefis Plo file                       */
/*------------------------------------------------------------------------ */

void ODSGetGrdDlprNef( char    *fname,
                       TInt4   *ftype,
                       TInt4   *indloc,
                       TInt4   *indx,
                       TInt4   *nocell,
                       TInt4   *igisty,
                       TInt4   *ierror)

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   3          I   Full filename, including extension.      */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       igisty  -          O   Type of geographic information           */
/*       indloc  -          I   Location index array (3*3) [IGNORED!]    */
/*       indx    -          O   Index array                              */
/*       nocell  1          O   Number of cells (model data) in grid etc.*/
/*                                                                       */
/*************************************************************************/
{
    TInt4   i, j , ij , iseg , nseg[3], ldum ;
    TInt4   buflen , buffer[6] , uindex[1][3] , usrord ;
    BInt4   deffds[2997] , datfds[999] ;
    char    grp_par_nam[17] , grp_res_nam[17] ;
    TInt4   err_nef ;

/* Only for DELPAR PLO files
*/
    *ierror = IEOK ;
    if ( *ftype != ITDPNP )
    {
       *ierror = IETYPE ;
       return ;
    }

/* Open the file
*/
   DlwOpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam,
                      grp_res_nam, ierror);

   if (*ierror != IEOK)
   {
      return;
   }

/* Read the required information and close the file
*/
   uindex[0][0] = 1; uindex[0][1] = 1; uindex[0][2] = 1;
   usrord = 1;
   buflen = 6 * sizeof( TInt4 ) ;
   err_nef = Getelt (deffds, grp_par_nam, "SIZES" , uindex,
                     &usrord, &buflen, buffer);
   if ( err_nef != 0 )
   {
      *ierror = IEINFO ;
   }
   CloseNefisFiles ( fname, datfds, deffds, &ldum );
   if ( *ierror != IEOK )
   {
      return ;
   }

   nseg[1] = buffer[5] ; /* Exchange dimensions! */
   nseg[2] = buffer[4] ;

/* Construct the matrix of indices:
   Unfortunately, the matrix of values is stored in an awkward
   way, necessitating some index manipulation
*/

   ij   = 0 ;
   for ( j = 0 ; j < nseg[2]+2 ; j ++ )
   {
      for ( i = 0 ; i < nseg[1]+2 ; i ++ )
      {
         if ( i == 0 || i == nseg[1]+1 || j == 0 || j == nseg[2]+1 )
         {
            indx[ij] = 0 ;
         }
         else
         {
            indx[ij] = (i-1)*nseg[2] + j ; /* Note: +1 */
         }
         ij ++ ;
      }
   }

/* Set the other parameters */
   *nocell = nseg[1] * nseg[2] ;
   *igisty = IGCURV ;


   return ;
}

/*-------------------------- end of dlwnef.c -------------------------------*/
