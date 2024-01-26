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
 *  ods2nef.c  -  ODS to NEFIS interface functions
 *
 *  Eric Verschuur
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
 *  $Date: 1-04-03 10:52 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/ods2nef.c,v $
*/
/*
 */

#include <string.h>
#include <stdio.h>
#include <float.h>
#include <math.h>

#include "ods.h"
#include "odsmodel.h"

#ifndef NOPROT

/* function prototypes */

TInt4 iftype ( char *fname) ;

#endif

/*                                1         2 */
/*                       12345678901234567890 */
#define BLANK20         "                    "
#define SUBSNAMELEN		20


/*----------------------------------------------------------------------
 * open nefis files
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
   char  coding[2];
   char *deffn;
   int   baslen;
   char *ext;

   *ierror = IEOK;
   strcpy( coding, "N");

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
/*       if ( strrstr(deffn, ".def") != NULL || strrstr(deffn, ".DEF") != NULL )*/
       if ( strstr(deffn, ".def") != NULL || strstr(deffn, ".DEF") != NULL )
       { /* it has a .def extension allright */
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

   /* open def file */
   if ((*ierror = Opndef ((BInt4 *)deffds,(const TString) deffn,
                          (TString) coding)) != 0 )
       return;

   /* open dat file */
   if ((*ierror = Opndat ((BInt4 *)datfds, fname, coding)) != 0 )
       return;
}

/*----------------------------------------------------------------------
 * open nefis file and get groupnames
 *----------------------------------------------------------------------
 */
#ifndef NOPROT
      void OpenNefisFiles ( TString fname,   TInt4 ftype,    BInt4 *datfds,
                            BInt4 *deffds, TString grp_par, TString grp_res,
                            TInt4 *ierror)
#else
      void OpenNefisFiles ( fname,      ftype,      datfds,      deffds,
                            grp_par,    grp_res,    ierror)

      TString fname, grp_par, grp_res;
      BInt4 *datfds, *deffds;
      TInt4 *ierror, ftype;
#endif
{
   char  grp_par_def[17];
   char  grp_res_def[17];

   *ierror = IEOK;

   /* open files */
   OPNNEF(fname, &ftype, datfds, deffds, ierror);
   if (*ierror != 0 )
       return;

   /* get group name of 1st and 2nd groups in file */
   if ((*ierror = Inqfst((BInt4 *)datfds, grp_par, grp_par_def)) != 0 )
       return;

   *ierror = Inqnxt((BInt4 *)datfds, grp_res, grp_res_def);
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
/*
    FM_close_nefis_files ();
*/

    *ierror = Clsdat(datfds);
    *ierror = Clsdef(deffds);
/*
    *ierror = clnfdt(datfds); /* HACK: use the names directly! */
    *ierror = clnfdf(deffds);
*/
    return;
}

/*---------------------------------------------------------------------
 * function to get a list of times from a Nefis file
 *---------------------------------------------------------------------
 */
#ifndef NOPROT
    void GetNefisTme ( TString fname,     TInt4 *ftype,      TReal8 *timdef,
                       TInt4 *maxdef,    TReal8 *timlst,  TInt4 *maxlst,
                       TInt4 *nrtim,     TInt4 *ierror,    TString option)
#else
    void GetNefisTme ( fname,     ftype,      timdef,     maxdef,
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
    char   elmnam[17], elmtyp[9], elmqty[17], elmunt[17], elmdes[65];
    char   celnam[17];
    char  *deffn;
    int    stap;
    TInt4 *timoff;
    TInt4 *buffer, offst;
    TInt4  ldum;
    BInt4  elmndm, elmdms[5];
    BInt4  maxdim, nbytsg, usrord, buflen, dim1;
    BInt4  uindex[1][3], deffds[2997], datfds[999], dims[5], ord[5];
    double *curdef, *curtim, juldat;
    double dlw_date, newtime;

    *ierror = IEOK;
    dlw_date = 0.0;

    OpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam, grp_res_nam, ierror);
    if ( *ierror != IEOK )
        return;

/*    if (!strncasecmp(grp_par_nam, "DELWAQ_PARAMS",13))*/
    if (!strncmp(grp_par_nam, "DELWAQ_PARAMS",13))
    {
       /* DELWAQ .MAP or .HIS file type */

       /* get nr of timesteps */
        *ierror = Inqgrp (deffds, grp_res_nam, celnam, &maxdim, dims, ord);
        if (dims[0] == 0)
        {
            *ierror = Inqmxi (deffds, datfds, grp_res_nam, &dim1);
            dims[0] = dim1;
        }

       /* get times */
        strcpy (elmnam, "TIME");
        usrord = 1;
        uindex[0][0] = 1; /* start,stop,step */
        uindex[0][1] = dims[0];
        uindex[0][2] = 1;
        buflen = dims[0] * sizeof(TInt4);
        buffer = (TInt4 *) malloc(buflen);
        *ierror = Getelt (deffds, datfds, grp_res_nam, elmnam,
                          uindex, &usrord, &buflen, buffer );

     /* get time offset */
        strcpy (elmnam, "TIME_OFFSET");
        usrord = 1;
        uindex[0][0] = 1; /* start,stop,step */
        uindex[0][1] = 7;
        uindex[0][2] = 1;
        buflen = 100 * sizeof(TInt4); /* BUG ?? 7 geeft Nefis error -25051 */
        timoff = (TInt4 *) malloc(buflen);
        *ierror = Getelt (deffds, datfds, grp_par_nam, elmnam,
                          uindex, &usrord, &buflen, timoff );

        if (*ierror != IEOK || timoff[0] == 0L)
        {   /* set to default start time */
            timoff[0] = 1900L; /* year */
            timoff[1] = 1L;    /* month */
            timoff[2] = 1L;    /* day */
        }
        ldum = 0L;
       /* convert start time to julian date */
        julian( &timoff[0], &timoff[1], &timoff[2], &ldum, &ldum, &ldum,
                &dlw_date);
        free (timoff);

       /* compare times to wanted list and copy wanted times */
        for (stap=0, offst=0;
             (stap < dims[0]) && (offst < *maxlst) ;
               stap++)
        {
           /* convert times to julian days first */
            newtime = buffer[stap];
            newtime /= (24*3600);
            juldat = dlw_date + newtime;

            /* check if all steps are wanted */
            if ( timdef[0] <= 0.0 && timdef[1] >= ALLSTEPS )
            { /* just copy step to outlist */
                timlst[offst] = juldat;
                offst++ ;
                continue;
            }

           /* compare against wanted list */
            for (curdef = timdef; curdef < timdef + *maxdef; curdef++)
            {
                if (fabs(juldat - *curdef)<= DBL_EPSILON)
                {
                    timlst[offst] = juldat;
                    offst++ ;
                    break;
                }
               /* assuming climbing wanted list entries */
              /*  if (*curdef > juldat) */
              /*      break; */
            }
        }
        *nrtim = offst;

        free (buffer);

        CloseNefisFiles ( fname, datfds, deffds, ierror);
        return;
    }

   /* add other file types here ... */

    *ierror = IEUNKN ;
    CloseNefisFiles ( fname, datfds, deffds, ierror);
    return ;
}

/*--------------------------------------------------------------------
* function to get a matrix from a Nefis file
*---------------------------------------------------------------------
*/

#ifndef NOPROT
      void GetNefisMat ( TString fname,   TInt4 *ftype,   TInt4 *parcod,
                         TReal8 *tim,   TInt4 *loc,     TReal4 *misval,
                         TInt4 *maxdim,  TReal4 *values, TInt4 *ierror,
                         TString option)
#else
      void GetNefisMat ( fname,     ftype,      parcod,   tim,
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
    char    elmnam[17], elmtyp[9], elmqty[17], elmunt[17], elmdes[65];
    char   *deffn;
    int     stap;
    TInt4   sizes[6];
    TInt4  *times, *timoff;
    TInt4   offst, nefstart, nefstep;
    TInt4   start_loc, end_loc, step_loc, iloc, outloc;
    TInt4   ldum;
    BInt4   elmndm, elmdms[5];
    BInt4   maxd, nbytsg, usrord, buflen, dim1;
    BInt4   uindex[1][3], deffds[2997], datfds[999], dims[5], ord[5];
    float  *fdata;
    double *curdef, *curtim, juldat, julstep, prevdat, outstep;
    double  dlw_date, newtime;

  /* do some initialisation */
   *ierror = IEOK;

   OpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam, grp_res_nam, ierror);
   if ( *ierror != IEOK )
       return;

  /* set output array elements to misval */
   for (fdata = values; fdata < (values + *maxdim); fdata++)
        *fdata = *misval;

   start_loc = loc[0]-1;
   end_loc = loc[3]-1;
   step_loc = (loc[6] > 0 ? loc[6] : 1) ;

   outloc = ((float)end_loc - (float)start_loc + 1)/(float)step_loc + 0.5 ;
   outstep = tim[2];

   maxd = 0;

/*   if (!strncasecmp(grp_par_nam, "DELWAQ_PARAMS", 13))*/
   if (!strncmp(grp_par_nam, "DELWAQ_PARAMS", 13))
   {
     /* DELWAQ .MAP or .HIS file type */

     /* get nr of timesteps */
       *ierror = Inqgrp (deffds, grp_res_nam, celnam, &maxd, dims, ord);
       if (dims[0] == 0)
       {
          *ierror = Inqmxi (deffds, datfds, grp_res_nam, &dim1);
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
       buflen = 6 * sizeof (TInt4);
       *ierror = Getelt (deffds, datfds, grp_par_nam, elmnam,
                   uindex, &usrord, &buflen, sizes);
       if ( *ierror != IEOK )
       {
           CloseNefisFiles ( fname, datfds, deffds, &ldum);
           return;
       }

     /* get time offset */
       strcpy (elmnam, "TIME_OFFSET");
       usrord = 1;
       uindex[0][0] = 1; /* start, stop, step */
       uindex[0][1] = 7;
       uindex[0][2] = 1;
       buflen = 100 * sizeof(TInt4); /* BUG ?? 7 geeft Nefis error -25051 */
       timoff = (TInt4 *) malloc(buflen);
       *ierror = Getelt (deffds, datfds, grp_par_nam, elmnam,
                   uindex, &usrord, &buflen, timoff );

       if (*ierror != IEOK || timoff[0] == 0L)
       {   /* set to default start time */
           timoff[0] = 1900L; /* year */
           timoff[1] = 1L;    /* month */
           timoff[2] = 1L;    /* day */
           *ierror = IEOK;
       }
       ldum = 0L;
     /* convert start time to julian date */
       julian( &timoff[0], &timoff[1], &timoff[2], &ldum, &ldum, &ldum,
               &dlw_date);
       julstep = timoff[5]/86400.0;

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
       nefstart = 1.1 + (tim[0] - dlw_date - (timoff[3]/86400.0))/julstep;
       nefstep = outstep / julstep;
       if (nefstep == 0L) nefstep = 1;
       free (timoff);

       strcpy (elmnam, "TIME");
       usrord = 1;
       uindex[0][0] = nefstart; /* start */
       uindex[0][1] = dims[0];  /* stop */
       uindex[0][2] = nefstep;  /* step */
       buflen = dims[0] * sizeof(TInt4);
       times = (TInt4 *) malloc(buflen);
       *ierror = Getelt (deffds, datfds, grp_res_nam, elmnam,
                   uindex, &usrord, &buflen, times );
       if ( *ierror != IEOK )
       {
           CloseNefisFiles ( fname, datfds, deffds, &ldum);
           free (times);
           return;
       }

     /* get data */
       /* create substance name */
       sprintf (elmnam, "SUBST_%03ld", (*parcod)+1);
       usrord = 1;
       uindex[0][0] = nefstart; /* start */
       uindex[0][1] = dims[0];  /* stop */
       uindex[0][2] = nefstep;  /* step */
       buflen = dims[0]  * sizes[2] * sizeof(float);
       fdata = (float *) malloc (buflen);
       *ierror = Getelt (deffds, datfds, grp_res_nam, elmnam,
                          uindex, &usrord, &buflen, fdata );
       if ( *ierror != IEOK )
       {
           CloseNefisFiles ( fname, datfds, deffds, &ldum);
           free (times);
           free (fdata);
           return;
       }

       /* compare times to wanted list and copy wanted data */
       for (stap=0, offst=0;
             (stap < dims[0]/nefstep) && (offst < *maxdim) ;
               stap++)
       {
           /* convert times to julian days first */
           newtime = times[stap];
           newtime /= 86400.0;
           juldat  = dlw_date + newtime;

           /* if timestep is within wanted range copy data */
           if ( juldat >= tim[0] && juldat <= tim[1])
           {
               /* current timestep matches, get data */
               for ( iloc = start_loc ; (iloc <= end_loc) && (offst < *maxdim) ;
                      iloc += step_loc )
               {
                   values[offst] = fdata[stap*sizes[2]+iloc];
                   offst++;
               }
           }
       }
       free (times);
       free (fdata);

       CloseNefisFiles ( fname, datfds, deffds, ierror);
       return;
   }

   /* add other file types here ... */

   *ierror = IEUNKN ;
   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return ;
}

/*--------------------------------------------------------------------
* function to get a list of locations from a Nefis file
*---------------------------------------------------------------------
*/


#ifndef NOPROT
      void GetNefisLoc ( TString fname,   TInt4 *ftype,   TString pardef,
                         TInt4 *maxdef,  TString parlst,  TInt4 *maxlst,
                         TInt4 *nrlst,   TInt4 *locnr,   TInt4 *ierror,
                         TString option)
#else
      void GetNefisLoc ( fname,     ftype,      pardef,      maxdef,
                         parlst,    maxlst,     nrlst,       locnr,
                         ierror,    option)

      TString fname,  pardef, parlst, option;
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
/*       locnr   maxlst     O   List of index numbers of locations found */
/*       maxdef  -          I   Max. nr of locations wanted.             */
/*       maxlst  -          I   Max. nr of locations to return.          */
/*       nrlst   -          O   Nr of locations returned.                */
/*       option  -         I/O  option (reserved for future extensions)  */
/*       pardef  maxdef     I   List of locations wanted.                */
/*       parlst  maxlst     O   List of locations found.                 */
/*                                                                       */
/*************************************************************************/

{
   char  grp_par_nam[17];
   char  grp_res_nam[17];
   char  elmnam[17];
   char  celnam[17];
   char  *locnam, *locdef, *unit, *ip1, *ip2;
   char  location[PARLEN+1];
   char *deffn;
   int   offst;
   TInt4 maxdim, loctel, dims[5], ord[5] ;
   TInt4 sizes[7];
   TInt4 *lbuffer;
   char *cbuffer;
   BInt4 usrord, buflen;
   BInt4 deffds[2997], datfds[999], uindex[1][3];

   *ierror = IEOK;

   OpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam, grp_res_nam, ierror);
   if ( *ierror != IEOK )
       return;

/*   if (!strncasecmp(grp_par_nam, "DELWAQ_PARAMS",13))*/
   if (!strncmp(grp_par_nam, "DELWAQ_PARAMS", 13))
   {
     /* DELWAQ .MAP or .HIS file type */

     /* get number of locations */
       strcpy (elmnam, "SIZES");
       uindex[0][0] = 1; uindex[0][1] = 1; uindex[0][2] = 1; /* start,stop,step */
       usrord = 1;
       buflen = 6 * sizeof (TInt4);
       lbuffer = (TInt4 *) malloc(buflen);
       *ierror = Getelt (deffds, datfds, grp_par_nam, elmnam,
                  uindex, &usrord, &buflen, lbuffer );
       if ( *ierror != IEOK )
       {
           free (lbuffer);
           CloseNefisFiles ( fname, datfds, deffds, ierror);
           return;
       }

     /* get location names */
       strcpy (elmnam, "LOCATION_NAMES");
       usrord = 1;
       uindex[0][0] = 1; /* start, stop, step */
       uindex[0][1] = lbuffer[2];
       uindex[0][2] = 1;
       buflen = lbuffer[2] * (PARLEN+1+300); /* NEFIS bug ?? -25051 bij juiste buffergrootte */
       free (lbuffer);
       cbuffer = (char *) malloc(buflen);
       *ierror = Getelt (deffds, datfds, grp_par_nam, elmnam,
                  uindex, &usrord, &buflen, cbuffer );
       if ( *ierror != IEOK )
       {
printf ("error %ld while getting location names\n",*ierror);
           free (cbuffer);
           CloseNefisFiles ( fname, datfds, deffds, ierror);
           return;
       }

      offst = 0;
     /* split strings into location names */
      for (locnam = cbuffer, loctel = 1 ;
           (locnam < cbuffer+buflen) && (offst < *maxlst);
           locnam +=PARLEN, loctel++ )
      {
         strncpy ( location, locnam, (size_t) PARLEN);
         *(location + PARLEN) = '\0' ;

          /* remove any trailing blanks from substance name */
          /* for ( locdef = location+PARLEN-1; */
          /*                locdef>location && *locdef==' '; locdef--) */
          /*     *locdef = '\0'; */

          /* check if substance name is on the wanted list,
          ** if so add it to parlst
          */
          for ( locdef=pardef; locdef<(pardef+(*maxdef*PARLEN)); locdef+=PARLEN+1)
          {
             /* next test is true if first non-blank characters of location
             ** match against wanted list, or first wanted list entry is "*"
             ** and location is meaningful (first character is not hex 0 or FF)
             */
              if ((!strcmp(locdef,"*") ||
                     !strncmp(locdef, location, strcspn(location," "))) &&
                        (location[0] != '\377' && location[0] != '\0') )
              {
                  strcpy(parlst +( offst * (PARLEN+1)), location);
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
   /* add other file types here ... */

   *ierror = IEUNKN ;
   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return ;
}

/*--------------------------------------------------------------------
* function to get a list of parameters from a Nefis file
*---------------------------------------------------------------------
*/

#ifndef NOPROT
      void GetNefisPar ( TString fname,   TInt4 *ftype,   TString pardef,
                         TInt4 *maxdef,  TString parlst,  TString paruni,
                         TInt4 *maxlst,  TInt4 *nrlst,   TInt4 *partyp,
                         TInt4 *parcod,  TInt4 *ierror,  TString option)
#else
      void GetNefisPar ( fname,     ftype,      pardef,      maxdef,
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
   char  celnam[17];
   char  *subnam, *unit, *subdef, *ip1, *ip2;
   char  substance[PARLEN+1], eenh[PARLEN+1];
   char *deffn;
   int   i, offst;
   TInt4 subnr, maxdim, dims[5], ord[5] ;
   TInt4 sizes[7];
   TInt4 *lbuffer;
   char *cbuffer;
   BInt4 usrord, buflen;
   BInt4 deffds[2997], datfds[999], uindex[1][3];

   *ierror = IEOK;

   OpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam, grp_res_nam, ierror);
   if ( *ierror != IEOK )
       return;

   if (!strncmp(grp_par_nam, "DELWAQ_PARAMS",13))
   {
     /* DELWAQ .MAP or .HIS file type */

     /* get number of substances */
       strcpy (elmnam, "SIZES");
       uindex[0][0] = 1; /* start */
       uindex[0][1] = 1; /* stop */
       uindex[0][2] = 1; /* step */
       usrord = 1;
       buflen = 6 * sizeof (TInt4);
       lbuffer = (TInt4 *) malloc(buflen);
       *ierror = Getelt (deffds, datfds, grp_par_nam, elmnam,
                  uindex, &usrord, &buflen, lbuffer );
       if ( *ierror != IEOK )
       {
           CloseNefisFiles ( fname, datfds, deffds, ierror);
           free (lbuffer);
           return;
       }

     /* get substance names */
       strcpy (elmnam, "SUBST_NAMES");
       uindex[0][0] = 1; uindex[0][1] = lbuffer[0]; uindex[0][2] = 1;
       buflen = lbuffer[0] * (PARLEN+1+300); /* NEFIS bug ?? -25051 bij juiste buffergrootte */
       free (lbuffer);
       cbuffer = (char *) malloc(buflen);
       *ierror = Getelt (deffds, datfds, grp_par_nam, elmnam,
                  uindex, &usrord, &buflen, cbuffer );
       if ( *ierror != IEOK )
       {
           CloseNefisFiles ( fname, datfds, deffds, ierror);
           free (cbuffer);
           return;
       }

      offst = 0;
     /* split strings into substance names and units */
      for (subnam = cbuffer, unit = paruni, subnr = 1 ;
           (subnam < cbuffer+buflen) && (offst < *maxlst);
           subnam +=PARLEN, unit +=PARLEN, subnr++ )
      {
          /* Assume DELWAQ names can have a unit between [] / * or ()  */

          ip1 = strchr ( subnam, (int) '[') ;
/*          if (ip1 == NULL) */
/*          { */
/*              ip1 = strchr ( subnam, (int) '(') ; */
/*              ip2 = strchr ( subnam, (int) ')') ; */
/*          } */
/*          else */
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
    for (i=0; i<*nrlst; i++)
        partyp[i] = ODS_PT_LOC_DEP | ODS_PT_LOC_MNK ;

      free (cbuffer);
      CloseNefisFiles ( fname, datfds, deffds, ierror);
      return;
   }
   /* add other file types here ... */

   *ierror = IEUNKN ;
   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return ;
}

/*--------------------------------------------------------------------
* get dimensions from a Nefis file
*---------------------------------------------------------------------
*/

#ifndef NOPROT
      void GetNefisDim ( TString fname,   TInt4 *ftype,   TString dim,
                         TInt4 *pardep,  TInt4 *timdep,  TInt4 *locdep,
                         TInt4 *ndim,    TInt4 *ierror,  TString option)
#else
      void GetNefisDim ( fname,      ftype,   dim,     pardep,   timdep,
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
   char  *deffn;
   BInt4  maxdim, dim1, dims[5], ord[5] ;
   TInt4 *buffer, sizes[7];
   BInt4  usrord, buflen;
   BInt4  uindex[1][3], deffds[2997], datfds[999];

   *ierror = IEOK;

   OpenNefisFiles (fname, *ftype, datfds, deffds, grp_par_nam, grp_res_nam, ierror);
   if ( *ierror != IEOK )
       return;

   if (!strncmp(grp_par_nam, "DELWAQ_PARAMS",13))
   {
   /* DELWAQ .MAP or .HIS file type */
       switch (dim[0])
       {
           case 't':  /* tim */
           case 'T':
           *ierror = Inqgrp (deffds, grp_res_nam, celnam, &maxdim, dims, ord);
           ndim[0] = (TInt4)1;
           ndim[1] = dims[0];
           if (dims[0] == 0)
           {
               *ierror = Inqmxi (deffds, datfds, grp_res_nam, &dim1);
               ndim[1] = dim1;
           }
           break;

           case 'p':  /* par */
           case 'P':
           strcpy (elmnam, "SIZES");
           uindex[0][0] = 1; uindex[0][1] = 1; uindex[0][2] = 1;
           usrord = 1;
           buflen = 6 * sizeof (TInt4);
           buffer = (TInt4 *)malloc (buflen);
           *ierror = Getelt (deffds, datfds, grp_par_nam, elmnam, uindex,
                             &usrord, &buflen, buffer);
           ndim[0] = (TInt4)1;
           ndim[1] = buffer[0]; /* nr of substances */
           free (buffer);
           break;

           case 'l':  /* loc */
           case 'L':
           strcpy (elmnam, "SIZES");
           uindex[0][0] = 1; uindex[0][1] = 1; uindex[0][2] = 1;
           usrord = 1;
           buflen = 6 * sizeof (TInt4);
           buffer = (TInt4 *)malloc (buflen);
           *ierror = Getelt (deffds, datfds, grp_par_nam, elmnam, uindex,
                             &usrord, &buflen, buffer);
           ndim[0] = (TInt4)1;
           ndim[1] = buffer[2]; /* nr of locations */
           free (buffer);
           break;

           default:
           *ierror = IEOTHR;
           return;
       } /* switch dim */
       return;
   } /* if grp_par_nam */

   /* add other file types here ... */

   *ierror = IEUNKN ;
   CloseNefisFiles ( fname, datfds, deffds, ierror);
   return ;
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
    TInt4 error;

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

/*-------------------------- end of ods2nef.c -------------------------------*/
