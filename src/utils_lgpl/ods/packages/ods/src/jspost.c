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
 *  jspost.c  -  ODS functions for standard JSPOST files
 *
 *   Peter van den Bosch
 */

/*
 *  $Author: Markus $
 *  $Date: 12/27/00 2:53p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/jspost.c,v $
 *
 */

/*   Project:     Open Data Structuur                                 */
/*   Module:      Loketjes                                            */
/*   Subroutines: ODSGetLocJSP Get Locations from JSPOST file         */
/*                ODSGetParJSP Get parameters from JSPOST file        */
/*                ODSGetTmeJSP Get times from JSPOST file             */
/*                ODSGetValJSP Get values from JSPOST file            */
/*   Function:    "Loket" functions per filetype                      */
/*   Comment:     General version                                     */
/*   Reference:   opendata.doc                                        */
/*   Review:                                                          */
/*                                                                    */
/**********************************************************************/
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <errno.h>
#ifdef HAVE_MALLOC_MALLOC_H
#include "malloc/malloc.h"
#else
#include "malloc.h"
#endif

#include "ods.h"
#include "portable.h"
#include "itrans.h"
#include "julian.h"
#include "gregor.h"
#include "equal.h"
#include "jsputil.h"

#ifndef NOPROT
#include "odsmodel.h"

 /* local functions */
 static void GetJspRunId (FILE *fp,TInt4 *nseg,TInt4 *nval, double *starttm,
                          double *stoptm, double *delttm,TInt4 *ierror);
 static void GetJspT0 (FILE *fp, double tdelt, double *timval, double *dt0,
                       TInt4 *ierror);
#else
 static void GetJspRunId ();
 static void GetJspT0 ();
#endif

#define FALSE   0
#define TRUE    1

#define MAXIBU 512

/* quicker way to fill strings with blanks */
#define Blanks(a,b) memset((a),' ',(b))

/* Nice patch for MS-Windows DLL's ! */
#ifdef MSWINDOS
#   define sprintf wsprintf
    int _far _cdecl wsprintf(char _far* lpszOut, const char _far* lpszFmt, ...);
#endif


/*************************************************************************/
/*    SUBROUTINE Get Location from JSPOST file                           */
/*************************************************************************/

void FUNTYPE ODSGetLocJSP ( char *fname,
                   char *locdef,
                  TInt4 maxdef,
                   char *loclst,
		  TInt4 *loctyp,
                  TInt4 maxlst,
                  TInt4 *nrlst,
                  TInt4 *locnr,
                  TInt4 *ierror,
		   char *option )

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       locdef  maxdef     I   List of parameters wanted.               */
/*       loclst  maxlst     O   List of parameters found.                */
/*       loctyp  maxlst     O   Type code 0-normal, 1-hierarchical       */
/*       locnr   maxlst     O   List of index numbers of locations found */
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*       option  -         I/O  Reserved for future extensions           */
/*                                                                       */
/*************************************************************************/
{
   FILE   *fdat, *fstu ;
   char   segnam [ 25], *p ;
   char   tmpstr[150] ;
   TInt4  i, ip, idef, noval, noseg, len;
   double dfdum ;

   JspOpen (fname, &fdat, &fstu, ierror);
   if ( *ierror != IEOK)
      return ;

  /* read number of parameters and locations, skip times */
   GetJspRunId (fstu, &noseg, &noval, &dfdum, &dfdum, &dfdum, ierror);
   if ( *ierror != IEOK)
   {
      Errfin (fdat, fstu, NULL, NULL, NULL);
      return ;
   }

   /* skip records with header information */
   for ( i = 0L; i < 3L ; i++ )
   {
      if ( fgets( tmpstr, sizeof( tmpstr ), fstu )  == NULL )
      {
         itrans ( ferror ( fstu), (TInt4 *)ierror) ;
         fclose (fstu) ;
         return ;
      }
   }

   /* skip records with parameter info */
   for ( i = 0L; i < noval ; i++ )
   {
      if ( fgets( tmpstr, sizeof( tmpstr ), fstu ) == NULL )
      {
         itrans ( ferror ( fstu),(TInt4 *) ierror) ;
         fclose ( fstu) ;
         return ;
      }
   }

   *nrlst = 0 ;
   *ierror = IEOK ;

   for ( i = 1L; i <= noseg; i++)
   {

     /*   Read JSPOST segment name from file      */
      fgets ( tmpstr, sizeof( tmpstr), fstu ) ;
      len = strlen( tmpstr ) ;

     /*
     ** get rid of any trailing CR/LF characters (Fortran field separators!)
     ** or blanks.
     */
      for (p = tmpstr+len-1 ; ( *p=='\n' || *p=='\r' || *p==' ' ); p--)
      {
         *p = '\0' ;
      }

     /* skip leading blanks */
      for (ip = 0 ; tmpstr[ ip ] == ' '; ip++) ;

      tmpstr[ PARLEN] = '\0' ;
      Blanks( segnam, sizeof( segnam )) ;
      strcpy( segnam, tmpstr + ip ) ;

      if ( feof( fstu ) )
      {
         sprintf( segnam, "%-d", i ) ;
      }
      if ( ferror ( fstu) )
      {
         itrans ( ferror ( fstu), (TInt4 *)ierror) ;
         fclose ( fstu) ;
         return ;
      }

      idef = 0 ;
      while (idef < maxdef)
      {
         if (equal ( locdef + ( idef * PARLEN), segnam))
         {
            if ( *nrlst < maxlst)
            {
               strncpy ( loclst + *nrlst * (PARLEN+1), segnam, (size_t) PARLEN) ;
               loclst [ *nrlst * (PARLEN+1) + PARLEN] = '\0' ;
               locnr[*nrlst] = i;
	       loctyp[*nrlst] = 0;
               (*nrlst)++ ;
            }
            else
            {
               *ierror = IELMNY ;
               fclose (fstu) ;
               return ;
            }
            break ;
         }
         else
         {
            idef++ ;
         }
      }
   }
   fclose (fstu) ;

   return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Parameter from JSPOST file                          */
/*************************************************************************/

void FUNTYPE ODSGetParJSP  ( char *fname,
                     char *pardef,
                    TInt4 maxdef,
                     char *parlst,
                     char *paruni,
                    TInt4 maxlst,
                    TInt4 *nrlst,
                    TInt4 *partyp,
                    TInt4 *parcod,
                    TInt4 *ierror,
		     char *option )

/*************************************************************************/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  -------------------------------          */
/*       fname   -          I   Full filename, including extension       */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       maxdef  -          I   Max. nr of parameters wanted.            */
/*       maxlst  -          I   Max. nr of parameters to return.         */
/*       nrlst   -          O   Nr of parameters returned.               */
/*       parcod  maxlst     O   List of parameter indices.               */
/*       pardef  maxdef     I   List of parameters wanted.               */
/*       parlst  maxlst     O   List of parameters found.                */
/*       partyp  maxlst     O   List of parameter types.                 */
/*       paruni  maxlst     O   List of units of parameters found.       */
/*       option  -         I/O  Reserved for future extensions           */
/*                                                                       */
/*************************************************************************/
{
    FILE   *fdat, *fstu ;
    char   subnam [ 25] ;
    char   tmpstr [ 150 ] ;
    char   * ip1, * ip2 ;
    TInt4  i, ip, idef, noval, noseg, len;
    double dfdum ;
#   ifdef UNIX
    TInt4  rec_delim ;
#   endif

    JspOpen (fname, &fdat, &fstu, ierror);
    if ( *ierror != IEOK)
        return ;

   /* read number of parameters and locations, skip times */
    GetJspRunId (fstu, &noseg, &noval, &dfdum, &dfdum, &dfdum, ierror);
    if ( *ierror != IEOK)
    {
        Errfin (fdat, fstu, NULL, NULL, NULL);
        return ;
    }

   /* skip records with header information */
    for ( i = 0; i < 3 ; i++ )
    {
        if ( fgets( tmpstr, sizeof( tmpstr ), fstu ) == NULL )
        {
            itrans ( ferror ( fstu), (TInt4 *)ierror) ;
            Errfin (fdat, fstu, NULL, NULL, NULL);
            return ;
        }
    }

    *nrlst = 0 ;
    *ierror = IEOK ;

    for ( i = 1; i <= noval; i++)
    {
       /*  Read JSPOST parameters from file          */
        if ( fgets ( tmpstr, sizeof( tmpstr), fstu ) == NULL)
        {
            if ( feof ( fstu) )
                *ierror = IEUEOF ;
            else
                itrans ( ferror ( fstu), (TInt4 *)ierror) ;
            Errfin (fdat, fstu, NULL, NULL, NULL);
            return ;
        }
        len = strlen( tmpstr ) ;

        if ( tmpstr[len-1] == '\n' )
        {
            tmpstr[len-1] = ' ' ;
        }
        if ( tmpstr[len-2] == '\r' )
        {
            tmpstr[len-2] = ' ' ;
        }
       /* skip leading blanks in segment name */
        for (ip=0 ; tmpstr[ip] == ' '; ip++) ;

        tmpstr[ PARLEN] = '\0' ;
        Blanks( subnam, sizeof( subnam )) ;
        strncpy( subnam, tmpstr + ip, PARLEN ) ;
        subnam [ PARLEN] = '\0' ;

       /* chop trailing blanks from segment name */
        for (ip = PARLEN-1; subnam[ip] == ' '; ip--)
            subnam[ip] = '\0';

        idef = 0 ;
        while ( idef < maxdef)
        {
            if ( equal ( pardef + ( idef * PARLEN), subnam))
            {
                if ( *nrlst < maxlst)
                {
                    /* Assume DELWAQ names can have a unit between []      */
                    /* Unit is stored in paruni but parname is not changed */

                    ip1 = strchr ( subnam, (int) '[') ;
                    ip2 = strchr ( subnam, (int) ']') ;
                    if (( ip2 > ip1) && ( ip1 != NULL) && ( ip2 != NULL))
                    {
                        strncpy ( parlst + ( *nrlst * (PARLEN+1)), subnam, (size_t) PARLEN ) ;
                        strncpy ( paruni + ( *nrlst * (PARLEN+1)), ip1 + 1,
                                    (size_t) (ip2-ip1-1)) ;
                        parlst [  *nrlst * (PARLEN+1) + ip1-subnam] = '\0' ;
                        paruni [  *nrlst * (PARLEN+1) + (ip2-ip1-1)]  = '\0' ;
                    }
                    else
                    {
                        strncpy ( parlst + ( *nrlst * (PARLEN+1)), subnam, (size_t) PARLEN) ;
                        strncpy ( paruni + ( *nrlst * (PARLEN+1)), "                   ", (size_t) PARLEN) ;
                        parlst [ *nrlst * (PARLEN+1) + PARLEN] = '\0' ;
                        paruni [ *nrlst * (PARLEN+1) + PARLEN] = '\0' ;
                    }
                    *(parcod + *nrlst) = i;
		    *(partyp + *nrlst) = ODS_PT_LOC_DEP ;
                    (*nrlst)++ ;
                }
                else /* *nrlst >= maxlst */
                {
                    *ierror = IEPMNY ;
                    Errfin (fdat, fstu, NULL, NULL, NULL);
                    return ;
                }
                break ;
            }
            else /* strings not equal */
            {
                idef++ ;
            }
        }
    }
    Errfin (fdat, fstu, NULL, NULL, NULL);

    return ;
}

/*************************************************************************/
/*    SUBROUTINE Get Times from JSPOST file                              */
/*************************************************************************/

void FUNTYPE ODSGetTmeJSP  ( char *fname,
                     double *timin,
                    TInt4 maxiti,
                     double *tim,
                    TInt4 maxoti,
                    TInt4 *nrtim,
		    TInt4 *timtyp,
                    TInt4 *ierror,
		     char *option )

/*------------------------------------------------------------------------
 *    Arguments:
 *
 *       Name    Size      I/O  Description
 *       ------  --------  ---  ------------------------------------
 *       fname   -          I   Full filename, including extension.
 *       ierror  -          O   Errorcode. See ods.h for definition.
 *       maxiti  -          I   Nr. of time intervals in input. (*2)
 *       maxoti  -          I   Max. nr. of time intervals in output.
 *       nrtim   -          O   Nr. of time intervals in output.
 *       tim     maxoti     O   Times found.
 *       timin   maxiti     I   Asked times. (2dim array [i] start, [i+1] stop)
 *       timtyp  maxlst     O   List of types.
 *       option  -         I/O  Reserved for future extensions
 *------------------------------------------------------------------------
 */
   {
   FILE   *fdat, *fstu ;
   char   ftype [] = "r" ;
   TInt4  noval, noseg, itim ;
   TInt4  ctime , ist ;
   double dlwtim, dt0, ftime ;
   double tstart, tstop, delt, tcur ;
   int    some_lower ;
   TInt4  lng0 = 0L ;

   JspOpen (fname, &fdat, &fstu, ierror);
   if ( *ierror != IEOK)
      return ;

  /* read number of parameters, locations and times */
   GetJspRunId (fstu, &noseg, &noval, &tstart, &tstop, &delt, ierror);
   if ( *ierror != IEOK)
   {
      Errfin (fdat, fstu, NULL, NULL, NULL);
      return ;
   }

   /* read header with T0 info */
   GetJspT0(fstu, delt, &dlwtim, &dt0, ierror);
   if ( *ierror != IEOK)
   {
      Errfin (fdat, fstu, NULL, NULL, NULL);
      return ;
   }

   *ierror = IEOK ;
   *nrtim = 0 ;

   /* now loop over the timesteps from tstart to tstop */

   *nrtim = 0      ;
   ist    = 0      ;
   tcur   = tstart ;

   while ( tcur <= tstop )
   {
      ctime = (TInt4) ( tcur*dt0 ) ; /* convert timestep to seconds */

      /* construct new julian date */
      /* Hopelessly convoluted and twisted
      ftime = AddToJulian( dlwtim, lng0, lng0, lng0, ctime ) ;
      */
      ftime = dlwtim + tcur ;

      /* check if it is still possible that we find something */
      some_lower = FALSE ;
      for ( itim = 0; itim < maxiti; itim++)
      {
         if ( ftime <= timin[itim+1])
         {
            some_lower = TRUE ;
            break;
         }
      }
      if ( ! some_lower)
         break ;  /* break while-loop */

     /* maxiti = */
      for ( itim = 0; itim < maxiti; itim++)
      {
         if (( ftime >= *(timin + itim)) &&
               ( ftime <= *(timin + itim + 1)))
         {
            if ( *nrtim < maxoti)
            {
               *(tim + *nrtim) = ftime ;
	       *(timtyp + *nrtim) = 0;
               (*nrtim)++ ;
               break ;
            }
            else
            {
               *nrtim  = maxoti ;
               *ierror = IETMNY ;
               break ;
            }
         } /* timin1 < ftime < timin2  */
      }  /* for itim */
      /* Was:
      tcur = tcur+delt ;
      ---- this causes numerical inaccuracies
      */
      ist  ++                    ;
      tcur = tstart + ist * delt ;
   }
   Errfin (fdat, fstu, NULL, NULL, NULL);

}


/*************************************************************************/
/*    SUBROUTINE Get Values from JSPOST file                             */
/*************************************************************************/

void FUNTYPE ODSGetValJSP  ( char *fname,
                     char *locin,
                     char *parin,
                     double *timin,
                    TInt4 maxilo,
                    TInt4 maxipa,
                    TInt4 maxiti,
                     float misval,
                     char *loc,
                     char *par,
                     double *tim,
                     float *values,
                    TInt4 maxolo,
                    TInt4 maxopa,
                    TInt4 maxoti,
                    TInt4 *nrloc,
                    TInt4 *nrpar,
                    TInt4 *nrtim,
                    TInt4 *ierror)

/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   -          I   Full filename, including extension.      */
/*       ierror  -          O   Errorcode. See ods.h for definition.     */
/*       loc     maxolo     O   Locations as found.                      */
/*       locin   maxilo     I   Asked locations.                         */
/*       maxilo  -          I   Nr. of location masks in input.          */
/*       maxipa  -          I   Nr. of parameter masks in input.         */
/*       maxiti  -          I   Nr. of time intervals in input.          */
/*       maxolo  -          I   Max. nr. of locations in output.         */
/*       maxopa  -          I   Max. nr. of parameters in output.        */
/*       maxoti  -          I   Max. nr. of time intervals in output.    */
/*       misval  -          I   Missing value indicator.                 */
/*       nrloc   -          O   Nr. of locations in output.              */
/*       nrpar   -          O   Nr. of parameters in output.             */
/*       nrtim   -          O   Nr. of time intervals in output.         */
/*       par     maxopa     O   Parameters as found.                     */
/*       parin   maxipa     I   Asked parameters.                        */
/*       tim     maxoti     O   Times found.                             */
/*       timin   maxiti     I   Asked times.                             */
/*       values  maxolo*    O   Values found.                            */
/*               maxopa*                                                 */
/*               maxoti                                                  */
/*                                                                       */
/*************************************************************************/
   {
   FILE   *fdat ;
   FILE   *fstu ;
   char   subnam [ 21] ;
   char   segnam [ 21] ;
   char   tmpstr [ 150] ;
   size_t items_read ;
   TInt4  isub, iseg, idef, noval, noseg, iloc, ipar, itim ;
   TInt4  ist, len, ip, ctime, notim ;
#  ifdef UNIX
   TInt4  rec_delim ;
#  endif
   int    some_lower ;
   TInt4  *ibuffr , *ibuffs  , *ibufft ;
   double dlwtim  , dt0      , ftime   ;
   TReal4 val                          ;
   double tstart, tstop, delt, tcur    ;
   TInt4  lng0 = 0L;

   *ierror = IEOK ;

   for ( iloc = 0; iloc < maxolo; iloc++) {
      for ( ipar = 0; ipar < maxopa; ipar++) {
         for ( itim = 0; itim < maxoti; itim++) {
            *(values + (itim * maxolo * maxopa +
                        ipar * maxolo +
                        iloc)) = misval ;
         }
      }
   }
/*       fortran: values ( maxolo, maxopa, maxoti)                       */

   JspOpen (fname, &fdat, &fstu, ierror);
   if ( *ierror != IEOK) {
      return ;
   }

   /* reserve space for buffers */
   ibuffr = (TInt4 *) malloc( (size_t) (maxopa * sizeof( TInt4 )) ) ;
   ibuffs = (TInt4 *) malloc( (size_t) (maxolo * sizeof( TInt4 )) ) ;
   ibufft = (TInt4 *) malloc( (size_t) (maxoti * sizeof( TInt4 )) ) ;

   /* initialize values array by setting all fields to missing value */
   for ( iloc = 0; iloc < maxolo; iloc++) {
      ibuffs [ iloc] = -1 ;
   }

   *nrloc = 0 ;
   *nrpar = 0 ;
   *nrtim = 0 ;

   /* read first part of JSPOST .stu file                               */

  /* read number of parameters, locations and times */
   GetJspRunId (fstu, &noseg, &noval, &tstart, &tstop, &delt, ierror);
   if ( *ierror != IEOK) {
      Errfin (fdat, fstu, (void*) ibuffr, (void*) ibuffs, (void*) ibufft);
      return ;
   }

   GetJspT0(fstu, delt, &dlwtim, &dt0, ierror);
   if ( *ierror != IEOK) {
      Errfin (fdat, fstu, (void*) ibuffr, (void*) ibuffs, (void*) ibufft);
      return ;
   }

   /* skip label information */

   if ( fgets( tmpstr, sizeof( tmpstr ), fstu )  == NULL )
      {
      itrans ( ferror ( fstu), (TInt4 *)ierror) ;
      Errfin( fdat, fstu, (int *) ibuffr, (int *) ibuffs, (int *) ibufft ) ;
      return ;
      }

   /* read the JSPOST parameters from the .stu file                   */

   *nrpar = 0 ;


   for ( isub = 0; isub < noval; isub++)
      {

      /*       Read JSPOST parameter from file                        */

      fgets ( tmpstr, sizeof( tmpstr), fstu ) ;
      len = strlen( tmpstr ) ;
      if ( tmpstr[len-1] == '\n' )
         {
         tmpstr[len-1] = ' ' ;
         }
      ip = 0 ;
      while ( tmpstr[ ip ] == ' ')
         {
         ip ++ ;
         }
      tmpstr[ PARLEN] = '\0' ;
      Blanks( subnam, sizeof( subnam )) ;
      strcpy( subnam, tmpstr + ip ) ;
      if ( ferror ( fstu) || feof ( fstu) )
         {
#           ifdef PRINTHIS
         printf ( "   SUB ERROR %d %d >%s<\n", ferror(fstu), feof(fstu),
                  strerror(errno));
#           endif
         if ( feof ( fstu) )
            *ierror = IEUEOF ;
         else
            itrans ( ferror ( fstu), (TInt4 *)ierror) ;
            Errfin( fdat, fstu, (int *) ibuffr, (int *) ibuffs, (int *) ibufft ) ;
            return ;
         }
      subnam [ PARLEN] = '\0' ;

      idef = 0 ;
      while ( idef < maxipa)
         {
         if ( equal ( parin + ( idef * PARLEN), subnam))
            {
            if ( *nrpar < maxopa)
               {
               strncpy ( par + ( *nrpar * PARLEN), subnam, (size_t) PARLEN) ;
               par [ ( *nrpar * PARLEN) + PARLEN] = '\0' ;
#                 ifdef PRINTHIS
               printf ( "   para >%s<\n", par + ( *nrpar * PARLEN));
#                 endif
#                 ifdef PRINTHIS
               printf ( "   ibuffr [ %ld ] = %ld\n", *nrpar, isub);
#                 endif
               ibuffr [ *nrpar] = (int) isub ;
               (*nrpar)++ ;
               }
            else
               {
               *ierror = IEPMNY ;
               break ;
               }
            break ;
            }
         else
            idef++ ;
         }
      }

   /*  fill the loc array with the numbers of the segments       */

   *nrloc = 0 ;

#     ifdef PRINTHIS
   printf ( "   maxolo, maxilo %ld %ld \n", maxolo, maxilo);
#     endif

   for ( iseg = 0; iseg < noseg; iseg++)
      {

#        ifdef PRINTHIS
      printf ( "   segment %d ", iseg);
#        endif

      /*   Read JSPOST segment name from file      */

      fgets ( tmpstr, sizeof( tmpstr), fstu ) ;
      len = strlen( tmpstr ) ;
      if ( tmpstr[len-1] == '\n' )
         {
         tmpstr[len-1] = ' ' ;
         }
      ip = 0 ;
      while ( tmpstr[ ip ] == ' ')
         {
         ip ++ ;
         }
      tmpstr[ PARLEN] = '\0' ;

      Blanks( segnam, sizeof( segnam )) ;
      strcpy( segnam, tmpstr + ip ) ;

      if ( feof( fstu ) )
         {
         sprintf( segnam, "%-d", iseg ) ;
         }
      if ( ferror ( fstu) )
         {
         itrans ( ferror ( fstu), (TInt4 *)ierror) ;
         Errfin( fdat, fstu, (int *) ibuffr, (int *) ibuffs, (int *) ibufft ) ;
         return ;
         }

      segnam [ PARLEN] = '\0' ;

#        ifdef PRINTHIS
      printf ( "   >%s< \n", segnam);
#        endif

      idef = 0 ;
      while ( idef < maxilo)
         {
#           ifdef PRINTHIS
         printf ( "   idef, locin %ld >%s<\n", idef, locin + ( idef * PARLEN));
#           endif
         if ( equal ( locin + ( idef * PARLEN), segnam))
            {
#              ifdef PRINTHIS
            printf ( "   equal, nrloc %ld \n", *nrloc);
#              endif
            if ( *nrloc < maxolo)
               {
               strncpy ( loc + ( *nrloc * PARLEN), segnam, (size_t) PARLEN) ;
               loc [ ( *nrloc * PARLEN) + PARLEN] = '\0' ;
#                 ifdef PRINTHIS
               printf ( "   ibuffs [ %ld ] = %ld\n", *nrloc, iseg);
#                 endif
               ibuffs [ *nrloc] = (int) iseg ;
               (*nrloc)++ ;
               }
            else
               {
               *ierror = IELMNY ;
               break ;
               }
            break ;
            }
         else
            idef++ ;
         }
      if (( *nrloc == maxolo) && ( *ierror == IELMNY)) break ;
      }

   /* now loop over the timesteps from tstart to tstop */

   *nrtim = 0 ;
   tcur = tstart ;
   ist  = 0 ;

   while ( tcur <= tstop )
      {
      ctime = (TInt4) ( tcur*dt0 ) ;

      /* construct new julian date */
      /* Hopelessly convoluted and twisted
      ftime = AddToJulian( dlwtim, lng0, lng0, lng0, ctime ) ;
      */
      ftime = dlwtim + tcur ;

      /* check if it is still possible that we find something */

      some_lower = FALSE ;
      for ( itim = 0; itim < maxiti; itim++)
         {
         if ( ftime <= *(timin + 2*itim + 1)) some_lower = TRUE ;
         }
      if ( ! some_lower) break ;

      for ( itim = 0; itim < maxiti; itim++)
         {
         if (( ftime >= *(timin + 2*itim)) &&
               ( ftime <= *(timin + 2*itim + 1)))
            {
            if ( *nrtim < maxoti)
               {
               *(tim + *nrtim) = ftime ;
               ibufft[ *nrtim ] = (int) ist ;
               (*nrtim)++ ;
               }
            else
               {
               *nrtim  = maxoti ;
               *ierror = IETMNY ;
               break ;
               }
            } /* timin1 < ftime < timin2  */
         }  /* for itim */
         tcur = tcur+delt ;
         ist++ ;
      }

   notim = ist ;

   /* now read the data from the .pst file */

   for ( iloc = 0; iloc < *nrloc; iloc++)
         {

         /* Read the number of the segment from the array */

         iseg = (TInt4) ibuffs [ iloc] ;
#         ifdef PRINTHIS
         printf ( "  iseg nr %ld  ", iseg) ;
#         endif

         for ( itim = 0; itim < *nrtim; itim++)
            {

            /* Get the time index number */

            ist = (TInt4) ibufft [itim] ;

#             ifdef PRINTHIS
            printf ( "  time nr %ld  ", ist) ;
#             endif

            for ( ipar = 0; ipar < *nrpar; ipar++)
               {
               isub = (TInt4) ibuffr [ ipar] ;
#                 ifdef PRINTHIS
               printf ( "  isub nr %ld ", isub) ;
               printf ( "  seek nr %ld  ", file_pos + 4 + 4 * (( iseg * nsub) + isub)) ;
#                 endif
               if ( fseek ( fdat, 4 * (( iseg * noval * notim) + ist * noval + isub), SEEK_SET))
                  {
                  itrans ( ferror (fdat), (TInt4 *)ierror) ;
                  Errfin( fdat, fstu, (int *) ibuffr, (int *) ibuffs, (int *) ibufft ) ;
                  return ;
                  }

               items_read = fread ( &val, (size_t) 1, (size_t) 4, fdat) ;
               if ( ferror ( fdat) | feof ( fdat) | ( items_read != 4))
                  {
#                    ifdef PRINTHIS
                  printf ( "\n  VAL ERROR %d >%s<\n",
                              ferror(fdat), strerror(errno));
#                    endif
                  itrans ( ferror ( fdat), (TInt4 *)ierror) ;
                  Errfin( fdat, fstu, (int *) ibuffr, (int *) ibuffs, (int *) ibufft ) ;
                  return ;
                  }
#                 ifdef PRINTHIS
               printf ( "   VAL: %f\n", val) ;
#                 endif

               *(values + (itim * maxolo * maxopa + ipar * maxolo + iloc)) = val ;

               }   /* for ipar */
            }  /* for itim */
         } /* for iloc */
   /* free buffers */
   Errfin( fdat, fstu, (int *) ibuffr, (int *) ibuffs, (int *) ibufft ) ;

   return ;
   }


/*--------------------------------------------------------------------
** SUBROUTINE Get Dimensions from JSPOST file
*---------------------------------------------------------------------
*/
void FUNTYPE ODSGetDimJSP ( char *fname,
                   TInt4 *ftype,
                    char *dim,
		   TInt4  pardep,
		   TInt4  timdep,
		   TInt4  locdep,
                   TInt4 *ndim,
                   TInt4 *ierror,
		    char *option)

/*-----------------------------------------------------------------------*/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       dim     -          I   Dimension wanted (loc, par, tim)         */
/*       fname   -          I   Full filename, including extension.      */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       ierror  -          O   Errorcode. See ods.h for definitions.    */
/*       ndim    4          O   dimension. [0]=n-dim [1]=x [2]=y [3]=z   */
/*                                                                       */
/*-----------------------------------------------------------------------*/
{
    FILE    *fdat, *fstu ;
    TInt4   noval, noseg ;
    double  tstart, tstop, delt ;

    JspOpen (fname, &fdat, &fstu, ierror);
    if ( *ierror != IEOK) {
        return ;
    }

   /* read number of parameters, locations and times */
    GetJspRunId (fstu, &noseg, &noval, &tstart, &tstop, &delt, ierror);
    if ( *ierror != IEOK) {
        Errfin (fdat, fstu, NULL, NULL, NULL);
        return ;
    }

    *ierror = IEOK ;

    switch (dim[0]) {
        case 'p':  /* par */
        case 'P':
        ndim[0] = (TInt4)1;
        ndim[1] = noval; /* nr of substances */
        break;

        case 'l':  /* loc */
        case 'L':
        ndim[0] = (TInt4)1;
        ndim[1] = noseg; /* nr of locations/segments */
        break;

        case 't':  /* tim */
        case 'T':
        ndim[0] = (TInt4)1;
        ndim[1] = (TInt4) ((tstop-tstart)/delt + 1.0); /* nr of timesteps */
        break;

        default:
        *ierror = IEOTHR;
        Errfin( fdat, fstu, NULL, NULL, NULL ) ;
        return;
    }

    Errfin( fdat, fstu, NULL, NULL, NULL ) ;
    return ;
}

/*--------------------------------------------------------------------
** SUBROUTINE Get Matrix from JSPOST file
*---------------------------------------------------------------------
*/
void FUNTYPE ODSGetMatJSP ( char    *fname,
                    TInt4    *ftype,
                    TInt4    *parcod,
                     double  *tim,
                    TInt4    *loc,
                     float   *misval,
                    TInt4    *maxdim,
                     float   *values,
                    TInt4    *ierror)

/*-----------------------------------------------------------------------*/
/*                                                                       */
/*    Arguments:                                                         */
/*                                                                       */
/*       Name    Size      I/O  Description                              */
/*       ------  --------  ---  ------------------------------------     */
/*       fname   -          I   Full filename, including extension.      */
/*       ftype   -          I   type of file. see ods.h for definition.  */
/*       parcod  -          I   Parameter (code) of data to be retrieved.*/
/*       tim     3          I   date start, stop, step (Julian)          */
/*       loc     3*3        I   location of data subset to be retrieved: */
/*                                        [,0] X start, stop, step       */
/*                                        [,1] Y start, stop, step       */
/*                                        [,2] Z start, stop, step       */
/*       misval  -          I   Missing value.                           */
/*       maxdim  -          I   Size of values array.                    */
/*       values  -          O   Retrieved data.                          */
/*       ierror  -          O   Errorcode. See ods.h for definitions.    */
/*                                                                       */
/*-----------------------------------------------------------------------*/
{
    FILE    *fdat, *fstu ;
    TInt4   iloc, iseg;
    TInt4   noval, noseg, notim ;
    size_t  items_read ;
    TInt4   nsub, nseg, iftime, isub ;
    TInt4   file_pos, offs ;
    double  dlwtim, dt0, ftime, fcurtime ;
    TInt4   start_loc, end_loc, step_loc;
    float   val;
    double  tstart, tstop, delt ;
    TInt4   nrtim,
            ist,
            ctime,
            itim,
            lng0 = 0L,
            seek_err ;
    TInt4 * ibufft ;
    double  tcur,
            delta ;

   /* Set all out-matrix elements to Missing_Value */

    for ( iloc = 0; iloc < *maxdim; iloc++) {
       *(values + (iloc)) = *misval ;
    }

   /* open files */
    JspOpen (fname, &fdat, &fstu, ierror);
    if ( *ierror != IEOK) {
        return ;
    }

   /* read number of parameters, locations and times */
    GetJspRunId (fstu, &noseg, &noval, &tstart, &tstop, &delt, ierror);
    if ( *ierror != IEOK) {
        Errfin (fdat, fstu, NULL, NULL, NULL);
        return ;
    }

    *ierror = IEOK ;

    /* reserve buffer for time indices */
    notim = (TInt4) ((tstop-tstart)/delt + 1.0); /* nr of timesteps */
    ibufft = (TInt4 *) malloc( (size_t) (notim * sizeof( TInt4 )) ) ;

   /* convert julian dates into indices on data file */
    GetJspT0 (fstu, delt, &dlwtim, &dt0, ierror);
    delta = 1.0e-3 * ( dt0 / 86400.0 ) * delt ;

   /* now loop over the timesteps in the file
      Note:
      There should be an offset of -1, because the location index starts
      at 1.
   */

    start_loc = loc[0]-1;
    end_loc   = loc[1]-1;
    step_loc  = loc[2];
    isub      = *parcod - 1;
    offs      = 0;
    iseg = start_loc;
    nsub = noval;

    ftime     = -1.0;
    fcurtime  = tim[0];

    /* read time indices from .stu file */
    nrtim = 0 ;
    tcur = tstart ;
    ist  = 0 ;

    while ( tcur <= tstop )
       {
       ctime = (TInt4)( tcur*dt0 ) ;

       /* construct new julian date */
       /* Hopelessly convoluted and twisted
       ftime = AddToJulian( dlwtim, lng0, lng0, lng0, ctime ) ;
       */
       ftime = dlwtim + tcur ;

       fcurtime  = tim[0] ;
       for ( itim = 0; fcurtime < tim[1] && fcurtime <= ftime+delta; itim++)
          {
          if ( fabs( ftime-fcurtime) < delta )
             {
             ibufft[ nrtim ] = (int) ist ;
             (nrtim)++ ;
             }
             /* Was:
             fcurtime += tim[2] ;
             */
             fcurtime = tim[0] + itim * tim[2] ;
          }  /* for itim */
          /* Was:
          tcur = tcur + delt ;
             ---- this causes numerical inaccuracies
          */
          ist++ ;
          tcur = tstart + ist * delt ;
       }

    /* now read the data from the .pst file */

    for ( iloc = start_loc ; (iloc <= end_loc); iloc += step_loc )
    {
       for ( itim = 0; itim < nrtim; itim++)
       {

          /* Get the time index number */

          ist = (TInt4) ibufft [itim] ;
          file_pos = 4 * ((iloc*noval*notim) + ist*noval + isub);
          if ( ( seek_err = fseek ( fdat, file_pos, SEEK_SET) ) )
          {
              itrans ( ferror (fdat), (TInt4 *) ierror) ;
              Errfin( fstu, fdat, (void *) ibufft, NULL, NULL ) ;
              return ;
          }
          items_read = fread ( &val, (size_t) 1, (size_t) 4, fdat) ;
          if ( ferror ( fdat) | feof ( fdat) | ( items_read != 4))
          {
#             ifdef PRINTHIS
              printf ( "\n  VAL ERROR %d >%s<\n",
                               ferror(fdat), strerror(errno));
#             endif
              itrans ( ferror ( fdat), (TInt4 *) ierror) ;
              Errfin( fstu, fdat, (void *) ibufft, NULL, NULL ) ;
              return ;
          }

          if ( offs >= *maxdim )
          {
              *ierror = IETMNY ;
              break ;
          }
          if ( val == -999.0 )
          {
             val = *misval ;
          }
          *(values + (offs++)) = val ;
       }
    }

    Errfin( fdat, fstu, (void *) ibufft, NULL, NULL );
    return ;
}

/*------------------------------------------------------------------------
 * subroutine for opening a set of jspost files
 *       returns IEOK on success, or IENOFI if opening either file failed
 *------------------------------------------------------------------------
 */
void FUNTYPE JspOpen( char *fname,
                      FILE **fd,
                      FILE **fs,
                     TInt4 *ierror)
{
    char   ftype [] = "rb" ;
    char   filstu [ODS_FILNAMLEN] ;
    char   *ipos;

    *ierror = IEOK;

   /* create control file name by changing filename extension to .stu */
    strcpy( filstu, fname ) ;
    ipos  = strstr( filstu, ".pst" );
    if (ipos == NULL) {
        *ierror = IETYPE;
        return ;
    }

   /* open data file */
    *fd = fopen ( fname, ftype) ;
    if ( *fd == NULL )  {
        *ierror = IENOFI ;
        return;
    }

    strcpy( ipos+1, "stu" ) ;

   /* open control file */
    *fs = fopen ( filstu, ftype) ;

    if ( *fs == NULL )  {
        fclose ( *fd);
        *ierror = IENOFI ;
    }

    return;
}

/*------------------------------------------------------------------------
 * subroutine for retrieving runID from header of JSPost .stu file
 *   file should already be open
 *------------------------------------------------------------------------
 */
static void GetJspRunId (FILE *fp,
                        TInt4 *nseg,
                        TInt4 *nval,
                        double *starttm,
                        double *stoptm,
                        double *delttm,
                        TInt4 *ierror)
{
    char tmpstr[150];

    *ierror = IEOK;
    memset (tmpstr, 0, 150);

   /* get noseg, noval and start/stop/step times  */

    fgets ( tmpstr, 6, fp ) ;
    *nseg = atol( tmpstr ) ;

    fgets ( tmpstr, sizeof(tmpstr), fp ) ;
    tmpstr[5] = '\0' ;
    *nval = atol( tmpstr ) ;

    if ( ferror ( fp) | feof ( fp) )  {
        itrans ( ferror ( fp ), (TInt4 *)ierror) ;
        return ;
    }

   /*   There is no foolproof way to check if this really is a JSPOST
   **   .stu file, but if the following holds, it certainly is no
   **   JSPOST file.
   */
    if (*nval < 1 || *nseg < 1)  {
        *ierror = IETYPE ;
        return ;
    }

    tmpstr[10] = '\0' ;
    fgets ( tmpstr, 11, fp ) ;
    *starttm = (double) atof( tmpstr ) ;
    fgets ( tmpstr, 11, fp ) ;
    *stoptm  = (double) atof( tmpstr ) ;
    fgets ( tmpstr, sizeof(tmpstr), fp ) ;
    tmpstr[10] = '\0' ;
    *delttm   = (double) atof( tmpstr ) ;

    if ( ferror ( fp) | feof ( fp) )  {
        itrans ( ferror ( fp), (TInt4 *)ierror) ;
        return ;
    }

    return;
}

/*------------------------------------------------------------------------
 * subroutine for retrieving T0 from header of JSPost .stu file
 *   file should already be open and positioned on the 3rd header line
 *   (as is done by GetJspRunID )
 *------------------------------------------------------------------------
 */
static void GetJspT0 (FILE   *fp,
                      double tdelt,
                      double *timval,
                      double *dt0,
                     TInt4   *ierror)
{
    char    tmpstr[150];
    char    timid [ 41] ;
    TInt4   iyear0, imonth0, iday0, ihour0, imin0, isec0, idt0 ;

    *ierror = IEOK;

   /* skip 3rd line of .stu header */
    if ( fgets( tmpstr, sizeof( tmpstr ), fp )  == NULL )
    {
        itrans ( ferror ( fp), (TInt4 *)ierror) ;
        return ;
    }

   /* read header with T0 info */
    fgets ( tmpstr, sizeof( tmpstr ), fp );
    tmpstr[80] = '\0' ;
    strcpy( timid, tmpstr + 40 ) ;

    if ( ferror ( fp) | feof ( fp) )
    {
        itrans ( ferror ( fp), (TInt4 *)ierror) ;
        return ;
    }

   /* read next line as well: the unit of time as a string */
    fgets ( tmpstr, sizeof( tmpstr ), fp );

    if ( ferror ( fp) | feof ( fp) )
    {
        itrans ( ferror ( fp), (TInt4 *)ierror) ;
        return ;
    }

    *timval = 0.0 ;
    *dt0    = 86400.0 ;

   /*   t0= 1993.12.31 12:59:59 (scu =12345678 )  */
   /*   01234567890123456789012345678901234567890  */
   /*             1111111111222222222233333333334  */

    timid [  8] = '\0' ;
    timid [ 11] = '\0' ;
    timid [ 14] = '\0' ;
    timid [ 17] = '\0' ;
    timid [ 20] = '\0' ;
    timid [ 23] = '\0' ;
    timid [ 38] = '\0' ;
    iyear0  = atol ( timid + 4) ;
    imonth0 = atol ( timid + 9) ;
    iday0   = atol ( timid +12) ;
    ihour0  = atol ( timid +15) ;
    imin0   = atol ( timid +18) ;
    isec0   = atol ( timid +21) ;
    idt0    = atol ( timid +30) ;

    *dt0    = (double) idt0 ; /* Factor given in seconds */

   /* If we recognise the time unit, then use this as an indication of time */
    if ( strncmp( tmpstr , "sec"  , 3 ) == 0 ) *dt0 =     1.0 ;
    if ( strncmp( tmpstr , "min"  , 3 ) == 0 ) *dt0 =    60.0 ;
    if ( strncmp( tmpstr , "hour" , 4 ) == 0 ) *dt0 =  3600.0 ;
    if ( strncmp( tmpstr , "day"  , 3 ) == 0 ) *dt0 = 86400.0 ;
    if ( strncmp( tmpstr , "uur"  , 3 ) == 0 ) *dt0 =  3600.0 ;
    if ( strncmp( tmpstr , "dag"  , 3 ) == 0 ) *dt0 = 86400.0 ;

    if (( iyear0 < 1800) || ( imonth0 < 1) || ( iday0 < 1) ||
         ( ihour0 <    0) || ( imin0   < 0) || ( isec0 < 0) ||
         ( idt0   <    0))
    {
/*      *timval = 0.0 ; */
       *dt0    = 1.0 ;
       iyear0  = 1900L;
       imonth0 = 1L;
       iday0   = 1L;
       ihour0  = 0L;
       imin0   = 0L;
       isec0   = 0L;

    }

    /* Problem: System clock unit is not always correct for .pst files */
    /* But that can not be solved! */

    julian ( (TInt4 *)&iyear0, (TInt4 *)&imonth0, (TInt4 *)&iday0,
             (TInt4 *)&ihour0, (TInt4 *)&imin0, (TInt4 *)&isec0,
             (TReal8 *)timval ) ;
    return ;
}


/*------------------------- end of jspost.c --------------------------*/

