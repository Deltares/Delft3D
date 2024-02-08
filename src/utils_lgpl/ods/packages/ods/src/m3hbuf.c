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
 *   m3hbuf.c  - allocate buffer for ODS_TRISULA_HIS_NEFIS_hismat
 *                           and for ODS_TRISULA_MAP_NEFIS_mapmat
 *
 *   Eric Verschuur
 */

/*
 *  $Author: Markus $
 *  $Date: 1-04-03 10:52 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/m3hbuf.c,v $
*/
/*
 */



#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include "ods.h"
#include "odsmodel.h"

#ifndef max
#define max(a,b)        ((a) > (b) ? (a) : (b))
#endif


/*----------------------------------------------------------------------
 * compute buffersize and return pointer to allocated buffer
 *----------------------------------------------------------------------
 */

void ODS_TRISULA_NEFIS_hismat_buffer(
    char *   fname,
    TInt4 *  parcod,
    float ** zbuffs,
    TInt4 *  ierror )
{
    BInt4    deffds[2997], datfds[999], uindex[1][3];
    BInt4    usrord, buflen;
    TInt4    nostat, ntruv, lmax, kmax, lmaxd, buffersize;
    TInt4    lstci, ltur;
    char     grp_par_nam[17], elmnam[17];
    TInt4    dummy;
    TInt4    ftype = ODS_TRISULA_HIS_NEFIS;

    *ierror = IEOK;

    OPNNEF(fname, &ftype, datfds, deffds, ierror);
    if ( *ierror != IEOK )
        return;

    /* get number of array dimensions */
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "his-const" );
    strcpy( elmnam, "NOSTAT" );
    *ierror = Getelt( deffds, grp_par_nam, elmnam,
                      uindex, &usrord, &buflen, &nostat );
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    strcpy (elmnam, "NTRUV");
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
                   uindex, &usrord, &buflen, &ntruv );
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    lmax = 0L;
    lstci = 0L;
    ltur = 0L;
    strcpy (elmnam, "LMAX");
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
                   uindex, &usrord, &buflen, &lmax );
    if ( *ierror != IEOK )
    {
        strcpy (elmnam, "LSTCI");
        *ierror = Getelt (deffds, grp_par_nam, elmnam,
                       uindex, &usrord, &buflen, &lstci );
        if ( *ierror != IEOK )
        {
            /* close NEFIS files */
            CloseNefisFiles( fname, datfds, deffds, &dummy );
            return;
        }
        strcpy (elmnam, "LTUR");
        *ierror = Getelt (deffds, grp_par_nam, elmnam,
                       uindex, &usrord, &buflen, &ltur );
        if ( *ierror != IEOK )
        {
            /* close NEFIS files */
            CloseNefisFiles( fname, datfds, deffds, &dummy );
            return;
        }
        lmax = lstci + ltur;
    }
    lmaxd = max(1L,lmax);

    strcpy (elmnam, "KMAX");
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
               uindex, &usrord, &buflen, &kmax );
    if ( *ierror != IEOK )
    {
        kmax = 1L;
        *ierror = IEOK;
    }

    /* calculate buffer size */

    buffersize = -1L;
    if ( (*parcod >=  1 && *parcod <= 32) ||
         (*parcod >= 38 && *parcod <= 43) )
    {
       if ( (*parcod == 15 || *parcod == 32) )
       {
           buffersize = nostat * lmaxd * (kmax+1) ;
       }
       else
       {
           buffersize = nostat * lmaxd * kmax ;
       }
    }
    else
    {
        if ( (*parcod >= 33 && *parcod <= 34) ||
             (*parcod >= 47 && *parcod <= 55) ||
             (*parcod >= 57 && *parcod <= 65) ||
             (*parcod >= 67 && *parcod <= 75) )
            buffersize = ntruv * lmaxd;
        else
            *ierror = IEINFO;
    }

    if (buffersize > 0)
    {
        *zbuffs = (float *) malloc( buffersize * sizeof(float) );
        if (*zbuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );
}

void ODS_TRISULA_NEFIS_mapmat_buffer(
    char *   fname,
    TInt4 *  parcod,
    TInt4 ** ibuffs,
    float ** zbuffs,
    TInt4 *  ierror )
{
    BInt4    deffds[2997], datfds[999], uindex[1][3];
    BInt4    usrord, buflen;
    TInt4    nmax, mmax, lmax, kmax, maxland3, noroco;
    TInt4    lstci, ltur;
    TInt4    ibuffersize, zbuffersize;
    char     grp_par_nam[17], elmnam[17];
    TInt4    dummy;
    TInt4    ftype = ODS_TRISULA_HIS_NEFIS;

    *ierror = IEOK;

    OPNNEF(fname, &ftype, datfds, deffds, ierror);
    if ( *ierror != IEOK )
        return;

    /* get number of array dimensions */
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "map-const" );
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

    lmax = 0L;
    lstci = 0L;
    ltur = 0L;
    strcpy (elmnam, "LMAX");
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
                   uindex, &usrord, &buflen, &lmax );
    if ( *ierror != IEOK )
    {
        strcpy (elmnam, "LSTCI");
        *ierror = Getelt (deffds, grp_par_nam, elmnam,
                       uindex, &usrord, &buflen, &lstci );
        if ( *ierror != IEOK )
        {
            /* close NEFIS files */
            CloseNefisFiles( fname, datfds, deffds, &dummy );
            return;
        }
        strcpy (elmnam, "LTUR");
        *ierror = Getelt (deffds, grp_par_nam, elmnam,
                       uindex, &usrord, &buflen, &ltur );
        if ( *ierror != IEOK )
        {
            /* close NEFIS files */
            CloseNefisFiles( fname, datfds, deffds, &dummy );
            return;
        }
        lmax = lstci + ltur;
    }
    maxland3 = max(3L,lmax);

    strcpy (elmnam, "KMAX");
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
               uindex, &usrord, &buflen, &kmax );
    if ( *ierror != IEOK )
    {
        kmax = 1L;
        *ierror = IEOK;
    }

    strcpy (elmnam, "NOROCO");
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
                   uindex, &usrord, &buflen, &noroco );
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    /* calculate buffer size zbuffs */
    /* see ods_tri_nef_map_mat for details */

    zbuffersize = 8L * nmax * mmax + (kmax + 2L) +
                  maxland3 * nmax * mmax * (kmax + 1L)  ;

    if (zbuffersize > 0)
    {
        *zbuffs = (float *) malloc( zbuffersize * sizeof(float) );
        if (*zbuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    /* calculate buffer size ibuffs */
    /* see ods_tri_nef_map_mat for details */

    ibuffersize = 7L * nmax * mmax + 5L * noroco ;

    if (ibuffersize > 0)
    {
        *ibuffs = (TInt4 *) malloc( ibuffersize * sizeof(TInt4) );
        if (*ibuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );
}


TVoid ODSGetGrdTriNefMap (
                      TString fname,
                      TInt4   *ftype,
                      TInt4   *indloc,
                      TInt4   *indx,
                      TInt4   *nocell,
                      TInt4   *igisty,
                      TInt4   *ierror)

/*--------------------------------------------------------------------*/
/*                                                                    */
/* Function: Get Index table from TRISULA_NEFIS_MAP file              */
/*                                                                    */
/* Arguments:                                                         */
/*                                                                    */
/*    Name    Size      I/O  Description                              */
/*    ------  --------  ---  ------------------------------------     */
/*    fname   -          I   Full filename, including extension.      */
/*    ftype   -          I   type of file. see ods.h for definition.  */
/*    indloc  3*3        I   location of data subset to be retrieved: */
/*                                     [0..2,0] X start, stop, step   */
/*                                     [0..2,1] Y start, stop, step   */
/*                                     [0..2,2] Z start, stop, step   */
/*    indx    *          O   Retrieved indices                        */
/*    nocell  -          O   total number of computational cells      */
/*    igisty  -          O   Grid type / GIS object                   */
/*                           IGCURV - curvilinear orthogonal          */
/*                           IGFIN3 - finite elements                 */
/*    ierror  -          O   Errorcode. See ods.h for definitions.    */
/*                                                                    */
/*--------------------------------------------------------------------*/
{
    TInt4   i, j, l ;
    TInt4   *ibuffs ;

    BInt4    deffds[2997], datfds[999], uindex[1][3];
    BInt4    usrord, buflen;
    TInt4    nmax, mmax ;
    TInt4    ibuffersize ;
    char     grp_par_nam[17], elmnam[17];
    TInt4    dummy;

    *ierror = IEOK ;
    *igisty = IGCURV ;
    *nocell = 0 ;

    OPNNEF(fname, ftype, datfds, deffds, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    /* get number of array dimensions */
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "map-const" );
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

    /* calculate buffer size ibuffs */
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

    /* get mask arry for zeta points (time independent)
     *     KCS  = 0 inactive point
     *          = 1 active point
     *          = 2 open boundary point
     */
    strcpy (elmnam, "KCS");
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
           case 0L :
               indx[l] = 0 ; /* inactive point */

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

} /* End of ODSGetGrdTriNefMap */


TVoid ODSGetGrdMorNefCom (
                      TString fname,
                      TInt4   *ftype,
                      TInt4   *indloc,
                      TInt4   *indx,
                      TInt4   *nocell,
                      TInt4   *igisty,
                      TInt4   *ierror)

/*--------------------------------------------------------------------*/
/*                                                                    */
/* Function: Get Index table from MORSYS_NEFIS_COM file              */
/*                                                                    */
/* Arguments:                                                         */
/*                                                                    */
/*    Name    Size      I/O  Description                              */
/*    ------  --------  ---  ------------------------------------     */
/*    fname   -          I   Full filename, including extension.      */
/*    ftype   -          I   type of file. see ods.h for definition.  */
/*    indloc  3*3        I   location of data subset to be retrieved: */
/*                                     [0..2,0] X start, stop, step   */
/*                                     [0..2,1] Y start, stop, step   */
/*                                     [0..2,2] Z start, stop, step   */
/*    indx    *          O   Retrieved indices                        */
/*    nocell  -          O   total number of computational cells      */
/*    igisty  -          O   Grid type / GIS object                   */
/*                           IGCURV - curvilinear orthogonal          */
/*                           IGFIN3 - finite elements                 */
/*    ierror  -          O   Errorcode. See ods.h for definitions.    */
/*                                                                    */
/*--------------------------------------------------------------------*/
{
    TInt4     i, j, k, l  ;
    TInt4   * ibuffs      ;

    BInt4     deffds[2997], datfds[999], uindex[1][3],elmdms[5];
    BInt4     usrord, buflen, nbytsg, elmndm;
    TInt4     nmax, mmax  ;
    TInt4     ibuffersize ;
    char      grp_par_nam[17], elmnam[17];
    char      elmtyp[10], elmqty[17], elmunt[17], elmdes[65];
    TInt4     dummy;

    switch (*ftype)
{
    case 5001L:

    *ierror = IEOK ;
    *igisty = IGCURV ;
    *nocell = 0 ;

    OPNNEF(fname, ftype, datfds, deffds, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    /* get number of array dimensions */
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

    /* calculate buffer size ibuffs */
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

    /* get mask arry for zeta points (time independent)
     *     KCS  = 0 inactive point
     *          = 1 active point
     *          = 2 open boundary point
     */
    strcpy (elmnam, "KCS");
    strcpy( grp_par_nam, "KENMCNST" );
    buflen = sizeof (TInt4) * mmax * nmax ;
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
                   uindex, &usrord, &buflen, ibuffs) ;
    if ( *ierror != IEOK )
    {
         strcpy (elmnam, "CODW");
         strcpy( grp_par_nam, "TEMPOUT" );
         buflen = sizeof (TInt4) * mmax * nmax ;
         *ierror = Getelt (deffds, grp_par_nam, elmnam,
                   uindex, &usrord, &buflen, ibuffs) ;
    }
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

    break;

    case 5002L:
    break;

    case 5003L:

    *ierror = IEOK ;
    *igisty = IGVERT ;
    *nocell = 0 ;

    OPNNEF(fname, ftype, datfds, deffds, ierror);
    if ( *ierror != IEOK )
    {
        return;
    }

    /* get number of array dimensions */
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    elmndm = 5;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "map-const" );
    strcpy( elmnam, "Xb" );
    *ierror = Inqelm( deffds, elmnam, elmtyp,
                     &nbytsg, elmqty, elmunt,
                      elmdes, &elmndm, elmdms) ;
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    mmax=elmdms[1];
    nmax=elmdms[0];

    /* calculate buffer size ibuffs */

    *nocell = mmax * nmax ;

    /* get mask arry for hiswa bottom points
     *          = 1 active point
     */

    for ( i=0 ; i<nmax ; i++ )
    {
       for ( j=0 ; j<mmax ; j++ )
       {
           l =  i * mmax + j ;
                 /* Note: counting starts at 1 as in FORTRAN */
               indx[l] = l + 1 ; /* active computational point */


       } /* end for j */
    } /* end for i */


    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );

    break;

    case 5004L:

    *ierror = IEOK ;
    *igisty = IGVERT ;
    *nocell = 0 ;

    OPNNEF(fname, ftype, datfds, deffds, ierror);

    if ( *ierror != IEOK )
    {
        return;
    }

    /* get number of array dimensions */
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    elmndm = 5;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "map-const" );
    strcpy( elmnam, "XP" );
    *ierror = Inqelm( deffds, elmnam, elmtyp,
                     &nbytsg, elmqty, elmunt,
                      elmdes, &elmndm, elmdms) ;
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    mmax=elmdms[1];
    nmax=elmdms[0];

    /* calculate buffer size ibuffs */
    ibuffersize = mmax * nmax ;
    *nocell     = ibuffersize ;

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
       *ierror = IEINFO ;
    }

    /* get mask arry for hiswa bottom points
     *          = 1 active point
     */

    strcpy (elmnam, "CODE");
    strcpy( grp_par_nam, "map-series" );
    buflen = sizeof (TInt4) * mmax * nmax ;
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
                 uindex, &usrord, &buflen, ibuffs) ;
    if ( *ierror == IEOK )
    {
       for ( i=0 ; i<nmax ; i++ )
       {
          for ( j=0 ; j<mmax ; j++ )
          {
              l =  i * mmax + j ;
                    /* Note: counting starts at 1 as in FORTRAN */
              indx[l] = l + 1 ; /* active computational point */
              if ( i == 0      ) indx[l] = 0 ;
              if ( j == 0      ) indx[l] = 0 ;
              if ( i == nmax-1 ) indx[l] = 0 ;
              if ( j == mmax-1 ) indx[l] = 0 ;
          } /* end for j */
       } /* end for i */

       for ( i=1 ; i<nmax ; i++ )
       {
          for ( j=1 ; j<mmax ; j++ )
          {
              l =  i * mmax + j ;
              k =  j * nmax + i ; /* Never mind this - this is correct! */
                    /* Note: counting starts at 1 as in FORTRAN */
              if ( ibuffs[k] <= 0 )
              {
                 indx[l] = 0 ;
              }
          } /* end for j */
       } /* end for i */
    }

    free( ibuffs ) ;

    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );

    break;
  }

    return;
}
TVoid ODSGetGrdPhidias (
                      TString fname,
                      TInt4   *ftype,
                      TInt4   *indloc,
                      TInt4   *indx,
                      TInt4   *nocell,
                      TInt4   *igisty,
                      TInt4   *ierror)
/*--------------------------------------------------------------------*/
/*                                                                    */
/* Function: Get Index table from MORSYS_NEFIS_COM file              */
/*                                                                    */
/* Arguments:                                                         */
/*                                                                    */
/*    Name    Size      I/O  Description                              */
/*    ------  --------  ---  ------------------------------------     */
/*    fname   -          I   Full filename, including extension.      */
/*    ftype   -          I   type of file. see ods.h for definition.  */
/*    indloc  3*3        I   location of data subset to be retrieved: */
/*                                     [0..2,0] X start, stop, step   */
/*                                     [0..2,1] Y start, stop, step   */
/*                                     [0..2,2] Z start, stop, step   */
/*    indx    *          O   Retrieved indices                        */
/*    nocell  -          O   total number of computational cells      */
/*    igisty  -          O   Grid type / GIS object                   */
/*                           IGCURV - curvilinear orthogonal          */
/*                           IGFIN3 - finite elements                 */
/*    ierror  -          O   Errorcode. See ods.h for definitions.    */
/*                                                                    */
/*--------------------------------------------------------------------*/
{
    TInt4   i, j, l ;

    BInt4    deffds[2997], datfds[999], uindex[1][3],elmdms[5];
    BInt4    usrord, buflen, nbytsg, elmndm;
    TInt4    nmax, mmax ;
    char     grp_par_nam[17], elmnam[17];
    char     elmtyp[10], elmqty[17], elmunt[17], elmdes[65];
    TInt4    dummy;

    switch (*ftype)

{

    case ODS_PHIDIAS_MAP :

    *ierror = IEOK ;
    *igisty = IGVERT ;
    *nocell = 0 ;

    OPNNEF(fname, ftype, datfds, deffds, ierror);

    if ( *ierror != IEOK )
    {
        return;
    }

    /* get number of array dimensions */
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    elmndm = 5;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "map-coord" );
    strcpy( elmnam, "XC" );
    *ierror = Inqelm( deffds, elmnam, elmtyp,
                     &nbytsg, elmqty, elmunt,
                      elmdes, &elmndm, elmdms) ;
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    mmax=elmdms[1];
    nmax=elmdms[0];

    /* calculate buffer size ibuffs */

    /* get mask arry for phidias grid points
     *          = 1 active point
     */

    *nocell = mmax * nmax ;

    for ( i=0 ; i<nmax ; i++ )
    {
       for ( j=0 ; j<mmax ; j++ )
       {
           l =  i * mmax + j ;
                 /* Note: counting starts at 1 as in FORTRAN */
               indx[l] = l + 1 ; /* active computational point */


       } /* end for j */
    } /* end for i */


    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );

    break;


    case ODS_PHIDIAS_SPECTRAL :

    *ierror = IEOK ;
    *igisty = IGVERT ;
    *nocell = 0 ;

/*********
    nmax = 1 + ( indloc[4] - indloc[3] )/indloc[5];
    mmax = 1 + ( indloc[7] - indloc[6] )/indloc[8];
*/
    nmax = 1 + ( indloc[1] - indloc[0] )/indloc[2];
    mmax = 1 + ( indloc[4] - indloc[3] )/indloc[5];

    *nocell = mmax * nmax ;

    for ( i=0 ; i<nmax ; i++ )
    {
       for ( j=0 ; j<mmax ; j++ )
       {
           l =  i * mmax + j ;
                 /* Note: counting starts at 1 as in FORTRAN */
               indx[l] = l + 1 ; /* active computational point */


       } /* end for j */
    } /* end for i */

    break;

}

    return;
} /* End of ODSGetGrdPhidias */

void ODS_MORSYS_NEFIS_commat_buffer(
    char *   fname,
    TInt4 *  parcod,
    TInt4    itype,
    TInt4 ** ibuffs,
    float ** zbuffs,
    TInt4 *  ierror )
{
    BInt4    deffds[2997], datfds[999], uindex[1][3],elmdms[5];
    BInt4    usrord, buflen, nbytsg, elmndm;
    char     grp_par_nam[17], elmnam[17];
    char     elmtyp[11], elmqty[17], elmunt[17], elmdes[65];
    TInt4    dummy;
    TInt4    nmax, mmax, lmax, kmax, maxland3, noroco;
    TInt4    ibuffersize, zbuffersize;
    TInt4    ftype ;/*= ODS_MORSYS_MAP_NEFIS;*/

    ftype=itype;

    switch(itype) {


 case 5001L:


    *ierror = IEOK;

    OPNNEF(fname, &ftype, datfds, deffds, ierror);
    if ( *ierror != IEOK )
        return;

    /* get number of array dimensions */
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

    strcpy (elmnam, "KMAX");
    *ierror = Getelt (deffds, grp_par_nam, elmnam,
                   uindex, &usrord, &buflen, &kmax );
    if ( kmax <= 0 ) kmax = 1 ;

    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }
    lmax     = 1L;
    maxland3 = max(3L,lmax);
    noroco   = 1L;

    /* calculate buffer size zbuffs */
    /* see ods_tri_nef_map_mat for details */

    zbuffersize = 5 * nmax * mmax +
                  4 * nmax * mmax * kmax  ;

    if (zbuffersize > 0)
    {
        *zbuffs = (float *) malloc( zbuffersize * sizeof(float) );
        if (*zbuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    /* calculate buffer size ibuffs */
    /* see ods_mor_nef_com_mat for details */

    ibuffersize = 7L * nmax * mmax  ;

    if (ibuffersize > 0)
    {
        *ibuffs = (TInt4 *) malloc( ibuffersize * sizeof(TInt4) );
        if (*ibuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );

    break;

  case 5002L:
  break;

 case 5003L:

    *ierror = IEOK;

   OPNNEF(fname, &ftype, datfds, deffds, ierror);
   if ( *ierror != IEOK )
   {
        return;
   }

    /* get number of array dimensions */
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    elmndm = 5;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "map-const" );
    strcpy( elmnam, "Xb" );
    *ierror = Inqelm( deffds, elmnam, elmtyp,
                     &nbytsg, elmqty, elmunt,
                      elmdes, &elmndm, elmdms) ;
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    mmax=elmdms[1];
    nmax=elmdms[0];
    lmax     = 1L;
    maxland3 = max(3L,lmax);
    kmax     = 1L;
    noroco   = 1L;

    /* calculate buffer size zbuffs */
    /* see ods_tri_nef_map_mat for details */

    zbuffersize = 8L * nmax * mmax + kmax +
                  maxland3 * nmax * mmax * kmax  ;

    if (zbuffersize > 0)
    {
        *zbuffs = (float *) malloc( zbuffersize * sizeof(float) );
        if (*zbuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    /* calculate buffer size ibuffs */
    /* see ods_mor_nef_com_mat for details */

    ibuffersize = 7L * nmax * mmax  ;

    if (ibuffersize > 0)
    {
        *ibuffs = (TInt4 *) malloc( ibuffersize * sizeof(TInt4) );
        if (*ibuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );

    break;

 case 5004L:

    *ierror = IEOK;

    OPNNEF(fname, &ftype, datfds, deffds, ierror);
    if ( *ierror != IEOK )
   {
        return;
   }

    /* get number of array dimensions */
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    elmndm = 5;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "map-const" );
    strcpy( elmnam, "XP" );
    *ierror = Inqelm( deffds, elmnam, elmtyp,
                     &nbytsg, elmqty, elmunt,
                      elmdes, &elmndm, elmdms) ;
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    mmax=elmdms[1];
    nmax=elmdms[0];
    lmax     = 1L;
    maxland3 = max(3L,lmax);
    kmax     = 1L;
    noroco   = 1L;

    /* calculate buffer size zbuffs */
    /* see ods_tri_nef_map_mat for details */

    zbuffersize = 8L * nmax * mmax + kmax +
                  maxland3 * nmax * mmax * kmax  ;

    if (zbuffersize > 0)
    {
        *zbuffs = (float *) malloc( zbuffersize * sizeof(float) );
        if (*zbuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    /* calculate buffer size ibuffs */
    /* see ods_mor_nef_com_mat for details */

    ibuffersize = 7L * nmax * mmax  ;

    if (ibuffersize > 0)
    {
        *ibuffs = (TInt4 *) malloc( ibuffersize * sizeof(TInt4) );
        if (*ibuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );

    break;
  }
}
void ODS_PHIDIAS_commat_buffer(
    char *   fname,
    TInt4 *  parcod,
    TInt4    itype,
    TInt4 ** ibuffs,
    float ** zbuffs,
    TInt4 *  ierror )
{
    BInt4    deffds[2997], datfds[999], uindex[1][3],elmdms[5];
    BInt4    usrord, buflen, nbytsg, elmndm;
    TInt4    nrelm, maxelm, ipar;
    char     grp_par_nam[17], elmnam[50][17];
    char     elmtyp[10], elmqty[17], elmunt[17], elmdes[65];
    TInt4    dummy;
    TInt4    nmax, mmax, lmax, kmax, maxland3, noroco;
    TInt4    ibuffersize, zbuffersize;
    TInt4    ftype ;

    maxelm = 50;
    ftype=itype;

    switch(itype) {
 case ODS_PHIDIAS_HISTORY :

    *ierror = IEOK;

    OPNNEF(fname, &ftype, datfds, deffds, ierror);
    if ( *ierror != IEOK )
   {
        return;
   }

    /* get number of array dimensions */
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    elmndm = 5;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "tsr-coord" );
    strcpy( elmnam[0], "XT" );
    *ierror = Inqelm( deffds, elmnam[0], elmtyp,
                     &nbytsg, elmqty, elmunt,
                      elmdes, &elmndm, elmdms) ;
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    mmax=1;
    nmax=elmdms[0];
    lmax     = 1L;
    maxland3 = max(3L,lmax);
    kmax     = 1L;
    noroco   = 1L;

    /* calculate buffer size zbuffs */
    /* see ods_tri_nef_map_mat for details */

    zbuffersize = 8L * nmax * mmax + kmax +
                  maxland3 * nmax * mmax * kmax  ;

    if (zbuffersize > 0)
    {
        *zbuffs = (float *) malloc( zbuffersize * sizeof(float) );
        if (*zbuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    /* calculate buffer size ibuffs */
    /* see ods_mor_nef_com_mat for details */

    ibuffersize = 7L * nmax * mmax  ;

    if (ibuffersize > 0)
    {
        *ibuffs = (TInt4 *) malloc( ibuffersize * sizeof(TInt4) );
        if (*ibuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );

    break;


 case ODS_PHIDIAS_MAP :

    *ierror = IEOK;

    OPNNEF(fname, &ftype, datfds, deffds, ierror);
    if ( *ierror != IEOK )
   {
        return;
   }

    /* get number of array dimensions */
    uindex[0][0] = 1;   /* start */
    uindex[0][1] = 1;   /* stop */
    uindex[0][2] = 1;   /* step */
    usrord = 1;
    elmndm = 5;
    buflen = sizeof (TInt4);
    strcpy( grp_par_nam, "map-coord" );
    strcpy( elmnam[0], "XC" );
    *ierror = Inqelm( deffds, elmnam[0], elmtyp,
                     &nbytsg, elmqty, elmunt,
                      elmdes, &elmndm, elmdms) ;
    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    mmax=elmdms[1];
    nmax=elmdms[0];
    lmax     = 1L;
    maxland3 = max(3L,lmax);
    kmax     = 1L;
    noroco   = 1L;

    /* calculate buffer size zbuffs */
    /* see ods_tri_nef_map_mat for details */

    zbuffersize = 8L * nmax * mmax + kmax +
                  maxland3 * nmax * mmax * kmax  ;

    if (zbuffersize > 0)
    {
        *zbuffs = (float *) malloc( zbuffersize * sizeof(float) );
        if (*zbuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    if ( *ierror != IEOK )
    {
        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );
        return;
    }

    /* calculate buffer size ibuffs */
    /* see ods_mor_nef_com_mat for details */

    ibuffersize = 7L * nmax * mmax  ;

    if (ibuffersize > 0)
    {
        *ibuffs = (TInt4 *) malloc( ibuffersize * sizeof(TInt4) );
        if (*ibuffs == NULL)
            *ierror = IEDISK;
    }
    else
        *ierror = IEINFO;

    /* close NEFIS files */
    CloseNefisFiles( fname, datfds, deffds, &dummy );

    break;

    case ODS_PHIDIAS_SPECTRAL :

        *ierror = IEOK ;

        OPNNEF(fname, &ftype, datfds, deffds, ierror);
        if ( *ierror != IEOK )
        {
            return;
        }
        if ( *parcod < 1000 )
        {
           nrelm = maxelm;
           strcpy( grp_par_nam, "spc-coord-01");
           *ierror = Inqcel( deffds, "spc-coord" , &nrelm, elmnam );
           if ( *ierror != IEOK )
           {
              *ierror = IEINFO;
              CloseNefisFiles( fname, datfds, deffds, &dummy );
              return;
           }

           elmndm = 5;
           *ierror = Inqelm( deffds, elmnam[*parcod], elmtyp, &nbytsg,
                            elmqty, elmunt, elmdes, &elmndm, elmdms );
           if ( *ierror != IEOK )
           {
              *ierror = IEOTHR;
              CloseNefisFiles( fname, datfds, deffds, &dummy );
              return;
           }
           nmax = elmdms[0]+1 ;
           mmax = elmdms[1] ;
        }
        else
        {
           ipar = *parcod - 1000 ;
           nrelm = maxelm ;
           strcpy( grp_par_nam, "spc-coord" );
           *ierror = Inqcel( deffds, "spc-step", &nrelm, elmnam );
           if ( *ierror != IEOK )
           {
              *ierror = IEINFO;
              CloseNefisFiles( fname, datfds, deffds, &dummy );
              return;
           }
           elmndm = 5;
           *ierror = Inqelm( deffds, elmnam[ipar], elmtyp, &nbytsg,
                            elmqty, elmunt, elmdes, &elmndm, elmdms );
           if ( *ierror != IEOK )
           {
              *ierror = IEOTHR;
              CloseNefisFiles( fname, datfds, deffds, &dummy );
              return;
           }
           nmax = elmdms[0] ;
           mmax = elmdms[1]+1 ;
        }
        lmax     = 1L;
        maxland3 = max(3L,lmax);
        kmax     = 1L;
        noroco   = 1L;

        /* calculate buffer size zbuffs */
        /* see ods_tri_nef_map_mat for details */

        zbuffersize = 8L * nmax * mmax + kmax +
                      maxland3 * nmax * mmax * kmax  ;

        if (zbuffersize > 0)
        {
            *zbuffs = (float *) malloc( zbuffersize * sizeof(float) );
            if (*zbuffs == NULL)
                *ierror = IEDISK;
        }
        else
            *ierror = IEINFO;

        if ( *ierror != IEOK )
        {
            /* close NEFIS files */
            CloseNefisFiles( fname, datfds, deffds, &dummy );
            return;
        }

        /* calculate buffer size ibuffs */
        /* see ods_mor_nef_com_mat for details */

        ibuffersize = 0  ;
        *ibuffs = NULL ;

        /* close NEFIS files */
        CloseNefisFiles( fname, datfds, deffds, &dummy );

        break;
     }
}



