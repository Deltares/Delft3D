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
 *  opnclose.c  -  ODS functions for opening and closing files
 *
 *  Eric Verschuur
 */

/*   Date:       24 Jan 1996                                          */
/*   Time:       09:23                                                */
/*   Program:    OPNCLOSE.C                                           */
/*   Version:    1.02                                                 */
/*   Programmer: Eric Verschuur                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.01 -- 24 Jan 1996 -- 09:13 -- Operating System: DOS            */
/*   1.00 -- 29 Juni 1994 -- 15:55 -- Operating System: DOS           */
/*   Project:    Open Data Structuur                                  */
/*   Module:                                                          */
/*   Function:                                                        */
/*   Comment:                                                         */
/*   Reference:                                                       */
/*   Review:                                                          */
/*
 *  $Author: Markus $
 *  $Date: 11-02-05 10:04 $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/opnclose.c,v $
 */

/**1************************ INCLUDE FILES ****************************/

#include <stdio.h>
#include <string.h>
#ifdef UNIX
#include <unistd.h>
#endif

#include "ods.h"
#include "odsmodel.h"
#include "nefis.h"
#include "portable.h"

/**2************************* LOCAL MACROS ****************************/

#define OC_CLOSED       0
#define OC_OPENED       1
#define OC_REOPENED     2
#define OC_ERROR        99

#define NEFIS_DATFDS_LEN 999
#define NEFIS_DEFFDS_LEN 2997

/* Other macros moved to opnclose.h! */

/**3************************ LOCAL TYPEDEFS ***************************/

/* Moved to opnclose.h */

/**4***************** DECLARATION LOCAL FUNCTIONS *********************/

/* OdsFilePtr              OdsOpenFile(); */
static void             OdsDoClose();
static OdsFilePtr       OdsSrchFileName();
static OdsFilePtr       OdsSrchFileProc();
static OdsFilePtr       OdsAddFile();
static void             OdsDropFile();
static TInt4            OdsGetID();
static TInt4            OdsMaxFiles();

/**5*********************** LOCAL VARIABLES ***************************/

static OdsFilePtr       filelijstkop = NULL;
static TInt4            NrOpen = 0L;

/**6**************** EXPORTED FUNCTION DEFINITIONS ********************/

/*----------------------------------------------------------------------
 * open file
 * FORTRAN interface to OdsOpenFile
 * This routine is called at the beginning of each Get... routine.
 *
 *  TString fname  Input   filename as used in Get... routines
 *  TInt4 *ftype   Input   file type as used in Get... routines (see ods.h)
 *  TInt4 *unitnr  Output  unit number of file, to be used in read/write statements
 *  TInt4 *ierror  Output  errorcode as defined in ods.h . IEOK if all went well
 *----------------------------------------------------------------------
 */

void C_openfl(fname, ftype, unitnr, ierror)
TString fname;
TInt4 *ftype;
TInt4 *unitnr;
TInt4 *ierror;
{
    OdsFilePtr  lijstptr = NULL;
    FILE *fp;

    lijstptr = OdsOpenFile(fname, ftype, &fp, ierror);
    if (lijstptr != NULL)
        *unitnr = lijstptr->unitnr;
    else
        *unitnr = -1;

    return;
}

/*----------------------------------------------------------------------
 * open file
 * Interface to OdsOpenFile for opening NEFIS files
 * This routine is called at the beginning of each Get... routine.
 * A copy of the FDS fields is kept at first Nefis Open call,
 * the contents are copied into the FDS parameters.
 *
 *  TString fname  Input   filename as used in Get... routines
 *  TInt4 *ftype   Input   file type as used in Get... routines (see ods.h)
 *  BInt4 *datfds  Output  Nefis datafile control blok
 *  BInt4 *deffds  Output  Nefis definition file control blok
 *  TInt4 *ierror  Output  errorcode as defined in ods.h . IEOK if all went well
 *----------------------------------------------------------------------
 */

void OPNNEF (fname, ftype, datfds, deffds, ierror)
TString fname;
TInt4   *ftype;
BInt4   *datfds;
BInt4   *deffds;
TInt4   *ierror;
{
    OdsFilePtr  lijstptr = NULL;
    FILE *fp;

    lijstptr = OdsOpenFile(fname, ftype, &fp, ierror);
    if (lijstptr != NULL)
    {
        memcpy (datfds, lijstptr->datfds, NEFIS_DATFDS_LEN * sizeof(BInt4));
        memcpy (deffds, lijstptr->deffds, NEFIS_DEFFDS_LEN * sizeof(BInt4));
    }

    return;
}


/*----------------------------------------------------------------------
 * open file
 * This routine can be called at the beginning of each Get... routine.
 * It first checks if the file is already known, then checks if the file
 * is already open.
 * For each new file, an entry is made in the open-file-list.
 * Actually opening the file is done by a file type dependent low level
 * open routine.
 * Whether the file is actually reopened or is assumed to have been left
 * open depends on ODS_REOPEN macro.
 *
 *  TString fname  Input   filename as used in Get... routines
 *  TInt4 *ftype   Input   file type as used in Get... routines (see ods.h)
 *  FILE  **fp     Output  filepointer, to be used in read/write statements
 *  TInt4 *ierror  Output  errorcode as defined in ods.h . IEOK if all went well
 *----------------------------------------------------------------------
 */

OdsFilePtr OdsOpenFile(fname, ftype, fp, ierror)
TString fname;
TInt4   *ftype;
FILE    **fp;
TInt4   *ierror;
{
    OdsFilePtr  lijstptr = NULL;

   /* find file in list */
    if ((lijstptr = OdsSrchFileName(fname)) == NULL)
    {
        if (NrOpen >= OdsMaxFiles() )
        {
            *ierror = IENOHA;
            return NULL;
        }
        if ((lijstptr = OdsAddFile(fname)) == NULL)
        {
            *ierror = IEOTHR;
            return NULL;
        }
        *ierror = IEOK;
        lijstptr->id = OdsGetID();
        lijstptr->filepos = 0L;
        lijstptr->ftype = *ftype;
        lijstptr->filestatus = OC_OPENED;
        NrOpen++;
    }
    else
    {
#ifndef ODS_REOPEN
        if ( lijstptr->filestatus == OC_OPENED )
        {
           *ierror = IEOK     ;
        }
        else
        {
           *ierror = IENOFI   ;
        }
        *fp = lijstptr->fp ;
        return lijstptr;
#endif
    }

    /* call low level open routine */
    switch (lijstptr->ftype)
    {
    case ODS_DELWAQ_HIS_BIN :
    case ODS_DELWAQ_BAL_BIN :
    case ODS_DELWAQ_MAP_BIN :
    case ODS_DELPAR_PLO_BIN :
        DlwBinOpen(fname, fp, ierror);
        if (*ierror == IEOK)
        {
            lijstptr->fp = *fp;
            lijstptr->filestatus = OC_OPENED;
        }
        else
        {
            lijstptr->filestatus = OC_ERROR;
            return NULL;
        }
        break;

    case ODS_ANY_TEKAL_ASCII_1D   :
    case ODS_ANY_TEKAL_ASCII_1DE  :
    case ODS_ANY_TEKAL_ASCII_2D   :
    case ODS_ANY_TEKAL_ASCII_VEC  :
    case ODS_ANY_TEKAL_ASCII_ANNO :
        *fp = fopen( fname, "r" ) ; /* Was: "rb" */
        if ( (*fp) != NULL )
        {
            if ( ferror(*fp) != 0 )
            {
                *ierror = IEOTHR;
                lijstptr->filestatus = OC_ERROR;
                return NULL;
            }
            else
            {
               lijstptr->fp = *fp;
               lijstptr->filestatus = OC_OPENED;
            }
        }
        else
        {
            *ierror = IENOFI;
            lijstptr->filestatus = OC_ERROR;
            return NULL;
        }
        break;

    case ODS_DELWAQ_HIS_NEFIS :
    case ODS_DELWAQ_MAP_NEFIS :
    case ODS_DELPAR_HIS_NEFIS :
    case ODS_DELPAR_MAP_NEFIS :
    case ODS_DELPAR_PLO_NEFIS :
    case ODS_DELPAR_TRK_NEFIS :
    case ODS_TRISULA_HIS_NEFIS :
    case ODS_TRISULA_MAP_NEFIS :
    case ODS_TRISULA_DRO_NEFIS :
    case ODS_MORSYS_MAP_NEFIS :
    case ODS_MORSYS_HIS_NEFIS :
    case ODS_MORSYS_HWBOD:
    case ODS_MORSYS_HWGXY:
    case ODS_MORSYS_SWAN_NEFIS:
    case ODS_MORSYS_BAGR_NEFIS:
    case ODS_PHIDIAS_HISTORY:
    case ODS_PHIDIAS_MAP:
    case ODS_PHIDIAS_SPECTRAL:
    case ODS_PHAROS_MAP_NEFIS :
    case ODS_PHAROS_AMP_NEFIS :
        lijstptr->datfds = (BInt4 *)malloc (NEFIS_DATFDS_LEN * sizeof(BInt4));
        lijstptr->deffds = (BInt4 *)malloc (NEFIS_DEFFDS_LEN * sizeof(BInt4));
        OpenNefisDefDat (fname, lijstptr->datfds, lijstptr->deffds, ierror);
        if (*ierror == IEOK)
            lijstptr->filestatus = OC_OPENED;
        else
        {
            lijstptr->filestatus = OC_ERROR;
            return NULL;
        }
        lijstptr->fp = NULL; /* nefis file not directly accessible */
        break;

    default:
        *ierror = IEUNDE;
        return NULL;
    }

    /* set current position in file to saved position */
    if (lijstptr->filepos != 0L && lijstptr->fp != NULL)
        fseek(lijstptr->fp, lijstptr->filepos, SEEK_SET);


    *ierror = IEOK;
    return lijstptr;
}


/*----------------------------------------------------------------------
 * close file temporarily
 * This routine is called at the end of each Get... routine
 * Whether the file is actually closed or left open depends on ODS_REOPEN
 * constant.
 * void OdsTmpCloseFile(fname, ierror)
 *
 *  TString fname  Input   filename as used in Get... routines
 *  TInt4 *ierror  Output  errorcode as defined in ods.h . IEOK if all went well
 *----------------------------------------------------------------------
 */

void CLOSFL(fname, ierror)
TString fname;
TInt4   *ierror;
{
    OdsFilePtr  lijstptr = NULL;

#ifndef ODS_REOPEN
    /* reset filepointer to begin of file: for TEKAL ASCII files only */
    if ((lijstptr = OdsSrchFileName (fname)) == NULL)
    {
        *ierror = IENOFI;
        return;
    }
    if ( lijstptr->ftype == ODS_ANY_TEKAL_ASCII_1D   ||
         lijstptr->ftype == ODS_ANY_TEKAL_ASCII_1DE  ||
         lijstptr->ftype == ODS_ANY_TEKAL_ASCII_2D   ||
         lijstptr->ftype == ODS_ANY_TEKAL_ASCII_VEC  ||
         lijstptr->ftype == ODS_ANY_TEKAL_ASCII_ANNO    )
    {
        fseek ( lijstptr->fp , 0L, SEEK_SET ) ;
        lijstptr->filepos = 0L ;
    }
#endif

#ifdef ODS_REOPEN
    /* find file in list */
    if ((lijstptr = OdsSrchFileName (fname)) == NULL)
    {
        *ierror = IENOFI;
        return;
    }

    /* save current position if appropriate */
    if (lijstptr->fp != NULL)
        lijstptr->filepos = ftell(lijstptr->fp);

    /* call low level close routine */
    OdsDoClose(lijstptr,ierror);
    lijstptr->filestatus = OC_CLOSED;
#endif

    *ierror = IEOK;
    return;
}


/*----------------------------------------------------------------------
 * close file
 * This routine is called from the same level as the Get... routines
 * It removes the entry in the open-file-list and calls a low level
 * file type dependent close routine.
 * If the filename is "" or NULL, then all files of the calling process
 * are closed.
 *
 *  TString fname  Input   filename as used in Get... routines
 *  TInt4 *ierror  Output  errorcode as defined in ods.h . IEOK if all went well
 *----------------------------------------------------------------------
 */

void DllExport CLOSAL(fname, ierror)
TString fname;
TInt4   *ierror;
{
    OdsFilePtr  lijstptr ;
    OdsFilePtr  next_ptr ;
    TInt4       myproc;

    next_ptr = NULL ;
    if (fname == NULL || fname[0] == '\0')
    {   /* close all files with the ID of this process */
        myproc   = OdsGetID();
        lijstptr = filelijstkop ;
        while( lijstptr != NULL )
        {
            next_ptr = lijstptr->next ;
            if (lijstptr->id == myproc)
            {
                OdsDoClose(lijstptr,ierror);
                OdsDropFile(lijstptr);
            }
            lijstptr = next_ptr ;
        }
        /* Reset the private information in the generic NEFIS routines
        */
        GNF_ClearAllFiles() ;

        return;
    }

    /* find file in list */
    if ((lijstptr = OdsSrchFileName (fname)) == NULL)
    {
        *ierror = IENOFI;
        return;
    }
    /* call low level close routine */
    OdsDoClose(lijstptr,ierror);

    /* remove file from list */
    OdsDropFile(lijstptr);

    return;
}


/**7****************** LOCAL FUNCTION DEFINITIONS *********************/

/*----------------------------------------------------------------------
 * Get Process ID
 * This routine returns a unique ID for the calling process
 * For UNIX a getpid is performed, for MS-Windows (where things really
 * matter because of a single file list instead of one for each process!)
 * we need to get the window handle of the calling process.
 * For MS-DOS, just return a fixed number, since it's the only process
 * running anyway.
 *----------------------------------------------------------------------
 */
static TInt4 OdsGetID()
{
    TInt4 ProcID;

#ifdef UNIX
    ProcID = (TInt4) getpid();
#else
    ProcID = 1L ; /* Force every file to belong to the same process.
                     This may not be nice, but it does work! */
#endif

#ifdef DOS
    /* MS-DOS runs only one process at a time */
    ProcID = 1L;
#endif

/*
#ifdef MSWINDOWS
*/
    /* we need Window Handle (plus instance ?) from LiBInit here */
    /* complete this statement first !! */
/*
    ProcID = (TInt4)GetWnd();
#endif
*/

    return (ProcID);
}


/*----------------------------------------------------------------------
 * Get Maximum Number of Files
 * This routine returns the max allowed number of open files, minus a
 * safe margin
 *
 * Note (AM, dd 25 april 2000)
 * The operating system should decide this. Instead, return a large
 * number (larger than what will actually be the system's limit).
 *----------------------------------------------------------------------
 */
static TInt4 OdsMaxFiles()
{
    TInt4 maxfil;

    maxfil = 1024 ;
    return (maxfil);
}


/*----------------------------------------------------------------------
 * close file
 * This routine calls a low level file type dependent close routine.
 *----------------------------------------------------------------------
 */
static void OdsDoClose(elemnt, ierror)
OdsFilePtr  elemnt;
TInt4 *ierror;
{
/* Distinguish the two methods of opening files
*/
    if ( elemnt->fp != NULL )
    {
        DlwBinClose (elemnt->fp, ierror);
    }
    else
    {
        if ( elemnt->datfds != NULL && elemnt->filestatus != OC_ERROR )
        {
           CloseNefisDefDat (elemnt->filename, elemnt->datfds,
                             elemnt->deffds, ierror);
        }
        else
        {
           *ierror = IEOK ;
        }
    }

    if ( *ierror == IEOK )
    {
        NrOpen--;
    }
    return;
}


/*----------------------------------------------------------------------
 * Search file in list by filename
 * This routine searches the file list for a certain file and returns
 * a pointer to the list entry if filename matches, or NULL if no matching
 * file could be found.
 *----------------------------------------------------------------------
 */

static OdsFilePtr OdsSrchFileName(zoeknaam)
char *zoeknaam;
{
    OdsFilePtr  lijstptr;

    for (lijstptr = filelijstkop; lijstptr != NULL; lijstptr = lijstptr->next)
    {
        if (strcmp(zoeknaam, lijstptr->filename)==0)
            return (lijstptr);
    }
    return (NULL);
}


/*----------------------------------------------------------------------
 * Search file in list by process ID
 * This routine searches the file list for the first file with a matching
 * process ID and returns a pointer to the list entry if process ID matches,
 * or NULL if no matching file could be found.
 *----------------------------------------------------------------------
 */

static OdsFilePtr OdsSrchFileProc(id)
TInt4 id;
{
    OdsFilePtr  lijstptr;

    for (lijstptr = filelijstkop; lijstptr != NULL; lijstptr = lijstptr->next)
    {
        if (lijstptr->id == id)
            return (lijstptr);
    }
    return (NULL);
}


/*----------------------------------------------------------------------
 * Add file to list
 * This routine inserts a new element at the tail of the list and returns
 * a pointer to the new element.
 *----------------------------------------------------------------------
 */

static OdsFilePtr OdsAddFile(filenaam)
char *filenaam;
{
    OdsFilePtr  lijstptr;

    if (filelijstkop == NULL) /* list is still empty */
    {
        filelijstkop = (OdsFilePtr) malloc (sizeof(struct OdsFile));
        lijstptr = filelijstkop;
    }
    else
    {
    /* search for last element */
        for (lijstptr = filelijstkop;
             lijstptr != NULL && lijstptr->next != NULL;
             lijstptr = lijstptr->next);

        lijstptr->next = (OdsFilePtr) malloc (sizeof(struct OdsFile));
        if (lijstptr->next == NULL)
            return (NULL);

        lijstptr = lijstptr->next;
    }

    lijstptr->next     = NULL;
    lijstptr->fp       = NULL;
    lijstptr->deffds   = NULL;
    lijstptr->datfds   = NULL;
    lijstptr->id       = 0   ;
    lijstptr->filepos  = 0   ;
    lijstptr->filestatus = 0 ;
    lijstptr->ftype    = 0   ;
    lijstptr->unitnr   = 0   ;
    lijstptr->filename = malloc (ODS_FILNAMLEN * ODS_FNAME_DIM * sizeof(char));
    memcpy (lijstptr->filename, filenaam, ODS_FILNAMLEN * ODS_FNAME_DIM *
                                          sizeof( char ) ) ;

    return (lijstptr);
}


/*----------------------------------------------------------------------
 * Remove file from list
 * This routine removes an element from the list
 *----------------------------------------------------------------------
 */

static void OdsDropFile(elemnt)
OdsFilePtr  elemnt;
{
    OdsFilePtr  lijstptr;

    if (filelijstkop == NULL) /* nothing left to drop */
        return;

    if (filelijstkop == elemnt)
    {
       /* pop element from chain */
        filelijstkop = elemnt->next;
    }
    else
    {
    /* search for element in front of the doomed one */
        for (lijstptr = filelijstkop;
             lijstptr != NULL && lijstptr->next != elemnt;
             lijstptr = lijstptr->next);

        if (lijstptr->next != elemnt) /* not found, stay cool */
            return;

       /* pop element from chain */
        lijstptr->next = elemnt->next;
    }

   /* free additional memory */
    if (elemnt->deffds != NULL)
        free (elemnt->deffds);
    if (elemnt->datfds != NULL)
        free (elemnt->datfds);

   /* free memory of filename in struct and of struct itself */
    free (elemnt->filename);
    free (elemnt);

    return;
}

#if defined( USE_WINNT )
#undef OPENFL
#undef OPNNEF
#undef CLOSFL
#undef CLOSAL

#define F_OPENFL OPENFL
#define F_OPNNEF OPNNEF
#define F_CLOSFL CLOSFL
#define F_CLOSAL CLOSAL
#endif

void FOR_CALL F_OPENFL(
        TString fname,
#if defined( IN_BETWEEN )
        int     lfname,
#endif
        TInt4  *ftype,
        TInt4  *unitnr,
        TInt4  *ierror
#if ! defined( IN_BETWEEN )
       ,int     lfname
#endif
        )
{
    C_openfl(fname, ftype, unitnr, ierror) ;
    return;
}

void FOR_CALL F_OPNNEF (
        TString fname,
#if defined( IN_BETWEEN )
        int     lfname,
#endif
        TInt4  *ftype,
        BInt4  *datfds,
        BInt4  *deffds,
        TInt4  *ierror
#if ! defined( IN_BETWEEN )
       ,int     lfname
#endif
        )
{
    C_opnnef(fname, ftype, datfds, deffds, ierror) ;
    return;
}

void FOR_CALL F_CLOSAL(
        TString fname,
#if defined( IN_BETWEEN )
        int     lfname,
#endif
        TInt4  *ierror
#if ! defined( IN_BETWEEN )
       ,int     lfname
#endif
        )
{
   C_closal(fname, ierror) ;
   return ;
}

void FOR_CALL F_CLOSFL(
        TString fname,
#if defined( IN_BETWEEN )
        int     lfname,
#endif
        TInt4  *ierror
#if ! defined( IN_BETWEEN )
       ,int     lfname
#endif
        )
{
   C_closfl(fname, ierror) ;
   return ;
}

/**8************************** TEST DRIVER ****************************/

#undef TESTDRIVER

#ifdef  TESTDRIVER

main (argc, argv)
        int               argc;
        char    **argv;
{


} /* main */

#endif  /* TESTDRIVER */

/*------------------------- end of opnclose.c ---------------------------*/
