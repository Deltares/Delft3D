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
 *  ods.h  -  ODS interface definition
 *
 *  Note: extra macro, USE_STDCALL, to control use of __stdcall
 *
 *  Marc Kool
 */

/*  Version 1.00   931206   by Andre Hendriks            */
/*                                                       */
/*  Supports platforms:  MSDOS      : Microsoft C 6      */
/*                       MSWINDOWS  : Microsoft C 6      */
/*                       HPOLD      : HP-UX C prior to 9 */
/*                       HP         : HP-UX C from 9 on  */
/*                       SUN        : SUN                */
/*                       IBM        : IBM AIX            */

#ifndef ODS_H_INCLUDED
#define ODS_H_INCLUDED

#include "portable.h"

#undef UNIX
#undef PC


#ifdef USE_HPUX
#define HP
#define UNIX
#endif

#ifdef USE_SUNOS
#define SUN
#define UNIX
#endif

#ifdef USE_AIX
#define IBM
#define UNIX
#endif

#ifdef USE_IRIX
#define SGI
#define UNIX
#endif

#ifdef USE_LINUX
#define LINUX
#define UNIX
#endif

#ifdef USE_MSDOS
#define MSDOS
#endif

#ifdef USE_MSWINDOWS
#define MSWINDOWS
#endif

#ifdef USE_WINNT
#define WINNT
#endif

#ifdef WINNT
#   define PC
#endif

#ifdef USE_ALPH
#define DEC
#define UNIX
#endif

#ifdef MSWINDOWS
#   define FUNTYPE _pascal
#   define PC
#   undef MSDOS
#else
#   ifdef MSDOS
#      define FUNTYPE _fortran
#      define PC
#   else
#      ifdef WINNT
#         ifdef USE_STDCALL
#            define STDCALL __stdcall
#         else
#            define STDCALL
#         endif
#         ifdef USE_DLL
#            define FUNTYPE __declspec(dllexport) STDCALL
#         else
#            ifdef IMPORT_DLL
#                define FUNTYPE __declspec(dllimport) STDCALL
#            else
#                define FUNTYPE
#            endif
#         endif
#         define PC
#      else
#         define FUNTYPE
#         ifdef HP
#            define UNIX
#         endif
#         ifdef HPOLD
#            define UNIX
#         endif
#         ifdef SUN
#            define UNIX
#         endif
#         ifdef IBM
#            define UNIX
#         endif
#      endif
#   endif
#endif

#ifndef UNIX
#   ifndef PC
       ODS_H_ERROR
       /* platform define not specified (use MSDOS, MSWINDOWS, HP, etc) */
#   endif
#endif

/*   Date:       28 Feb 1994                                           */
/*   Time:       11:50                                                */
/*   Program:    odstypes.h                                           */
/*   Version:    1.01                                                 */
/*   Programmer: Andre Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   1.00 -- 4 Aug 1993 -- 07:44 -- Operating System: DOS              */
/*   Project:    Open Data Structuur                                  */
/*   Module:     ODSTypes.h                                           */
/*   Function:   Include file with defined types                      */
/*   Comment:  1.01: support for WASPRO files (*.WS1 and *.WAS)       */
/*   Reference:                                                       */
/*   Review:                                                          */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/*    Defined types:                                                  */
/*                                                                    */
/*    Name    Filetype                                                */
/*    ------  ----------------------------------------                */
/*    ITDBF3  DBASE 3 dbf file                                        */
/*    ITDBF4  DBASE 4 dbf file                                        */
/*    ITDLWH  Delwaq 3.xx his file                                    */
/*    ITDLWM  Delwaq 3.xx map file                                    */
/*    ITDLWG  Delwaq 3.xx grid file                                   */
/*    ITDLPP  Delpar 2.xx PLO file                                    */
/*    ITJSP   JsPost file                                             */
/*    ITMPX   Mappix file                                             */
/*    ITNFS   Nefis file                                              */
/*    ITNFTH  Trisula Nefis his file                                  */
/*    ITNFTM  Trisula Nefis map file                                  */
/*    ITNFCM  Morsys Nefis map file                                   */
/*    ITNFCH  Morsys Nefis history file                               */
/*    ITDATH  Any Tekal ASCII 1d file                                 */
/*    ITDATHE Any Tekal ASCII 1d file equidistant                     */
/*    ITDATM  Any Tekal ASCII 2d file                                 */
/*    ITDATV  Any Tekal ASCII 2d file (vectors)                       */
/*    ITDATA  Any Tekal ASCII Annotations (text at specified location)*/
/*    ITUNDE  Not defined, uses extension to get type                 */
/*    ITWAS   Waspro WAS file                                         */
/*    ITWS1   Waspro WS1 file                                         */
/*    ITSHYG  SHYFEM grid file                                        */
/*                                                                    */
/**********************************************************************/

/* Undefined */
#define ITUNDE   0

/* Delwaq/Delpar */
#define ITDBF3   1
#define ITDBF4   2
#define ITDLWM   3
#define ITDLWH   4
#define ITDLWG  14
#define ITDLPP  15
#define ITDLWB  16
#define ITMPX    5
#define ITNFS    7
#define ITWS1   70
#define ITWAS   71
#define ITDWNM 103
#define ITDWNH 104
#define ITDPNM 203
#define ITDPNH 204
#define ITDPNP 205
#define ITDPNT 206
#define ITDTLM 207

/* Trisula */
#define ITNFTH   1001
#define ITNFTM   1003
#define ITNFTD   1004

/* Samples */
#define ISMP2D   3001
#define ISMPTB   3002
#define ISMPTM   3003

/* Morsys  */
#define ITNFCM   5001
#define ITNFCH   5002
#define ITNHWB   5003
#define ITNHWG   5004
#define ITNFMB   5005

/* Phidias */
#define ITNPHH   5050
#define ITNPHM   5051
#define ITNPHS   5052

#define ITDATH   3101
#define ITDATHE  3111
#define ITDBTH   3102
#define ITDATM   3103
#define ITDBTM   3104
#define ITDATV   3105
#define ITDATA   3106
#define ITJSP   3120


/* Shyfem */
#define ITSHYG  4001

/* Pharos */
#define ITPHARM 6001
#define ITPHARA 6002

/* Geographical types */
#define IGCURV  10001
#define IGFEM3  10002
#define IGLAND  10003
#define IGFEM3P 10004
#define IGVERT  10005
#define IGX1D   10006

/* Files with general geographical information */
#define ITBNA   7701
#define ITDXF   7702
#define ITTLND  7703

/* Better file type macros */
#define ODS_TRISULA_HIS_BIN   1000
#define ODS_TRISULA_HIS_NEFIS ITNFTH
#define ODS_TRISULA_MAP_BIN   1002
#define ODS_TRISULA_MAP_NEFIS ITNFTM
#define ODS_TRISULA_DRO_NEFIS ITNFTD

#define ODS_DELWAQ_HIS_BIN    ITDLWH
#define ODS_DELWAQ_BAL_BIN    ITDLWB
#define ODS_DELWAQ_HIS_NEFIS  ITDWNH
#define ODS_DELWAQ_MAP_BIN    ITDLWM
#define ODS_DELWAQ_MAP_NEFIS  ITDWNM
#define ODS_DELWAQ_GRID_UNF   ITDLWG
#define ODS_DELWAQ_TELEMAC    ITDTLM

#define ODS_DELPAR_HIS_NEFIS  ITDPNH
#define ODS_DELPAR_MAP_NEFIS  ITDPNM
#define ODS_DELPAR_PLO_NEFIS  ITDPNP
#define ODS_DELPAR_PLO_BIN    ITDLPP
#define ODS_DELPAR_TRK_NEFIS  ITDPNT

#define ODS_ANY_TEKAL_ASCII_1D   ITDATH
#define ODS_ANY_TEKAL_ASCII_1DE  ITDATHE
#define ODS_ANY_TEKAL_ASCII_2D   ITDATM
#define ODS_ANY_TEKAL_ASCII_VEC  ITDATV
#define ODS_ANY_TEKAL_ASCII_ANNO ITDATA

#define ODS_ANY_JSPOST          ITJSP
#define ODS_SAMPLES_2D          ISMP2D
#define ODS_SAMPLES_TABLE       ISMPTB
#define ODS_SAMPLES_TIME        ISMPTM

#define ODS_GEOGR_BNA           ITBNA
#define ODS_GEOGR_DXF           ITDXF
#define ODS_GEOGR_TEKAL         ITTLND

#define ODS_SHYFEM_GRID         ITSHYG

#define ODS_MORSYS_MAP_NEFIS    ITNFCM
#define ODS_MORSYS_HIS_NEFIS    ITNFCH
#define ODS_MORSYS_HWBOD        ITNHWB
#define ODS_MORSYS_HWGXY        ITNHWG
#define ODS_MORSYS_BAGR_NEFIS   ITNFMB

#define ODS_MORSYS_HWGXY_NEFIS  5101
#define ODS_MORSYS_BOTM_NEFIS   5102
#define ODS_MORSYS_BOTH_NEFIS   5103
#define ODS_MORSYS_TRAH_NEFIS   5104
#define ODS_MORSYS_TRAM_NEFIS   5105
#define ODS_MORSYS_SWAN_NEFIS   5106

#define ODS_PHIDIAS_HISTORY     ITNPHH
#define ODS_PHIDIAS_MAP         ITNPHM
#define ODS_PHIDIAS_SPECTRAL    ITNPHS

#define ODS_PHAROS_MAP_NEFIS    ITPHARM
#define ODS_PHAROS_AMP_NEFIS    ITPHARA

/*  Codes for partyp parameter (partyp may be a sum of the following codes)
 */
#define IPHIER  1
#define IPLDEP  2
#define IPLLST  4
#define IPLMNK  8
#define IPLMET  16
#define IPLFEM  32
#define IPTDEP  512
#define ODS_PT_HIERACHY                  IPHIER         /* parameter has a hierarchy */
#define ODS_PT_LOC_DEP                  IPLDEP         /* parameter depends on "a" location */
#define ODS_PT_LOC_LIST                  IPLLST         /* depends on a list of symbolic locs */
#define ODS_PT_LOC_MNK                  IPLMNK         /* depends on a grid location */
#define ODS_PT_LOC_METRIC         IPLMET         /* depends on a metric location */
#define ODS_PT_LOC_FEM                  IPLFEM         /* depends on finite element locs */
/*      ODS_PT_LOC_*            */
#define ODS_PT_TIME_DEP                  IPTDEP         /* depends on "a" time */
/*      ODS_PT_*                */

/* String lengths etc. */
#define ODS_FILNAMLEN                  256
#define ODS_FNAME_DIM                  3
#define PARLEN              20
#define ALLSTEPS            999999999.0

/*   Date:       4 Aug 1993                                           */
/*   Time:       07:44                                                */
/*   Program:    odserr.h                                             */
/*   Version:    1.00                                                 */
/*   Programmer: Andre Hendriks                                       */
/*   Copyright (C)  Stichting Deltares, 2011-2024.                    */
/*   Previous version(s):                                             */
/*   0.0 -- 4 Aug 1993 -- 07:44 -- Operating System: DOS              */
/*   Project:    Open Data Structuur                                  */
/*   Module:     ODSErr.h                                             */
/*   Function:   Include file with defined errors                     */
/*   Comment:                                                         */
/*   Reference:                                                       */
/*   Review:                                                          */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/*    Defined types:                                                  */
/*                                                                    */
/*    Name    Filetype                                                */
/*    ------  ----------------------------------------                */
/*    IEOK    Okay                                                    */
/*    IEUNDE  Unable to determine filetype                            */
/*    IEUNKN  File type not implemented in this version               */
/*    IETYPE  File is not of indicated type                           */
/*    IENOFI  File does not exist                                     */
/*    IEFIAO  File already open                                       */
/*    IEFLCK  File locked by other program                            */
/*    IEFIRO  Access denied/file is read-only                         */
/*    IENOHA  No free handle (unit)                                   */
/*    IERLCK  Record locked by other program                          */
/*    IENLCK  Cannot lock file/record/share not installed             */
/*    IEINFO  File does not contain wanted information                */
/*    IEUEOF  Unexpected end of file                                  */
/*    IEPMNY  Too many parameters found for array space               */
/*    IELMNY  Too many locations found for array space                */
/*    IETMNY  Too many times found for array space                    */
/*    IETIME  Bad date/time format                                    */
/*    IEBUFF  Buffer space too small (warn WL technical support)      */
/*    IEDISK  No space left on device                                 */
/*    IEOTHR  Other error                                             */
/*    IEPLOW  Nr. of parameters < 1                                   */
/*    IELLOW  Nr. of locations < 1                                    */
/*    IETLOW  Nr. of times < 1                                        */
/*    IEFNNW  File not new                                            */
/*    IERUNI  RunId not equal                                         */
/*    IEPARI  Par.  not equal                                         */
/*    IELOCI  Loc.  not equal                                         */
/*    IETIMI  Time  not equal                                         */
/*    IESTEP  Time step too small                                     */
/*                                                                    */
/**********************************************************************/

#define IEOK      0
#define IEUNDE    1
#define IEUNKN    2
#define IETYPE    3
#define IENOFI   10
#define IENOHA   11
#define IEFIAO   12
#define IEFLCK   13
#define IEFIRO   14
#define IERLCK   15
#define IENLCK   16
#define IEFNNW   17
#define IEINFO   20
#define IEUEOF   21
#define IEPMNY   30
#define IELMNY   31
#define IETMNY   32
#define IEPLOW   35
#define IELLOW   36
#define IETLOW   37
#define IETIME   40
#define IERUNI   41
#define IEPARI   42
#define IELOCI   43
#define IETIMI   44
#define IESTEP   45
#define IEBUFF   91
#define IEDISK   92
#define IEOTHR   99

/*---------------------------------------------------------------------
 *
 * Company:        Deltares
 *
 * System:         ODS (Open Data Structure)
 *
 * Project:        --
 *
 * Programmer:     Jules Overmars
 *
 * Date:           August 1998
 *
 * Summary:        This file contains the prototypes of C functions
 *                 getdim, getgrd, getloc, getmat, getpar, gettme, getval
 *                 It is taken into account Unix and PC library or
 *                 PC and DLL.
 *
 *--------------------------------------------------------------------*/

/* Define a macro and this macro is used within all the get... files */

#ifdef USE_DLL
    /* == For DLL on PC == */
#   define  DllExport  __declspec ( dllexport ) STDCALL
#else
#   ifdef IMPORT_DLL
#       define  DllExport  __declspec ( dllimport ) STDCALL
#   else
    /* == For library on PC and on Unix == */
#       define  DllExport
#   endif
#endif

void DllExport getdim (
                        char  *fname,
                        TInt4 *itype,
                        char  *dim,
                        TInt4 *pardep,
                        TInt4 *timdep,
                        TInt4 *locdep,
                        TInt4 *ndim,
                        TInt4 *ierror,
                        char  *option
                       ) ;


void DllExport getgrd (
                        char  *fname,
                        TInt4 *itype,
                        TInt4 *indloc,
                        TInt4 *indx,
                        TInt4 *nocell,
                        TInt4 *igisty,
                        TInt4 *ierror
                      ) ;


void DllExport getloc (
                        char   *fname,
                        TInt4  *itype,
                        char   *locdef,
                        TInt4  *maxdef,
                        TInt4  *pardep,
                        TInt4  *timdep,
                        TInt4  *maxlst,
                        char   *loclst,
                        TInt4  *loctyp,
                        TInt4  *locnr ,
                        TInt4  *nrlst,
                        TInt4  *ierror,
                        char   *option
                      ) ;


void DllExport getmat (
                        char   *fname,
                        TInt4  *itype,
                        TInt4  *parcod,
                        TInt4  *loc,
                        double *tim,
                        float  *misval,
                        TInt4  *i3gl,
                        TInt4  *maxdim,
                        float  *data,
                        TInt4  *ierror,
                        char   *option
                      ) ;


void DllExport getpar (
                        char   *fname,
                        TInt4  *itype,
                        char   *pardef,
                        TInt4  *maxdef,
                        TInt4  *timdep,
                        TInt4  *locdep,
                        TInt4  *maxlst,
                        TInt4  *lang,
                        char   *parlst,
                        char   *paruni,
                        TInt4  *partyp,
                        TInt4  *parcod,
                        TInt4  *nrlst,
                        TInt4  *ierror,
                        char   *option
                      ) ;


void DllExport gettme (
                        char   *fname,
                        TInt4  *itype,
                        double *timdef,
                        TInt4  *maxdef,
                        TInt4  *pardep,
                        TInt4  *locdep,
                        TInt4  *maxlst,
                        double *timlst,
                        TInt4  *timtyp,
                        TInt4  *nrlst,
                        TInt4  *ierror,
                        char   *option
                      )  ;


void DllExport getval (
                        char   *fname,
                        TInt4  *itype,
                        char   *locin,
                        char   *parin,
                        double  *timin,
                        TInt4  *maxilo,
                        TInt4  *maxipa,
                        TInt4  *maxiti,
                        float  *misval,
                        char   *loc,
                        char   *par,
                        double *tim,
                        float  *values,
                        TInt4  *maxolo,
                        TInt4  *maxopa,
                        TInt4  *maxoti,
                        TInt4  *nrloc ,
                        TInt4  *nrpar,
                        TInt4  *nrtim ,
                        TInt4  *ierror
                      ) ;

void DllExport odserr( long ierror, char **text ) ;

void DllExport gregor( double *julian,
                       TInt4  *iyear  ,
                       TInt4  *imonth ,
                       TInt4  *iday   ,
                       TInt4  *ihour  ,
                       TInt4  *imin   ,
                       TInt4  *isec   ) ;

void DllExport julian( TInt4  *iyear  ,
                       TInt4  *imonth ,
                       TInt4  *iday   ,
                       TInt4  *ihour  ,
                       TInt4  *imin   ,
                       TInt4  *isec   ,
                       double *julian ) ;

#endif  /* ODS_H_INCLUDED */

