! ---- LGPL --------------------------------------------------------------------
!
! Copyright (C)  Stichting Deltares, 2011-2024.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation version 2.1.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
! contact: delft3d.support@deltares.nl
! Stichting Deltares
! P.O. Box 177
! 2600 MH Delft, The Netherlands
!
! All indications and logos of, and references to, "Delft3D" and "Deltares"
! are registered trademarks of Stichting Deltares, and remain the property of
! Stichting Deltares. All rights reserved.
!
!------------------------------------------------------------------------------
!
!   ods.inc  -  ODS Fortran interface
!
!   Marc Kool
!
!
!   $Author: Markus $
!   $Date: 19-11-03 11:33 $
!   $Source: /u/cvsroot/gpp/include/ods.inc,v $
!
!     Flags for PUTxxx routines Open Data Structure
!
!     version 1.00  1993.08.31  Andre Hendriks */
!
!     IFADDL  keep item in new file, if not in argument list
!     IFADDP  (items are Location, Parameter, Time and Values)
!     IFADDT
!     IFADDV
!     IFSAML  generate error if item is in file but not in
!     IFSAMP  argument list or vice versa (and IFADDx flag
!     IFSAMT  not set)
!     IFSAMV
!     IFFNEW  return with error if not new file
!     IFSAMR  force that runid must be equal
!
      INTEGER  IFADDL, IFADDP, IFADDT, IFADDV, &
               IFSAML, IFSAMP, IFSAMT, IFSAMV, IFSAMR, &
               IFFNEW

      PARAMETER ( IFADDL =    1, &
                  IFADDP =    2, &
                  IFADDT =    4, &
                  IFADDV =    8, &
                  IFSAML =   16, &
                  IFSAMP =   32, &
                  IFSAMT =   64, &
                  IFSAMV =  128, &
                  IFFNEW =  256, &
                  IFSAMR =  512)

!     Date:       3 Mrt 1994
!     Time:       10:55
!     Program:    TYPES.INC
!     Version:    1.00
!     Programmer: Andre Hendriks
!     Copyright (C)  Stichting Deltares, 2011-2024.
!     Previous version(s):
!     0.01 -- 10 Jun 1993 -- 15:11 -- Operating System: DOS
!     0.00 -- 10 Jun 1993 -- 15:05 -- Operating System: DOS
!     Project:    Open data-structuur
!     Module:     Types.inc
!     Function:   Include file with defined types
!     Comment:    1.00:added WAS and WS1 files
!     Reference:
!     Review:
!
!***********************************************************************
!
!     Defined types:
!
!     Name    Filetype
!     ------  ----------------------------------------------------------
!     ITDBF3  DBASE 3 dbf file
!     ITDBF4  DBASE 4 dbf file
!     ITDLWH  Delwaq 3.xx his file
!     ITDLWM  Delwaq 3.xx map file
!     ITDLWG  Delwaq 3.xx grid file
!     ITJSP   JsPost file
!     ITMPX   Mappix file
!     ITNFS   Nefis file
!     ITNFTH  Nefis Trisula his file
!     ITNFTM  Nefis Trisula map file
!     ITUNDE  Not defined, uses extension to get type
!     ITWAS   WASPRO .WAS file
!     ITWS1   WASPRO .WS1 file
!     ITDATH  default ASCII Tekal his file
!     ITDATHE default ASCII Tekal his file equidistant
!     ITDBTH  default binary Tekal his file
!     ITDATM  default ASCII Tekal map file
!     ITDBTM  default binary Tekal map file
!
      INTEGER  ITDBF3, ITDBF4, ITDLWH, ITDLWM, ITJSP , &
               ITMPX , ITNFS , ITUNDE, ITWAS , ITWS1 , &
               ITNFTH, ITNFTM, ITDATH, ITDBTH, ITDATM, &
               ITDBTM, ITDLWG, IGCURV, IGFEM3, IGLAND, ITDATHE

      PARAMETER ( ITUNDE =  0, &
                  ITDBF3 =  1, &
                  ITDBF4 =  2, &
                  ITDLWM =  3, &
                  ITDLWH =  4, &
                  ITDLWG = 14, &
                  ITMPX  =  5, &
                  ITJSP  =  6, &
                  ITNFS  =  7, &
                  ITNFTH =  1001, &
                  ITNFTM =  1003, &
                  ITDATH =  3101, &
                  ITDATHE=  3111, &
                  ITDBTH =  3102, &
                  ITDATM =  3103, &
                  ITDBTM =  3104, &
                  ITWAS  = 71, &
                  ITWS1  = 70)
      PARAMETER ( IGCURV = 10001, &
                  IGFEM3 = 10002, &
                  IGLAND = 10003)

!     Date:       10 Jun 1993
!     Time:       15:11
!     Program:    ERRORS.INC
!     Version:    0.00
!     Programmer: Andre Hendriks
!     Copyright (C)  Stichting Deltares, 2011-2024.
!     Previous version(s):
!     0.0 -- 10 Jun 1993 -- 15:35 -- Operating System: DOS
!     Project:    Open data-structuur
!     Module:     Errors.inc
!     Function:   Include file with defined error codes
!     Comment:
!     Reference:
!     Review:
!
!***********************************************************************
!
!     Defined types:
!
!     Name    Error code
!     ------  ----------------------------------------------------------
!     IEOK    Okay
!     IEUNDE  Unable to determine filetype
!     IEUNKN  File type not implemented in this version
!     IETYPE  File is not of indicated type
!     IENOFI  File does not exist
!     IEFIAO  File already open
!     IEFLCK  File locked by other program
!     IEFIRO  Access denied/file is read-only
!     IENOHA  No free handle (unit)
!     IERLCK  Record locked by other program
!     IENLCK  Cannot lock file/record/share not installed
!     IEINFO  File does not contain wanted information
!     IEUEOF  Unexpected end of file
!     IEPLOW  Nr. of parameters < 1
!     IELLOW  Nr. of locations < 1
!     IETLOW  Nr. of times < 1
!     IEPMNY  Too many parameters found for array space
!     IELMNY  Too many locations found for array space
!     IETMNY  Too many times found for array space
!     IETIME  Bad date/time format
!     IEBUFF  Buffer space too small (warn WL technical support)
!     IEDISK  No space left on device
!     IEFNNW  File exists already
!     IERUNI  RunId not equal
!     IEPARI  Par.  not equal
!     IELOCI  Loc.  not equal
!     IETIMI  Time  not equal
!     IESTEP  Timestep too small (time can not be represented accurately)
!     IEOTHR  Other error
!
      INTEGER     IEBUFF, IEDISK, IEFIAO, IEFIRO, IEFLCK, &
                  IEINFO, IELMNY, IENLCK, IENOFI, IENOHA, &
                  IEOK  , IEOTHR, IEPMNY, IERLCK, IETIME, &
                  IETMNY, IETYPE, IEUEOF, IEUNDE, IEUNKN, &
                  IEPLOW, IELLOW, IETLOW, IEFNNW, IERUNI, &
                  IEPARI, IELOCI, IETIMI, IESTEP

      PARAMETER ( IEOK   =  0, &
                  IEUNDE =  1, &
                  IEUNKN =  2, &
                  IETYPE =  3, &
                  IENOFI = 10, &
                  IENOHA = 11, &
                  IEFIAO = 12, &
                  IEFLCK = 13, &
                  IEFIRO = 14, &
                  IERLCK = 15)

      PARAMETER ( IENLCK = 16, &
                  IEFNNW = 17, &
                  IEINFO = 20, &
                  IEUEOF = 21, &
                  IEPMNY = 30, &
                  IELMNY = 31, &
                  IETMNY = 32, &
                  IEPLOW = 35, &
                  IELLOW = 36, &
                  IETLOW = 37, &
                  IETIME = 40, &
                  IERUNI = 41, &
                  IEPARI = 42, &
                  IELOCI = 43, &
                  IETIMI = 44, &
                  IESTEP = 45, &
                  IEBUFF = 91, &
                  IEDISK = 92, &
                  IEOTHR = 99)


      INTEGER     IPHIER, IPLDEP, IPLLST, IPLMNK, IPLMET, IPLFEM, &
                  IPTDEP

      PARAMETER ( IPHIER = 1, &
                  IPLDEP = 2, &
                  IPLLST = 4, &
                  IPLMNK = 8, &
                  IPLMET = 16, &
                  IPLFEM = 32, &
                  IPTDEP = 512 )
