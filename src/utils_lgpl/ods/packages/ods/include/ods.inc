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
c
c   ods.inc  -  ODS Fortran interface
c
c   Marc Kool
c
c
c   $Author: Markus $
c   $Date: 19-11-03 11:33 $
c   $Source: /u/cvsroot/gpp/include/ods.inc,v $
C
C     Flags for PUTxxx routines Open Data Structure
C
C     version 1.00  1993.08.31  Andre Hendriks */
C
C     IFADDL  keep item in new file, if not in argument list
C     IFADDP  (items are Location, Parameter, Time and Values)
C     IFADDT
C     IFADDV
C     IFSAML  generate error if item is in file but not in
C     IFSAMP  argument list or vice versa (and IFADDx flag
C     IFSAMT  not set)
C     IFSAMV
C     IFFNEW  return with error if not new file
C     IFSAMR  force that runid must be equal
C
      INTEGER  IFADDL, IFADDP, IFADDT, IFADDV,
     1         IFSAML, IFSAMP, IFSAMT, IFSAMV, IFSAMR,
     2         IFFNEW

      PARAMETER ( IFADDL =    1,
     1            IFADDP =    2,
     2            IFADDT =    4,
     3            IFADDV =    8,
     4            IFSAML =   16,
     5            IFSAMP =   32,
     6            IFSAMT =   64,
     7            IFSAMV =  128,
     8            IFFNEW =  256,
     9            IFSAMR =  512)

C     Date:       3 Mrt 1994
C     Time:       10:55
C     Program:    TYPES.INC
C     Version:    1.00
C     Programmer: Andre Hendriks
C     Copyright (C)  Stichting Deltares, 2011-2024.
C     Previous version(s):
C     0.01 -- 10 Jun 1993 -- 15:11 -- Operating System: DOS
C     0.00 -- 10 Jun 1993 -- 15:05 -- Operating System: DOS
C     Project:    Open data-structuur
C     Module:     Types.inc
C     Function:   Include file with defined types
C     Comment:    1.00:added WAS and WS1 files
C     Reference:
C     Review:
C
C***********************************************************************
C
C     Defined types:
C
C     Name    Filetype
C     ------  ----------------------------------------------------------
C     ITDBF3  DBASE 3 dbf file
C     ITDBF4  DBASE 4 dbf file
C     ITDLWH  Delwaq 3.xx his file
C     ITDLWM  Delwaq 3.xx map file
C     ITDLWG  Delwaq 3.xx grid file
C     ITJSP   JsPost file
C     ITMPX   Mappix file
C     ITNFS   Nefis file
C     ITNFTH  Nefis Trisula his file
C     ITNFTM  Nefis Trisula map file
C     ITUNDE  Not defined, uses extension to get type
C     ITWAS   WASPRO .WAS file
C     ITWS1   WASPRO .WS1 file
C     ITDATH  default ASCII Tekal his file
C     ITDATHE default ASCII Tekal his file equidistant
C     ITDBTH  default binary Tekal his file
C     ITDATM  default ASCII Tekal map file
C     ITDBTM  default binary Tekal map file
C
      INTEGER  ITDBF3, ITDBF4, ITDLWH, ITDLWM, ITJSP ,
     1         ITMPX , ITNFS , ITUNDE, ITWAS , ITWS1 ,
     2         ITNFTH, ITNFTM, ITDATH, ITDBTH, ITDATM,
     3         ITDBTM, ITDLWG, IGCURV, IGFEM3, IGLAND, ITDATHE

      PARAMETER ( ITUNDE =  0,
     1            ITDBF3 =  1,
     2            ITDBF4 =  2,
     3            ITDLWM =  3,
     4            ITDLWH =  4,
     4            ITDLWG = 14,
     5            ITMPX  =  5,
     6            ITJSP  =  6,
     7            ITNFS  =  7,
     8            ITNFTH =  1001,
     9            ITNFTM =  1003,
     A            ITDATH =  3101,
     A            ITDATHE=  3111,
     B            ITDBTH =  3102,
     C            ITDATM =  3103,
     D            ITDBTM =  3104,
     E            ITWAS  = 71,
     F            ITWS1  = 70)
      PARAMETER ( IGCURV = 10001,
     1            IGFEM3 = 10002,
     2            IGLAND = 10003)

C     Date:       10 Jun 1993
C     Time:       15:11
C     Program:    ERRORS.INC
C     Version:    0.00
C     Programmer: Andre Hendriks
C     Copyright (C)  Stichting Deltares, 2011-2024.
C     Previous version(s):
C     0.0 -- 10 Jun 1993 -- 15:35 -- Operating System: DOS
C     Project:    Open data-structuur
C     Module:     Errors.inc
C     Function:   Include file with defined error codes
C     Comment:
C     Reference:
C     Review:
C
C***********************************************************************
C
C     Defined types:
C
C     Name    Error code
C     ------  ----------------------------------------------------------
C     IEOK    Okay
C     IEUNDE  Unable to determine filetype
C     IEUNKN  File type not implemented in this version
C     IETYPE  File is not of indicated type
C     IENOFI  File does not exist
C     IEFIAO  File already open
C     IEFLCK  File locked by other program
C     IEFIRO  Access denied/file is read-only
C     IENOHA  No free handle (unit)
C     IERLCK  Record locked by other program
C     IENLCK  Cannot lock file/record/share not installed
C     IEINFO  File does not contain wanted information
C     IEUEOF  Unexpected end of file
C     IEPLOW  Nr. of parameters < 1
C     IELLOW  Nr. of locations < 1
C     IETLOW  Nr. of times < 1
C     IEPMNY  Too many parameters found for array space
C     IELMNY  Too many locations found for array space
C     IETMNY  Too many times found for array space
C     IETIME  Bad date/time format
C     IEBUFF  Buffer space too small (warn WL technical support)
C     IEDISK  No space left on device
C     IEFNNW  File exists already
C     IERUNI  RunId not equal
C     IEPARI  Par.  not equal
C     IELOCI  Loc.  not equal
C     IETIMI  Time  not equal
C     IESTEP  Timestep too small (time can not be represented accurately)
C     IEOTHR  Other error
C
      INTEGER     IEBUFF, IEDISK, IEFIAO, IEFIRO, IEFLCK,
     1            IEINFO, IELMNY, IENLCK, IENOFI, IENOHA,
     2            IEOK  , IEOTHR, IEPMNY, IERLCK, IETIME,
     3            IETMNY, IETYPE, IEUEOF, IEUNDE, IEUNKN,
     4            IEPLOW, IELLOW, IETLOW, IEFNNW, IERUNI,
     5            IEPARI, IELOCI, IETIMI, IESTEP

      PARAMETER ( IEOK   =  0,
     1            IEUNDE =  1,
     2            IEUNKN =  2,
     3            IETYPE =  3,
     4            IENOFI = 10,
     5            IENOHA = 11,
     6            IEFIAO = 12,
     7            IEFLCK = 13,
     8            IEFIRO = 14,
     9            IERLCK = 15)

      PARAMETER ( IENLCK = 16,
     1            IEFNNW = 17,
     2            IEINFO = 20,
     3            IEUEOF = 21,
     4            IEPMNY = 30,
     5            IELMNY = 31,
     6            IETMNY = 32,
     7            IEPLOW = 35,
     8            IELLOW = 36,
     9            IETLOW = 37,
     A            IETIME = 40,
     B            IERUNI = 41,
     C            IEPARI = 42,
     D            IELOCI = 43,
     E            IETIMI = 44,
     F            IESTEP = 45,
     G            IEBUFF = 91,
     H            IEDISK = 92,
     I            IEOTHR = 99)


      INTEGER     IPHIER, IPLDEP, IPLLST, IPLMNK, IPLMET, IPLFEM,
     1            IPTDEP

      PARAMETER ( IPHIER = 1,
     1            IPLDEP = 2,
     2            IPLLST = 4,
     3            IPLMNK = 8,
     4            IPLMET = 16,
     5            IPLFEM = 32,
     6            IPTDEP = 512 )

