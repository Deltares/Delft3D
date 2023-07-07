!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine secchi ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Calculation secchi depth for visible-light (370-680nm)

!
!     Description of the module :
!
! Name    T   L I/O   Description                              Units
! ----    --- -  -    -------------------                      ----
! EXT     R*4 1 I total extinction coefficient                   [1/m]
! AIM1    R*4 1 I inorganic suspended matter 1                  [g/m3]
! AIM2    R*4 1 I inorganic suspended matter 2                  [g/m3]
! AIM3    R*4 1 I inorganic suspended matter 3                  [g/m3]
! POC1    R*4 1 I fast decomposing detritus                    [gC/m3]
! POC2    R*4 1 I medium decomposing detritus                  [gC/m3]
! POC3    R*4 1 I slow decomposing detritus                    [gC/m3]
! POC4    R*4 1 I refractory detritus                          [gC/m3]
! AH_380  R*4 1 I extinction of dissolved organic matter         [1/m]
! CHLORP  R*4 1 I chlorophyll-a concentration                  [mg/m3]
! SW_UIT  R*4 1 I extinction by UITZUCHT on (1) or Off (0)         [-]
! DIEP1   R*4 1 I argument UITZICHT
! DIEP2   R*4 1 I argument UITZICHT
! CORCHL  R*4 1 I argument UITZICHT
! C_DET   R*4 1 I argument UITZICHT
! C_GL1   R*4 1 I argument UITZICHT
! C_GL2   R*4 1 I argument UITZICHT
! HELHUM  R*4 1 I argument UITZICHT
! TAU     R*4 1 I argument UITZICHT
! ANGLE   R*4 1 I argument UITZICHT
! DETCDM  R*4 1 I dry matter carbon ratio detritus               [g/g]
! PAC     R*4 1 I Poole-Atkins constant                            [-]
! SECCHI  R*4 1 O secchi depth                                     [m]
!
!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------
!
      IMPLICIT REAL (A-H,J-Z)
!
      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( 23 ) , INCREM(23) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
!
      INTEGER  IP(23)
      INTEGER  IFLUX , ISEG
      REAL     AH_380, EXT   , PAC  , SECCH, AIM1  , AIM2  , AIM3  ,
     J         POC1  , POC2  , POC3  , POC4  , CHLORP, DIEP1 , DIEP2 ,
     J         CORCHL, C_DET , C_GL1 , C_GL2 , HELHUM, TAU   , ANGLE ,
     J         DETCDM, GLOEIR, DETRIC, EXTIO , EXTP_D, D_1   , SW_UITZ
!
      IP  = IPOINT
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG
      IF (BTEST(IKNMRK(ISEG),0)) THEN
!
          SW_UITZ = PMSA(IP(11))
          IF (NINT(SW_UITZ) .EQ. 0) THEN
!
!  Calculate secchi depth without UITZICHT
!
              EXT = PMSA(IP(1))
              PAC = PMSA(IP(22))
              IF ( EXT .GT. 0.0 ) THEN
                  SECCH = PAC/EXT
              ELSE
                  SECCH = -999.
              ENDIF
!
          ELSE
!
!  Calculate secchi depth with UITZICHT
!
              AIM1   = PMSA(IP(2))
              AIM2   = PMSA(IP(3))
              AIM3   = PMSA(IP(4))
              POC1   = PMSA(IP(5))
              POC2   = PMSA(IP(6))
              POC3   = PMSA(IP(7))
              POC4   = PMSA(IP(8))
              AH_380 = PMSA(IP(9))
              CHLORP = PMSA(IP(10))
              DIEP1  = PMSA(IP(12))
              DIEP2  = PMSA(IP(13))
              CORCHL = PMSA(IP(14))
              C_DET  = PMSA(IP(15))
              C_GL1  = PMSA(IP(16))
              C_GL2  = PMSA(IP(17))
              HELHUM = PMSA(IP(18))
              TAU    = PMSA(IP(19))
              ANGLE  = PMSA(IP(20))
              DETCDM = PMSA(IP(21))
!
              DETRIC = MAX ( 0.0, DETCDM * (POC1 + POC2 + POC3 + POC4) )
              GLOEIR = AIM1 + AIM2 + AIM3
!
!  Calculate total extinction with UITZICHT
!
              CALL UIT_ZI( DIEP1 , DIEP2 , ANGLE , C_GL1 , C_GL2 ,
     1                     C_DET , HELHUM, TAU   , CORCHL, CHLORP,
     2                     DETRIC, GLOEIR, AH_380, SECCH, D_1   ,
     3                     EXTIO , EXTP_D,.TRUE.)
!
          ENDIF
!
          PMSA(IP(23)) = SECCH
!
      ENDIF
!
      IFLUX = IFLUX + NOFLUX
      IP   = IP   + INCREM
!
 9000 CONTINUE
!
      RETURN
!
      END
