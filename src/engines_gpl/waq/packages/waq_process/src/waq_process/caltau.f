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
      module m_caltau

      implicit none

      contains


      subroutine caltau ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_write_error_message
      use m_evaluate_waq_attribute

!>\file
!>       Calculation of bottom friction

!
!     Description of the module :
!
!        General water quality module for DELWAQ:
!        CALCULATE SHEER STRESS, CAUSED BY FLOW, WIND AND HUMAN ACIVITYS
!        (SHIPS, DREDGING). MODEL IS VERTICALLY AVERAGED.
!
!        THE SUBROUTINE ALSO PRODUCES A CALCULATED STREAM VELOCITY
!        BASED ON TAU-TOTAL FOR COMPARISON WITH CRITICAL STREAM VELOCITIES
!        IN STEAD OF CRITICAL TAUS (DELTA STUDY, T692).
!
!        AVERAGED MODELS
!
! Name    T   L I/O  Description                              Units
! ----    --- -  -   -------------------                      ----
! CHZ     R   1  L   Chezy coefficient                         [sqrt(m)/s]
! DEPTH   R   1  I   Water depth                                       [m]
! TOTDEP  R   1  I   Total water depth                                 [m]
! G       R   1  I   Acceleration of gravity                        [m/s2]
! RHOW    R   1  I   Density of water                              [kg/m3]
! TAUWIN  R   1  O   Shearstress by wind                  [kg/m/s2 = N/m2]
! TAUFLO  R   1  O   Shearstress by flow                            [N/m2]
! TAUSCH  R   1  O   Shearstress by ships and human activity        [N/m2]
! TAU     R   1  O   Total shearstress                              [N/m2]
! TAUVEL  R   1  O   Calculated velocity based on TAU                [m/s]
! VELOC   R   1  I   Velocity                                        [m/s]

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT REAL (A-H,J-Z)
      IMPLICIT INTEGER (I)

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
!
!     Local declarations, constants in source
!
      PARAMETER ( G      =     9.8    ,
     +            RHOW   =  1000.0    ,
     +            PI     = 3.14159265 ,
     +            KARMAN = 0.41       ,
     +            GRVITY = 9.811      )
      INTEGER     LUNREP
!
      IP1  = IPOINT( 1)
      IP2  = IPOINT( 2)
      IP3  = IPOINT( 3)
      IP4  = IPOINT( 4)
      IP5  = IPOINT( 5)
      IP6  = IPOINT( 6)
      IP7  = IPOINT( 7)
      IP8  = IPOINT( 8)
      IP9  = IPOINT( 9)
      IP10 = IPOINT(10)
      IP11 = IPOINT(11)
      IP12 = IPOINT(12)
      IP13 = IPOINT(13)
      IP14 = IPOINT(14)
      IP15 = IPOINT(15)
!
      KARMC1 = SQRT(GRVITY) / KARMAN
      KARMC2 = KARMAN / SQRT(GRVITY)
!
      IFLUX = 0
      DO 9000 ISEG = 1 , NOSEG

      IF (BTEST(IKNMRK(ISEG),0)) THEN
      CALL evaluate_waq_attribute(2,IKNMRK(ISEG),IKMRK2)
      IF (IKMRK2.EQ.0 .OR. IKMRK2 .EQ. 3) THEN
!
      H       = PMSA(IP1 )
      RL      = PMSA(IP2 )
      T       = PMSA(IP3 )
      TAUSCH  = PMSA(IP4 )
      ISWTAUVELOC = NINT(PMSA(IP5 ))
      TAUFLO  = PMSA(IP6 )
      VELOC   = PMSA(IP7 )
      CHZ     = PMSA(IP8 )
      TOTDEP  = PMSA(IP9 )
      ISWTAU  = NINT(PMSA(IP10))
      DEPTH   = PMSA(IP11)
!
!     Nelson criteria
!
      IF (H .GT. 0.55*TOTDEP) H = 0.55*TOTDEP
!
      CHZ=MAX(1.0,CHZ)
      IF ((ISWTAU .NE. 1) .AND. (ISWTAU .NE. 2) .AND. (ISWTAU .NE. 3))
     &     CALL write_error_message ('Invald switch for tau (ISWTAU) in CALTAU')
      IF ((ISWTAUVELOC .NE. 1) .AND. (ISWTAUVELOC .NE. 2))
     &     CALL write_error_message ('Invald switch for tau (ISWTAUVELOC) in CALTAU')

!     Initialisation
      TAU     = 0.0
      TAUWIN  = 0.0
      TAUVEL  = 0.0

!     Calculate roughness from Chezy
      ROUGH = 12. * TOTDEP / (10**(CHZ/18.))

!     Calculate Chezy 3D
      IF (IKMRK2.EQ.3) THEN
! we are doing a bottom cell --> it must be 3d
         IF ( TOTDEP > 0.0 ) THEN
            CHZ3D = KARMC1 *   LOG( 1 + ( ( 0.5 * DEPTH )
     +                     / (TOTDEP * EXP(-1.*(1+(KARMC2 * CHZ))))
     +                       )     )
            CHZ3D=MAX(1.0,CHZ3D)
         ELSE
            CHZ3D = 1.0
         ENDIF
      ELSE
         CHZ3D = CHZ
      ENDIF

!     Shear stress by flow, calculate if wanted otherwise from input

      IF ( ISWTAUVELOC .EQ. 1 ) THEN
         TAUFLO = RHOW * G * VELOC**2 / CHZ3D**2
      ENDIF

!     Shear stress by waves

      IF ( ABS(H) .LT. 1.E-20 .OR. ABS(RL) .LT. 1.E-20 .OR.
     +     ABS(T) .LT. 1.E-20                              ) THEN
         TAUWIN = 0
      ELSE
         A6     = 2.0 * PI*TOTDEP/RL
         IF ( A6 .LE. 10.0 ) THEN
            UBG    = PI  * H/(T * SINH(A6))
            ALM    = UBG * T / ( 2*PI )
            RLF    = ALM/ROUGH
            IF (ISWTAU .EQ. 1) THEN
!           option Tamminga
                FWG    = 0.16* SQRT( ROUGH / ALM )
            ELSEIF (ISWTAU .EQ. 2) THEN
!           option Swart
                IF ( RLF .GT. (PI/2) ) THEN
                  FWG = 0.00251 * EXP ( 5.213 * RLF**(-0.19) )
                ELSE
                  FWG = 0.32
                ENDIF
            ELSEIF (ISWTAU .EQ. 3) THEN
!           option Soulsby
                FWG    = 0.237* RLF**(-0.52)
            ENDIF

            TAUWIN = FWG * 0.25 * RHOW * UBG**2
         ELSE
            TAUWIN = 0.0
         ENDIF
      ENDIF

  150 CONTINUE

!     Total shear stress moet eigenlijk via windrichting opgeteld worden,
!     gebeurt hier niet

!     Als TAUSCH = -1.0 dan wordt tau -1.0
      IF (TAUSCH .EQ. -1.0) THEN
          TAU = -1.0
          TAUVEL = -1.0
      ELSE
         TAU = TAUFLO  + TAUWIN + TAUSCH
!PBO  Re-calculate total sheerstress (TAU) to a total stream velocity
         TAUVEL = SQRT ( TAU * CHZ3D**2 / (RHOW * G) )
      ENDIF

      PMSA (IP12) = TAU
      PMSA (IP13) = TAUFLO
      PMSA (IP14) = TAUWIN
      PMSA (IP15) = TAUVEL
!
      ENDIF
      ENDIF
!
      IFLUX = IFLUX + NOFLUX
      IP1   = IP1   + INCREM (  1 )
      IP2   = IP2   + INCREM (  2 )
      IP3   = IP3   + INCREM (  3 )
      IP4   = IP4   + INCREM (  4 )
      IP5   = IP5   + INCREM (  5 )
      IP6   = IP6   + INCREM (  6 )
      IP7   = IP7   + INCREM (  7 )
      IP8   = IP8   + INCREM (  8 )
      IP9   = IP9   + INCREM (  9 )
      IP10  = IP10  + INCREM ( 10 )
      IP11  = IP11  + INCREM ( 11 )
      IP12  = IP12  + INCREM ( 12 )
      IP13  = IP13  + INCREM ( 13 )
      IP14  = IP14  + INCREM ( 14 )
      IP15  = IP15  + INCREM ( 15 )
!
 9000 CONTINUE
!

      RETURN
      END

      end module m_caltau
