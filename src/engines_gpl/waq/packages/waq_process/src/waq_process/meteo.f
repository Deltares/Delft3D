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

      subroutine meteo  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Process meteo from various meteo-stations

!
!     Description of the module :
!
!        General water quality module for DELWAQ:
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! Y       R*4 8 I     dependent value pairs
! X       R*4 8 I     independent value pairs
! VALUE   R*4 1 I     independent value
! RESULT  R*4 1 I     resulting dependent value
!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
      INTEGER   MAXSTA,MAXVAR, IP , NP
!
!     aantal meteo stations
!
      PARAMETER (MAXSTA=10)
!
!     aantal variabelen per station
!
      PARAMETER (MAXVAR=7)

!
!     aantal ongebonden variabelen
!
      PARAMETER (NP=5)


      DIMENSION RAD(MAXSTA),VWIND(MAXSTA), DIR(MAXSTA), HUM(MAXSTA),
     &          TEMP(MAXSTA), PRES(MAXSTA), SUN(MAXSTA)
      DIMENSION X(MAXSTA), Y(MAXSTA)
      DIMENSION IP((MAXSTA+1)* MAXVAR +  MAXSTA*2 + NP)
      DIMENSION DIST(MAXSTA), WFAC(MAXSTA)

      DO 10 I=1, (MAXSTA + 1)  * MAXVAR +  MAXSTA*2 + NP
        IP(I) = IPOINT(I)
   10 CONTINUE
!
      DO 9000 ISEG = 1 , NOSEG

      IF (BTEST(IKNMRK(ISEG),0)) THEN
!
!     waarden per station
!
      DO 20 I   = 1,MAXSTA
        RAD(I)  = PMSA(IP((I-1)*MAXVAR+1))
        VWIND(I)= PMSA(IP((I-1)*MAXVAR+2))
        DIR(I)  = PMSA(IP((I-1)*MAXVAR+3))
        HUM(I)  = PMSA(IP((I-1)*MAXVAR+4))
        TEMP(I) = PMSA(IP((I-1)*MAXVAR+5))
        PRES(I) = PMSA(IP((I-1)*MAXVAR+6))
        SUN(I) =  PMSA(IP((I-1)*MAXVAR+7))
   20 CONTINUE
!
!     coordinaten van de stations

      DO 21 I = 1, MAXSTA
          X(I) = PMSA(IP(MAXSTA*MAXVAR+(I-1)*2+1))
          Y(I) = PMSA(IP(MAXSTA*MAXVAR+(I-1)*2+2))
   21 CONTINUE
!
!     overige parameters
!
      SCALE  = PMSA(IP(MAXSTA*(MAXVAR+2)+1))
      NOSTAT = PMSA(IP(MAXSTA*(MAXVAR+2)+2))
      ICALCSW= NINT(PMSA(IP(MAXSTA*(MAXVAR+2)+3)))
      XSEG   = PMSA(IP(MAXSTA*(MAXVAR+2)+4))
      YSEG   = PMSA(IP(MAXSTA*(MAXVAR+2)+5))

!*******************************************************************************
!**** RESULT Calculated meteo parameters based on 1-MAXSTA meteo stations.
!*****       Option 1 looks up the nearest meteo station for each segment
!*****       Option 2 calculates the distance weighted average of NoStations
!*****                for each segment
!***********************************************************************

!     Bereken Distance Cell to all stations (in meters)
      MIN = -1.0
      SUM = 0.0
      SUM2 = 0.0
      IF ( NOSTAT .GT. MAXSTA ) THEN
          NOSTAT = MAXSTA
      ENDIF

      IF ( NOSTAT .LT. 1) THEN
          NOSTAT = 1
      ENDIF

!
      DO 30 I = 1, NOSTAT
          DIST(I) = SQRT ( (XSEG - X(I)*SCALE)*(XSEG - X(I)*SCALE) +
     &                     (YSEG - Y(I)*SCALE)*(YSEG - Y(I)*SCALE) )
!
        dist(i) = 1./max(dist(i),1.0)
        SUM  = SUM  + DIST(I)
        SUM2 = SUM2 + DIST(I) * DIST(I)

!
      IF (MIN .LT. 0.0) THEN
           MIN = DIST(I)
           INEAR = I
      ELSEIF (DIST(I) .LT. MIN) THEN
           MIN = DIST(I)
           INEAR = I
      ENDIF
!
   30 CONTINUE


!
!     optie 1:  nearest station
!
      IF  ( ICALCSW .EQ. 1 ) THEN
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +1)) = RAD(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +2)) = VWIND(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +3)) = DIR(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +4)) = HUM(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +5)) = TEMP(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +6)) = PRES(INEAR)
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP +7)) = SUN(INEAR)
!
!     optie 2a: dist weighted lineair
!
      ELSE
!         optie 2 lineair inv dist
          IF ( ICALCSW .EQ. 2 ) THEN
              DO 41 I = 1 ,  NOSTAT
                                WFAC(I) = DIST(I) / SUM
   41         CONTINUE
!
!         optie 2b: inv dist kwadratisch
          ELSEIF ( ICALCSW .EQ. 3 ) THEN
              DO 42 I = 1 ,  NOSTAT
                  WFAC(I) = DIST(I)*DIST(I) / SUM2
   42         CONTINUE

          ENDIF

          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 1)) = 0.0
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 2)) = 0.0
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 4)) = 0.0
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 5)) = 0.0
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 6)) = 0.0
          PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 7)) = 0.0
          DO 50 I = 1, NOSTAT
              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 1)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 1)) + WFAC(I) * RAD(I)

              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 2)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 2)) + WFAC(I) * VWIND(I)

              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 4)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 4)) + WFAC(I) * HUM(I)

              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 5)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 5)) + WFAC(I) * TEMP(I)

              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 6)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 6)) + WFAC(I) * PRES(I)

              PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 7)) =
     &        PMSA(IP(MAXSTA*(MAXVAR+2)+NP + 7)) + WFAC(I) * SUN(I)

   50     CONTINUE
!
!         wind ricthing niet middelen
!
          PMSA(IP(MAXSTA*(MAXVAR+2) + NP +3)) = DIR(INEAR)

      ENDIF

      ENDIF

!
      DO 60 I=1, (MAXSTA + 1) * MAXVAR + MAXSTA*2 + NP
          IP(I) = IP(I) + INCREM (I)
   60 CONTINUE

!
 9000 CONTINUE
!
      RETURN
!
      END
