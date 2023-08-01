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
      module m_staday

      implicit none

      contains


      subroutine staday ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_evaluate_waq_attribute

!>\file
!>       Periodic (day) average of a given substance

!
!     Description of the module :
!
! Name    T   L I/O   Description                                  Units
! ----    --- -  -    -------------------                          -----
!
! CONC           I    Concentration of the substance            1
! TINIT         I/O   Initial time (reset at end period)        2
! PERIOD         I    Period of te periodic average             3
! TIME           I    Time in calculation                       4
! DELT           I    Timestep                                  5
!
! TCOUNT         O    Count of times (must be imported!)        6
! AVCUM          O    Work array for summing over time          7
! AVPERD         O    Periodic average (calcuated at the end)   8
!
! Note: to prevent strange results, the actual output parameter is
!       AVPERD. This is updated once in a while!
!

!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      IMPLICIT NONE

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
!
      INTEGER  IP1   , IP2   , IP3   , IP4   , IP5   ,
     +         IP6   , IP7   , IP8   ,
     +         IN1   , IN2   , IN3   , IN4   , IN5   ,
     +         IN6   , IN7   , IN8
      INTEGER  IKMRK , ISEG
      INTEGER  IACTION
      INTEGER  ATTRIB
      REAL     TINIT , PERIOD, TIME  , DELT  , TCOUNT

      INTEGER, PARAMETER :: MAXWARN = 50
      INTEGER, SAVE      :: NOWARN  = 0

      IP1 = IPOINT(1)
      IP2 = IPOINT(2)
      IP3 = IPOINT(3)
      IP4 = IPOINT(4)
      IP5 = IPOINT(5)
      IP6 = IPOINT(6)
      IP7 = IPOINT(7)
      IP8 = IPOINT(8)

      IN1 = INCREM(1)
      IN2 = INCREM(2)
      IN3 = INCREM(3)
      IN4 = INCREM(4)
      IN5 = INCREM(5)
      IN6 = INCREM(6)
      IN7 = INCREM(7)
      IN8 = INCREM(8)

      TINIT  = PMSA(IP2)
      PERIOD = PMSA(IP3)
      TIME   = PMSA(IP4)
      DELT   = PMSA(IP5)
      TCOUNT = PMSA(IP6)

!
!      Start and stop criteria are somewhat involved:
!      - The first time for the first period is special, as this
!        is the only time there is no previous period.
!      - If there is a previous period, update the averages
!        for that period and reset the accumulative values
!        for the next
!
!      To formulate the ideas more clearly:
!      - The first period is a closed interval
!      - All other periods are half-open intervals (the last time
!        of the previous period should not be reused.)
!
      IACTION = 0
      IF ( TIME >= TINIT-0.5*DELT ) THEN
         IACTION = 2
         IF ( TIME <= TINIT+0.5*DELT ) THEN
            DO ISEG=1,NOSEG
               IP6       = IPOINT(6) + (ISEG-1) * INCREM(6)
               IP7       = IPOINT(7) + (ISEG-1) * INCREM(7)
               PMSA(IP6) = 0.0
               PMSA(IP7) = 0.0
            ENDDO
         ENDIF
      ENDIF

      IF ( TIME .GE. TINIT+PERIOD-0.5*DELT .AND. TIME .LE. TINIT+PERIOD+0.5*DELT ) THEN
         IACTION = 3
      ENDIF

      IF ( IACTION .EQ. 0 ) RETURN

      IP6    = IPOINT(6)
      IP7    = IPOINT(7)

      DO 9000 ISEG=1,NOSEG
         IF (BTEST(IKNMRK(ISEG),0)) THEN

!
!           Keep track of the time within the current quantile specification
!           that each segment is active
!
            TCOUNT    = PMSA(IP6) + DELT
            PMSA(IP6) = TCOUNT

            PMSA(IP7)  = PMSA(IP7) + PMSA(IP1) * DELT

         ENDIF
!
!        Always do the final processing whether the segment is active at this moment or not
!

         IF ( IACTION .EQ. 3 ) THEN
            IF ( TCOUNT .GT. 0.0 ) THEN
               PMSA(IP8) = PMSA(IP7) / TCOUNT
            ELSE
               PMSA(IP8) = 0.0

               IF ( NOWARN < MAXWARN ) THEN
                  CALL evaluate_waq_attribute(IKNMRK(ISEG), 3, ATTRIB )
                  IF ( ATTRIB .NE. 0 ) THEN
                     NOWARN = NOWARN + 1
                     WRITE(*,'(a,i0)') 'Periodic average could not be determined for segment ', ISEG
                     WRITE(*,'(a)')    '    - segment was not active. Average set to zero'

                     IF ( NOWARN == MAXWARN ) THEN
                        WRITE(*,'(a)') '(Further messages suppressed)'
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF

!
!           Reset for the next round
!
            PMSA(IP6) = 0.0
            PMSA(IP7) = 0.0
         ENDIF

         IP1  = IP1  + IN1
         IP6  = IP6  + IN6
         IP7  = IP7  + IN7
         IP8  = IP8  + IN8

 9000 CONTINUE

!
!     Be sure to also reset the initial time, so that we can restart the
!     averaging for the next period
!
      IF ( IACTION .EQ. 3 ) THEN
         PMSA(IP2) = TINIT  + PERIOD
      ENDIF

      RETURN
      END

      end module m_staday
