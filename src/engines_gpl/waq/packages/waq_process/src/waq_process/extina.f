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

      subroutine extina ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Extinction of light by algae and POC

      REAL     PMSA  ( * ) , FL    (*)
      INTEGER  IPOINT( * ) , INCREM(*) , NOSEG , NOFLUX,
     +         IEXPNT(4,*) , IKNMRK(*) , NOQ1, NOQ2, NOQ3, NOQ4
!
!     Local declarations
!
      INTEGER  NALG  , ISWFIX, NIPALG, IFLUX , ISEG  ,
     +         IALG  , IP    , IFIX
      REAL(8)  EXTALG, EXTCF , BIOMAS
!
      NALG  = NINT(PMSA(IPOINT(1)))
      ISWFIX= NINT(PMSA(IPOINT(2)))
      IF ( ISWFIX .EQ. 1 ) THEN
         NIPALG = 4
      ELSE
         NIPALG = 2
      ENDIF
      IFLUX = 0

      DO 9000 ISEG = 1 , NOSEG

      IF (BTEST(IKNMRK(ISEG),0)) THEN

      EXTALG = 0.0
      DEPTH  = PMSA ( IPOINT(3) + (ISEG-1)*INCREM(3) )
!
!     Loop over algae

      DO 100 IALG = 1,NALG

          IP = 3 + IALG
          EXTCF  = PMSA ( IPOINT(IP) + (ISEG-1)*INCREM(IP) )

          IP = 3 + NALG + IALG
          BIOMAS = PMSA ( IPOINT(IP) + (ISEG-1)*INCREM(IP) )

          IF ( ISWFIX .EQ. 1 ) THEN
             IP = 3 + 2*NALG + IALG
             IFIX   = NINT(PMSA ( IPOINT(IP) + (ISEG-1)*INCREM(IP) ))
             IF ( IFIX .LT. 0 ) THEN

                ! Rooted algae, inlclude only if sdmix positive

                IP = 3 + 3*NALG + IALG
                SDMIX = PMSA ( IPOINT(IP) + (ISEG-1)*INCREM(IP) )
                IF ( SDMIX .GT. 1E-10 ) THEN
                   BIOMAS = BIOMAS/DEPTH
                ELSE
                   BIOMAS = 0.0
                ENDIF
             ENDIF
          ENDIF

          IF ( BIOMAS .GT. 0.0 )
     J    EXTALG = EXTALG + BIOMAS*EXTCF

  100 CONTINUE

      IP = 3 + NIPALG*NALG + 1
      PMSA ( IPOINT(IP) + (ISEG-1)*INCREM(IP) ) = EXTALG

      ENDIF
!
      IFLUX = IFLUX + NOFLUX
!
 9000 CONTINUE
!
      RETURN

      END
