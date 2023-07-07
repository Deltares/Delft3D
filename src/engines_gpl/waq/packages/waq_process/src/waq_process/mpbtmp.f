!!  Copyright(C) Stichting Deltares, 2012-2023.
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

      SUBROUTINE MPBTMP ( PMSA   , FL     , IPOINT , INCREM , NOSEG  ,
     +                    NOFLUX , IEXPNT , IKNMRK , NOQ1   , NOQ2   ,
     +                    NOQ3   , NOQ4   )
C***********************************************************************
C     +----------------------------------------+
C     |    D E L F T   H Y D R A U L I C S     |
C     +----------------------------------------+
C***********************************************************************
C
C     Function : MPB nutrient limitation function
C
C***********************************************************************

      IMPLICIT NONE

C     arguments

      REAL               :: PMSA(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
      REAL               :: FL(*)              ! in/out flux array
      INTEGER            :: IPOINT(*)          ! in     start index input-output parameters in the PMSA array (segment or exchange number 1)
      INTEGER            :: INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the PMSA array
      INTEGER            :: NOSEG              ! in     number of segments
      INTEGER            :: NOFLUX             ! in     total number of fluxes (increment in FL array)
      INTEGER            :: IEXPNT(4,*)        ! in     exchange pointer table
      INTEGER            :: IKNMRK(*)          ! in     segment features array
      INTEGER            :: NOQ1               ! in     number of exchanges in first direction
      INTEGER            :: NOQ2               ! in     number of exchanges in second direction
      INTEGER            :: NOQ3               ! in     number of exchanges in third direction
      INTEGER            :: NOQ4               ! in     number of exchanges in fourth direction

C     from PMSA array

      REAL               :: TEMP               !  1 in  , ambient water temperature                     (oC)
      REAL               :: KTGP               !  2 in  , MPB1 temperature coefficient gross production  (-)
      INTEGER            :: ITIME              !  3 in  , DELWAQ time                                  (scu)
      INTEGER            :: IDT                !  4 in  , DELWAQ timestep                              (scu)
      INTEGER            :: ITSTRT             !  5 in  , DELWAQ start time                            (scu)
      INTEGER            :: AUXSYS             !  6 in  , ratio between days and system clock        (scu/d)
      REAL               :: FTMP               !  7 i/o , MPB temperature function                       (-)
      REAL               :: WS                 !  8 i/o , workspace MPB temperature function             (-)

C     local decalrations

      INTEGER            :: ISEG               ! loop counter segment loop
      INTEGER, parameter :: NO_POINTER = 10    ! number of input output variables in PMSA array
      INTEGER            :: IP(NO_POINTER)     ! index pointer in PMSA array updated for each segment
      REAL               :: FTMP_NOW           ! actual MPB temperature function                         (-)

C     initialise pointers for PMSA and FL array

      IP = IPOINT(1:NO_POINTER)

C     loop over the segments

      DO 1000 ISEG = 1 , NOSEG

C        input, the workspace and ftmp are input-output only the input pointer is used

         TEMP       = PMSA(IP(1))
         KTGP       = PMSA(IP(2))
         ITIME      = NINT(PMSA(IP(3)))
         IDT        = NINT(PMSA(IP(4)))
         ITSTRT     = NINT(PMSA(IP(5)))
         AUXSYS     = NINT(PMSA(IP(6)))
         FTMP       = PMSA(IP(7))
         WS         = PMSA(IP(8))

         FTMP_NOW = KTGP**(TEMP-20.)

C        update FTMP every day (AUXSYS is one day)

         IF   ( MOD(ITIME-ITSTRT,AUXSYS) .LT. IDT )   THEN
            IF ( ITIME .EQ. ITSTRT ) THEN
               FTMP  = FTMP_NOW
            ELSE
               FTMP  = WS / AUXSYS
            ENDIF
            WS       = 0.0
         ENDIF

C        cummulate in workspace

         WS          = WS  + FTMP_NOW * IDT

C        output

         PMSA(IP(7)) = FTMP
         PMSA(IP(8)) = WS

C        update pointering in PMSA

         IP    = IP    + INCREM(1:NO_POINTER)

 1000 CONTINUE


      RETURN
      END
