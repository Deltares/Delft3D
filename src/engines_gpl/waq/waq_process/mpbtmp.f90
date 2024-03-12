!!  Copyright(C) Stichting Deltares, 2012-2024.
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
module m_mpbtmp
    use m_waq_precision

    implicit none

contains


    SUBROUTINE MPBTMP (PMSA, FL, IPOINT, INCREM, NOSEG, &
            NOFLUX, IEXPNT, IKNMRK, NOQ1, NOQ2, &
            NOQ3, NOQ4)
        !     ***********************************************************************
        !          +----------------------------------------+
        !          |    D E L F T   H Y D R A U L I C S     |
        !          +----------------------------------------+
        !     ***********************************************************************
        !
        !          Function : MPB nutrient limitation function
        !
        !     ***********************************************************************

        IMPLICIT NONE

        !          arguments

        REAL(kind = real_wp) :: PMSA(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
        REAL(kind = real_wp) :: FL(*)              ! in/out flux array
        INTEGER(kind = int_wp) :: IPOINT(*)          ! in     start index input-output parameters in the PMSA array (segment or exchange number 1)
        INTEGER(kind = int_wp) :: INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the PMSA array
        INTEGER(kind = int_wp) :: NOSEG              ! in     number of segments
        INTEGER(kind = int_wp) :: NOFLUX             ! in     total number of fluxes (increment in FL array)
        INTEGER(kind = int_wp) :: IEXPNT(4, *)        ! in     exchange pointer table
        INTEGER(kind = int_wp) :: IKNMRK(*)          ! in     segment features array
        INTEGER(kind = int_wp) :: NOQ1               ! in     number of exchanges in first direction
        INTEGER(kind = int_wp) :: NOQ2               ! in     number of exchanges in second direction
        INTEGER(kind = int_wp) :: NOQ3               ! in     number of exchanges in third direction
        INTEGER(kind = int_wp) :: NOQ4               ! in     number of exchanges in fourth direction

        !          from PMSA array

        REAL(kind = real_wp) :: TEMP               !  1 in  , ambient water temperature                     (oC)
        REAL(kind = real_wp) :: KTGP               !  2 in  , MPB1 temperature coefficient gross production  (-)
        INTEGER(kind = int_wp) :: ITIME              !  3 in  , DELWAQ time                                  (scu)
        INTEGER(kind = int_wp) :: IDT                !  4 in  , DELWAQ timestep                              (scu)
        INTEGER(kind = int_wp) :: ITSTRT             !  5 in  , DELWAQ start time                            (scu)
        INTEGER(kind = int_wp) :: AUXSYS             !  6 in  , ratio between days and system clock        (scu/d)
        REAL(kind = real_wp) :: FTMP               !  7 i/o , MPB temperature function                       (-)
        REAL(kind = real_wp) :: WS                 !  8 i/o , workspace MPB temperature function             (-)

        !          local decalrations

        INTEGER(kind = int_wp) :: ISEG               ! loop counter segment loop
        INTEGER(kind = int_wp), parameter :: NO_POINTER = 10    ! number of input output variables in PMSA array
        INTEGER(kind = int_wp) :: IP(NO_POINTER)     ! index pointer in PMSA array updated for each segment
        REAL(kind = real_wp) :: FTMP_NOW           ! actual MPB temperature function                         (-)

        !          initialise pointers for PMSA and FL array

        IP = IPOINT(1:NO_POINTER)

        !          loop over the segments

        DO ISEG = 1, NOSEG

            !             input, the workspace and ftmp are input-output only the input pointer is used

            TEMP = PMSA(IP(1))
            KTGP = PMSA(IP(2))
            ITIME = NINT(PMSA(IP(3)))
            IDT = NINT(PMSA(IP(4)))
            ITSTRT = NINT(PMSA(IP(5)))
            AUXSYS = NINT(PMSA(IP(6)))
            FTMP = PMSA(IP(7))
            WS = PMSA(IP(8))

            FTMP_NOW = KTGP**(TEMP - 20.)

            !             update FTMP every day (AUXSYS is one day)

            IF   (MOD(ITIME - ITSTRT, AUXSYS) < IDT)   THEN
                IF (ITIME == ITSTRT) THEN
                    FTMP = FTMP_NOW
                ELSE
                    FTMP = WS / AUXSYS
                ENDIF
                WS = 0.0
            ENDIF

            !             cummulate in workspace

            WS = WS + FTMP_NOW * IDT

            !             output

            PMSA(IP(7)) = FTMP
            PMSA(IP(8)) = WS

            !             update pointering in PMSA

            IP = IP + INCREM(1:NO_POINTER)

        end do

        RETURN
    END

end module m_mpbtmp
