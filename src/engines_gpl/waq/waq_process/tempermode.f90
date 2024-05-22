!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module m_tempermode
    use m_waq_precision

    implicit none

contains


    subroutine tmode  (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger

        !>\file
        !>       Defnines meaning of two modelled statevars ModTemp and NatTemp

        IMPLICIT NONE

        !     arguments

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

        !     from PMSA array

        REAL(kind = real_wp) :: MTEMP              ! 1  in  Modelled temperature                                [oC]
        REAL(kind = real_wp) :: TMPNAT             ! 2  in  natural temperature of ambient water                [oC]
        INTEGER(kind = int_wp) :: ISWTMP             ! 3  in  DELWAQ process time step                             [d]
        REAL(kind = real_wp) :: TTEMP              ! 4  out Total temperature                                   [oC]
        REAL(kind = real_wp) :: ETEMP              ! 5  out EXCESS! temperature                                 [oC]

        !     local decalrations

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6
        INTEGER(kind = int_wp) :: IFLUX, ISEG, IKMRK2

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)

        DO ISEG = 1, NOSEG

            MTEMP = PMSA(IP1)
            TMPNAT = PMSA(IP2)
            ISWTMP = NINT(PMSA(IP3))

            !        What is the meaning of modelled temperatures (one or two may be modelled)

            !        User defines total and natural
            IF (ISWTMP == 0) THEN
                TTEMP = MTEMP
                ETEMP = TTEMP - TMPNAT
                !        User defines excess and natural
            ELSEIF (ISWTMP == 1) THEN
                ETEMP = MTEMP
                TTEMP = ETEMP + TMPNAT
                !        User defines excess and total
            ELSEIF (ISWTMP == 2) THEN
                ETEMP = MTEMP
                TTEMP = TMPNAT
            ELSE
                CALL write_error_message ('SwitchTemp has no valid value <0,1,2> in TMODE')
            ENDIF

            !
            !        Output flux, temp, surtemp, heat exchage and temperature increase due to radiation
            !

            PMSA (IP4) = TTEMP
            PMSA (IP5) = ETEMP
            PMSA (IP6) = TMPNAT + 1
            !
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP6 = IP6 + INCREM (6)
            !
        end do
        !
        RETURN
        !
    END

end module m_tempermode
