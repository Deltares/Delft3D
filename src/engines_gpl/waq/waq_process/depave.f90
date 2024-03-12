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
module m_depave
    use m_waq_precision

    implicit none

contains


    subroutine depave (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_srstop
        use m_monsys

        !>\file
        !>       Average depth for a Bloom time step (typically a day)

        !
        !     Description of the module :
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        INTEGER(kind = int_wp) :: LUNREP

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6
        REAL(kind = real_wp) :: DEPTH, ADEPTH
        INTEGER(kind = int_wp) :: TELLER, NAVERA, NSWITS, ISEG
        LOGICAL  FIRST
        SAVE     FIRST
        DATA     FIRST /.TRUE./
        SAVE     TELLER
        DATA     TELLER /0/

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)

        !     Check whether certain input parameters are independent of X

        IF (FIRST) THEN
            FIRST = .FALSE.
            IF ((INCREM(1) > 0) .OR. &
                    (INCREM(2) > 0)) THEN
                CALL GETMLU(LUNREP)
                WRITE (LUNREP, *) &
                        ' DEPAVE: INPUT parameters function(x) not ALLOWED'
                WRITE (*, *) &
                        ' DEPAVE: INPUT parameters function(x) not ALLOWED'
                CALL SRSTOP(1)
            ENDIF
        ENDIF

        !     Retrieve switch for averaging and nr. of steps to be averaged

        NSWITS = NINT(PMSA(IP1))
        NAVERA = NINT(PMSA(IP2))

        !     Add 1 to counter and check for period

        TELLER = TELLER + 1
        IF (TELLER > NAVERA) TELLER = TELLER - NAVERA

        !     Loop over segments

        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                DEPTH = PMSA(IP3)
                ADEPTH = PMSA(IP4)
                PMSA(IP6) = ADEPTH

                IF (NSWITS == 0) THEN

                    !                 No averaging: copy depth to average depth

                    PMSA(IP5) = DEPTH

                ELSE

                    !                 Averaging: FANCY FORMULA!!!!!

                    PMSA(IP5) = (ADEPTH * REAL(TELLER - 1) + DEPTH) &
                            / REAL(TELLER)
                ENDIF
            ENDIF
            !
            IP1 = IP1 + INCREM(1)
            IP2 = IP2 + INCREM(2)
            IP3 = IP3 + INCREM(3)
            IP4 = IP4 + INCREM(4)
            IP5 = IP5 + INCREM(5)
            IP6 = IP6 + INCREM(6)
            !
        end do
        !
        RETURN
        !
    END

end module m_depave
