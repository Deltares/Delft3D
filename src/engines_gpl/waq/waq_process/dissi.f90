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
module m_dissi
    use m_waq_precision

    implicit none

contains


    subroutine dissi  (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        use m_logger

        !>\file
        !>       Dissolution of Si in opal

        !
        !     Description of the module :
        !     dissolution of opal silicate
        !
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! CSID    R*4 1 I     concentration dissolved silicate            [gSi/m3]
        ! CSIDE   R*4 1 I     saturation concentration dissolved silicate [gSi/m3]
        ! FSOL    R*4 1 O     dissolution flux                          [gSi/m3/d]
        ! KSOL    R*4 1 I     dissolution rate                          [m3/gSi/d]
        ! OPAL    R*4 1 I     concentration opal silicate                 [gSi/m3]
        ! POROS   R*4 1 I     porosity                                         [-]
        ! TC      R*4 1 I     temperature coefficient of dissolution           [-]
        ! TEMP    R*4 1 I     temperature                                     [oC]
        ! TEMPC   R*4 1 -     temperature function                             [-]
        ! SWDISSI R*4 1 I     option: 0.0 2nd order diss., 1.0 1st order diss.
        !
        !     Logical Units : -
        !
        !     Modules called : -
        !
        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        REAL(kind = real_wp) :: KSOL, FSOL, TEMP, TEMPC, TC, CSID, OPAL, &
                CSIDE, POROS, SWDISSI
        INTEGER(kind = int_wp) :: LUNREP, NOWARN
        DATA     NOWARN / 0 /
        SAVE     NOWARN
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        !
        IFLUX = 0
        !
        DO ISEG = 1, NOSEG
            !
            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CSID = MAX(PMSA(IP1), 0.0)
                OPAL = MAX(PMSA(IP2), 0.0)
                CSIDE = PMSA(IP3)
                KSOL = PMSA(IP4)
                TC = PMSA(IP5)
                TEMP = PMSA(IP6)
                POROS = PMSA(IP7)
                SWDISSI = PMSA(IP8)
                !
                !     Calculation of the dissolution flux
                !
                FSOL = 0.0
                !
                IF (POROS > 0.05) THEN
                    !
                    TEMPC = TC**(TEMP - 20.0)
                    !
                    IF (NINT(SWDISSi) == 0) THEN
                        FSOL = KSOL * TEMPC * OPAL * (CSIDE - CSID / POROS)
                    ELSE
                        FSOL = KSOL * TEMPC * OPAL
                    ENDIF
                    !
                ELSE
                    FSOL = 0.0
                    NOWARN = NOWARN + 1
                    IF (NOWARN <= 25) THEN
                        CALL get_log_unit_number(LUNREP)
                        write (LUNREP, *) 'warning: poros < 0.05 in process DisSi, ISEG=', ISEG, ' POROS=', POROS
                    ELSEIF (NOWARN == 26) THEN
                        CALL get_log_unit_number(LUNREP)
                        write (LUNREP, *) 'number of warnings poros < 0.05 in process DisSi >25 firther messages surpressed'
                    ENDIF
                ENDIF
                !
                !     Output of module
                !
                FL(1 + IFLUX) = FSOL
                PMSA(IP9) = FSOL
                !
                !     End active cells block
                !
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            IP6 = IP6 + INCREM (6)
            IP7 = IP7 + INCREM (7)
            IP8 = IP8 + INCREM (8)
            IP9 = IP9 + INCREM (9)
            !
        end do
        !
        RETURN
        !
    END

end module m_dissi
