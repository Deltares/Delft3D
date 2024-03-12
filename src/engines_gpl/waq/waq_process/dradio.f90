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
module m_dradio
    use m_waq_precision

    implicit none

contains


    subroutine dradio (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Radio-active decay and estimation of the radiation
        !!       Because the module produces both the radio-active decay (as a
        !!       flux and an estimate of the radiation, it requires the atomic
        !!       mass of the substance. The concentration is expected to be in
        !!       mg/m3, so that the radiation is in Bq/m3.
        !!
        !!       Usage note:
        !!       The half-life and the atomic mass are physical constants. The
        !!       module therefore assumes they are DELWAQ constants, not
        !!       general input parameters. This is slightly more efficient.

        !
        !        Conversion of radiation units: https://www.remm.nlm.gov/radmeasurement.htm
        !        Half-life constants: https://en.wikipedia.org/wiki/List_of_radioactive_nuclides_by_half-life
        !

        !
        !     Description of the module :
        !
        ! Name      T   L I/O   Description                                    Units
        ! ----      --- -  -    -------------------                             ----
        ! CONC      R*4 1 I     Concentration of the radio-active substance    mg/m3
        ! HALFLIFE  R*4 1 I     Half-life                                      year
        ! ATOMMASS  R*4 1 I     Atomic mass                                    g/mol
        ! RADIODC   R*4 1 O     Radio-active decay                             mg/m3/day
        ! RADIATION R*4 1 O     Radiation intensity                            Bq/m3

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IFLUX, ISEG
        REAL(kind = real_wp) :: CONC, HALFLIFE, ATOMMASS, RADIODC, RADIATION
        REAL(kind = real_wp) :: DRADDECAY, RADIATION_CONV

        REAL(kind = real_wp), PARAMETER :: AVOGADRO = 6.022E23 ! /mol

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        !
        IFLUX = 0

        !
        !     Calculate the decay rate from the half-life as well
        !     as the conversion to Bq/m3
        !
        HALFLIFE = PMSA(IP2)
        ATOMMASS = PMSA(IP3)
        DRADDECAY = LOG(2.0) / HALFLIFE / 365.0 ! /day
        RADIATION_CONV = DRADDECAY      & ! /day
                / 86400.0     & ! s/day -> /s
                * AVOGADRO    & ! number per mol
                / ATOMMASS    & ! g/mol -> number per g
                / 1.0E3      ! g/mg  -> /mg
        ! number / mg / s = Bq/mg

        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                CONC = PMSA(IP1)
                !
                !     Calculate decay
                !
                RADIODC = DRADDECAY * CONC
                !
                !     Output
                !
                PMSA(IP4) = RADIODC * RADIATION_CONV
                FL(1 + IFLUX) = RADIODC
                !
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + INCREM (1)
            IP4 = IP4 + INCREM (4)
            !
        end do
        !
        RETURN
    END

end module m_dradio
