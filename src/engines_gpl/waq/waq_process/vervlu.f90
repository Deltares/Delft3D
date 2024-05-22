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
module m_vervlu
    use m_waq_precision

    implicit none

contains


    subroutine vervlu (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Atmospheric exchange OMPs (volatilization/intake)

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                    Uni
        ! ----    --- -  -    -------------------                             --
        ! ATMC    R*4 1 I  Concentration OMV in atmosphere                [g.m3]
        ! CONC    R*4 1 I  Total concentration OMV in water               [g.m3]
        ! C1      R*4 1 I  Constant in temperature dependance of Henrys
        !                  value represents delta S0 (entropy) / R           [-]
        ! C2      R*4 1 L  Constant in temperature dependence of Henrys
        !                  value represents delta H0 (enthalpy) / R          [-]
        ! DEPTH   R*4 1 I  Depth                                             [m]
        ! E       R*4 1 LC Natural logaritmic                                [-]
        ! FDIS    R*4 1 I  Fraction omive free dissolved                     [-]
        ! FL      R*4 1 O  Calculated volatilizatioin flux              [g/m3/d]
        ! H0TREF  R*4 1 I  Henrys constant at reference temperature  [Pa.m3\mol]
        ! H2TREF  R*4 1 L  Dimensionless Henry at Tref
        !                  on a basis of moelfraction      [molefracG/molefracL]
        ! H1TEMP  R*4 1 I  Dimensionless Henry at any TEMP     [mol/m3/(mol.m3)]
        ! KL      R*4 1 I  Mass transport coefficient liquid phase         [m/d]
        ! KG      R*4 1 I  Mass transport coefficient gas phase            [m/d]
        ! KV      R*4 1 O  volatilization rate constant                    [m/d]
        ! KELVIN  R*4 1 LC absolute temperature reference                    [-]
        ! NG      R*4 1 L  amount moles in 1m3 gas                     [mole/m3]
        ! NL      R*4 1 LC amount moles in 1m3 water                   [mole/m3]
        ! P       R*4 1 LC atmospheric pressure                             [Pa]
        ! R       R*4 1 LC universal gas constant                  [Pa.m3/mol/K]
        ! TREF    R*4 1 I  Reference temperature for H0                  [gradC]
        ! TEMP    R*4 1 I  Temperature                                   [gradC]
        !-----------------------------------------------------------------------

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        use m_logger_helper
        use m_evaluate_waq_attribute
        USE PHYSICALCONSTS, ONLY : CtoKelvin
        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: PMSA  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), NOSEG, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), NOQ1, NOQ2, NOQ3, NOQ4
        !
        !     Local declarations, constants in source
        !
        PARAMETER (E = 2.718, &
                KELVIN = real(CtoKelvin), &
                NL = 55510., &
                P = 1.01E+5, &
                R = 8.314)
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
        IP10 = IPOINT(10)
        IP11 = IPOINT(11)
        !
        IFLUX = 0
        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==1)) THEN
                    !
                    !
                    !     Map PMSA on local variables
                    !
                    CONC = MAX (0.0, PMSA(IP1))
                    ATMC = PMSA(IP2)
                    KL = PMSA(IP3)
                    KG = PMSA(IP4)
                    H0TREF = PMSA(IP5)
                    TREF = PMSA(IP6)
                    C1 = PMSA(IP7)
                    TEMP = PMSA(IP8)
                    DEPTH = PMSA(IP9)
                    !
                    !
                    !     Error messages
                    IF (H0TREF < 1E-30)  CALL write_error_message ('H0TREF in VERVLU =<0')
                    IF (TEMP <= -KELVIN) CALL &
                            write_error_message ('TEMP in VERVLU < 0 DEG KELVIN')
                    IF (KL < 1E-30) CALL write_error_message ('KL in VERVLU zero')
                    IF (KG < 1E-30) CALL write_error_message ('KG in VERVLU zero')
                    !
                    !     Calculation of temperarure dependence of Henry
                    H2TREF = H0TREF * NL / P
                    !
                    C2 = (KELVIN + TREF) * (LOG(H2TREF) - C1)
                    !
                    NG = P / (R * (KELVIN + TEMP))
                    !
                    H1TEMP = NG / NL * E**(C2 / (KELVIN + TEMP) + C1)
                    !
                    !     Calculation of volatilization rate constant
                    !
                    KV = 1. / (1. / KL + 1. / (H1TEMP * KG))
                    !
                    !     Calculation of volatilization flux
                    !
                    FL (1 + IFLUX) = (CONC - ATMC / H1TEMP) * KV / DEPTH
                    !
                    !     Output
                    PMSA(IP10) = KV
                    PMSA(IP11) = H1TEMP
                    !
                ENDIF
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
            IP10 = IP10 + INCREM (10)
            IP11 = IP11 + INCREM (11)
            !
        end do
        !
        !
        RETURN
    END

end module m_vervlu
