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
module m_vivian
    use m_waq_precision

    implicit none

contains


    subroutine vivian (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !>\file
        !>       Dissolution/precipitation of P in vivianite

        !
        !     Description of the module :
        !     precipitation and dissolution of vivianite
        !
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----

        ! CPHD    R*4 1 I     concentration dissolved phosphate            [gP/m3]
        ! CPHDE   R*4 1 I     saturation concentration dissolved phosphate [gP/m3]
        ! CPHPR   R*4 1 I     concentration vivianite                      [gP/m3]
        ! CROXY   R*4 1 I     critical concentration dissolved oxygen     [gO2/m3]
        ! DELT    R*4 1 I     timestep                                         [d]
        ! FPRC    R*4 1 O     precipitation flux                         [gP/m3/d]
        ! FRP     R*4 1 -     switch concerning redox pot. for precipitation   [-]
        ! FRS     R*4 1 -     switch concerning redox pot. for dissolution     [-]
        ! FSOL    R*4 1 O     dissolution flux                           [gP/m3/d]
        ! KPRC    R*4 1 I     precipitation rate                             [1/d]
        ! KSOL    R*4 1 I     dissolution rate                          [m3/gO2/d]
        ! OXY     R*4 1 I     concentration dissolved oxygen              [gO2/m3]
        ! POROS   R*4 1 I     porosity                                         [-]
        ! TCPRC   R*4 1 I     temperature coefficient of precipitation         [-]
        ! TCSOL   R*4 1 I     temperature coefficient of dissolution           [-]
        ! TEMP    R*4 1 I     temperature                                     [oC]
        ! TMPPRC  R*4 1 -     temperature function for precipitation           [-]
        ! TMPSOL  R*4 1 -     temperature function for dissolution             [-]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        INTEGER(kind = int_wp) :: NOSEG, NOFLUX, NOQ1, NOQ2, NOQ3, NOQ4
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), &
                IEXPNT(4, *), IKNMRK(*)
        REAL(kind = real_wp) :: PMSA(*), FL(*)

        REAL(kind = real_wp) :: KSOL, KPRC, FSOL, FPRC, &
                TEMP, TMPSOL, TMPPRC, TCSOL, TCPRC, &
                CPHD, CPHPR, CPHDE, POROS, OXY, CROXY, &
                DELT

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
        IP12 = IPOINT(12)
        !
        IFLUX = 0

        DO ISEG = 1, NOSEG

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CPHD = MAX(PMSA(IP1), 0.0)
                CPHPR = MAX(PMSA(IP2), 0.0)
                CPHDE = PMSA(IP3)
                KPRC = PMSA(IP4)
                TCPRC = PMSA(IP5)
                KSOL = PMSA(IP6)
                TCSOL = PMSA(IP7)
                OXY = PMSA(IP8)
                CROXY = PMSA(IP9)
                TEMP = PMSA(IP10)
                POROS = PMSA(IP11)
                DELT = PMSA(IP12)
                !
                !     Calculation of the precipitation or dissolution flux
                !     dependent on dissolved oxygen
                !
                IF (OXY >= CROXY) THEN
                    FPRC = 0.0
                    TMPSOL = TCSOL**(TEMP - 20.0)
                    FSOL = KSOL * TMPSOL * CPHPR * OXY / POROS
                    IF (FSOL < 0.0) FSOL = 0.0
                    IF (FSOL * DELT >= CPHPR) FSOL = 0.5 * CPHPR / DELT
                ELSE
                    FSOL = 0.0
                    TMPPRC = TCPRC**(TEMP - 20.0)
                    FPRC = KPRC * TMPPRC * (CPHD / POROS - CPHDE) * POROS
                    IF (FPRC < 0.0) FPRC = 0.0
                ENDIF
                !
                !     Output of module
                !
                FL(1 + IFLUX) = FPRC
                FL(2 + IFLUX) = FSOL
                !
                !     End active cells block
                !
            ENDIF

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
            IP12 = IP12 + INCREM (12)
            !
        end do
        !
        RETURN
        !
    END

end module m_vivian
