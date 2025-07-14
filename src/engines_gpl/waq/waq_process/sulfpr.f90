!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_sulfpr
    use m_waq_precision

    implicit none

contains


    subroutine sulfpr (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Precipitation and dissolution of sulphide as first order process

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Sulphide precipitation and dissolution kinetics as first order
        !        process with respect to the difference of actual and
        !        equilibrium free dissolved sulphide concentrations.
        !        Process is valid for overlying water as well as sediment.
        !
        !        ----- description of parameters -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! CSP     R*4 1 I concentration of precipitated sulphide            [g/m3]
        ! CSD     R*4 1 I actual concentration of free dissolved sulphide [mole/l]
        ! CSDE    R*4 1 I equilibrium conc. of free dissolved sulphide    [mole/l]
        ! DELT    R*4 1 I timestep                                             [d]
        ! FL (1)  R*4 1 O sulphide precipitation flux                    [gS/m3/d]
        ! FL (2)  R*4 1 O sulphide dissolution flux                      [gS/m3/d]
        ! FLUXPR  R*4 1 - sulphide precipitation flux                    [gS/m3/d]
        ! FLUXDS  R*4 1 - sulphide dissolution flux                      [gS/m3/d]
        ! KDIS    R*4 1 I first order sulphide dissolution rate              [1/d]
        ! KPRC    R*4 1 I first order sulphide precipitation rate            [1/d]
        ! KTDIS   R*4 1 I temperature coefficient for dissolution              [-]
        ! KTPRC   R*4 1 I temperature coefficient for precipitation            [-]
        ! POROS   R*4 1 I porosity                                             [-]
        ! TEMP    R*4 1 I ambient temperature                                 [oC]
        ! TEMPCD  R*4 1 L ambient temp. correction function for dissolution    [-]
        ! TEMPCP  R*4 1 L ambient temp. correction function for precipitation  [-]
        ! TEMP20  R*4 1 L ambient temperature - stand. temp (20)              [oC]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT NONE
        !
        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10
        INTEGER(kind = int_wp) :: IFLUX, ISEG
        !
        REAL(kind = real_wp) :: CSP, CSD
        REAL(kind = real_wp) :: KDIS, KPRC, CSDE, KTDIS, KTPRC
        REAL(kind = real_wp) :: POROS, TEMP, TEMPCD, TEMPCP, TEMP20
        REAL(kind = real_wp) :: DELT, FLUXPR, FLUXDS
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)
        IN8 = INCREM(8)
        IN9 = INCREM(9)
        IN10 = INCREM(10)
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
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                CSP = MAX (process_space_real(IP1), 0.0)
                CSD = MAX (process_space_real(IP2), 0.0)
                CSDE = MAX (process_space_real(IP3), 0.0)
                KDIS = process_space_real(IP4)
                KTDIS = process_space_real(IP5)
                KPRC = process_space_real(IP6)
                KTPRC = process_space_real(IP7)
                TEMP = process_space_real(IP8)
                POROS = process_space_real(IP9)
                DELT = process_space_real(IP10)
                !
                !           Calculate the precipitation and dissolution fluxes
                !           The constant 32000 concerns conversion mole/l to gS/m3
                !
                TEMP20 = TEMP - 20.0
                TEMPCP = KTPRC ** TEMP20
                TEMPCD = KTDIS ** TEMP20
                !
                FLUXPR = 32000.0 * KPRC * TEMPCP * (CSD - CSDE) * POROS
                FLUXDS = 32000.0 * KDIS * TEMPCD * (CSDE - CSD) * POROS
                !
                !           Correct fluxes depending on under- or supersaturation
                !
                IF (FLUXPR < 0.0) FLUXPR = 0.0
                IF (FLUXDS < 0.0) FLUXDS = 0.0
                IF (FLUXDS * DELT >= CSP) FLUXDS = 0.5 * CSP / DELT
                !
                FL(1 + IFLUX) = FLUXPR
                FL(2 + IFLUX) = FLUXDS
                !
            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
            !
        end do
        !
        RETURN
        !
    END

end module m_sulfpr
