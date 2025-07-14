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
module m_sulfox
    use m_waq_precision

    implicit none

contains


    subroutine sulfox (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper

        !>\file
        !>       Oxidation of dissolved sulphide (0 and 2nd order) (new, generic !)

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Sulphide oxidation kinetics composed of zeroth order and
        !        second order process for dissolved sulphide and oxygen.
        !        Process is valid for overlying water as well as sediment.
        !
        !        ----- description of parameters -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! COX     R*4 1 I concentration of dissolved oxygen                 [g/m3]
        ! COXC    R*4 1 I critical oxygen conc. for sulphide oxidation      [g/m3]
        ! CSUD    R*4 1 I concentration of total dissolved sulphide        [gS/m3]
        ! DELT    R*4 1 I timestep                                             [d]
        ! FL (1)  R*4 1 O sulphide oxidation flux                        [gS/m3/d]
        ! FLUXOX  R*4 1 - sulphide oxidation flux                        [gS/m3/d]
        ! K0OXI   R*4 1 I zeroth order sulphide oxidation rate for DO    [gS/m3/d]
        ! KOXI    R*4 1 I second order sulphide oxidation rate            [m3.d/g]
        ! KTOXI   R*4 1 I temperature coefficient for oxidation                [-]
        ! POROS   R*4 1 I porosity                                             [-]
        ! TEMP    R*4 1 I ambient temperature                                 [oC]
        ! TEMPC   R*4 1 L ambient temperature correction function              [-]
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
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9
        INTEGER(kind = int_wp) :: IFLUX, ISEG, ILUMON
        !
        REAL(kind = real_wp) :: CSUD, COX
        REAL(kind = real_wp) :: K0OXI, KOXI, COXC
        REAL(kind = real_wp) :: POROS, KTOXI, TEMP, TEMPC, TEMP20
        REAL(kind = real_wp) :: DELT, FLUXOX
        LOGICAL  FIRST
        SAVE     FIRST
        DATA     FIRST /.TRUE./
        !
        CALL get_log_unit_number(ILUMON)
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
        !     -----Warnings-----
        !
        IF (FIRST) THEN
            IF (process_space_real(IP8) <= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : Poros should be', &
                        ' greater than zero'
            ENDIF
            FIRST = .FALSE.
        ENDIF
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                CSUD = MAX (0.0, process_space_real(IP1))
                COX = MAX (0.0, process_space_real(IP2))
                K0OXI = process_space_real(IP3)
                KOXI = process_space_real(IP4)
                KTOXI = process_space_real(IP5)
                COXC = process_space_real(IP6)
                TEMP = process_space_real(IP7)
                POROS = process_space_real(IP8)
                DELT = process_space_real(IP9)
                !
                !           Set the rates according to the DO concentration
                !
                IF (COX <= 0.0) THEN
                    KOXI = 0.0
                ENDIF
                IF (COX > (COXC * POROS)) THEN
                    K0OXI = 0.0
                ENDIF
                !
                !           Calculate the sulphide oxidation flux
                !
                TEMP20 = TEMP - 20.0
                TEMPC = KTOXI ** TEMP20
                !
                FLUXOX = K0OXI + KOXI * TEMPC * CSUD * COX / POROS
                FLUXOX = MIN(FLUXOX, 0.9 * CSUD / DELT)
                FLUXOX = MIN(FLUXOX, 0.5 * COX / 2.0 / DELT)
                !
                FL(1 + IFLUX) = FLUXOX
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
            !
        end do
        !
        RETURN
        !
    END

end module m_sulfox
