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
module m_ebuch4
    use m_waq_precision

    implicit none

contains


    subroutine ebuch4 (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper

        !>\file
        !>       Ebullition of methane (new, generic!)

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Methane ebullition on the basis of the assumption that
        !        all methane in excess of saturation excapes the water system
        !        instantaneously. The building up of a gas bubbles stock in
        !        in the sediment is ignored. Process is valid for overlying
        !        water as well as sediment.
        !
        !        ----- description of parameters -----
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! CCH4    R*4 1 I concentration of dissolved methane               [gC/m3]
        ! CCH4S   R*4 1 O saturation concentration of dissolved methane    [gC/m3]
        ! DCH4    R*4 1 L diff. between actual and saturation methane conc.[gC/m3]
        ! DELT    R*4 1 I computational time-step                              [d]
        ! DEPTH   R*4 1 I total depth of the overlying water column            [m]
        ! FSCALE  R*4 1 I scaling factor methane ebullition                    [-]
        ! POROS   R*4 1 I porosity                                             [-]
        ! TEMP    R*4 1 I ambient temperature                                 [oC]
        ! TEMP20  R*4 1 L stand. temperature (20) minus ambient temperature   [oC]
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
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7
        INTEGER(kind = int_wp) :: IFLUX, ISEG, ILUMON
        !
        REAL(kind = real_wp) :: CCH4, CCH4S, DCH4, FSCALE
        REAL(kind = real_wp) :: POROS, TEMP, TEMP20, DEPTH, DELT
        !
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
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        !
        !     -----Warnings-----
        !
        IF (FIRST) THEN
            IF (process_space_real(IP4) <= 0.0) THEN
                WRITE (ILUMON, *) 'WARNING : Poros', &
                        ' should be greater than zero'
            ENDIF
            FIRST = .FALSE.
        ENDIF
        !
        IFLUX = 0
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
                CCH4 = MAX (0.0, process_space_real(IP1))
                FSCALE = process_space_real(IP2)
                TEMP = process_space_real(IP3)
                POROS = process_space_real(IP4)
                DEPTH = process_space_real(IP5)
                DELT = process_space_real(IP6)
                !
                !           Calculate the saturation concentration
                !
                TEMP20 = 20 - TEMP
                CCH4S = 18.76 * (1 + DEPTH / 10.0) * (1.024**TEMP20)
                DCH4 = CCH4 / POROS - CCH4S
                !
                !           Calculate the ebullition flux
                !
                IF (DCH4 < 0.0) THEN
                    FL(1 + IFLUX) = 0.0
                ELSE
                    FL(1 + IFLUX) = (FSCALE * DCH4) / DELT
                ENDIF
                !
                !           The saturation concentration is output
                !
                process_space_real(IP7) = CCH4S
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
            !
        end do
        !
        RETURN
        !
    END

end module m_ebuch4
