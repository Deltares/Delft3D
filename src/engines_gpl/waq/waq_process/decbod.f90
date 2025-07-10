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
module m_decbod
    use m_waq_precision

    implicit none

contains


    subroutine decbod (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : stop_with_error, get_log_unit_number

        !>\file
        !>       Oxydation of BOD-fractions with Monod kinetics for the TEWOR models

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        Oxydation of three fractions of BOD (background, sewage overflow
        !        slow and fast settling) via MONOD-kinetics
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! BOD5_1  R*4 1 I carbonaceous BOD (first pool) at 5 days          [gO/m3]
        ! BOD5_2  R*4 1 I carbonaceous BOD (second pool) at 5 days         [gO/m3]
        ! BOD5_3  R*4 1 I carbonaceous BOD (third pool) at 5 days          [gO/m3]
        ! BOD5    R*4 1 O total BOD at 5 days                              [gO/m3]
        ! BODU_1  R*4 1 L carbonaceous BOD (first pool) ultimate           [gO/m3]
        ! BODU_2  R*4 1 L carbonaceous BOD (second pool) ultimate          [gO/m3]
        ! BODU_3  R*4 1 L carbonaceous BOD (third pool) ultimate           [gO/m3]
        ! BODU    R*4 1 L total BOD ultimate                               [gO/m3]
        ! BOD5_1  R*4 1 I oxydation flux of BOD (first pool) at 5 days   [gO/m3,d]
        ! BOD5_2  R*4 1 I oxydation flux of BOD (second pool) at 5 days  [gO/m3,d]
        ! BOD5_3  R*4 1 I oxydation flux of BOD (third pool) at 5 days   [gO/m3,d]
        ! BODU_1  R*4 1 L oxydation flux of BOD (first pool) ultimate    [gO/m3,d]
        ! BODU_2  R*4 1 L oxydation flux of BOD (second pool) ultimate   [gO/m3,d]
        ! BODU_3  R*4 1 L oxydation flux of BOD (third pool) ultimate    [gO/m3,d]
        ! OXYDEM  R*4 1 I total oxygen demand oxydation of BOD           [gO/m3,d]
        ! RCBOD1  R*4 1 I oxydation reaction rate BOD (first pool)           [1/d]
        ! RCBOD2  R*4 1 I oxydation reaction rate BOD (second pool)          [1/d]
        ! RCBOD3  R*4 1 I oxydation reaction rate BOD (third pool)           [1/d]
        ! KMOX    R*4 1 I half sat const for oxygen limit. of BOD oxydation[gO/m3]
        ! OXFUNC  R*4 1 O limitation function of OXY on BOD oxydation          [-]
        ! OXY     R*4 1 I concentration of dissolved oxygen                 [g/m3]
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------
        !
        IMPLICIT NONE
        !
        REAL(kind = real_wp) :: process_space_real  (*), FL  (*)
        INTEGER(kind = int_wp) :: num_cells, NOFLUX, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), &
                IEXPNT(4, *), IKNMRK(*)
        !
        INTEGER(kind = int_wp) :: LUNREP
        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8, IP9, IP10, &
                IP11, IP12, IP13, IP14
        INTEGER(kind = int_wp) :: IN1, IN2, IN3, IN4, IN5, IN6, IN7, IN8, IN9, IN10, &
                IN11, IN12, IN13, IN14
        INTEGER(kind = int_wp) :: IFLUX, ISEG
        REAL(kind = real_wp) :: OXFUNC, OXY, BOD5_1, BOD5_2, BOD5_3, BODU_1, BODU_2, &
                BODU_3, RCBOD1, RCBOD2, RCBOD3, BOD5, BODU, KMOX, &
                dBOD51, dBOD52, dBOD53, dBODU1, dBODU2, dBODU3, OXYDEM
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
        IN11 = INCREM(11)
        IN12 = INCREM(12)
        IN13 = INCREM(13)
        IN14 = INCREM(14)
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
        IP12 = IPOINT(12)
        IP13 = IPOINT(13)
        IP14 = IPOINT(14)
        !
        IFLUX = 0
        DO ISEG = 1, num_cells
            !
            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                BOD5_1 = MAX (0.0, process_space_real(IP1))
                BOD5_2 = MAX (0.0, process_space_real(IP2))
                BOD5_3 = MAX (0.0, process_space_real(IP3))
                RCBOD1 = process_space_real(IP4)
                RCBOD2 = process_space_real(IP5)
                RCBOD3 = process_space_real(IP6)
                KMOX = process_space_real(IP7)
                OXY = process_space_real(IP8)

                !           Check if RC's are non zero

                IF (RCBOD1 < 1E-10) THEN
                    CALL get_log_unit_number(LUNREP)
                    WRITE (LUNREP, *) 'RCBOD: Invalid value (zero)!'
                    WRITE (*, *) 'RCBOD: Invalid value (zero)!'
                    CALL stop_with_error()
                ENDIF
                IF (RCBOD2 < 1E-10) THEN
                    CALL get_log_unit_number(LUNREP)
                    WRITE (LUNREP, *) 'RCBOD_2: Invalid value (zero)!'
                    WRITE (*, *) 'RCBOD_2: Invalid value (zero)!'
                    CALL stop_with_error()
                ENDIF
                IF (RCBOD3 < 1E-10) THEN
                    CALL get_log_unit_number(LUNREP)
                    WRITE (LUNREP, *) 'RCBOD_3: Invalid value (zero)!'
                    WRITE (*, *) 'RCBOD_3: Invalid value (zero)!'
                    CALL stop_with_error()
                ENDIF
                !
                !           Calculation of ultimate BOD concentrations
                !
                BODU_1 = BOD5_1 / (1 - EXP(-5 * RCBOD1))
                BODU_2 = BOD5_2 / (1 - EXP(-5 * RCBOD2))
                BODU_3 = BOD5_3 / (1 - EXP(-5 * RCBOD3))
                !
                !           Calculation of oxygen limitation
                !
                OXFUNC = OXY / (KMOX + OXY)
                !
                !           Calculation of fluxes
                !
                dBOD51 = RCBOD1 * OXFUNC * BOD5_1
                dBOD52 = RCBOD2 * OXFUNC * BOD5_2
                dBOD53 = RCBOD3 * OXFUNC * BOD5_3
                dBODU1 = RCBOD1 * OXFUNC * BODU_1
                dBODU2 = RCBOD2 * OXFUNC * BODU_2
                dBODU3 = RCBOD3 * OXFUNC * BODU_3
                OXYDEM = dBODU1 + dBODU2 + dBODU3
                !
                FL(1 + IFLUX) = dBOD51
                FL(2 + IFLUX) = dBOD52
                FL(3 + IFLUX) = dBOD53
                FL(4 + IFLUX) = OXYDEM
                !
                !           Output of module
                !
                process_space_real(IP9) = BODU_1
                process_space_real(IP10) = BODU_2
                process_space_real(IP11) = BODU_3
                process_space_real(IP12) = OXFUNC
                !
                BOD5 = BOD5_1 + BOD5_2 + BOD5_3
                BODU = BODU_1 + BODU_2 + BODU_3
                !
                process_space_real(IP13) = BOD5
                process_space_real(IP14) = BODU
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
            IP11 = IP11 + IN11
            IP12 = IP12 + IN12
            IP13 = IP13 + IN13
            IP14 = IP14 + IN14
            !
        ENDDO
        !
        RETURN
        !
    END

end module m_decbod
