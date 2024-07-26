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
module m_satoxy
    use m_waq_precision

    implicit none

contains


    subroutine satoxy (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : stop_with_error, get_log_unit_number

        !>\file
        !>       Saturation concentration of oxygen

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        COMPUTATION OF OXYGEN SATURATION CONCENTRATION
        !
        ! Name    T   L I/O   Description                                   Units
        ! ----    --- -  -    -------------------                            ----
        ! CL      R*4 1 I concentration of chloride                         [g/m3]
        ! OXSAT   R*4 1 O saturation concentration of dissolved oxygen      [g/m3]
        ! SAL     R*4 1 I Salinity                                           [ppt]
        ! SWITCH  I*4 1 I Switch for formulation options                       [-]
        ! TEMP    R*4 1 I ambient temperature                                 [xC]


        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT NONE

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     Local declarations
        !
        INTEGER(kind = int_wp) :: SWITCH, LUNREP, ISEG, IP1, &
                IP2, IP3, IP4, IP5
        REAL(kind = real_wp) :: CL, TEMP, SAL, TEMP2, PART1, &
                PART2, OXSAT, A1, A2, A3, &
                A4, B1, B2, B3
        PARAMETER (A1 = -173.4292, &
                A2 = 249.6339, &
                A3 = 143.3483, &
                A4 = -21.8492, &
                B1 = -0.033096, &
                B2 = 0.014259, &
                B3 = -0.0017)
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        !
        !     Initial calculations
        !
        DO ISEG = 1, num_cells

            CL = process_space_real(IP1)
            TEMP = process_space_real(IP2)
            SWITCH = NINT(process_space_real(IP3))
            SAL = process_space_real(IP4)

            IF (SWITCH == 1) THEN
                !
                !        Weiss volgens Gils (WL)
                !
                OXSAT = (14.652 &
                        - (0.41022 * TEMP) &
                        + (0.089392 * TEMP)**2 &
                        - (0.042685 * TEMP)**3) &
                        * (1. - CL / 1E+5)
            ELSEIF (SWITCH == 2) THEN
                !
                !        Weiss volgen Monteiro (CISR)
                !        1.428571 = 32.*1000./22400.
                !
                TEMP2 = (TEMP + 273.) / 100.
                PART1 = A1 + A2 / TEMP2 + A3 * LOG(TEMP2) + A4 * TEMP2
                PART2 = SAL * (B1 + B2 * TEMP2 + B3 * TEMP2 * TEMP2)
                OXSAT = EXP(PART1 + PART2) * 1.428571
                !
            ELSE
                CALL get_log_unit_number(LUNREP)
                WRITE(LUNREP, *) 'ERROR in SATOXY'
                WRITE(LUNREP, *) 'Illegal option for oxygen saturation formula'
                WRITE(LUNREP, *) 'Option in input:', SWITCH
                WRITE(*, *) ' ERROR in SATOXY'
                WRITE(*, *) ' Illegal option for oxygen saturation formula'
                WRITE(*, *) ' Option in input:', SWITCH
                CALL stop_with_error()
            ENDIF

            !     Output of calculated oxygen saturation

            process_space_real (IP5) = OXSAT
            !
            !jvb  ENDIF
            !
            IP1 = IP1 + INCREM (1)
            IP2 = IP2 + INCREM (2)
            IP3 = IP3 + INCREM (3)
            IP4 = IP4 + INCREM (4)
            IP5 = IP5 + INCREM (5)
            !
        end do
        !
        RETURN
        !
    END

end module m_satoxy
