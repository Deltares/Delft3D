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
    use chemical_utils, only: chlorinity_from_sal

    implicit none
    private
    public :: satoxy

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
        ! OXSAT   R*4 1 O saturation concentration of dissolved oxygen      [g/m3]
        ! SAL     R*4 1 I Salinity                                           [ppt]
        ! SWITCH  I*4 1 I Switch for formulation options                       [-]
        ! TEMP    R*4 1 I ambient temperature                                 [xC]


        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        implicit none

        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(*), increm(*), num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     local declarations
        !
        integer(kind = int_wp) :: lunrep, iseg, ip1, ip2, ip3, ip4
        real(kind = real_wp)   :: oxsat             ! saturation concentration of dissolved oxygen      [g/m3]
        real(kind = real_wp)   :: sal               ! Salinity                                           [ppt]
        integer(kind = int_wp) :: switch          ! Switch for formulation options                       [-]
        real(kind = real_wp)   :: temp              ! ambient temperature                                 [xC]

        real(kind = real_wp)   :: cl, temp2, part1, part2, &
                                  a1, a2, a3, a4, b1, b2, b3
        parameter (a1 = -173.4292, &
                a2 = 249.6339, &
                a3 = 143.3483, &
                a4 = -21.8492, &
                b1 = -0.033096, &
                b2 = 0.014259, &
                b3 = -0.0017)

        ip1 = ipoint(1)
        ip2 = ipoint(2)
        ip3 = ipoint(3)
        ip4 = ipoint(4)
        !
        !     initial calculations
        !
        do iseg = 1, num_cells

            temp = process_space_real(ip1)
            switch = nint(process_space_real(ip2))
            sal = process_space_real(ip3)

            if (switch == 1) then
                !
                !        Weiss according to van Gils (wl)
                !
                cl    = chlorinity_from_sal( sal, temp )
                oxsat = (14.652 &
                        - (0.41022 * temp) &
                        + (0.089392 * temp)**2 &
                        - (0.042685 * temp)**3) &
                        * (1. - cl / 1e+5)

            elseif (switch == 2) then
                !
                !        weiss according to Monteiro (cisr)
                !        1.428571 = 32.*1000./22400.
                !
                temp2 = (temp + 273.) / 100.
                part1 = a1 + a2 / temp2 + a3 * log(temp2) + a4 * temp2
                part2 = sal * (b1 + b2 * temp2 + b3 * temp2 * temp2)
                oxsat = exp(part1 + part2) * 1.428571
                !
            else
                call get_log_unit_number(lunrep)
                write(lunrep, *) 'Error in SATOXY'
                write(lunrep, *) 'Invalid option for oxygen saturation formula'
                write(lunrep, *) 'Option in input:', switch
                write(*, *) ' Error in SATOXY'
                write(*, *) ' Invalid option for oxygen saturation formula'
                write(*, *) ' option in input:', switch
                call stop_with_error()
            endif

            !     output of calculated oxygen saturation

            process_space_real (ip4) = oxsat

            ip1 = ip1 + increm (1)
            ip2 = ip2 + increm (2)
            ip3 = ip3 + increm (3)
            ip4 = ip4 + increm (4)

        end do

    end subroutine satoxy

end module m_satoxy
