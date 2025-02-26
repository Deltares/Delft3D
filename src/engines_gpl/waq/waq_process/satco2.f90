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
module m_satco2
    use m_waq_precision
    use chemical_utils, only: chlorinity_from_sal

    implicit none
    private
    public :: satco2

contains


    subroutine satco2 (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Saturation concentration carbon dioxide

        use m_logger_helper, only : stop_with_error, get_log_unit_number
        use physicalconsts, only : ctokelvin
        implicit none

        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(*), increm(*), num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     local declarations
        !
        integer(kind = int_wp) :: lunrep, iseg, ip1, ip2, ip3, ip4, ip5, ip6
        real(kind = real_wp)   :: co2sat            ! Saturation concentration                          [g/m3]
        real(kind = real_wp)   :: papco2            ! Partial CO2 pressure                              [g/m3]
        real(kind = real_wp)   :: sal               ! Salinity                                           [ppt]
        integer(kind = int_wp) :: switch            ! Switch for formulation options                       [-]
        real(kind = real_wp)   :: temp              ! ambient temperature                                 [xC]

        real(kind = real_wp) :: cl2, tempa, rion, rkco2, temp2, part1, part2, &
                a1, a2, a3, b1, b2, b3
        parameter (a1 = -58.0931, &
                a2 = 90.5069, &
                a3 = 22.2940, &
                b1 = 0.027766, &
                b2 = -0.025888, &
                b3 = 0.0050578)

        ip1 = ipoint(1)
        ip2 = ipoint(2)
        ip3 = ipoint(3)
        ip4 = ipoint(4)
        ip5 = ipoint(5)

        do iseg = 1, num_cells

            temp = process_space_real(ip1)
            switch = nint(process_space_real(ip2))
            sal = process_space_real(ip3)
            papco2 = process_space_real(ip4)

            if (switch == 1) then

                ! === reaeration co2 ==================================================
                !
                !     saturation concentration co2 = partial pressure co2 in atmosphere
                !     (assumed mol/l)              * reaction constant
                !
                !     partial pressure = 10**-3.5 atm (pag. 180)
                !
                !     reaction constant kco2 = function (abs.temperature,chlorinity)
                !     abs. temp  = 273.15 + tempd (model temp. in degrees celsius)
                !     chlorinity = 0.001*cl (cl is model conc. cl- in mg/l)
                !
                !     ref.: aquatic chemistry,  stumm & morgan, wiley & sons, 1981
                !
                ! =====================================================================

                cl2 = chlorinity_from_sal( sal, temp ) / 1000.
                tempa = temp + real(ctokelvin)
                rion = 0.147e-02 + 0.3592e-01 * cl2 + 0.68e-04 * cl2**2
                rkco2 = 10.0**(-(- 0.238573e+04 / tempa + 0.140184e+02 - &
                        0.152642e-01 * tempa + rion * (0.28569 - 0.6167e-05 * tempa)))

            elseif (switch == 2) then
                !
                !        weiss volgen monteiro (cisr)
                !
                temp2 = (temp + 273.) / 100.
                part1 = a1 + a2 / temp2 + a3 * log(temp2)
                part2 = sal * (b1 + b2 * temp2 + b3 * temp2 * temp2)
                rkco2 = exp(part1 + part2)
                !
            else
                call get_log_unit_number(lunrep)
                write(lunrep, *) 'Error in SATCO2'
                write(lunrep, *) 'Invalid option for CO2 saturation formula'
                write(lunrep, *) 'Option in input:', switch
                write(*, *) ' Error in satco2'
                write(*, *) ' Invalid option for CO2 saturation formula'
                write(*, *) ' Option in input:', switch
                call stop_with_error()
            endif

            !     output of calculated saturation

            co2sat = papco2 * rkco2 * 1000. * 44.
            process_space_real (ip5) = co2sat

            ip1 = ip1 + increm (1)
            ip2 = ip2 + increm (2)
            ip3 = ip3 + increm (3)
            ip4 = ip4 + increm (4)
            ip5 = ip5 + increm (5)

        end do

        return

    end subroutine satco2

end module m_satco2
