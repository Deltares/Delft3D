!! Copyright (C)  Stichting Deltares, 2011-2024.
!!
!! This program is free software: you can redistribute it and/or modify
!! it under the terms of the GNU General Public License as published by
!! the Free Software Foundation version 3.
!!
!! This program is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU General Public License for more details.
!!
!! You should have received a copy of the GNU General Public License
!! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!!
!! contact: delft3d.support@deltares.nl
!! Stichting Deltares
!! P.O. Box 177
!! 2600 MH Delft, The Netherlands
!!
!! All indications and logos of, and references to, "Delft3D" and "Deltares"
!! are registered trademarks of Stichting Deltares, and remain the property of
!! Stichting Deltares. All rights reserved.
module m_salchl
    use m_waq_precision
    use chemical_utils, only: salinity_from_chloride
    use m_logger_helper, only : stop_with_error, get_log_unit_number

    implicit none
    private
    public :: salchl

contains

    subroutine salchl (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Converts chloride into salinity (Aquatic Chemistry 2nd ed 1981 p567)
        !
        real(kind = real_wp) :: process_space_real  (*), fl    (*)
        integer(kind = int_wp) :: ipoint(*), increm(*), num_cells, noflux, &
                iexpnt(4, *), iknmrk(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        real(kind = real_wp) :: cl         ! chloride concentration                         [g/m3]
        real(kind = real_wp) :: sal        ! salinity                                       [g/kg]
        real(kind = real_wp) :: sal0       ! salinity at zero chloride                      [g/kg]
        real(kind = real_wp) :: gtcl       ! ratio of salinity and chloride                 [g/g]
        real(kind = real_wp) :: temp       ! ambient temperature                            [oC]
        real(kind = real_wp) :: dens       ! density of water with dissolved salt           [kg/m3]
        real(kind = real_wp) :: swsalcl    ! option: 0 SAL simulated, 1 CL simulated
        integer(kind = int_wp) :: iseg, ip1, ip2, ip3, ip4, ip5, iflux
        integer(kind = int_wp) :: lunrep

        ip1 = ipoint(1)
        ip2 = ipoint(2)
        ip3 = ipoint(3)
        ip4 = ipoint(4)
        ip5 = ipoint(5)

        iflux = 0
        do iseg = 1, num_cells

            if (btest(iknmrk(iseg), 0)) then
                !
                cl = process_space_real(ip1)
                temp = process_space_real(ip2)
                swsalcl = process_space_real(ip3)
                !
                !***********************************************************************
                !**** Processes connected to the normalization RIZA method
                !***********************************************************************
                !
                !     factor 0.7 in density correction was derived empirically from RIZA Standard methods
                !     table 210 on p 109 is repoduced within 0.15%
                !     basic relation sal-chlorinity: sal = 0.03 +1.805*chlor/density
                !     density = f(temp and salt concentration)
                !
                !     Note: gtcl and sal0 are fixed to 1.805 and 0.03 respectively
                !           Chlorinity expressed in g/m3, temperature in degrees C
                !
                !     Note: the switch is maintained to make sure that incorrect use is caught
                !           Of old the routine allowed the reverse conversion via this switch.
                !
                if (nint(swsalcl) == 1) then
                    call salinity_from_chloride( cl, temp, sal, dens )
                else
                    call get_log_unit_number(lunrep)
                    write(lunrep, *) 'Error in SALCHL'
                    write(lunrep, *) 'Obsolete option for conversion - only the value 1 is allowed'
                    write(lunrep, *) 'Option in input:', swsalcl
                    write(*, *) 'Error in SALCHL'
                    write(*, *) 'Obsolete option for conversion - only the value 1 is allowed'
                    write(*, *) 'Option in input:', swsalcl
                    call stop_with_error()
                endif

                process_space_real (ip4) = dens
                process_space_real (ip5) = sal

            endif

            iflux = iflux + noflux
            ip1 = ip1 + increm (1)
            ip2 = ip2 + increm (2)
            ip3 = ip3 + increm (3)
            ip4 = ip4 + increm (4)
            ip5 = ip5 + increm (5)

        end do

        return
    end subroutine salchl

end module m_salchl
