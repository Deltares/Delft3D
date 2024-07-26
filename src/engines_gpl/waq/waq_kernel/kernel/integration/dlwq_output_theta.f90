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
module m_dlwq_output_theta
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine dlwq_output_theta (num_output_variables_extra, ounam, ipoint, num_constants, num_spatial_parameters, &
            num_time_functions, num_spatial_time_fuctions, num_substances_total, num_cells, num_local_vars, &
            proloc, num_defaults, theta)

        !     Deltares - Delft Software Department

        !     Created   :          2007 by Pauline van Slingerland

        !     Function  : puts the theta array in the process local array for output

        !     Modified  :

        use timers
        implicit none

        !     Arguments           :

        !     Kind           Function         Name                  Description

        integer(kind = int_wp), intent(in) :: num_output_variables_extra              ! number of output parameters
        character(20), intent(in) :: ounam(num_output_variables_extra)       ! output parameter names
        integer(kind = int_wp), intent(in) :: ipoint(num_output_variables_extra)      ! output parameter pointers
        integer(kind = int_wp), intent(in) :: num_constants              ! number of constants
        integer(kind = int_wp), intent(in) :: num_spatial_parameters                ! number of parameters
        integer(kind = int_wp), intent(in) :: num_time_functions               ! number of functions
        integer(kind = int_wp), intent(in) :: num_spatial_time_fuctions              ! number of segment functions
        integer(kind = int_wp), intent(in) :: num_substances_total               ! number of substances
        integer(kind = int_wp), intent(in) :: num_cells               ! number of default values
        integer(kind = int_wp), intent(in) :: num_local_vars               ! number of default values
        real(kind = real_wp), intent(out) :: proloc(num_local_vars, num_cells) ! process local array
        integer(kind = int_wp), intent(in) :: num_defaults               ! number of default values
        real(kind = real_wp), intent(in) :: theta(num_cells)        ! theta array

        ! local declarations

        character(20) :: parnam                   ! output parameter name
        logical, save :: first = .true.           ! initialisation flag
        integer(kind = int_wp) :: parindx                  ! index in output parameter name array
        integer(kind = int_wp), save :: ip_theta                 ! index of theta in process local array
        integer(kind = int_wp), parameter :: nopred = 6               ! number of predefined outputs
        integer(kind = int_wp) :: iocons                   ! offset to the constants
        integer(kind = int_wp) :: iopa                     ! offset to the parameters
        integer(kind = int_wp) :: iofunc                   ! offset to the functions
        integer(kind = int_wp) :: iosfun                   ! offset to the segment functions
        integer(kind = int_wp) :: ioconc                   ! offset to the concentrations
        integer(kind = int_wp) :: ioloc                    ! offset to the process local array
        integer(kind = int_wp) :: iodef                    ! offset to the process default array
        integer(kind = int_wp) :: iseg                     ! segment loop counter

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwq_output_theta", ithandl)

        !        initialise

        if (first) then

            first = .false.

            !        pointer offsets

            iocons = nopred + 1
            iopa = iocons + num_constants
            iofunc = iopa + num_spatial_parameters
            iosfun = iofunc + num_time_functions
            ioconc = iosfun + num_spatial_time_fuctions
            ioloc = ioconc + num_substances_total
            iodef = ioloc + num_local_vars

            !        look for parameter theta in output

            parnam = 'theta'
            parindx = index_in_array(parnam, ounam)
            if (parindx > 0) then
                ip_theta = ipoint(parindx)
                if (ip_theta >= ioloc .and. ip_theta < iodef) then
                    ip_theta = ip_theta - ioloc + 1
                else
                    ip_theta = -1
                endif
            endif

        endif

        !     fill output array

        if (ip_theta > 0) then
            do iseg = 1, num_cells
                proloc(ip_theta, iseg) = theta(iseg)
            enddo
        endif

        if (timon) call timstop (ithandl)
        return
    end

end module m_dlwq_output_theta
