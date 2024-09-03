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
module m_theta_calculation
    use m_waq_precision
    use timers
    use m_string_utils

    implicit none

    private
    public :: calculate_theta, output_theta

contains

    !> Compute the values for theta
    !! The calculation is limited to horizontal directions.
    !! For the vertical direction it is almost always theta= 1.0 so it is assumed to be that.
    subroutine calculate_theta(idt, num_cells, num_boundary_conditions, volold, num_exchanges, &
            num_exchanges_u_dir, num_exchanges_v_dir, ipoint, flowtot, disptot, &
            theta, thetaseg, antidiffusion, iexseg)

        integer(kind = int_wp), intent(in) :: idt                   !< Time step
        integer(kind = int_wp), intent(in) :: num_cells                 !< Number of segments
        integer(kind = int_wp), intent(in) :: num_boundary_conditions                 !< Number of boundaries
        real(kind = real_wp), intent(in) :: volold(num_cells)         !< Volumes at beginning of step (dim: num_cells)
        integer(kind = int_wp), intent(in) :: num_exchanges                   !< Total number of exchanges
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir                  !< Number of exchanges in the first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir                  !< Number of exchanges in the second direction
        integer(kind = int_wp), intent(in) :: ipoint(4, num_exchanges)        !< Exchange pointers (dim: 4 x num_exchanges)
        real(kind = real_wp), intent(in) :: flowtot(num_exchanges)          !< Total flows accross exchange surfs (dim: num_exchanges)
        real(kind = real_wp), intent(in) :: disptot(num_exchanges) !< Total flows accross exchange surfs (dim: num_exchanges)
        real(kind = real_wp), intent(out) :: theta(num_exchanges)  !< Variable theta coefficients (dim: num_exchanges)
        real(kind = real_wp), intent(out) :: thetaseg(num_cells)       !< Variable theta coefficients per segment
        logical(4), intent(in) :: antidiffusion         !< If true: replace diffusion error by antidiffusion error
        integer(kind = int_wp), intent(out) :: iexseg(num_cells + num_boundary_conditions) !< 0 if volume is explicit

        ! Local variables
        integer(kind = int_wp) :: i    !< From cell
        integer(kind = int_wp) :: j    !< To cell
        integer(kind = int_wp) :: iq   !< Current edge
        integer(kind = int_wp) :: cell_i !< Current cell
        integer(kind = int_wp) :: iexp !< Explicit fraction of the problem

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("calculate_theta", ithandl)

        ! initialisation
        do cell_i = 1, num_cells
            thetaseg(cell_i) = 0.0
        end do
        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
            theta   (iq) = 0.0
        end do
        do iq = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges
            theta   (iq) = 1.0
        end do

        ! store the sum of outflows per volume in thetaseg
        ! horizontal only, vertical will be delt with implicitly
        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i > 0) thetaseg(i) = thetaseg(i) + max(0.0, flowtot(iq)) + disptot(iq)
            if (j > 0) thetaseg(j) = thetaseg(j) + max(0.0, -flowtot(iq)) + disptot(iq)
        end do

        ! store local theta coefficients per volume in thetaseg
        do cell_i = 1, num_cells
            if (thetaseg(cell_i) > 0) thetaseg(cell_i) = max(0.0, 1.0 - volold(cell_i) / (real(idt) * thetaseg(cell_i)))
        enddo

        ! store local theta coefficients per edge in theta
        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i > 0 .and. j > 0) then
                theta(iq) = max(thetaseg(i), thetaseg(j))
            end if
            if (i > 0 .and. j < 0) theta(iq) = thetaseg(i) ! j is a boundary cell
            if (i < 0 .and. j > 0) theta(iq) = thetaseg(j) ! i is a boundary cell
        end do

        ! replace antidiffusion error by diffusion error
        if (.not. antidiffusion) then ! implicit coefficients minimal 0.5 (default setting)
            do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
                if (theta(iq) > 0) theta(iq) = max(0.5, theta(iq))
            enddo
        endif

        ! search for explicit cells
        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir
            i = ipoint(1, iq)
            j = ipoint(2, iq)
            if (i > 0) thetaseg(i) = max(thetaseg(i), theta(iq))
            if (j > 0) thetaseg(j) = max(thetaseg(j), theta(iq))
        end do
        iexp = 0
        iexseg = 0
        do cell_i = 1, num_cells
            if (thetaseg(cell_i) < 1.0e-25) then
                iexp = iexp + 1
            else
                iexseg(cell_i) = 1
            end if
        end do

        if (timon) call timstop (ithandl)
    end subroutine calculate_theta

    subroutine output_theta(num_output_variables_extra, ounam, ipoint, num_constants, num_spatial_parameters, &
            num_time_functions, num_spatial_time_fuctions, num_substances_total, num_cells, num_local_vars, &
            proloc, num_defaults, theta)

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
        integer(kind = int_wp) :: cell_i                     ! segment loop counter

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("output_theta", ithandl)

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
            do cell_i = 1, num_cells
                proloc(ip_theta, cell_i) = theta(cell_i)
            enddo
        endif

        if (timon) call timstop (ithandl)
        return
    end subroutine output_theta

end module m_theta_calculation
