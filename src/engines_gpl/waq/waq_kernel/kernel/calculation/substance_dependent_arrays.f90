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
module m_substance_dependent_arrays
    use m_waq_precision
    use timers

    implicit none

    private
    public :: create_substance_dependent_flow_and_diffusion_array

contains


    !> Creates the arrays with substance dependent flow (flowtot) and diffusion (disptot)
    !! This routine takes 3 times as much computation time than the more complicated
    !! calculate_theta routine.
    !! The only reason for that is that the 2 dimensional indices of
    !! the velo and disper arrays should have num_exchanges as first dimension rather than second.
    !! For the coarse Hong Kong situation with 37500 volumes and 20 substances, the full
    !! span of 20*37500*4 = 3 Mb of the disper array schould be read and approximately
    !! half of it for the velo array. This is done 20 times per time step, so 90 Mb per
    !! time step. With a 1066 MHz front side bus and the DDR3 memory of the Intel T9400
    !! processor of my portable this takes 60/2 = 30 ms per time step or 7 seconds for
    !! 240 time steps for the test computation. If the 2 indeces are interchanged, only
    !! 4.5 Mb should be transported, which costs only 360 ms for the test.
    subroutine create_substance_dependent_flow_and_diffusion_array(substance_i, num_substances_transported, &
            num_exchanges, num_exchanges_u_dir, num_exchanges_v_dir, area, flow, flowtot, num_velocity_arrays, ivpnt, &
            velo, disp, disptot, num_dispersion_arrays, idpnt, &
            disper, mixlen)

        integer(kind = int_wp), intent(in) :: substance_i                !< Current active substance
        integer(kind = int_wp), intent(in) :: num_substances_transported    !< Number of active substances
        integer(kind = int_wp), intent(in) :: num_exchanges                 !< Number of exchanges
        integer(kind = int_wp), intent(in) :: num_exchanges_u_dir           !< Number of exchanges in first direction
        integer(kind = int_wp), intent(in) :: num_exchanges_v_dir           !< Number of exchanges in second direction
        real(kind = real_wp), intent(in) :: area(num_exchanges)           !< Exchange surface areas (dim: num_exchanges)
        real(kind = real_wp), intent(in) :: flow(num_exchanges)     !< Flows accross exchange surfs (dim: num_exchanges)
        real(kind = real_wp), intent(out) :: flowtot(num_exchanges) !< Flows plus additional velos. (dim: num_exchanges)
        integer(kind = int_wp), intent(in) :: num_velocity_arrays              !< Number  of additional velos.
        integer(kind = int_wp), intent(in) :: ivpnt(num_substances_transported) !< Pointer systems to velocities (dim: num_substances_transported)
        real(kind = real_wp), intent(in) :: velo(num_velocity_arrays, num_exchanges)   !< Additional velocity array (dim: num_velocity_arrays*num_exchanges)
        real(kind = real_wp), intent(in) :: disp(3)             !< Dispersion in 3 directions
        real(kind = real_wp), intent(out) :: disptot(num_exchanges)!< Dispersion plus additional dipers. (dim: num_exchanges)
        integer(kind = int_wp), intent(in) :: num_dispersion_arrays              !< Number  of additional dispers.
        integer(kind = int_wp), intent(in) :: idpnt(num_substances_transported)        !< Pointer systems to dispersions (dim: num_substances_transported)
        real(kind = real_wp), intent(in) :: disper(num_dispersion_arrays, num_exchanges) !< Additional dispersion array (dim: num_dispersion_arrays*num_exchanges)
        real(kind = real_wp), intent(in) :: mixlen(num_exchanges)         !< Area / length

        ! Local variables
        integer(kind = int_wp) :: iq !< Current edge
        integer(kind = int_wp) :: iv !< Index for additional volume
        integer(kind = int_wp) :: id !< Index for additional dispersion

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("create_substance_dependent_flow_and_diffusion_array", ithandl)

        iv = ivpnt(substance_i)
        id = idpnt(substance_i)

        if (iv == 0) then
            flowtot = flow
        else
            do iq = 1, num_exchanges
                flowtot(iq) = flow(iq) + velo(iv, iq) * area(iq)
            end do
        end if

        if (id == 0) then
            do iq = 1, num_exchanges_u_dir
                disptot(iq) = disp(1) * mixlen(iq)
            end do
            do iq = num_exchanges_u_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir
                disptot(iq) = disp(2) * mixlen(iq)
            end do
            do iq = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges
                disptot(iq) = disp(3) * mixlen(iq)
            end do
        else
            do iq = 1, num_exchanges_u_dir
                disptot(iq) = (disp(1) + disper(id, iq)) * mixlen(iq)
            end do
            do iq = num_exchanges_u_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir
                disptot(iq) = (disp(2) + disper(id, iq)) * mixlen(iq)
            end do
            do iq = num_exchanges_u_dir + num_exchanges_v_dir + 1, num_exchanges
                disptot(iq) = (disp(3) + disper(id, iq)) * mixlen(iq)
            end do
        end if

        if (timon) call timstop (ithandl)
    end subroutine create_substance_dependent_flow_and_diffusion_array
end module m_substance_dependent_arrays
