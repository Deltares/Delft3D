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
module m_dlwqm1
    use m_waq_precision

    implicit none

contains

    !> Compute the values for theta
    !! The calculation is limited to horizontal directions.
    !! For the vertical direction it is almost always theta= 1.0 so it is assumed to be that.
    subroutine dlwqm1(idt, num_cells, num_boundary_conditions, volold, num_exchanges, &
            num_exchanges_u_dir, num_exchanges_v_dir, ipoint, flowtot, disptot, &
            theta, thetaseg, antidiffusion, iexseg)

        use timers
        implicit none

        integer(kind = int_wp), intent(in   ) :: idt                   !< Time step
        integer(kind = int_wp), intent(in   ) :: num_cells                 !< Number of segments
        integer(kind = int_wp), intent(in   ) :: num_boundary_conditions                 !< Number of boundaries
        real(kind = real_wp),   intent(in   ) :: volold(num_cells)         !< Volumes at beginning of step (dim: num_cells)
        integer(kind = int_wp), intent(in   ) :: num_exchanges                   !< Total number of exchanges
        integer(kind = int_wp), intent(in   ) :: num_exchanges_u_dir                  !< Number of exchanges in the first direction
        integer(kind = int_wp), intent(in   ) :: num_exchanges_v_dir                  !< Number of exchanges in the second direction
        integer(kind = int_wp), intent(in   ) :: ipoint(4, num_exchanges)        !< Exchange pointers (dim: 4 x num_exchanges)
        real(kind = real_wp),   intent(in   ) :: flowtot(num_exchanges)          !< Total flows accross exchange surfs (dim: num_exchanges)
        real(kind = real_wp),   intent(in   ) :: disptot(num_exchanges)          !< Total flows accross exchange surfs (dim: num_exchanges)
        real(kind = real_wp),   intent(  out) :: theta(num_exchanges)            !< Variable theta coefficients (dim: num_exchanges)
        real(kind = real_wp),   intent(  out) :: thetaseg(num_cells)       !< Variable theta coefficients per segment
        logical(4),             intent(in   ) :: antidiffusion         !< If true: replace diffusion error by antidiffusion error
        integer(kind = int_wp), intent(  out) :: iexseg(num_cells + num_boundary_conditions) !< 0 if volume is explicit

        ! Local variables
        integer(kind = int_wp) :: i    !< From cell
        integer(kind = int_wp) :: j    !< To cell
        integer(kind = int_wp) :: iq   !< Current edge
        integer(kind = int_wp) :: iseg !< Current cell
        integer(kind = int_wp) :: iexp !< Explicit fraction of the problem

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqm1", ithandl)

        ! initialisation
        do iseg = 1, num_cells
            thetaseg(iseg) = 0.0
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
        do iseg = 1, num_cells
            if (thetaseg(iseg) > 0) thetaseg(iseg) = max(0.0, 1.0 - volold(iseg) / (real(idt) * thetaseg(iseg)))
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
        do iseg = 1, num_cells
            if (thetaseg(iseg) < 1.0e-25) then
                iexp = iexp + 1
            else
                iexseg(iseg) = 1
            end if
        end do

        if (timon) call timstop (ithandl)
    end subroutine dlwqm1
end module m_dlwqm1
