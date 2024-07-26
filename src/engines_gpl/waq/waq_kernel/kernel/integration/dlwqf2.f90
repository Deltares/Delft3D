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
module m_dlwqf2
    use m_waq_precision

    implicit none

contains


    !> Initialize diagonal for fast solvers
    subroutine dlwqf2(num_cells, num_boundary_conditions, idt, volnew, trace)

        use timers

        implicit none

        integer(kind = int_wp), intent(in) :: num_cells !< Number of cells or computational volumes
        integer(kind = int_wp), intent(in) :: num_boundary_conditions !< Number of open boundaries
        integer(kind = int_wp), intent(in) :: idt   !< Time step size

        real(kind = real_wp), intent(in   ) :: volnew(num_cells)        !< Volumes end of time step
        real(kind = dp),      intent(  out) :: trace(num_cells + num_boundary_conditions) !< Diagonal vector

        ! Local variables
        real(kind = dp) :: dt           !< Time step in double precision
        integer(kind = int_wp) :: iseg  !< Loop variable

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqf2", ithandl)

        ! set the diagonal
        dt = idt
        do iseg = 1, num_cells
            trace(iseg) = volnew(iseg) / dt
        end do
        do iseg = num_cells + 1, num_cells + num_boundary_conditions
            trace(iseg) = 1.0
        end do
        if (timon) call timstop (ithandl)
    end subroutine dlwqf2
end module m_dlwqf2
