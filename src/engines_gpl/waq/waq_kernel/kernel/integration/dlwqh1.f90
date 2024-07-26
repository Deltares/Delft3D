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
module m_dlwqh1
    use m_waq_precision

    implicit none

contains


    !> Sets the diagonal for the steady state option,
    !! updates first order term on the diagonal
    !! and compresses DERIV for use in SGMRES
    subroutine dlwqh1(num_cells, num_substances_total, num_boundary_conditions, isys, diag, &
            delvol, conc)

        use timers

        implicit none

        integer(kind = int_wp), intent(in   ) :: num_cells               !< Number of computational volumes
        integer(kind = int_wp), intent(in   ) :: num_substances_total               !< Total number of substances
        integer(kind = int_wp), intent(in   ) :: num_boundary_conditions               !< Number of open boundaries
        integer(kind = int_wp), intent(in   ) :: isys                !< This substance number
        real(kind = dp),        intent(inout) :: diag(num_cells + num_boundary_conditions) !< Diagonal vector (1st order term)
        real(kind = real_wp),   intent(in   ) :: delvol(num_cells)       !< Closure error correction
        real(kind = real_wp),   intent(in   ) :: conc(num_substances_total, num_cells)  !< First order term

        ! Local variables
        integer(kind = int_wp) :: iseg ! loop counter for computational volumes

        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt("dlwqh1", ithandl)

        ! set the right hand side and
        ! set the diagonal for steady state
        ! first order decay in conc
        do iseg = 1, num_cells
            diag(iseg) = -conc(isys, iseg) + delvol(iseg)
        end do
        do iseg = num_cells + 1, num_cells + num_boundary_conditions
            diag(iseg) = 1.0
        end do
        if (timon) call timstop(ithandl)
    end subroutine dlwqh1
end module m_dlwqh1
