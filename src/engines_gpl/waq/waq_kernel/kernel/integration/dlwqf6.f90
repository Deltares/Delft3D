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
module m_dlwqf6
    use m_waq_precision

    implicit none

contains

    !>  Tranfers solution from RHS into CONC, and makes RHS equal to zero
    subroutine dlwqf6(num_cells, num_substances_total, isys, nsys, rhs, conc, iknmrk)

        use timers

        integer(kind=int_wp), intent(in   ) :: num_cells              !< Number of computational volumes
        integer(kind=int_wp), intent(in   ) :: num_substances_total              !< Total number of substances
        integer(kind=int_wp), intent(in   ) :: isys               !< First substance to update
        integer(kind=int_wp), intent(in   ) :: nsys               !< Total number of substances to update
        real(kind=dp),        intent(inout) :: rhs(nsys, num_cells)   !< RHS matrix for the nsys substances
        real(kind=real_wp),   intent(inout) :: conc(num_substances_total, num_cells) !< Target array for update
        integer(kind=int_wp), intent(in   ) :: iknmrk(num_cells)      !< feature array, bit zero indicates wet or not

        ! Local variables
        integer(kind=int_wp) :: iseg, j       !< Loop variables
        integer(kind=int_wp) :: ithandl = 0

        if (timon) call timstrt("dlwqf6", ithandl)

        do iseg = 1, num_cells
            do j = 1, nsys
                conc(isys + j - 1, iseg) = rhs(j, iseg)
                rhs(j, iseg) = 0.0d00
            end do
        end do
        if (timon) call timstop(ithandl)
    end subroutine dlwqf6
end module m_dlwqf6
