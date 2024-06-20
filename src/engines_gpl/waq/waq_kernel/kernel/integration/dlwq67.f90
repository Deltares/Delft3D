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
module m_dlwq67
    use m_waq_precision

    implicit none

contains

    !> updates the diagonal if zero
    subroutine dlwq67(amat, noseg, jtrack)
        use timers

        real(kind = real_wp), intent(inout) :: amat(*) !< Matrix to invert
        integer(kind = int_wp), intent(in)  :: noseg   !< Number of cells or segments
        integer(kind = int_wp), intent(in)  :: jtrack  !< Number of codiagonals

        ! Local variables
        integer(kind = int_wp) :: iseg, istep, iset
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("dlwq67", ithandl)
        ! set the diagonal
        istep = jtrack * 2 + 1
        iset = jtrack + 1
        do iseg = 1, noseg
            if (abs(amat(iset)) < 1.0e-35) amat(iset) = 1.0
            iset = iset + istep
        end do

        if (timon) call timstop (ithandl)
    end subroutine dlwq67
end module m_dlwq67
