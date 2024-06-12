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
module m_dlwq61
    use m_waq_precision

    implicit none

contains


    !> Zeroes the matrix, updates first-order term on the diagonal
    !! and compresses DERIV for use in DELMAT
    subroutine dlwq61(conc, deriv, amass, amat, noseg, &
            notot, isys, nsys, jtrack)

        use timers

        real(kind = real_wp), intent(in   ) :: conc(notot, *) !< First order term
        real(kind = real_wp), intent(inout) :: deriv(*)       !< Right hand side matrix
        real(kind = real_wp), intent(in   ) :: amass(*)       !< Closure error correction
        real(kind = real_wp), intent(inout) :: amat(*)        !< Matrix to invert

        integer(kind = int_wp), intent(in   ) :: noseg  !< Number of cells (or segments)
        integer(kind = int_wp), intent(in   ) :: notot  !< Total number of systems
        integer(kind = int_wp), intent(in   ) :: isys   !< System considered
        integer(kind = int_wp), intent(in   ) :: nsys   !< Number of systems to take
        integer(kind = int_wp), intent(in   ) :: jtrack !< Number of codiagonals


        ! Local variables
        integer(kind = int_wp) :: ntot, i, istep, iset, iseg, ioff
        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("dlwq61", ithandl)

        ! zero the matrix
        istep = jtrack * 2 + 1
        ntot = noseg * istep
        do i = 1, ntot
            amat(i) = 0.0
        end do

        ! set the diagonal
        iset = jtrack + 1
        do iseg = 1, noseg
            amat(iset) = -conc(isys, iseg) + amass(iseg)
            iset = iset + istep
        end do

        ! set the right hand side
        iset = 1
        ioff = 0
        do iseg = 1, noseg
            do i = isys, isys + nsys - 1
                deriv(iset) = deriv(ioff + i)
                iset = iset + 1
            end do
            ioff = ioff + notot
        end do
        !
        if (timon) call timstop (ithandl)
    end

end module m_dlwq61
