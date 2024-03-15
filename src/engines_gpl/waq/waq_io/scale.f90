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
module m_scale
    use m_waq_precision

    implicit none

contains


    subroutine scale (arrin, factor, nitem, nvals)

        !       Deltares Software Centre

        !>\file
        !>                   Scales an array

        !     Created            : March 1988 By M.E. Sileon / L. Postma
        !                          May   2011    Leo Postma : Fortran 90 look and feel

        !     Logical units      : none

        !     Subroutines called : none

        use timers       !   performance timers

        implicit none

        !     Parameters

        !     kind           function         name                        Descriptipon

        integer(kind = int_wp), intent(in) :: nvals                      !< number of values
        integer(kind = int_wp), intent(in) :: nitem                      !< number of items
        real(kind = real_wp), intent(inout) :: arrin (nvals, nitem)        !< number of items
        real(kind = real_wp), intent(in) :: factor(nvals)              !< scale factors

        !     local decalations

        integer(kind = int_wp) :: i1, i2        ! loop counters
        real(kind = real_wp) :: fact          ! factor
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("scale", ithndl)

        do i1 = 1, nvals
            fact = factor(i1)
            do i2 = 1, nitem
                arrin (i1, i2) = arrin (i1, i2) * fact
            enddo
        enddo

        if (timon) call timstop(ithndl)
        return
    end

end module m_scale
