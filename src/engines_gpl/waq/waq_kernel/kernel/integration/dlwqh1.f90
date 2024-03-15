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


    subroutine dlwqh1 (noseg, notot, nobnd, isys, diag, &
            delvol, conc)

        !     Deltares Software Centre

        !     Function: - sets the diagonal for the steady state option
        !               - updates first order term on the diagonal
        !               - compresses DERIV for use in SGMRES

        !     Created : 10 February 1997 by RJ Vos
        !     Modified:  7 June     2010 by Leo Postma  Double precision version
        !                                               NSYS fixated at 1

        !     File I/O: none

        !     Subroutines called  : none

        use timers                         ! WAQ performance timers

        implicit none

        !     Arguments           :

        !     Kind        Function         Name                   Description

        integer(kind = int_wp), intent(in) :: noseg                ! Number of computational volumes
        integer(kind = int_wp), intent(in) :: notot                ! Total number of substances
        integer(kind = int_wp), intent(in) :: nobnd                ! Number of open boundaries
        integer(kind = int_wp), intent(in) :: isys                 ! This substance number
        real(kind = dp), intent(out) :: diag  (noseg + nobnd)  ! Diagonal vector (1st order term)
        real(kind = real_wp), intent(in) :: delvol(noseg)        ! Closure error correction
        real(kind = real_wp), intent(in) :: conc  (notot, noseg)  ! First order term

        !     local variables

        integer(kind = int_wp) :: iseg               ! loop counter for computational volumes

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqh1", ithandl)

        !         set the right hand side and
        !         set the diagonal for steady state
        !                          first order decay in conc

        do iseg = 1, noseg
            diag(iseg) = -conc(isys, iseg) + delvol(iseg)
        enddo
        do iseg = noseg + 1, noseg + nobnd
            diag(iseg) = 1.0
        enddo

        if (timon) call timstop (ithandl)
        return
    end

end module m_dlwqh1
