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
module m_constr
    use m_waq_precision
    use m_ebcalc

    implicit none

contains


    !  *********************************************************************
    !  *    SUBROUTINE TO DETERMINE LIMITS ON THE EXTINCTION COEFFICIENT   *
    !  *********************************************************************

    subroutine constr(surf, dmix, emin, root, numgr)

        use bloom_data_dim
        use bloom_data_arran

        implicit none

        real(kind = dp) :: root(2)
        real(kind = dp) :: s0
        real(kind = dp) :: surf
        real(kind = dp) :: x
        real(kind = dp) :: fx
        real(kind = dp) :: derx
        real(kind = dp) :: phi
        real(kind = dp) :: emin
        real(kind = dp) :: y
        real(kind = dp) :: dmix
        real(kind = dp) :: b(2)
        real(kind = dp) :: a(2)
        real(kind = dp) :: fm
        real(kind = dp) :: fpr
        real(kind = dp) :: ebar
        real(kind = dp) :: deriv
        real(kind = dp) :: xm
        real(kind = dp) :: fp

        integer(kind = int_wp) :: i, k, numgr
        logical :: rootsexist

        !  Determines limits on the extinction coefficient
        root(1) = -1.0
        root(2) = -1.0
        s0 = dexp(-zvec(nz))
        if (surf <= s0) return

        !  Find interval containing all roots
        x = -log(surf)
        call ebcalc(x, fx, derx, numgr)
        phi = (fun(nz, numgr) - fx) / emin
        y = x + phi
        if (derx >= emin) root(1) = 0.0
        if (y >= zvec(nz)) root(2) = phi / dmix
        if (root(1) >= 0.0 .and. root(2) >= 0.0) return

        !  Split interval to isolate each root.
        rootsexist = .false.
        b(1) = x
        b(2) = y
        do k = 1, 10
            a(1) = 0.5 * (b(1) + b(2))
            call ebcalc(a(1), fm, fpr, numgr)
            ebar = (fm - fx) / (a(1) - x)
            if (ebar >= emin) then
                rootsexist = .true.
                exit
            end if
            deriv = (fpr - ebar) / (a(1) - x)
            if (deriv < 0.0) b(2) = a(1)
            if (deriv >= 0.0) b(1) = a(1)
        end do

        !  No roots exist
        if(.not.rootsexist) then
            return
        endif

        !  We have separated the roots.
        a(2) = a(1)
        do i = 1, 2
            if (root(i) >= 0.0) cycle
            do k = 1, 10
                xm = 0.5 * (a(i) + b(i))
                call ebcalc(xm, fm, fp, numgr)
                ebar = (fm - fx) / (xm - x)
                if (ebar > emin) a(i) = xm
                if (ebar <= emin) b(i) = xm
            end do
            root(i) = (0.5 * (a(i) + b(i)) - x) / dmix
        end do
        return
    end

end module m_constr
