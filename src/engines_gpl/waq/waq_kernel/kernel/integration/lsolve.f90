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
module m_lsolve
    use m_waq_precision

    implicit none

contains


    subroutine lsolve (ntrace, noseg, nolay, nsegl, nomat, &
            amat, imat, diag, idiag, x, &
            rhs, triwrk, iadd, iexseg)

        !     Deltares - Delft Software Department

        !     Created   : November 1996 by Kian Tan

        !     Function  : solve with lower triangular matrix:
        !                 Let A = (D - L - U) , solve (D-L) x = y
        !                 [ How about (I - L/D) x = y ?? ]

        !     Modified  : July     2008, Leo Postma  : WAQ perfomance timers
        !                 July     2009, Leo Postma  : double precission version
        !                 July     2009, Leo Postma  : vertical expanded in this code
        !                 November 2009, Leo Postma  : streamlined for parallel computing

        use timers
        implicit none

        !     Arguments           :

        !     Kind        Function         Name                  Description

        integer(kind = int_wp), intent(in) :: ntrace               ! dimension of matrix (length of diagonal)
        integer(kind = int_wp), intent(in) :: noseg                ! number of volumes
        integer(kind = int_wp), intent(in) :: nolay                ! number of layers in the vertical
        integer(kind = int_wp), intent(in) :: nsegl                ! number of volumes per layer
        integer(kind = int_wp), intent(in) :: nomat                ! number of off-diagonal entries matrix
        real(kind = dp), intent(in) :: amat  (nomat)     ! off-diagonal entries matrix
        integer(kind = int_wp), intent(in) :: imat  (nomat)     ! collumn nrs of off-diagonal entries matrix
        real(kind = dp), intent(in) :: diag  (ntrace)     ! off-diagonal entries matrix
        integer(kind = int_wp), intent(in) :: idiag (0:ntrace)     ! start of row in amat
        real(kind = dp), intent(out) :: x     (ntrace)     ! x = M^{-1} y
        real(kind = dp), intent(in) :: rhs   (ntrace)     ! right hand side of this iteration only
        real(kind = dp), intent(inout) :: triwrk(nolay)     ! work array for vertical double sweep
        integer(kind = int_wp), intent(in) :: iadd                 ! offset for vertical off-diagonals
        integer(kind = int_wp), intent(in) :: iexseg(ntrace)     ! = 0 if volume is fully explicit

        !        local variables

        real(kind = dp) :: pivot                ! multiplier in double sweep vertical
        integer(kind = int_wp) :: isegl                ! this volume of one layer
        integer(kind = int_wp) :: iseg                 ! this volume
        integer(kind = int_wp) :: ilay                 ! this layer
        integer(kind = int_wp) :: jcol                 ! collumn counter for off-diagonals
        integer(kind = int_wp) :: ilow, ihigh          ! loop boundaries

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("lsolve", ithandl)

        !        First copy rhs into x

        x = rhs

        !        loop over all interior grid points in top layer.

        do isegl = 1, nsegl

            do ilay = 1, nolay

                iseg = isegl + (ilay - 1) * nsegl
                if (iexseg(iseg) == 0) cycle
                ilow = idiag(iseg - 1) + 1
                ihigh = idiag(iseg)
                do jcol = ilow + iadd, ihigh
                    if (imat(jcol) > 0 .and. (imat(jcol) < iseg .or. imat(jcol) > noseg)) &
                            x(iseg) = x(iseg) - amat(jcol) * x(imat(jcol))
                enddo
            enddo

            if (nolay == 1) then

                x(isegl) = x(isegl) / diag(isegl)

            else

                !           direct tridiagonal solver expanded in this code

                pivot = diag(isegl)
                x(isegl) = x(isegl) / pivot
                ilow = idiag(isegl - 1) + 2
                iseg = isegl
                triwrk(1) = amat(ilow) / pivot
                do ilay = 2, nolay
                    iseg = iseg + nsegl
                    pivot = diag(iseg) - amat(idiag(iseg - 1) + 1) * triwrk(ilay - 1)
                    x(iseg) = (x   (iseg) - amat(idiag(iseg - 1) + 1) * x(iseg - nsegl)) / pivot
                    triwrk(ilay) = amat(idiag(iseg - 1) + 2) / pivot
                enddo
                do ilay = nolay - 1, 1, -1
                    x(isegl + (ilay - 1) * nsegl) = x(isegl + (ilay - 1) * nsegl) - triwrk(ilay) * x(isegl + ilay * nsegl)
                enddo

            endif
        enddo

        if (timon) call timstop (ithandl)
        RETURN
    END

end module m_lsolve
