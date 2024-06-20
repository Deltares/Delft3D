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
module m_delmat
    use m_waq_precision
    implicit none

contains

    !> Solves a set of linear equations described by a
    !! band matrix with largest elements on the diagonal.
    !! The subroutine expects the matrix (A) to be stored in a
    !! one dimensional equivalent of the two dimensional matrix
    !! representation. In two dimensional form, the storage rules
    !! used by the imsl package for band matrices must be used.
    !! However, the rows and columns must be interchanged.
    !! The same holds for the full matrix of known vectors "b".
    subroutine delmat(n, nuc, nlc, m, a, b, option)

        use m_logger_helper, only : stop_with_error
        use timers

        integer(kind = int_wp), intent(in   ) :: n      !< Order of the matrix a
        integer(kind = int_wp), intent(in   ) :: nuc    !< Number of upper codiagonals
        integer(kind = int_wp), intent(in   ) :: nlc    !< Number of lower codiagonals
        integer(kind = int_wp), intent(in   ) :: m      !< Number of known vectors of order n in "b"
        integer(kind = int_wp), intent(in   ) :: option !< Operations to carry out on the system:
                                                        !< 1 = returns only the LU-decomposition of the matrix a,
                                                        !< while b remains unchanged
                                                        !< 2 = returns the solution for the unknown vectors in b
                                                        !< it needs a proper decomposed and stored matrix a
                                                        !< as an input
                                                        !< 0 or any other value than 1 and 2:
                                                        !< returns the LU-decomposition in matrix a (which can
                                                        !< then be used again for new known vectors) and it
                                                        !< returns the solution for the unknown vectors
                                                        !< in matrix b

        real(kind = real_wp),   intent(inout) :: a(*) !< Band-matrix in one-dimensional form
        real(kind = real_wp),   intent(inout) :: b(*) !< Vector of knowns in one-dimensional form
        
        ! Local variables
        integer(kind = int_wp) :: ithandl = 0
        integer(kind = int_wp) :: nmuc, nmlc, ndm1, nd, n1
        integer(kind = int_wp) :: l1, l2, l3, l4, l5
        integer(kind = int_wp) :: k1, k2, k3, k4, k5, k6
        real(kind = real_wp)   :: f, p

        if (timon) call timstrt ("delmat", ithandl)
        nmuc = n - nuc
        nmlc = n - nlc
        ndm1 = nuc + nlc
        nd = ndm1 + 1
        if (option==2) goto 1000

        ! LU-decomposition
        ! k1 = outer loop variable, counting the number of
        !    columns with full length
        ! l1 = corresponding limit variable
        ! p  = pivot element. For the advection dispersion
        !    equation this is almost always the largest element
        k1 = nlc + 1
        l1 = k1 + nmlc * nd
        100 p = a(k1)
        if (abs(p) < 1.0e-35) then
            write(6, '('' Matrix for DELMAT singular at element:'',I5)') &
                    k1 / nd + 1
            call stop_with_error()
        endif

        ! k2 is the middle loop variable counting the number of
        !    elements to be eleminated
        ! l2 is the corresponding limit variable
        ! f  is the multiplication factor for the correction of
        !    the row of elimination. it enters a(k2) as the negative
        !    of the element of the new lower triangular matrix
        k2 = k1
        l2 = k1 + nlc * ndm1
        200 k2 = k2 + ndm1
        f = a(k2) / p
        a(k2) = f
        k3 = k1

        ! k4 is the inner loop variable counting the elements on
        !    the row of elimination
        ! l4 is the corresponding limmit variable
        ! k3 is the corresponding counter for the elements on the
        !    row of the diagonal element
        k4 = k2
        l4 = k2 + nuc
        300 k3 = k3 + 1
        k4 = k4 + 1
        a(k4) = a(k4) - f * a(k3)

        ! book-keeping of the loop variables
        if (k4<l4) goto 300
        if (k2<l2) goto 200
        k1 = k1 + nd
        if (k1<l1) goto 100
        if (nlc==1) goto 700

        ! the columns become shorter, the rest is the same
        n1 = nlc
        l1 = (n - 1) * nd
        400 n1 = n1 - 1
        p = a(k1)
        if (abs(p) < 1.0e-35) then
            write(6, '('' Matrix for DELMAT singular at element:'',I5)') &
                    k1 / nd + 1
            call stop_with_error()
        endif
        k2 = k1
        l2 = k1 + n1 * ndm1
        500 k2 = k2 + ndm1
        f = a(k2) / p
        a(k2) = f
        k3 = k1
        k4 = k2
        l4 = k2 + nuc
        600 k3 = k3 + 1
        k4 = k4 + 1
        a(k4) = a(k4) - f * a(k3)
        if (k4<l4) goto 600
        if (k2<l2) goto 500
        k1 = k1 + nd
        if (k1<l1) goto 400

        ! entry for substitution option
        700 if(option==1) goto 9999  !   return
        1000 continue

        ! The forward substitution has essentially the same
        ! structure
        ! k1 = outer loop counter lower triangular matrix
        ! k2 = inner loop counter lower triangular matrix
        ! l1 and l2 are the loop limits
        ! k5 = outer loop counter matrix of known vectors
        ! k4 = row counter in relation to k5
        ! k3 = inner loop and row counter matrix of known vectors
        ! l3 = row limit for one substitution element "f"
        ! f  = substitution element of lower triangular matrix
        k1 = - nuc
        l1 = k1 + nmlc * nd
        k5 = - m
        1100 k1 = k1 + nd
        k5 = k5 + m
        k2 = k1
        l2 = k1 + nlc * ndm1
        k3 = k5 + m
        l3 = k3
        1200 k2 = k2 + ndm1
        f = a(k2)
        l3 = l3 + m
        k4 = k5
        1300 k3 = k3 + 1
        k4 = k4 + 1
        b(k3) = b(k3) - f * b(k4)
        if (k3<l3) goto 1300
        if (k2<l2) goto 1200
        if (k1<l1) goto 1100
        if (nlc==1) goto 2000

        ! the collumns become shorter, the rest is the same
        n1 = nlc
        l1 = (n - 2) * nd
        1400 k1 = k1 + nd
        k5 = k5 + m
        k2 = k1
        n1 = n1 - 1
        l2 = k1 + n1 * ndm1
        k3 = k5 + m
        l3 = k3
        1500 k2 = k2 + ndm1
        f = a(k2)
        l3 = l3 + m
        k4 = k5
        1600 k3 = k3 + 1
        k4 = k4 + 1
        b(k3) = b(k3) - f * b(k4)
        if (k3<l3) goto 1600
        if (k2<l2) goto 1500
        if (k1<l1) goto 1400

        ! backward substitution
        2000 k1 = n * nd + nlc + 1
        l1 = k1 - nmuc * nd
        k5 = n * m + 1
        2100 k1 = k1 - nd
        l5 = k5 - m
        f = a(k1)
        2200 k5 = k5 - 1
        b(k5) = b(k5) / f
        if (k5>l5) goto 2200
        k2 = k1
        l2 = k1 - nuc * ndm1
        k3 = k5
        l3 = k5
        k6 = k5 + m
        2300 k2 = k2 - ndm1
        f = a(k2)
        l3 = l3 - m
        k4 = k6
        2400 k3 = k3 - 1
        k4 = k4 - 1
        b(k3) = b(k3) - f * b(k4)
        if (k3>l3) goto 2400
        if (k2>l2) goto 2300
        if (k1>l1) goto 2100
        if (nuc==1) goto 2850
        n1 = nuc
        l1 = 2 * nd
        2500 k1 = k1 - nd
        l5 = k5 - m
        f = a(k1)
        n1 = n1 - 1
        2600 k5 = k5 - 1
        b(k5) = b(k5) / f
        if (k5>l5) goto 2600
        k2 = k1
        l2 = k1 - n1 * ndm1
        k3 = k5
        l3 = k5
        k6 = k5 + m
        2700 k2 = k2 - ndm1
        f = a(k2)
        l3 = l3 - m
        k4 = k6
        2800 k3 = k3 - 1
        k4 = k4 - 1
        b(k3) = b(k3) - f * b(k4)
        if (k3>l3) goto 2800
        if (k2>l2) goto 2700
        if (k1>l1) goto 2500
        2850 k1 = k1 - nd
        l5 = k5 - m
        f = a(k1)
        2900 k5 = k5 - 1
        b(k5) = b(k5) / f
        if (k5>l5) goto 2900

        9999 if (timon) call timstop (ithandl)
    end subroutine delmat
end module m_delmat
