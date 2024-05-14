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
module m_qslp
    use m_waq_precision

    implicit none

contains


    !-----------------------------------------------------------------------
    ! QSLP Quick Simplex algorithm to solve a Linear Program.
    ! The technique used here is a variant of the primal - dual algorithm.
    ! The call to this routine is similar to the one to DOSP.

    !-----------------------------------------------------------------------
    subroutine qslp(a, ia, nr, nc, b, lsc, c, integration_id_list, irs, lib, d, mt, x, p, ier)

        implicit none

        real(kind = dp) :: a(1:ia, 1:mt), b(*), c(*), d(*), x(*), p(*)
        real(kind = dp) :: xopt, bpivot, bmin, cpivot, apivot, aipj, cj, aijp, bi, ap, cjpap
        integer(kind = int_wp) :: lib(*), ier, lsc(*), integration_id_list(*), irs(*), nqslp
        integer(kind = int_wp) :: i, j, k, l, ia, mt, ip, jp, jpneg
        integer(kind = int_wp) :: ineg, iter, itflag, ihelp1, ihelp2
        integer(kind = int_wp) :: nr, nc, method

        data nqslp /0/

        ! If the subroutine is called for the first time, perform some initial
        ! checks.
        nqslp = nqslp + 1
        if (nqslp > 1) go to 10
        if (nr >= ia .or. nc >= mt) then
            ier = 1000
            go to 340
        end if

        ! Initiate control integers, copy arrays and set signs for constraints.
        10 continue
        ier = 0
        irs(2) = 0
        iter = 0
        itflag = 0
        xopt = 0.0d0
        x (nr + nc + 1) = 0.0d0

        ! Initiate LIB. This array contains information on currently basic
        ! variables.
        do i = 1, nr + nc
            lib(i) = i
            x (i) = 0.0d0
        end do

        ! Optionally copy A, B and C to D, which is not used for computational
        ! purposes in this subroutine (unlike DOSP).
        ! At the end the original values of these arrays are restored.
        if (integration_id_list(3) == 1) go to 60
        k = 0
        l = nr * nc
        do j = 1, nc
            l = l + 1
            d(l) = c(j)
            do i = 1, nr
                k = k + 1
                d(k) = a(i, j)
            end do
        end do
        do i = 1, nc
            l = l + 1
            d(l) = b(i)
        end do
        60 continue

        ! Check if there are any ">" constraint. Reverse the sign of the entries
        ! in the A matrix and of the B vector.
        ! Check if all "<" constraints are positive. If not, problem is
        ! infeasible. Set exit values and leave.
        ineg = 0
        do i = 1, nr
            if (lsc (i) <= 0) then
                if (b(i) >= 0.0) cycle
                ier = 100
                irs(2) = 4
                irs(3) = i

                go to 290
            end if
            do j = 1, nc
                a(i, j) = - a(i, j)
            end do
            b (i) = - b(i)
            ineg = ineg + 1
        end do

        ! Reverse the sign of the C vector for a maximization problem.
        ! Note: minimization is NOT currently supported!
        if (integration_id_list(4) == 1) then
            do j = 1, nc
                c (j) = - c(j)
            end do
        end if
        if (ineg == 0) go to 170
        !-----------------------------------------------------------------------
        ! Satisfy greater than constraints, if there are any. Use the
        ! Dual method.
        !-----------------------------------------------------------------------
        100 continue
        ier = 100
        irs(2) = 4
        method = 1

        ! Get pivot row. Use two different steps. First determine if there
        ! are any negative ">" constraints left. If this is the case, these
        ! are resolved first (INEG > 0). If there are none left, variables
        ! which may have become negative are selected for pivoting.
        ! Select the minimum value of B(I) as pivot row.
        ineg = 0
        do i = 1, nr
            if (b(i) >= -1.0d-12) cycle
            if (lib(i) <= nr .and. lsc (lib(i)) == 1) ineg = ineg + 1
        end do
        bpivot = 1.0d40
        bmin = 1.0d40
        do i = 1, nr
            if (b(i) < bmin) bmin = b(i)
            if (b(i) >= bpivot) cycle
            if (lib(i) <= nr .and. lsc (lib(i)) <= 0 .and. ineg > 0) cycle
            bpivot = b(i)
            ip = i
        end do

        ! If minimum of B(I) is positive, the DUAL part of the algorithm can
        ! be terminated successfully. Continue with the primal method. A
        ! feasible solution exists. If no pivot was found, but the BMIN is
        ! still negative, no feasible solution exists.
        if (bmin > -1.0d-12) go to 170
        if (bpivot > -1.0d-12) go to 140

        ! Get pivot column. For ">" constraints select Min C(J)/A(IP,J)
        ! under the condition that A(IP,J) and C(J) are negative.
        ! Additionally record the (absolute) minimum A(IP,J). If no pivot row
        ! was found, but there is at least one negative A(IP,J) value, this
        ! is used for pivoting. This pivot row is also used for "<" constraints
        ! and structural variables, which have become negative in previous
        ! iterations.
        jp = 0
        jpneg = 0
        cpivot = 1.0d40
        apivot = 0.0
        do j = 1, nc
            aipj = a(ip, j)
            if (aipj > -1.0d-12) cycle
            if (aipj < apivot) then
                apivot = aipj
                jpneg = j
            end if
            if (ineg == 0) cycle
            cj = c(j)
            if (cj >= 0.0) cycle
            if (cj / aipj > cpivot) cycle
            cpivot = cj / aipj
            jp = j
        end do
        if (jp > 0) go to 200
        if (jpneg > 0) then
            jp = jpneg
            go to 200
        end if
        !-----------------------------------------------------------------------
        ! No feasible solution. Check which variables are negative. Replace
        ! all neagative structural variables and "<" constraint by ">"
        ! constraints.
        !-----------------------------------------------------------------------
        140 continue
        method = 3
        ip = 0
        bpivot = -1.0d-12
        bmin = -1.0d-12
        do i = 1, nr
            if (b(i) <= -1.0d-12 .and. lib(i) <= nr .and. lsc (lib(i)) == 1) then
                if (b(i) < bmin) then
                    bmin = b(i)
                    irs(3) = lib(i)
                end if
                cycle
            end if
            if (b(i) >= bpivot) cycle
            bpivot = b(i)
            ip = i
        end do
        if (ip == 0) go to 290

        ! Find the pivot column. Use Max A(IP,J) under the condition
        ! A(IP,J) > 0.0. Note: do not replace IP by a structural variable!
        jp = 0
        apivot = -1.0d40
        do j = 1, nc
            if (lib(nr + j) > nr .or. lsc(lib(nr + j)) /= 1) cycle
            if (a(ip, j) < 1.0d-12) cycle
            if (a(ip, j) < apivot) cycle
            apivot = a(ip, j)
            jp = j
        end do
        if (jp == 0) go to 290
        go to 200
        !-----------------------------------------------------------------------
        ! Satisfy smaller than constraints using the primal method.
        !-----------------------------------------------------------------------
        170 continue
        !
        ! Get pivot column. Use Minimum C(J). Terminate when CMIN >= 0.0.
        method = 2
        jp = 0
        cpivot = 1.0d40
        do j = 1, nc
            if (c(j) >= cpivot) cycle
            cpivot = c(j)
            jp = j
        end do
        if (cpivot >= -1.0d-12) then
            ier = 0
            irs(2) = 0
            irs(3) = nr + nc + 1
            go to 290
        end if

        ! Get pivot row. Choose IP as MIN B(I) / A(I,JP) under the condition
        ! that A(I,JP) > 0.0.
        ip = 0
        bpivot = 1.0d40
        do i = 1, nr
            aijp = a(i, jp)
            if (aijp < 1.0d-12) cycle
            bi = b(i)
            if (bi / aijp >= bpivot) cycle
            bpivot = bi / aijp
            ip = i
        end do
        !-----------------------------------------------------------------------
        ! Start pivot operation. Update A, B and C.
        !-----------------------------------------------------------------------
        200 continue
        iter = iter + 1
        itflag = itflag + 1

        ! Check the number of iterations. If exceeded, abort.
        if (iter >= integration_id_list(1)) then
            ier = 100
            irs(2) = 2
            go to 290
        end if

        ! Check the number of operations since the last update of the arrays.
        ! If exceeded, reset all small numbers to 0.0D0 to avoid round-off
        ! errors becoming to large.
        if (itflag >= integration_id_list(2)) then
            itflag = 0
            do i = 1, nr
                if (dabs(b(i)) < 1.0d-12) b(i) = 0.0d0
                do j = i, nc
                    if (dabs(a(i, j)) < 1.0d-12) a(i, j) = 0.0d0
                end do
            end do
            do j = i, nc
                if (dabs(c(j)) < 1.0d-12) c(j) = 0.0d0
            end do
        end if
        !
        ! Modify pivot and pivot row.
        ap = a (ip, jp)
        a(ip, jp) = 1 / ap
        do i = 1, nr
            if (i == ip) cycle
            a (i, jp) = - a (i, jp) / ap
        end do

        ! Update optimum.
        cjpap = c(jp) / ap
        xopt = xopt - b(ip) * cjpap

        ! Update C vector.
        do j = 1, nc
            if (j == jp) cycle
            if (dabs(a(ip, j)) < 1.0d-12) cycle
            c(j) = c(j) - a(ip, j) * cjpap
        end do

        ! Modify elements not in pivot row or column. Update B vector.
        do i = 1, nr
            if (i == ip) cycle
            aijp = a(i, jp)
            if (dabs(aijp) < 1.0d-12) cycle
            do j = 1, nc
                if (j == jp) cycle
                if (dabs(a(ip, j)) < 1.0d-12) cycle
                a(i, j) = a(i, j) + a(ip, j) * aijp
            end do
            bi = b (i) + b(ip) * aijp
            if (dabs(bi) < 1.0d-12) then
                b(i) = 0.0d0
            else
                b(i) = bi
            end if
        end do

        ! Modify pivot column.
        b(ip) = b(ip) / ap
        c(jp) = - cjpap
        do j = 1, nc
            if (j == jp) cycle
            a(ip, j) = a(ip, j) / ap
        end do

        ! Update LIB to indicate basic variables.
        ihelp1 = lib (jp + nr)
        ihelp2 = lib (ip)
        lib (jp + nr) = ihelp2
        lib (ip) = ihelp1
        if (method == 1) go to 100
        if (method == 2) go to 170
        if (method == 3) go to 140
        !-----------------------------------------------------------------------
        ! Get X-vector and leave subroutine.
        !-----------------------------------------------------------------------
        290 continue
        do i = 1, nr
            x(lib(i)) = b(i)
        end do
        x(nr + nc + 1) = xopt

        ! Optionally restore the original A, B and C arrays.
        if (integration_id_list(3) == 1) go to 340
        k = 0
        l = nr * nc
        do j = 1, nc
            l = l + 1
            c(j) = d(l)
            do i = 1, nr
                k = k + 1
                a(i, j) = d(k)
            end do
        end do
        do i = 1, nr
            l = l + 1
            b(i) = d(l)
        end do
        340 continue
        return
    end

end module m_qslp
