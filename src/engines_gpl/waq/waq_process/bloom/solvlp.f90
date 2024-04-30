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
module m_solvlp
    use m_waq_precision

    implicit none

contains


    !  *********************************************************************
    !  *    SUBROUTINE FOR SETTING UP, SOLVING AND ARRANGING OUTPUT        *
    !  *                     OF LINEAR PROGRAM.                            *
    !  *********************************************************************
    !
    subroutine solvlp(inow, x, biomax, ier, irs, nonuni, numuni, lib)

        use m_qslp
        use bloom_data_dim
        use bloom_data_size
        use bloom_data_matrix
        use bloom_data_io
        use bloom_data_phyt

        implicit none

        !
        ! This subroutine sets up the A matrix, and B and C vectors for the
        ! linear programming routine QSLP.
        ! A considerable increase in speed is obtained by dropping all
        ! variables and constraints from consideration which are redundant
        ! for a particular extinction interval. These are:
        ! 1.  All types not considered in an interval.
        ! 2.  All constraints with a zero value. (i.e. Mortality constraints).
        ! 3.  All constraints with only zero A coefficients. (i.e. growth
        !     constraints of species not permitted).

        integer(kind = int_wp) :: lsc(ia), lsctmp(ia), integration_id_list(4), irs(3), lib(mx), libtmp(mx), libbas(mx), nonuni(mt)
        integer(kind = int_wp) :: i, j, j1, k, l
        integer(kind = int_wp) :: index, ier, inow, numuni, nsolve, nustmp, nurtmp, nsolv
        real(kind = dp) :: atmp(ia, mt)
        real(kind = dp) :: btmp(ia)
        real(kind = dp) :: ctmp(mt)
        real(kind = dp) :: xtmp(mx)
        real(kind = dp) :: biomax
        real(kind = dp) :: d(1)
        real(kind = dp) :: p(1)
        real(kind = dp) :: x(mx)
        real(kind = dp) :: sumx
        save    lsc, integration_id_list
        logical :: lbasis(mx)
        data nsolv /0/

        !  Indicate additional run through the subroutine.
        nsolv = nsolv + 1
        if (nsolv > 1) go to 80

        ! Set all constraints to less or equal LSC(I)=-1, except the minimum
        ! energy constraint.
        ! Put the B-value of the exclusion row to 0.0.
        index = 0
        do i = 1, nuexro
            index = index + 1
            lsc(index) = -1
        end do
        lsc(nufili) = 1
        do i = 1, nuecog
            index = index + 1
            lsc(index) = -1
        end do
        do i = 1, nuecog
            index = index + 1
            lsc(index) = 1
        end do
        b(nuexro) = 0.0d0

        !  Set values for maximum number of iterations: integration_id_list(1).
        !  Set number of iterations before checking for numerical errors: integration_id_list(2)
        !  Indicate that the original matrices do not have to be retored by QSLP: integration_id_list(3)
        !  NOTE: change minimum dimension of D to (NR+1)*(NC+1) when integration_id_list(3)
        !  is NOT equal to 1!!
        !  Indicate that the objective is to maximize: integration_id_list(4).
        integration_id_list(1) = 50
        integration_id_list(2) = 10
        integration_id_list(3) = 1
        integration_id_list(4) = 1

        ! Put coefficients for growth constraints into A-matrix.
        k = nuexro
        do i = 1, nuecog
            k = k + 1
            do j = it2(i, 1), it2(i, 2)
                a(k, j) = 1.0
            end do
        end do

        ! Put coefficients for mortality constraints into A-matrix.
        do i = 1, nuecog
            k = k + 1
            do j = it2(i, 1), it2(i, 2)
                a(k, j) = 1.0
            end do
        end do

        ! Determine which species might occur. Copy their C value into CTMP,
        ! count their number and set LBASIS = true.
        80 continue
        nustmp = 0
        do j = 1, nuspec
            if (a(nuexro, j) < 1.0d-6) then
                nustmp = nustmp + 1
                lbasis(nurows + j) = .true.
                ctmp(nustmp) = c(j)
            else
                lbasis(nurows + j) = .false.
            end if
        end do

        ! Determine which constraints might be active.
        !
        ! Bug fix 08/25/2005 by Hans Los
        ! Nutrient contraints should never be dropped when levels become 0.
        ! Hence the first NUNUCO rows are now always considered by the
        ! optimization subroutine
        !
        ! Drop a constraint if either
        ! 1. B(I) = 0.0 or
        ! 2. All A(I,J) coefficients are 0.0 for each J considered.
        ! If a "<" constraint is negative, the problem is infeasible. Skip
        ! calling QSLP and directly set relevant exit values.
        !
        ! Copy relevant part of B vector into BTMP,
        ! count the number of potential constraint and set LBASIS = true.
        ! Set the conversion array LIBBAS to indicate original numbers of
        ! constraints to be considered. Copy relevant entries of LSC.
        !
        ! Always include nutrient constraints
        nurtmp = 0
        do i = 1, nunuco
            nurtmp = nurtmp + 1
            lbasis(i) = .true.
            btmp(nurtmp) = b(i)
            libbas(nurtmp) = i
            lsctmp(nurtmp) = lsc(i)
        end do

        outer : do i = nunuco + 1, nurows
            if (b(i) < -1.0d-12 .and. lsc(i) <= 0) then
                ier = 100
                irs(2) = 4
                irs(3) = i
                go to 138 ! jump outside a loop
            end if
            if (b(i) > 1.0d-12) then
                do j = 1, nuspec
                    if (.not. lbasis (j + nurows)) cycle
                    if (a(i, j) > 1.0d-12) then
                        nurtmp = nurtmp + 1
                        lbasis(i) = .true.
                        btmp(nurtmp) = b(i)
                        libbas(nurtmp) = i
                        lsctmp(nurtmp) = lsc(i)
                        cycle outer
                    end if
                end do
            end if
            lbasis(i) = .false.
        end do outer

        ! Set the conversion array LIBBAS to indicate original numbers
        ! of variables to be considered.
        ! Contruct A-matrix for LP routine.
        j1 = 0
        do j = 1, nuspec
            if (lbasis(nurows + j)) then
                j1 = j1 + 1
                libbas(nurtmp + j1) = nurows + j
                do i = 1, nurtmp
                    atmp(i, j1) = a(libbas(i), j)
                end do
            end if
        end do

        ! If a dump is requested, print the objective function.
        if (idump == 1 .and. inow == 1) write (outdbg, 125) (c(j), j = 1, nuspec)
        125  format (' Objective function of types:', /, 2X, 20(F5.2, 2X))

        !  Call subroutine "QSLP" to solve the linear program by the ordinary
        !  simplex algorithm.
        call qslp(atmp, ia, nurtmp, nustmp, btmp, lsctmp, ctmp, integration_id_list, irs, libtmp, d, mt, xtmp, p, ier)

        !  Put results in appropriate form. Construct X, and LIB as if the
        !  complete problem had been solved by QSLP.
        if (ier == 0) then
            irs(3) = nucols + 1
        else
            if (irs(3) <= mx) then
                irs(3) = libbas(irs(3))
            end if
        end if

        !  Copy or set values for variables and constraints. Note: values
        !  for ">" constraints, which were NOT considered by QSLP, are
        !  incorrect. These values are, however, overriden in the next section.
        k = 0
        do i = 1, nurows
            if (lbasis(i)) then
                k = k + 1
                x(i) = xtmp(k)
                lib(i) = libbas(libtmp(k))
            else
                lib(i) = i
                x(i) = b(i)
            end if
        end do
        do i = nurows + 1, nucols
            if (lbasis(i)) then
                k = k + 1
                x(i) = xtmp(k)
                lib(i) = libbas(libtmp(k))
            else
                lib(i) = i
                x(i) = 0.0d0
            end if
        end do
        x(nucols + 1) = xtmp (nurtmp + nustmp + 1)

        ! Compute slacks for ">" constraints not considered in QSLP.
        ! For the lower extinction contraint, use the information on the
        ! maximum extinction contraint.
        ! Put the mortality constraints not conisered by QSLP equal to the
        ! total biomass of each species.
        ! Note: if a species' growth constraint was not considered, its
        ! total biomass must be 0.0 so we do not have to compute it!
        x(nufili) = b(nuabco) - x(nuabco) - b(nufili)
        if (lmorch /= 1) go to 138
        k = nuexro + nuecog
        do j = 1, nuecog
            k = k + 1
            if (lbasis(k)) cycle
            if (.not. lbasis(nuexro + j)) cycle
            sumx = 0.0
            do l = it2(j, 1), it2(j, 2)
                sumx = sumx + x(nurows + l)
            end do
            x(k) = sumx
        end do
        138  continue
        if (ier /= 0) return

        !  Determine, which species have a reduced cost coefficient of 0.0:
        !  these might have replaced one of the species in the optimal solution.
        !  Note: use the resulting CTMP. Thisis only possible when integration_id_list(3) = 1!
        j1 = 0
        numuni = 0
        do j = 1, nuspec
            nonuni(j) = 0
            if (.not. lbasis (j + nurows)) cycle
            j1 = j1 + 1
            if (x(j + nurows) > 1.0d-12) cycle
            if (ctmp(j1) > 1.0d-12) cycle
            numuni = numuni + 1
            nonuni(numuni) = j
        end do

        ! Compute maximum biomass of solution and store in BIOMAX.
        ! Note: this is NOT equal to X(NUCOLS + 1)
        ! when growth rather than biomass is maximized.
        if (lobfun == 1) then
            biomax = 0.0
            do j = 1, nuspec
                biomax = biomax + x (nurows + j)
            end do
        else
            biomax = x(nucols + 1)
        end if
        return
    end

end module m_solvlp
