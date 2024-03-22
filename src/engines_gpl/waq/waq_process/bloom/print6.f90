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
module m_print6
    use m_waq_precision

    implicit none

contains


    !
    !  *********************************************************************
    !  *   SUBROUTINE TO PRINT SOLUTIONS FOR ALL FEASIBILITY INTERVALS     *
    !  *         DETERMINE AND RECORD THE MAXIMUM SOLUTION                 *
    !  *********************************************************************
    !
    subroutine print6(bio, biomax, x, xdef, inow, jnow, linf, irs, int, nin, nonuni, nonun, numuni, numun, lib)

        use bloom_data_dim
        use bloom_data_size
        use bloom_data_matrix
        use bloom_data_io
        use bloom_data_phyt
        use bloom_data_sumou

        implicit none

        integer(kind = int_wp) :: jt(mt), nonuni(*), nonun(*), irs(*), lib(*)
        real(kind = dp) :: x(*), xdef(*), bio(*), xopt, biomax, dbio
        integer(kind = int_wp) :: npause = 0
        integer(kind = int_wp) :: ii, ii1, ii2, ii2max, inow, int, jjj, jnow
        integer(kind = int_wp) :: k, klx, linf, nin, numun, numuni

        save

        !  Initialize XOPT if this is the first interval for a time period.
        !  If DUMP is specified, then print solution for interval JNOW.
        if (inow == 1) xopt = 0.0d0
        if (idump == 1) then
            if (inow == 1 .or. npause >= 20) then
                npause = 0
            end if
            write (outdbg, 10) jnow, (b(ii), ii = nufili, nuabco)
            10    format (30X, 'Interval ', I2, /, 2X, 'Extinction limits', 3X, 2(3X, F8.4))
            klx = 0
            do jjj = 1, nuspec
                jt(jjj) = 0
                if (a(nuexro, jjj) > 1.0 - 1.0d-6) cycle
                klx = klx + 1
                jt(klx) = jjj
            end do
            write (outdbg, 30) (jt(jjj), jjj = 1, klx)
            30    format(2X, 'Types permitted', 4X, 20I4)
            write (outdbg, 31) (spname(jt(jjj)), jjj = 1, klx)
            31    format(2X, 'Types permitted', 4X, 30(A8, 1X))
        end if

        !  Check for feasibility of interval JNOW.
        !  if LINF ne 0, exit after increasing NIN with 1
        if (linf == 0) go to 180
        if (inow == 1 .and. lmorch == 0) go to 50
        if (idump == 0) go to 170
        50 npause = npause + 5
        write (outdbg, 60) jnow
        60 format ('  Error message issued for interval ', I2, ':')
        go to (70, 90, 110, 130, 150), irs(2)
        70 write (outdbg, 80)
        80 format('  Solution is feasible, but not yet optimal')
        go to 170
        90 write (outdbg, 100)
        100 format('  A feasible solution in not yet obtained')
        go to 170
        110 write (outdbg, 120)
        120 format('  A finite solution does not exist, solution is unbounded')
        go to 170
        130 write (outdbg, 140) cnames(irs(3))
        140 format('  A feasible solution does not exist due to constraint ', A16)
        go to 170
        150 write (outdbg, 160)
        160 format('  A finite solution can not be found', /, '  all elements of a prospective pivot column are zero')
        170 nin = nin + 1
        return

        !  Solution for interval JNOW is feasible.
        !  Determine maximum biomass and record in BIO(1)
        180 continue
        bio(1) = biomax

        !  Print solution for interval JNOW, if DUMP was specified.
        if (idump == 0) go to 290
        npause = npause + 10
        write (outdbg, 190) (x(ii), ii = 1, nunuco)
        190 format (2X, 'Nutrient Slacks', 2X, 6(F8.2, 2X))
        write (outdbg, 200) (x(ii), ii = nufili, nuabco)
        200 format (4X, 'Energy Slacks', 2X, 2(F8.2, 2X))

        !  Print slacks for (optional) growth constraints.
        if (lgroch == 0) go to 250
        ii1 = nuexro - 4
        ii2 = ii1 + 5
        ii2max = nuexro + nuecog
        210 ii1 = ii1 + 5
        ii2 = ii2 + 5
        ii2 = min0(ii2, ii2max)
        write (outdbg, 220) (x(ii), ii = ii1, ii2)
        220 format (4X, 'Growth slacks', 2X, 10(F8.2, 2X))
        if (ii2 < ii2max) go to 210

        !  Print slacks for (optional) mortality constraints.
        if (lmorch == 0) go to 250
        ii1 = nuexro + nuecog - 4
        ii2 = ii1 + 5
        ii2max = nuexro + 2 * nuecog
        230 ii1 = ii1 + 5
        ii2 = ii2 + 5
        ii2 = min0(ii2, ii2max)
        write (outdbg, 240) (x(ii), ii = ii1, ii2)
        240 format (3x, 'Mortal. slacks', 2x, 10(f8.2, 2x))
        if (ii2 < ii2max) go to 230

        ! Print type biomasses and the optimum of the solution.
        250 continue
        ii1 = nuspe1 - 5
        ii2 = nuspe1 - 1
        260 ii1 = ii1 + 5
        ii2 = ii2 + 5
        ii2 = min0(ii2, nucols)
        write (outdbg, 270) (x(ii), ii = ii1, ii2)
        270 format (2x, 'Types   ', 5(f8.2, 2x))
        if (ii2 < nucols) go to 260
        write (outdbg, 280) biomax, x(nucols + 1)
        280 format (2x, 'Total biomass', 2x, f8.2, 2x, 'Optimum', 2x, f8.2)
        290 continue

        !  Compare optimum solution of JNOW to the absolute optimum of ALL
        !  previous intervals recorded in XOPT, and exit if XOPT is larger.
        dbio = x(nucols + 1) - xopt
        if (inow == 1) go to 310
        if (dbio < -1.0d-6) return
        if (dbio >  1.0d-6) go to 310

        !  BIO(1) is equal to BIO(2):
        !  two intervals have (approximately) the same total biomass.
        !  Record solution in XST, maximum in BIOST,
        !  and the interval number in INTST.
        lst = 1
        biost = bio(1)
        xopt = x(nucols + 1)
        xdef(nucols + 2) = bio(2)
        intst = jnow
        do k = 1, nucols + 1
            xst(k) = x(k)
        end do
        return

        !  BIO(1) is larger than BIO(2):
        !  Record solution in XDEF, maximum in BIO(2), the number of types
        !  with 0.0 reduced cost in NUMUN and the types with reduced costs
        !  of 0.0 in NONUM(20). Put the interval number in INT.
        310 continue
        lst = 0
        bio(2) = bio(1)
        xopt = x(nucols + 1)
        xdef(nucols + 2) = bio(2)
        int = jnow
        do k = 1, nucols + 1
            xdef(k) = x(k)
        end do

        ! Update 1.2: removed storage limiting factors in ISPLIM.
        ! Implemented new agorithm in PRINT6
        numun = numuni
        if (numun == 0) return
        do k = 1, numun
            nonun(k) = nonuni(k)
        end do
        return
    end

end module m_print6
