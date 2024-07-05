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
module m_fixinf
    use m_waq_precision

    implicit none

contains


    !  Purpose of this module: modify some of the boundary conditions for
    !  infeasible systems. We follow a different approach for different
    !  model versions for reasons to be explained here. Infeasible systems
    !  occur, when a minimum constrain (usually a mortality constraint)
    !  conflicts with a maximum constraint (nutrient; energy). To deal with
    !  this problem we must either lower the value of the mortality
    !  constraint, or increase the available amount of the maximum
    !  constraint, which is violated.
    !
    !  1. Stand alone BLOOM II
    !     a. Steady state computation.
    !        If there is no valid solution, simply put all biomasses to 0.0:
    !        this is obviously the correct steady state solution.
    !     b. Dynamic computation.
    !        Drop the mortality constraint which is violated. Argument:
    !        the boundary conditions are based on measured conditions and
    !        therefore relatively well defined. Little is known about the
    !        mortality of declining species. Therefore drop the moratlity
    !        constraint.
    !   2. BLOOM as a module (DELWAQ; JSBACH)
    !        The mortality is pre-calculated and so are the corresponding
    !        nutrient fluxes. Conflicts between nutrient and mortality
    !        constraints cannot occur in theory (they do occasionally though
    !        due to numerical round-off errors and small time-step
    !        problems). In almost all cases the infeasibility is caused by
    !        a mortality constraint and the energy constraint. Therefore
    !        try to increase the energy consrtaint sufficiently and rerun
    !        the problem. If this won't help, drop the mortality constraint.
    !        Note: this may lead to NEGATIVE production rates as BLOOM
    !        returns a value below the mortality constraint!
    !
    !  *********************************************************************
    !  * SUBROUTINE TO DEAL WITH CASES WHERE ALL INTERVALS ARE INFEASIBLE  *
    !  *********************************************************************

    subroutine fixinf(x, bio, exttot, extb, inhib, ni, irerun, irs, infeas, errind, jkmax, aroot, cdate, swblsa)

        use bloom_data_dim
        use bloom_data_size
        use bloom_data_caldynam
        use bloom_data_io
        use bloom_data_matrix
        use bloom_data_phyt
        use bloom_data_sumou

        implicit none

        real(kind = dp) :: x(*), bio(*), sumnut(mn), aroot(*)
        integer(kind = int_wp) :: irs (*), jkmax(*), swblsa, i, j, k
        character(8) :: cdate
        character(1) :: errind

        integer(kind = int_wp) :: inhib
        integer(kind = int_wp) :: irerun
        integer(kind = int_wp) :: ni
        integer(kind = int_wp) :: index
        integer(kind = int_wp), save :: irmax = 0 ! Hack to solve a problem with it being non-initialised in some circumstances at line 166
        integer(kind = int_wp) :: mof
        integer(kind = int_wp) :: infeas
        real(kind = dp) :: exttot
        real(kind = dp) :: extb
        real(kind = dp) :: extrem
        real(kind = dp) :: biomax
        real(kind = dp) :: xdefk
        real(kind = dp) :: xi

        ! Set flag for non-unique solutions (LST) to 0.
        lst = 0

        ! ----------------------------------------------------------------------
        ! Start of steady state BLOOM section.
        ! In a run WITHOUT mortality constraints, set all biomasses to zero,
        ! all nutrient slacks to the total available concentrations and the
        ! total extinction to the background extinction.
        ! Note: this is also the final solution in dynamic runs when everything
        ! else fails.
        if (lmorch == 1) go to 40
        10 continue
        do j = 1, nunuco
            x(j) = b(j)
        end do
        x(nuabco) = 0.0
        if (inhib == 1) then
            x(nufili) = 0.0
        else
            x(nufili) = 1.0
        end if
        do j = nurows, nucols
            x(j) = 0.0
        end do
        bio(2) = -1.0
        x(nucols + 2) = bio(2)
        exttot = extb
        irerun = 0
        return

        ! ----------------------------------------------------------------------
        ! The ultimate solution: put all biomasses to 0!
        ! Was the problem rerun already? Is there still hope for a neat
        ! solution?
        40 continue
        if (irerun == 3) then
            write (outdbg, 50) cdate
            50    format (/, ' ', ' !!! SEVERE ERROR MESSAGE time ', A8, '.')
            write (outdbg, 60) irs(2)
            60    format ('  Problem remains infeasible due to constraint ', I2, &
                    '.', /, '  All biomasses set to 0.0.')
            errind = '!'
            go to 10
        end if

        ! ----------------------------------------------------------------------
        ! If there are no intervals at all, or if the infeasibility is not due
        ! to a mortality constraint, set all species at their mortality
        ! constraint.
        if (ni == 0 .or. irs(3) <= nuexro + nuecog) go to 130

        ! ----------------------------------------------------------------------
        ! Problem is infeasible due to a mortality constraint.
        !
        ! In coupled model versions, it is almost always sufficient to increase
        ! the energy constraint. Compute the minimum extinction value that could
        ! make the problem feasible. This is the background extinction EXTB
        ! (which includes detritus) plus the extinction of all phytoplankton
        ! species at their minimum level. If the old Kmax is high enough, the
        ! violation must be due to a nutrient constraint afterall so drop the
        ! mortality constraint. Rerun.
        ! Note: we have assumed that the type whose objective function is the
        ! highest of a species also has the highest specific extinction
        ! coefficient. This is quite logical, and therefore UNCHECKED!
        !
        ! Note: Restore the original growth constraint vector.
        index = irs(3) - nuexro - nuecog
        if (index > nuecog) go to 130
        if (irerun /= 2) irmax = 0
        irmax = irmax + 1
        if (irmax > nuspec) go to 130
        irerun = 2
        errind = '*'
        extrem = extb
        if (idump == 1) write (outdbg, 90) cdate
        90    format (/, ' ', ' *** Warning message for time ', A8, '.')
        do k = 1, nuecog
            b(k + nuexro) = bgro(k)
            extrem = extrem + b(nuexro + nuecog + k) * a(nuabco, jkmax(k))
        end do

        if (swblsa == 1) then
            b(irs(3)) = 0.0
            if (idump == 1) write (outdbg, 110) grname (index)
            110          format (' Mortality constraint of species ', A8, ' is violated.', /, ' This constraint is dropped.')
        else
            do k = it2(index, 1), it2(index, 2)
                if (aroot(2 * k) < extrem) then
                    aroot(2 * k) = extrem
                    if (idump == 1) write (outdbg, 120) grname(index)
                    120              format (' Mortality constraint of species ', A8, 'is violated.', /, ' KMAX NOW set above mortality constaint.')
                else
                    b(irs(3)) = 0.0
                    if (idump == 1) write (outdbg, 110) grname(index)
                end if
            end do
        end if
        return

        ! ----------------------------------------------------------------------
        ! In a final attempt to fix the problem set all biomasses to their
        ! minimum permissible values: solve the differential equation for
        ! the mortality. This solution has been optained elsewhere; the result
        ! must have been stored in the X-vector. It is computed as:
        !
        !     xdefj = x(nurows + j) * dexp(-tstep * rmort(j))
        !
        ! Note: the total extinction will be computed in subroutine BLOOM no
        ! matter wheter the solution was feasible.
        ! Get minimum (=actual) biomasses from X-vector.
        ! Compute nutrients in phytoplankton and (in a non-dynamic run) in
        ! detritus. Compute the total biomass. Set slacks for mortality
        ! constraints of all species, whose biomasses are set equal to the
        ! mortality constraint.
        130 continue
        do i = 1, nunuco
            sumnut(i) = 0.0
        end do
        biomax = 0.0
        do j = 1, nuecog
            mof = 0
            do k = it2(j, 1), it2(j, 2)
                xdefk = x(nurows + k)
                isplim(k) = 0
                if (xdefk < 1.d-6) cycle
                mof = mof + 1
                isplim(k) = nurows - nuecog + j
                biomax = biomax + xdefk
                do i = 1, nunuco
                    sumnut(i) = sumnut(i) + a(i, k) * xdefk
                end do
            end do
            if (mof > 0) x(nurows - nuecog + j) = 0.0d1
        end do
        bio(2) = biomax
        x(nucols + 2) = bio(2)

        !  Compute the nutrient slacks. Check, whether they are positive.
        !  If not, perform various actions depending on the kind of run:
        !
        !  1. In a dynamic run, declare the problem to be infeasible.
        !     This situation should NEVER occur!
        !  Otherwise,
        !  2. Set all species equal to their mortality constraint, if
        !     no extinction intervals exist,
        !  3. Release the mortality constraints and re-run the problem if
        !     valid extinction intervals do exist.
        do i = 1, nunuco
            xi = b(i) - sumnut(i)
            if (xi < 0.0) then
                if (swblsa /= 1) then
                    if (idump == 1) then
                        write (outdbg, 50) cdate
                        write (outdbg, 180) cstra(i)
                        180             format (' One of the mortality constraints violates the ', A8, ' constraint.', /, ' Problem is infeasible.')
                        errind = '!'
                    end if
                    go to 10
                else
                    if (ni == 0) then
                        xi = 0.0
                        if (idump == 1) then
                            write (outdbg, 90) cdate
                            write (outdbg, 190) cstra(i)
                            190                 format (' One of the mortality constraints violates the ', A8, ' constraint.', /, ' Negative concentrations ', &
                                    'are tolerated.', /, ' All species set to their mortality constraints.')
                            errind = '*'
                        end if
                    else
                        if (idump == 1) then
                            write (outdbg, 90) cdate
                            write (outdbg, 200) cstra(i)
                            200               format (' One of the mortality constraints violates the ', A8, ' constraint.', /, &
                                    ' All mortality constaints will be released.')
                            errind = '*'
                        end if
                        do k = 1, nuecog
                            b(k + nuexro + nuecog) = 0.0
                            b(k + nuexro) = bgro(k)
                        end do
                        irerun = 3
                        return
                    end if
                end if
            end if
            x(i) = xi
        end do

        if (inhib == 1) then
            x(nufili) = 0.0d0
        else
            x(nufili) = 1.0
        end if
        if (ni == 0) then
            x(nuabco) = 0.0d0
        else
            x(nuabco) = 1.0
        end if
        irerun = 0
        infeas = 0
        return
    end

end module m_fixinf
