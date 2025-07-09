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
module m_exclud
    use m_waq_precision

    implicit none

contains


    !  *********************************************************************
    !  * SUBROUTINE EXCLUD TO DETERMINE SPECIES PERMITTED IN EACH INTERVAL *
    !  *********************************************************************

    subroutine exclud (inow, linf, irs)

        use bloom_data_dim
        use bloom_data_size
        use bloom_data_matrix
        use bloom_data_phyt

        implicit none

        integer(kind = int_wp) :: ntypes (ms), irs(3), i, inow, k, nexclu, notprs, linf, iform
        save ntypes
        data nexclu /0/

        ! If the subroutine is called for the first time, compute and store
        ! the number of types in each phytoplankton group.
        nexclu = nexclu + 1
        if (nexclu == 1) then
            do i = 1, nuecog
                ntypes(i) = it2(i, 2) - it2(i, 1) + 1
            end do
        end if

        !  If a species is not permitted in a feasibility interval,
        !  put 1.0 in the exclusion row of matrix A.
        !  If no species is permitted in interval INOW, exit with LINF = 2
        do k = 1, nuspec
            if(aco(inow, k) < 1.0d-6) go to 30
        end do
        linf = 2
        return

        !  Determine species permitted in interval INOW
        30 continue

        ! Correct for species with a positive mortality constraint, that are
        ! no longer permitted in interval INOW: allow all types in this interval,
        ! but limit their biomasses requiring that the B values for the growth
        ! and mortality constraint are the same.
        if (lmorch == 0) then
            do k = 1, nuspec
                a(nuexro, k) = aco(inow, k)
            end do
        else

            ! Use ACO (INOW,K) if the num_layers_grid of SOME type of species I is not yet
            ! exceeded, or if the mortality constraint is 0.0: nothing to conserve.
            ! Otherwise allow EACH type of I, but make the growth constraint equal
            ! to the mortality constraint.
            ! Note: make a copy of this adjusted exclusion row in ACO to record
            ! all types premitted in any interval. This information is used to
            ! determine, whether or not simplex should be called AFTER infeasible
            ! intervals have been detected.
            do i = 1, nuecog
                notprs = 0
                do k = it2(i, 1), it2(i, 2)
                    if (aco(inow, k) > 0.0) notprs = notprs + 1
                end do
                if (notprs < ntypes(i) .or. b(nuexro + nuecog + i) < 1.d-6) then
                    do k = it2(i, 1), it2(i, 2)
                        a(nuexro, k) = aco(inow, k)
                    end do
                else
                    do k = it2(i, 1), it2(i, 2)
                        aco(inow, k) = 0.0
                        a(nuexro, k) = 0.0
                    end do
                    b(nuexro + i) = b(nuexro + nuecog + i)
                end if
            end do
        end if

        !  Exit if the previous interval was feasible (IRS(2) = 0)).
        !  Exit if the previous interval was infeasible due to a mortality
        !  contstraint: call simplex for the next interval, it might be
        !  feasible.
        if (irs(2) == 0) return
        if (lmorch == 1 .and. irs(3) > nuexro + nuecog) then
            linf = 0
            return
        end if

        !  If interval IFORM was infeasible and if no new types
        !  are permitted in INOW,
        !  INOW must be infeasible too: exit with LINF = 1
        linf = 1
        iform = inow - 1
        do k = 1, nuspec
            if (aco(inow, k) < aco(iform, k)) then
                linf = 0
                return
            end if
        end do
        return
    end

end module m_exclud
