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
module m_prinsu
    use m_waq_precision

    implicit none

contains


    !  *********************************************************************
    !  *         SUBROUTINE TO PRINT SUMMARIZED SOLUTIONS                  *
    !  *********************************************************************
    !
    subroutine prinsu(x, xeco, bio2, total, ntstot, itnum, ntape)

        use bloom_data_dim
        use bloom_data_size
        use bloom_data_matrix
        use bloom_data_io
        use bloom_data_phyt
        use bloom_data_sumou

        implicit none

        integer(kind = int_wp) :: i, j, k, k1, k2
        integer(kind = int_wp) :: itnum, ntape, ntstot, numlim, ncon
        real(kind = dp) :: bio2, xbio, total, tot2

        real(kind = dp) :: x(*), xeco(*)
        character*8 words(14)
        logical lcon

        !  Calculate totals for species, the total chlorophyll concentration
        !  and record in OUT.
        total = 0.
        do k = 1, nuecog
            tot2 = 0.
            do j = it2(k, 1), it2(k, 2)
                xbio = x(j + nurows)
                tot2 = tot2 + xbio
                if (sdmix (k) < 0.0) cycle
                total = total + xbio / chlr(j)
            end do
            xeco(k) = tot2
        end do

        !  Determine limiting factors and record their names in COUT.
        !  Record in LIMIT in 1,0 notation.
        write (limit, 70) ('0', k = 1, nuabco + 1)
        70 format (9(1x, a1))

        !  Initiate ISPLIM at 0
        do i = 1, nuspec
            isplim(i) = 0
        end do

        ! 1. nutrient constraints.
        k1 = 1
        numlim = 0
        ncon = 0
        do k = 1, nunuco
            ncon = ncon + 1
            if (x(k) > 1.d-4) cycle
            k1 = k1 + 1
            numlim = numlim + 1
            isplim (numlim) = ncon
            limit (2 * k:2 * k) = '1'
        end do

        ! 2. energy constraints.
        k2 = 2 + 2 * nunuco
        do k = nunuco + 1, nuabco
            ncon = ncon + 1
            if (x(k) > 1.d-4) cycle
            numlim = numlim + 1
            isplim (numlim) = ncon
            k1 = k1 + 1
            limit (k2:k2) = '1'
        end do

        !  Increment NCON by 1 to skip exclusion row!
        ncon = ncon + 1

        ! 3. Growth constraints.
        !
        !  Print slacks for (optional) growth constraints.
        !  Note: if both the growth and mortality slack of a phytoplankton
        !  are 0.0, assume that the mortality constraint is the actual
        !  limitation: do not write "GRO" to output files.
        lcon = .false.
        if (lgroch == 0) go to 150
        k2 = 2 * (nuabco - 1) + 2
        do i = 1, nuecog
            ncon = ncon + 1
            if (x(i + nuexro) > 1.d-4) cycle
            if (x(i + nuexro + nuecog) < 1.d-4 .and. lmorch == 1) cycle
            numlim = numlim + 1
            isplim (numlim) = ncon
            if (.not. lcon) then
                k1 = k1 + 1
                limit (k2:k2) = '1'
                lcon = .true.
            end if
        end do

        ! 4. Mortality constraints.
        !  Print slacks for (optional) mortality constraints.
        130 continue
        lcon = .false.
        if (lmorch == 0) go to 150
        k2 = k2 + 2
        do i = 1, nuecog
            ncon = ncon + 1
            if (x(i + nuexro + nuecog) > 1.d-4) cycle
            if (xeco(i) < 1.d-4) cycle
            numlim = numlim + 1
            isplim (numlim) = ncon
            if (.not. lcon) then
                k1 = k1 + 1
                limit (k2:k2) = '1'
                lcon = .true.
            end if
        end do
        150 continue

        return
    end

end module m_prinsu
