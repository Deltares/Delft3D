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
module m_bvect
    use m_waq_precision

    implicit none

contains


    !  *********************************************************************
    !  *  SUBROUTINE TO SET THE MORTALITY CONSTRAINTS INTO THE B-VECTOR    *
    !  *********************************************************************

    subroutine bvect(x, xdef)

        use bloom_data_dim
        use bloom_data_matrix
        use bloom_data_phyt

        implicit none

        real(kind = dp) :: x(*)
        real(kind = dp) :: b2(ms)
        real(kind = dp) :: xdef(*)
        real(kind = dp) :: sumsp
        real(kind = dp) :: dmax1
        integer(kind = int_wp) :: i, i1, k, k1, l1, l2

        ! To tell Bloom how much biomass of the living phytoplankton
        ! species is left at the end of the time-step, the 'minimum
        ! biomass' of each species is set in the B-vector. This value is used
        ! as the mortality constraint (the minimum biomass to be returned
        ! by simplex).
        ! The new, minimum biomass levels are also stored in the original
        ! XDEF-vector. This is to enable the program to deal with infeasible
        ! solutions for example due to light limitation.

        i1 = 0
        k1 = nurows
        do i = 1, nuecog
            sumsp = 0.0
            l1 = it2(i, 1)
            l2 = it2(i, 2)
            do k = l1, l2
                i1 = i1 + 1
                k1 = k1 + 1
                sumsp = sumsp + x(i1)
                xdef(k1) = x(i1)
            end do
            b2(i) = dmax1(sumsp, 0.0d0)
        end do

        i1 = nuexro + nuecog
        do i = 1, nuecog
            i1 = i1 + 1
            b(i1) = b2(i)
        end do

        return
    end

end module m_bvect
