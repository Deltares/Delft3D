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
module m_iblbal
    use m_waq_precision

    implicit none

contains


    subroutine iblbal(ntyp_m, ntyp_a, algtyp, ipoint)

        use bloom_data_mass_balance
        use m_real_array_indices

        implicit none
        !
        !     Function : set common CBLBAL communication with balance routines
        !     Jos van Gils, May 2011: bug fix for N-fixers and heterotrophs
        !
        integer(kind = int_wp) :: ntyp_m              ! Max number of types
        integer(kind = int_wp) :: ntyp_a              ! Actual number of types
        real(kind = real_wp) :: algtyp(0:20, ntyp_m) ! Characteristics per algae type
        integer(kind = int_wp) :: ipoint(ntyp_a)      ! pointers to bloom algea concentration array

        integer(kind = int_wp) :: ialg
        !                     index  4 is NC-ratio
        !                     index  5 is PC-ratio
        !                     index 16 is NC-ratio detritus uptake
        !                     index 17 is PC-ratio detritus uptake
        !                     index 18 is NC-ratio N fixers
        !
        ntypa2 = ntyp_a
        do ialg = 1, ntyp_a
            iblsub(ialg) = ipoint(ialg) - iconc + 1
            ncralg(ialg) = max(algtyp(4, ialg), 0.0) + max(algtyp(16, ialg), 0.0) + max(algtyp(18, ialg), 0.0)
            pcralg(ialg) = max(algtyp(5, ialg), 0.0) + max(algtyp(17, ialg), 0.0)
        enddo
        !
        return
    end

end module m_iblbal
