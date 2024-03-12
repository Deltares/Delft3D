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
module m_blclmort
    use m_waq_precision

    implicit none

contains


    !     This file contains various subroutines concerning the setting of BLOOM parameters

    subroutine blclst(mrtm1, mrtm2, mrtb1, mrtb2, ntyp_a, cl)

        use bloom_data_size

        implicit none

        integer(kind = int_wp) :: ntyp_a              ! Actual number of algae types
        real(kind = real_wp) :: mrtm1(ntyp_a)       ! Original mortality rates
        real(kind = real_wp) :: mrtm2(ntyp_a)       ! M2 mort rate coeff
        real(kind = real_wp) :: mrtb1(ntyp_a)       ! B1 mort rate sal stress coeff
        real(kind = real_wp) :: mrtb2(ntyp_a)       ! B2 mort rate sal stress coeff
        real(kind = real_wp) :: cl                  ! Chlorine concentration
        integer(kind = int_wp) :: ialg                ! Counter over algae types

        !     Loop over algae types
        do ialg = 1, ntyp_a
            !        Store the original value
            mrtm1(ialg) = rmort1(ialg)
            !        Salinity dep. mortality ??
            if (mrtm2(ialg)>0.) then
                cl = min(cl, 35000.0)
                rmort1(ialg) = (mrtm2(ialg) - mrtm1(ialg)) / (1.0 + exp(mrtb1(ialg) * (cl - mrtb2(ialg)))) + mrtm1(ialg)
            endif
        enddo

        return
    end


    subroutine blclrs(mrtm1, ntyp_a)

        use bloom_data_size

        implicit none

        integer(kind = int_wp) :: ntyp_a              ! Actual number of algae types
        real(kind = real_wp) :: mrtm1(ntyp_a)       ! Original mortality rates
        integer(kind = int_wp) :: ialg                ! Counter over algae types

        !     Loop over algae types
        do ialg = 1, ntyp_a
            !        Store the original value
            rmort1(ialg) = mrtm1(ialg)
        end do

        return
    end


    subroutine blsppm(ialg, ppmax)

        use bloom_data_size

        implicit none

        integer(kind = int_wp) :: ialg                ! index alg involved
        real(kind = real_wp) :: ppmax               ! PPMAX value to be set

        pmax1(ialg) = ppmax

        return
    end


    subroutine blssdm (ialg, sdmixn)

        use bloom_data_size

        implicit none

        integer(kind = int_wp) :: ialg                ! index alg involved
        real(kind = real_wp) :: sdmixn              ! SDMIX value to be set

        sdmix(ialg) = sdmixn

        return
    end


    subroutine blsaef(igroup, effin)

        use bloom_data_size
        use bloom_data_phyt

        implicit none

        integer(kind = int_wp) :: igroup              ! Index of group involved
        integer(kind = int_wp) :: itype               ! Index of algae involved
        real(kind = real_wp) :: effin               ! EFFI value to be set

        do itype = it2(igroup, 1), it2(igroup, 2)
            aveffi(itype) = effin
        end do

        return
    end

end module m_blclmort
