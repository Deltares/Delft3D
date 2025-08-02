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
module m_set_effi
    use m_waq_precision

    implicit none

contains


    subroutine set_effi(temper, radiat, ext, depthw, daylen, &
            id)
        !>\file
        !>       calculate and store efficiency for all species

        !     modules

        use m_bloom_3dl
        use m_natmor
        use m_maxprd
        use      bloom_data_3dl   ! data and routine for 3D light approach

        use bloom_data_dim
        use bloom_data_size
        use bloom_data_arran
        use bloom_data_phyt
        use bloom_data_putin

        implicit none

        !     arguments

        real(kind = real_wp) :: temper     ! input , temperature
        real(kind = real_wp) :: radiat     ! input , radiation
        real(kind = real_wp) :: ext        ! input , total extinction
        real(kind = real_wp) :: depthw     ! input , depth of the layer
        real(kind = real_wp) :: daylen     ! input , daylength in hours
        integer(kind = int_wp) :: id         ! input , weeknumber

        !     local decalarations

        real(kind = dp) :: alpha      ! reflection factor
        real(kind = dp) :: temp       ! temperature
        real(kind = dp) :: csol       ! radiation
        real(kind = dp) :: dsol       ! radiation
        real(kind = dp) :: dep        ! depth
        real(kind = dp) :: exttot     ! total extinction
        real(kind = dp) :: day        ! daylength in hours
        real(kind = dp) :: deat       ! DEAT
        real(kind = dp) :: tcorr      ! TCORR
        real(kind = dp) :: surf_typ   ! scaled, converted and corrected radiation for a type
        integer(kind = int_wp) :: igroup     ! index number of BLOOM algae group
        integer(kind = int_wp) :: itype      ! index number of BLOOM algae type
        real(kind = dp) :: pmax20(mt), sdmixn(mt)

        dep = depthw
        exttot = ext
        temp = temper
        csol = solaco * radiat
        day = daylen
        deat = 0d0
        call natmor (deat, temp)
        do itype = 1, ntyp_3dl
            if (sdmix(itype) < 0.0) then
                sdmixn(itype) = 1.0d0 + sdmix(itype)
                !           dmix(k) = abs(sdmix(itype)) * dep
            else
                sdmixn(itype) = 0.0d0
            endif
        enddo

        call maxprd (tefcur)
        do itype = 1, ntyp_3dl
            pmax20(itype) = pmax(itype)
        enddo
        call maxprd (temp)

        dsol = 1428.57 * csol
        do igroup = 1, ngro_3dl
            do itype = it2(igroup, 1), it2(igroup, 2)
                tcorr = pmax20(itype) / pmax(itype)
                surf_typ = tcorr * dsol * dexp (- exttot * sdmixn(itype) * dep)
                surf_typ = surf_typ / day
                call effilay_3dl(surf_typ, exttot, dep, igroup, itype)
            enddo
        enddo

        return
    end subroutine set_effi

end module m_set_effi
