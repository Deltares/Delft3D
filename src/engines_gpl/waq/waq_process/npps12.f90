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
module m_npps12
    use m_waq_precision

    implicit none

contains


    subroutine npps12     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !
        !*******************************************************************************
        !
        implicit none
        !
        !     type    name         i/o description
        !
        real(kind = real_wp) :: process_space_real(*)     !i/o process manager system array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! o  array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(15) ! i  array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(15) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! i  number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! i  number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! i  nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! i  nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(15)   !    local work array for the pointering
        integer(kind = int_wp) :: iseg        !    local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !
        !     type    name         i/o description                                        unit
        !
        real(kind = real_wp) :: nh4         ! i  depth of segment                                   (g/m3)
        real(kind = real_wp) :: po4         ! i  total depth water column                           (g/m3)
        real(kind = real_wp) :: dmn1s1      ! i  first mineralisation flux N in layer 1             (g/m3/d)
        real(kind = real_wp) :: dmn2s1      ! i  secnd mineralisation flux N in layer 1             (g/m3/d)
        real(kind = real_wp) :: dmn1s2      ! i  first mineralisation flux N in layer 2             (g/m3/d)
        real(kind = real_wp) :: dmn2s2      ! i  secnd mineralisation flux N in layer 2             (g/m3/d)
        real(kind = real_wp) :: dmp1s1      ! i  first mineralisation flux P in layer 1             (g/m3/d)
        real(kind = real_wp) :: dmp2s1      ! i  secnd mineralisation flux P in layer 1             (g/m3/d)
        real(kind = real_wp) :: dmp1s2      ! i  first mineralisation flux P in layer 2             (g/m3/d)
        real(kind = real_wp) :: dmp2s2      ! i  secnd mineralisation flux P in layer 2             (g/m3/d)
        real(kind = real_wp) :: dlen        ! i  diffusion length                                   (m)
        real(kind = real_wp) :: dcoef       ! i  diffusion coefficient                              (m2/d)
        real(kind = real_wp) :: depth       ! i  depth                                              (m)

        real(kind = real_wp) :: nh4s12      ! o  estimated NH4 in pore water                        (m)
        real(kind = real_wp) :: po4s12      ! o  estimated PO4 in pore water                        (m)

        ! local

        integer(kind = int_wp) :: ikmrk1      !    first attribute
        integer(kind = int_wp) :: ikmrk2      !    second attribute
        real(kind = real_wp) :: num_fluxes       !    total flux
        real(kind = real_wp) :: pflux       !    total flux

        ipnt = ipoint

        do iseg = 1, num_cells
            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            if (ikmrk1==1) then
                call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                if (ikmrk2==0.or.ikmrk2==3) then

                    nh4 = max(process_space_real(ipnt(1)), 0.0)
                    po4 = max(process_space_real(ipnt(2)), 0.0)
                    dmn1s1 = process_space_real(ipnt(3))
                    dmn2s1 = process_space_real(ipnt(4))
                    dmn1s2 = process_space_real(ipnt(5))
                    dmn2s2 = process_space_real(ipnt(6))
                    dmp1s1 = process_space_real(ipnt(7))
                    dmp2s1 = process_space_real(ipnt(8))
                    dmp1s2 = process_space_real(ipnt(9))
                    dmp2s2 = process_space_real(ipnt(10))
                    dlen = process_space_real(ipnt(11))
                    dcoef = process_space_real(ipnt(12))
                    depth = process_space_real(ipnt(13))

                    num_fluxes = (dmn1s1 + dmn2s1 + dmn1s2 + dmn2s2) * depth   ! g/m2/d
                    pflux = (dmp1s1 + dmp2s1 + dmp1s2 + dmp2s2) * depth
                    nh4s12 = num_fluxes * dlen / dcoef + nh4
                    po4s12 = pflux * dlen / dcoef + po4

                    process_space_real(ipnt(14)) = nh4s12
                    process_space_real(ipnt(15)) = po4s12

                endif
            endif
            ipnt = ipnt + increm

        end do

        return
    end subroutine

end module m_npps12
