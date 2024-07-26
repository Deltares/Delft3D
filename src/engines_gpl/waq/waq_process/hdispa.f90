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
module m_hdispa
    use m_waq_precision

    implicit none

contains


    subroutine hdispa     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Facilitate variable (2D) horizontal dispersion defined per segment (instead of per exchanges as in #4)

        !
        !*******************************************************************************
        !
        implicit none
        !
        !     type    name         i/o description
        !
        real(kind = real_wp) :: process_space_real(*)     !i/o process manager system array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! o  array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(2) ! i  array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(2) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! i  number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! i  number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
        integer(kind = int_wp) :: num_exchanges_v_dir        ! i  nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(2)   !    local work array for the pointering
        integer(kind = int_wp) :: iq          !    local loop counter for exchanges
        integer(kind = int_wp) :: iseg1       !    segment number from
        integer(kind = int_wp) :: iseg2       !    segment number to

        ipnt = ipoint

        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir

            ! input on segments

            iseg1 = iexpnt(1, iq)
            iseg2 = iexpnt(2, iq)

            ! set output

            if (iseg1 > 0 .and. iseg2 > 0) then
                ! if both are internal segments use the minimum value of both segments
                process_space_real(ipnt(2)) = min(process_space_real(ipnt(1) + (iseg1 - 1) * increm(1)), &
                        process_space_real(ipnt(1) + (iseg2 - 1) * increm(1)))
            else if (iseg1 > 0) then
                ! if only 'from' is an internal segment, use this one
                process_space_real(ipnt(2)) = process_space_real(ipnt(1) + (iseg1 - 1) * increm(1))
            else if (iseg2 > 0) then
                ! if only 'to' is an internal segment, use this one
                process_space_real(ipnt(2)) = process_space_real(ipnt(1) + (iseg2 - 1) * increm(1))
            else
                ! no internal node available, probably from zero to zero
                process_space_real(ipnt(2)) = 0.0
            endif

            ! update pointering in process_space_real

            ipnt(2) = ipnt(2) + increm(2)

        enddo

        return
    end

end module m_hdispa
