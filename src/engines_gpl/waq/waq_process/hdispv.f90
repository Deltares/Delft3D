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
module m_hdispv
    use m_waq_precision

    implicit none

contains


    subroutine hdispv     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       (2D) Horizontal dispersion as velocity dependent reprofunction

        !
        !*******************************************************************************
        !
        implicit none
        !
        !     type    name         i/o description
        !
        real(kind = real_wp) :: process_space_real(*)     !i/o process manager system array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! o  array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(12) ! i  array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(12) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! i  number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! i  number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
        integer(kind = int_wp) :: num_exchanges_v_dir        ! i  nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(12)   !    local work array for the pointering
        integer(kind = int_wp) :: iq          !    local loop counter for exchanges
        integer(kind = int_wp) :: iseg1       !    segment number from
        integer(kind = int_wp) :: iseg2       !    segment number to
        real(kind = real_wp) :: velocity    !    velocity
        real(kind = real_wp) :: depth1      !    depth from segment
        real(kind = real_wp) :: depth2      !    depth to segment
        !
        !*******************************************************************************
        !
        !     type    name         i/o description                                        unit
        !
        real(kind = real_wp) :: dfact_a     ! i  dispersion coefficient at low velocities           (m2/s)
        real(kind = real_wp) :: dfact_b     ! i  dispersion coefficient at low velocities           (m2/s)
        real(kind = real_wp) :: dfact_c     ! i  dispersion coefficient at low velocities           (m2/s)
        real(kind = real_wp) :: dback       ! i  dispersion coefficient at low velocities           (m2/s)
        real(kind = real_wp) :: dmin        ! i  dispersion coefficient at low velocities           (m2/s)
        real(kind = real_wp) :: dmax        ! i  dispersion coefficient at high velocities          (m2/s)
        real(kind = real_wp) :: depth       ! i  segment depth                                      (m)
        real(kind = real_wp) :: xarea       ! i  exchange area                                      (m2)
        real(kind = real_wp) :: flow        ! i  flow rate                                          (m3/s)
        real(kind = real_wp) :: horzdispv   ! o  variable horizontal dispersion                     (m2/s)
        !
        !*******************************************************************************

        ipnt = ipoint

        do iq = 1, num_exchanges_u_dir + num_exchanges_v_dir

            ! input on segments

            iseg1 = iexpnt(1, iq)
            if (iseg1 <= 0) iseg1 = iexpnt(2, iq)
            if (iseg1 <= 0) iseg1 = 1
            iseg2 = iexpnt(2, iq)
            if (iseg2 <= 0) iseg2 = iexpnt(1, iq)
            if (iseg2 <= 0) iseg2 = 1

            dfact_a = process_space_real(ipnt(1) + (iseg1 - 1) * increm(1))
            dfact_b = process_space_real(ipnt(2) + (iseg1 - 1) * increm(2))
            dfact_c = process_space_real(ipnt(3) + (iseg1 - 1) * increm(3))
            dback = process_space_real(ipnt(4) + (iseg1 - 1) * increm(4))
            dmin = process_space_real(ipnt(5) + (iseg1 - 1) * increm(5))
            dmax = process_space_real(ipnt(6) + (iseg1 - 1) * increm(6))
            depth1 = process_space_real(ipnt(7) + (iseg1 - 1) * increm(7))
            depth2 = process_space_real(ipnt(7) + (iseg2 - 1) * increm(7))
            depth = (depth1 + depth2) / 2.

            ! input on exchange

            xarea = process_space_real(ipnt(8))
            flow = process_space_real(ipnt(9))

            ! calculate velocity

            if (xarea < 1e-10) then
                velocity = 0.0
            else
                velocity = abs(flow / xarea)
            endif

            ! calculate dispersion

            horzdispv = dfact_a * (velocity**dfact_b) * (depth**dfact_c) + dback
            horzdispv = max(horzdispv, dmin)
            horzdispv = min(horzdispv, dmax)

            ! set output

            if (iq <= num_exchanges_u_dir) then
                process_space_real(ipnt(10) + (iseg1 - 1) * increm(10)) = horzdispv
                process_space_real(ipnt(10) + (iseg2 - 1) * increm(10)) = horzdispv
            else
                process_space_real(ipnt(11) + (iseg1 - 1) * increm(11)) = horzdispv
                process_space_real(ipnt(11) + (iseg2 - 1) * increm(11)) = horzdispv
            endif
            process_space_real(ipnt(12)) = horzdispv

            ! update pointering in process_space_real

            ipnt(8) = ipnt(8) + increm(8)
            ipnt(9) = ipnt(9) + increm(9)
            ipnt(12) = ipnt(12) + increm(12)

        enddo

        return
    end

end module m_hdispv
