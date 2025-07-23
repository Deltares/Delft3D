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
module m_sumfrc
    use m_waq_precision

    implicit none

contains


    subroutine sumfrc     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)

        ! delwaq-pl routine to make a summation over a variable number of fractions

        implicit none

        ! declaration of the arguments

        real(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)   ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(*)   ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

        ! variables from process_space_real array

        integer(kind = int_wp) :: nfrac       ! I  number of fractions to be summed
        real(kind = real_wp) :: frac        ! I  fraction to be summed
        real(kind = real_wp) :: sum         ! O  sum of the fractions
        real(kind = real_wp) :: rfrac       ! O  relative fraction

        ! local variables

        integer(kind = int_wp) :: npnt        !    number of pointers in process_space_real
        integer(kind = int_wp), allocatable :: ipnt(:)     !    local work array for the pointering
        integer(kind = int_wp) :: iseg        !    loop counter for computational element loop
        integer(kind = int_wp) :: ifrac       !    loop counter for fractions

        ! initialise pointering in process_space_real

        nfrac = process_space_real(ipoint(1))
        npnt = 2 * nfrac + 2
        allocate(ipnt(npnt))
        ipnt = ipoint(1:npnt)

        ! loop over all segments

        do iseg = 1, num_cells

            ! sum the fractions

            sum = 0.0
            do ifrac = 1, nfrac
                frac = process_space_real(ipnt(1 + ifrac))
                sum = sum + frac
            enddo

            ! store sum in process_space_real

            process_space_real(ipnt(nfrac + 2)) = sum

            ! calculate relative fractions

            if (abs(sum) < 1e-20) then
                rfrac = 1. / nfrac
                do ifrac = 1, nfrac
                    process_space_real(ipnt(nfrac + 2 + ifrac)) = rfrac
                enddo
            else
                do ifrac = 1, nfrac
                    rfrac = process_space_real(ipnt(1 + ifrac)) / sum
                    process_space_real(ipnt(nfrac + 2 + ifrac)) = rfrac
                enddo
            endif

            ! update pointering

            ipnt = ipnt + increm(1:npnt)

        enddo

        return
    end

end module m_sumfrc
