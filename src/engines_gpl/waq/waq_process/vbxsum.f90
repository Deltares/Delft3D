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
module m_vbxsum
    use m_waq_precision

    implicit none

contains


    subroutine VBXSUM     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(*)  ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(*)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

        integer(kind = int_wp) :: iflux, icohort, iseg, ioutput
        integer(kind = int_wp), save :: ncohorts, nfluxes
        integer(kind = int_wp), dimension(:, :), allocatable, save :: ipnt, incr
        logical, save :: first = .true.
        !
        !*******************************************************************************
        !     Sum the uptake of nutrients from the sediment and release to the sediment by terrestrial vegetation (VEGMOD)
        !     (This is a workaround for having the fluxes per cohort in the process definition of the DelwaqG process)
        !

        if (first) then
            first = .false.
            ncohorts = int(process_space_real(ipoint(1)))
            nfluxes = int(process_space_real(ipoint(2)))

            allocate(ipnt(nfluxes, ncohorts + 1))
            allocate(incr(nfluxes, ncohorts + 1))
        endif

        ipnt = reshape(ipoint(3:2 + (ncohorts + 1) * nfluxes), [nfluxes, ncohorts + 1])
        incr = reshape(increm(3:2 + (ncohorts + 1) * nfluxes), [nfluxes, ncohorts + 1])

        do iseg = 1, num_cells
            do iflux = 1, nfluxes
                process_space_real(ipnt(iflux, ncohorts + 1)) = 0.0
            enddo
            do icohort = 1, ncohorts
                do iflux = 1, nfluxes
                    process_space_real(ipnt(iflux, ncohorts + 1)) = process_space_real(ipnt(iflux, ncohorts + 1)) + process_space_real(ipnt(iflux, icohort))
                enddo
            enddo
            ipnt = ipnt + incr
        enddo

        return
    end

end module m_vbxsum
