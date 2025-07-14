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
module m_effblo
    use m_waq_precision
    use m_get_effi

    implicit none

contains


    subroutine effblo     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Light efficiency function BLOOM algae
        !>
        !>              Routine is called for all algae species with species specific coefficients

        implicit none

        !     Type    Name         I/O Description

        real(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(37) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(37) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(37)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop

        !     arguments

        integer(kind = int_wp) :: sweff      ! input , Switch to use classic(1) or direct(2) BLOOM Efficiency calculation
        real(kind = real_wp) :: temper     ! input , temperature
        real(kind = real_wp) :: radiat     ! input , radiation
        real(kind = real_wp) :: ext        ! input , total extinction
        real(kind = real_wp) :: depthw     ! input , depth of the layer
        real(kind = real_wp) :: daylen     ! input , daylength in hours

        !     local decalarations

        integer(kind = int_wp) :: nspe       ! number of bloom algae species
        integer(kind = int_wp) :: ispe       ! index number of bloom algae species
        real(kind = real_wp) :: effi(30)   ! efficiencies per species group

        ipnt = ipoint

        !     we might not have the bloom parameters loaded yet. This could already be done here or in BLOOM? Only the first time step.
        !     this is in a module/include, so we might put a flag if it was read of not.
        !     this should be a 'proto-proces', and thus needs to be added to the BLOOM.SPE
        do iseg = 1, num_cells

            if (btest(iknmrk(iseg), 0)) then

                SWEff = nint(process_space_real(ipnt(1)))
                Temper = process_space_real(ipnt(2))
                Radiat = process_space_real(ipnt(3)) * 60.48  ! Conversion from W/m2 to J/cm2/7days
                Ext = process_space_real(ipnt(4))
                Depthw = process_space_real(ipnt(5))
                DayLen = process_space_real(ipnt(6)) * 24.   ! Conversion from days to hours
                !     test for extinction and depth to prevent diff by zero!!
                effi = 0.0e0
                if(ext>0.0e0 .and. depthw>0.0e0) then
                    call get_effi(SWEff, temper, radiat, ext, depthw, daylen, nspe, effi)
                endif
                do ispe = 1, nspe
                    process_space_real(ipnt(7 + ispe)) = effi(ispe)
                enddo
            endif

            ipnt = ipnt + increm

        end do

        return
    end subroutine

end module m_effblo
