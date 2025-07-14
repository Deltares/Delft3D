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
module m_vbstat
    use m_waq_precision

    implicit none

contains


    subroutine VBSTAT     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper
        use m_extract_waq_attribute

        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(16) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(16) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(16)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop
        real(kind = real_wp) :: DELT        ! I  timestep for processes                             (d)
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        real(kind = real_wp) :: SwEmersion  ! I  switch indicating submersion(0) or emersion(1)     (-)
        real(kind = real_wp) :: SwWV        ! I  use wetland vegetation model (0=no,1=yes)          (-)
        integer(kind = int_wp) :: VBType      ! I  code of vegetation type for error and warnings     (-)
        real(kind = real_wp) :: nsfVB       ! I  nr successive emersion(flood) VB01                 (d)
        real(kind = real_wp) :: CrnsfVB01   ! I  critical number successive flood days VB01         (d)
        real(kind = real_wp) :: SwNutVB01   ! I  switch indicating nutrient limitation (0=no,1=yes) (-)
        real(kind = real_wp) :: Initnsf     ! I  initial nr of flood days at start of simulation    (d)
        real(kind = real_wp) :: CrdepVB01   ! I  critical depth for inundation mortality VB01       (m)
        real(kind = real_wp) :: Initnscd    ! I  initial critical depth exceedence days at start    (d)
        real(kind = real_wp) :: TotalDepth  ! I  total depth water column                           (m)
        real(kind = real_wp) :: SwVB01Gro   ! O  vegetation biomass growth allowed (0=no,1=yes)     (-)
        real(kind = real_wp) :: SwVB01Mrt   ! O  vegetation biomass dead (0=no,1=yes)               (-)
        real(kind = real_wp) :: nscdVB01    ! O  nr successive critical depth exceedence VB01       (d)
        integer(kind = int_wp), save :: ifirst(1:18) = 0     !    for initialisation
        integer(kind = int_wp) :: ikmrk1         ! first feature
        integer(kind = int_wp) :: ikmrk2         ! second feature
        integer(kind = int_wp) :: ip             ! base output location for bottom segement pointer output
        integer(kind = int_wp) :: inc            ! increment in output location for bottom segement pointer output
        integer(kind = int_wp) :: iq             ! counter for pointer loop
        integer(kind = int_wp) :: ifrom          ! from location
        integer(kind = int_wp) :: ito            ! to location
        integer(kind = int_wp) :: ibotseg        ! bottom segement for current segement
        integer(kind = int_wp) :: ilumon
        !
        !*******************************************************************************
        !
        ipnt = ipoint
        !
        CALL get_log_unit_number(ILUMON)
        VBType = NINT(process_space_real(ipnt(3)))
        ! define outputs at least once
        SwWV = 0
        nsfVB = 0
        nscdVB01 = 0

        ! initialise bottom segment pointer

        if (ifirst(VBType)==0) then
            ip = ipoint(16)
            inc = increm(16)

            do iseg = 1, num_cells
                process_space_real(ip + inc * (iseg - 1)) = real(-1)
            end do

            ! set botseg equal to iseg for the segments which have a bottom

            do iseg = 1, num_cells
                call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
                if (ikmrk1<3) then
                    call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
                    if ((ikmrk2==0).or.(ikmrk2==3)) then
                        process_space_real(ip + inc * (iseg - 1)) = real(iseg)
                    endif
                endif
            enddo

            ! loop to find bottom segment in water columns

            do iq = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir, num_exchanges_u_dir + num_exchanges_v_dir + 1, -1
                ifrom = iexpnt(1, iq)
                ito = iexpnt(2, iq)
                if (ifrom > 0 .and. ito > 0) then
                    ibotseg = process_space_real(ip + inc * (ito - 1))
                    if (ibotseg > 0) then
                        process_space_real(ip + inc * (ifrom - 1)) = real(ibotseg)
                    endif
                endif
            enddo

            ! do the same for the delwaq-g bottom

            do iq = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir
                ifrom = iexpnt(1, iq)
                ito = iexpnt(2, iq)
                if (ifrom > 0 .and. ito > 0) then
                    ibotseg = process_space_real(ip + inc * (ifrom - 1))
                    if (ibotseg > 0) then
                        process_space_real(ip + inc * (ito - 1)) = real(ibotseg)
                    endif
                endif
            enddo
        endif

        do iseg = 1, num_cells
            !
            !
            !   *****     Insert your code here  *****
            !
            !        lowest water and 2d segments only
            call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
            call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
            if (ikmrk1<3 .and. (ikmrk2==0).or.(ikmrk2==3)) then

                SwEmersion = process_space_real(ipnt(1))
                SwWV = NINT(process_space_real(ipnt(2)))
                VBType = NINT(process_space_real(ipnt(3)))
                nsfVB = process_space_real(ipnt(4))
                CrnsfVB01 = process_space_real(ipnt(5))
                Initnsf = process_space_real(ipnt(6))
                CrdepVB01 = process_space_real(ipnt(7))
                nscdVB01 = process_space_real(ipnt(8))
                Initnscd = process_space_real(ipnt(9))
                DELT = process_space_real(ipnt(10))
                TotalDepth = process_space_real(ipnt(11))

                !           initialise growth
                SWVB01Gro = 1.0
                SwVB01Mrt = 0.0

                if (SwWV == 0) then
                    if (ifirst(1) == 0) then
                        nsfVB = Initnsf
                    endif

                    if (NINT(SwEmersion) == 0) then
                        nsfVB = nsfVB + DELT
                        SWVB01Gro = 0.0
                    else
                        nsfVB = 0
                    endif

                    if (nsfVB > CrnsfVB01) then
                        SwVB01Mrt = 1.0
                    endif
                else
                    if (ifirst(vbtype) == 0) then
                        nscdVB01 = Initnscd
                        nsfVB = 0.0
                    endif
                    if (TotalDepth > CrdepVB01) then
                        nscdVB01 = nscdVB01 + DELT
                        SWVB01Gro = 0.0
                    else
                        nscdVB01 = 0
                    endif

                    if (nscdVB01 > CrnsfVB01) then
                        SwVB01Mrt = 1.0
                    endif

                endif
                !
                !   *****     End of your code       *****
                !
                process_space_real(ipnt(12)) = SwVB01Gro
                process_space_real(ipnt(13)) = SwVB01Mrt
                process_space_real(ipnt(14)) = nsfVB
                process_space_real(ipnt(15)) = nscdVB01
            endif
            !
            ipnt = ipnt + increm
            !
        end do

        if (SwWV == 0) then
            ifirst (1) = 1
        else
            ifirst (vbtype) = 1
        end if
        !
        return
    end

end module m_vbstat
