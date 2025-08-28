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
module m_agecart
    use m_waq_precision

    implicit none

contains


    subroutine AGECART    (process_space_real, fl, ipoint, increm, num_cells, &
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
        integer(kind = int_wp) :: ipoint(4) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(4) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(4)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        real(kind = real_wp) :: watersrc    ! I  Source of water to be traced                       (g/m3)
        real(kind = real_wp) :: ageconc     ! I  Age concentration                                  (g.d/m3)
        real(kind = real_wp) :: age_threshold     ! I  user defined threshold to avoid small concentration (g/m3), default 0.0
        real(kind = real_wp) :: ageprod     ! F  production of waterage                             (d)
        integer(kind = int_wp) :: Iageprod     !  Pointer to the production of waterage
        real(kind = real_wp) :: temp_watersrc_c     ! F  new threshold ratio to avoid small concentration (g/m3), default 0.0
        
        !
        !*******************************************************************************
        !
        ipnt = ipoint
        Iageprod = 1
        !
        temp_watersrc_c = 0.0
        do iseg = 1, num_cells
            watersrc = process_space_real(ipnt(1))
            temp_watersrc_c =temp_watersrc_c +watersrc
            ipnt(1) = ipnt(1) + increm(1)
        end do
        temp_watersrc_c = temp_watersrc_c/num_cells
                    
        ipnt = ipoint            
        do iseg = 1, num_cells
            !
            watersrc = process_space_real(ipnt(1))
            ageconc = process_space_real(ipnt(2))
            age_threshold = process_space_real(ipnt(3))
            !
            !   *****     Insert your code here  *****
            !
            ageprod = watersrc
            !
            !   *****     End of your code       *****
            !
            fl  (Iageprod) = ageprod

            if (watersrc > age_threshold*temp_watersrc_c)then
                process_space_real(ipnt(4)) = ageconc / watersrc
            else
                process_space_real(ipnt(4)) = -999.0
            endif
            !
            Iageprod = Iageprod + noflux
            ipnt = ipnt + increm
            !
        end do
        !
        return
    end subroutine

end module m_agecart
