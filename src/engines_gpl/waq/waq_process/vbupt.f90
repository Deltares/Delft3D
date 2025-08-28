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
module m_vbupt
    use m_waq_precision

    implicit none

contains


    subroutine VBUPT      (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : write_error_message

        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(25) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(25) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(25)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        real(kind = real_wp) :: fVB         ! I  area scaled flux dVB01                             (gC/m2/d)
        real(kind = real_wp) :: SwVBGro     ! I  vegetation biomass growth allowed (0=no,1=yes)     (-)
        real(kind = real_wp) :: F1VB        ! I  allocation factor comp. 1 (stem) VB01              (-)
        real(kind = real_wp) :: F2VB        ! I  allocation factor comp. 2 (foliage) VB01           (-)
        real(kind = real_wp) :: F3VB        ! I  allocation factor comp. 3 (branch) VB01            (-)
        real(kind = real_wp) :: F4VB        ! I  allocation factor comp. 4 (root) VB01              (-)
        real(kind = real_wp) :: F5VB        ! I  allocation factor comp. 5 (fineroot) VB01          (-)
        real(kind = real_wp) :: CNf1VB      ! I  carbon-nitrogen ratio in stem VB01                 (gC/gN)
        real(kind = real_wp) :: CNf2VB      ! I  carbon-nitrogen ratio in foliage VB01              (gC/gN)
        real(kind = real_wp) :: CNf3VB      ! I  carbon-nitrogen ratio in branch VB01               (gC/gN)
        real(kind = real_wp) :: CNf4VB      ! I  carbon-nitrogen ratio in root VB01                 (gC/gN)
        real(kind = real_wp) :: CNf5VB      ! I  carbon-nitrogen ratio in fineroot VB01             (gC/gN)
        real(kind = real_wp) :: CPf1VB      ! I  carbon-phosporus ratio in stem VB01                (gC/gP)
        real(kind = real_wp) :: CPf2VB      ! I  carbon-phosporus ratio in foliage VB01             (gC/gP)
        real(kind = real_wp) :: CPf3VB      ! I  carbon-phosporus ratio in branch VB01              (gC/gP)
        real(kind = real_wp) :: CPf4VB      ! I  carbon-phosporus ratio in root VB01                (gC/gP)
        real(kind = real_wp) :: CPf5VB      ! I  carbon-phosporus ratio in fineroot VB01            (gC/gP)
        real(kind = real_wp) :: CSf1VB      ! I  carbon-sulphur ratio in stem VB01                  (gC/gS)
        real(kind = real_wp) :: CSf2VB      ! I  carbon-sulphur ratio in foliage VB01               (gC/gS)
        real(kind = real_wp) :: CSf3VB      ! I  carbon-sulphur ratio in branch VB01                (gC/gS)
        real(kind = real_wp) :: CSf4VB      ! I  carbon-sulphur ratio in root VB01                  (gC/gS)
        real(kind = real_wp) :: CSf5VB      ! I  carbon-sulphur ratio in fineroot                   (gC/gS)

        real(kind = real_wp) :: fNVB01up    ! O  uptake roots VB01                                  (gN/m2/d)
        real(kind = real_wp) :: fPVB01up    ! O  uptake roots VB01                                  (gP/m2/d)
        real(kind = real_wp) :: fSVB01up    ! O  uptake roots VB01                                  (gS/m2/d)
        real(kind = real_wp) :: weighCN
        real(kind = real_wp) :: weighCP
        real(kind = real_wp) :: weighCS
        !
        !*******************************************************************************
        !
        ipnt = ipoint
        !
        do iseg = 1, num_cells
            !
            fVB = process_space_real(ipnt(1))
            SwVBGro = process_space_real(ipnt(2))
            F1VB = process_space_real(ipnt(3))
            F2VB = process_space_real(ipnt(4))
            F3VB = process_space_real(ipnt(5))
            F4VB = process_space_real(ipnt(6))
            F5VB = process_space_real(ipnt(7))
            CNf1VB = process_space_real(ipnt(8))
            CNf2VB = process_space_real(ipnt(9))
            CNf3VB = process_space_real(ipnt(10))
            CNf4VB = process_space_real(ipnt(11))
            CNf5VB = process_space_real(ipnt(12))
            CPf1VB = process_space_real(ipnt(13))
            CPf2VB = process_space_real(ipnt(14))
            CPf3VB = process_space_real(ipnt(15))
            CPf4VB = process_space_real(ipnt(16))
            CPf5VB = process_space_real(ipnt(17))
            CSf1VB = process_space_real(ipnt(18))
            CSf2VB = process_space_real(ipnt(19))
            CSf3VB = process_space_real(ipnt(20))
            CSf4VB = process_space_real(ipnt(21))
            CSf5VB = process_space_real(ipnt(22))

            !
            !   *****     Insert your code here  *****
            !
            !
            fNVB01up = 0.0
            fPVB01up = 0.0
            fSVB01up = 0.0

            if (Nint(SwVBGro) == 1) then

                !            make sure allocation factors for roots > 0

                if ((F4VB + F5VB) - 1.E-10 < 0.0) then
                    CALL write_error_message ('(no valid values for F4VB and F5VB (alloction factors vegetation  roots)')
                else
                    !               average Nutrient content of cohort
                    weighCN = F1VB * CNf1VB + F2VB * CNf2VB + F3VB * CNf3VB + F4VB * CNf4VB + F5VB * CNf5VB
                    weighCP = F1VB * CPf1VB + F2VB * CPf2VB + F3VB * CPf3VB + F4VB * CPf4VB + F5VB * CPf5VB
                    weighCS = F1VB * CSf1VB + F2VB * CSf2VB + F3VB * CSf3VB + F4VB * CSf4VB + F5VB * CSf5VB

                    !               calculate 2D nutrient uptake flux
                    fNVB01up = fVB / weighCN
                    fPVB01up = fVB / weighCP
                    fSVB01up = fVB / weighCS
                endif

                !        evaluate SwVBGro
            endif
            !
            !   *****     End of your code       *****
            !
            process_space_real(ipnt(23)) = fNVB01up
            process_space_real(ipnt(24)) = fPVB01up
            process_space_real(ipnt(25)) = fSVB01up
            !
            ipnt = ipnt + increm
            !
        end do
        !
        return
    end

end module m_vbupt
