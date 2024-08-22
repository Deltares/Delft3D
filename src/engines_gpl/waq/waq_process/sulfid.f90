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
module m_sulfid
    use m_waq_precision

    implicit none

contains


    subroutine SULFID     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Speciation of dissolved sulphide (S= and HS-) in pore water

        !
        implicit none
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(14) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(14) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(14)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        real(kind = dp) :: sud         ! I  total dissolved sulphide (SUD)                     (gS/m3)
        real(kind = dp) :: lksth2s     ! I  log acidity constant for H2S (l.mole-1)            (-)
        real(kind = dp) :: tcksth2s    ! I  temperature coefficient for KstH2S                 (-)
        real(kind = dp) :: lksths      ! I  log acidity constant for HS- (l.mole-1)            (-)
        real(kind = dp) :: tcksths     ! I  temperature coefficient for KstHS                  (-)
        real(kind = dp) :: ph          ! I  pH                                                 (-)
        real(kind = dp) :: temp        ! I  ambient water temperature                          (oC)
        real(kind = dp) :: poros       ! I  volumetric porosity                                (-)
        real(kind = dp) :: dish2swk    ! O  hydrogen sulphide concentration H2S                (mole/l)
        real(kind = dp) :: dishswk     ! O  (HS-) in water column                              (mole/l)
        real(kind = dp) :: disswk      ! O  (S--) in water column                              (mole/l)
        real(kind = dp) :: frh2sdis    ! O  fraction of dissolved hydrogen sulphide            (-)
        real(kind = dp) :: frhsdis     ! O  fraction (HS-) in water column                     (-)
        real(kind = dp) :: frsdis      ! O  fraction (S--) in water column                     (-)

        ! local declaration

        real(kind = dp) :: h_ion       ! L  proton concentration                               (mole/l)
        real(kind = dp) :: ks1         ! L  acidity hydrolyses equilibrium constant for H2CO3  (-)
        real(kind = dp) :: ks2         ! L  hydrolyses equilibrium constant for CO2            (-)
        real(kind = dp) :: csdt        ! L  total dissolved                                    (mole/l)
        real(kind = dp) :: csd1        ! L  dissolved H2S                                      (mole/l)
        real(kind = dp) :: csd2        ! L  dissolved HS                                       (mole/l)
        real(kind = dp) :: csd3        ! L  dissolved S                                        (mole/l)

        ! initialise pointering in process_space_real

        ipnt = ipoint

        do iseg = 1, num_cells

            sud = process_space_real(ipnt(1))
            lksth2s = process_space_real(ipnt(2))
            tcksth2s = process_space_real(ipnt(3))
            lksths = process_space_real(ipnt(4))
            tcksths = process_space_real(ipnt(5))
            ph = process_space_real(ipnt(6))
            temp = process_space_real(ipnt(7))
            poros = process_space_real(ipnt(8))

            if (sud > 1e-20) then

                ! speciation

                h_ion = 10.**(-ph)
                ks1 = 10.**lksth2s * tcksth2s**(temp - 20.)
                ks2 = 10.**lksths * tcksths**(temp - 20.)
                csdt = sud / (32000. * poros)
                csd1 = csdt / (1. + ks1 / h_ion + (ks1 * ks2) / (h_ion * h_ion))
                csd2 = ks1 * csd1 / h_ion
                csd3 = csdt - csd1 - csd2

                dish2swk = csd1
                dishswk = csd2
                disswk = csd3
                frh2sdis = csd1 / csdt
                frhsdis = csd2 / csdt
                frsdis = 1.0 - frh2sdis - frhsdis
                if (frsdis < 0.0) then
                    frsdis = csd3 / csdt
                endif

            else

                dish2swk = 0.0
                dishswk = 0.0
                disswk = 0.0
                frh2sdis = 0.0
                frhsdis = 0.0
                frsdis = 0.0

            endif

            ! store in process_space_real array

            process_space_real(ipnt(9)) = dish2swk
            process_space_real(ipnt(10)) = dishswk
            process_space_real(ipnt(11)) = disswk
            process_space_real(ipnt(12)) = frh2sdis
            process_space_real(ipnt(13)) = frhsdis
            process_space_real(ipnt(14)) = frsdis

            ipnt = ipnt + increm

        end do

        return
    end

end module m_sulfid
