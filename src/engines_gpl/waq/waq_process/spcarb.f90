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
module m_spcarb
    use m_waq_precision

    implicit none

contains


    subroutine SPCARB     (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !JVB$ ATTRIBUTES DLLEXPORT, ALIAS: 'SPCARB' :: SPCARB
        !
        !*******************************************************************************
        !
        use physicalconsts, only : CtoKelvin
        implicit none
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: pmsa(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(15) ! I  Array of pointers in pmsa to get and store the data
        integer(kind = int_wp) :: increm(15) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: noseg       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
        integer(kind = int_wp) :: noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(15)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop

        ! from pmsa

        real(kind = dp) :: tic         ! I  total inorganic carbonate                          (gC/m3)
        real(kind = dp) :: co2         ! I  CO2                                                (g/m3)
        integer(kind = int_wp) :: swticco2    ! I  switch (0=use TIC, 1=use CO2)                      (-)
        real(kind = dp) :: ph          ! I  pH                                                 (-)
        real(kind = dp) :: salinity    ! I  Salinity                                           (g/kg)
        real(kind = dp) :: temp        ! I  ambient water temperature                          (oC)
        real(kind = dp) :: poros       ! I  volumetric porosity                                (-)
        real(kind = dp) :: disco2      ! O  concentration of dissolved carbon dioxide          (g/m3)
        real(kind = dp) :: dish2co3    ! O  concentration of dissolved true H2CO3              (gC/m3)
        real(kind = dp) :: dishco3     ! O  concentration of dissolved HCO3(-)                 (gC/m3)
        real(kind = dp) :: disco3      ! O  concentration of dissolved CO3(2-)                 (gC/m3)
        real(kind = dp) :: frco2d      ! O  fraction of dissolved carbon dioxide               (-)
        real(kind = dp) :: frh2co3d    ! O  fraction of dissolved true H2CO3                   (-)
        real(kind = dp) :: frhco3d     ! O  fraction of dissolved HCO3(-)                      (-)
        real(kind = dp) :: frco3d      ! O  fraction of dissolved CO3(2-)                      (-)

        ! local declaration

        real(kind = dp) :: tabs        ! L  absolute temperature                               (K)
        real(kind = dp) :: h_ion       ! L  proton concentration                               (mole/l)
        real(kind = dp) :: kc0         ! L  hydrolyses equilibrium constant for CO2            (-)
        real(kind = dp) :: lkc1        ! L  log acidity hydrolyses equilibrium constant for H2CO3  (-)
        real(kind = dp) :: lkc2        ! L  log hydrolyses equilibrium constant for CO2        (-)
        real(kind = dp) :: kc1         ! L  acidity hydrolyses equilibrium constant for H2CO3  (-)
        real(kind = dp) :: kc2         ! L  hydrolyses equilibrium constant for CO2            (-)
        real(kind = dp) :: ccdt        ! L  total dissolved carbon                             (mole/l)
        real(kind = dp) :: ccd0        ! L  dissolved carbon dioxide                           (mole/l)
        real(kind = dp) :: ccd1        ! L  dissolved H2C03                                    (mole/l)
        real(kind = dp) :: ccd2        ! L  dissolved HC03                                     (mole/l)
        real(kind = dp) :: ccd3        ! L  dissolved CO3                                      (mole/l)

        ! initialise pointering in pmsa

        ipnt = ipoint

        do iseg = 1, noseg

            tic = pmsa(ipnt(1))
            co2 = pmsa(ipnt(2)) * 12. / 44.
            swticco2 = nint(pmsa(ipnt(3)))
            ph = pmsa(ipnt(4))
            salinity = max (pmsa(ipnt(5)), 0.0)
            temp = pmsa(ipnt(6))
            poros = pmsa(ipnt(7))

            ! use tic or co2 depending on the switch

            if (swticco2 == 1) then
                tic = co2
            endif

            if (tic > 1e-20) then

                ! speciation

                tabs = temp + real(CtoKelvin)
                h_ion = 10.**(-ph)
                kc0 = 650.
                lkc1 = -3404.71 / tabs - 0.032786 * tabs + 14.7120 + 0.19178 * ((0.543 * salinity)**0.333)
                lkc2 = -2902.39 / tabs - 0.023790 * tabs + 6.4710 + 0.46930 * ((0.543 * salinity)**0.333)
                kc1 = 10.**lkc1
                kc2 = 10.**lkc2

                ccdt = tic / (12000. * poros)
                ccd1 = ccdt / ((1. + kc1 / h_ion + (kc1 * kc2) / (h_ion * h_ion)) * (1 + kc0))
                ccd0 = kc0 * ccd1
                ccd2 = kc1 * (kc0 + 1) * ccd1 / h_ion
                ccd3 = ccdt - ccd0 - ccd1 - ccd2
                if (ccd3 < 0.0) then
                    ccd3 = kc2 * ccd2 / h_ion
                endif

                disco2 = ccd0 * 44000.
                dish2co3 = ccd1 * 12000.
                dishco3 = ccd2 * 12000.
                disco3 = ccd3 * 12000.
                frco2d = ccd0 / ccdt
                frh2co3d = ccd1 / ccdt
                frhco3d = ccd2 / ccdt
                frco3d = 1.0 - frco2d - frh2co3d - frhco3d
                if (frco3d < 0.0) then
                    frco3d = ccd3 / ccdt
                endif

            else
                disco2 = 0.0
                dish2co3 = 0.0
                dishco3 = 0.0
                disco3 = 0.0
                frco2d = 1.0
                frh2co3d = 0.0
                frhco3d = 0.0
                frco3d = 0.0
            endif

            ! store in pmsa array

            pmsa(ipnt(8)) = disco2
            pmsa(ipnt(9)) = dish2co3
            pmsa(ipnt(10)) = dishco3
            pmsa(ipnt(11)) = disco3
            pmsa(ipnt(12)) = frco2d
            pmsa(ipnt(13)) = frh2co3d
            pmsa(ipnt(14)) = frhco3d
            pmsa(ipnt(15)) = frco3d

            ipnt = ipnt + increm

        enddo

        return
    end

end module m_spcarb
