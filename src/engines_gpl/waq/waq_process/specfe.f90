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
module m_specfe
    use m_waq_precision

    implicit none

contains


    subroutine SPECFE     (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !JVB$ ATTRIBUTES DLLEXPORT, ALIAS: 'SPECFE' :: SPECFE
        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: pmsa(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(25) ! I  Array of pointers in pmsa to get and store the data
        integer(kind = int_wp) :: increm(25) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: noseg       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
        integer(kind = int_wp) :: noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(25)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        real(kind = real_wp) :: feiiid      ! I  dissolved oxidizing iron                           (gFe/m3)
        real(kind = real_wp) :: feiid       ! I  total dissolved reducing iron                      (gFe/m3)
        real(kind = real_wp) :: lkstfe3oh   ! I  log stability constant for Fe3OH2+ (l.mole-1)      (-)
        real(kind = real_wp) :: lkstfe3oh2  ! I  log stability constant for Fe3OH2+ (l.mole-1)      (-)
        real(kind = real_wp) :: tckfe3oh    ! I  temperature coefficient for KstFe3OH               (-)
        real(kind = real_wp) :: tckfe3oh2   ! I  temperature coefficient for KstFe3OH2              (-)
        real(kind = real_wp) :: lkstfe2oh   ! I  log stability constant for Fe2OH+ (l.mole-1)       (-)
        real(kind = real_wp) :: lkstfe2oh2  ! I  log stability constant for Fe2OH2 (l.mole-1)       (-)
        real(kind = real_wp) :: tckfe2oh    ! I  temperature coefficient for KstFe2OH               (-)
        real(kind = real_wp) :: tckfe2oh2   ! I  temperature coefficient for KstFe2OH2              (-)
        real(kind = real_wp) :: ph          ! I  pH                                                 (-)
        real(kind = real_wp) :: temp        ! I  ambient water temperature                          (oC)
        real(kind = real_wp) :: poros       ! I  volumetric porosity                                (-)
        real(kind = real_wp) :: disfe3      ! O  concentration of free dissolved iron(III)          (mole/l)
        real(kind = real_wp) :: disfe3oh    ! O  concentration of dissolved FeOH2+                  (mole/l)
        real(kind = real_wp) :: disfe3oh2   ! O  concentration of dissolved Fe(OH)2+                (mole/l)
        real(kind = real_wp) :: frfe3d      ! O  fraction of free dissolved iron(III)               (-)
        real(kind = real_wp) :: frfe3ohd    ! O  fraction of dissolved FeOH2+                       (-)
        real(kind = real_wp) :: frfe3oh2d   ! O  fraction of dissolved Fe(OH)2+                     (-)
        real(kind = real_wp) :: disfe2      ! O  concentration of free dissolved iron(II)           (mole/l)
        real(kind = real_wp) :: disfe2oh    ! O  concentration of dissolved FeOH+                   (mole/l)
        real(kind = real_wp) :: disfe2oh2   ! O  concentration of dissolved Fe(OH)2                 (mole/l)
        real(kind = real_wp) :: frfe2d      ! O  fraction of free dissolved iron(II)                (-)
        real(kind = real_wp) :: frfe2ohd    ! O  fraction of dissolved FeOH+                        (-)
        real(kind = real_wp) :: frfe2oh2d   ! O  fraction of dissolved Fe(OH)2                      (-)

        ! local declaration

        real(kind = real_wp) :: h_ion       ! L  proton concentration                               (mole/l)
        real(kind = real_wp) :: kfe31       ! L  stability (equilibrium, hydrolysis) constant for FeOH2+ (mole.l-1)
        real(kind = real_wp) :: kfe32       ! L  stability (equilibrium, hydrolysis) constant for Fe(OH)2+ (mole.l-1)
        real(kind = real_wp) :: cfe3dt      ! L  concentration of total dissolved oxidizing iron (mole.l-1)
        real(kind = real_wp) :: cfe3d1      ! L  concentration of free dissolved Fe3+ (mole.l-1)
        real(kind = real_wp) :: cfe3d2      ! L  concentration of dissolved FeOH2+ (mole.l-1)
        real(kind = real_wp) :: cfe3d3      ! L  concentration of dissolved Fe(OH)2+ (mole.l-1)
        real(kind = real_wp) :: kfe21       ! L  stability (equilibrium, hydrolysis) constant for FeOH+ (mole.l-1)
        real(kind = real_wp) :: kfe22       ! L  stability (equilibrium, hydrolysis) constant for Fe(OH)2+ (mole.l-1)
        real(kind = real_wp) :: cfe2dt      ! L  concentration of total dissolved reducing iron
        real(kind = real_wp) :: cfe2d1      ! L  concentration of free dissolved Fe2+ (mole.l-1)
        real(kind = real_wp) :: cfe2d2      ! L  concentration of dissolved FeOH+ (mole.l-1)
        real(kind = real_wp) :: cfe2d3      ! L  concentration of dissolved Fe(OH)2 (mole.l-1)

        ! initialise pointering in pmsa

        ipnt = ipoint

        do iseg = 1, noseg

            feiiid = pmsa(ipnt(1))
            feiid = pmsa(ipnt(2))
            lkstfe3oh = pmsa(ipnt(3))
            lkstfe3oh2 = pmsa(ipnt(4))
            tckfe3oh = pmsa(ipnt(5))
            tckfe3oh2 = pmsa(ipnt(6))
            lkstfe2oh = pmsa(ipnt(7))
            lkstfe2oh2 = pmsa(ipnt(8))
            tckfe2oh = pmsa(ipnt(9))
            tckfe2oh2 = pmsa(ipnt(10))
            ph = pmsa(ipnt(11))
            temp = pmsa(ipnt(12))
            poros = pmsa(ipnt(13))

            ! fe(III)

            cfe3dt = feiiid / (56000. * poros)

            ! if concentration small then calculate fractions with 1.e-20

            if (feiiid < 1.e-20) then
                cfe3dt = 1.e-20 / (56000. * poros)
            endif

            ! speciation

            h_ion = 10.**(-ph)
            kfe31 = 10.**lkstfe3oh * tckfe3oh**(temp - 20.)
            kfe32 = 10.**lkstfe3oh2 * tckfe3oh2**(temp - 20.)
            cfe3d1 = cfe3dt / (1. + kfe31 / h_ion + kfe32 / (h_ion * h_ion))
            cfe3d2 = kfe31 * cfe3d1 / h_ion
            cfe3d3 = cfe3dt - cfe3d1 - cfe3d2
            if (cfe3d3 < 0.0) then
                cfe3d3 = kfe32 * cfe3d1 / (h_ion * h_ion)
            endif

            disfe3 = cfe3d1
            disfe3oh = cfe3d2
            disfe3oh2 = cfe3d3
            frfe3d = cfe3d1 / cfe3dt
            frfe3ohd = cfe3d2 / cfe3dt
            frfe3oh2d = 1.0 - frfe3d - frfe3ohd
            if (frfe3oh2d < 0.0) then
                frfe3oh2d = cfe3d3 / cfe3dt
            endif

            ! if concentration small then recalculate the concentrations of the fractions with original feiiid concentration

            if (feiiid < 1.e-20) then
                cfe3dt = feiiid / (56000. * poros)
                disfe3 = cfe3dt * frfe3d
                disfe3oh = cfe3dt * frfe3ohd
                disfe3oh2 = cfe3dt * frfe3oh2d
            endif

            ! fe(II)

            cfe2dt = feiid / (56000. * poros)

            ! if concentration small then calculate fractions with 1.e-20

            if (feiid < 1.e-20) then
                cfe2dt = 1.e-20 / (56000. * poros)
            endif

            ! speciation

            h_ion = 10.**(-ph)
            kfe21 = 10.**lkstfe2oh * tckfe2oh**(temp - 20.)
            kfe22 = 10.**lkstfe2oh2 * tckfe2oh2**(temp - 20.)
            cfe2d1 = cfe2dt / (1. + kfe21 / h_ion + kfe22 / (h_ion * h_ion))
            cfe2d2 = kfe21 * cfe2d1 / h_ion
            cfe2d3 = cfe2dt - cfe2d1 - cfe2d2
            if (cfe2d3 < 0.0) then
                cfe2d3 = kfe22 * cfe2d1 / (h_ion * h_ion)
            endif

            disfe2 = cfe2d1
            disfe2oh = cfe2d2
            disfe2oh2 = cfe2d3
            frfe2d = cfe2d1 / cfe2dt
            frfe2ohd = cfe2d2 / cfe2dt
            frfe2oh2d = 1.0 - frfe2d - frfe2ohd
            if (frfe2oh2d < 0.0) then
                frfe2oh2d = cfe2d3 / cfe2dt
            endif

            ! if concentration small then recalculate the concentrations of the fractions with original feiid concentration

            if (feiid < 1.e-20) then
                cfe2dt = feiid / (56000. * poros)
                disfe2 = cfe2dt * frfe2d
                disfe2oh = cfe2dt * frfe2ohd
                disfe2oh2 = cfe2dt * frfe2oh2d
            endif

            ! store in pmsa array

            pmsa(ipnt(14)) = disfe3
            pmsa(ipnt(15)) = disfe3oh
            pmsa(ipnt(16)) = disfe3oh2
            pmsa(ipnt(17)) = frfe3d
            pmsa(ipnt(18)) = frfe3ohd
            pmsa(ipnt(19)) = frfe3oh2d
            pmsa(ipnt(20)) = disfe2
            pmsa(ipnt(21)) = disfe2oh
            pmsa(ipnt(22)) = disfe2oh2
            pmsa(ipnt(23)) = frfe2d
            pmsa(ipnt(24)) = frfe2ohd
            pmsa(ipnt(25)) = frfe2oh2d

            ipnt = ipnt + increm

        end do

        return
    end

end module m_specfe
