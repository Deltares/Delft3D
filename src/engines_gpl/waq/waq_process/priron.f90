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
module m_priron
    use m_waq_precision

    implicit none

contains


    subroutine PRIRON     (pmsa, fl, ipoint, increm, noseg, &
            noflux, iexpnt, iknmrk, noq1, noq2, &
            noq3, noq4)
        !JVB$ ATTRIBUTES DLLEXPORT, ALIAS: 'PRIRON' :: PRIRON
        !
        !*******************************************************************************
        !
        IMPLICIT NONE
        !
        !     Type    Name         I/O Description
        !
        real(kind = real_wp) :: pmsa(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(45) ! I  Array of pointers in pmsa to get and store the data
        integer(kind = int_wp) :: increm(45) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: noseg       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
        integer(kind = int_wp) :: noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(45)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop
        !
        !*******************************************************************************
        !
        !     Type    Name         I/O Description                                        Unit
        !
        real(kind = dp) :: feiiipa     ! I  particulate amorphous oxidizing iron               (gFe/m3)
        real(kind = dp) :: feiiid      ! I  dissolved oxidizing iron                           (gFe/m3)
        real(kind = dp) :: fes         ! I  iron(II) sulphide                                  (gFe/m3)
        real(kind = dp) :: feiid       ! I  total dissolved reducing iron                      (gFe/m3)
        real(kind = dp) :: feco3       ! I  iron(II) carbonate concentration                   (gFe/m3)
        real(kind = dp) :: sud         ! I  total dissolved sulphide (SUD)                     (gS/m3)
        real(kind = dp) :: tic         ! I  total inorganic carbonate                          (gC/m3)
        real(kind = dp) :: co2         ! I  CO2                                                (g/m3)
        real(kind = dp) :: frfe3dis    ! I  fraction dissolved free iron(III)                  (-)
        real(kind = dp) :: lkspfeoh3   ! I  log solubility product for Fe(OH)3                 (-)
        real(kind = dp) :: rcagfe320   ! I  specific iron(III) aging rate at 20 oC             (1/d)
        real(kind = dp) :: rcdisfe320  ! I  specific iron(III) dissolution rate at 20 oC       (1/d)
        real(kind = dp) :: rcprcfe320  ! I  specific iron(III) precipitation rate              (gFe/m3/d)
        real(kind = dp) :: tcagfe3     ! I  temperature coeff. for iron(III) aging             (-)
        real(kind = dp) :: tcdisfe3    ! I  temperature coeff. for iron(III) dissolution       (-)
        real(kind = dp) :: tcprcfe3    ! I  temperature coeff. for iron(III) precipitation     (-)
        real(kind = dp) :: frfe2dis    ! I  fraction dissolved free iron(II)                   (-)
        real(kind = dp) :: frh2sdis    ! I  fraction of dissolved hydrogen sulphide            (-)
        real(kind = dp) :: frs2dis     ! I  fraction dissolved free sulphide                   (-)
        real(kind = dp) :: frco3dis    ! I  fraction dissolved free carbonate                  (-)
        real(kind = dp) :: lkspfes     ! I  log solubility product for FeS                     (-)
        real(kind = dp) :: lkspfeco3   ! I  log solubility product for FeCO3                   (-)
        real(kind = dp) :: rcpyrite20  ! I  specific pyrite formation rate at 20 oC            (gS/m3/d)
        real(kind = dp) :: rcdisfes20  ! I  iron(II) sulphide dissolution rate                 (1/d)
        real(kind = dp) :: rcprcfes20  ! I  iron(II) sulphide precipitation rate               (gFe/m3/d)
        real(kind = dp) :: rcdisfec20  ! I  iron(II) carbonate dissolution rate                (1/d)
        real(kind = dp) :: rcprcfec20  ! I  iron(II) carbonate precipitation rate              (gFe/m3/d)
        real(kind = dp) :: tcpyrite    ! I  temperature coeff. for pyrite formation            (-)
        real(kind = dp) :: tcdisfes    ! I  temperature coeff. for iron(II) sulphide diss.     (-)
        real(kind = dp) :: tcprcfes    ! I  temperature coeff. for iron(II) sulphide prec.     (-)
        real(kind = dp) :: tcdisfeco3  ! I  temperature coeff. for iron(II) carbonate diss.    (-)
        real(kind = dp) :: tcprcfeco3  ! I  temperature coeff. for iron(II) carbonate prec.    (-)
        real(kind = dp) :: swticco2    ! I  switch (0=use TIC, 1=use CO2)                      (-)
        real(kind = dp) :: ph          ! I  pH                                                 (-)
        real(kind = dp) :: temp        ! I  ambient water temperature                          (oC)
        real(kind = dp) :: delt        ! I  timestep for processes                             (d)
        real(kind = dp) :: poros       ! I  volumetric porosity                                (-)
        real(kind = dp) :: fpfe3       ! O  rate of amorphous iron(III) precipitat.            (gFe/m3/d)
        real(kind = dp) :: fdfe3       ! O  rate of amorphous iron(III) dissolution            (gFe/m3/d)
        real(kind = dp) :: fafe3       ! O  rate of amorphous iron(III) aging                  (gFe/m3/d)
        real(kind = dp) :: fpfes       ! O  rate of iron(II) sulphide precipitation            (gFe/m3/d)
        real(kind = dp) :: fdfes       ! O  rate of iron(II) sulphide dissolution              (gFe/m3/d)
        real(kind = dp) :: fpfeco3     ! O  rate of iron(II) carbonate precipitation           (gFe/m3/d)
        real(kind = dp) :: fdfeco3     ! O  rate of iron(II) carbonate dissolution             (gFe/m3/d)
        real(kind = dp) :: fpyr        ! O  rate of pyrite formation                           (gFe/m3/d)
        real(kind = dp) :: dafe3       ! F  rate of amorphous iron(III) aging                  (gFe/m3/d)
        integer(kind = int_wp) :: idpfe3      !    Pointer to the rate of amorphous iron(III) precipitat.
        integer(kind = int_wp) :: iddfe3      !    Pointer to the rate of amorphous iron(III) dissolution
        integer(kind = int_wp) :: idafe3      !    Pointer to the rate of amorphous iron(III) aging
        integer(kind = int_wp) :: idpfes      !    Pointer to the rate of iron(II) sulphide precipitation
        integer(kind = int_wp) :: iddfes      !    Pointer to the rate of iron(II) sulphide dissolution
        integer(kind = int_wp) :: idpfeco3    !    Pointer to the rate of iron(II) carbonate precipitation
        integer(kind = int_wp) :: iddfeco3    !    Pointer to the rate of iron(II) carbonate dissolution
        integer(kind = int_wp) :: idpyr       !    Pointer to the rate of pyrite formation
        real(kind = dp) :: ksp1        ! L  solubility product for Fe(OH)3 ((mole.l-1)4)
        real(kind = dp) :: cfe3d       ! L  equilibrium dissolved free iron(III) concentration (mole.l-1)
        real(kind = dp) :: oh          ! L  hydroxyl concentration (mole.l-1)
        real(kind = dp) :: iap1        ! L  ion activity product for Fe(OH)3 ((mole.l-1)4)
        real(kind = dp) :: kpfe3       ! L  specific iron(III) precipitation rate (gFe.m-3b.d-1)
        real(kind = dp) :: kdfe3       ! L  specific iron(III) dissolution rate (d-1)
        real(kind = dp) :: kafe3       ! L  specific iron(III) aging rate (d-1)
        real(kind = dp) :: ksp2        ! L  solubility product for FeS ((mole.l-1)2)
        real(kind = dp) :: csd3        ! L  dissolved free sulphide concentration (mole.l-l)
        real(kind = dp) :: cfe2d       ! L  equilibrium dissolved free iron(II) concentration (mole.l-1)
        real(kind = dp) :: iap2        ! L  ion activity product for FeS ((mole.l-1)2)
        real(kind = dp) :: kpfes       ! L  specific FeS precipitation rate (gFe.m-3b.d-1)
        real(kind = dp) :: kdfes       ! L  specific FeS dissolution rate (d-1)
        real(kind = dp) :: ksp3        ! L  solubility product for FeCO3 ((mole.l-1)2)
        real(kind = dp) :: cco3d       ! L  total dissolved free carbonate concentration (mole.l-l)
        real(kind = dp) :: iap3        ! L  ion activity product for FeCO3 ((mole.l-1)2)
        real(kind = dp) :: kpfeco3     ! L  specific FeCO3 precipitation rate (gFe.m-3b.d-1)
        real(kind = dp) :: kdfeco3     ! L  specific FeCO3 dissolution rate (d-1)
        real(kind = dp) :: kpyr        ! L  specific pyrite formation rate (gS-1.m3.d-1)

        ! initialise pointering in pmsa array

        ipnt = ipoint
        idpfe3 = 1
        iddfe3 = 2
        idafe3 = 3
        idpfes = 4
        iddfes = 5
        idpfeco3 = 6
        iddfeco3 = 7
        idpyr = 8

        do iseg = 1, noseg

            feiiipa = pmsa(ipnt(1))
            feiiid = pmsa(ipnt(2))
            fes = pmsa(ipnt(3))
            feiid = pmsa(ipnt(4))
            feco3 = pmsa(ipnt(5))
            sud = pmsa(ipnt(6))
            tic = pmsa(ipnt(7))
            co2 = pmsa(ipnt(8))
            frfe3dis = pmsa(ipnt(9))
            lkspfeoh3 = pmsa(ipnt(10))
            rcagfe320 = pmsa(ipnt(11))
            rcdisfe320 = pmsa(ipnt(12))
            rcprcfe320 = pmsa(ipnt(13))
            tcagfe3 = pmsa(ipnt(14))
            tcdisfe3 = pmsa(ipnt(15))
            tcprcfe3 = pmsa(ipnt(16))
            frfe2dis = pmsa(ipnt(17))
            frh2sdis = pmsa(ipnt(18))
            frs2dis = pmsa(ipnt(19))
            frco3dis = pmsa(ipnt(20))
            lkspfes = pmsa(ipnt(21))
            lkspfeco3 = pmsa(ipnt(22))
            rcpyrite20 = pmsa(ipnt(23))
            rcdisfes20 = pmsa(ipnt(24))
            rcprcfes20 = pmsa(ipnt(25))
            rcdisfec20 = pmsa(ipnt(26))
            rcprcfec20 = pmsa(ipnt(27))
            tcpyrite = pmsa(ipnt(28))
            tcdisfes = pmsa(ipnt(29))
            tcprcfes = pmsa(ipnt(30))
            tcdisfeco3 = pmsa(ipnt(31))
            tcprcfeco3 = pmsa(ipnt(32))
            swticco2 = nint(pmsa(ipnt(33)))
            ph = pmsa(ipnt(34))
            temp = pmsa(ipnt(35))
            delt = pmsa(ipnt(36))
            poros = pmsa(ipnt(37))

            ! use tic or co2 depending on the switch

            if (swticco2 == 1) then
                tic = co2 * 12. / 44.
            endif

            ! precipitation and dissolution of iron(III)

            ksp1 = 10.**lkspfeoh3
            cfe3d = frfe3dis * feiiid * (1. / (56000. * poros))
            oh = 10**(ph - 14.)
            iap1 = cfe3d * oh * oh * oh

            if (iap1 >= ksp1) then

                ! pecipitation

                kpfe3 = rcprcfe320 * tcprcfe3**(temp - 20.)
                fpfe3 = kpfe3 * (iap1 / ksp1 - 1.0) * poros
                if (fpfe3 > feiiid / delt) fpfe3 = 0.5 * feiiid / delt
                fdfe3 = 0.0

            else

                ! dissolution

                kdfe3 = rcdisfe320 * tcdisfe3**(temp - 20.)
                fdfe3 = kdfe3 * feiiipa * (1.0 - iap1 / ksp1)
                if (fdfe3 > feiiipa / delt) fdfe3 = 0.5 * feiiipa / delt
                fpfe3 = 0.0

            endif

            ! aging of iron(III)

            kafe3 = rcagfe320 * tcagfe3**(temp - 20.)
            fafe3 = kafe3 * feiiipa
            dafe3 = fafe3

            ! precipitation and dissolution of iron(II)
            ! iron sulphide

            ksp2 = 10.**lkspfes
            csd3 = frs2dis * sud * (1. / (32000 * poros))
            cfe2d = frfe2dis * feiid * (1. / (56000 * poros))
            iap2 = cfe2d * csd3

            if (iap2 >= ksp2) then

                ! pecipitation

                kpfes = rcprcfes20 * tcprcfes**(temp - 20.)
                fpfes = kpfes * (iap2 / ksp2 - 1.0) * poros
                if (fpfes > feiid / delt) fpfes = 0.5 * feiid / delt
                fdfes = 0.0

            else

                ! dissolution

                kdfes = rcdisfes20 * tcdisfes**(temp - 20.)
                fdfes = kdfes * fes * (1.0 - iap2 / ksp2)
                if (fdfes > fes / delt) fdfes = 0.5 * fes / delt
                fpfes = 0.0

            endif

            ! iron carbonate formation

            ksp3 = 10.**lkspfeco3
            cco3d = frco3dis * tic * (1. / (12000 * poros))
            iap3 = cfe2d * cco3d

            if (iap3 >= ksp3) then

                ! pecipitation

                kpfeco3 = rcprcfec20 * tcprcfeco3**(temp - 20.)
                fpfeco3 = kpfeco3 * (iap3 / ksp3 - 1.0) * poros
                if (fpfeco3 > feiid / delt) fpfeco3 = 0.5 * feiid / delt
                fdfeco3 = 0.0

            else

                ! dissolution

                kdfeco3 = rcdisfec20 * tcdisfeco3**(temp - 20.)
                fdfeco3 = kdfeco3 * feco3 * (1.0 - iap3 / ksp3)
                if (fdfeco3 > feco3 / delt) fdfeco3 = 0.5 * feco3 / delt
                fpfeco3 = 0.0

            endif

            ! formation of pyrite

            kpyr = rcpyrite20 * tcpyrite**(temp - 20.)
            fpyr = kpyr * fes * frh2sdis * sud / poros

            ! store result in flux and pmsa array

            fl  (idpfe3) = fpfe3
            fl  (iddfe3) = fdfe3
            fl  (idafe3) = fafe3
            fl  (idpfes) = fpfes
            fl  (iddfes) = fdfes
            fl  (idpfeco3) = fpfeco3
            fl  (iddfeco3) = fdfeco3
            fl  (idpyr) = fpyr
            pmsa(ipnt(38)) = fpfe3
            pmsa(ipnt(39)) = fdfe3
            pmsa(ipnt(40)) = fafe3
            pmsa(ipnt(41)) = fpfes
            pmsa(ipnt(42)) = fdfes
            pmsa(ipnt(43)) = fpfeco3
            pmsa(ipnt(44)) = fdfeco3
            pmsa(ipnt(45)) = fpyr

            idpfe3 = idpfe3 + noflux
            iddfe3 = iddfe3 + noflux
            idafe3 = idafe3 + noflux
            idpfes = idpfes + noflux
            iddfes = iddfes + noflux
            idpfeco3 = idpfeco3 + noflux
            iddfeco3 = iddfeco3 + noflux
            idpyr = idpyr + noflux
            ipnt = ipnt + increm

        end do

        return
    end

end module m_priron
