!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      subroutine PRIRON     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!JVB$ ATTRIBUTES DLLEXPORT, ALIAS: 'PRIRON' :: PRIRON
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 45) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 45) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 45)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(8) feiiipa     ! I  particulate amorphous oxidizing iron               (gFe/m3)
      real(8) feiiid      ! I  dissolved oxidizing iron                           (gFe/m3)
      real(8) fes         ! I  iron(II) sulphide                                  (gFe/m3)
      real(8) feiid       ! I  total dissolved reducing iron                      (gFe/m3)
      real(8) feco3       ! I  iron(II) carbonate concentration                   (gFe/m3)
      real(8) sud         ! I  total dissolved sulphide (SUD)                     (gS/m3)
      real(8) tic         ! I  total inorganic carbonate                          (gC/m3)
      real(8) co2         ! I  CO2                                                (g/m3)
      real(8) frfe3dis    ! I  fraction dissolved free iron(III)                  (-)
      real(8) lkspfeoh3   ! I  log solubility product for Fe(OH)3                 (-)
      real(8) rcagfe320   ! I  specific iron(III) aging rate at 20 oC             (1/d)
      real(8) rcdisfe320  ! I  specific iron(III) dissolution rate at 20 oC       (1/d)
      real(8) rcprcfe320  ! I  specific iron(III) precipitation rate              (gFe/m3/d)
      real(8) tcagfe3     ! I  temperature coeff. for iron(III) aging             (-)
      real(8) tcdisfe3    ! I  temperature coeff. for iron(III) dissolution       (-)
      real(8) tcprcfe3    ! I  temperature coeff. for iron(III) precipitation     (-)
      real(8) frfe2dis    ! I  fraction dissolved free iron(II)                   (-)
      real(8) frh2sdis    ! I  fraction of dissolved hydrogen sulphide            (-)
      real(8) frs2dis     ! I  fraction dissolved free sulphide                   (-)
      real(8) frco3dis    ! I  fraction dissolved free carbonate                  (-)
      real(8) lkspfes     ! I  log solubility product for FeS                     (-)
      real(8) lkspfeco3   ! I  log solubility product for FeCO3                   (-)
      real(8) rcpyrite20  ! I  specific pyrite formation rate at 20 oC            (gS/m3/d)
      real(8) rcdisfes20  ! I  iron(II) sulphide dissolution rate                 (1/d)
      real(8) rcprcfes20  ! I  iron(II) sulphide precipitation rate               (gFe/m3/d)
      real(8) rcdisfec20  ! I  iron(II) carbonate dissolution rate                (1/d)
      real(8) rcprcfec20  ! I  iron(II) carbonate precipitation rate              (gFe/m3/d)
      real(8) tcpyrite    ! I  temperature coeff. for pyrite formation            (-)
      real(8) tcdisfes    ! I  temperature coeff. for iron(II) sulphide diss.     (-)
      real(8) tcprcfes    ! I  temperature coeff. for iron(II) sulphide prec.     (-)
      real(8) tcdisfeco3  ! I  temperature coeff. for iron(II) carbonate diss.    (-)
      real(8) tcprcfeco3  ! I  temperature coeff. for iron(II) carbonate prec.    (-)
      real(8) swticco2    ! I  switch (0=use TIC, 1=use CO2)                      (-)
      real(8) ph          ! I  pH                                                 (-)
      real(8) temp        ! I  ambient water temperature                          (oC)
      real(8) delt        ! I  timestep for processes                             (d)
      real(8) poros       ! I  volumetric porosity                                (-)
      real(8) fpfe3       ! O  rate of amorphous iron(III) precipitat.            (gFe/m3/d)
      real(8) fdfe3       ! O  rate of amorphous iron(III) dissolution            (gFe/m3/d)
      real(8) fafe3       ! O  rate of amorphous iron(III) aging                  (gFe/m3/d)
      real(8) fpfes       ! O  rate of iron(II) sulphide precipitation            (gFe/m3/d)
      real(8) fdfes       ! O  rate of iron(II) sulphide dissolution              (gFe/m3/d)
      real(8) fpfeco3     ! O  rate of iron(II) carbonate precipitation           (gFe/m3/d)
      real(8) fdfeco3     ! O  rate of iron(II) carbonate dissolution             (gFe/m3/d)
      real(8) fpyr        ! O  rate of pyrite formation                           (gFe/m3/d)
      real(8) dafe3       ! F  rate of amorphous iron(III) aging                  (gFe/m3/d)
      integer idpfe3      !    Pointer to the rate of amorphous iron(III) precipitat.
      integer iddfe3      !    Pointer to the rate of amorphous iron(III) dissolution
      integer idafe3      !    Pointer to the rate of amorphous iron(III) aging
      integer idpfes      !    Pointer to the rate of iron(II) sulphide precipitation
      integer iddfes      !    Pointer to the rate of iron(II) sulphide dissolution
      integer idpfeco3    !    Pointer to the rate of iron(II) carbonate precipitation
      integer iddfeco3    !    Pointer to the rate of iron(II) carbonate dissolution
      integer idpyr       !    Pointer to the rate of pyrite formation
      real(8) ksp1        ! L  solubility product for Fe(OH)3 ((mole.l-1)4)
      real(8) cfe3d       ! L  equilibrium dissolved free iron(III) concentration (mole.l-1)
      real(8) oh          ! L  hydroxyl concentration (mole.l-1)
      real(8) iap1        ! L  ion activity product for Fe(OH)3 ((mole.l-1)4)
      real(8) kpfe3       ! L  specific iron(III) precipitation rate (gFe.m-3b.d-1)
      real(8) kdfe3       ! L  specific iron(III) dissolution rate (d-1)
      real(8) kafe3       ! L  specific iron(III) aging rate (d-1)
      real(8) ksp2        ! L  solubility product for FeS ((mole.l-1)2)
      real(8) csd3        ! L  dissolved free sulphide concentration (mole.l-l)
      real(8) cfe2d       ! L  equilibrium dissolved free iron(II) concentration (mole.l-1)
      real(8) iap2        ! L  ion activity product for FeS ((mole.l-1)2)
      real(8) kpfes       ! L  specific FeS precipitation rate (gFe.m-3b.d-1)
      real(8) kdfes       ! L  specific FeS dissolution rate (d-1)
      real(8) ksp3        ! L  solubility product for FeCO3 ((mole.l-1)2)
      real(8) cco3d       ! L  total dissolved free carbonate concentration (mole.l-l)
      real(8) iap3        ! L  ion activity product for FeCO3 ((mole.l-1)2)
      real(8) kpfeco3     ! L  specific FeCO3 precipitation rate (gFe.m-3b.d-1)
      real(8) kdfeco3     ! L  specific FeCO3 dissolution rate (d-1)
      real(8) kpyr        ! L  specific pyrite formation rate (gS-1.m3.d-1)

      ! initialise pointering in pmsa array

      ipnt        = ipoint
      idpfe3      = 1
      iddfe3      = 2
      idafe3      = 3
      idpfes      = 4
      iddfes      = 5
      idpfeco3    = 6
      iddfeco3    = 7
      idpyr       = 8

      do 9000 iseg = 1 , noseg

         feiiipa    = pmsa( ipnt(  1) )
         feiiid     = pmsa( ipnt(  2) )
         fes        = pmsa( ipnt(  3) )
         feiid      = pmsa( ipnt(  4) )
         feco3      = pmsa( ipnt(  5) )
         sud        = pmsa( ipnt(  6) )
         tic        = pmsa( ipnt(  7) )
         co2        = pmsa( ipnt(  8) )
         frfe3dis   = pmsa( ipnt(  9) )
         lkspfeoh3  = pmsa( ipnt( 10) )
         rcagfe320  = pmsa( ipnt( 11) )
         rcdisfe320 = pmsa( ipnt( 12) )
         rcprcfe320 = pmsa( ipnt( 13) )
         tcagfe3    = pmsa( ipnt( 14) )
         tcdisfe3   = pmsa( ipnt( 15) )
         tcprcfe3   = pmsa( ipnt( 16) )
         frfe2dis   = pmsa( ipnt( 17) )
         frh2sdis   = pmsa( ipnt( 18) )
         frs2dis    = pmsa( ipnt( 19) )
         frco3dis   = pmsa( ipnt( 20) )
         lkspfes    = pmsa( ipnt( 21) )
         lkspfeco3  = pmsa( ipnt( 22) )
         rcpyrite20 = pmsa( ipnt( 23) )
         rcdisfes20 = pmsa( ipnt( 24) )
         rcprcfes20 = pmsa( ipnt( 25) )
         rcdisfec20 = pmsa( ipnt( 26) )
         rcprcfec20 = pmsa( ipnt( 27) )
         tcpyrite   = pmsa( ipnt( 28) )
         tcdisfes   = pmsa( ipnt( 29) )
         tcprcfes   = pmsa( ipnt( 30) )
         tcdisfeco3 = pmsa( ipnt( 31) )
         tcprcfeco3 = pmsa( ipnt( 32) )
         swticco2   = nint(pmsa( ipnt( 33) ))
         ph         = pmsa( ipnt( 34) )
         temp       = pmsa( ipnt( 35) )
         delt       = pmsa( ipnt( 36) )
         poros      = pmsa( ipnt( 37) )

         ! use tic or co2 depending on the switch

         if ( swticco2 .eq. 1 ) then
            tic  = co2*12./44.
         endif

         ! precipitation and dissolution of iron(III)

         ksp1  = 10.**lkspfeoh3
         cfe3d = frfe3dis*feiiid*(1./(56000.*poros))
         oh    = 10**(ph-14.)
         iap1  = cfe3d*oh*oh*oh

         if ( iap1 .ge. ksp1 ) then

            ! pecipitation

            kpfe3 = rcprcfe320*tcprcfe3**(temp-20.)
            fpfe3 = kpfe3*(iap1/ksp1-1.0)*poros
            if ( fpfe3 .gt. feiiid/delt ) fpfe3 = 0.5*feiiid/delt
            fdfe3 = 0.0

         else

            ! dissolution

            kdfe3 = rcdisfe320*tcdisfe3**(temp-20.)
            fdfe3 = kdfe3*feiiipa*(1.0-iap1/ksp1)
            if ( fdfe3 .gt. feiiipa/delt ) fdfe3 = 0.5*feiiipa/delt
            fpfe3 = 0.0

         endif

         ! aging of iron(III)

         kafe3      = rcagfe320*tcagfe3**(temp-20.)
         fafe3      = kafe3*feiiipa
         dafe3      = fafe3

         ! precipitation and dissolution of iron(II)
         ! iron sulphide

         ksp2  = 10.**lkspfes
         csd3  = frs2dis*sud*(1./(32000*poros))
         cfe2d = frfe2dis*feiid*(1./(56000*poros))
         iap2  = cfe2d*csd3

         if ( iap2 .ge. ksp2 ) then

            ! pecipitation

            kpfes = rcprcfes20*tcprcfes**(temp-20.)
            fpfes = kpfes*(iap2/ksp2-1.0)*poros
            if ( fpfes .gt. feiid/delt ) fpfes = 0.5*feiid/delt
            fdfes = 0.0

         else

            ! dissolution

            kdfes = rcdisfes20*tcdisfes**(temp-20.)
            fdfes = kdfes*fes*(1.0-iap2/ksp2)
            if ( fdfes .gt. fes/delt ) fdfes = 0.5*fes/delt
            fpfes = 0.0

         endif

         ! iron carbonate formation

         ksp3  = 10.**lkspfeco3
         cco3d = frco3dis*tic*(1./(12000*poros))
         iap3  = cfe2d*cco3d

         if ( iap3 .ge. ksp3 ) then

            ! pecipitation

            kpfeco3 = rcprcfec20*tcprcfeco3**(temp-20.)
            fpfeco3 = kpfeco3*(iap3/ksp3-1.0)*poros
            if ( fpfeco3 .gt. feiid/delt ) fpfeco3 = 0.5*feiid/delt
            fdfeco3 = 0.0

         else

            ! dissolution

            kdfeco3 = rcdisfec20*tcdisfeco3**(temp-20.)
            fdfeco3 = kdfeco3*feco3*(1.0-iap3/ksp3)
            if ( fdfeco3 .gt. feco3/delt ) fdfeco3 = 0.5*feco3/delt
            fpfeco3 = 0.0

         endif

         ! formation of pyrite

         kpyr  = rcpyrite20*tcpyrite**(temp-20.)
         fpyr  = kpyr*fes*frh2sdis*sud/poros

         ! store result in flux and pmsa array

         fl  ( idpfe3      ) = fpfe3
         fl  ( iddfe3      ) = fdfe3
         fl  ( idafe3      ) = fafe3
         fl  ( idpfes      ) = fpfes
         fl  ( iddfes      ) = fdfes
         fl  ( idpfeco3    ) = fpfeco3
         fl  ( iddfeco3    ) = fdfeco3
         fl  ( idpyr       ) = fpyr
         pmsa( ipnt( 38)   ) = fpfe3
         pmsa( ipnt( 39)   ) = fdfe3
         pmsa( ipnt( 40)   ) = fafe3
         pmsa( ipnt( 41)   ) = fpfes
         pmsa( ipnt( 42)   ) = fdfes
         pmsa( ipnt( 43)   ) = fpfeco3
         pmsa( ipnt( 44)   ) = fdfeco3
         pmsa( ipnt( 45)   ) = fpyr

         idpfe3      = idpfe3      + noflux
         iddfe3      = iddfe3      + noflux
         idafe3      = idafe3      + noflux
         idpfes      = idpfes      + noflux
         iddfes      = iddfes      + noflux
         idpfeco3    = idpfeco3    + noflux
         iddfeco3    = iddfeco3    + noflux
         idpyr       = idpyr       + noflux
         ipnt        = ipnt        + increm

 9000 continue

      return
      end
