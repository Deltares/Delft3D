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

      subroutine IRONRE     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!JVB$ ATTRIBUTES DLLEXPORT, ALIAS: 'IRONRE' :: IRONRE
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 17) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 17) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 17)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(4) fes         ! I  iron(II) sulphide                                  (gFe/m3)
      real(4) feiiipa     ! I  particulate amorphous oxidizing iron               (gFe/m3)
      real(4) feiiipc     ! I  particulate crystalline oxidizing iron             (gFe/m3)
      real(4) sud         ! I  total dissolved sulphide (SUD)                     (gS/m3)
      real(4) frh2sdis    ! I  fraction of dissolved hydrogen sulphide            (-)
      real(4) rcfeah2s20  ! I  rate of amorphous iron red. with H2S               (m3/gS/d)
      real(4) rcfech2s20  ! I  rate of crystalline iron red. with H2S             (m3/gS/d)
      real(4) rcfeafes20  ! I  rate of amorphous iron red. with FeS               (m3/gFe/d)
      real(4) rcfecfes20  ! I  rate of crystalline iron red. with FeS             (m3/gFe/d)
      real(4) tcfered     ! I  temperature coeff. for abiotic iron reduction      (-)
      real(4) temp        ! I  ambient water temperature                          (oC)
      real(4) delt        ! I  timestep for processes                             (d)
      real(4) poros       ! I  volumetric porosity                                (-)
      real(4) fire1       ! O  rate of amorphous iron red. with H2S               (gFe/m3/d)
      real(4) fire2       ! O  rate of crystalline iron red. with H2S             (gFe/m3/d)
      real(4) fire3       ! O  rate of amorphous iron red. with FeS               (gFe/m3/d)
      real(4) fire4       ! O  rate of crystalline iron red. with FeS             (gFe/m3/d)
      real(4) dire1       ! F  rate of amorphous iron red. with H2S               (gFe/m3/d)
      real(4) dire2       ! F  rate of crystalline iron red. with H2S             (gFe/m3/d)
      real(4) dire3       ! F  rate of amorphous iron red. with FeS               (gFe/m3/d)
      real(4) dire4       ! F  rate of crystalline iron red. with FeS             (gFe/m3/d)
      integer idire1      !    Pointer to the rate of amorphous iron red. with H2S
      integer idire2      !    Pointer to the rate of crystalline iron red. with H2S
      integer idire3      !    Pointer to the rate of amorphous iron red. with FeS
      integer idire4      !    Pointer to the rate of crystalline iron red. with FeS
      real(4) tffered     ! L  temperature function for abiotic iron reduction
      real(4) kire1       ! L  rate of amorphous iron red. with H2S
      real(4) kire2       ! L  rate of crystalline iron red. with H2S
      real(4) kire3       ! L  rate of amorphous iron red. with FeS
      real(4) kire4       ! L  rate of crystalline iron red. with FeS

      ! initialise pointering in pmsa

      ipnt        = ipoint
      idire1      = 1
      idire2      = 2
      idire3      = 3
      idire4      = 4

      do 9000 iseg = 1 , noseg

         fes        = max(pmsa( ipnt(  1) ), 0.0 )
         feiiipa    = max(pmsa( ipnt(  2) ), 0.0 )
         feiiipc    = max(pmsa( ipnt(  3) ), 0.0 )
         sud        = max(pmsa( ipnt(  4) ), 0.0 )
         frh2sdis   = pmsa( ipnt(  5) )
         rcfeah2s20 = pmsa( ipnt(  6) )
         rcfech2s20 = pmsa( ipnt(  7) )
         rcfeafes20 = pmsa( ipnt(  8) )
         rcfecfes20 = pmsa( ipnt(  9) )
         tcfered    = pmsa( ipnt( 10) )
         temp       = pmsa( ipnt( 11) )
         delt       = pmsa( ipnt( 12) )
         poros      = pmsa( ipnt( 13) )

         ! temperature function

         tffered    = tcfered**(temp-20.0)

         ! temperature corrected rates

         kire1      = rcfeah2s20*tffered
         kire2      = rcfech2s20*tffered
         kire3      = rcfeafes20*tffered
         kire4      = rcfecfes20*tffered

         ! fluxes

         dire1      = kire1*feiiipa*frh2sdis*sud
         dire2      = kire2*feiiipc*frh2sdis*sud
         dire3      = kire3*fes*feiiipa
         dire4      = kire4*fes*feiiipc

         ! maximise fluxes if neccesary

         if ( dire1 + dire3 .gt. feiiipa/delt ) then
            dire1 = (dire1/(dire1+dire3))*0.5*feiiipa/delt
            dire3 = (dire3/(dire1+dire3))*0.5*feiiipa/delt
         endif
         if ( dire2 + dire4 .gt. feiiipc/delt ) then
            dire2 = (dire2/(dire2+dire4))*0.5*feiiipc/delt
            dire3 = (dire4/(dire2+dire4))*0.5*feiiipc/delt
         endif
         if ( dire1 + dire2 .gt. 0.0714*frh2sdis*sud/delt ) then
            dire1 = (dire1/(dire1+dire2))*0.5*0.0714*frh2sdis*sud/delt
            dire2 = (dire2/(dire1+dire2))*0.5*0.0714*frh2sdis*sud/delt
         endif
         if ( dire3 + dire4 .gt. 0.125*fes/delt ) then
            dire3 = (dire3/(dire3+dire4))*0.5*0.125*fes/delt
            dire4 = (dire4/(dire3+dire4))*0.5*0.125*fes/delt
         endif

         fire1      = dire1
         fire2      = dire2
         fire3      = dire3
         fire4      = dire4

         ! store flux and pmsa

         fl  ( idire1      ) = dire1
         fl  ( idire2      ) = dire2
         fl  ( idire3      ) = dire3
         fl  ( idire4      ) = dire4
         pmsa( ipnt( 14)   ) = fire1
         pmsa( ipnt( 15)   ) = fire2
         pmsa( ipnt( 16)   ) = fire3
         pmsa( ipnt( 17)   ) = fire4

         idire1      = idire1      + noflux
         idire2      = idire2      + noflux
         idire3      = idire3      + noflux
         idire4      = idire4      + noflux
         ipnt        = ipnt        + increm

 9000 continue

      return
      end
