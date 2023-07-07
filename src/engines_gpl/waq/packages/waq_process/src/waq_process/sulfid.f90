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

      subroutine SULFID     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!>\file
!>       Speciation of dissolved sulphide (S= and HS-) in pore water

!
      implicit none
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 14) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 14) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 14)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(8) sud         ! I  total dissolved sulphide (SUD)                     (gS/m3)
      real(8) lksth2s     ! I  log acidity constant for H2S (l.mole-1)            (-)
      real(8) tcksth2s    ! I  temperature coefficient for KstH2S                 (-)
      real(8) lksths      ! I  log acidity constant for HS- (l.mole-1)            (-)
      real(8) tcksths     ! I  temperature coefficient for KstHS                  (-)
      real(8) ph          ! I  pH                                                 (-)
      real(8) temp        ! I  ambient water temperature                          (oC)
      real(8) poros       ! I  volumetric porosity                                (-)
      real(8) dish2swk    ! O  hydrogen sulphide concentration H2S                (mole/l)
      real(8) dishswk     ! O  (HS-) in water column                              (mole/l)
      real(8) disswk      ! O  (S--) in water column                              (mole/l)
      real(8) frh2sdis    ! O  fraction of dissolved hydrogen sulphide            (-)
      real(8) frhsdis     ! O  fraction (HS-) in water column                     (-)
      real(8) frsdis      ! O  fraction (S--) in water column                     (-)

      ! local declaration

      real(8) h_ion       ! L  proton concentration                               (mole/l)
      real(8) ks1         ! L  acidity hydrolyses equilibrium constant for H2CO3  (-)
      real(8) ks2         ! L  hydrolyses equilibrium constant for CO2            (-)
      real(8) csdt        ! L  total dissolved                                    (mole/l)
      real(8) csd1        ! L  dissolved H2S                                      (mole/l)
      real(8) csd2        ! L  dissolved HS                                       (mole/l)
      real(8) csd3        ! L  dissolved S                                        (mole/l)

      ! initialise pointering in pmsa

      ipnt        = ipoint

      do 9000 iseg = 1 , noseg

         sud        = pmsa( ipnt(  1) )
         lksth2s    = pmsa( ipnt(  2) )
         tcksth2s   = pmsa( ipnt(  3) )
         lksths     = pmsa( ipnt(  4) )
         tcksths    = pmsa( ipnt(  5) )
         ph         = pmsa( ipnt(  6) )
         temp       = pmsa( ipnt(  7) )
         poros      = pmsa( ipnt(  8) )

         if ( sud .gt. 1e-20 ) then

            ! speciation

            h_ion      = 10.**(-ph)
            ks1        = 10.**lksth2s * tcksth2s**(temp-20.)
            ks2        = 10.**lksths  * tcksths**(temp-20.)
            csdt       = sud/(32000.*poros)
            csd1       = csdt/(1.+ks1/h_ion+(ks1*ks2)/(h_ion*h_ion))
            csd2       = ks1*csd1/h_ion
            csd3       = csdt - csd1 - csd2

            dish2swk   = csd1
            dishswk    = csd2
            disswk     = csd3
            frh2sdis   = csd1/csdt
            frhsdis    = csd2/csdt
            frsdis     = 1.0 - frh2sdis - frhsdis
            if ( frsdis .lt. 0.0 ) then
               frsdis = csd3/csdt
            endif

         else

            dish2swk   = 0.0
            dishswk    = 0.0
            disswk     = 0.0
            frh2sdis   = 0.0
            frhsdis    = 0.0
            frsdis     = 0.0

         endif

         ! store in pmsa array

         pmsa( ipnt(  9)   ) = dish2swk
         pmsa( ipnt( 10)   ) = dishswk
         pmsa( ipnt( 11)   ) = disswk
         pmsa( ipnt( 12)   ) = frh2sdis
         pmsa( ipnt( 13)   ) = frhsdis
         pmsa( ipnt( 14)   ) = frsdis

         ipnt        = ipnt        + increm

 9000 continue

      return
      end
