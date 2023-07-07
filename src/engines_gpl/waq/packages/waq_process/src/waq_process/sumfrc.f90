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

      subroutine sumfrc     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )

      ! delwaq-pl routine to make a summation over a variable number of fractions

      implicit none

      ! declaration of the arguments

      real(4)               :: pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4)               :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer               :: ipoint(*)   ! I  Array of pointers in pmsa to get and store the data
      integer               :: increm(*)   ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer               :: noseg       ! I  Number of computational elements in the whole model schematisation
      integer               :: noflux      ! I  Number of fluxes, increment in the fl array
      integer               :: iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer               :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer               :: noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer               :: noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer               :: noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer               :: noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

      ! variables from pmsa array

      integer               :: nfrac       ! I  number of fractions to be summed
      real(4)               :: frac        ! I  fraction to be summed
      real(4)               :: sum         ! O  sum of the fractions
      real(4)               :: rfrac       ! O  relative fraction

      ! local variables

      integer               :: npnt        !    number of pointers in pmsa
      integer, allocatable  :: ipnt(:)     !    local work array for the pointering
      integer               :: iseg        !    loop counter for computational element loop
      integer               :: ifrac       !    loop counter for fractions

      ! initialise pointering in pmsa

      nfrac = pmsa( ipoint(1) )
      npnt  = 2*nfrac + 2
      allocate(ipnt(npnt))
      ipnt  = ipoint(1:npnt)

      ! loop over all segments

      do iseg = 1 , noseg

         ! sum the fractions

         sum = 0.0
         do ifrac = 1, nfrac
            frac = pmsa(ipnt(1+ifrac))
            sum  = sum + frac
         enddo

         ! store sum in pmsa

         pmsa(ipnt(nfrac+2)) = sum

         ! calculate relative fractions

         if ( abs(sum) .lt. 1e-20 ) then
            rfrac = 1./nfrac
            do ifrac = 1, nfrac
               pmsa(ipnt(nfrac+2+ifrac)) = rfrac
            enddo
         else
            do ifrac = 1, nfrac
               rfrac = pmsa(ipnt(1+ifrac))/sum
               pmsa(ipnt(nfrac+2+ifrac)) = rfrac
            enddo
         endif

         ! update pointering

         ipnt             = ipnt + increm(1:npnt)

      enddo

      return
      end
