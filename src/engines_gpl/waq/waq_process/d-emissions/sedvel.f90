!!  Copyright (C)  Stichting Deltares, 2012-2026.
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
module m_sedvel
contains
   subroutine sedvel(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)
!>    Module to calculate the weighted average settling velocity of sediments for the settling of sorbed substances

      implicit none

      real(4) pmsa(*) !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*) ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(*) ! I  Array of pointers in pmsa to get and store the data
      integer increm(*) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg ! I  Number of computational elements in the whole model schematisation
      integer noflux ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*) ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1 ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2 ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3 ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4 ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

!     local variables
      integer iseg !    Local loop counter for computational element loop
      integer ip1, ip2, ip3, ip4, ip5, ip6, ip7
      integer in1, in2, in3, in4, in5, in6, in7

      real im1, im2, im3, tim
      real v1, v2, v3
      real vout

      ip1 = ipoint(1)
      ip2 = ipoint(2)
      ip3 = ipoint(3)
      ip4 = ipoint(4)
      ip5 = ipoint(5)
      ip6 = ipoint(6)
      ip7 = ipoint(7)

      in1 = increm(1)
      in2 = increm(2)
      in3 = increm(3)
      in4 = increm(4)
      in5 = increm(5)
      in6 = increm(6)
      in7 = increm(7)

      do iseg = 1, noseg

         ! water concentrations of IM fractions and their settling velocity
         im1 = max(0.0, pmsa(ip1))
         v1 = pmsa(ip2)
         im2 = max(0.0, pmsa(ip3))
         v2 = pmsa(ip4)
         im3 = max(0.0, pmsa(ip5))
         v3 = pmsa(ip6)

         ! calculate the weighted average settling velocity of the mixture
         tim = im1 + im2 + im3
         if (tim > 0.0) then
            vout = (im1 * v1 + im2 * v2 + im3 * v3) / tim
         else
            vout = 0.0
         end if
         pmsa(ip7) = vout

         ip1 = ip1 + in1
         ip2 = ip2 + in2
         ip3 = ip3 + in3
         ip4 = ip4 + in4
         ip5 = ip5 + in5
         ip6 = ip6 + in6
         ip7 = ip7 + in7
      end do
   end subroutine sedvel
end module m_sedvel
