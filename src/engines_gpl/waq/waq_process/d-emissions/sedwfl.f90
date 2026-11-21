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
module m_sedwfl
   use m_extract_waq_attribute
contains
   subroutine sedwfl(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)

!>    Module to calculate settling as a transport process of a single component

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
      integer ip1, ip2, ip3, ip4, ip5, ip6, ip7, ip8
      integer in1, in2, in3, in4, in5, in6, in7, in8
      integer ioq, iseg, ikmrk1, ikmrk2, iflux
      real settling, shear_stress, taucr, surface, rousecr
      real actual_settling, settling_velo, conc, depth, alfa, settling_flux
      real, parameter :: prandtlschmidt = 0.7
      real, parameter :: vonkarman = 0.41
      real, parameter :: rhowater = 1000.

      ip1 = ipoint(1)
      ip2 = ipoint(2)
      ip3 = ipoint(3)
      ip4 = ipoint(4)
      ip5 = ipoint(5)
      ip6 = ipoint(6)
      ip7 = ipoint(7)
      ip8 = ipoint(8)

      in1 = increm(1)
      in2 = increm(2)
      in3 = increm(3)
      in4 = increm(4)
      in5 = increm(5)
      in6 = increm(6)
      in7 = increm(7)
      in8 = increm(8)

      ! zero settling in horizontal direction
      do ioq = 1, 3 * noseg
         pmsa(ip8) = 0.0
         ip8 = ip8 + in8
      end do

      iflux = 0
      do iseg = 1, noseg
         call extract_waq_attribute(1, iknmrk(iseg), ikmrk1)
         if (ikmrk1 == 1) then
            call extract_waq_attribute(2, iknmrk(iseg), ikmrk2)
            if (ikmrk2 <= 4) then ! surface water

               conc = pmsa(ip1)
               settling = pmsa(ip2)
               shear_stress = pmsa(ip3)
               rousecr = pmsa(ip4)
               surface = pmsa(ip5)
               depth = pmsa(ip6)
               !
               ! Probability of settling
               !
               taucr = (settling / 86400.*prandtlschmidt / vonkarman / rousecr)**2 * rhowater
               actual_settling = settling * max(1.0 - shear_stress / taucr, 0.0)
               settling_flux = actual_settling * max(conc, 0.0)
               settling_velo = actual_settling / 86400.*surface

               ! set output and fluxes
               pmsa(ip7) = settling_flux
               fl(iflux + 1) = settling_flux / depth

               ! assume there is a vertical exchange for all segments
               pmsa(ip8) = settling_velo
            end if
         end if
         !
         ! Increment the pointers
         !
         ip1 = ip1 + in1
         ip2 = ip2 + in2
         ip3 = ip3 + in3
         ip4 = ip4 + in4
         ip5 = ip5 + in5
         ip6 = ip6 + in6
         ip7 = ip7 + in7
         ip8 = ip8 + in8
         iflux = iflux + noflux
      end do
   end subroutine sedwfl
end module m_sedwfl
