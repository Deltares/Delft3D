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
module m_modinp
   implicit none
contains
   subroutine MODINP(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)
!*******************************************************************************
!
      implicit none
!
!     Type    Name         I/O Description
!
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
!
!*******************************************************************************
!     DELWAQ ITEM MODIFICATOR FOR CC PROJECTIONS
!

!
!     Type    Name         I/O Description                                        Unit
!
      integer :: iseg

      ! PMSA admin
      integer :: ip1, ip2, ip3, ip4, ip5, ip6, ip7
      integer :: in1, in2, in3, in4, in5, in6, in7

      ! input and output items
      real :: itemin, modvalue, itemout, dayno, refday, timescu, auxsys
      integer :: modmode, ip, imonth

      ! loop for processing
      ip1 = ipoint(13)
      ip2 = ipoint(14)
      ip3 = ipoint(15)
      ip4 = ipoint(16)
      ip5 = ipoint(17)
      ip6 = ipoint(18)
      ip7 = ipoint(19)

      in1 = increm(13)
      in2 = increm(14)
      in3 = increm(15)
      in4 = increm(16)
      in5 = increm(17)
      in6 = increm(18)
      in7 = increm(19)

      do iseg = 1, noseg

         timescu = pmsa(ip1)
         refday = pmsa(ip2)
         auxsys = pmsa(ip3)
         itemin = pmsa(ip4)
         modmode = nint(pmsa(ip5))

         ! decide on month / don't be too precize
         dayno = refday + timescu / auxsys
         dayno = mod(dayno, 365.25)
         imonth = int(dayno / (365.25 / 12.)) + 1
         imonth = max(min(imonth, 12), 1)

         ! let monthly values be first 12 inputs
         ip = ipoint(imonth) + (iseg - 1) * increm(imonth)
         modvalue = pmsa(ip)

         if (modmode == 1) then

            itemout = itemin + modvalue

         else

            itemout = itemin * modvalue

         end if

         pmsa(ip6) = itemout
         pmsa(ip7) = modvalue

         ip1 = ip1 + in1
         ip2 = ip2 + in2
         ip3 = ip3 + in3
         ip4 = ip4 + in4
         ip5 = ip5 + in5
         ip6 = ip6 + in6
         ip7 = ip7 + in7
      end do

      return
   end
end module m_modinp
