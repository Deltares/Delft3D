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
module m_stwcol
   implicit none
contains
   subroutine STWCOL(pmsa, fl, ipoint, increm, noseg, &
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
!     D-EM Preprocessor to initialize Nitrogen coefficients
!

!
!     Type    Name         I/O Description                                        Unit
!
      integer :: iseg

      ! PMSA admin
      integer :: ip1, ip2, ip3, ip4
      integer :: in1, in2, in3, in4

      ! input and output items
      real :: smod, smodlim, frainsew, frout

      ! loop for processing
      ip1 = ipoint(1)
      ip2 = ipoint(2)
      ip3 = ipoint(3)
      ip4 = ipoint(4)

      in1 = increm(1)
      in2 = increm(2)
      in3 = increm(3)
      in4 = increm(4)

      do iseg = 1, noseg

         smod = pmsa(ip1)
         smodlim = pmsa(ip2)
         frainsew = pmsa(ip3)

         frout = 0.0
         if (smod > smodlim) frout = frainsew

         pmsa(ip4) = frout

         ip1 = ip1 + in1
         ip2 = ip2 + in2
         ip3 = ip3 + in3
         ip4 = ip4 + in4
      end do

      return
   end
end module m_stwcol
