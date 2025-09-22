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
module m_emlgen
   use m_waq_precision

   implicit none

contains

   subroutine emlgen(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)
!     module is generic, now relies on DELWAQ input in Block 7 for reading EM output
!     the interpolation algorithm is no longer needed
!     the complexity to read multiple comartments for a layered model is no longer needed and has been removed
!
!     Type    Name         I/O Description
!
      real(kind=real_wp) :: pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(kind=real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer(kind=int_wp) :: ipoint(*)   ! I  Array of pointers in pmsa to get and store the data
      integer(kind=int_wp) :: increm(*)   ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer(kind=int_wp) :: noseg       ! I  Number of computational elements in the whole model schematisation
      integer(kind=int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
      integer(kind=int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer(kind=int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer(kind=int_wp) :: noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer(kind=int_wp) :: noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer(kind=int_wp) :: noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_wp) :: noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

      !     support variables
      integer(kind=int_wp) iseg        !    Local loop counter for computational element loop
      integer(kind=int_wp) iflux
      integer(kind=int_wp) istoch, ip

      !     input items
      integer(kind=int_wp), parameter :: ip_volume = 1
      integer(kind=int_wp), parameter :: ip_wemis = 2
      integer(kind=int_wp), parameter :: ip_nstoch = 3
      integer(kind=int_wp), parameter :: ip_stoch0 = 3
      integer(kind=int_wp), parameter :: nstochmax = 5
      integer(kind=int_wp), parameter :: npmsa = ip_stoch0 + nstochmax

      !     input items
      real(kind=real_wp) :: volume        ! segment (bulk) volume
      real(kind=real_wp) :: wemis         ! emissions to water g/s
      integer(kind=int_wp) :: nstoch        ! # of active stochi rules in proces.asc table
      real(kind=real_wp) :: stoch         ! stochiometric rate

      integer(kind=int_wp) :: ipnt(npmsa)

!
!*******************************************************************************
!
!      ipnt = ipoint(1:npmsa)
      do iseg = 1, noseg

         ! water emissions and volume
         volume = pmsa(ipoint(ip_volume) + (iseg - 1) * increm(ip_volume))
         wemis = pmsa(ipoint(ip_wemis) + (iseg - 1) * increm(ip_wemis))

         ! distribute
         iflux = (iseg - 1) * noflux
         nstoch = nint(pmsa(ipoint(ip_nstoch) + (iseg - 1) * increm(ip_nstoch)))
         do istoch = 1, nstoch
            ip = ip_stoch0 + istoch
            stoch = pmsa(ipoint(ip) + (iseg - 1) * increm(ip))
            fl(iflux + istoch) = wemis * 86400./volume * stoch
         end do

      end do

   end subroutine emlgen
end module m_emlgen
