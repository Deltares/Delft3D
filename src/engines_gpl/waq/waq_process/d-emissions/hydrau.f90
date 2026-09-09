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
module m_hydrau
   use m_waq_precision

   implicit none

contains

   subroutine hydrau(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)
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

      !
      !     support variables
      integer(kind=int_wp), parameter :: npmsa = 9
      integer(kind=int_wp) :: ipnt(npmsa) !    Local work array for the pointering
      integer(kind=int_wp) :: iseg        !    Local loop counter for computational element loop
      integer(kind=int_wp) :: iflux       !    Local loop counter for computational element loop

      !     input items
      real(kind=real_wp) :: volume
      real(kind=real_wp) :: Surf
      real(kind=real_wp) :: Rivlen
      real(kind=real_wp) :: Chezy

      !     input for exchanges
      real(kind=real_wp) :: flow

      !     output items
      real(kind=real_wp) :: velocity
      real(kind=real_wp) :: depth
      real(kind=real_wp) :: tau
      !
      !*******************************************************************************

      ipnt = ipoint(1:npmsa)
      iflux = 0

      ! loop over segments

      do iseg = 1, noseg
         volume = pmsa(ipnt(1))
         surf = pmsa(ipnt(2))
         rivlen = pmsa(ipnt(3))
         chezy = pmsa(ipnt(4))
         depth = volume / surf

         !       THIS RELIES ON THE FIRST NOSEG EXCHANGES BEING THE OUTFLOWS OF THE SEGMENTS!!!!
         flow = pmsa(ipnt(5))
         if (rivlen > 1.) then
            velocity = abs(flow) / (volume / rivlen)
         else
            velocity = 0.1
         end if
         tau = 1000.*9.81 * velocity * velocity / chezy / chezy
         pmsa(ipnt(6)) = velocity
         pmsa(ipnt(7)) = depth
         pmsa(ipnt(8)) = depth
         pmsa(ipnt(9)) = tau

         ipnt = ipnt + increm(1:npmsa)
         iflux = iflux + noflux
      end do

   end subroutine hydrau
end module m_hydrau
