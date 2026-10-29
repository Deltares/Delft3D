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
module m_diftem
   use m_waq_precision
   use m_demissions_input_checks, only: check_fraction

   implicit none

contains

   subroutine diftem(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)
!     TRANSPORT FOR MULTIMEDIA MODEL
      implicit none
!
!     Type    Name         I/O Description
!
      real(kind=real_wp) :: pmsa(*) !I/O Process Manager System Array, window of routine to process library
      real(kind=real_wp) :: fl(*) ! O  Array of fluxes made by this process in mass/volume/time
      integer(kind=int_wp) :: ipoint(*) ! I  Array of pointers in pmsa to get and store the data
      integer(kind=int_wp) :: increm(*) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer(kind=int_wp) :: noseg ! I  Number of computational elements in the whole model schematisation
      integer(kind=int_wp) :: noflux ! I  Number of fluxes, increment in the fl array
      integer(kind=int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer(kind=int_wp) :: iknmrk(*) ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer(kind=int_wp) :: noq1 ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer(kind=int_wp) :: noq2 ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer(kind=int_wp) :: noq3 ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_wp) :: noq4 ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

!
      !     support variables
      integer(kind=int_wp), parameter :: lins = 11
      integer(kind=int_wp), parameter :: line = 1
      integer(kind=int_wp), parameter :: louts = 3
      integer(kind=int_wp), parameter :: loute = 0
      integer(kind=int_wp), parameter :: npmsa = lins + line + louts + loute
      integer :: ipnt(npmsa)

      integer ioq1, ioq2, iseg, iflux
      real tflux

      !     input items
      integer(kind=int_wp), parameter :: ip_noexf1toW = 1
      integer(kind=int_wp), parameter :: ip_noexf2toW = 2
      integer(kind=int_wp), parameter :: ip_tempair = 3
      integer(kind=int_wp), parameter :: ip_volume = 4
      integer(kind=int_wp), parameter :: ip_cloudf = 5
      integer(kind=int_wp), parameter :: ip_period1 = 6
      integer(kind=int_wp), parameter :: ip_wsin1 = 7
      integer(kind=int_wp), parameter :: ip_period2 = 8
      integer(kind=int_wp), parameter :: ip_wsin2 = 9
      integer(kind=int_wp), parameter :: ip_delt = 10
      integer(kind=int_wp), parameter :: ip_tempmin = 11
      integer(kind=int_wp), parameter :: ip_flow = 12
      integer(kind=int_wp), parameter :: ip_cloudp = 13
      integer(kind=int_wp), parameter :: ip_wsout1 = 14
      integer(kind=int_wp), parameter :: ip_wsout2 = 15

      integer(kind=int_wp) noexf1toW
      integer(kind=int_wp) noexf2toW
      real(kind=real_wp) volume
      real(kind=real_wp) tempair
      real(kind=real_wp) cloudf
      real(kind=real_wp) period1
      real(kind=real_wp) ws1
      real(kind=real_wp) period2
      real(kind=real_wp) ws2
      real(kind=real_wp) delt
      real(kind=real_wp) tempmin
      real(kind=real_wp) flow1, flow2
      real(kind=real_wp) cloudp

      logical first
      data first/.true./
      save
!
!*******************************************************************************
!
      if (first) then
         ! input by definition independent of space and time, these parameters are picked up for the first time and the first cell only and saved
         noexf1toW = nint(pmsa(ipoint(ip_noexf1toW)))
         noexf2toW = nint(pmsa(ipoint(ip_noexf2toW)))
         delt = pmsa(ipoint(ip_delt))
         tempmin = pmsa(ipoint(ip_tempmin))
      end if

      ! LOOP OVER SEGMENTS
      ipnt = ipoint(1:npmsa)
      iflux = 0
      do iseg = 1, noseg
         ioq1 = (noexf1toW - 1) * noseg + iseg
         ioq2 = (noexf2toW - 1) * noseg + iseg
         flow1 = max(pmsa(ipoint(ip_flow) + (ioq1 - 1) * increm(ip_flow)), 0.0) ! m3/s
         flow2 = max(pmsa(ipoint(ip_flow) + (ioq2 - 1) * increm(ip_flow)), 0.0) ! m3/s
         tempair = pmsa(ipnt(ip_tempair))
         volume = pmsa(ipnt(ip_volume))

         ! buffer flux1 over period
         period1 = pmsa(ipnt(ip_period1)) / delt ! expressed as timesteps
         ws1 = pmsa(ipnt(ip_wsin1))
         ws1 = ((period1 - 1.0) * ws1 + tempair) / period1
         pmsa(ipnt(ip_wsin1)) = ws1

         ! buffer flux2 over period
         period2 = pmsa(ipnt(ip_period2)) / delt ! expressed as timesteps
         ws2 = pmsa(ipnt(ip_wsin2))
         ws2 = ((period2 - 1.0) * ws2 + tempair) / period2
         pmsa(ipnt(ip_wsin2)) = ws2

         tflux = (flow1 * 86400.*max(ws1, tempmin) + flow2 * 86400.*max(ws2, tempmin)) / volume
!        if (isnan(tflux)) write (*,*) 'DIFTEM: ',iseg
         fl(iflux + 1) = tflux

         cloudf = pmsa(ipnt(ip_cloudf))
         call check_fraction(cloudf, "CloudFrac", iseg)
         cloudp = cloudf * 100.
         pmsa(ipnt(ip_cloudp)) = cloudp

         ipnt = ipnt + increm(1:npmsa)
         iflux = iflux + noflux
      end do

      first = .false.

   end subroutine diftem
end module m_diftem
