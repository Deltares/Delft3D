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
module m_soinit
   use m_waq_precision

   implicit none

contains

   subroutine soinit(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)
!     D-EM Preprocessor to initialize Nitrogen coefficients
!
!     Type    Name         I/O Description
!
      real(kind=real_wp) :: pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(kind=real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer(kind=int_wp) :: ipoint(*)  ! I  Array of pointers in pmsa to get and store the data
      integer(kind=int_wp) :: increm(*)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer(kind=int_wp) :: noseg       ! I  Number of computational elements in the whole model schematisation
      integer(kind=int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
      integer(kind=int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer(kind=int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer(kind=int_wp) :: noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer(kind=int_wp) :: noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer(kind=int_wp) :: noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer(kind=int_wp) :: noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
      integer(kind=int_wp) :: iseg

      ! PMSA admin
      integer(kind=int_wp), parameter :: lins = 6
      integer(kind=int_wp), parameter :: louts = 1
      integer(kind=int_wp) :: ipnt(lins + louts)    !    Local work array for the pointering

      ! pointers to concrete items
      integer(kind=int_wp), parameter :: ip_c1 = 1
      integer(kind=int_wp), parameter :: ip_c2 = 2
      integer(kind=int_wp), parameter :: ip_c3 = 3
      integer(kind=int_wp), parameter :: ip_f1 = 4
      integer(kind=int_wp), parameter :: ip_f2 = 5
      integer(kind=int_wp), parameter :: ip_f3 = 6
      integer(kind=int_wp), parameter :: ip_cout = 7

      ! input and output items
      real(kind=real_wp) :: c1, c2, c3
      real(kind=real_wp) :: f1, f2, f3
      real(kind=real_wp) :: cout

      logical first
      data first/.true./
      save

      if (.not. first) return

      ! loop for processing
      ipnt = ipoint(1:lins + louts)

      do iseg = 1, noseg

         c1 = max(pmsa(ipnt(ip_c1)), 0.0)
         c2 = max(pmsa(ipnt(ip_c2)), 0.0)
         c3 = max(pmsa(ipnt(ip_c3)), 0.0)
         f1 = pmsa(ipnt(ip_f1))
         f2 = pmsa(ipnt(ip_f2))
         f3 = pmsa(ipnt(ip_f3))

         ! unit conversion, cg/kg to mg/kg
         cout = (f1 * max(c1, 0.0) + f2 * max(c2, 0.0) + f3 * max(c3, 0.0)) * 10.0

         pmsa(ipnt(ip_cout)) = cout

         ipnt = ipnt + increm(1:lins + louts)
      end do

      first = .false.
   end subroutine soinit
end module m_soinit
