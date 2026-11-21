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
module m_soipho
   use m_waq_precision
   use m_demissions_input_checks, only: check_fraction

   implicit none

contains

   subroutine soipho(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)
      !*******************************************************************************
      !     D-EM Preprocessor to initialize Nitrogen coefficients
      !
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
      !*******************************************************************************

      integer(kind=int_wp) :: iseg

      ! PMSA admin
      integer(kind=int_wp), parameter :: lins = 6
      integer(kind=int_wp), parameter :: louts = 2
      integer(kind=int_wp) :: ipnt(lins + louts) ! Local work array for the pointering

      ! index of pointers to concrete items
      integer(kind=int_wp), parameter :: i_ptot = 1
      integer(kind=int_wp), parameter :: i_plab = 2
      integer(kind=int_wp), parameter :: i_thick = 3
      integer(kind=int_wp), parameter :: i_poros = 4
      integer(kind=int_wp), parameter :: i_dmden = 5
      integer(kind=int_wp), parameter :: i_cbakr = 6
      integer(kind=int_wp), parameter :: i_qtot = 7
      integer(kind=int_wp), parameter :: i_cbak = 8

      ! input and output items
      real(kind=real_wp) :: ptot
      real(kind=real_wp) :: plab
      real(kind=real_wp) :: cbakr
      real(kind=real_wp) :: thick
      real(kind=real_wp) :: poros
      real(kind=real_wp) :: dmden
      real(kind=real_wp) :: qtot
      real(kind=real_wp) :: qlab
      real(kind=real_wp) :: cbak

      logical first
      data first/.true./
      save

      if (.not. first) return

      ! loop for processing
      ipnt = ipoint(1:lins + louts)

      do iseg = 1, noseg
         ptot = max(pmsa(ipnt(i_ptot)), 0.0)
         plab = max(pmsa(ipnt(i_plab)), 0.0)
         thick = pmsa(ipnt(i_thick))
         poros = min(pmsa(ipnt(i_poros)), 0.99)
         call check_fraction(poros, "SoilPoros", iseg)
         dmden = max(pmsa(ipnt(i_dmden)), 100.)
         cbakr = pmsa(ipnt(i_cbakr))

         ! mg/kg        g/m2      m              kg/m3
         qtot = 1000.*ptot / (thick * (1.-poros) * dmden)
         qlab = 1000.*plab / (thick * (1.-poros) * dmden)
         cbak = cbakr * qlab

         pmsa(ipnt(i_qtot)) = qtot
         pmsa(ipnt(i_cbak)) = cbak

         ipnt = ipnt + increm(1:lins + louts)
      end do

      first = .false.
   end subroutine soipho
end module m_soipho
