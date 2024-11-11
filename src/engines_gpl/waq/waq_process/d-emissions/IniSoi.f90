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
module m_inisoi
   use m_waq_precision
   use m_extract_waq_attribute
   use m_demissions_input_checks, only: check_fraction, warn_below_minimum

   implicit none

contains

   subroutine inisoi(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)
!     D-EM Preprocessor to initialize top soil (Unp)

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

      integer(kind=int_wp) :: iseg, iflux, iatt1

      ! PMSA admin
      integer(kind=int_wp), parameter :: lins = 10
      integer(kind=int_wp), parameter :: louts = 1
      integer(kind=int_wp) :: ipnt(lins + louts) ! Local work array for the pointering

      ! pointers to concrete items
      integer(kind=int_wp), parameter :: ip_delt = 1
      integer(kind=int_wp), parameter :: ip_fUnpaved = 2
      integer(kind=int_wp), parameter :: ip_Thickness = 3
      integer(kind=int_wp), parameter :: ip_Poros = 4
      integer(kind=int_wp), parameter :: ip_RhoDM = 5
      integer(kind=int_wp), parameter :: ip_QTopSoil = 6
      integer(kind=int_wp), parameter :: ip_Area = 7
      integer(kind=int_wp), parameter :: ip_FrPas = 8
      integer(kind=int_wp), parameter :: ip_Soi = 9
      integer(kind=int_wp), parameter :: ip_Sop = 10
      integer(kind=int_wp), parameter :: ip_QSim = 11

      ! input and output items
      real(kind=real_wp) :: Delt
      real(kind=real_wp) :: fUnpaved
      real(kind=real_wp) :: Thicknessmm
      real(kind=real_wp) :: Thickness
      real(kind=real_wp) :: Poros
      real(kind=real_wp) :: RhoDM
      real(kind=real_wp) :: QTopSoil
      real(kind=real_wp) :: Area, FrPas
      real(kind=real_wp) :: Soi, Sop
      real(kind=real_wp) :: QSim

      real(kind=real_wp) :: fluxinit, massinit, soildm

      logical first
      data first/.true./

      save
      delt = pmsa(ipoint(ip_delt))

      ! loop for processing
      ipnt = ipoint(1:lins + louts)
      iflux = 0

      do iseg = 1, noseg

         call extract_waq_attribute(1, iknmrk(iseg), iatt1) ! pick up first attribute
         if (iatt1 > 0) then

            fUnpaved = pmsa(ipnt(ip_fUnpaved))
            call check_fraction(fUnpaved, "fUnpaved", iseg)
            Thicknessmm = pmsa(ipnt(ip_Thickness))
            call warn_below_minimum(Thicknessmm, 100.0, "SoilThick", iseg)
            Thickness = Thicknessmm / 1000. ! from mm to m
            Poros = pmsa(ipnt(ip_Poros))
            call check_fraction(Poros, "SoilPoros", iseg)
            RhoDM = pmsa(ipnt(ip_RhoDM))
            QTopSoil = pmsa(ipnt(ip_QTopSoil))
            Area = pmsa(ipnt(ip_Area))
            FrPas = pmsa(ipnt(ip_FrPas))
            call check_fraction(FrPas, "FracPass", iseg)
            Soi = pmsa(ipnt(ip_Soi))
            Sop = pmsa(ipnt(ip_Sop))

            ! kg      m2   -        m          -      kg/m3
            soildm = Area * fUnpaved * Thickness * (1.-Poros) * RhoDM
            if (soildm > 1.0) then
               ! mg/kg       mg    / kg
               QSim = 1000.*(Soi + Sop) / soildm
            else
               QSim = 0.0
            end if
            pmsa(ipnt(ip_QSim)) = QSim

            if (first) then
               !  g        mg/kg     g/mg       kg
               massinit = QTopsoil / 1000.*soildm
               fluxinit = massinit / delt ! g/s
               fl(iflux + 1) = fluxinit * (1.0 - FrPas)
               fl(iflux + 2) = fluxinit * FrPas
            end if

         end if

         ipnt = ipnt + increm(1:lins + louts)
         iflux = iflux + noflux

      end do

      first = .false.
      !******************************************************************************* NO PROCESSING in TIME LOOP

   end subroutine inisoi
end module m_inisoi
