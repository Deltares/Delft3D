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
module m_soiprt
   use m_waq_precision
   use m_extract_waq_attribute

   implicit none

contains

   subroutine soiprt(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)

      !     D-EM Preprocessor to set soil partitioning
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
      !*******************************************************************************
      !     D-EM Preprocessor to set soil partitioning
      !

      !
      !     Type    Name         I/O Description                                        Unit
      !
      integer(kind=int_wp) :: iseg, iatt1

      ! PMSA admin
      integer(kind=int_wp), parameter :: lins = 5
      integer(kind=int_wp), parameter :: louts = 1
      integer(kind=int_wp) :: ipnt(lins + louts)    !    Local work array for the pointering

      ! pointers to concrete items

      integer(kind=int_wp), parameter :: ip_Poros = 1
      integer(kind=int_wp), parameter :: ip_RhoDM = 2
      integer(kind=int_wp), parameter :: ip_fOM = 3
      integer(kind=int_wp), parameter :: ip_Kd = 4
      integer(kind=int_wp), parameter :: ip_lKoc = 5
      integer(kind=int_wp), parameter :: ip_fDis = 6

      ! input and output items

      real(kind=real_wp) :: Poros
      real(kind=real_wp) :: RhoDM, fOM
      real(kind=real_wp) :: Kd, lKoc, Kdcalc
      real(kind=real_wp) :: fDis

      save

      ! loop for processing
      ipnt = ipoint(1:lins + louts)

      do iseg = 1, noseg

         call extract_waq_attribute(1, iknmrk(iseg), iatt1) ! pick up first attribute
         if (iatt1 > 0) then

            Poros = pmsa(ipnt(ip_Poros))
            RhoDM = pmsa(ipnt(ip_RhoDM))
            fOM = pmsa(ipnt(ip_fOM))
            Kd = pmsa(ipnt(ip_Kd))
            lKoc = pmsa(ipnt(ip_lKoc))

            ! simple partitioning
            if (Kd > 0.0) then
               ! m3/kg
               Kdcalc = Kd
            elseif (lKoc > 0.0) then ! convert from log(l/kg)
               Kdcalc = (10.**(lKoc)) / 1000.*fOM
            else
               Kdcalc = 0.0
            end if
            ! -                m3/kg          kg/m3
            fdis = 1./(1.+Kdcalc * (1.-Poros) * RhoDM)
            !
            !---- Output of module
            !
            pmsa(ipnt(ip_fDis)) = fDis

            ! end IF active column
         end if

         ipnt = ipnt + increm(1:lins + louts)

      end do

      !******************************************************************************* NO PROCESSING in TIME LOOP

   end subroutine soiprt
end module m_soiprt
