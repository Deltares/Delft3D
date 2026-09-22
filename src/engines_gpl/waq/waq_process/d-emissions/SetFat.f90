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
module m_setfat
   use m_waq_precision
   use m_extract_waq_attribute

   implicit none

contains

   subroutine setfat(pmsa, fl, ipoint, increm, noseg, &
                     noflux, iexpnt, iknmrk, noq1, noq2, &
                     noq3, noq4)
!     D-EM Preprocessor to initialize Nitrogen coefficients
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
      integer(kind=int_wp) :: iseg, iatt1

      ! PMSA admin
      integer(kind=int_wp), parameter :: lins = 11
      integer(kind=int_wp), parameter :: louts = 5
      integer(kind=int_wp) :: ipnt(lins + louts)    !    Local work array for the pointering

      ! pointers to concrete items
      integer(kind=int_wp), parameter :: ip_decpav20 = 1
      integer(kind=int_wp), parameter :: ip_decunp20 = 2
      integer(kind=int_wp), parameter :: ip_decsoi20 = 3
      integer(kind=int_wp), parameter :: ip_kdunpa20 = 4
      integer(kind=int_wp), parameter :: ip_kdsoi20 = 5
      integer(kind=int_wp), parameter :: ip_theta_decpav = 6
      integer(kind=int_wp), parameter :: ip_theta_decunp = 7
      integer(kind=int_wp), parameter :: ip_theta_decsoi = 8
      integer(kind=int_wp), parameter :: ip_theta_kdunpa = 9
      integer(kind=int_wp), parameter :: ip_theta_kdsoi = 10
      integer(kind=int_wp), parameter :: ip_temp = 11

      integer(kind=int_wp), parameter :: ip_decpav = 12
      integer(kind=int_wp), parameter :: ip_decunp = 13
      integer(kind=int_wp), parameter :: ip_decsoi = 14
      integer(kind=int_wp), parameter :: ip_kdunpa = 15
      integer(kind=int_wp), parameter :: ip_kdsoi = 16

      ! input and output items
      real(kind=real_wp) :: decpav20
      real(kind=real_wp) :: decunp20
      real(kind=real_wp) :: decsoi20
      real(kind=real_wp) :: kdunpa20
      real(kind=real_wp) :: kdsoi20
      real(kind=real_wp) :: theta_decpav
      real(kind=real_wp) :: theta_decunp
      real(kind=real_wp) :: theta_decsoi
      real(kind=real_wp) :: theta_kdunpa
      real(kind=real_wp) :: theta_kdsoi
      real(kind=real_wp) :: temp

      real(kind=real_wp) :: decpav
      real(kind=real_wp) :: decunp
      real(kind=real_wp) :: decsoi
      real(kind=real_wp) :: kdunpa
      real(kind=real_wp) :: kdsoi

      save

      ! loop for processing
      ipnt = ipoint(1:lins + louts)

      do iseg = 1, noseg

         call extract_waq_attribute(1, iknmrk(iseg), iatt1) ! pick up first attribute
         if (iatt1 > 0) then

            decpav20 = pmsa(ipnt(ip_decpav20))
            decunp20 = pmsa(ipnt(ip_decunp20))
            decsoi20 = pmsa(ipnt(ip_decsoi20))
            kdunpa20 = pmsa(ipnt(ip_kdunpa20))
            kdsoi20 = pmsa(ipnt(ip_kdsoi20))
            theta_decpav = pmsa(ipnt(ip_theta_decpav))
            theta_decunp = pmsa(ipnt(ip_theta_decunp))
            theta_decsoi = pmsa(ipnt(ip_theta_decsoi))
            theta_kdunpa = pmsa(ipnt(ip_theta_kdunpa))
            theta_kdsoi = pmsa(ipnt(ip_theta_kdsoi))
            temp = pmsa(ipnt(ip_temp))

            temp = max(min(temp, 30.), 0.)
            decpav = decpav20 * (theta_decpav**(temp - 20.))
            decunp = decunp20 * (theta_decunp**(temp - 20.))
            decsoi = decsoi20 * (theta_decsoi**(temp - 20.))
            ! The temperature effect is on the dissolved fraction
            kdunpa = kdunpa20 * (theta_kdunpa**(temp - 20.))
            kdsoi = kdsoi20 * (theta_kdsoi**(temp - 20.))

            pmsa(ipnt(ip_decpav)) = decpav
            pmsa(ipnt(ip_decunp)) = decunp
            pmsa(ipnt(ip_decsoi)) = decsoi
            pmsa(ipnt(ip_kdunpa)) = kdunpa
            pmsa(ipnt(ip_kdsoi)) = kdsoi

            ! end IF active column
         end if

         ipnt = ipnt + increm(1:lins + louts)

      end do

      !******************************************************************************* NO PROCESSING in TIME LOOP

   end subroutine setfat
end module m_setfat
