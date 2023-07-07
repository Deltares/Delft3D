!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      subroutine hdispa     ( pmsa   , fl     , ipoint , increm, noseg ,
     &                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     &                        noq3   , noq4   )
!>\file
!>       Facilitate variable (2D) horizontal dispersion defined per segment (instead of per exchanges as in #4)

!
!*******************************************************************************
!
      implicit none
!
!     type    name         i/o description
!
      real(4) pmsa(*)     !i/o process manager system array, window of routine to process library
      real(4) fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer ipoint(  2) ! i  array of pointers in pmsa to get and store the data
      integer increm(  2) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt(  2)   !    local work array for the pointering
      integer iq          !    local loop counter for exchanges
      integer iseg1       !    segment number from
      integer iseg2       !    segment number to

      ipnt        = ipoint

      do iq = 1 , noq1+noq2

         ! input on segments

         iseg1 = iexpnt(1,iq)
         iseg2 = iexpnt(2,iq)

         ! set output

         if ( iseg1 .gt. 0 .and. iseg2 .gt. 0 ) then
         ! if both are internal segments use the minimum value of both segments
            pmsa(ipnt(2)) = min(pmsa(ipnt(1)+(iseg1-1)*increm(1)),
     &                          pmsa(ipnt(1)+(iseg2-1)*increm(1)))
         else if ( iseg1 .gt. 0) then
         ! if only 'from' is an internal segment, use this one
            pmsa(ipnt(2)) = pmsa(ipnt(1)+(iseg1-1)*increm(1))
         else if ( iseg2 .gt. 0) then
         ! if only 'to' is an internal segment, use this one
            pmsa(ipnt(2)) = pmsa(ipnt(1)+(iseg2-1)*increm(1))
         else
         ! no internal node available, probably from zero to zero
            pmsa(ipnt(2)) = 0.0
         endif

         ! update pointering in pmsa

         ipnt(2) = ipnt(2) + increm(2)

      enddo

      return
      end
