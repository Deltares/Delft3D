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

      subroutine hdispv     ( pmsa   , fl     , ipoint , increm, noseg ,
     &                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     &                        noq3   , noq4   )
!>\file
!>       (2D) Horizontal dispersion as velocity dependent reprofunction

!
!*******************************************************************************
!
      implicit none
!
!     type    name         i/o description
!
      real(4) pmsa(*)     !i/o process manager system array, window of routine to process library
      real(4) fl(*)       ! o  array of fluxes made by this process in mass/volume/time
      integer ipoint( 12) ! i  array of pointers in pmsa to get and store the data
      integer increm( 12) ! i  increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! i  number of computational elements in the whole model schematisation
      integer noflux      ! i  number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! i  from, to, from-1 and to+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! i  active-inactive, surface-water-bottom, see manual for use
      integer noq1        ! i  nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer noq2        ! i  nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! i  nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! i  nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 12)   !    local work array for the pointering
      integer iq          !    local loop counter for exchanges
      integer iseg1       !    segment number from
      integer iseg2       !    segment number to
      real    velocity    !    velocity
      real    depth1      !    depth from segment
      real    depth2      !    depth to segment
!
!*******************************************************************************
!
!     type    name         i/o description                                        unit
!
      real(4) dfact_a     ! i  dispersion coefficient at low velocities           (m2/s)
      real(4) dfact_b     ! i  dispersion coefficient at low velocities           (m2/s)
      real(4) dfact_c     ! i  dispersion coefficient at low velocities           (m2/s)
      real(4) dback       ! i  dispersion coefficient at low velocities           (m2/s)
      real(4) dmin        ! i  dispersion coefficient at low velocities           (m2/s)
      real(4) dmax        ! i  dispersion coefficient at high velocities          (m2/s)
      real(4) depth       ! i  segment depth                                      (m)
      real(4) xarea       ! i  exchange area                                      (m2)
      real(4) flow        ! i  flow rate                                          (m3/s)
      real(4) horzdispv   ! o  variable horizontal dispersion                     (m2/s)
!
!*******************************************************************************

      ipnt        = ipoint

      do iq = 1 , noq1+noq2

         ! input on segments

         iseg1 = iexpnt(1,iq)
         if ( iseg1 .le. 0 ) iseg1 = iexpnt(2,iq)
         if ( iseg1 .le. 0 ) iseg1 = 1
         iseg2 = iexpnt(2,iq)
         if ( iseg2 .le. 0 ) iseg2 = iexpnt(1,iq)
         if ( iseg2 .le. 0 ) iseg2 = 1

         dfact_a     = pmsa(ipnt(1)+(iseg1-1)*increm(1))
         dfact_b     = pmsa(ipnt(2)+(iseg1-1)*increm(2))
         dfact_c     = pmsa(ipnt(3)+(iseg1-1)*increm(3))
         dback       = pmsa(ipnt(4)+(iseg1-1)*increm(4))
         dmin        = pmsa(ipnt(5)+(iseg1-1)*increm(5))
         dmax        = pmsa(ipnt(6)+(iseg1-1)*increm(6))
         depth1      = pmsa(ipnt(7)+(iseg1-1)*increm(7))
         depth2      = pmsa(ipnt(7)+(iseg2-1)*increm(7))
         depth       = (depth1+depth2)/2.

         ! input on exchange

         xarea      = pmsa(ipnt(8))
         flow       = pmsa(ipnt(9))

         ! calculate velocity

         if ( xarea .lt. 1e-10 ) then
            velocity = 0.0
         else
            velocity = abs(flow/xarea)
         endif

         ! calculate dispersion

         horzdispv  = dfact_a*(velocity**dfact_b)*(depth**dfact_c) + dback
         horzdispv  = max(horzdispv,dmin)
         horzdispv  = min(horzdispv,dmax)

         ! set output

         if ( iq .le. noq1 ) then
            pmsa(ipnt(10)+(iseg1-1)*increm(10)) = horzdispv
            pmsa(ipnt(10)+(iseg2-1)*increm(10)) = horzdispv
         else
            pmsa(ipnt(11)+(iseg1-1)*increm(11)) = horzdispv
            pmsa(ipnt(11)+(iseg2-1)*increm(11)) = horzdispv
         endif
         pmsa(ipnt(12)) = horzdispv

         ! update pointering in pmsa

         ipnt(8) = ipnt(8) + increm(8)
         ipnt(9) = ipnt(9) + increm(9)
         ipnt(12) = ipnt(12) + increm(12)

      enddo

      return
      end
