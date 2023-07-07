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

      subroutine varsal     ( pmsa   , fl     , ipoint , increm, noseg ,
     &                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     &                        noq3   , noq4   )
!>\file
!>       Salinity in case of constant river discharge

      implicit none

      ! declaration of the arguments

      real    pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real    fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(4)   ! I  Array of pointers in PMSA to get and store the data
      integer increm(4)   ! I  Increments in IPOINT for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the FL array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction, only horizontal dir if irregular mesh
      integer noq2        ! I  Nr of exchanges in 2nd direction, NOQ1+NOQ2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

      ! variables from the pmsa array

      real    frcon       ! I  fraction fresh water from constant river discharge (-)
      real    frflow      ! I  fraction fresh water from variable river discharge (-)
      real    salbnd      ! I  salinity from the boundary                        (ppt)
      real    salinity    ! O  salinity                                          (ppt)

      ! other local declarations

      integer ipnt(4)     !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop

      ! initialise pointers in pmsa array

      ipnt = ipoint

      ! loop over the segments

      do iseg = 1 , noseg
         if (btest(iknmrk(iseg),0)) then

         ! input from pmsa array

         frcon      = pmsa( ipnt(  1) )
         frflow     = pmsa( ipnt(  2) )
         salbnd     = pmsa( ipnt(  3) )

         ! calculate salinity from input

         if ( abs(1.-frcon) .gt. 1.e-10 ) then
            salinity = (1.-frflow)/(1.-frcon)*salbnd
            salinity = max(0.0,salinity)
         else
            salinity = 0.0
         endif

         ! store salinity in pmsa array

         pmsa( ipnt(  4)   ) = salinity

         endif

         ! update pointers in pmsa

         ipnt = ipnt + increm

      enddo

      return
      end
