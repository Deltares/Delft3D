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

      subroutine AGECART    ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(  4) ! I  Array of pointers in pmsa to get and store the data
      integer increm(  4) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt(  4)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(4) watersrc    ! I  Source of water to be traced                       (g/m3)
      real(4) ageconc     ! I  Age concentration                                  (g.d/m3)
      real(4) age_threshold     ! I  user defined threshold to avoid small concentration (g/m3), default 0.0
      real(4) ageprod     ! F  production of waterage                             (d)
      integer Iageprod    !    Pointer to the production of waterage
!
!*******************************************************************************
!
      ipnt        = ipoint
      Iageprod    = 1
!
      do 9000 iseg = 1 , noseg
!
         watersrc       = pmsa( ipnt(  1) )
         ageconc        = pmsa( ipnt(  2) )
         age_threshold  = pmsa( ipnt(  3) )
!
!   *****     Insert your code here  *****
!
         ageprod    = watersrc
!
!   *****     End of your code       *****
!
         fl  ( Iageprod    ) = ageprod

         if (watersrc .gt. age_threshold) then
             pmsa(ipnt(4))       = ageconc / watersrc
         else
             pmsa(ipnt(4))       = -999.0
         endif
!
         Iageprod    = Iageprod    + noflux
         ipnt        = ipnt        + increm
!
 9000 continue
!
      return
      end subroutine
