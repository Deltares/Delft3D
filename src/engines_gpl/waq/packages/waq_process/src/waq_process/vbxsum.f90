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

      subroutine VBXSUM     ( pmsa   , fl     , ipoint , increm, noseg , &
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
      integer ipoint(*)  ! I  Array of pointers in pmsa to get and store the data
      integer increm(*)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)

      integer                                    :: iflux, icohort, iseg, ioutput
      integer, save                              :: ncohorts, nfluxes
      integer, dimension(:,:), allocatable, save :: ipnt, incr
      logical, save                              :: first = .true.
!
!*******************************************************************************
!     Sum the uptake of nutrients from the sediment and release to the sediment by terrestrial vegetation (VEGMOD)
!     (This is a workaround for having the fluxes per cohort in the process definition of the DelwaqG process)
!

      if ( first ) then
          first    = .false.
          ncohorts = int(pmsa(ipoint(1)))
          nfluxes  = int(pmsa(ipoint(2)))

          allocate( ipnt(nfluxes,ncohorts+1) )
          allocate( incr(nfluxes,ncohorts+1) )
      endif

      ipnt = reshape( ipoint(3:2+(ncohorts+1) * nfluxes), [nfluxes, ncohorts+1] )
      incr = reshape( increm(3:2+(ncohorts+1) * nfluxes), [nfluxes, ncohorts+1] )

      do iseg = 1,noseg
          do iflux = 1,nfluxes
              pmsa(ipnt(iflux,ncohorts+1)) = 0.0
          enddo
          do icohort = 1,ncohorts
              do iflux = 1,nfluxes
                  pmsa(ipnt(iflux,ncohorts+1)) = pmsa(ipnt(iflux,ncohorts+1)) + pmsa(ipnt(iflux,icohort))
              enddo
          enddo
          ipnt = ipnt + incr
      enddo

      return
      end
