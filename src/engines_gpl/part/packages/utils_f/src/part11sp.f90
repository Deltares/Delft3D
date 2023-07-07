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

      subroutine part11sp( lgrid  , xp     , yp     , nmax   , npart  ,        &
                           mpart  , xpart  , ypart  , xa     , ya     ,        &
                           lgrid2 , mmax)

!       Deltares Software Centre

!>\file
!>         Calculates real world x,y from the particles in-cell position for a single particle in 2D
!>
!>         Interpolates bi-linearly between the corner coordinates of the particle cell.
!>         That is why both active grid (lgrid) and the total (lgrid2) are needed.\n
!>         It seems that in part10 the n,m of non-floating particles is set to a negative
!>         number to exclude them from this computation (needs to be checked).

!     System administration : Michelle Jeuken

!     Created by            : Michelle Jeuken from part11

!     Modified              : none

!     Logical unit numbers  : none

!     Subroutines called    : none

!     Functions   called    : none

      use precision_part               ! single/double precision
      use timers

      implicit none               ! force explicit typing

!     Arguments

!     kind            function         name                      description

      integer  ( ip), intent(in   ) :: nmax                    !< first grid index
      integer  ( ip), intent(in   ) :: mmax                    !< second grid index
      integer  ( ip), intent(in   ) :: lgrid (nmax,mmax)       !< active grid matrix
      integer  ( ip), intent(in   ) :: lgrid2(nmax,mmax)       !< total grid matrix
      real     ( rp), intent(in   ) :: xp    (nmax*mmax)       !< x of the grid cell corner
      real     ( rp), intent(in   ) :: yp    (nmax*mmax)       !< y of the grid cell corner
      integer  ( ip), intent(in   ) :: npart                   !< first grid cell index particles
      integer  ( ip), intent(in   ) :: mpart                   !< second grid cell index particles
      real     ( rp), intent(in   ) :: xpart                   !< x-in the grid of particles
      real     ( rp), intent(in   ) :: ypart                   !< y-in the grid of particles
      real     ( rp), intent(  out) :: xa                      !< absolute x of particles
      real     ( rp), intent(  out) :: ya                      !< absolute y of particles

!     Locals:

      integer( ip) n0      ! linear 2D grid cell number of the particle
      integer( ip) n1      ! first index minus 1 grid number
      integer( ip) n2      ! second index minus 1 grid number
      integer( ip) n3      ! both indices minus 1 grid number

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "part11sp", ithndl )

!     loop over the number of particles


!        only perform transformation particles with positive (n,m) indices
!        works as a mask to exclude certain particles from transformation.
!        in later delpar version used to exclude non-floating particles
!        (see end of routine part10)

      if ( npart .ge. 0 .and. mpart .ge. 0 ) then
         n0 = lgrid(npart, mpart)
         if ( n0 .gt. 0 ) then
            n1        = lgrid2(npart - 1, mpart    )
            n2        = lgrid2(npart    , mpart - 1)
            n3        = lgrid2(npart - 1, mpart - 1)

!              horizontal coordinates (x and y)

            xa = xp(n3) +                                          &
                     xpart * (xp(n1) - xp(n3)) + ypart * (xp(n2) - xp(n3)) +     &
                     xpart * ypart * (xp(n0) - xp(n1) - xp(n2) + xp(n3))
            ya = yp(n3) +                                          &
                     xpart * (yp(n1) - yp(n3)) + ypart * (yp(n2) - yp(n3)) +     &
                     xpart * ypart * (yp(n0) - yp(n1) - yp(n2) + yp(n3))

         else
!              particles outside model area
            xa = 999.999
            ya = 999.999
         endif
      endif
!     end of subroutine

      if ( timon ) call timstop ( ithndl )
      return
!
      end subroutine part11sp

