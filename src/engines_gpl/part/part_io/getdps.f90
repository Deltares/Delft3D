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
module m_getdps
use m_stop_exit


implicit none

contains


subroutine getdps ( lunpr  , lundp  , lnam   , nmax   , mmax   ,      &
                    nosegl , dps    , cellpnt, ltrack )
!
!     programmer : antoon koster
!     function   : read bathymetry from *.dps file,
!                  as created by coup203
!                  note that the bathymetry is defined
!                  w.r.t. reference level
!     date       : feb 2003
!
!     note:
!       1. note that the depths in this routine refer to
!          depths w.r.t. reference level, so called
!          bathymetry.
!
!       2. as these depths are only required in case of
!          particle tracking, this file may be missing
!          in other cases (see itrack)

      use m_waq_precision      ! single and double precision
      use timers
      use fileinfo       ! file information for all input/output files
      use openfl_mod     ! explicit interface for subroutine calls
      implicit none

!     Arguments

!     kind           function         name                  description

      integer  ( int_wp ), intent(in   ) :: lunpr               !< unit nr of the diagnostics file
      integer  ( int_wp ), intent(in   ) :: lundp               !< unit nr of the depth file
      character( * ), intent(in   ) :: lnam                !< name of the depth file
      integer  ( int_wp ), intent(in   ) :: nmax                !< first dimension of the grid
      integer  ( int_wp ), intent(in   ) :: mmax                !< second dimension of the grid
      integer  ( int_wp ), intent(in   ) :: nosegl              !< nr of active segments of a layer
      real     ( real_wp), intent(  out) :: dps   (nmax*mmax)   !< array with depth values
      integer  ( int_wp ), intent(in   ) :: cellpnt(nosegl)     !< backpointer of nosegl to mnmax
      logical       , intent(in   ) :: ltrack              !< if .true. then particle tracing

!     local scalars

      integer :: idum, nmnw, iocond
      real(sp), allocatable :: depwrk(:)          !  work array to read depth
      logical :: ex

      integer(4) ithndl              ! handle to time this subroutine
      data       ithndl / 0 /
      if ( timon ) call timstrt( "getdps", ithndl )

      dps = 0.0
      if ( ltrack ) then

!        for particle tracking, get depth

         inquire (file = trim(lnam), exist = ex)
         if (ex) then
            call openfl ( lundp, lnam, 0 )
            read (lundp) idum,idum,nmnw,nmnw,nmnw,idum
            if ( nmnw .ne. nosegl ) then
               write ( lunpr, * ) 'ERROR, dimension in dps file does not match!',nmnw,nosegl
               call stop_exit(1)
            endif
            allocate ( depwrk(nosegl) )
            read (lundp, iostat = iocond) depwrk
            close(lundp)
            if (iocond == 0 ) then
               dps(cellpnt(:)) = depwrk(:)
            else
               write (lunpr, *) 'Error 4407. Reading the depth file :', lnam(:len_trim(lnam))
               write (lunpr, *) '            Depths not read correctly, the calculation is terminated'
               write (*,     *) 'Error 4407. Reading the depth file :', lnam(:len_trim(lnam))
               write (*,     *) '            Depths not read correctly, the calculation is terminated'
               call stop_exit(1)
            endif
         else
            write ( lunpr, * ) 'ERROR: Depth file does not exist. The calculation is terminated'
            write ( *,     * ) 'ERROR: Depth file does not exist. The calculation is terminated'
            call stop_exit(1)
         endif
      endif

      if ( timon ) call timstop ( ithndl )

end subroutine

end module m_getdps
