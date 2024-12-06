!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

!> Write tecplot output in already opened file
!>   note: file is closed when tim==tstop
   subroutine tecplot_out(mtecfil, tim, Lwriheader)
      use precision, only: dp

      use m_flowgeom
      use m_flow
      use m_flowtimes
      use m_netw
      implicit none

      integer :: mtecfil
      real(kind=dp), intent(in) :: tim
      logical, intent(in) :: Lwriheader

      integer :: i, j, k, icell
      integer, dimension(4) :: inode
      real(kind=dp), dimension(:), allocatable :: s1k, a1k, hk
      character(len=256) :: zone

! Write header
      if (Lwriheader) then
         write (mtecfil, '(a)') 'Title = "output"'
         write (mtecfil, '(a)') 'Variables = x y z e h u v'
      end if

      ! Write cell centred data

!   write (mtecfil, *) 'Zone f=point, t="', trim(zone),'"', &
!        &      ', i=', ndx
!   do i = 1, ndx
!      write (mtecfil, '(3f18.7)') xz(i), yz(i), s1(i) !, u1(inod)
!   enddo

      ! Write net node data

      ! Interpolate s1 to net nodes

      allocate (s1k(1:numk))
      allocate (a1k(1:numk))
      allocate (hk(1:numk))
      s1k = 0.0d0
      a1k = 0.0d0
      hk = 0.0d0

      ! Loop over flow nodes (flow cells/elements) and over all its net nodes (corners) = expensive!
      ! Weighted interpolation based on cell area

      do i = 1, ndxi
         do j = 1, netcell(i)%N
            k = netcell(i)%nod(j)
            s1k(k) = s1k(k) + a1(i) * s1(i)
            a1k(k) = a1k(k) + a1(i)
         end do
      end do

      ! Scale the interpolated water levels

      s1k = s1k / a1k

      ! Compute total depth in net nodes

      do i = 1, numk
         hk(i) = max(0.0d0, s1k(i) - zk(i))
      end do

      ! Compute velocities to make sure the velocities in net nodes (corner points) have been computed

      call setvelocityfield()

      !
      ! Write zone information
      !
      write (zone, '(f16.4)') tim
      write (mtecfil, '(3a)') 'Zone T ="', trim(zone), '"' !, numk
      write (mtecfil, '(a,i0,a,i0,a)') 'N=', numk, ', E=', ndxi, ', F=FEPOINT ET=QUADRILATERAL'

      do i = 1, numk
         write (mtecfil, '(7f18.7)') xk(i), yk(i), zk(i), s1k(i), hk(i), ucnx(i), ucny(i) !, u1k(i)
      end do

      ! Write connectivity table to file

      do icell = 1, ndxi
         do j = 1, 3
            inode(j) = netcell(icell)%nod(j)
         end do
         if (netcell(icell)%N == 3) then
            inode(4) = netcell(icell)%nod(1)
         else
            inode(4) = netcell(icell)%nod(4)
         end if
         write (mtecfil, '(4(i0,3x))') inode(1), inode(2), inode(3), inode(4)
      end do

   end subroutine tecplot_out
