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

module m_plotdots
   use precision, only: dp
   implicit none
   integer :: numdots ! number of dots
   integer :: NSIZE = 0 ! array size
   real(kind=dp), dimension(:), allocatable :: xdots, ydots ! dot coordinates, dim(NSIZE)
   real(kind=dp), dimension(:), allocatable :: zdots ! dot z-value
   integer, dimension(:), allocatable :: colnumber ! colour number
   real(kind=dp), parameter :: ZDOTDEFAULT = 0d0

contains

   subroutine reallocdots(N)
      use m_alloc
      use m_missing
      implicit none

      integer, intent(in) :: N

      if (N > NSIZE) then
         NSIZE = 1 + int(1.2d0 * dble(N))

         call realloc(xdots, NSIZE, keepExisting=.true., fill=DMISS)
         call realloc(ydots, NSIZE, keepExisting=.true., fill=DMISS)
         call realloc(zdots, NSIZE, keepExisting=.true., fill=ZDOTDEFAULT)
         call realloc(colnumber, NSIZE, keepExisting=.true., fill=imiss)
      end if

      return
   end subroutine reallocdots

!> add a dot
   subroutine adddot(x, y, z, colournumber)
      use precision, only: dp
      implicit none

      real(kind=dp), intent(in) :: x, y
      real(kind=dp), optional, intent(in) :: z
      integer, optional, intent(in) :: colournumber
      real(kind=dp) :: zloc

      zloc = ZDOTDEFAULT
      if (present(z)) then
         zloc = z
      end if

      numdots = numdots + 1
      call reallocdots(numdots)
      xdots(numdots) = x
      ydots(numdots) = y
      zdots(numdots) = zloc
      if (present(colournumber)) then
         colnumber(numdots) = colournumber
      else
         colnumber(numdots) = 31 ! ncolhl
      end if

      return
   end subroutine adddot

!  write dots to sample file
   subroutine write_dots(FNAM, jawritten)
      implicit none

      character(len=*), intent(in) :: FNAM
      integer, intent(out) :: jawritten

      integer :: i, id

      jawritten = 0

      if (numdots < 1) goto 1234

      call newfil(id, FNAM)

      do i = 1, numdots
         write (id, "(3E15.5)") xdots(i), ydots(i), zdots(i)
      end do

      call doclose(id)

      jawritten = 1

1234  continue

      return
   end subroutine

   subroutine deldots()
      implicit none

      Numdots = 0

      return
   end subroutine

end module m_plotdots
