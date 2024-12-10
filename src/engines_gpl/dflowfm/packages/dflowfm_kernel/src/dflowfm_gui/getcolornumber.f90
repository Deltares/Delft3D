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

module m_getcolornumber

   implicit none

contains

   subroutine GETCOLORNUMBER(XP, YP, NUMCOL, N1O, N2O, N3O)
      use precision, only: dp
      use m_disvalcolors
      use m_set_col
      use m_ptabs
      implicit none
      integer :: i
      integer :: n1
      integer :: n1o
      integer :: n2
      integer :: n2o
      integer :: n3
      integer :: n3o
      integer :: numcol
      real(kind=dp) :: xp
      real(kind=dp) :: yp
      call IGRGETPIXELRGB(real(XP), real(YP), N1O, N2O, N3O)
      do I = 0, 255
         call SETCOL(I)
         call PTABS(XP, YP)
         call IGRGETPIXELRGB(real(XP), real(YP), N1, N2, N3)
         if (N1 == N1O .and. N2 == N2O .and. N3 == N3O) then
            NUMCOL = I
            call DISVALCOLORS(NUMCOL, N1, N2, N3, 1)
            return
         end if
      end do
      return
   end

end module m_getcolornumber
