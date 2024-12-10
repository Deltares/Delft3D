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
module m_read_dry2pli
   implicit none
contains
   subroutine readry2pli(mthd, mout) ! convert barrier v file to model independent, barv content =  m,n,sill depth
      use precision, only: dp
      use m_grid, only: xc, yc

      integer :: mthd, mout
      real(kind=dp) :: z = 9999d0
      character(len=132) :: rec
      integer :: m, n

10    read (mthd, '(a)', end=999) rec

      read (rec, *) m, n

      write (mout, '(a)') 'Line'
      write (mout, '(a)') ' 5 3'

      write (mout, *) xc(m, n - 1), yc(m, n - 1), z
      write (mout, *) xc(m, n), yc(m, n), z
      write (mout, *) xc(m - 1, n), yc(m - 1, n), z
      write (mout, *) xc(m - 1, n - 1), yc(m - 1, n - 1), z
      write (mout, *) xc(m, n - 1), yc(m, n - 1), z

      goto 10

999   call doclose(mthd)
      call doclose(mout)

   end subroutine readry2pli
end module m_read_dry2pli
