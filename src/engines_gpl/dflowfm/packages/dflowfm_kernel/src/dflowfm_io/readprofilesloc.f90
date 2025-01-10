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

module m_readprofilesloc

   implicit none

contains

   subroutine readprofilesloc(minp)
      use m_profiles
      use m_filez, only: doclose

      integer :: minp
      character rec * 256
      integer :: ierr, n

      minproflocnr = 99999999; maxproflocnr = 0

      n = 0
10    read (minp, '(a)', end=999) rec
      if (rec(1:1) == '*') goto 10
      n = n + 1
      goto 10

999   rewind (minp)
      allocate (xpr(n), ypr(n), zpr(n), npr(n), stat=ierr)

      n = 0
20    read (minp, '(a)', end=888) rec
      if (rec(1:1) == '*') goto 20
      n = n + 1
      read (rec, *) xpr(n), ypr(n), npr(n)

      goto 20

888   call doclose(minp)
      nproflocs = n

   end subroutine readprofilesloc

end module m_readprofilesloc
