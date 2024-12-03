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

module m_alloc9basicwavearrays

implicit none

private

public :: alloc9basicwavearrays

contains

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
   subroutine alloc9basicwavearrays()
      use m_flow
      use m_flowgeom
      use m_waves
      implicit none
      integer :: ierr

      ! Not 9 anymore, sorry
      call realloc(hwav, ndx, stat=ierr, keepExisting=.false., fill=hwavuni)
      call aerr('hwav    (ndx)', ierr, ndx)
      call realloc(twav, ndx, stat=ierr, keepExisting=.false., fill=twavuni)
      call aerr('twav    (ndx)', ierr, ndx)
      call realloc(phiwav, ndx, stat=ierr, keepExisting=.false., fill=phiwavuni)
      call aerr('phiwav  (ndx)', ierr, ndx)
      call realloc(rlabda, ndx, stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('rlabda  (ndx)', ierr, ndx)
      call realloc(uorb, ndx, stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('uorb    (ndx)', ierr, ndx)
      call realloc(ustokes, lnkx, stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('ustokes(lnkx)', ierr, lnkx)
      call realloc(vstokes, lnkx, stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('vstokes(lnkx)', ierr, lnkx)
      call realloc(wblt, lnx, stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('wblt(lnx)', ierr, lnx)
      call realloc(cfwavhi, lnx, stat=ierr, keepExisting=.false., fill=0d0)
      call aerr('cfwavhi(lnx)', ierr, lnx)
      if (modind == 9 .and. kmx == 0) then
         call realloc(cfhi_vanrijn, lnx, stat=ierr, keepExisting=.false., fill=0d0)
         call aerr('cfhi_vanrijn(lnx)', ierr, lnx)
      end if
   end subroutine alloc9basicwavearrays

end module m_alloc9basicwavearrays
