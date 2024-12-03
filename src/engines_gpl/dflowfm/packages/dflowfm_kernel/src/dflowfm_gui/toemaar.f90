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
module m_toemaar
   implicit none
contains
   subroutine TOEMAAR()
      use m_okay
      use m_timlin
      use m_fkeys

      integer :: key
      call OKAY(0)
      call TIMLIN()
10    continue
      call INFLUSH()
      call INKEYEVENT(KEY)
      if (KEY == 50 .or. (KEY >= 254 .and. KEY <= 259)) then
         goto 10
      else if (KEY >= 24 .and. KEY <= 26) then
         call FKEYS(KEY)
         goto 10
      end if
      call TIMLIN()
      return
   end
end module m_toemaar
