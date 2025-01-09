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

!> Initializes all administration necessary for writing lateral discharge output to his-files.
module m_init_lateral_his

   implicit none

   private

   public :: init_lateral_his

contains

   subroutine init_lateral_his()
      use m_laterals, only: qplatCum, qplatCumPre, qplatAve, qLatReal, qLatRealCum, qLatRealCumPre, qLatRealAve, numlatsg
      use m_flowparameters, only: jahislateral
      use m_alloc

      ! At the starting time of history output, initialize variables
      if (jahislateral > 0 .and. numlatsg > 0) then
         call realloc(qplatCum, numlatsg, keepExisting=.false., fill=0d0)
         call realloc(qplatCumPre, numlatsg, keepExisting=.false., fill=0d0)
         call realloc(qplatAve, numlatsg, keepExisting=.false., fill=0d0)
         call realloc(qLatReal, numlatsg, keepExisting=.false., fill=0d0)
         call realloc(qLatRealCum, numlatsg, keepExisting=.false., fill=0d0)
         call realloc(qLatRealCumPre, numlatsg, keepExisting=.false., fill=0d0)
         call realloc(qLatRealAve, numlatsg, keepExisting=.false., fill=0d0)
      end if
   end subroutine init_lateral_his

end module m_init_lateral_his
