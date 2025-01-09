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

module m_flow_bl_ave_init

   implicit none

   private

   public :: flow_bl_ave_init

contains

   subroutine flow_bl_ave_init()

      use m_flowgeom, only: bl_ave, bl_ave0, ndx
      use m_missing, only: dmiss
      use m_alloc, only: realloc, aerr

      integer :: ierr

      call realloc(bl_ave, ndx, keepExisting=.false., fill=dmiss, stat=ierr)
      call aerr('bl_ave(ndx)', ierr, ndx)

      call realloc(bl_ave0, ndx, keepExisting=.false., fill=dmiss, stat=ierr)
      call aerr('bl_ave0(ndx)', ierr, ndx)

   end subroutine flow_bl_ave_init

end module m_flow_bl_ave_init
