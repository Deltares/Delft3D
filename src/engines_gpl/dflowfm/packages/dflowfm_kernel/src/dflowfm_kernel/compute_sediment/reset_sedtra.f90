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

module m_reset_sedtra

   implicit none

   private

   public :: reset_sedtra

contains

   subroutine reset_sedtra()
      use m_sediment
      use morphology_data_module
      use m_rdstm
      use message_module

      implicit none

      integer :: istat

      if (.not. stm_included) return
      istat = clrstm(stmpar)
      call clrsedtra(istat, sedtra)
      if (associated(mtd%dzbdt)) then
         deallocate (mtd%dzbdt)
         deallocate (mtd%uau)
         deallocate (mtd%seddif)
         deallocate (mtd%sed)
         deallocate (mtd%ws)
         deallocate (mtd%blchg)

         call clearstack(mtd%messages)
         deallocate (mtd%messages)
      end if
   end subroutine reset_sedtra

end module m_reset_sedtra
