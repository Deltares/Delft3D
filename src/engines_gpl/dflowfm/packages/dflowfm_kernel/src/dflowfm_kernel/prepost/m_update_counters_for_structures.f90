!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2026.
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

module m_update_counters_for_structures

   implicit none

   private

   public :: update_counters_for_dambreak_or_pump

contains

   !> Update counter arrays for structures (dambreak and pump) that use "signals" to store information on number of structures and their links.
   subroutine update_counters_for_dambreak_or_pump(qid, numlinks, nsignals, l1signals, l2signals, strucidx, structure_index)
      use messagehandling, only: msgbuf, msg_flush
      use m_alloc, only: realloc

      character(len=*), intent(in) :: qid !< qid is the id of the structure.
      integer, intent(in) :: numlinks !< numlinks is the number of flow links.
      integer, intent(inout) :: nsignals !< nsignals is the number of signals.
      integer, dimension(:), allocatable, intent(inout) :: l1signals !< l1signal is the start index of the signals.
      integer, dimension(:), allocatable, intent(inout) :: l2signals !< l2signal is the end index of the signals.
      integer, dimension(:), allocatable, intent(inout) :: strucidx !< strucidx is the index of the structure.
      integer, intent(in) :: structure_index !< structure_index is the index of the structure.

      write (msgbuf, '(a,i8,a)') trim(qid), numlinks, ' nr of structure links'
      call msg_flush()
      nsignals = nsignals + 1
      strucidx(nsignals) = structure_index
      call realloc(L1signals, nsignals)
      call realloc(L2signals, nsignals)
      if (nsignals == 1) then
         L1signals(nsignals) = 1
         L2signals(nsignals) = numlinks
      else
         L1signals(nsignals) = L2signals(nsignals - 1) + 1
         L2signals(nsignals) = L2signals(nsignals - 1) + numlinks
      end if

   end subroutine update_counters_for_dambreak_or_pump

end module m_update_counters_for_structures
