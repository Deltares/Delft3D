!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2018-2024.
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

module m_fm_wq_processes_sub

   implicit none

   private

   public :: default_fm_wq_processes, fm_wq_processes_ini_proc, fm_wq_processes_ini_sub, fm_wq_processes_step, &
             get_waqinputname

   interface

      module subroutine default_fm_wq_processes()
         implicit none
      end subroutine default_fm_wq_processes

      module subroutine fm_wq_processes_ini_sub()
         implicit none
      end subroutine fm_wq_processes_ini_sub

      module subroutine fm_wq_processes_ini_proc()
         implicit none
      end subroutine fm_wq_processes_ini_proc

      module subroutine fm_wq_processes_step(dt, time)
         use precision, only: dp
         implicit none
         real(kind=dp), intent(in) :: dt !< timestep for waq in seconds
         real(kind=dp), intent(in) :: time !< time     for waq in seconds
      end subroutine fm_wq_processes_step

      module subroutine get_waqinputname(qid, inputname, qidname)
         !> Convert qid (from .ext file) to waq input name (split in generic qidname and specific input name).
    !! If the input qid is not waq input name, then the same qid is returned (and no waq input name)
         implicit none

         character(len=*), intent(in) :: qid !< Original quantityid, e.g., 'waqfunctionradsurf'.
         character(len=*), intent(inout) :: inputname !< The trimmed waq input name, e.g., 'fluor'.
         character(len=*), intent(inout) :: qidname !< The base input name for further use in external file analisys, e.g., 'tracerbnd'.
      end subroutine get_waqinputname

   end interface

end module m_fm_wq_processes_sub
