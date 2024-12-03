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

!> Runs a user-timestep (do computational flowsteps until timeuser), but not the init and finalize.
!!
!! Should be preceded by a flow_run_usertimestep and followed by a flow_finalize_usertimestep.
subroutine flow_run_usertimestep(key, iresult) ! do computational flowsteps until timeuser
   use m_flow_single_timestep, only: flow_single_timestep
   use m_get_s_key
   use m_flowtimes
   use unstruc_messages
   use m_partitioninfo
   use m_gui
   use dfm_error
   implicit none
   integer, intent(out) :: key
   integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

   key = 0
   iresult = DFM_GENERICERROR

   do while (time0 < time_user) ! nb, outside flow_singletimestep, time0=time1 !

      call flow_single_timestep(key, iresult)
      if (iresult /= DFM_NOERR .and. iresult /= DFM_TIMESETBACK) then
         goto 888
      end if

      if (jaGUI == 1) then ! TODO: MICHAL Another Gui call

         call get_s_key(key)

         if (jampi == 1) then
            call reduce_key(key)
         end if

         if (key == 1) then
            call mess(LEVEL_INFO, 'User interrupt')
            iresult = DFM_NOERR
            return
         end if
      end if
   end do

   iresult = DFM_NOERR
   return ! Return with success.

888 continue
end subroutine flow_run_usertimestep
