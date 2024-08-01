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

!> Initializes a single computational timestep, call this prior to flow_perform_single_timestep.
subroutine flow_init_single_timestep(iresult)
   use timers
   use m_flow
   use m_flowgeom
   use m_flowtimes
   use m_timer
   use dfm_error
   implicit none

   integer, parameter :: JAZWS0 = 0
   logical, parameter :: SET_HU = .true.
   logical, parameter :: USE_U1 = .true.
   integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if successful.

   iresult = DFM_GENERICERROR

   if (lnx == 0) then
      iresult = DFM_MODELNOTINITIALIZED
      goto 888
   end if

   call timstrt('Time steps', handle_steps)

   if (jatimer == 1) call starttimer(ITIMESTEP)

   call flow_initimestep(JAZWS0, SET_HU, USE_U1, iresult) ! initialise timestep

   if (iresult /= DFM_NOERR) then
      goto 888
   end if

   iresult = DFM_NOERR
   return ! Return with success

888 continue
   ! Error

end subroutine flow_init_single_timestep
