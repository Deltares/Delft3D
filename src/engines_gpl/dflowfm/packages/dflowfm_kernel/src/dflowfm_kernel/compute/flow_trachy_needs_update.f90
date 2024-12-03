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

!> helper function to make sure that the check for updating cross sections is in line with the flow_trachyupdate
module m_flow_trachy_needs_update

implicit none

contains

logical function flow_trachy_needs_update(time1)
   use precision_basics, only: hp
   use m_flowtimes, only: tstart_user, dt_user
   use m_trachy, only: itimtt

   real(kind=hp), intent(in) :: time1 !< current time

   real(kind=hp) :: ntrtsteps, dt_trachy

   dt_trachy = dt_user * real(itimtt, hp)
   ntrtsteps = (time1 - tstart_user) / dt_trachy
   flow_trachy_needs_update = (abs(ntrtsteps - floor(ntrtsteps)) < 1d-6)
end function flow_trachy_needs_update

end module m_flow_trachy_needs_update
