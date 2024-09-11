!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2022.
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

!> Updates the flow variables of FM when the Sobek-RE kernel is used.

!This function cannot be in the <fm1dimp> module because it uses FM variables and
!while the kernel of FM depends on the <fm1dimp> module, the opposite is not true.

subroutine flow_initialize_fm1dimp_timestep(iresult, time1)
   use m_fm1dimp_update_network
   use m_fm1dimp_update_bc
   implicit none

!
!DECLARATION
!

!input
   double precision, intent(in) :: time1

!output
   integer, intent(out) :: iresult !< Error status, DFM_NOERR==0 if succesful.

!
!CALC
!

   call fm1dimp_update_network(iresult) !update of the flow variables (change every time step)
   call fm1dimp_update_bc(iresult, time1) !update of the boundary conditions

end subroutine flow_initialize_fm1dimp_timestep
