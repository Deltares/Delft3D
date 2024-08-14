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

module m_sedtrails_data
   use coordinate_reference_system
   implicit none

   character(len=255) :: sedtrails_analysis
   !
   ! flow geometry from md_sedtrailsfile
   !
   double precision, allocatable, target :: xk(:) !< [-] net node x coordinate {"shape": ["numk"]}
   double precision, allocatable, target :: yk(:) !< [-] net node y coordinate {"shape": ["numk"]}
   double precision, allocatable, target :: zk(:) !< [-] net node z coordinate {"shape": ["numk"]}
   double precision, allocatable :: xk0(:), yk0(:), zk0(:) !< backup for xk, etc.
   double precision, allocatable :: xk1(:), yk1(:), zk1(:) !< work array for xk, etc.

   integer :: numk0
   integer, target :: numk !< [-] nr. of sedtrails nodes. {"shape": []}
   integer :: kmax
   type(t_crs), target :: crs !< crs read from net file, to be written to output geom.

   integer, allocatable :: st_ind(:, :) !< indexes and weight factors for interpolation cell centre->sedtrails grid
   double precision, allocatable :: st_wf(:, :) !< (3,:)

   integer, allocatable :: idomain(:)
   integer, allocatable :: iglobal_s(:)
   integer, allocatable :: iwork(:)

contains

   subroutine sedtrails_resetdata()

      implicit none

      sedtrails_analysis = 'all'
      numk0 = 0
      numk = 0
      kmax = 0

   end subroutine sedtrails_resetdata

end module m_sedtrails_data
