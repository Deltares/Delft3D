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

module m_pillar
   use precision, only: dp

   implicit none(type, external)

   private

   public :: initialize_pillar
   public :: pillar_type
   public :: PILLAR_NONE
   public :: PILLAR_DELFT3D_FLOWNODE
   public :: PILLAR_VEGETATION_BAPTIST
   public :: PILLAR_DELFT3D_FLOWLINK
   public :: PILLAR_DELFT3D_FLOWNODE_TO_FLOWLINK

   integer :: pillar_type !< Switch for methods for computing bridge pillars
   integer, parameter :: PILLAR_NONE = 0 !< pillar_type not active
   integer, parameter :: PILLAR_DELFT3D_FLOWNODE = 1 !< pillar_type based on reduced surface area (top view)
   integer, parameter :: PILLAR_VEGETATION_BAPTIST = 2 !< pillar_type using vegetation approach of
   integer, parameter :: PILLAR_DELFT3D_FLOWLINK = 3 !< pillar_type based on reduced cross-sectional area (side view)
   integer, parameter :: PILLAR_DELFT3D_FLOWNODE_TO_FLOWLINK = 4 !< pillar_type based on reduced cross-sectional area (side view) 

   type t_pillar
       integer :: np !< number of pillars
       real(kind=dp), dimension(:), allocatable :: xcor !< x-coordinates of pillars
       real(kind=dp), dimension(:), allocatable :: ycor !< y-coordinates of pillars
       real(kind=dp), dimension(:), allocatable :: dia !< radius od pillars
       real(kind=dp), dimension(:), allocatable :: cd !< Cd coefficient of pillars

          contains
       procedure :: initialize => initialize_pillar
   end type t_pillar

   contains

   subroutine initialize_pillar(self, np)
      ! Parameters
       class(t_pillar), intent(out) :: self
       integer, intent(in) :: np
       self%np = np
       allocate(self%xcor(np))
       allocate(self%ycor(np))
       allocate(self%dia(np))
       allocate(self%cd(np))
   end subroutine initialize_pillar

end module m_pillar
