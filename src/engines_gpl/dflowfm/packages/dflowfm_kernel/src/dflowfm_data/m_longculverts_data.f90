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

!> Module for long culvert data in a dflowfm model.
!! Long culverts are read from the structures.ini file(s), and converted into
!! new netlinks and prof1D definitions.
module m_longculverts_data
   use precision, only: dp
   use messagehandling, only: idlen
   implicit none

   private

   !> Type definition for longculvert data.
   type, public :: t_longculvert
      character(len=IdLen) :: id
      character(len=IdLen) :: branchid !< if newculverts, corresponding network branch
      character(len=IdLen) :: csdefid !< if newculverts, corresponding network crossdef
      integer :: numlinks !< Number of links of the long culvert
      integer, dimension(:), allocatable :: netlinks !< Net link numbers of the long culvert
      integer, dimension(:), allocatable :: flowlinks !< Flow link numbers of the long culvert
      integer :: friction_type = -999 !< Friction type
      integer :: allowed_flowdir !< Allowed flowdir:
      !< 0 all directions
      !< 1 only positive flow
      !< 2 only negative flow
      !< 3 no flow allowed
      real(kind=dp) :: friction_value = -999.0_dp !< Friction value
      real(kind=dp), dimension(:), allocatable :: xcoords !< X-coordinates of the numlinks+1 points
      real(kind=dp), dimension(:), allocatable :: ycoords !< Y-coordinates of the numlinks+1 points
      real(kind=dp), dimension(:), allocatable :: bl !< Bed level on numlinks+1 points
      real(kind=dp) :: width !< Width of the rectangular culvert
      real(kind=dp) :: height !< Height of the rectangular culvert
      real(kind=dp) :: valve_relative_opening !< Relative valve opening: 0 = fully closed, 1 = fully open
      integer :: flownode_up = 0 !< Flow node index at upstream
      integer :: flownode_dn = 0 !< Flow node index at downstream
   end type

   type(t_longculvert), dimension(:), allocatable, public :: longculverts !< Array containing long culvert data (size >= nlongculverts)

   integer, public :: nlongculverts !< Number of longculverts
   logical, public :: newculverts

end module m_longculverts_data
