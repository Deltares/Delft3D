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

module m_samples_refine ! used in refinecellsandfaces2 and in sample paths
   integer :: NDIM = 5 !< sample vector dimension
   double precision, allocatable, dimension(:, :, :) :: zss !< sample data [zs, direction vector x-component, direction vector y-component, refinement criterion, ridge distance], dim(NDIM,MXSAM,MYSAM)

   integer :: Nsamplesmooth = 0 !< number of sample smoothing iterations
   integer :: Nsamplesmooth_last = -1 !< last number of sample smoothing iterations
   integer :: MAXLEVEL = 10 !< maximum number of refinement levels
   double precision :: threshold = 1d2 !< typical obstacle height in grid refinement
   double precision :: thresholdmin = 1d0 !< minimum obstacle height grid refinement
   double precision :: hmin = 1d3 !< minimum cell size
   integer :: jadirectional = 0 !< directional refinement (1) or not (0)

   integer, parameter :: iHesstat_OK = 0 !< sample Hessians up-to-date
   integer, parameter :: iHesstat_DIRTY = 1 !< sample Hessians out-of-date
   integer :: iHesstat = 0 !< sample Hessians up-to-date (0) or not (1)

   integer, parameter :: ITYPE_RIDGE = 1 !< critetrion based on ridge-detection
   integer, parameter :: ITYPE_WAVECOURANT = 2 !< critetrion based on wave Courant number
   integer, parameter :: ITYPE_MESHWIDTH = 3 !< criterion based on maximum mesh width
   integer :: irefinetype = ITYPE_WAVECOURANT !< refinement criterion type
   integer :: jaconnect = 1 !< connect hanging nodes (1) or not (0)
   double precision :: Dt_maxcour = 120d0 !< maximum time-step in courant grid
   double precision :: Dx_mincour = 750. !< minimum edge length in courant grid
   double precision :: dminsampledist = 0d0 !< minimum sample distance
   integer :: jaoutsidecell = 1 !< take samples outside cell into account (1) or not (0)
   integer :: numrefcycles = 20, numrefcyc = 0 !< max and act nr of non interactive cycles
end module m_samples_refine
