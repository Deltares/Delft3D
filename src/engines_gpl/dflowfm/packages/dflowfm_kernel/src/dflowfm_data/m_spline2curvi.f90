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

!> global variables for spline2curvi
module m_spline2curvi
   use precision, only: dp
   integer, parameter :: Nsubmax = 10 !< maximum number of subintervals of grid layers, each having their own exponential grow factor
   type tspline !< center spline type that contains information derived from cross splines
!     cross spline data
      integer :: id !< 0: center spline, 1: true cross spline, 2: unassociated bounding spline, 3: artificial cross spline, -i, where i>0: associated bounding spline, with centerspline -i
      integer :: ncs !< number of cross splines
      real(kind=dp) :: length !< spline path length
      real(kind=dp) :: hmax !< maximum grid height
      integer, allocatable, dimension(:) :: ics !< cross spline numbers
      logical, allocatable, dimension(:) :: Lorient !< cross spline is left to right (.true.) or not (.false.) w.r.t. center spline
      real(kind=dp), allocatable, dimension(:) :: t !< center spline coordinates of cross splines
      real(kind=dp), allocatable, dimension(:) :: cosphi !< cosine of crossing angle
      real(kind=dp), allocatable, dimension(:, :) :: hL !< left-hand side grid heights at cross spline locations for each grid layer subinterval, hL(1,:) being the height of the first subinterval, etc.
      real(kind=dp), allocatable, dimension(:, :) :: hR !< right-hand side grid heights at cross spline locations for each grid layer subinterval, hR(1,:) being the height of the first subinterval, etc.
      integer, allocatable, dimension(:) :: NsubL !< number of subintervals of grid layers at cross spline locations at the left-hand side of the spline, each having their own exponential grow factor
      integer, allocatable, dimension(:) :: NsubR !< number of subintervals of grid layers at cross spline locations at the right-hand side of the spline, each having their own exponential grow factor

!     grid data
      integer :: mfac !< number of grid intervals on the spline
      integer, dimension(Nsubmax) :: nfacL !< number of grid layers in each subinterval at the left-hand side of the spline *not used yet*
      integer, dimension(Nsubmax) :: nfacR !< number of grid layers in each subinterval at the right-hand side of the spline *not used yet*
      integer :: iL !< index in the whole gridline array of the first grid point on the left-hand side of the spline
      integer :: iR !< index in the whole gridline array of the first grid point on the right-hand side of the spline
   end type

   type(tspline), allocatable, dimension(:) :: splineprops !< spline properties

   real(kind=dp), allocatable, dimension(:) :: xg1, yg1 !< coordinates of the first gridline
   real(kind=dp), allocatable, dimension(:) :: sg1 !< center spline coordinates of the first gridline

   integer :: jacirc = 0 !< circularly connected grid (1) or not (0) (for netbound2curvi), note: valid for one gridline only

   integer :: jaoutside = 1 ! grow the grid outside the prescribed grid height

   real(kind=dp) :: daspect = 0.1d0 ! aspect ratio
   real(kind=dp) :: dgrow = 1.1d0 ! grow factor of aspect ratio
   real(kind=dp) :: dheight0 = 1.0d1 ! grid layer height
   real(kind=dp) :: maxaspect = 1.0d0 ! maximum cell aspect ratio *inoperative*
   real(kind=dp) :: dwidth = 0.5d3 ! average mesh width on center spline

   real(kind=dp) :: dtolLR = 1d-4 ! on-top-of-each-other tolerance *IMPORTANT*
   real(kind=dp) :: dtolcos = 0.95d0 ! minimum allowed absolute value of crossing-angle cosine

   integer :: NFACUNIMAX = 5 ! maximum number of layers in the uniform part

   integer :: jaCheckFrontCollision = 0 ! check for collisions with other parts of the front (1) or not (0)

   real(kind=dp) :: dunigridsize = 0d0 ! uniform grid size (netboundary to grid only)

   integer :: jacurv = 1 ! curvature adapted grid spacing (1) or not (0)

end module m_spline2curvi
