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

 ! delete curviliniar grid outside polygon(s)
 subroutine del_grid_outside_pol()
    use m_grid
    use m_polygon
    use m_tpoly
    use m_missing
    implicit none

    type(tpoly), dimension(:), allocatable :: pols

    integer, dimension(:, :), allocatable :: kn ! grid node-based mask
    integer, dimension(:, :), allocatable :: kc ! grid cell-based mask

    integer :: numpols, inpol
    integer :: i, j
    integer :: ipol

    integer :: ierror ! error (1) or not (0)

    ierror = 0
    if (NPL < 2 .or. MC < 1 .or. NC < 1) goto 1234 ! nothing to do

    ierror = 1

!  allocate
    allocate (kn(MC, NC))
    kn = 0
    allocate (kc(MC - 1, NC - 1))
    kc = 0

!  convert global polygon to array of tpoly-type polygons
    call pol_to_tpoly(numpols, pols, keepExisting=.false.)

!        loop over polygons
    do ipol = 1, numpols
!     mask grid points that are inside a polygon
       inpol = 0 ! do not initialize (already in pol_to_tpoly)
       do j = 1, NC
          do i = 1, MC
             call dbpinpol_tpoly(pols(ipol), xc(i, j), yc(i, j), inpol)
             if (inpol == 1) then
                kn(i, j) = 1
             end if
          end do
       end do
    end do

!  mark grid cells inside oudside polygons when at least one of its nodes is inside
    do j = 1, NC - 1
       do i = 1, MC - 1
          if (kn(i, j) == 1 .or. kn(i + 1, j) == 1 .or. kn(i, j + 1) == 1 .or. kn(i + 1, j + 1) == 1) then
             kc(i, j) = 1
          end if
       end do
    end do

!  mark nodes that are member of a cell inside the polygon(s)
    kn = 0
    do j = 1, NC - 1
       do i = 1, MC - 1
          if (kc(i, j) == 1) then
             kn(i, j) = 1
             kn(i + 1, j) = 1
             kn(i, j + 1) = 1
             kn(i + 1, j + 1) = 1
          end if
       end do
    end do

!  remove grid cells outside polygon
    do j = 1, NC
       do i = 1, MC
          if (kn(i, j) == 0) then
             xc(i, j) = DMISS
             yc(i, j) = DMISS
          end if
       end do
    end do

    ierror = 0
1234 continue

    call dealloc_tpoly(pols)
    if (allocated(kn)) deallocate (kn)
    if (allocated(kc)) deallocate (kc)

    return
 end subroutine del_grid_outside_pol
