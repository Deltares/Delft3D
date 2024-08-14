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

!> netcell-based cell-coarsening information
double precision function coarsening_info(k)

   use m_netw
   use m_missing
   use m_sferic, only: dtol_pole
   use gridoperations

   implicit none

   integer :: k !< netcell number

   integer, parameter :: NMAX = 100 !< maximum of directly or indirectly connected netcells

   integer :: ndirect, nindirect
   integer, dimension(NMAX) :: kdirect, kindirect
   integer, dimension(2, nmax) :: kne

   double precision :: xc, yc
   double precision :: area, area_tot, Darea
   double precision :: funct

   double precision :: area_opt = 1d5

   integer :: kk

   coarsening_info = DMISS

   call find_surrounding_cells(k, NMAX, ndirect, nindirect, kdirect, kindirect, kne)

   if (ndirect < 1 .or. nindirect < 1) return

   area_tot = 0d0
   funct = 0d0

   call getcellsurface(k, area, xc, yc)
   area_tot = area_tot + area
   funct = funct - (area - area_opt)**2
   do kk = 1, ndirect
      call getcellsurface(kdirect(kk), area, xc, yc)
      area_tot = area_tot + area
      funct = funct - (area - area_opt)**2
   end do

!  compute the area increase of the indirectly connected cells
   Darea = area_tot / dble(nindirect)

!  compute the change in the functional
   do kk = 1, nindirect
      call getcellsurface(kindirect(kk), area, xc, yc)
      funct = funct - (area - area_opt)**2
      area = area + Darea
      funct = funct + (area - area_opt)**2
   end do

   coarsening_info = -funct
   if (coarsening_info <= 0d0) coarsening_info = DMISS

   return
end function
