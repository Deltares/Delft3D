!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2026.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D" and "Deltares"
!  are registered trademarks of Stichting Deltares, and remain the property of
!  Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------
!
submodule (geometry_module) geometry_module_dbpinpol
   
   implicit none
   
contains
   
   module subroutine dbpinpol(xp, yp, in, dmiss, JINS, NPL, xpl, ypl, zpl)
      use m_cellmask_from_polygon_set, only: cellmask_from_polygon_set_init, &
                                             cellmask_from_polygon_set, &
                                             cellmask_from_polygon_set_cleanup
      implicit none

      real(kind=dp), intent(in) :: xp, yp
      integer, intent(inout) :: in
      real(kind=dp), intent(in) :: dmiss
      integer, intent(in) :: JINS, NPL
      real(kind=dp), optional, intent(in) :: xpl(:), ypl(:), zpl(:)

      integer :: num
      logical, save :: initialized = .false.

      ! Special case: NPL == 0 means "no polygons to check, everything is considered inside"
      if (NPL == 0) then
         in = 1
         return
      end if

      ! Initialization phase (when in < 0)
      if (in < 0) then
         ! Clean up any previous initialization
         if (initialized) then
            call cellmask_from_polygon_set_cleanup()
         end if

         ! Build optimized spatial index
         if (present(xpl)) then
            call cellmask_from_polygon_set_init(NPL, xpl, ypl, zpl)
            initialized = .true.
         end if

         in = 0 ! Reset for subsequent queries
      end if

      ! Query phase (when in >= 0)
      if (.not. initialized) then
         ! Safety: if someone forgot to initialize
         in = 0
         return
      end if

      ! Use your optimized point-in-polygon with bounding boxes
      in = cellmask_from_polygon_set(xp, yp)

   end subroutine dbpinpol

end submodule geometry_module_dbpinpol