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

module m_partition_to_idomain

   implicit none

   private

   public :: partition_to_idomain

contains

!> generate partition numbers from polygons, or with METIS of no polygons are present
   subroutine partition_to_idomain()
      use m_cosphiunetcheck, only: cosphiunetcheck
      use m_polygon, only: npl
      use m_partitioninfo, only: ndomains, numndx, idomain, generate_partition_pol_from_idomain, &
                                 generate_partitioning_from_pol, partition_pol_to_idomain
      use MessageHandling, only: LEVEL_INFO, mess
      use gridoperations, only: findcells
      use m_find1dcells, only: find1dcells
      use m_getint, only: getint
      use m_partition_METIS_to_idomain, only: partition_METIS_to_idomain
      use m_delete_dry_points_and_areas, only: delete_dry_points_and_areas

      integer :: japolygon
      integer :: i, jacontiguous, method
      integer :: NPL_save
      integer :: ierror

      character(len=100) :: message

!     save polygons
      NPL_save = NPL

!     disable polygons
      NPL = 0

      call findcells(0)
      call find1dcells()

!     reenable polygons
      NPL = NPL_save

      call delete_dry_points_and_areas()

      call cosphiunetcheck(1)

      if (NPL > 1) then ! use the polygons
         call generate_partitioning_from_pol()
      else ! use metis
         Ndomains = 0
         do while (Ndomains < 1)
            call getint('Number of domains', Ndomains)
         end do
         method = -1
         do while (method < 0 .or. method > 3)
            method = 1
            call getint('Partition method? (1: K-Way, 2: Recursive bisection, 3: Mesh-dual)', method) ! default method is K-way
         end do
         jacontiguous = -1
         if (method == 1 .or. method == 0) then ! K-Way (default) method enables contiguous
            do while (jacontiguous /= 0 .and. jacontiguous /= 1)
               jacontiguous = 1
               call getint('Enforce contiguous domains? (0:no, 1:yes)', jacontiguous)
            end do
         end if
         call partition_METIS_to_idomain(Ndomains, jacontiguous, method, 0)

         japolygon = -1
         do while (japolygon /= 1 .and. japolygon /= 0)
            japolygon = 0
            call getint('Generate polygon? (0: no, 1: yes)', japolygon)
         end do
         if (japolygon == 1) then
!            generate partitioning polygons
            call generate_partition_pol_from_idomain(ierror)

            if (ierror == 0) then
!               get 1D domain numbers from polygons
               call partition_pol_to_idomain(2)
            else
               japolygon = 0
            end if
         end if
      end if

      if (allocated(numndx)) then
         deallocate (numndx)
      end if
      allocate (numndx(0:ndomains - 1))

!     count and output number of cells
      do i = 0, ndomains - 1
         numndx(i) = count(idomain == i)
         write (message, "('domain', I5, ' contains', I7, ' cells.')") i, numndx(i)
         call mess(LEVEL_INFO, message)
      end do

   end subroutine partition_to_idomain

end module m_partition_to_idomain
