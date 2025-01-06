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

module m_monitoring_runupgauges
   use precision, only: dp
   use m_crspath
   use m_missing
   use MessageHandling, only: IdLen

   implicit none

   type t_runup_gauge
      character(len=IdLen) :: name !< Name
      type(tcrspath) :: path !< Polyline+crossed flow links that defines this runup gauge.
      real(kind=dp) :: max_x !< flow node xz of max runup
      real(kind=dp) :: max_y !< flow node xy of max runup
      real(kind=dp) :: max_rug_height !< Runup water level
   end type t_runup_gauge

   type(t_runup_gauge), allocatable, target :: rug(:)
   integer :: num_rugs = 0
   integer :: rug_allocated_size = 2
   character(len=*), parameter, private :: defaultName_ = 'Rug'
   integer, private :: runup_gauge_id = 1

contains

!> Double run up gauge array size if necessary
   subroutine increase_runup_gauges(new_rug_size)
      integer, intent(in) :: new_rug_size !< Desired number of run up gauges

      type(t_runup_gauge), allocatable :: temp_rug_array(:)

      if (new_rug_size < rug_allocated_size .and. allocated(rug)) then
         return
      end if

      call allocate_runup_gauges(temp_rug_array, rug_allocated_size)

      if (new_rug_size > rug_allocated_size) then
         rug_allocated_size = max(rug_allocated_size, int(2 * new_rug_size))
      end if

      if (allocated(rug)) then
         call copy_runup_gauges(rug, temp_rug_array)
      end if
      call allocate_runup_gauges(rug, rug_allocated_size)
      call copy_runup_gauges(temp_rug_array, rug)

      call deallocate_runup_gauges(temp_rug_array)

   end subroutine increase_runup_gauges

!> set rug_allocated_size to num_rugs, after all necessary runupgauges have been added
   subroutine decrease_runup_gauges()

      type(t_runup_gauge), allocatable :: temp_rug_array(:)

      if (num_rugs == rug_allocated_size .or. .not. allocated(rug)) then
         return
      end if

      call allocate_runup_gauges(temp_rug_array, num_rugs)
      call copy_runup_gauges(rug(1:num_rugs), temp_rug_array)
      call deallocate_runup_gauges(rug)
      call allocate_runup_gauges(rug, num_rugs)
      call copy_runup_gauges(temp_rug_array, rug)
      call deallocate_runup_gauges(temp_rug_array)
      rug_allocated_size = num_rugs

   end subroutine decrease_runup_gauges

!> Allocates an array of runup gauges, deallocating any existing memory.
   subroutine allocate_runup_gauges(rug, rug_size)

      type(t_runup_gauge), allocatable, intent(inout) :: rug(:) !< Array of run up gauges
      integer, intent(in) :: rug_size !< Desired size of rug

      call deallocate_runup_gauges(rug)
      allocate (rug(rug_size))

   end subroutine allocate_runup_gauges

!> Deallocates an array of runup gauges by first deallocating the cross section paths
   subroutine deallocate_runup_gauges(rug)
      type(t_runup_gauge), allocatable, intent(inout) :: rug(:)

      integer :: i

      if (.not. allocated(rug)) return

      do i = 1, size(rug)
         call deallocCrossSectionPath(rug(i)%path)
      end do
      deallocate (rug)
   end subroutine deallocate_runup_gauges

!> Copies array of runup gauges into another array of runup gauges.
   subroutine copy_runup_gauges(source_rug, target_rug)
      use m_alloc
      type(t_runup_gauge), intent(inout) :: source_rug(:) !> array with runup gauges that need to be copied to target
      type(t_runup_gauge), intent(inout) :: target_rug(:) !> target array runup gauges need to be copied to

      integer :: i, size_source_rug

      size_source_rug = size(source_rug)
      if (size_source_rug > size(target_rug) .or. size_source_rug == 0) return

      do i = 1, size_source_rug
         target_rug(i) = source_rug(i)
      end do
   end subroutine copy_runup_gauges

!> Converts a set of polylines into runup gauges.
!! The input arrays have the structure of the global polygon:
!! one or more polylines separated by dmiss values.
   subroutine polyline_to_runupgauges(pl_x, pl_y, pl_n, names)
      use precision, only: dp
      use m_missing

      real(kind=dp), intent(in) :: pl_x(:), pl_y(:) !< Long array with one or more polylines, separated by dmiss
      integer, intent(in) :: pl_n !< Total number of polyline points
      character(len=*), optional, intent(in) :: names(:) !< Optional names for run up gauges

      integer :: i, i_start, i_end, i_poly, num_names
      character(len=IdLen) :: name

      if (present(names)) then
         num_names = size(names)
      else
         num_names = 0
      end if

      i_start = 1 ! First possible start index
      i_end = 0 ! No end index found yet.
      i_poly = 0 ! polyline index
      do i = 1, pl_n
         if (pl_x(i) == dmiss .or. i == pl_n) then
            if (i == pl_n .and. pl_x(i) /= dmiss) then
               i_end = i ! Last polyline, no dmiss separator, so also include last point #pl_n.
            end if
            if (i_start <= i_end) then
               ! 1: Special name for this rug or not?
               i_poly = i_poly + 1
               if (i_poly <= num_names) then
                  name = names(i_poly)
               else
                  name = ' '
               end if
               ! 2: add the current polyline as a new rug.
               call add_runup_gauges(name, pl_x(i_start:i_end), pl_y(i_start:i_end))
            end if
            i_start = i + 1
            cycle
         else
            i_end = i ! Advance end point by one.
         end if
      end do

      call decrease_runup_gauges()

   end subroutine polyline_to_runupgauges

!> Reads observation rug and adds them to the normal rug adm
   subroutine load_runup_gauges(file_name, append)
      use unstruc_messages

      implicit none
      character(len=*), intent(in) :: file_name !< File containing the observation rug. Either a *_rug.pli.
      logical, intent(in) :: append !< Append to existing observation rug or not

      logical :: file_exists, file_is_polyline

      inquire (file=file_name, exist=file_exists)
      if (file_exists) then
         if (.not. append) then
            num_rugs = 0
         end if
         file_is_polyline = index(file_name, '_rug.pli') > 0
         if (file_is_polyline) then
            call load_runup_gauges_from_pli(file_name)
         else
            call mess(LEVEL_WARN, "Runup gauge file ('"//trim(file_name)//"') does not end with _rug.pli.")
         end if
      else
         call mess(LEVEL_ERROR, "Runup gauge file '"//trim(file_name)//"' not found!")
      end if
   end subroutine load_runup_gauges

!> Reads rugs from an *.pli file.
   subroutine load_runup_gauges_from_pli(file_name)
      use messageHandling
      use dfm_error
      use m_polygon
      use m_reapol_nampli, only: reapol_nampli
      use m_filez, only: oldfil, doclose

      implicit none
      character(len=*), intent(in) :: file_name !< name of polyline file to load runup gauges from

      integer :: file_pointer, polyline_index

      call oldfil(file_pointer, file_name)
      polyline_index = 0
      call reapol_nampli(file_pointer, 0, 1, polyline_index)
      call polyline_to_runupgauges(xpl, ypl, npl) !TODO refactor reapol_nampli so we no longer have to use horrible global arrays
      call doclose(file_pointer)

   end subroutine load_runup_gauges_from_pli

!> adds runup gauge with name and polyline coordinates
   subroutine add_runup_gauges(name, pl_x, pl_y)
      use precision, only: dp
      character(len=*), intent(in) :: name !> name of the new runup gauge
      real(kind=dp), intent(in) :: pl_x(:), pl_y(:) !> x- and y coordinates of polyline

      integer :: name_length
      character(len=1) :: runup_gauge_digits

      call increase_runup_gauges(num_rugs + 1)

      num_rugs = num_rugs + 1
      call setCrossSectionPathPolyline(rug(num_rugs)%path, pl_x, pl_y)
      rug(num_rugs)%path%lnx = 0

      ! Set name (or generate one)
      name_length = len_trim(name)
      if (name_length > 0) then
         name_length = min(len(rug(num_rugs)%name), len(name))
         rug(num_rugs)%name = ' '
         rug(num_rugs)%name(1:name_length) = name(1:name_length)
      else ! No name given, generate one.
         write (runup_gauge_digits, '(i1)') max(2, int(floor(log10(dble(runup_gauge_id)) + 1)))
         write (rug(num_rugs)%name, '(a,i'//runup_gauge_digits//'.'//runup_gauge_digits//')') trim(defaultName_), runup_gauge_id
         runup_gauge_id = runup_gauge_id + 1
      end if

      ! Set default values
      rug(num_rugs)%max_x = 0d0
      rug(num_rugs)%max_y = 0d0
      rug(num_rugs)%max_rug_height = -huge(0d0)

   end subroutine add_runup_gauges

   subroutine clear_runup_gauges()
      integer :: i
      ! Reset data for next iteration
      do i = 1, num_rugs
         rug(i)%max_rug_height = -huge(0d0)
         rug(i)%max_x = 0d0
         rug(i)%max_y = 0d0
      end do
   end subroutine

end module
