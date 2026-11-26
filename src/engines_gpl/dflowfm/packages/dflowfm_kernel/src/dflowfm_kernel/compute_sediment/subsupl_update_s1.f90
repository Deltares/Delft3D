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

!
!

! update the water levels for the bed level change caused by subsidence/uplift
module m_subsupl_update_s1

   implicit none

   private

   public :: subsupl_update_s1
   public :: subsupl_update_constituents
   public :: remap_constituents_after_bed_change

contains

   subroutine subsupl_update_s1()
      use m_subsidence, only: sdu_update_s1, sdu_blp
      use m_flowgeom, only: ndx, bl
      use m_flow, only: s1
      use m_flowparameters, only: epshs

      implicit none

      integer :: k !< face/cell index

      if (sdu_update_s1 == 1) then
         ! update s1 in all cells
         do k = 1, ndx
            ! adjust water level for subsidence/uplift
            s1(k) = s1(k) + (bl(k) - sdu_blp(k))
         end do
      else
         ! update s1 only in dry cells
         do k = 1, ndx
            if ((s1(k) - sdu_blp(k)) < epshs) then
               ! adjust water level in dry areas for subsidence/uplift
               s1(k) = s1(k) + (bl(k) - sdu_blp(k))
            else
               ! avoid negative depths in case of drying due to uplift
               s1(k) = max(s1(k), bl(k))
            end if
         end do
      end if
   end subroutine subsupl_update_s1

   !> Preserve transported mass using actual volumes before and after the ground movement.
   subroutine subsupl_update_constituents()
      use m_subsidence, only: sdu_volp
      use m_flow, only: kmx, ktop0

      character(len=*), parameter :: drying_message = 'Subsidence/uplift dried a cell containing transported mass. '// &
                                                       'Use SubsUplUpdateS1=1 to retain its water volume.'

      if (kmx > 0) then
         call remap_constituents_after_bed_change(sdu_volp, drying_message, ktop0)
      else
         call remap_constituents_after_bed_change(sdu_volp, drying_message)
      end if
   end subroutine subsupl_update_constituents

   !> Conserve each transported constituent across a change in wet cell or layer volume.
   subroutine remap_constituents_after_bed_change(previous_volume, drying_message, previous_ktop, stranded_mass, first_cell)
      use precision, only: dp
      use m_flow, only: vol1
      use m_flowgeom, only: ndxi
      use m_transport, only: numconst, constituents
      use m_get_kbot_ktop, only: getkbotktop
      use messagehandling, only: mess, LEVEL_FATAL

      real(kind=dp), dimension(:), intent(in) :: previous_volume !< Water volumes before the bed change.
      character(len=*), intent(in), optional :: drying_message !< Fatal message if no dry-cell transfer is supplied.
      integer, dimension(:), optional, intent(in) :: previous_ktop !< Top layers before the bed change.
      real(kind=dp), dimension(:, :), allocatable, intent(out), optional :: stranded_mass !< Amount left in dry cells.
      integer, intent(in), optional :: first_cell !< First internal cell to remap.

      integer :: n, k, kb, kt, old_kt, start_cell
      integer :: k_receiver !< cell receiving the constituent mass
      real(dp) :: displaced_mass(numconst)

      if (numconst == 0) then
         return
      end if
      start_cell = 1
      if (present(first_cell)) then
         start_cell = first_cell
      end if
      do n = start_cell, ndxi
         call getkbotktop(n, kb, kt)
         old_kt = kt
         if (present(previous_ktop)) then
            old_kt = previous_ktop(n)
         end if
         displaced_mass = 0.0_dp
         do k = kb, max(kt, old_kt)
            if (vol1(k) > 0.0_dp) then
               if (previous_volume(k) > 0.0_dp) then
                  constituents(:, k) = constituents(:, k) * (previous_volume(k) / vol1(k))
               else
                  constituents(:, k) = 0.0_dp
               end if
            else
               if (previous_volume(k) > 0.0_dp) then
                  displaced_mass = displaced_mass + constituents(:, k) * previous_volume(k)
               end if
               constituents(:, k) = 0.0_dp
            end if
         end do
         if (any(displaced_mass /= 0.0_dp)) then
            if (sum(vol1(kb:kt)) <= 0.0_dp) then
               if (.not. present(stranded_mass)) then
                  if (present(drying_message)) then
                     call mess(LEVEL_FATAL, drying_message)
                  else
                     call mess(LEVEL_FATAL, 'A bed change dried a cell containing transported mass.')
                  end if
                  return
               end if
               if (.not. allocated(stranded_mass)) then
                  allocate (stranded_mass(numconst, ndxi), source=0.0_dp)
               end if
               stranded_mass(:, n) = displaced_mass
               cycle
            end if
            k_receiver = kb - 1 + maxloc(vol1(kb:kt), dim=1)
            constituents(:, k_receiver) = constituents(:, k_receiver) + displaced_mass / vol1(k_receiver)
         end if
      end do
   end subroutine remap_constituents_after_bed_change

end module m_subsupl_update_s1
