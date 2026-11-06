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
      use precision, only: dp
      use m_subsidence, only: sdu_volp
      use m_flow, only: vol1, kmx, ktop0
      use m_flowgeom, only: ndxi
      use m_transport, only: numconst, constituents
      use m_get_kbot_ktop, only: getkbotktop
      use messagehandling, only: mess, LEVEL_FATAL

      integer :: n, k, kb, kt, old_kt, receiver
      real(dp) :: displaced_mass(numconst)

      if (numconst == 0) then
         return
      end if
      do n = 1, ndxi
         call getkbotktop(n, kb, kt)
         old_kt = kt
         if (kmx > 0) then
            old_kt = ktop0(n)
         end if
         displaced_mass = 0.0_dp
         do k = kb, max(kt, old_kt)
            if (vol1(k) > 0.0_dp) then
               if (sdu_volp(k) > 0.0_dp) then
                  constituents(:, k) = constituents(:, k) * (sdu_volp(k) / vol1(k))
               else
                  constituents(:, k) = 0.0_dp
               end if
            else
               if (sdu_volp(k) > 0.0_dp) then
                  displaced_mass = displaced_mass + constituents(:, k) * sdu_volp(k)
               end if
               constituents(:, k) = 0.0_dp
            end if
         end do
         if (any(displaced_mass /= 0.0_dp)) then
            if (sum(vol1(kb:kt)) <= 0.0_dp) then
               call mess(LEVEL_FATAL, 'Subsidence/uplift dried a cell containing transported mass. '// &
                         'Use SubsUplUpdateS1=1 to retain its water volume.')
               return
            end if
            receiver = kb - 1 + maxloc(vol1(kb:kt), dim=1)
            constituents(:, receiver) = constituents(:, receiver) + displaced_mass / vol1(receiver)
         end if
      end do
   end subroutine subsupl_update_constituents

end module m_subsupl_update_s1
