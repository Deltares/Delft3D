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

module m_settaubxu_nowave

   implicit none

   private

   public :: settaubxu_nowave

contains

   subroutine settaubxu_nowave(use_u1)
      use precision, only: dp
      use m_flowgeom, only: ln, lnx
      use m_flow, only: taubxu, u1, u0, hu, epshu, frcu, v, ifrcutp, z0urou, VON_KARMAN, sag, rhomean, cfuhi, kmx
      use m_vegetation, only: javeg, jabaptist, rnveg, densvegminbap
      use m_get_Lbot_Ltop, only: getlbotltop
      use m_get_chezy, only: get_chezy
      use m_mathconstants, only: ee
      implicit none

      logical, intent(in) :: use_u1 !< Flag for using `u1` (.true.) or `u0` (.false.) in computing `taubxu` in subroutine `settaubxu_nowave`

      integer :: L, Lb, Lt, k1, k2
      logical :: use_vegetation_bed_drag, vegetation_link
      real(kind=dp), parameter :: minimum_roughness = 1.0e-200_dp
      real(kind=dp) :: cz, cwall, rz, umod2
      real(kind=dp), pointer :: velocity_pointer(:)

      taubxu = 0.0_dp
      use_vegetation_bed_drag = kmx == 0 .and. javeg == 1 .and. jabaptist > 1

      if (use_u1) then
         velocity_pointer => u1
      else
         velocity_pointer => u0
      end if

      do L = 1, lnx
         call getLbotLtop(L, Lb, Lt)
         if (Lt < Lb .or. hu(L) <= epshu) then
            cycle
         end if

         vegetation_link = .false.
         if (use_vegetation_bed_drag) then
            k1 = ln(1, L)
            k2 = ln(2, L)
            vegetation_link = 0.5_dp * (rnveg(k1) + rnveg(k2)) > densvegminbap .and. cfuhi(L) >= 0.0_dp
         end if

         if (vegetation_link) then
            ! Undo the normalization by link depth in setbaptist.
            cwall = cfuhi(L) * hu(L)
            z0urou(L) = minimum_roughness
            if (cwall > 0.0_dp) then
               z0urou(L) = max(minimum_roughness, hu(L) * exp(-1.0_dp - VON_KARMAN / sqrt(cwall)))
            end if
         else if (frcu(L) > 0.0_dp) then ! input friction or trachytopes
            cz = get_chezy(hu(L), frcu(L), u1(L), v(L), ifrcutp(L))
            z0urou(L) = max(minimum_roughness, hu(L) * exp(-1.0_dp - VON_KARMAN * cz / sag))
            rz = max(hu(Lb), epshu) / ee / z0urou(L)
            cz = log(rz) / VON_KARMAN
            cwall = 1.0_dp / (cz**2)
         else
            cycle
         end if

         umod2 = velocity_pointer(Lb)**2 + v(Lb)**2
         taubxu(L) = rhomean * cwall * umod2
      end do

   end subroutine settaubxu_nowave

end module m_settaubxu_nowave
