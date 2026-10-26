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

module m_pillar_update

   implicit none

contains

   ! =================================================================================================
   ! =================================================================================================
   !< Updates the pillar advection term for each link or node depending on the chosen pillar method.
   subroutine pillar_update()
      use precision, only: dp
      use fm_external_forcings_data, only: Cpil
      use m_flowgeom, only: lnx, ln, dx
      use m_flow, only: u1, v, advi, kmx
      use m_flowparameters, only: pillar_method, PILLAR_DELFT3D_FLOWNODE, PILLAR_DELFT3D_FLOWLINK
      use m_get_Lbot_Ltop, only: getLbotLtop

      integer :: L !< Link counter
      integer :: k1 !< First neighbouring node to link
      integer :: k2 !< Second neighbouring node to link
      integer :: LL !< Loop counter for vertical layers within a link
      integer :: Lb !< Bottom layer index of the link
      integer :: Lt !< Top layer index of the link
      real(kind=dp) :: CpilL !> Pillar drag factor as 0.5 * Cd * D * velocity_squared_correction_per_width

      if (kmx == 0) then
         select case (pillar_method)
         case (PILLAR_DELFT3D_FLOWNODE)
            do L = 1, lnx
               k1 = ln(1, L)
               k2 = ln(2, L)
               CpilL = (Cpil(k1) + Cpil(k2)) * 0.5_dp
               if (CpilL == 0.0_dp) then
                  cycle
               end if
               advi(L) = advi(L) + compute_pillar_advection_term(u1(L), v(L), dx(L), CpilL)
            end do
         case (PILLAR_DELFT3D_FLOWLINK)
            do L = 1, lnx
               if (Cpil(L) == 0.0_dp) then
                  cycle
               end if
               CpilL = Cpil(L)
               advi(L) = advi(L) + compute_pillar_advection_term(u1(L), v(L), dx(L), CpilL)
            end do
         end select
      else
         select case (pillar_method)
         case (PILLAR_DELFT3D_FLOWNODE)
            do L = 1, lnx
               k1 = ln(1, L)
               k2 = ln(2, L)
               CpilL = (Cpil(k1) + Cpil(k2)) * 0.5_dp
               if (CpilL == 0.0_dp) then
                  cycle
               end if
               call getLbotLtop(L, Lb, Lt)
               do LL = Lb, Lt
                  advi(LL) = advi(LL) + compute_pillar_advection_term(u1(LL), v(LL), dx(L), CpilL)
               end do
            end do
         case (PILLAR_DELFT3D_FLOWLINK)
            do L = 1, lnx
               call getLbotLtop(L, Lb, Lt)
               if (Cpil(L) == 0.0_dp) then
                  cycle
               end if
               CpilL = Cpil(L)
               do LL = Lb, Lt
                  advi(LL) = advi(LL) + compute_pillar_advection_term(u1(LL), v(LL), dx(L), CpilL)
               end do
            end do
         end select
      end if

   end subroutine pillar_update

   !> Compute the advection term due to pillar drag in the horizontal plane
   pure function compute_pillar_advection_term(uL, vL, dxL, CpilL) result(advi_Cpil)
      use precision, only: dp

      real(kind=dp), intent(in) :: uL !> Velocity component in the x-direction
      real(kind=dp), intent(in) :: vL !> Velocity component in the y-direction
      real(kind=dp), intent(in) :: dxL !> Grid spacing in the x-direction
      real(kind=dp), intent(in) :: CpilL !> Pillar drag factor as 0.5 * Cd * D * velocity_squared_correction_per_width
      real(kind=dp) :: advi_Cpil !> Advection term due to pillar drag
      real(kind=dp) :: uv !> Magnitude of the velocity vector in the horizontal plane

      uv = sqrt(uL * uL + vL * vL)
      advi_Cpil = CpilL * uv / dxL

   end function compute_pillar_advection_term

end module m_pillar_update
