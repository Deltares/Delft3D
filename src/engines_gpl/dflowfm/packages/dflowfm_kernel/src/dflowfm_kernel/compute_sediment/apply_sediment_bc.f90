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

module m_apply_sediment_bc

   use precision, only: dp
   implicit none

   private

   public :: apply_sediment_bc

contains

   !> apply sediment boundary conditions
   subroutine apply_sediment_bc()
      use m_flowgeom
      use m_flow, only: q1
      use m_meteo
      use m_transport, only: ised1, constituents, ifrac2const
      use sediment_basics_module
      use m_fm_erosed
      use m_get_Lbot_Ltop
      implicit none

      integer :: kb !< boundary cell index
      integer :: ki !< internal cell index corresponding to boundary cell
      integer :: L !< 3D flow link index
      integer :: ll !< sediment fraction index (with or without boundary condition)
      integer :: iconst !< constituent index
      integer :: k !< boundary index
      integer :: kk !< 3D boundary index
      integer :: Lb !< bottom-most 3D flow link index
      integer :: Lt !< top-most 3D flow link index
      integer :: LLL !< 2D boundary flow link index

      ! default Neumann boundary condition applied for all suspended sediments
      do ll = 1, lsed ! sediment-fraction index
         iconst = ll + ISED1 - 1 ! constituent index
         do LLL = Lnxi + 1, Lnx
            call getLbotLtop(LLL, Lb, Lt)
            if (Lt < Lb) then
               cycle
            end if
            do L = Lb, Lt
               kb = ln(1, L)
               ki = ln(2, L)
               constituents(iconst, kb) = constituents(iconst, ki)
            end do
         end do
      end do
      !
      ! loop over sediment fractions with specified boundary conditions
      do ll = 1, numfracs
         iconst = ifrac2const(ll)
         if (iconst == 0) then
            cycle
         end if
         ! loop over 2D boundary links associated with this sediment fraction
         do k = 1, nbndsf(ll)
            LLL = bndsf(ll)%k(3, k)
            call getLbotLtop(LLL, Lb, Lt)
            if (Lt < Lb) then
               cycle
            end if
            if (hu(LLL) > 0.0_dp) then
               do L = Lb, Lt
                  kb = ln(1, L)
                  ki = ln(2, L)
                  kk = kmxd * (k - 1) + L - Lb + 1
                  if (q1(L) > 0) then ! inflow
                     constituents(iconst, kb) = bndsf(ll)%z(kk)
                  else ! outflow, same Neumann condition set above
                     constituents(iconst, kb) = constituents(iconst, ki)
                  end if
               end do
            else
               ! set other values (e.g. dry links)
               do L = Lb, Lb + kmxL(LLL) - 1
                  kb = ln(1, L)
                  constituents(iconst, kb) = 0.0_dp
               end do
            end if
         end do
      end do
      !
   end subroutine apply_sediment_bc

end module m_apply_sediment_bc
