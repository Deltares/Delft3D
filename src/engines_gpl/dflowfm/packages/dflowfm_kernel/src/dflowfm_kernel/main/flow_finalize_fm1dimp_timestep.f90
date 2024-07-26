!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2022.
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

!> Updates the flow variables of FM when the Sobek-RE kernel is used.

!This function cannot be in the <fm1dimp> module because it uses FM variables and
!while the kernel of FM depends on the <fm1dimp> module, the opposite is not true.

subroutine flow_finalize_fm1dimp_timestep()

!
!MODULES
!

   use m_flow, only: s1, u1, s0, au, qa
   use m_fm_erosed, only: ndx_mor, lnx_mor, ln_mor
   use m_f1dimp, only: f1dimppar

   implicit none

!
!DECLARATION
!

!pointer

   integer, pointer :: ngrid

   integer, dimension(:), pointer :: grd_sre_fm
   integer, dimension(:), pointer :: grd_fm_sre

   integer, dimension(:, :), pointer :: grd_fmL_sre
   integer, dimension(:, :), pointer :: grd_fmLb_sre

   real, dimension(:), pointer :: x

   real, dimension(:, :), pointer :: waoft

   double precision, dimension(:, :), pointer :: hpack
   double precision, dimension(:, :), pointer :: qpack

!locals

   integer :: L, n1, n2, idx_sre, kndx

!
!SET POINTERS
!

!dependent on gridpoints
   x => f1dimppar%x
   waoft => f1dimppar%waoft
   hpack => f1dimppar%hpack
   qpack => f1dimppar%qpack
   ngrid => f1dimppar%ngrid
   grd_sre_fm => f1dimppar%grd_sre_fm
   grd_fm_sre => f1dimppar%grd_fm_sre
   grd_fmL_sre => f1dimppar%grd_fmL_sre
   grd_fmLb_sre => f1dimppar%grd_fmLb_sre

!
!UPDATE
!

   do kndx = 1, ndx_mor !loop on FM nodes
      idx_sre = grd_fm_sre(kndx)

      s0(kndx) = hpack(idx_sre, 1)
      s1(kndx) = hpack(idx_sre, 3)
   end do

   do L = 1, lnx_mor
      n1 = grd_fm_sre(ln_mor(1, L))
      n2 = grd_fm_sre(ln_mor(2, L))
      u1(L) = 0.5 * qpack(n1, 3) / waoft(n1, 3) + 0.5 * qpack(n2, 3) / waoft(n2, 3)
      au(L) = 0.5 * waoft(n1, 3) + 0.5 * waoft(n2, 3)
      !q1(L)=au(L)*u1(L)
      qa(L) = au(L) * u1(L)
   end do

end subroutine flow_finalize_fm1dimp_timestep
