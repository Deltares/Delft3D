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

!>  prescribe link-based aspect ratios in nets for mesh refinement (obsolete)
subroutine orthonet_prescribe_aspect_net(smp_mu, idir, aspect)
   use m_netw
   use m_sferic
   use m_missing
   use geometry_module, only: dbdistance, dcosphi

   implicit none

   double precision, dimension(numk) :: smp_mu !< mesh attractor

   integer :: idir !< mesh adaptation direction

   double precision, dimension(numL) :: aspect !< aspect ratio at the links
   double precision :: A, mu

   integer :: L, k1, k2

   do L = 1, numL
!     compute the angle of link L with the prescribed orientation
      k1 = kn(1, L)
      k2 = kn(2, L)

      if (kc(k1) /= 1 .or. kc(k2) /= 1) cycle

      mu = 0.5d0 * (smp_mu(k1) + smp_mu(k2))

      A = mu
      aspect(L) = aspect(L) * A

   end do

end subroutine orthonet_prescribe_aspect_net
