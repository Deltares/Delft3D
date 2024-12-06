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
module m_get_zlayer_indices
   implicit none
contains
   !> Gets the local layer numbers for a given grid cell.
   subroutine getzlayerindices(n, nlayb, nrlay)
      use precision, only: dp
      use m_flowgeom, only: bl
      use m_flow

      integer, intent(in) :: n !< Flow node/grid cell number
      integer, intent(out) :: nlayb !< Layer number for the bottom layer (in 1:kmx)
      integer, intent(out) :: nrlay !< Nr. of active layers for this flow node.

      integer :: k, Ltn, mx ! layerdistribution indexes

      real(kind=dp) :: fac, dzz

      Ltn = laydefnr(n)
      mx = laymx(Ltn)
      nlayb = mx; nrlay = 1 ! default

      if (keepzlayeringatbed == 0 .or. keepzlayeringatbed == 1 .and. keepzlay1bedvol == 1) then
         fac = 0.2d0
      else
         fac = 0.0d0
      end if

      if (nlaybn(n) == 0) then
         do k = 1, mx
            dzz = fac * (zslay(k, Ltn) - zslay(k - 1, Ltn))
            if (numtopsig > 0 .and. janumtopsiguniform == 1) then
               if (zslay(k, Ltn) > bl(n) + dzz .or. mx - k + 1 <= numtopsig) then
                  nlayb = k
                  nrlay = mx - k + 1
                  exit
               end if
            else
               if (zslay(k, Ltn) > bl(n) + dzz) then
                  nlayb = k
                  nrlay = mx - k + 1
                  exit
               end if
            end if
         end do
         nlaybn(n) = nlayb
         nrlayn(n) = nrlay

      else
         nlayb = nlaybn(n)
         nrlay = nrlayn(n)
      end if

   end subroutine getzlayerindices
end module m_get_zlayer_indices
