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

module m_copynetlinkstosam

   implicit none

contains

   subroutine copynetlinkstosam()
      use m_samples, only: ns, xs, ys, zs, increasesam
      use m_netw, only: kn, numl, rlin, xe, ye, xk, yk, lc
      use m_missing, only: dmiss, jins
      use m_polygon, only: NPL, xpl, ypl, zpl
      use geometry_module, only: dbpinpol

      integer :: in, k, l, K1, K2
      in = -1
      k = ns

!need to compute the coordinates of the links
      if (.not. allocated(xe)) then
         allocate (xe(numl))
      end if
      if (.not. allocated(ye)) then
         allocate (ye(numl))
      end if

      LC = 0
      do l = 1, numl
         if (rlin(l) /= dmiss) then
            K1 = KN(1, L)
            K2 = KN(2, L)
            ! calculate the centre of the link
            xe(l) = .5d0 * (xk(K1) + xk(K2)) ! TODO: LC: make this sferic+3D-safe
            ye(l) = .5d0 * (yk(K1) + yk(K2))
            call DBPINPOL(xe(l), ye(l), IN, dmiss, JINS, NPL, xpl, ypl, zpl)
            if (IN == 1) then
               LC(l) = 1
               K = K + 1
            end if
         end if
      end do

      call INCREASESAM(k)

!assign the calculated value in rlin
      K = NS
      do l = 1, numl
         if (LC(l) == 1) then
            k = k + 1
            xs(k) = xe(l); ys(k) = ye(l); zs(k) = rlin(l)
         end if
      end do
      ns = k

   end subroutine copynetlinkstosam

end module m_copynetlinkstosam
