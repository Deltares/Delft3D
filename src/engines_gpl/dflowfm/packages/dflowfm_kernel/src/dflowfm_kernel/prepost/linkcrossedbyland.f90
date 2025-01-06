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

!> check if a link is close to a land boundary segment
module m_linkcrossedbyland

implicit none

private

public :: linkcrossedbyland

contains

subroutine linkcrossedbyland(L, jstart, jend, jland, jacross)
   use precision, only: dp
   use m_netw
   use m_landboundary
   use m_missing, only: dmiss
   use geometry_module, only: dbdistance
   use m_sferic, only: jsferic, jasfer3D
   use m_d_line_dis3

   integer, intent(in) :: L !< link number
   integer, intent(in) :: jstart, jend !< start and end point of land boundary segment respectively
   integer, intent(inout) :: jland !< point in land boundary that is (in:) visited first (out:) found

   integer, intent(out) :: jacross !< crossed (1) or not (0)

   integer :: k1, k2 ! nodes comprising the link

   integer :: iter, j, j_
   integer :: ja, jastop

   real(kind=dp) :: x1, y1, x2, y2 !  node coordinates
   real(kind=dp) :: x3, y3, x4, y4 ! land boundary point coordinates

   real(kind=dp) :: sm, DL, Dm, Dtol, dismin

   real(kind=dp) :: dis, xn, yn, rL1, rL2 ! for dlinedis3

   jacross = 0

   j = max(min(jland, jend - 1), jstart)

   k1 = kn(1, L)
   k2 = kn(2, L)

   if (k1 < 0 .or. k2 < 0) then ! safety
      return
   end if

   x1 = xk(k1)
   y1 = yk(k1)

   x2 = xk(k2)
   y2 = yk(k2)

   DL = dbdistance(x1, y1, x2, y2, jsferic, jasfer3D, dmiss)
   Dtol = DCLOSE * DL
   dismin = 1d99

!  loop over the segments of the land boundary
   jacross = 0
   jastop = 0
   j_ = 0
   do
      x3 = xlan(j)
      y3 = ylan(j)
      x4 = xlan(j + 1)
      y4 = ylan(j + 1)
      Dm = dbdistance(x3, y3, x4, y4, jsferic, jasfer3D, dmiss)
      if (x3 /= dmiss .and. x4 /= dmiss .and. Dm > 0d0) then
         rL1 = 0d0
         rL2 = 1d0
         call dlinedis3(x1, y1, x3, y3, x4, y4, ja, dis, xn, yn, rL1)
         if (dis <= dtol) then
            jacross = 1
            jland = j
            if (rL1 >= 0d0 .and. rL1 <= 1d0) jastop = 1
         else
            call dlinedis3(x2, y2, x3, y3, x4, y4, ja, dis, xn, yn, rL2)
            if (dis <= dtol) then
               jacross = 1
               jland = j
               if (rL2 >= 0d0 .and. rL2 <= 1d0) jastop = 1
            end if
         end if

         dismin = min(dis, dismin)

         if (jastop == 1) exit
      end if

!     move pointer left-right-left-right-left-right etc.
      iter = 0
      do while ((iter == 0 .or. j < jstart .or. j > jend - 1) .and. iter < 3)
         iter = iter + 1
         if (j_ < 0) then ! to right
            j_ = -j_ + 1
         else ! to left
            j_ = -j_ - 1
         end if
         j = j + j_
      end do
      if (iter == 3) exit
   end do

!   if ( jacross.eq.1 .and. max(rL1,rL2).gt.0d0 .and. min(rL1,rL2).le.1d0 ) then
   if (jacross == 1) then
      j = jland
!     set outer land boundary segment points
!     minimum
      sm = min(rL1, rL2)
      if (j < jleft) then
         jleft = j
         rLleft = min(max(sm, 0d0), 1d0)
      else if (j == jleft) then
         rLleft = min(max(sm, 0d0), rLleft)
      end if
!     maximum
      sm = max(rL1, rL2)
      if (j > jright) then
         jright = j
         rLright = min(max(sm, 0d0), 1d0)
      else if (j == jright) then
         rLright = max(min(sm, 1d0), rLright)
      end if

   end if

   return
end subroutine linkcrossedbyland

end module m_linkcrossedbyland
