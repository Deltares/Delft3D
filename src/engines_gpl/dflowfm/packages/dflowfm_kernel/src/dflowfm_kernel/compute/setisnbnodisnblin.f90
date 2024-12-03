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
module m_setisnbnodisnblin

implicit none

private

public :: setisnbnodisnblin

contains

   subroutine setisnbnodisnblin()
      use m_flow
      use m_flowgeom
      use m_netw, only: kc

      integer :: L, LL, LLL, LLLa, L1, L2, L1a, L2a, n, nx, ja1D

      if (allocated(isnbnod)) deallocate (isnbnod)
      if (allocated(isnblin)) deallocate (isnblin)
      allocate (isnbnod(2, lnx), isnblin(2, lnx))

      if (kmx == 0 .and. lnx1D > 0) then ! setuc
         kc = 0
         isnbnod = 0
         isnblin = 0
         do n = ndx2D + 1, ndx
            nx = nd(n)%lnx
            if (nx == 2) then
               ja1D = 1
               do LL = 1, nx
                  LLL = nd(n)%ln(LL)
                  LLLa = abs(LLL)
                  if (abs(kcu(LLLa)) /= 1) then
                     ja1D = 0
                     exit
                  end if
               end do
               kc(n) = ja1D
               if (ja1D == 1) then
                  L1 = nd(n)%ln(1) ! uc1D on a node follows sign of u1 of its first link
                  L1a = abs(L1)
                  L2 = nd(n)%ln(2) ! this is the second link
                  L2a = abs(L2)

                  if (L1 > 0) then ! first link is incoming for node n
                     isnbnod(2, L1a) = 1 ! node is on side 2 of first link
                     if (L2 < 0) then ! second link is outgoing
                        isnbnod(1, L2a) = 1 ! so follows sign of node on left side
                     else
                        isnbnod(2, L2a) = -1 ! so follows sign of node on right side
                     end if
                  else ! first link is outgoing for node n
                     isnbnod(1, L1a) = 1 ! node has sign of first link
                     if (L2 < 0) then ! second link is outgoing
                        isnbnod(1, L2a) = -1 ! so follows sign of node on left side
                     else
                        isnbnod(2, L2a) = 1 ! so follows sign of node on left side
                     end if
                  end if

               end if
            end if
         end do

      end if

      do L = 1, lnx
         if (isnbnod(1, L) /= 0) then
            n = ln(1, L)
            if (nd(n)%ln(1) * nd(n)%ln(2) < 0) then
               isnblin(1, L) = 1
            else
               isnblin(1, L) = -1
            end if
         end if
         if (isnbnod(2, L) /= 0) then
            n = ln(2, L)
            if (nd(n)%ln(1) * nd(n)%ln(2) < 0) then
               isnblin(2, L) = 1
            else
               isnblin(2, L) = -1
            end if
         end if

      end do

      deallocate (isnbnod) ! no time now to make efficient version

   end subroutine setisnbnodisnblin

end module m_setisnbnodisnblin
