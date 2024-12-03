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

module m_xbeachwaves_getcellcentergradients

implicit none

private

public :: getcellcentergradients

contains

subroutine getcellcentergradients(hh, dhsdx, dhsdy)
   use m_flow
   use m_flowgeom

   implicit none

   double precision, intent(in), dimension(ndx) :: hh
   double precision, intent(out), dimension(ndx) :: dhsdx, dhsdy

   integer :: L, k1, k2, k, kb, ki
   double precision :: hs1, hs2

   ! Tegeltjesdiepte approach is eenvoudiger en onnauwkeuriger, maar werkt altijd, ook met morfologie
   dhsdx = 0d0
   dhsdy = 0d0
   do L = 1, Lnx
      if (hu(L) > epshu) then ! link flows
         k1 = ln(1, L)
         k2 = ln(2, L)
         hs1 = hh(k1)
         hs2 = hh(k2)

         dhsdx(k1) = dhsdx(k1) + wcx1(L) * (hs2 - hs1) * dxi(L) ! dimension m/m
         dhsdy(k1) = dhsdy(k1) + wcy1(L) * (hs2 - hs1) * dxi(L)
         dhsdx(k2) = dhsdx(k2) + wcx2(L) * (hs2 - hs1) * dxi(L)
         dhsdy(k2) = dhsdy(k2) + wcy2(L) * (hs2 - hs1) * dxi(L)
      end if
   end do

   do k = 1, nbndu
      kb = kbndu(1, k)
      ki = kbndu(2, k)
      dhsdx(kb) = dhsdx(ki)
      dhsdy(kb) = dhsdy(ki)
   end do

   do k = 1, nbndz
      kb = kbndz(1, k)
      ki = kbndz(2, k)
      dhsdx(kb) = dhsdx(ki)
      dhsdy(kb) = dhsdy(ki)
   end do

end subroutine getcellcentergradients

endmodule  m_xbeachwaves_getcellcentergradients
