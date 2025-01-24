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

module m_setwindstress

   implicit none

   private

   public :: setwindstress

contains

   subroutine setwindstress()
      use precision, only: dp
      use m_setcdwcoefficient, only: setcdwcoefficient
      use m_flowgeom
      use m_flow
      use m_wind
      use m_fm_icecover, only: fm_ice_drag_effect, ice_modify_winddrag, ICE_WINDDRAG_NONE, ice_af
      implicit none
      real(kind=dp) :: uwi, cdw, tuwi, roro, wxL, wyL, uL, vL, uxL, uyL, ust, ust2, tau, z0w, roa, row
      real(kind=dp) :: local_ice_af
      integer :: L, numwav, k ! windstuff

      windxav = 0d0
      windyav = 0d0

      ! wdsu_x = 0.155d0 ; wdsu_y = 0d0 ! testcase wx=10

      if (jawindstressgiven > 0) then

         if (jastresstowind == 0) then ! stress directly
            if (jamapwind > 0) then
               wx = 0d0
               wy = 0d0
            end if
            do L = 1, lnx
               if (jaroro > 0) then
                  k = ln(2, L)
                  wdsu(L) = (wdsu_x(L) * csu(L) + wdsu_y(L) * snu(L)) / rho(ktop(k))
               else
                  wdsu(L) = (wdsu_x(L) * csu(L) + wdsu_y(L) * snu(L)) / rhomean
               end if
            end do
         else ! first reconstruct wx, wy

            do L = 1, lnx
               tau = sqrt(wdsu_x(L) * wdsu_x(L) + wdsu_y(L) * wdsu_y(L))
               if (tau > 0) then
                  ust2 = tau / rhoair
                  ust = sqrt(ust2)
                  z0w = cdb(2) * viskinair / ust + cdb(1) * ust2 / ag
                  uwi = log(10d0 / (z0w)) * ust / vonkarw
                  wx(L) = uwi * wdsu_x(L) / tau
                  wy(L) = uwi * wdsu_y(L) / tau
               else
                  wx(L) = 0d0
                  wy(L) = 0d0
               end if
            end do

         end if

      end if

      if (jawindstressgiven == 0 .or. jastresstowind == 1) then
         roa = rhoair
         row = rhomean
         wdsu = 0d0
         numwav = 0
         do L = 1, lnx
            if (wx(L) /= 0d0 .or. wy(L) /= 0d0 .or. relativewind > 0) then ! only if some wind

               wxL = wx(L)
               wyL = wy(L)
               if (relativewind > 0d0) then
                  uL = relativewind * U1(Ltop(L))
                  vL = relativewind * v(Ltop(L))
                  uxL = uL * csu(L) - vL * snu(L)
                  uyL = uL * snu(L) + vL * csu(L)
                  wxL = wxL - uxL
                  wyL = wyL - uyL
               end if
               uwi = sqrt(wxL * wxL + wyL * wyL)
               if (jaspacevarcharn == 1) then
                  cdb(1) = wcharnock(L)
               end if
               call setcdwcoefficient(uwi, cdw, L)
               if (ice_modify_winddrag /= ICE_WINDDRAG_NONE) then
                  local_ice_af = 0.5d0 * (ice_af(ln(1, L)) + ice_af(ln(2, L)))
                  cdw = fm_ice_drag_effect(local_ice_af, cdw)
               end if
               if (jatem == 5) then
                  cdwcof(L) = cdw
               end if
               if (jaroro > 0) then
                  k = ln(2, L)
                  row = rho(ktop(k))
                  if (jaroro > 1) then
                     roa = roair(k)
                  end if
               end if
               if (ja_airdensity + ja_computed_airdensity > 0) then
                  k = ln(2, L)
                  roa = airdensity(k)
               end if
               tuwi = roa * cdw * uwi
               if (jamapwindstress > 0) then
                  wdsu_x(L) = tuwi * wxL
                  wdsu_y(L) = tuwi * wyL
               end if
               if (kmx > 0) then
                  roro = roa / row
                  ustw(L) = sqrt(roro * cdw) * uwi
               end if
               wdsu(L) = tuwi * (wxL * csu(L) + wyL * snu(L)) / row
               windxav = windxav + wxL
               windyav = windyav + wyL
               numwav = numwav + 1

            end if
         end do
         if (numwav > 0) then
            windxav = windxav / numwav
            windyav = windyav / numwav
         end if
      end if
   end subroutine setwindstress

end module m_setwindstress
