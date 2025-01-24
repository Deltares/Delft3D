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

module m_setucxucy_mor

   implicit none

   private

   public :: setucxucy_mor

contains

   ! =================================================================================================
   ! =================================================================================================
   subroutine setucxucy_mor(u1_loc)
      use precision, only: dp
      use m_flowgeom
      use m_flow
      use m_fm_erosed, only: ucxq_mor, ucyq_mor
      use m_sobekdfm
      use m_sediment, only: jased, stm_included
      use m_missing
      use m_flowparameters, only: jabarrieradvection, flow_solver
      use m_sferic
      use m_get_Lbot_Ltop
      use m_lin2nodx, only: lin2nodx
      use m_lin2nody, only: lin2nody
      use m_nod2linx, only: nod2linx
      use m_nod2liny, only: nod2liny
      implicit none
      real(kind=dp), dimension(lnkx), intent(in) :: u1_loc

      integer :: L, KK, k1, k2, k, Lb, Lt, LL, nn, n, kt, kb, kbk, k2k
      integer :: itpbn
      real(kind=dp) :: uu, vv, uucx, uucy, wcxu, wcyu, cs, sn, hul, dzz, uin
      real(kind=dp) :: dischcorrection
      real(kind=dp) :: uinx, uiny

      ucxq_mor = 0d0; ucyq_mor = 0d0 ! zero arrays
      ucx_mor = 0d0; ucy_mor = 0d0

      if (kmx < 1) then ! original 2D coding
         do L = 1, lnx1D
            if (u1_loc(L) /= 0d0 .and. kcu(L) /= 3) then ! link flows ; in 2D, the loop is split to save kcu check in 2D
               k1 = ln(1, L); k2 = ln(2, L)
               wcxu = wcx1(L) * u1_loc(L)
               ucx_mor(k1) = ucx_mor(k1) + wcxu
               ucxq_mor(k1) = ucxq_mor(k1) + wcxu * hu(L)
               wcyu = wcy1(L) * u1_loc(L)
               ucy_mor(k1) = ucy_mor(k1) + wcyu
               ucyq_mor(k1) = ucyq_mor(k1) + wcyu * hu(L)
               wcxu = wcx2(L) * u1_loc(L)
               ucx_mor(k2) = ucx_mor(k2) + wcxu
               ucxq_mor(k2) = ucxq_mor(k2) + wcxu * hu(L)
               wcyu = wcy2(L) * u1_loc(L)
               ucy_mor(k2) = ucy_mor(k2) + wcyu
               ucyq_mor(k2) = ucyq_mor(k2) + wcyu * hu(L)
            end if
         end do
         do L = lnx1D + 1, lnx
            if (jabarrieradvection == 3) then
               if (struclink(L) == 1) cycle
            end if
            if (u1_loc(L) /= 0d0) then ! link flows
               k1 = ln(1, L); k2 = ln(2, L)
               wcxu = wcx1(L) * u1_loc(L)
               ucx_mor(k1) = ucx_mor(k1) + wcxu
               ucxq_mor(k1) = ucxq_mor(k1) + wcxu * hu(L)
               wcyu = wcy1(L) * u1_loc(L)
               ucy_mor(k1) = ucy_mor(k1) + wcyu
               ucyq_mor(k1) = ucyq_mor(k1) + wcyu * hu(L)
               wcxu = wcx2(L) * u1_loc(L)
               ucx_mor(k2) = ucx_mor(k2) + wcxu
               ucxq_mor(k2) = ucxq_mor(k2) + wcxu * hu(L)
               wcyu = wcy2(L) * u1_loc(L)
               ucy_mor(k2) = ucy_mor(k2) + wcyu
               ucyq_mor(k2) = ucyq_mor(k2) + wcyu * hu(L)
            end if
         end do
      else
         do LL = 1, lnx
            Lb = Lbot(LL); Lt = Lb - 1 + kmxL(LL)
            do L = Lb, Lt
               if (u1_loc(L) /= 0d0) then ! link flows
                  k1 = ln0(1, L) ! use ln0 in reconstruction and in computing ucxu, use ln when fluxing
                  k2 = ln0(2, L)
                  huL = hu(L)
                  if (L > Lbot(LL)) then
                     huL = huL - hu(L - 1)
                  end if
                  ucx_mor(k1) = ucx_mor(k1) + wcx1(LL) * u1_loc(L)
                  ucxq_mor(k1) = ucxq_mor(k1) + wcx1(LL) * u1_loc(L) * huL
                  ucy_mor(k1) = ucy_mor(k1) + wcy1(LL) * u1_loc(L)
                  ucyq_mor(k1) = ucyq_mor(k1) + wcy1(LL) * u1_loc(L) * huL
                  ucx_mor(k2) = ucx_mor(k2) + wcx2(LL) * u1_loc(L)
                  ucxq_mor(k2) = ucxq_mor(k2) + wcx2(LL) * u1_loc(L) * huL
                  ucy_mor(k2) = ucy_mor(k2) + wcy2(LL) * u1_loc(L)
                  ucyq_mor(k2) = ucyq_mor(k2) + wcy2(LL) * u1_loc(L) * huL
               end if
            end do
            if (jazlayercenterbedvel == 1) then ! copy bed velocity down
               do k1 = kbot(ln0(1, LL)), ln0(1, Lb) - 1
                  ucx_mor(k1) = ucx_mor(k1) + wcx1(LL) * u1_loc(Lb)
                  ucy_mor(k1) = ucy_mor(k1) + wcy1(LL) * u1_loc(Lb)
               end do
               do k2 = kbot(ln0(2, LL)), ln0(2, Lb) - 1
                  ucx_mor(k2) = ucx_mor(k2) + wcx2(LL) * u1_loc(Lb)
                  ucy_mor(k2) = ucy_mor(k2) + wcy2(LL) * u1_loc(Lb)
               end do
            end if
         end do
      end if

      if (kmx < 1) then ! original 2D coding
         !$OMP PARALLEL DO           &
         !$OMP PRIVATE(k)
         do k = 1, ndxi
            if (hs(k) > 0d0) then
               ucxq_mor(k) = ucxq_mor(k) / hs(k)
               ucyq_mor(k) = ucyq_mor(k) / hs(k)
               if (iperot == 2) then
                  ucx_mor(k) = ucxq_mor(k)
                  ucy_mor(k) = ucyq_mor(k)
               end if
            end if
         end do
         !$OMP END PARALLEL DO
      else
         do nn = 1, ndxi
            if (hs(nn) > 0d0) then
               kb = kbot(nn)
               kt = ktop(nn)
               ucxq_mor(nn) = sum(ucxq_mor(kb:kt)) / hs(nn) ! Depth-averaged cell center velocity in 3D, based on ucxq
               ucyq_mor(nn) = sum(ucyq_mor(kb:kt)) / hs(nn)
               do k = kb, kt
                  dzz = zws(k) - zws(k - 1)
                  if (dzz > 0d0) then
                     ucxq_mor(k) = ucxq_mor(k) / dzz
                     ucyq_mor(k) = ucyq_mor(k) / dzz
                  end if
                  if (iperot == 2) then
                     ucx_mor(k) = ucxq_mor(k)
                     ucy_mor(k) = ucyq_mor(k)
                  end if
               end do
            end if
         end do
      end if

      do n = 1, nbndz ! waterlevel boundaries
         kb = kbndz(1, n)
         k2 = kbndz(2, n)
         LL = kbndz(3, n)
         itpbn = kbndz(4, n)
         cs = csu(LL); sn = snu(LL)
         if (kmx == 0) then
            if (hs(kb) > epshs) then
               if (jacstbnd == 0 .and. itpbn /= 2) then ! Neumann: always
                  if (jasfer3D == 1) then
                     uin = nod2linx(LL, 2, ucx_mor(k2), ucy_mor(k2)) * cs + nod2liny(LL, 2, ucx_mor(k2), ucy_mor(k2)) * sn
                     ucx_mor(kb) = uin * lin2nodx(LL, 1, cs, sn)
                     ucy_mor(kb) = uin * lin2nody(LL, 1, cs, sn)
                     uin = nod2linx(LL, 2, ucxq_mor(k2), ucyq_mor(k2)) * cs + nod2liny(LL, 2, ucxq_mor(k2), ucyq_mor(k2)) * sn
                     ucxq_mor(kb) = uin * lin2nodx(LL, 1, cs, sn)
                     ucyq_mor(kb) = uin * lin2nody(LL, 1, cs, sn)
                  else
                     uin = ucx_mor(k2) * cs + ucy_mor(k2) * sn
                     ucx_mor(kb) = uin * cs
                     ucy_mor(kb) = uin * sn
                     uin = ucxq_mor(k2) * cs + ucyq_mor(k2) * sn
                     ucxq_mor(kb) = uin * cs
                     ucyq_mor(kb) = uin * sn
                  end if
               else
                  if (jasfer3D == 1) then
                     uinx = nod2linx(LL, 2, ucx_mor(k2), ucy_mor(k2))
                     uiny = nod2liny(LL, 2, ucx_mor(k2), ucy_mor(k2))
                     ucx_mor(kb) = lin2nodx(LL, 1, uinx, uiny)
                     ucy_mor(kb) = lin2nody(LL, 1, uinx, uiny)
                     uinx = nod2linx(LL, 2, ucxq_mor(k2), ucyq_mor(k2))
                     uiny = nod2liny(LL, 2, ucxq_mor(k2), ucyq_mor(k2))
                     ucxq_mor(kb) = lin2nodx(LL, 1, uinx, uiny)
                     ucyq_mor(kb) = lin2nody(LL, 1, uinx, uiny)
                  else
                     ucx_mor(kb) = ucx_mor(k2)
                     ucy_mor(kb) = ucy_mor(k2)
                     ucxq_mor(kb) = ucxq_mor(k2)
                     ucyq_mor(kb) = ucyq_mor(k2)
                  end if
               end if
               if (jased > 0 .and. stm_included) then
                  dischcorrection = hs(k2) / hs(kb)
                  !ucx_mor(kb)  = ucx_mor(kb)  * dischcorrection
                  !ucy_mor(kb)  = ucy_mor(kb)  * dischcorrection
                  ucxq_mor(kb) = ucxq_mor(kb) * dischcorrection
                  ucyq_mor(kb) = ucyq_mor(kb) * dischcorrection
               end if
            end if
         else
            call getLbotLtop(LL, Lb, Lt)
            do L = Lb, Lt
               kbk = ln(1, L); k2k = ln(2, L)
               if (jacstbnd == 0 .and. itpbn /= 2) then
                  if (jasfer3D == 1) then
                     uin = nod2linx(LL, 2, ucx_mor(k2k), ucy_mor(k2k)) * cs + nod2liny(LL, 2, ucx_mor(k2k), ucy_mor(k2k)) * sn
                     ucx_mor(kbk) = uin * lin2nodx(LL, 1, cs, sn)
                     ucy_mor(kbk) = uin * lin2nody(LL, 1, cs, sn)
                     !uin = nod2linx(LL,2,ucxq_mor(k2k),ucyq_mor(k2k))*cs + nod2liny(LL,2,ucxq_mor(k2k),ucyq_mor(k2k))*sn
                     ucxq_mor(kbk) = uin * lin2nodx(LL, 1, cs, sn)
                     ucyq_mor(kbk) = uin * lin2nody(LL, 1, cs, sn)
                  else
                     uin = ucx_mor(k2k) * cs + ucy_mor(k2k) * sn
                     ucx_mor(kbk) = uin * cs
                     ucy_mor(kbk) = uin * sn
                     ucxq_mor(kbk) = uin * cs; ucyq_mor(kbk) = uin * sn
                  end if
               else
                  if (jasfer3D == 1) then
                     uinx = nod2linx(LL, 2, ucx_mor(k2k), ucy_mor(k2k))
                     uiny = nod2liny(LL, 2, ucx_mor(k2k), ucy_mor(k2k))
                     ucx_mor(kbk) = lin2nodx(LL, 1, uinx, uiny)
                     ucy_mor(kbk) = lin2nody(LL, 1, uinx, uiny)
                     uinx = nod2linx(LL, 2, ucxq_mor(k2k), ucyq_mor(k2k))
                     uiny = nod2liny(LL, 2, ucxq_mor(k2k), ucyq_mor(k2k))
                     ucxq_mor(kbk) = lin2nodx(LL, 1, uinx, uiny)
                     ucyq_mor(kbk) = lin2nody(LL, 1, uinx, uiny)
                  else
                     ucx_mor(kbk) = ucx_mor(k2k)
                     ucy_mor(kbk) = ucy_mor(k2k)
                     ucxq_mor(kbk) = ucxq_mor(k2k)
                     ucyq_mor(kbk) = ucyq_mor(k2k)
                  end if
               end if
               !if (jased > 0 .and. stm_included) then   ! similar as 2D, JRE to check
               !   dischcorrection = hs(k2) / hs(kb)
               !   !ucx_mor(kb)  = ucx_mor(kb)  * dischcorrection
               !   !ucy_mor(kb)  = ucy_mor(kb)  * dischcorrection
               !   ucxq_mor(kbk) = ucxq_mor(kbk) * dischcorrection
               !   ucyq_mor(kbk) = ucyq_mor(kbk) * dischcorrection
               !endif
            end do
         end if
      end do

      if (jaZerozbndinflowadvection == 1) then
         do n = 1, nbndz ! on waterlevel boundaries put inflow advection velocity to 0
            LL = kbndz(3, n)
            do L = Lbot(LL), Ltop(LL)
               k1 = ln(1, L)
               if (u1_loc(LL) > 0) then
                  ucx_mor(k1) = 0d0; ucy_mor(k1) = 0d0
               end if
            end do
         end do
      else if (jaZerozbndinflowadvection == 2) then
         do n = 1, nbndz ! on waterlevel boundaries put all advection velocity to 0
            LL = kbndz(3, n)
            do L = Lbot(LL), Ltop(LL)
               k1 = ln(1, L)
               ucx_mor(k1) = 0d0; ucy_mor(k1) = 0d0
            end do
         end do
      end if

      do n = 1, nbndu ! velocity boundaries
         kb = kbndu(1, n)
         k2 = kbndu(2, n)
         LL = kbndu(3, n)
         cs = csu(LL); sn = snu(LL)
         if (kmx == 0) then
            if (hs(kb) > epshs) then
               if (jacstbnd == 0) then
                  if (jasfer3D == 1) then
                     uin = nod2linx(LL, 2, ucx_mor(k2), ucy_mor(k2)) * cs + nod2liny(LL, 2, ucx_mor(k2), ucy_mor(k2)) * sn
                     ucx_mor(kb) = uin * lin2nodx(LL, 1, cs, sn)
                     ucy_mor(kb) = uin * lin2nody(LL, 1, cs, sn)
                     uin = nod2linx(LL, 2, ucxq_mor(k2), ucyq_mor(k2)) * cs + nod2liny(LL, 2, ucxq_mor(k2), ucyq_mor(k2)) * sn
                     ucxq_mor(kb) = uin * lin2nodx(LL, 1, cs, sn)
                     ucyq_mor(kb) = uin * lin2nody(LL, 1, cs, sn)
                  else
                     uin = ucx_mor(k2) * cs + ucy_mor(k2) * sn
                     ucx_mor(kb) = uin * cs
                     ucy_mor(kb) = uin * sn
                     uin = ucxq_mor(k2) * cs + ucyq_mor(k2) * sn
                     ucxq_mor(kb) = uin * cs
                     ucyq_mor(kb) = uin * sn
                  end if
               else
                  if (jasfer3D == 1) then
                     uinx = nod2linx(LL, 2, ucx_mor(k2), ucy_mor(k2))
                     uiny = nod2liny(LL, 2, ucx_mor(k2), ucy_mor(k2))
                     ucx_mor(kb) = lin2nodx(LL, 1, uinx, uiny)
                     ucy_mor(kb) = lin2nody(LL, 1, uinx, uiny)
                     uinx = nod2linx(LL, 2, ucxq_mor(k2), ucyq_mor(k2))
                     uiny = nod2liny(LL, 2, ucxq_mor(k2), ucyq_mor(k2))
                     ucxq_mor(kb) = lin2nodx(LL, 1, uinx, uiny)
                     ucyq_mor(kb) = lin2nody(LL, 1, uinx, uiny)
                  else
                     ucx_mor(kb) = ucx_mor(k2)
                     ucy_mor(kb) = ucy_mor(k2)
                     ucxq_mor(kb) = ucxq_mor(k2)
                     ucyq_mor(kb) = ucyq_mor(k2)
                  end if
               end if
               if (jased > 0 .and. stm_included) then
                  dischcorrection = hs(k2) / hs(kb)
                  !ucx_mor(kb)  = ucx_mor(kb)  * dischcorrection
                  !ucy_mor(kb)  = ucy_mor(kb)  * dischcorrection
                  ucxq_mor(kb) = ucxq_mor(kb) * dischcorrection
                  ucyq_mor(kb) = ucyq_mor(kb) * dischcorrection
               end if
            end if
         else
            do k = 1, kmxL(LL)
               kbk = kbot(kb) - 1 + min(k, kmxn(kb))
               k2k = kbot(k2) - 1 + min(k, kmxn(k2))
               if (jacstbnd == 0) then
                  if (jasfer3D == 1) then
                     uin = nod2linx(LL, 2, ucx_mor(k2k), ucy_mor(k2k)) * cs + nod2liny(LL, 2, ucx_mor(k2k), ucy_mor(k2k)) * sn
                     ucx_mor(kbk) = uin * lin2nodx(LL, 1, cs, sn)
                     ucy_mor(kbk) = uin * lin2nody(LL, 1, cs, sn)
                     ucxq_mor(kbk) = uin * lin2nodx(LL, 1, cs, sn)
                     ucyq_mor(kbk) = uin * lin2nody(LL, 1, cs, sn)
                  else
                     uin = ucx_mor(k2k) * cs + ucy_mor(k2k) * sn
                     ucx_mor(kbk) = uin * cs
                     ucy_mor(kbk) = uin * sn
                     ucxq_mor(kbk) = uin * cs
                     ucyq_mor(kbk) = uin * sn
                  end if
               else
                  if (jasfer3D == 1) then
                     uinx = nod2linx(LL, 2, ucx_mor(k2k), ucy_mor(k2k))
                     uiny = nod2liny(LL, 2, ucx_mor(k2k), ucy_mor(k2k))
                     ucx_mor(kbk) = lin2nodx(LL, 1, uinx, uiny)
                     ucy_mor(kbk) = lin2nody(LL, 1, uinx, uiny)
                     uinx = nod2linx(LL, 2, ucxq_mor(k2k), ucyq_mor(k2k))
                     uiny = nod2liny(LL, 2, ucxq_mor(k2k), ucyq_mor(k2k))
                     ucxq_mor(kbk) = lin2nodx(LL, 1, uinx, uiny)
                     ucyq_mor(kbk) = lin2nody(LL, 1, uinx, uiny)
                  else
                     ucx_mor(kbk) = ucx_mor(k2k)
                     ucy_mor(kbk) = ucy_mor(k2k)
                     ucxq_mor(kbk) = ucxq_mor(k2k)
                     ucyq_mor(kbk) = ucyq_mor(k2k)
                  end if
               end if
            end do
         end if
      end do

      do n = 1, nbndt ! tangential velocity boundaries, override other types
         kb = kbndt(1, n)
         k2 = kbndt(2, n)
         LL = kbndt(3, n)
         cs = csu(LL); sn = snu(LL)
         call getLbotLtop(LL, Lb, Lt)
         do L = Lb, Lt
            kbk = ln(1, L)
            kk = kmxd * (n - 1) + L - Lb + 1
            uu = u0(L); vv = zbndt(kk) ! v(L)
            uucx = uu * cs - vv * sn
            uucy = uu * sn + vv * cs
            if (jasfer3D == 1) then
               ucx_mor(kbk) = lin2nodx(LL, 1, uucx, uucy)
               ucy_mor(kbk) = lin2nody(LL, 1, uucx, uucy)
               ucxq_mor(kbk) = ucx_mor(kbk)
               ucyq_mor(kbk) = ucy_mor(kbk)
            else
               ucx_mor(kbk) = uucx
               ucy_mor(kbk) = uucy
               ucxq_mor(kbk) = ucx_mor(kbk)
               ucyq_mor(kbk) = ucy_mor(kbk)
            end if
         end do
      end do

      if (zbnduxyval /= dmiss) then
         zbnduxy(1) = zbnduxyval
      end if

      do n = 1, nbnduxy ! do3d                     ! uxuy velocity boundaries, override other types
         kb = kbnduxy(1, n)
         LL = kbnduxy(3, n)
         call getLbotLtop(LL, Lb, Lt)
         do L = Lb, Lt
            kbk = ln(1, L)
            kk = kmxd * (n - 1) + L - Lb + 1
            if (jasfer3D == 1) then
               ucx_mor(kbk) = lin2nodx(LL, 1, zbnduxy(2 * kk - 1), zbnduxy(2 * kk))
               ucy_mor(kbk) = lin2nody(LL, 1, zbnduxy(2 * kk - 1), zbnduxy(2 * kk))
               ucxq_mor(kbk) = ucx_mor(kbk)
               ucyq_mor(kbk) = ucy_mor(kbk)
            else
               ucx_mor(kbk) = zbnduxy(2 * kk - 1)
               ucy_mor(kbk) = zbnduxy(2 * kk)
               ucxq_mor(kbk) = ucx_mor(kbk)
               ucyq_mor(kbk) = ucy_mor(kbk)
            end if
         end do
      end do

      do n = 1, nbndn ! normal velocity boundaries, override other types
         kb = kbndn(1, n)
         k2 = kbndn(2, n)
         LL = kbndn(3, n)
         cs = csu(LL); sn = snu(LL)
         call getLbotLtop(LL, Lb, Lt)
         do L = Lb, Lt
            kbk = ln(1, L)
            kk = kmxd * (n - 1) + L - Lb + 1
            uu = zbndn(kk); vv = 0d0
            uucx = uu * cs - vv * sn !
            uucy = uu * sn + vv * cs
            if (jasfer3D == 1) then
               ucx_mor(kbk) = lin2nodx(LL, 1, uucx, uucy)
               ucy_mor(kbk) = lin2nody(LL, 1, uucx, uucy)
               ucxq_mor(kbk) = ucx_mor(kbk)
               ucyq_mor(kbk) = ucy_mor(kbk)
            else
               ucx_mor(kbk) = uucx
               ucy_mor(kbk) = uucy
               ucxq_mor(kbk) = ucx_mor(kbk)
               ucyq_mor(kbk) = ucy_mor(kbk)
            end if
         end do
      end do

      do n = 1, nbnd1d2d
         kb = kbnd1d2d(1, n)
         k2 = kbnd1d2d(2, n)
         LL = kbnd1d2d(3, n)
         if (kmx == 0) then ! 2D
            if (jasfer3D == 1) then
               uinx = nod2linx(LL, 2, ucx_mor(k2), ucy_mor(k2))
               uiny = nod2liny(LL, 2, ucx_mor(k2), ucy_mor(k2))
               ucx_mor(kb) = lin2nodx(LL, 1, uinx, uiny)
               ucy_mor(kb) = lin2nody(LL, 1, uinx, uiny)
               ucxq_mor(kb) = ucx_mor(kb)
               ucyq_mor(kb) = ucy_mor(kb)
            else
               ucx_mor(kb) = ucx_mor(k2)
               ucy_mor(kb) = ucy_mor(k2)
               ucxq_mor(kb) = ucx_mor(kb)
               ucyq_mor(kb) = ucy_mor(kb)
            end if
         else ! 3D
         end if
      end do

   end subroutine setucxucy_mor

end module m_setucxucy_mor
