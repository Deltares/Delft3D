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

 subroutine setucxucyucxuucyu()
    use m_flowgeom
    use m_flow
    use m_sobekdfm
    use m_sediment, only: jased, stm_included
    use m_missing
    use m_flowparameters, only: jabarrieradvection
    use m_sferic
    implicit none

    logical :: make2dh
    integer :: L, KK, k1, k2, k, Lb, Lt, LL, nn, n, kt, kb, kbk, k2k
    integer :: itpbn, newucxq = 0
    double precision :: uu, vv, uucx, uucy, wcxu, wcyu, cs, sn, hul, dzz, uin, duxdn, duydn
    double precision :: dischcorrection
    double precision :: uinx, uiny, u1L

    double precision, external :: nod2linx, nod2liny
    double precision, external :: lin2nodx, lin2nody

    if (iperot /= -1) then
       ucxq = 0d0; ucyq = 0d0 ! zero arrays
       ucx = 0d0; ucy = 0d0

       make2dh = (kmx < 1) .or. (kmx > 0 .and. (jasedtrails > 0 .or. jamapucmag > 0 .or. jamapucvec > 0))

       if (make2dh) then ! original 2D coding

          do L = 1, lnx1D
             if (u1(L) /= 0d0) then ! link flows ; in 2D, the loop is split to save kcu check in 2D
                if ((kcu(L) == 3 .or. kcu(L) == 4 .or. (iadv(L) >= 21 .and. iadv(L) <= 29)) .and. ChangeVelocityAtStructures) then
                   ! Apply only on some barrier-like hydraulic structures, and typically on 1D2D links for dambreaks
                   u1L = q1(L) / au_nostrucs(L)
                else
                   u1L = u1(L)
                end if
                k1 = ln(1, L); k2 = ln(2, L)
                wcxu = wcx1(L) * u1L
                ucx(k1) = ucx(k1) + wcxu
                ucxq(k1) = ucxq(k1) + wcxu * hu(L)
                wcyu = wcy1(L) * u1L
                ucy(k1) = ucy(k1) + wcyu
                ucyq(k1) = ucyq(k1) + wcyu * hu(L)
                wcxu = wcx2(L) * u1L
                ucx(k2) = ucx(k2) + wcxu
                ucxq(k2) = ucxq(k2) + wcxu * hu(L)
                wcyu = wcy2(L) * u1L
                ucy(k2) = ucy(k2) + wcyu
                ucyq(k2) = ucyq(k2) + wcyu * hu(L)
             end if
          end do

          do L = lnx1D + 1, lnx
             if (jabarrieradvection == 3) then
                if (struclink(L) == 1) cycle
             end if
             if (u1(L) /= 0d0) then ! link flows
                if ((kcu(L) == 3 .or. kcu(L) == 4 .or. (iadv(L) >= 21 .and. iadv(L) <= 29)) .and. ChangeVelocityAtStructures) then
                   ! Apply only on some barrier-like hydraulic structures, and typically on 1D2D links for dambreaks
                   u1L = q1(L) / au_nostrucs(L)
                else
                   u1L = u1(L)
                end if
                k1 = ln(1, L); k2 = ln(2, L)
                wcxu = wcx1(L) * u1L
                ucx(k1) = ucx(k1) + wcxu
                ucxq(k1) = ucxq(k1) + wcxu * hu(L)
                wcyu = wcy1(L) * u1L
                ucy(k1) = ucy(k1) + wcyu
                ucyq(k1) = ucyq(k1) + wcyu * hu(L)
                wcxu = wcx2(L) * u1L
                ucx(k2) = ucx(k2) + wcxu
                ucxq(k2) = ucxq(k2) + wcxu * hu(L)
                wcyu = wcy2(L) * u1L
                ucy(k2) = ucy(k2) + wcyu
                ucyq(k2) = ucyq(k2) + wcyu * hu(L)
             end if
          end do
       end if

       if (kmx > 0) then
          do LL = 1, lnx
             Lb = Lbot(LL); Lt = Lb - 1 + kmxL(LL)
             do L = Lb, Lt
                if (u1(L) /= 0d0) then ! link flows
                   k1 = ln0(1, L) ! use ln0 in reconstruction and in computing ucxu, use ln when fluxing
                   k2 = ln0(2, L)

                   huL = hu(L)
                   if (L > Lbot(LL)) then
                      huL = huL - hu(L - 1)
                   end if
                   ucx(k1) = ucx(k1) + wcx1(LL) * u1(L)
                   ucxq(k1) = ucxq(k1) + wcx1(LL) * u1(L) * huL
                   ucy(k1) = ucy(k1) + wcy1(LL) * u1(L)
                   ucyq(k1) = ucyq(k1) + wcy1(LL) * u1(L) * huL
                   ucx(k2) = ucx(k2) + wcx2(LL) * u1(L)
                   ucxq(k2) = ucxq(k2) + wcx2(LL) * u1(L) * huL
                   ucy(k2) = ucy(k2) + wcy2(LL) * u1(L)
                   ucyq(k2) = ucyq(k2) + wcy2(LL) * u1(L) * huL

                end if
             end do

             if (jazlayercenterbedvel == 1) then ! copy bed velocity down
                do k1 = kbot(ln0(1, LL)), ln0(1, Lb) - 1
                   ucx(k1) = ucx(k1) + wcx1(LL) * u1(Lb)
                   ucy(k1) = ucy(k1) + wcy1(LL) * u1(Lb)
                end do
                do k2 = kbot(ln0(2, LL)), ln0(2, Lb) - 1
                   ucx(k2) = ucx(k2) + wcx2(LL) * u1(Lb)
                   ucy(k2) = ucy(k2) + wcy2(LL) * u1(Lb)
                end do
             end if

          end do

       end if

       if (make2dh) then ! original 2D coding
          !$OMP PARALLEL DO           &
          !$OMP PRIVATE(k)
          do k = 1, ndxi
             if (hs(k) > 0d0) then
                ucxq(k) = ucxq(k) / hs(k)
                ucyq(k) = ucyq(k) / hs(k)
                if (iperot == 2) then
                   ucx(k) = ucxq(k)
                   ucy(k) = ucyq(k)
                end if
             end if
          end do
          !$OMP END PARALLEL DO
       end if

       if (kmx > 0) then
          do nn = 1, ndxi
             if (hs(nn) > 0d0) then
                kb = kbot(nn)
                kt = ktop(nn)
                ucxq(nn) = sum(ucxq(kb:kt)) / hs(nn) ! Depth-averaged cell center velocity in 3D, based on ucxq
                ucyq(nn) = sum(ucyq(kb:kt)) / hs(nn)
                do k = kb, kt
                   dzz = zws(k) - zws(k - 1)
                   if (dzz > 0d0) then
                      ucxq(k) = ucxq(k) / dzz
                      ucyq(k) = ucyq(k) / dzz
                   end if
                   if (iperot == 2) then
                      ucx(k) = ucxq(k)
                      ucy(k) = ucyq(k)
                   end if
                end do
             end if
          end do
       end if
    end if

    do n = 1, nbndz ! waterlevel boundaries
       kb = kbndz(1, n)
       k2 = kbndz(2, n)
       LL = kbndz(3, n)
       itpbn = kbndz(4, n)
       cs = csu(LL); sn = snu(LL)
       if (make2dh) then
          if (hs(kb) > epshs) then
             if (jacstbnd == 0 .and. itpbn /= 2) then ! Neumann: always
                if (jasfer3D == 1) then
                   uin = nod2linx(LL, 2, ucx(k2), ucy(k2)) * cs + nod2liny(LL, 2, ucx(k2), ucy(k2)) * sn
                   ucx(kb) = uin * lin2nodx(LL, 1, cs, sn)
                   ucy(kb) = uin * lin2nody(LL, 1, cs, sn)
                   uin = nod2linx(LL, 2, ucxq(k2), ucyq(k2)) * cs + nod2liny(LL, 2, ucxq(k2), ucyq(k2)) * sn
                   ucxq(kb) = uin * lin2nodx(LL, 1, cs, sn)
                   ucyq(kb) = uin * lin2nody(LL, 1, cs, sn)
                else
                   uin = ucx(k2) * cs + ucy(k2) * sn
                   ucx(kb) = uin * cs
                   ucy(kb) = uin * sn
                   uin = ucxq(k2) * cs + ucyq(k2) * sn
                   ucxq(kb) = uin * cs
                   ucyq(kb) = uin * sn
                end if
             else
                if (jasfer3D == 1) then
                   uinx = nod2linx(LL, 2, ucx(k2), ucy(k2))
                   uiny = nod2liny(LL, 2, ucx(k2), ucy(k2))
                   ucx(kb) = lin2nodx(LL, 1, uinx, uiny)
                   ucy(kb) = lin2nody(LL, 1, uinx, uiny)
                   uinx = nod2linx(LL, 2, ucxq(k2), ucyq(k2))
                   uiny = nod2liny(LL, 2, ucxq(k2), ucyq(k2))
                   ucxq(kb) = lin2nodx(LL, 1, uinx, uiny)
                   ucyq(kb) = lin2nody(LL, 1, uinx, uiny)
                else
                   ucx(kb) = ucx(k2)
                   ucy(kb) = ucy(k2)
                   ucxq(kb) = ucxq(k2)
                   ucyq(kb) = ucyq(k2)
                end if
             end if
             if (jased > 0 .and. stm_included) then
                dischcorrection = hs(k2) / hs(kb)
                !ucx(kb)  = ucx(kb)  * dischcorrection
                !ucy(kb)  = ucy(kb)  * dischcorrection
                ucxq(kb) = ucxq(kb) * dischcorrection
                ucyq(kb) = ucyq(kb) * dischcorrection
             end if
          end if
       end if

       if (kmx > 0) then
          call getLbotLtop(LL, Lb, Lt)
          do L = Lb, Lt
             kbk = ln(1, L); k2k = ln(2, L)
             if (jacstbnd == 0 .and. itpbn /= 2) then
                if (jasfer3D == 1) then
                   uin = nod2linx(LL, 2, ucx(k2k), ucy(k2k)) * cs + nod2liny(LL, 2, ucx(k2k), ucy(k2k)) * sn
                   ucx(kbk) = uin * lin2nodx(LL, 1, cs, sn)
                   ucy(kbk) = uin * lin2nody(LL, 1, cs, sn)
                   ! Nabi: uin = nod2linx(LL,2,ucxq(k2k),ucyq(k2k))*cs + nod2liny(LL,2,ucxq(k2k),ucyq(k2k))*sn
                   ucxq(kbk) = uin * lin2nodx(LL, 1, cs, sn)
                   ucyq(kbk) = uin * lin2nody(LL, 1, cs, sn)
                else
                   uin = ucx(k2k) * cs + ucy(k2k) * sn
                   ucx(kbk) = uin * cs
                   ucy(kbk) = uin * sn
                   ucxq(kbk) = uin * cs; ucyq(kbk) = uin * sn
                end if
             else
                if (jasfer3D == 1) then
                   uinx = nod2linx(LL, 2, ucx(k2k), ucy(k2k))
                   uiny = nod2liny(LL, 2, ucx(k2k), ucy(k2k))
                   ucx(kbk) = lin2nodx(LL, 1, uinx, uiny)
                   ucy(kbk) = lin2nody(LL, 1, uinx, uiny)
                   uinx = nod2linx(LL, 2, ucxq(k2k), ucyq(k2k))
                   uiny = nod2liny(LL, 2, ucxq(k2k), ucyq(k2k))
                   ucxq(kbk) = lin2nodx(LL, 1, uinx, uiny)
                   ucyq(kbk) = lin2nody(LL, 1, uinx, uiny)
                else
                   ucx(kbk) = ucx(k2k)
                   ucy(kbk) = ucy(k2k)
                   ucxq(kbk) = ucxq(k2k)
                   ucyq(kbk) = ucyq(k2k)
                end if
             end if
          end do
       end if
    end do

    if (jaZerozbndinflowadvection == 1) then
       do n = 1, nbndz ! on waterlevel boundaries put inflow advection velocity to 0
          LL = kbndz(3, n)
          do L = Lbot(LL), Ltop(LL)
             k1 = ln(1, L)
             if (u1(LL) > 0) then
                ucx(k1) = 0d0; ucy(k1) = 0d0
             end if
          end do
       end do
    else if (jaZerozbndinflowadvection == 2) then
       do n = 1, nbndz ! on waterlevel boundaries put all advection velocity to 0
          LL = kbndz(3, n)
          do L = Lbot(LL), Ltop(LL)
             k1 = ln(1, L)
             ucx(k1) = 0d0; ucy(k1) = 0d0
          end do
       end do
    end if

    do n = 1, nbndu ! velocity boundaries
       kb = kbndu(1, n)
       k2 = kbndu(2, n)
       LL = kbndu(3, n)
       cs = csu(LL); sn = snu(LL)
       if (make2dh) then
          if (hs(kb) > epshs) then
             if (jacstbnd == 0) then
                if (jasfer3D == 1) then
                   uin = nod2linx(LL, 2, ucx(k2), ucy(k2)) * cs + nod2liny(LL, 2, ucx(k2), ucy(k2)) * sn
                   ucx(kb) = uin * lin2nodx(LL, 1, cs, sn)
                   ucy(kb) = uin * lin2nody(LL, 1, cs, sn)

                   uin = nod2linx(LL, 2, ucxq(k2), ucyq(k2)) * cs + nod2liny(LL, 2, ucxq(k2), ucyq(k2)) * sn
                   ucxq(kb) = uin * lin2nodx(LL, 1, cs, sn)
                   ucyq(kb) = uin * lin2nody(LL, 1, cs, sn)
                else
                   uin = ucx(k2) * cs + ucy(k2) * sn
                   ucx(kb) = uin * cs
                   ucy(kb) = uin * sn
                   uin = ucxq(k2) * cs + ucyq(k2) * sn
                   ucxq(kb) = uin * cs
                   ucyq(kb) = uin * sn
                end if
             else
                if (jasfer3D == 1) then
                   uinx = nod2linx(LL, 2, ucx(k2), ucy(k2))
                   uiny = nod2liny(LL, 2, ucx(k2), ucy(k2))
                   ucx(kb) = lin2nodx(LL, 1, uinx, uiny)
                   ucy(kb) = lin2nody(LL, 1, uinx, uiny)

                   uinx = nod2linx(LL, 2, ucxq(k2), ucyq(k2))
                   uiny = nod2liny(LL, 2, ucxq(k2), ucyq(k2))
                   ucxq(kb) = lin2nodx(LL, 1, uinx, uiny)
                   ucyq(kb) = lin2nody(LL, 1, uinx, uiny)
                else
                   ucx(kb) = ucx(k2)
                   ucy(kb) = ucy(k2)
                   ucxq(kb) = ucxq(k2)
                   ucyq(kb) = ucyq(k2)
                end if
             end if
             if (jased > 0 .and. stm_included) then
                dischcorrection = hs(k2) / hs(kb)
                !ucx(kb)  = ucx(kb)  * dischcorrection
                !ucy(kb)  = ucy(kb)  * dischcorrection
                ucxq(kb) = ucxq(kb) * dischcorrection
                ucyq(kb) = ucyq(kb) * dischcorrection
             end if
          end if
       end if

       if (kmx > 0) then
          do k = 1, kmxL(LL)
             kbk = kbot(kb) - 1 + min(k, kmxn(kb))
             k2k = kbot(k2) - 1 + min(k, kmxn(k2))
             if (jacstbnd == 0) then
                if (jasfer3D == 1) then
                   uin = nod2linx(LL, 2, ucx(k2k), ucy(k2k)) * cs + nod2liny(LL, 2, ucx(k2k), ucy(k2k)) * sn
                   ucx(kbk) = uin * lin2nodx(LL, 1, cs, sn)
                   ucy(kbk) = uin * lin2nody(LL, 1, cs, sn)
                   ucxq(kbk) = uin * lin2nodx(LL, 1, cs, sn)
                   ucyq(kbk) = uin * lin2nody(LL, 1, cs, sn)
                else
                   uin = ucx(k2k) * cs + ucy(k2k) * sn
                   ucx(kbk) = uin * cs
                   ucy(kbk) = uin * sn
                   ucxq(kbk) = uin * cs
                   ucyq(kbk) = uin * sn
                end if
             else
                if (jasfer3D == 1) then
                   uinx = nod2linx(LL, 2, ucx(k2k), ucy(k2k))
                   uiny = nod2liny(LL, 2, ucx(k2k), ucy(k2k))
                   ucx(kbk) = lin2nodx(LL, 1, uinx, uiny)
                   ucy(kbk) = lin2nody(LL, 1, uinx, uiny)
                   uinx = nod2linx(LL, 2, ucxq(k2k), ucyq(k2k))
                   uiny = nod2liny(LL, 2, ucxq(k2k), ucyq(k2k))
                   ucxq(kbk) = lin2nodx(LL, 1, uinx, uiny)
                   ucyq(kbk) = lin2nody(LL, 1, uinx, uiny)
                else
                   ucx(kbk) = ucx(k2k)
                   ucy(kbk) = ucy(k2k)
                   ucxq(kbk) = ucxq(k2k)
                   ucyq(kbk) = ucyq(k2k)
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
             ucx(kbk) = lin2nodx(LL, 1, uucx, uucy)
             ucy(kbk) = lin2nody(LL, 1, uucx, uucy)
             ucxq(kbk) = ucx(kbk)
             ucyq(kbk) = ucy(kbk)
          else
             ucx(kbk) = uucx
             ucy(kbk) = uucy
             ucxq(kbk) = ucx(kbk)
             ucyq(kbk) = ucy(kbk)
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
             ucx(kbk) = lin2nodx(LL, 1, zbnduxy(2 * kk - 1), zbnduxy(2 * kk))
             ucy(kbk) = lin2nody(LL, 1, zbnduxy(2 * kk - 1), zbnduxy(2 * kk))
             ucxq(kbk) = ucx(kbk)
             ucyq(kbk) = ucy(kbk)
          else
             ucx(kbk) = zbnduxy(2 * kk - 1)
             ucy(kbk) = zbnduxy(2 * kk)
             ucxq(kbk) = ucx(kbk)
             ucyq(kbk) = ucy(kbk)
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
             ucx(kbk) = lin2nodx(LL, 1, uucx, uucy)
             ucy(kbk) = lin2nody(LL, 1, uucx, uucy)
             ucxq(kbk) = ucx(kbk)
             ucyq(kbk) = ucy(kbk)
          else
             ucx(kbk) = uucx
             ucy(kbk) = uucy
             ucxq(kbk) = ucx(kbk)
             ucyq(kbk) = ucy(kbk)
          end if
       end do
    end do

    newucxq = 0
    if (newucxq == 1) then ! test later, see testcase willem
       if (jasfer3D == 1) then ! boundary points ucxq, ucyq, independend of bnd types
          do L = lnxi + 1, lnx
             kb = ln(1, L); k2 = ln(2, L)
             do LL = Lbot(L), Ltop(L)
                uinx = nod2linx(LL, 2, ucxq(k2), ucyq(k2))
                uiny = nod2liny(LL, 2, ucxq(k2), ucyq(k2))
                ucxq(kb) = lin2nodx(LL, 1, uinx, uiny)
                ucyq(kb) = lin2nody(LL, 1, uinx, uiny)
             end do
          end do
       else
          do L = lnxi + 1, lnx
             kb = ln(1, L); k2 = ln(2, L)
             do LL = Lbot(L), Ltop(L)
                ucxq(kb) = ucxq(k2)
                ucyq(kb) = ucyq(k2)
             end do
          end do
       end if
    end if

    do n = 1, nbnd1d2d
       kb = kbnd1d2d(1, n)
       k2 = kbnd1d2d(2, n)
       LL = kbnd1d2d(3, n)

       if (kmx == 0) then ! 2D
          if (jasfer3D == 1) then
             uinx = nod2linx(LL, 2, ucx(k2), ucy(k2))
             uiny = nod2liny(LL, 2, ucx(k2), ucy(k2))
             ucx(kb) = lin2nodx(LL, 1, uinx, uiny)
             ucy(kb) = lin2nody(LL, 1, uinx, uiny)
             ucxq(kb) = ucx(kb)
             ucyq(kb) = ucy(kb)
          else
             ucx(kb) = ucx(k2)
             ucy(kb) = ucy(k2)
             ucxq(kb) = ucx(kb)
             ucyq(kb) = ucy(kb)
          end if
       else ! 3D

       end if
    end do

    if (limtypmom == 6) then

       ducxdx = 0d0; ducxdy = 0d0
       ducydx = 0d0; ducydy = 0d0
       do LL = 1, lnx
          Lb = Lbot(LL); Lt = Lb - 1 + kmxL(LL)
          do L = Lb, Lt
             k1 = ln(1, L)
             k2 = ln(2, L)
             if (jasfer3D == 1) then
                duxdn = dxi(LL) * (nod2linx(LL, 2, ucx(k2), ucy(k2)) - nod2linx(LL, 1, ucx(k1), ucy(k1)))
             else
                duxdn = dxi(LL) * (ucx(k2) - ucx(k1))
             end if
             ducxdx(k1) = ducxdx(k1) + wcx1(LL) * duxdn
             ducxdy(k1) = ducxdy(k1) + wcy1(LL) * duxdn
             ducxdx(k2) = ducxdx(k2) + wcx2(LL) * duxdn
             ducxdy(k2) = ducxdy(k2) + wcy2(LL) * duxdn

             if (jasfer3D == 1) then
                duydn = dxi(LL) * (nod2liny(LL, 2, ucx(k2), ucy(k2)) - nod2liny(LL, 1, ucx(k1), ucy(k1)))
             else
                duydn = dxi(LL) * (ucy(k2) - ucy(k1))
             end if
             ducydx(k1) = ducydx(k1) + wcx1(LL) * duydn
             ducydy(k1) = ducydy(k1) + wcy1(LL) * duydn
             ducydx(k2) = ducydx(k2) + wcx2(LL) * duydn
             ducydy(k2) = ducydy(k2) + wcy2(LL) * duydn
          end do
       end do

       !do nw  = 1,mxwalls   ! to be finished later zz
       !   csw = walls(7,nw)
       !   snw = walls(8,nw)
       !   ducdn = 2d0*(ucx(k1)*csw +
       !   ducxdx(k1) = ducxdx(k1) + *duxdn
       !enddo

    end if

    if (kmx < 1) then

       if (jasfer3D == 1) then
          !$OMP PARALLEL DO           &
          !$OMP PRIVATE(L)
          do L = 1, lnx
             if (qa(L) > 0) then ! set upwind ucxu, ucyu  on links
                ucxu(L) = nod2linx(L, 1, ucx(ln(1, L)), ucy(ln(1, L)))
                ucyu(L) = nod2liny(L, 1, ucx(ln(1, L)), ucy(ln(1, L)))
             else if (qa(L) < 0) then
                ucxu(L) = nod2linx(L, 2, ucx(ln(2, L)), ucy(ln(2, L)))
                ucyu(L) = nod2liny(L, 2, ucx(ln(2, L)), ucy(ln(2, L)))
             else
                ucxu(L) = 0d0
                ucyu(L) = 0d0
             end if
          end do
          !$OMP END PARALLEL DO
       else
          !$OMP PARALLEL DO           &
          !$OMP PRIVATE(L)
          do L = 1, lnx
             if (qa(L) > 0) then ! set upwind ucxu, ucyu  on links
                ucxu(L) = ucx(ln(1, L))
                ucyu(L) = ucy(ln(1, L))
             else if (qa(L) < 0) then
                ucxu(L) = ucx(ln(2, L))
                ucyu(L) = ucy(ln(2, L))
             else
                ucxu(L) = 0d0
                ucyu(L) = 0d0
             end if
          end do
          !$OMP END PARALLEL DO
       end if

    else

       if (jasfer3D == 1) then
          !$OMP PARALLEL DO           &
          !$OMP PRIVATE(LL,L,Lb,Lt)
          do LL = 1, lnx
             call getLbotLtop(LL, Lb, Lt)
             do L = Lb, Lt
                if (qa(L) > 0) then ! set upwind ucxu, ucyu  on links
                   ucxu(L) = nod2linx(LL, 1, ucx(ln0(1, L)), ucy(ln0(1, L)))
                   ucyu(L) = nod2liny(LL, 1, ucx(ln0(1, L)), ucy(ln0(1, L)))
                   if (jarhoxu > 0) then
                      ucxu(L) = ucxu(L) * rho(ln(1, L))
                      ucyu(L) = ucyu(L) * rho(ln(1, L))
                   end if
                else if (qa(L) < 0) then
                   ucxu(L) = nod2linx(LL, 2, ucx(ln0(2, L)), ucy(ln0(2, L)))
                   ucyu(L) = nod2liny(LL, 2, ucx(ln0(2, L)), ucy(ln0(2, L)))
                   if (jarhoxu > 0) then
                      ucxu(L) = ucxu(L) * rho(ln(2, L))
                      ucyu(L) = ucyu(L) * rho(ln(2, L))
                   end if
                else
                   ucxu(L) = 0d0
                   ucyu(L) = 0d0
                end if
             end do
          end do
          !$OMP END PARALLEL DO

       else

          !$OMP PARALLEL DO           &
          !$OMP PRIVATE(LL,L,Lb,Lt)
          do LL = 1, lnx
             call getLbotLtop(LL, Lb, Lt)
             do L = Lb, Lt
                if (qa(L) > 0) then ! set upwind ucxu, ucyu  on links
                   ucxu(L) = ucx(ln0(1, L))
                   ucyu(L) = ucy(ln0(1, L))
                   if (jarhoxu > 0) then
                      ucxu(L) = ucxu(L) * rho(ln(1, L))
                      ucyu(L) = ucyu(L) * rho(ln(1, L))
                   end if
                else if (qa(L) < 0) then
                   ucxu(L) = ucx(ln0(2, L))
                   ucyu(L) = ucy(ln0(2, L))
                   if (jarhoxu > 0) then
                      ucxu(L) = ucxu(L) * rho(ln(2, L))
                      ucyu(L) = ucyu(L) * rho(ln(2, L))
                   end if
                else
                   ucxu(L) = 0d0
                   ucyu(L) = 0d0
                end if
             end do
          end do
          !$OMP END PARALLEL DO

       end if

    end if

    if (kmx == 0 .and. lnx1D > 0) then ! setuc
       call setuc1D()
    end if

 end subroutine setucxucyucxuucyu
