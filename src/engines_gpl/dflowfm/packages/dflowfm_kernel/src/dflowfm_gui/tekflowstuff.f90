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

!----------------------------------------------------------------------
! subroutines from unstruc.f90
!----------------------------------------------------------------------
 subroutine tekflowstuff(ja)
    use unstruc_display
    use m_netw
    use m_flowgeom
    use m_flow
    use m_wind
    use m_reduce
    use geometry_module, only: getdx, getdy, getdxdy
    use gridoperations

    use m_observations

    implicit none

    integer :: ndraw
    double precision :: vfac, vfacforce
    integer :: nvec
    common / drawthis / ndraw(50)
    common / VFAC / VFAC, VFACFORCE, NVEC

    !locals
    integer :: k, kk, L, LL, k1, k2, ncol, nn, k3, k4
    integer :: nodemode, linkmode ! how  to show on flow nodes and links
    integer :: nodewhat ! what to show on flow nodes and links
    double precision :: znod, zcorn ! what to show functions
    double precision :: xx1, yy1, Zz1 ! help only
    double precision :: xx2, yy2, Zz2 ! help only
    double precision :: x3, y3 ! help only
    double precision :: zn
    integer :: model24 = 0 ! colourmodel 0/1
    double precision :: uux, uuy
    integer :: n, ja, ja2
    logical inview

    ! ndraw(28)= show what on nodes   ndraw(19)=how to show on nodes , NDRAW(8) = SHOW WHAT ON NETNODES
    ! ndraw(29)= show what on links   ndraw(11)=how to show on links , NDRAW(7) = SHOW WHAT ON NETLINKS

    if (ndx == 0) return

! nplot = min(ndxi, nplot)

    nplot = max(1, min(Ndx, nplot))

    nodemode = ndraw(19)
    linkmode = ndraw(11)
    nodewhat = ndraw(28)
    ja = 0

    call tekbathy(ja)

    if (nodemode > 1 .and. nodewhat > 1) then
       if (NDRAW(8) == 1) call minmxnds() ! ONLY ADAPT VERTICAL LIMITS FOR FLOW NODES IF NO NET NODES ASKED

       call tekflownodes(ja)
       if (ja > 0) then
          return
       end if

       if (jaHighlight == 1) then
          if (ndmax /= 0) then
             call gtext('NDMAX', xz(ndmax), yz(ndmax), 31)
          end if
          if (ndmin /= 0) then
             call gtext('ndmin', xz(ndmin), yz(ndmin), 221)
          end if
       end if

       if (ndraw(37) >= 1) then
          call tekprofpoint()
       end if
       if (ndraw(37) >= 2) then
          do k = 1, nbndz ! boundary points tekflowstuff
             k1 = kbndz(1, k)
             k2 = kbndz(2, k)
             LL = kbndz(3, k)
             zn = znod(k1)
             call isocol(zn, ncol)

             ja2 = 1
             if (wetplot > 0d0 .and. hu(LL) > 0d0) then
                if (hs(k1) < wetplot) then
                   ja2 = 0
                end if
                if (ja2 == 1) then ! nodewhat==3: always show bottom
                   if (inview(xz(k1), yz(k1))) then
                      call dhtext(zn, xz(k1), yz(k1), bl(k1))
                   end if
                end if
             end if

             call dmovabs(xz(k1), yz(k1), bl(k1))
             call dlnabs(xz(k2), yz(k2), bl(k2))
          end do

          do k = 1, nbndu ! boundary points tekflowstuff
             k1 = kbndu(1, k)
             k2 = kbndu(2, k)
             zn = znod(k1)
             call isocol(zn, ncol)
             call dmovabs(xz(k1), yz(k1), bl(k1))
             call dlnabs(xz(k2), yz(k2), bl(k2))
             call dhtext(zn, xz(k1), yz(k1), bl(k1))
          end do
       end if

       if (ndraw(28) == 28) then ! checkerboard gauss elimin / conj grad
          ncol = 31
          do n = nogauss0 + 1, nogauss0 + nocg0
             k = noel0(n)
             nn = size(nd(k)%x)
             call PFILLER(nd(k)%x, nd(k)%y, nn, NCOL, NCol)
          end do

          ncol = 221
          do n = 1, nogauss0
             k = noel0(n)
             nn = size(nd(k)%x)
             call PFILLER(nd(k)%x, nd(k)%y, nn, NCOL, NCol)
          end do

       end if

    end if

    if (model24 == 1) then
       call igrcolourmodel(8)
    end if

    call tekflowlinks()

    if (jaHighlight == 1) then
       if (Lnmax /= 0) then
          call gtext('Lmax', xu(Lnmax), yu(Lnmax), 31)
       end if
       if (Lnmin /= 0) then
          call gtext('Lmin', xu(Lnmin), yu(Lnmin), 221)
       end if
    end if

    if (ndraw(31) >= 2) then ! cell corners
       call setcol(221)
       do k = 1, size(ucnx)
          if (inview(xk(k), yk(k))) then
             if (ndraw(31) <= 4) then ! numbers
                zn = zcorn(k)
                call dhtext(zn, xk(k), yk(k), zk(k))
             else if (ndraw(31) == 5) then ! vectors
                call arrowsxy(xk(k), yk(k), ucnx(k), ucny(k), VFAC)
             end if
          end if
       end do
    end if

    if (ndraw(30) >= 2) then ! show links in white
       call setcol(221) ! NCOLRG)
       do L = 1, lnx
          k1 = ln(1, L)
          k2 = ln(2, L)
          if (inview(xz(k1), yz(k1)) .or. inview(xz(k2), yz(k2))) then
             XX1 = XZ(K1)
             YY1 = YZ(K1)
             ZZ1 = Bob(1, L)
             XX2 = XZ(K2)
             YY2 = YZ(K2)
             ZZ2 = Bob(2, L)
             if (ndraw(30) == 2) then
                call dmovabs(Xx1, Yy1, Zz1)
                call dlnabs(Xx2, Yy2, Zz2)
                call dcirr(Xx1, Yy1, Zz1, 221)
                call dcirr(Xx2, Yy2, Zz2, 221)
             else if (ndraw(30) == 3) then
                if (L > lnx1D) then
                   k3 = lncn(1, L)
                   k4 = lncn(2, L)
                   X3 = 0.5d0 * (Xk(k3) + Xk(k4))
                   Y3 = 0.5d0 * (Yk(k3) + Yk(k4))
                else ! Arrows for 1D links
                   X3 = XX1
                   Y3 = YY1
                end if
                call arrowrcir(x3, y3, csu(L), snu(L))
             else if (ndraw(30) == 4) then
                if (hu(L) < epshs) then
                   if (abs(kcu(L)) == 2) then
                      k3 = lncn(1, L)
                      k4 = lncn(2, L)
                      call movabs(xk(k3), yk(k3))
                      call lnabs(xk(k4), yk(k4))
                   else
                      call movabs(xx1, yy1)
                      call lnabs(xx2, yy2)
                   end if
                end if
             end if
          end if
       end do

    end if

! do k = 1,ns
!    call ptabs(xs(k), ys(k))
! enddo

    if (ndraw(13) >= 2 .and. ndraw(13) <= 4) then ! show vectors centre based
       call setcol(KLVEC)
       do kk = 1, ndx, nvec
          if (mod(kk, 200) == 0) then
             call halt2(ja)
             if (ja == 1) return
          end if

          if (inview(xz(kk), yz(kk))) then
             k = kk
             if (kmx > 0) then
                call getktoplot(kk, k)
                if (k < 0) cycle
             end if

             if (ndraw(13) == 2) then
                uux = ucx(k)
                uuy = ucy(k)
             else if (ndraw(13) == 3) then
                uux = ucx(k)
                uuy = ucy(k)
             else if (ndraw(13) == 4) then
                uux = uqcx(k)
                uuy = uqcy(k)
             end if
             call arrowsxy(xz(kk), yz(kk), uux, uuy, VFAC)
          end if
       end do
    else if (ndraw(13) == 5) then ! show vectors u based
       call setcol(3)
       do L = 1, lnx
          if (inview(xu(L), yu(L))) then
             uux = wx(L)
             uuy = wy(L)
             call arrowsxy(xu(L), yu(L), uux, uuy, VFAC)
          end if
       end do
    else if (ndraw(13) == 6) then ! show arc wind
       call setcol(221)
       call tekarcuv(vfac, ndraw(13))
    else if (ndraw(13) == 7) then ! show arc pressure
       call setcol(31)
       call tekarcuv(vfac, ndraw(13))
    else if (ndraw(13) == 8) then ! show arc wind
       call setcol(221)
       call tekspw(vfac, ndraw(13))
    else if (ndraw(13) == 9) then ! show primitive velocity u1
       call setcol(2)
       do LL = 1, lnx
          if (inview(xu(LL), yu(LL))) then
             L = Lbot(LL) - 1 + min(kplot, kmxL(LL))
             uux = u1(L) * csu(LL)
             uuy = u1(L) * snu(LL)
             call arrowsxy(xu(LL), yu(LL), uux, uuy, VFAC)
          end if
       end do
    end if

    if (nodnegtek /= 0) then
       call setcol(221)
       call rcirc(xz(nodnegtek), yz(nodnegtek))
       nodnegtek = 0
    end if

    ! call tekcflmx()

    if (jased > 0 .and. jased <= 3) call tekbanfs()

    call tekship()

 end subroutine tekflowstuff
