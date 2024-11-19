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

module m_zlin

implicit none

contains

 double precision function zlin(LL) ! get various values at flow links
    use m_getltoplot
    use m_flow
    use m_flowgeom
    use m_wind
    use m_sediment
    use m_sferic
    use m_missing
    use m_drawthis
    use m_cosphiu
    implicit none

    integer, intent(in) :: LL
    integer :: L, linval, k1, k2, n1, n2, lll, ka, kb
    double precision :: omega1, omega2, zb1, zb2, dum, alfa

    zlin = dmiss
    if (LL < 1) then
       return
    end if

    L = LL
    if (kmx > 0) then
       call getLtoplot(LL, L)
       if (L < 0) return
    end if

    linval = ndraw(29)
    if (linval == 2) then
       zlin = abs(u1(L))
    else if (linval == 3) then
       zlin = q1(L) * wui(LL)
    else if (linval == 4) then
       zlin = q1(L)
    else if (linval == 5) then
       zlin = au(L)
    else if (linval == 6) then
       if (hu(LL) > epshu) then
          zlin = hu(L)
       else
          zlin = dmiss
       end if
    else if (linval == 7) then
       zlin = frcu(LL)

       if (LL <= lnx1D) then
          if (prof1D(1, LL) < 0) then ! profile interpolation
             ka = -prof1D(1, LL); kb = -prof1D(2, LL)
             alfa = prof1d(3, LL)
             if (profiles1D(ka)%frccf /= dmiss .and. profiles1D(kb)%frccf /= dmiss .and. &
                 profiles1D(ka)%frctp == profiles1D(kb)%frctp) then
                zlin = (1d0 - alfa) * profiles1D(ka)%frccf + alfa * profiles1D(kb)%frccf
             end if
          end if
       end if
    else if (linval == 8) then
       zlin = dx(LL)
    else if (linval == 9) then
       zlin = wu(LL)
    else if (linval == 10) then ! rust aan de ogen
       zlin = bob(1, LL)
    else if (linval == 11) then
       zlin = bob(2, LL)
    else if (linval == 12) then
       zlin = dble(kcu(LL))
    else if (linval == 13) then
       zlin = vicLu(L)
    else if (linval == 14) then
       zlin = teta(LL)
    else if (linval == 15) then
    else if (linval == 16) then
       zlin = u1(L)
    else if (linval == 17) then
       zlin = adve(L) !*csu(LL)
    else if (linval == 18) then
       zlin = advi(L)
    else if (linval == 19) then
       zlin = Fu(L)
    else if (linval == 20) then
       zlin = Ru(L) ! -ag*dxi(L)*( s0(ln(2,L)) - s0(ln(1,L)) ) -  adve(L)
    else if (linval == 21) then
       zlin = suu(L)
    else if (linval == 22) then
       if (javeg == 0) then
          if (jaconveyance2D >= 1) then
             zlin = aifu(LL) ! ccr(lv2(LL))
          end if
       else
          k1 = ln(1, L); k2 = ln(2, L)
          if (diaveg(k1) > 0 .and. diaveg(k2) > 0) then
             zlin = 0.5d0 * (diaveg(k1) + diaveg(k2))
          else
             zlin = max(diaveg(k1), diaveg(k2))
          end if
       end if
    else if (linval == 23) then
       if (javeg == 0) then
          zlin = (s1(ln(2, LL)) - s1(ln(1, LL))) * dxi(LL)
       else
          k1 = ln(1, L); k2 = ln(2, L)
          zlin = 0.5d0 * (rnveg(k1) + rnveg(k2))
       end if
    else if (linval == 24) then
       if (javeg == 0) then
          zlin = cfuhi(LL)
       else
          k1 = ln(1, L); k2 = ln(2, L)
          if (stemheight(k1) > 0 .and. stemheight(k2) > 0) then
             zlin = 0.5d0 * (stemheight(k1) + stemheight(k2))
          else
             zlin = max(stemheight(k1), stemheight(k2))
          end if
       end if
    else if (linval == 25) then
       if (jawind > 0) then
          zlin = wx(LL)
       end if
    else if (linval == 26) then
       if (jawind > 0) then
          zlin = wy(LL)
       end if
    else if (linval == 27) then
       if (jawind > 0) then
          zlin = wdsu_x(LL); jamapwindstress = 1
       end if
    else if (linval == 28) then
       zlin = abs(cosphiu(LL))
    else if (linval == 29) then
       zlin = LL
    else if (linval == 30) then
       zlin = v(L)
    else if (linval == 31) then
       zlin = fu(L)
    else if (linval == 32) then
       zlin = ru(L)
    else if (linval == 33) then
       zlin = iadv(LL)
    else if (linval == 34) then
       zlin = plotlin(L)
    else if (linval == 35) then
       zlin = ln(1, L)
    else if (linval == 36) then
       zlin = ln(2, L)
    else if (linval == 37) then !
       zlin = plotlin(L)
    else if (linval == 38) then
       zlin = plotlin(L)
    else if (linval == 39) then
       zlin = dxi(LL) * (bl(ln(2, LL)) - bl(ln(1, LL)))
    else if (linval == 40) then
       zlin = ifrcutp(LL)
    else if (linval == 41) then
       if (kmx > 0) then
          zlin = turkin0(L)
       else
          if (LL <= lnx1D) zlin = prof1D(1, LL)
       end if
    else if (linval == 42) then
       if (kmx > 0) then
          zlin = tureps0(L)
       else
          if (LL <= lnx1D) zlin = prof1D(2, LL)
       end if
    else if (linval == 43) then
       if (kmx > 0) then
          zlin = vicwwu(L)
       else
          if (LL <= lnx1D) zlin = prof1D(3, LL)
       end if
    else if (linval == 44) then
       zlin = ustb(LL)
    else if (linval == 45) then
       if (jawind > 0 .and. kmx > 0) then
          zlin = ustw(LL)
       else if (L < ltop(LL)) then
          k1 = ln(1, L); k2 = ln(2, L)
          n1 = ln(1, LL); zb1 = zws(kbot(n1) - 1)
          n2 = ln(2, LL); zb2 = zws(kbot(n2) - 1)
          omega1 = qw(k1) / a1(ln(1, LL))
          omega2 = qw(k2) / a1(ln(2, LL))

          zlin = 0.5d0 * omega1 + 0.5d0 * omega2 + 0.5d0 * (u0(L) + u0(L + 1)) * (zws(k2) - zb2 - (zws(k1) - zb1)) * dxi(LL)
       end if
    else if (linval == 46) then
       if (hu(LL) > epshu) then
          zlin = hu(L) - hu(L - 1)
       end if
    else if (linval == 47) then
       if (jafrculin > 0) then
          zlin = frculin(LL)
       else
          zlin = dmiss
       end if
    else if (linval == 48) then
       if (jawave > 2 .and. jawave < 5) then
          zlin = wavfu(L)
       end if
    else if (linval == 54 .and. stm_included) then
       select case (sedparopt)
       case (1)
          dum = 0d0
          do lll = 1, stmpar%lsedtot
             dum = dum + sedtra%e_sbcn(LL, lll)
          end do
          zlin = dum
       case (2)
          dum = 0d0
          do lll = 1, stmpar%lsedsus
             dum = dum + sedtra%e_ssn(LL, lll)
          end do
          zlin = dum
       case (3)
          dum = 0d0
          do lll = 1, stmpar%lsedtot
             dum = dum + sedtra%e_sbwn(LL, lll)
          end do
          zlin = dum
       case (4)
          dum = 0d0
          do lll = 1, stmpar%lsedtot
             dum = dum + sedtra%e_sswn(LL, lll)
          end do
          zlin = dum
       case (5)
          dum = 0d0
          do lll = 1, stmpar%lsedsus
             dum = dum + sedtra%e_ssn(LL, lll)
          end do
          do lll = 1, stmpar%lsedtot
             dum = dum + sedtra%e_sswn(LL, lll) + sedtra%e_sbwn(LL, lll) + sedtra%e_sbcn(LL, lll)
          end do
          zlin = dum
       case (6)
          dum = 0d0
          do lll = 1, stmpar%lsedtot
             dum = dum + bermslopecontrib(LL, lll)
          end do
          zlin = dum
       case (7)
          zlin = merge(1.0d0, 0.0d0, bermslopeindex(LL))
       end select
    else if (linval == 49) then
       zlin = Ltop(LL) - Lbot(LL) + 1; zlin = max(zlin, 0d0)
    else if (linval == 50) then
       zlin = kmxL(LL)
    else if (linval == 51) then
       zlin = Lbot(LL)
    else if (linval == 52) then
       zlin = Ltop(LL)
    else if (linval == 53) then
       dum = min(a1(ln(1, LL)), a1(ln(2, LL)))
       if (dum > 0) then
          zlin = max(a1(ln(1, LL)), a1(ln(2, LL))) / dum
       end if
    end if

 end function zlin

end module m_zlin
