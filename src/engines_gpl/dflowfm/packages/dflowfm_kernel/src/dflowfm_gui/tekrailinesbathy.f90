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

 subroutine tekrailinesBATHY(ncol, ITYP)
    use m_flowgeom
    use M_FLOW
    use m_flowtimes
    use m_sferic
    use unstruc_display
    use m_netw, only: xk, yk, zk, kc
    use m_sediment
    use m_polygon

    implicit none
    integer :: ncol, ITYP
    integer :: L, k1, k2, kk, k, n
    double precision :: zz1, zz2, xx1, xx2, yy1, yy2
    integer :: ja, jg

    call setcol(ncol)
    do L = 1, lnx
       if (mod(L, 200) == 0) then
          call halt2(ja)
          if (ja == 1) exit
       end if

       k1 = ln(1, L)
       k2 = ln(2, L)
       if (npl > 1) then
          if (kc(k1) * kc(k2) == 0) cycle
       end if

       if (ityp == 1) then ! bottom layer
          if (ibedlevtyp == 1 .or. ibedlevtyp == 6) then ! tegelen
             xx1 = xz(k1)
             xx2 = xz(k2)
             zz1 = bl(k1)
             zz2 = bl(k2)
             if (yfac > 0) then
                yy1 = yz(k1)
                yy2 = yz(k2)
             end if
          else
             k1 = lncn(1, L)
             k2 = lncn(2, L)
             xx1 = xk(k1)
             xx2 = xk(k2)
             zz1 = zk(k1)
             zz2 = zk(k2)
             if (yfac > 0) then
                yy1 = yk(k1)
                yy2 = yk(k2)
             end if
          end if
       else ! non erodable layer
          jg = ityp - 1
          if (jaceneqtr == 1) then ! combined data not really drawn precise if no tegel
             k1 = ln(1, L)
             k2 = ln(2, L)
             xx1 = xz(k1)
             xx2 = xz(k2)
             zz1 = bl(k1) - sum(grainlay(1:jg, k1))
             zz2 = bl(k2) - sum(grainlay(1:jg, k2))
             if (yfac > 0) then
                yy1 = yz(k1)
                yy2 = yz(k2)
             end if
          else
             k1 = lncn(1, L)
             k2 = lncn(2, L)
             xx1 = xk(k1)
             xx2 = xk(k2)
             zz1 = zk(k1) - sum(grainlay(1:jg, k1))
             zz2 = zk(k2) - sum(grainlay(1:jg, k2))
             if (yfac > 0) then
                yy1 = yk(k1)
                yy2 = yk(k2)
             end if
          end if
       end if

       if (zz1 == dmiss .or. zz2 == dmiss) cycle

       if (yfac > 0) then
          zz1 = zz1 + (yy1 - ymn) * yfac
          zz2 = zz2 + (yy1 - ymn) * yfac
       end if

       if (jsferic == 1) then ! jglobe
          if (abs(xz(ln(1, L)) - xz(ln(2, L))) > 10d0) cycle
       end if

       if (abs(zz1) < 1d-6) zz1 = 0d0 ! heh heh, eindelijk
       if (abs(zz2) < 1d-6) zz2 = 0d0

       call movabs(xx1, zz1)
       call lnabs(xx2, zz2)
    end do

    do L = 1, lnx1D
       k1 = ln(1, L)
       k2 = ln(2, L)
       if (npl > 1) then
          if (kc(k1) * kc(k2) == 0) cycle
       end if

       xx1 = xz(k1)
       xx2 = xz(k2)
       zz1 = bl(k1)
       zz2 = bl(k2)
       if (yfac > 0) then
          yy1 = yz(k1)
          yy2 = yz(k2)
          zz1 = zz1 + (yy1 - ymn) * yfac
          zz2 = zz2 + (yy1 - ymn) * yfac
       end if
       call movabs(xx1, zz1)
       call lnabs(xx2, zz2)
    end do

    if (jaceneqtr == 2 .and. ityp == 4) then
       do kk = 1, mxban
          n = nban(1, kk)
          k = nban(2, kk)
          xx1 = xz(k)
          yy1 = yz(k)
          zz1 = zmn + grainlay(1, kk) ! bl(k) - grainlay(1,kk)
          xx2 = xk(n)
          yy2 = yk(n)
          zz2 = zmn + grainlay(1, kk) ! zk(n) - grainlay(1,kk)

          call movabs(xx1, zz1)
          call lnabs(xx2, zz2)
       end do
    end if

 end subroutine tekrailinesBATHY
