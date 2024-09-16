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

 subroutine shift1Dnetnodestoduikers()
    use m_netw
    use m_flowgeom
    use m_flow
    use m_missing
    use m_polygon
    use unstruc_files
    use geometry_module, only: dbdistance
    use m_sferic, only: jsferic, jasfer3D
    use gridoperations
    use m_reapol
    use m_wripol

    implicit none
    integer :: minp, Ls, n, k1, k2, kL, kR, LnL
    double precision :: x1, y1, z1, x2, y2, z2, xc, yc, XLS, YLS, dum, dis12, dis11, disL, disd, dis22, dis21, alf, zx, xL, yL, xR, yR, half, xkc, ykc
    character(len=maxlength) :: pipefilein, pipefileout
    if (numL == 0) return

    pipefilein = defaultFilename('pipe') ! is dit duikers.pliz ?
    pipefileout = defaultFilename('pipe2') ! duikers2.pliz ?

    call oldfil(minp, pipefilein)
    call reapol(minp, 0)
    kn3typ = 1

    do n = 1, npl - 1

       x1 = xpl(n); y1 = ypl(n); z1 = zpl(n)
       x2 = xpl(n + 1); y2 = ypl(n + 1); z2 = zpl(n + 1)
       if (x1 /= dmiss .and. x2 /= dmiss) then
          xc = 0.5d0 * (x1 + x2)
          yc = 0.5d0 * (y1 + y2)
          zx = max(z1, z2)
          call CLOSETO1Dnetlink(Xc, Yc, LS, XLS, YLS, dum, 0)
          if (Ls > 0) then
             k1 = kn(1, LS); k2 = kn(2, LS)
             dis11 = dbdistance(x1, y1, xk(k1), yk(k1), jsferic, jasfer3D, dmiss)
             dis12 = dbdistance(x2, y2, xk(k1), yk(k1), jsferic, jasfer3D, dmiss)
             dis21 = dbdistance(x1, y1, xk(k2), yk(k2), jsferic, jasfer3D, dmiss)
             dis22 = dbdistance(x2, y2, xk(k2), yk(k2), jsferic, jasfer3D, dmiss)
             disL = dbdistance(xk(k1), yk(k1), xk(k2), yk(k2), jsferic, jasfer3D, dmiss)
             disd = dbdistance(x1, y1, x2, y2, jsferic, jasfer3D, dmiss)
             alf = min(1d0, disd / disL); half = 0.5d0 * alf
             if (nmk(k1) > 0 .and. nmk(k2) > 0) then ! both fixed, shift to centre
                ! call splitlink(x1, y1, L_, 0.9d0, 1, ierror)
                xkc = 0.5d0 * (xk(k2) + xk(k1))
                ykc = 0.5d0 * (yk(k2) + yk(k1))
                xL = xkc - half * (xk(k2) - xk(k1))
                yL = ykc - half * (yk(k2) - yk(k1))
                xR = xkc + half * (xk(k2) - xk(k1))
                yR = ykc + half * (yk(k2) - yk(k1))
                kn(1, Ls) = 0; kn(2, Ls) = 0; kn(3, Ls) = 0
                call setnewpoint(xL, YL, zx, kL)
                call connectdbn(k1, kL, LnL)
                call setnewpoint(xR, YR, zx, kR)
                call connectdbn(kL, kR, LnL)
                call connectdbn(kR, k2, LnL)
                k1 = kL; k2 = kR
             else if (nmk(k1) > 2) then ! keep k1
                if (nmk(k2) == 2) then ! shift to k1
                   xk(k2) = xk(k1) + alf * (xk(k2) - xk(k1))
                   yk(k2) = yk(k1) + alf * (yk(k2) - yk(k1))
                end if
             else if (nmk(k2) > 2) then ! keep k2
                if (nmk(k1) == 2) then ! shift to k2
                   xk(k1) = xk(k2) + alf * (xk(k1) - xk(k2))
                   yk(k1) = yk(k2) + alf * (yk(k1) - yk(k2))
                end if
             else
                if (dis11 < dis12) then
                   xk(k1) = x1; yk(k1) = y1
                else
                   xk(k1) = x2; yk(k1) = y2
                end if
                if (dis21 < dis22) then
                   xk(k2) = x1; yk(k2) = y1
                else
                   xk(k2) = x2; yk(k2) = y2
                end if
             end if
             zk(k1) = dmiss; zk(k2) = dmiss
             xpl(n) = xk(k1); ypl(n) = yk(k1)
             xpl(n + 1) = xk(k2); ypl(n + 1) = yk(k2)
          end if
       end if
    end do

    call newfil(minp, pipefileout)
    call wripol(minp)

    return
 end subroutine shift1Dnetnodestoduikers
