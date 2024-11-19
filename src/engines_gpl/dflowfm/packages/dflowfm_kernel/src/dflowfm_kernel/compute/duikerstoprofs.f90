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

module m_duikerstoprofs

implicit none

contains

 subroutine duikerstoprofs()
    use m_netw
    use m_flowgeom
    use m_flow
    use m_missing
    use m_polygon
    use unstruc_model
    use m_sferic
    use geometry_module, only: dbdistance
    use m_partitioninfo
    use m_qnerror
    use m_reapol
    implicit none

    integer :: minp, Ls, Lf, n, k1, k2
    double precision :: x1, y1, z1, x2, y2, z2, xc, yc, XLS, YLS, dum, w1, w2, h1, h2
    logical :: jawel

    inquire (file=trim(md_pipefile), exist=jawel)
    if (.not. jawel) return

    call oldfil(minp, md_pipefile)
    call reapol(minp, 0)
    allocate (jaduiktmp(1:Lnx1D)); jaduiktmp = 0

    do n = 1, npl - 1

       x1 = xpl(n); y1 = ypl(n); z1 = zpl(n); w1 = dzL(n); h1 = dzR(n)
       x2 = xpl(n + 1); y2 = ypl(n + 1); z2 = zpl(n + 1); w2 = dzL(n + 1); h2 = dzR(n + 1)
       if (x1 == DMISS .or. x2 == DMISS) cycle
       if (w1 <= 0d0 .or. w2 <= 0d0) then
          call qnerror(' pipes: width <= 0d0, fourth column', 'in', md_pipefile)
          call qnerror(' pipes: width <= 0d0, fourth column', 'in', md_pipefile)
       end if

       if (x1 /= dmiss .and. x2 /= dmiss) then
          xc = 0.5d0 * (x1 + x2)
          yc = 0.5d0 * (y1 + y2)
          call CLOSETO1Dnetlink(Xc, Yc, LS, XLS, YLS, dum, 0)
          if (Ls > 0) then
             Lf = lne2ln(Ls)
             if (kcu(Lf) == 1 .or. kcu(Lf) == 5) then
                k1 = ln(1, Lf); k2 = ln(2, Lf)
                if (dbdistance(X1, Y1, Xzw(K1), Yzw(K1), jsferic, jasfer3D, dmiss) < dbdistance(X1, Y1, Xzw(K2), Yzw(K2), jsferic, jasfer3D, dmiss)) then
                   bob(1, Lf) = z1; bl(k1) = min(z1, bl(k1))
                   bob(2, Lf) = z2; bl(k2) = min(z2, bl(k2))
                else
                   bob(1, Lf) = z2; bl(k1) = min(z2, bl(k1))
                   bob(2, Lf) = z1; bl(k2) = min(z1, bl(k2))
                end if
                prof1D(1, Lf) = w1; wu(Lf) = w1
                prof1D(2, Lf) = h1
                prof1D(3, Lf) = -2 ! for now, simple rectan, closed
                jaduiktmp(Lf) = 1
             end if
          end if
       end if

    end do
 end subroutine duikerstoprofs

end module m_duikerstoprofs
