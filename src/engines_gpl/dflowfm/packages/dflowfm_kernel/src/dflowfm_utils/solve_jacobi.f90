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

 subroutine solve_jacobi(s1, ndx, itsol) ! uses both s0 and s1
    use m_flowgeom, only: ln, kfs, nd
    use m_flowtimes
    use m_jacobi
    use m_reduce

    implicit none

    double precision :: ds, rrn ! max error
    integer :: L, n, ndx, itsol, nn, La, n1, n2, ni
    double precision :: s1(ndx)

    !$OMP PARALLEL DO                                          &
    !$OMP PRIVATE(n)
    do n = 1, ndx
       if (kfs(n) == 1) then
          bbi(n) = 1d0 / bbr(n)
          db(n) = ddr(n) * bbi(n)
       end if
    end do
    !$OMP END PARALLEL DO

    itmxjac = 100000
    itsol = 0
    ds = 1d10

    do while (ds > epscg) ! Jacobi

       if (mod(itsol, 2) == 0) then
          n1 = 1; n2 = ndx; ni = 1
       else
          n2 = 1; n1 = ndx; ni = -1
       end if

       !$OMP PARALLEL DO                                          &
       !$OMP PRIVATE(n,nn,L,La)
       do n = n1, n2, ni
          if (kfs(n) == 1) then
             s1(n) = db(n)
             do nn = 1, nd(n)%lnx
                L = nd(n)%ln(nn); La = iabs(L)
                if (ccr(Lv2(La)) < 0d0) then
                   if (L > 0) then
                      s1(n) = s1(n) - ccr(Lv2(La)) * s1(ln(1, La)) * bbi(n)
                   else
                      s1(n) = s1(n) - ccr(Lv2(La)) * s1(ln(2, La)) * bbi(n)
                   end if
                end if
             end do
          end if
       end do
       !$OMP END PARALLEL DO

       ds = 1e10 ! some big nr
       if (mod(itsol, 100) == 0) then
          !$xOMP PARALLEL DO                                          &
          !$xOMP PRIVATE(n,nn,L,La,rrn)
          do n = 1, ndx
             if (kfs(n) == 1) then
                rrn = ddr(n) - bbr(n) * s1(n) ! For explicit points db = s0, so this does won't hurt
                do nn = 1, nd(n)%lnx
                   L = nd(n)%ln(nn); La = iabs(L)
                   if (ccr(Lv2(La)) < 0d0) then
                      if (L > 0) then
                         rrn = rrn - ccr(Lv2(La)) * s1(ln(1, La))
                      else
                         rrn = rrn - ccr(Lv2(La)) * s1(ln(2, La))
                      end if
                   end if
                end do
                ds = abs(rrn)
                if (ds > epscg) exit
             end if
          end do
          !$xOMP END PARALLEL DO
       end if

       itsol = itsol + 1
       if (itsol == itmxjac) then
          exit
       end if

    end do
 end subroutine solve_jacobi
