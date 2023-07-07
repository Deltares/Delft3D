!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!    Date:       22 Oct 1992
!    Time:       22:01
!    Program:    LP.FOR
!    Version:    6.00.00
!    Programmer: Nicolaas M de Rooij
!
      subroutine lp(mon)
!           4-69
!           sets up a linear programming problem and calls subroutine
!        simple to find a feasible solution.
!           lp uses subprograms
!              bar, leave, simple
!           lp uses double precision version of
!              alog, amin1, float
      use m_char1

      dimension xx(1), kout(7), cc(1), p(1), jh(1)
      mon = 0
      if (xstart.le.0.0) xstart = 1.e-6
!        clear v4 storage for new column of matrix
      do 10 i=1,m
         p(i) = b(i)
         v4(i) = 0.0
   10 continue
      do 20 j=1,naij
         i = irow(j)
         v4(i) = v4(i) + aij(j)
   20 continue
!        test for space for new column
      if (naij+m.le.maxaij) go to 30
!        no room for new column
      write (not,99999) naij
      call leave(ilp, not)
      return
!        move summation column into matrix area
   30 j = ntot + 1
      do 40 i=1,m
         ii = naij + i
         irow(ii) = i
         jcol(ii) = j
         aij(ii) = v4(i)
   40 continue
      naij = naij + m
      do 50 j=1,ntot
         cc(j) = 0.0
   50 continue
      cc(n+1) = -1.0
!        zero-th simplex is to determine feasibility
      call simple(0, m, n+1, naij, aij, irow, jcol, p, cc, kout, xx,
     1 pie, jh, v3, v4, x3, r)
      zt = xx(n+1)
      if (pf.ge.0) write (not,99998) kout(2), zt, kout(1)
      zzt = dmin1(zt/2.0,xstart)
      k = naij
   60 if (jcol(k).ne.n+1) go to 70
      i = irow(k)
      p(i) = p(i) - zzt*aij(k)
      k = k - 1
      go to 60
!        restore element count to original value
   70 naij = naij - m
      do 80 j=1,ntot
         x(j) = xx(j)
         xmf(j) = 1.0
         x1(j) = 0.0
   80 continue
      if (zt.le.0. .or. kout(1).ne.0) go to 160
!        simplex loop
      fr2 = 1.e+20
      do 130 nn=1,ncomp
         do 90 j=1,ntot
            cc(j) = c(j) + xmf(j) - 1.0
   90    continue
         fn = dble(nn) - 1.0
         call simple(1, m, n, naij, aij, irow, jcol, p, cc, kout, xx,
     1    pie, jh, v3, v4, x3, r)
         if (kout(1).ne.0) go to 240
         do 110 j=1,ntot
            x(j) = xx(j)
            x(j) = (fn*x1(j)+x(j))/(fn+1.0)
            x1(j) = x(j)
  110    continue
         call barx(x, xbar)
         k = 1
         fr = 0.0
         do 120 j=1,n
            if (j.ge.kl(k+1)) k = k + 1
            if (j.eq.kl(k) .and. xbar(k).gt.0.0) fr = fr -
     1       xbar(k)*dlog(xbar(k))
            if (x(j).gt.0.0) fr = fr + x(j)*(dlog(x(j))+c(j))
            xmf(j) = 0.
            if (xbar(k).ne.0.) xmf(j) = x(j)/xbar(k)
  120    continue
         if (pf.ge.0) write (not,99997) nn, kout(2), fr
         if (fr.ge.fr2) go to 140
         fr2 = fr
  130 continue
  140 do 150 j=1,n
         x(j) = x(j) + zzt
  150 continue
      return
  160 if (kout(1).gt.1) go to 240
      write (not,99996)
      do 170 i=1,m
         if (pie(i).ne.0.) write (not,99995) pie(i), nr(i,1), nr(i,2)
  170 continue
      write (not,99994)
      ii = 1
      do 210 k=1,ncomp
         mta = kl(k)
         mtb = kl(k+1) - 1
         do 200 j=mta,mtb
            d = 0.
  180       if (jcol(ii).ne.j) go to 190
            i = irow(ii)
            d = pie(i)*aij(ii) + d
            ii = ii + 1
            go to 180
  190       if (d.ne.0.) write (not,99993) d, kn(j), nam(k,1), nam(k,2)
  200    continue
  210 continue
      d = 0.
      do 220 i=1,m
         d = pie(i)*b(i) + d
  220 continue
      write (not,99992) d
  230 mon = 1
      return
  240 if (kout(1).ne.2) go to 250
!           simplex declared an unbounded solution.  species number is
!        in kout(6).  lp will continue with current solution.  no
!        message output.
      mon = -kout(6)
      write(not,1001)kn(-mon)
1001  format(' simplex declared an unbounded solution for species: ',a6)
      return
  250 write (not,99991)
      go to 230
99999 format (' matrix storage has been exceeded in lp subroutine, wh',
     1 'en new species created for first simplex.  total entries =',i6/
     2 1h )
99998 format (' simplex  0,', i4, ' iterations, max min element=',
     1 1pe15.8, ', condition ', i3)
99997 format (' simplex', i3, 1h,, i4, ' iterations ', ' fr eng=',
     1 1pe15.8)
99996 format (' this problem is infeasible.  the following linear com',
     1 'bination of components'/1x)
99995 format (10x, 3h+ (, f15.8, 5h ) * , 2a6)
99994 format ('   leads to the following infeasible equation,  '/1x)
99993 format (10x, 3h+ (, f15.8, 5h ) * , a6, ' in ', 2a6)
99992 format (' ', 15x, 7h+ 0.0 =, f15.8)
99991 format (' simplex routine has failed due to excessive round-off',
     1 ' error')
      end
