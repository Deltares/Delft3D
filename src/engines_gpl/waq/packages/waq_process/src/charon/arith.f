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
!    Time:       21:30
!    Program:    ARITH.FOR
!    Version:    6.00.00
!    Programmer: Nicolaas M de Rooij
!
      subroutine arith
!           4-69
!           recompute solution values using current x.
!           compute x-bar and mole fraction
!           compute errors and set iopt toggle
!              iopt = 1 for valid solution
!              iopt = 0 for invalid solution
!           compute r and r inverse if marith = 0, skip computation if
!        marith not equal to zero.
!           arith resets marith to zero before returning to the
!        program that called it.
!           arith uses subprograms
!              bar, divide, leave, matinv, phcalc, rcalc
!           arith uses double precision version of
!              abs, exp, sqrt
      use m_char1
      dimension aneg(1), pos(1)
      common /ovflo/ anumbr, anumlg, aquot, iquot
      iopt = 0
      ermb = -0.0
      xemb = -0.0
      nemb = 0
      erma = -0.0
      xema = -0.0
      nema = 0
!        set tolerance tmbal for mass balance error ermb
      tmbal = 3.0e-05
!        set tolerance tmact for mass action error erma
! changed from 3.0e-05 naar 3.0e-03  NmdR, 24-6-92
      tmact = 3.0e-03
      call barx(x, xbar)
      do 10 j=1,n
         k = jcomp(j)
         xmf(j) = x(j)/xbar(k)
   10 continue
!        compute mass balance errors
      do 50 i=1,m
         if (b(i).lt.0.0) go to 40
         pos(i) = 0.0
         aneg(i) = b(i)
         go to 50
   40    pos(i) = -b(i)
         aneg(i) = 0.0
   50 continue
      do 70 k=1,naij
         aaa = aij(k)
         i = irow(k)
         j = jcol(k)
         if (aaa.lt.0.0) go to 60
         pos(i) = pos(i) + aaa*x(j)
         go to 70
   60    aneg(i) = aneg(i) - aaa*x(j)
   70 continue
      do 100 i=1,m
         if (pos(i).eq.0.0 .and. aneg(i).eq.0.0) go to 100
         xembb = dabs(divide((pos(i)-aneg(i)),pos(i)))
         if (xembb.lt.xemb) go to 80
         xemb = xembb
         nemb = i
   80    if (iquot.eq.0) go to 90
         ermb = aquot
         go to 110
   90    ermb = ermb + xembb**2
  100 continue
      ermb = dsqrt(ermb/dble(m))
!        compute mass action errors
  110 k = 1
      do 130 j=1,n
         if (jcol(k).ne.j) go to 130
         pda = -c(j)
  120    i = irow(k)
         pda = pda + pie(i)*aij(k)
         k = k + 1
         if (jcol(k).eq.j .and. k.le.naij) go to 120
         xeq = dexp2(pda)
         if((xeq.eq.0.0 .or. xmf(j).eq.0.0 ).and.dabs(xeq-xmf(j)).lt.
     2   1.0d-60) go to 130
         if(xeq.gt.0.990d+00 .and. dabs(xeq-xmf(j)) .lt. 0.10d+00
     2    .and. x(j) .lt.  slacks/1.0d-10) go to 130
         xemaa = 2.0*dabs((xeq-xmf(j))/(xeq+xmf(j)))
         erma = erma + xemaa**2
         if (xemaa.lt.xema) go to 130
         xema = xemaa
         nema = j
  130 continue
      erma = dsqrt(erma/dble(n))
!        if errors within tolerance set toggle for valid solution
      if (ermb.lt.tmbal .and. erma.lt.tmact) iopt = 1
!        if marith not zero skip r calculation
      if (marith.ne.0) go to 140
!        compute free energy
      fe = 0.0
      do 20 i=1,m
         fe = fe + pie(i)*b(i)
   20 continue
      fe2 = rt*fe
!        compute ph and eh and ceccal
! already done in solvex 7-11-1992 NMdR
!        recompute r and r inverse
      call rcalc
      call matinv(r, mend, v4, 0, v1, v2, ke)
      if (ke.ne.0) go to 150
!        set marith = 0 before return
  140 marith = 0
      return
  150 if (ke.gt.m) go to 160
      write (not,99999) nr(ke,1), nr(ke,2)
      go to 170
  160 ke = ke - m
      write (not,99998) nam(ke,1), nam(ke,2)
  170 if(insolv .ne. 1) call leave(iarith, not)
      go to 140
99999 format (29h0r matrix has nullity on row , 2a6, 15h  (message prin,
     1 25hted by subroutine arith) /1h )
99998 format (45h0r matrix has nullity on row for compartment , 2a6,
     1 40h  (message printed by subroutine arith) /1h )
      end
