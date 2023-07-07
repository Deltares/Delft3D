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

!    Date:       1 Oct 1992
!    Time:       12:03
!    Program:    SIMPLE.FOR
!    Version:    6.00.00
!    Programmer: Nicolaas M de Rooij
!
      subroutine simple(inflag, mx, nn, nct, a, iro, jc, b, c, ko, zb,
     1 p, jh, x, y, pe, e)
!               4-69
!        automatic simplex       redundant equations cause infeasibility
!               zb(j) replaces kb(j) of earlier versions.  the index for
!              position in basis is a floating point number.
!               simple uses subprogram
!                  find
!               simple uses double precision version of
!                  abs, amax1
      implicit real*8 (a-h,o-z)
      equivalence (xx,ll)
      dimension b(1), c(1), p(1), x(1), y(1), pe(1), e(1)
      dimension zb(1)
!        real aa,aijt,bb,cost,dt,rcost,texp,tpiv,ty,xold,xx,xy,yi,ymax
      integer inflag, mx, nn, ko(6), jh(1)
      integer i, ia, invc, ir, iter, j, jt, k, kbj, l, ll, m, m2, mm, n
      integer ncut, numvr, nver, numpv
      logical feas, trig, ver, kq
!        matrix is single dimensioned, with row and column numbers
!        in separate arrays
      real*8 a(1)
      integer iro(1), jc(1)
!        set initial values, set constant values
      iter = 0
      numvr = 0
      numpv = 0
      m = mx
      n = nn
      texp = .5**16
      nver = m/2 + 5
      ncut = 4*m + 10
      m2 = m**2
      feas = .false.
      if (inflag.ne.0) go to 30
!        *          start  phase one with singleton basis
      j = 0
      ko(1) = 0
      do 20 ii=1,nct
         jj = jc(ii)
         if (jj.eq.j) go to 10
         if (ko(1).eq.1 .and. ko(2).eq.0) zb(j) = 1.0
         j = jj
         zb(j) = 0.0
         ko(1) = 0
         ko(2) = 0
   10    if (a(ii).gt.0.0) ko(1) = ko(1) + 1
         if (a(ii).lt.0.0) ko(2) = 1
   20 continue
      if (ko(1).eq.1 .and. ko(2).eq.0) zb(j) = 1.0
   30 do 40 i=1,m
         jh(i) = -1
   40 continue
!        create inverse from 'zb' and 'jh'     (step 7)
   50 ver = .true.
      invc = 0
      numvr = numvr + 1
      trig = .false.
      do 60 i=1,m2
         e(i) = 0.0
   60 continue
      mm = 1
      do 70 i=1,m
         e(mm) = 1.0
         pe(i) = 0.0
         x(i) = b(i)
         if (jh(i).ne.0) jh(i) = -1
         mm = mm + m + 1
   70 continue
      jt = 1
   80 if (zb(jt).eq.0.0) go to 130
      go to 290
!        get column(jt)
   90 ty = tpiv
      ir = 0
      do 100 i=1,m
         absyi = dabs(y(i))
         if (jh(i).ne.(-1) .or. x(i).ne.0.0 .or. absyi.le.ty) go to 100
         ty = absyi
         ir = i
  100 continue
      if (ir.ne.0) go to 120
      ty = 0.
      do 110 i=1,m
         absyi = dabs(y(i))
         absxi = dabs(x(i))
         if (jh(i).ne.(-1) .or. x(i).eq.0.0 .or. absyi.le.tpiv) go to
     1    110
         if (absyi.le.ty*absxi) go to 110
         ty = absyi/absxi
         ir = i
  110 continue
  120 zb(jt) = 0.0
      if (ir.eq.0) go to 130
      go to 400
!        pivot(ir,jt)
  130 jt = jt + 1
      if (jt.le.n) go to 80
      do 140 i=1,m
         if (jh(i).eq.(-1)) jh(i) = 0
         if (jh(i).eq.0) feas = .false.
  140 continue
!        *          determine feasibility                 (step 1)
  150 ver = .false.
      if (feas) go to 170
      do 160 i=1,m
         if (x(i).lt.0.0 .or. jh(i).eq.0) go to 190
  160 continue
      feas = .true.
!        *          get applicable prices                 (step 2)
  170 do 180 i=1,m
         p(i) = pe(i)
         if (x(i).lt.0.) x(i) = 0.
  180 continue
      kq = .false.
      go to 250
  190 feas = .false.
      do 200 j=1,m
         p(j) = 0.
  200 continue
      kq = .true.
      do 240 i=1,m
         mm = i
         if (x(i).ge.0.0) go to 220
         kq = .false.
         do 210 j=1,m
            p(j) = p(j) + e(mm)
            mm = mm + m
  210    continue
         go to 240
  220    if (jh(i).ne.0) go to 240
         kq = kq .and. x(i).eq.0.
         do 230 j=1,m
            p(j) = p(j) - e(mm)
            mm = mm + m
  230    continue
  240 continue
!        *          find minimum reduced cost             (step 3)
  250 jt = 0
      bb = 0.0
      jj = 0
      do 280 ii=1,nct
         j = jc(ii)
         if (j.eq.jj) go to 260
         if (zb(j).ne.0.0) go to 280
         jj = j
         dt = 0.0
  260    i = iro(ii)
         dt = dt + p(i)*a(ii)
         if (ii.eq.nct) go to 270
         if (jc(ii+1).eq.jj) go to 280
  270    if (feas) dt = dt + c(j)
         if (kq) dt = -dabs(dt)
         if (dt.ge.bb) go to 280
         bb = dt
         jt = j
  280 continue
      if (jt.le.0) go to 470
      if (iter.ge.ncut) go to 460
      iter = iter + 1
!        *          multiply inverse times a(.,jt)        (step 4)
!             begin subroutine get column(jt)
  290 do 300 i=1,m
         y(i) = 0.0
  300 continue
      ll = 0
      cost = c(jt)
      do 330 i=1,m
         call find(i, jt, ii, iflag)
         if (iflag.ne.0) go to 320
         aijt = a(ii)
         cost = cost + aijt*pe(i)
         do 310 j=1,m
            ll = ll + 1
            y(j) = y(j) + aijt*e(ll)
  310    continue
         go to 330
  320    ll = ll + m
  330 continue
      ymax = 0.0
      do 340 i=1,m
         ymax = dmax1(dabs(y(i)),ymax)
  340 continue
      tpiv = ymax*texp
!        end of get column
      if (ver) go to 90
      rcost = ymax/bb
      if (trig .and. bb.ge.(-tpiv)) go to 470
      trig = bb.ge.(-tpiv)
!        *          select pivot row                      (step 5)
      aa = tpiv
      ir = 0
      if (feas) go to 360
      do 350 i=1,m
         absyi = dabs(y(i))
         if (jh(i).ne.0 .or. x(i).ne.0.0 .or. absyi.le.aa) go to 350
         aa = absyi
         ir = i
  350 continue
      if (ir.ne.0) go to 390
  360 do 370 i=1,m
         if (x(i).ne.0. .or. y(i).le.aa) go to 370
         aa = y(i)
         ir = i
  370 continue
      if (ir.ne.0) go to 390
      aa = 0.
      do 380 i=1,m
         if (x(i).eq.0. .or. dabs(y(i)).le.tpiv) go to 380
         if (y(i)/x(i).le.aa) go to 380
         aa = y(i)/x(i)
         ir = i
  380 continue
      if (ir.eq.0) go to 450
!        *          pivot on (ir,jt)                      (step 6)
  390 ia = jh(ir)
      if (ia.gt.0) zb(ia) = 0.0
!        begin subroutine pivot(ir,jt)
  400 numpv = numpv + 1
      jh(ir) = jt
      zb(jt) = ir
      yi = -y(ir)
      y(ir) = -1.0
      ll = 0
      do 430 j=1,m
         l = ll + ir
         if (e(l).ne.0.0) go to 410
         ll = ll + m
         go to 430
  410    xy = e(l)/yi
         pe(j) = pe(j) + cost*xy
         e(l) = 0.0
         do 420 i=1,m
            ll = ll + 1
            e(ll) = e(ll) + xy*y(i)
  420    continue
  430 continue
      xy = x(ir)/yi
      do 440 i=1,m
         xold = x(i)
         x(i) = xold + xy*y(i)
         if (.not.ver .and. x(i).lt.0. .and. xold.ge.0.) x(i) = 0.
  440 continue
      y(ir) = -yi
      x(ir) = -xy
!        end of pivot
      if (ver) go to 130
!        to step 1 if not inverting, to step 7 if inverting
      if (numpv.le.m) go to 150
      invc = invc + 1
      if (invc.eq.nver) go to 50
      go to 150
!        *  end of algorithm, set exit values            ***
  450 if (.not.feas .or. rcost.le.(-1000.0)) go to 470
!        infinite solution
      k = 2
      go to 480
!        problem is cycling perhaps
  460 k = 4
      go to 480
!        feasible or infeasible solution
  470 k = 0
  480 if (.not.feas) k = k + 1
      do 490 j=1,n
         kbj = zb(j)
!        store final solution values in zb
         if (kbj.eq.0) zb(j) = 0.0
         if (kbj.ne.0) zb(j) = x(kbj)
  490 continue
      ko(1) = k
      ko(2) = iter
      ko(3) = invc
      ko(4) = numvr
      ko(5) = numpv
      ko(6) = jt
      return
      end
