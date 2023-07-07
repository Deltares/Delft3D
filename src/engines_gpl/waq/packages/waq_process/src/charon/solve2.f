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
!    Program:    SOLVE2.FOR
!    Version:    6.00.00
!    Programmer: Nicolaas M de Rooij
!
      subroutine solve2(kkk)
!           finds an initial feasible solution, does the first and
!        second order methods to obtain an optimal solution.
!           normal termination sets ierror = 1
!           solve uses subprograms
!              bar, berror, xlog, del, lp, matinx, rcalc
!           solve uses double precision version of
!              abs, amax1, amin1, exp, float, sqrt
      use m_char1
      dimension dx(maxn), alpha(maxn), th(maxn), g(maxmd)
      logical bsec
      data ittt/1/
      bsec=.false.
      bready = .false.
!***********************************************************************
!    note ijfind(1) is set to -1 in: matrix, intera, delete
!    if a change or install of coeffficients did occur.
!***********************************************************************
!***********************************************************************
!          store entry locations in ijfind, rcalc uses this array
!          instead of calling find each time (n.m.de rooij 7/11/79)
!***********************************************************************
      if(ijfind(1) .ge. 0) go to 9
      jj=2
      ijfind(1)=1
      do 8 ii=1,naij
      if(jcol(ii).ne.jj) go to 8
      ijfind(jj)=ii
      jj=jj+1
8     continue
      ijfind(jj)=naij+1
      kkk = 1
9     if(ittt.gt.1) go to 15
      ittt = 2
      iopt = 0
      mend = m + ncomp
      ierror = 0
      if (m.gt.maxm .or. mend.gt.maxmd .or. n.gt.maxn .or.
     1 ncomp.gt.maxp) go to 420
      if (tol1.le.0.0) tol1 = .01
      if (tol2.le.0.0) tol2 = 1.0e-5
!        values for xmin, barmin, and slacks are set in
!        subroutine start
      if (itmax.le.0) itmax = 40
      if (pf.gt.0) write (not,99999)
      do 10 j=1,ntot
         if (x(j).lt.0.0d+00) go to 70
   10 continue
!        if x is strictly positive,  begin projection
!                 try projection up to 5 times
!
!        no projection method used any more ( n.m. de rooij 20/5/80)
!        go directly to second order method
!        try second order method up to 15 times, err must be lesser
!        than  1.0 d-4.
!******
15    bsec=.true.
      iter=0
      err = 0.0
      if(kkk .eq. 0) go to 280
      if(kkk .eq. 2) go to 70
!******
      kkk = 1
      size0 = 1.0e+20
      if(ijohn .ge. 1)call scrgen('Solve ','projec',0,25,60)
      do 60 nl=1,15
         call barx(x, xbar)
         call berror(err)
         call rcalc
         call matinv(r, mend, g, -1, v2, v3, ke)
         if (ke.ne.0) go to 70
         call del(dx, g)
         size = 0.
         do 40 k=1,ncomp
            kta = kl(k)
            ktb = kl(k+1) - 1
            mk = m + k
            do 30 j=kta,ktb
               dx(j) = dx(j) + g(mk)
               size = dmax1(size,-dx(j))
   30       continue
   40    continue
         scale = 1.
         if (size.gt.0.875) scale = .875/size
         if (nl.eq.5 .and. size.lt.1.) scale = 1.
         if (pf.gt.0) write (not,99998) nl, size, scale
         if ( (nl.ge.3 .and. scale.ne.1. .and.
     2    (size0- size)/size0 .lt. 0.15) .or.
     3    (nl .ge. 3 .and. size .gt. 20.) ) go to 70
         size0 = size
         do 50 j=1,n
            x(j) = x(j)*(1.+scale*dx(j))
   50    continue
         if (scale.eq.1.) go to 80
   60 continue
!        linear programming routine
!******
   70 bsec=.false.
      kkk = 2
!******
      if(ijohn .ge. 1)call scrgen('Solve ','lin pr',0,25,60)
      call lp(kf)
      if (kf.ne.0) go to 410
   80 call barx(x, xbar)
      call xlog(x, xbar)
      fe2 = 1.e+20
!        first order method loop
      do 240 iter=1,itmax
         call berror(err)
         do 90 i=1,mend
            pie(i) = 0.
   90    continue
         ii = 1
         do 120 k=1,ncomp
            kta = kl(k)
            ktb = kl(k+1) - 1
            mk = m + k
            do 110 j=kta,ktb
               ax = alpha(j)*x(j)
               pie(mk) = pie(mk) + ax
  100          if (jcol(ii).ne.j) go to 110
               i = irow(ii)
               pie(i) = pie(i) + ax*aij(ii)
               ii = ii + 1
               go to 100
  110       continue
  120    continue
         do 130 i=1,m
            pie(i) = g(i) + pie(i)
  130    continue
         call rcalc
         call matinv(r, mend, pie, -1, v2, v3, ke)
         if (ke.ne.0) go to 380
         dmax = 1.e+20
         call del(th, pie)
         gnorm = 0.
         ideg = 0
         tda = 0.
         fe = 0.
         do 160 k=1,ncomp
            mk = m + k
            kta = kl(k)
            ktb = kl(k+1) - 1
            do 150 j=kta,ktb
               th(j) = th(j) + pie(mk) - alpha(j)
               gnorm = gnorm + th(j)**2
               th(j) = th(j)*x(j)
               if (x(j).gt.xmin) go to 140
               ideg = ideg + 1
               th(j) = dmax1(th(j),0.0d00)
  140          tda = tda + th(j)*alpha(j)
               if (x(j).lt.(-dmax)*th(j)) dmax = divide(-x(j),th(j))
               fe = fe + x(j)*alpha(j)
  150       continue
  160    continue
         eps = dsqrt(gnorm/dble(ntot))
         dfe = fe - fe2
         fe2 = fe
         if (iter.eq.1) go to 170
         itr = iter - 1

         if (pf.gt.0) write (not,99997)itr,dfe,eps,err,tda,ideg
170      if(ijohn .ge. 1) call scrgen('Solve ','meth 1',iter,25,60)
         optl = dmin1(1.0d00,0.99*dmax)
         if (eps.le.tol1) go to 260
         if (tda.ge.0.) go to 250
         do 200 ii=1,54
            do 180 j=1,n
               dx(j) = dmax1(x(j)+optl*th(j),xmin)
  180       continue
            call barx(dx, xbar)
            call xlog(dx, xbar)
            tda = 0.
            do 190 j=1,ntot
               tda = tda + th(j)*alpha(j)
  190       continue
            if (tda.lt.0.) go to 210
            optl = optl/1.4142
  200    continue
         call barx(x, xbar)
         go to 270
  210    do 220 j=1,ntot
            x(j) = dx(j)
  220    continue
         fe = 0.
         do 230 j=1,n
            fe = fe + alpha(j)*x(j)
  230    continue
  240 continue
      go to 370
!        end of first order method loop
  250 if (pf.gt.0) write (not,99994) iter
      go to 280
  260 if (pf.gt.0) write (not,99993) iter
      go to 280
  270 if (pf.gt.0) write (not,99992) iter
!        second order method loop
  280 iter1 = iter + 1
      pmax = 1.e+20
      pmax1 = 1.e+21
      pmax2 = 1.e+22
!*****
      itt=0
!*****
      do 340 iter=iter1,itmax
!*****
       itt=itt+1
         call del(dx, pie)
         do 300 k=1,ncomp
            mta = kl(k)
            mtb = kl(k+1) - 1
            do 290 j=mta,mtb
               xmf(j) = dexp2(dx(j)-c(j))
               x(j) = xmf(j)*xbar(k)
  290       continue
            if (xbar(k).le.barmin ) go to 400
  300    continue
!        test for end of second order method
         aa=pmax
         if(bsec)aa=dmax1(err,pmax)
         if (aa.lt.tol2 .and. dabs(err) .lt. tol1) go
     1    to 350
         if (pmax.ge.pmax1 .and. pmax.ge.pmax2) go
     1    to 350
         call berror(err)
         call rcalc
         call matinv(r, mend, g, -1, v2, v3, ke)
         if (ke.ne.0) go to 380
         pmax2 = pmax1
         pmax1 = pmax
         pmax = 0.
         do 310 i=1,mend
            pmax = dmax1(pmax,dabs(g(i)))
  310    continue
         if (pmax.eq.0.0) go to 350
         zm = dmin1(1.0d0/pmax,1.0d0)
         do 320 i=1,m
            pie(i) = pie(i) + zm*g(i)
  320    continue
         do 330 k=1,ncomp
            mk = m + k
            xbar(k) = xbar(k)*dexp2(zm*g(mk))
  330    continue
         if (pf.gt.0) write (not,99991) iter, pmax, err
         if(ijohn .ge. 1) call scrgen('Solve ','meth 2',itt,25,60)
  340 continue
      go to 370
!        end of second order method loop
!******
350      if (pf.eq.0) write (not,99991) iter, pmax, err
!*******
!  calculate mass balance mass action errors
!*******
      if(ijohn .ge. 1)call scrgen('Solve ','contrl',iter,25,60)
      marith = 1
      call arith
      if(iopt .eq. 1) go to 355
      if(pf.gt.0) write(not,1235)kn(nema),xema,nr(nemb,1),xemb
 1235 format(' solve, mass action error: ',a6,e14.5,/,
     2       '       mass balance error: ',a6,e14.5)
!******
      ierror = 8
      go to 360
355   ierror = 1
      if (.not.bactiv) go to 360
      if (bready) go to 360
      if(ijohn .ge. 1)call scrgen('Solve ','actcon',0,25,60)
      call actcon
      kkk   = 1
      if(pf.gt.0)write(not,99990)str
!        carry out new computation with corrected c values.
      go to 15
!******
!     go to 280
!******
 360  return
  370 ierror = 2
      write (not,99989)
      iter=itmax
      go to 360
  380 ierror = 3
      if (ke.gt.m) go to 390
      write (not,99988) nr(ke,1), nr(ke,2)
      go to 360
  390 ierror = 4
      ke = ke - m
      write (not,99987) nam(ke,1), nam(ke,2)
      go to 360
  400 if(bsec) go to 70
      ierror = 5
      write (not,99986) nam(k,1), nam(k,2)
      lastcp = k
      go to 360
  410 ierror = 6
      if(kf .le. 0) ierror = kf
      go to 360
  420 write (not,99985) maxm, maxn, maxp, maxaij, maxmd, m, n, ncomp,
     1 naij, mend
      ierror = 7
      call chexit
99999 format (' ')
99998 format (' projection', i3, ' size', f12.2, ',  scale', f12.2)
99997 format (' ',i3,' dfe=',e11.4,
     2 ' av th=',e10.3,' row err=',e10.3,' tda=',e10.3,' ideg=',i3)
99994 format (' iter', i4, ' positive tda, go to method 2 ')
99993 format (' iter', i4, ' av theta less than tol(1), go to m',
     1 'ethod 2')
99992 format (' iter', i4, ' step size too small, go to method 2')
99991 format (' iter', i4, ' dpie=',1pe15.8,
     1 ' row err=',e10.3)
99990 format (' new computation, ionic strength change ', e12.4)
99989 format (' iteration limit exceeded  ')
99988 format (' r matrix has nullity on component ', 2a6,/,
     1 '      (message printed by subroutine solve) '/1h )
99987 format (' r matrix has nullity on component  for phase ', 2a6,/,
     1 '      (message printed by subroutine solve) '/1h )
99986 format (' phase  ', 2a6, ' too small')
99985 format (' *** program has exceeded one or more of the limits of',
     1 /,i11, ' components,', i5, ' species,', i4, ' phases,',/,
     2 i15, ' non zero matrix entries,', i4, ' components plus phases'/,
     3 4x,'current problem has',/,8x,i3,' components,', i5, ' species,',
     4 i4, ' phases,',/, i5, ' non zero matrix entries,',
     5 i4, ' components plus phases',/,' solve cannot continue.'/' ' )
      end
