!
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!     *                                                               *
!     *                  copyright (c) 1998-2024 by UCAR                   *
!     *                                                               *
!     *       University Corporation for Atmospheric Research         *
!     *                                                               *
!     *                      all rights reserved                      *
!     *                                                               *
!     *                      SPHEREPACK version 3.2                   *
!     *                                                               *
!     *       A Package of Fortran77 Subroutines and Programs         *
!     *                                                               *
!     *              for Modeling Geophysical Processes               *
!     *                                                               *
!     *                             by                                *
!     *                                                               *
!     *                  John Adams and Paul Swarztrauber             *
!     *                                                               *
!     *                             of                                *
!     *                                                               *
!     *         the National Center for Atmospheric Research          *
!     *                                                               *
!     *                Boulder, Colorado  (80307)  U.S.A.             *
!     *                                                               *
!     *                   which is sponsored by                       *
!     *                                                               *
!     *              the National Science Foundation                  *
!     *                                                               *
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
!
! ... file sphcom.f
!
!     this file must be loaded with all main program files
!     in spherepack.  it includes undocumented subroutines
!     called by some or all of main programs
!
subroutine dnlfk (m,n,cp)
!
!     cp requires n/2+1 double precision locations
!
   integer, intent(in) :: m, n
   double precision, intent(out) :: cp(1)
   double precision :: fnum,fden,fnmh,a1,b1,c1,cp2,fnnp1,fnmsq,fk,&
   &t1,t2,pm1,sc10,sc20,sc40
   parameter (sc10=1024.d0)
   parameter (sc20=sc10*sc10)
   parameter (sc40=sc20*sc20)
   integer :: ma, nmms2, i, l
!
   cp(1) = 0.
   ma = iabs(m)
   if(ma .gt. n) return
   if(n-1) 2,3,5
2  cp(1) = dsqrt(2.d0)
   return
3  if(ma .ne. 0) go to 4
   cp(1) = dsqrt(1.5d0)
   return
4  cp(1) = dsqrt(.75d0)
   if(m .eq. -1) cp(1) = -cp(1)
   return
5  if(mod(n+ma,2) .ne. 0) go to 10
   nmms2 = (n-ma)/2
   fnum = n+ma+1
   fnmh = n-ma+1
   pm1 = 1.d0
   go to 15
10 nmms2 = (n-ma-1)/2
   fnum = n+ma+2
   fnmh = n-ma+2
   pm1 = -1.d0
!      t1 = 1.
!      t1 = 2.d0**(n-1)
!      t1 = 1.d0/t1
15 t1 = 1.d0/sc20
   nex = 20
   fden = 2.d0
   if(nmms2 .lt. 1) go to 20
   do 18 i=1,nmms2
      t1 = fnum*t1/fden
      if(t1 .gt. sc20) then
         t1 = t1/sc40
         nex = nex+40
      end if
      fnum = fnum+2.
      fden = fden+2.
18 continue
20 t1 = t1/2.d0**(n-1-nex)
   if(mod(ma/2,2) .ne. 0) t1 = -t1
   t2 = 1.
   if(ma .eq. 0) go to 26
   do 25 i=1,ma
      t2 = fnmh*t2/(fnmh+pm1)
      fnmh = fnmh+2.
25 continue
26 cp2 = t1*dsqrt((n+.5d0)*t2)
   fnnp1 = n*(n+1)
   fnmsq = fnnp1-2.d0*ma*ma
   l = (n+1)/2
   if(mod(n,2) .eq. 0 .and. mod(ma,2) .eq. 0) l = l+1
   cp(l) = cp2
   if(m .ge. 0) go to 29
   if(mod(ma,2) .ne. 0) cp(l) = -cp(l)
29 if(l .le. 1) return
   fk = n
   a1 = (fk-2.)*(fk-1.)-fnnp1
   b1 = 2.*(fk*fk-fnmsq)
   cp(l-1) = b1*cp(l)/a1
30 l = l-1
   if(l .le. 1) return
   fk = fk-2.
   a1 = (fk-2.)*(fk-1.)-fnnp1
   b1 = -2.*(fk*fk-fnmsq)
   c1 = (fk+1.)*(fk+2.)-fnnp1
   cp(l-1) = -(b1*cp(l)+c1*cp(l+1))/a1
   go to 30
end
subroutine dnlft (m,n,theta,cp,pb)
   double precision cp(*),pb,theta,cdt,sdt,cth,sth,chh
   cdt = dcos(theta+theta)
   sdt = dsin(theta+theta)
   nmod=mod(n,2)
   mmod=mod(m,2)
   if(nmod)1,1,2
1  if(mmod)3,3,4
!
!     n even, m even
!
3  kdo=n/2
   pb = .5*cp(1)
   if(n .eq. 0) return
   cth = cdt
   sth = sdt
   do 170 k=1,kdo
!     pb = pb+cp(k+1)*dcos(2*k*theta)
      pb = pb+cp(k+1)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
170 continue
   return
!
!     n even, m odd
!
4  kdo = n/2
   pb = 0.
   cth = cdt
   sth = sdt
   do 180 k=1,kdo
!     pb = pb+cp(k)*dsin(2*k*theta)
      pb = pb+cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
180 continue
   return
2  if(mmod)13,13,14
!
!     n odd, m even
!
13 kdo = (n+1)/2
   pb = 0.
   cth = dcos(theta)
   sth = dsin(theta)
   do 190 k=1,kdo
!     pb = pb+cp(k)*dcos((2*k-1)*theta)
      pb = pb+cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
190 continue
   return
!
!     n odd, m odd
!
14 kdo = (n+1)/2
   pb = 0.
   cth = dcos(theta)
   sth = dsin(theta)
   do 200 k=1,kdo
!     pb = pb+cp(k)*dsin((2*k-1)*theta)
      pb = pb+cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
200 continue
   return
end
subroutine dnlftd (m,n,theta,cp,pb)
!
!     computes the derivative of pmn(theta) with respect to theta
!
   dimension cp(1)
   double precision cp,pb,theta,cdt,sdt,cth,sth,chh
   cdt = dcos(theta+theta)
   sdt = dsin(theta+theta)
   nmod=mod(n,2)
   mmod=mod(abs(m),2)
   if(nmod)1,1,2
1  if(mmod)3,3,4
!
!     n even, m even
!
3  kdo=n/2
   pb = 0.d0
   if(n .eq. 0) return
   cth = cdt
   sth = sdt
   do 170 k=1,kdo
!     pb = pb+cp(k+1)*dcos(2*k*theta)
      pb = pb-2.d0*k*cp(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
170 continue
   return
!
!     n even, m odd
!
4  kdo = n/2
   pb = 0.
   cth = cdt
   sth = sdt
   do 180 k=1,kdo
!     pb = pb+cp(k)*dsin(2*k*theta)
      pb = pb+2.d0*k*cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
180 continue
   return
2  if(mmod)13,13,14
!
!     n odd, m even
!
13 kdo = (n+1)/2
   pb = 0.
   cth = dcos(theta)
   sth = dsin(theta)
   do 190 k=1,kdo
!     pb = pb+cp(k)*dcos((2*k-1)*theta)
      pb = pb-(2.d0*k-1)*cp(k)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
190 continue
   return
!
!     n odd, m odd
!
14 kdo = (n+1)/2
   pb = 0.
   cth = dcos(theta)
   sth = dsin(theta)
   do 200 k=1,kdo
!     pb = pb+cp(k)*dsin((2*k-1)*theta)
      pb = pb+(2.d0*k-1)*cp(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
200 continue
   return
end
subroutine legin(mode,l,nlat,m,w,pmn,km)
!     this subroutine computes legendre polynomials for n=m,...,l-1
!     and  i=1,...,late (late=((nlat+mod(nlat,2))/2)gaussian grid
!     in pmn(n+1,i,km) using swarztrauber's recursion formula.
!     the vector w contains quantities precomputed in shigc.
!     legin must be called in the order m=0,1,...,l-1
!     (e.g., if m=10 is sought it must be preceded by calls with
!     m=0,1,2,...,9 in that order)
   dimension w(1),pmn(1)
!     set size of pole to equator gaussian grid
   late = (nlat+mod(nlat,2))/2
!     partition w (set pointers for p0n,p1n,abel,bbel,cbel,pmn)
   i1 = 1+nlat
   i2 = i1+nlat*late
   i3 = i2+nlat*late
   i4 = i3+(2*nlat-l)*(l-1)/2
   i5 = i4+(2*nlat-l)*(l-1)/2
   call legin1(mode,l,nlat,late,m,w(i1),w(i2),w(i3),w(i4),&
   &w(i5),pmn,km)
   return
end
subroutine legin1(mode,l,nlat,late,m,p0n,p1n,abel,bbel,cbel,&
&pmn,km)

   dimension p0n(nlat,late),p1n(nlat,late)
   dimension abel(1),bbel(1),cbel(1),pmn(nlat,late,3)
   data km0,km1,km2/ 1,2,3/
   save km0,km1,km2
!     define index function used in storing triangular
!     arrays for recursion coefficients (functions of (m,n))
!     for 2.le.m.le.n-1 and 2.le.n.le.l-1
   indx(m,n) = (n-1)*(n-2)/2+m-1
!     for l.le.n.le.nlat and 2.le.m.le.l
   imndx(m,n) = l*(l-1)/2+(n-l-1)*(l-1)+m-1

!     set do loop indices for full or half sphere
   ms = m+1
   ninc = 1
   if (mode.eq.1) then
!     only compute pmn for n-m odd
      ms = m+2
      ninc = 2
   else if (mode.eq.2) then
!     only compute pmn for n-m even
      ms = m+1
      ninc = 2
   end if


   if (m.gt.1) then
      do 100 np1=ms,nlat,ninc
         n = np1-1
         imn = indx(m,n)
         if (n.ge.l) imn = imndx(m,n)
         do 100 i=1,late
            pmn(np1,i,km0) = abel(imn)*pmn(n-1,i,km2)&
            &+bbel(imn)*pmn(n-1,i,km0)&
            &-cbel(imn)*pmn(np1,i,km2)
100   continue

   else if (m.eq.0) then
      do 101 np1=ms,nlat,ninc
         do 101 i=1,late
            pmn(np1,i,km0) = p0n(np1,i)
101   continue

   else if (m.eq.1) then
      do 102 np1=ms,nlat,ninc
         do 102 i=1,late
            pmn(np1,i,km0) = p1n(np1,i)
102   continue
   end if

!     permute column indices
!     km0,km1,km2 store m,m-1,m-2 columns
   kmt = km0
   km0 = km2
   km2 = km1
   km1 = kmt
!     set current m index in output param km
   km = kmt
   return
end


subroutine zfin (isym,nlat,nlon,m,z,i3,wzfin)
   double precision z, wzfin
   dimension       z(1)        ,wzfin(1)
   imid = (nlat+1)/2
   lim = nlat*imid
   mmax = min0(nlat,nlon/2+1)
   labc = ((mmax-2)*(nlat+nlat-mmax-1))/2
   iw1 = lim+1
   iw2 = iw1+lim
   iw3 = iw2+labc
   iw4 = iw3+labc
!
!     the length of wzfin is 2*lim+3*labc
!
   call zfin1 (isym,nlat,m,z,imid,i3,wzfin,wzfin(iw1),wzfin(iw2),&
   &wzfin(iw3),wzfin(iw4))
   return
end
subroutine zfin1 (isym,nlat,m,z,imid,i3,zz,z1,a,b,c)
   double precision z,zz,z1,a,b,c
   dimension       z(imid,nlat,3),zz(imid,1),z1(imid,1),&
   &a(1),b(1),c(1)
   save i1,i2
   ihold = i1
   i1 = i2
   i2 = i3
   i3 = ihold
   if(m-1)25,30,35
25 i1 = 1
   i2 = 2
   i3 = 3
   do 45 np1=1,nlat
      do 45 i=1,imid
         z(i,np1,i3) = zz(i,np1)
45 continue
   return
30 do 50 np1=2,nlat
      do 50 i=1,imid
         z(i,np1,i3) = z1(i,np1)
50 continue
   return
35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
   if(isym .eq. 1) go to 36
   do 85 i=1,imid
      z(i,m+1,i3) = a(ns)*z(i,m-1,i1)-c(ns)*z(i,m+1,i1)
85 continue
36 if(m .eq. nlat-1) return
   if(isym .eq. 2) go to 71
   ns = ns+1
   do 70 i=1,imid
      z(i,m+2,i3) = a(ns)*z(i,m,i1)-c(ns)*z(i,m+2,i1)
70 continue
71 nstrt = m+3
   if(isym .eq. 1) nstrt = m+4
   if(nstrt .gt. nlat) go to 80
   nstp = 2
   if(isym .eq. 0) nstp = 1
   do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
         z(i,np1,i3) = a(ns)*z(i,np1-2,i1)+b(ns)*z(i,np1-2,i3)&
         &-c(ns)*z(i,np1,i1)
75 continue
80 return
end
subroutine zfinit (nlat,nlon,wzfin,dwork)
   double precision wzfin
   dimension       wzfin(*)
   double precision dwork(*)
   imid = (nlat+1)/2
   iw1 = 2*nlat*imid+1
!
!     the length of wzfin is 3*((l-3)*l+2)/2 + 2*l*imid
!     the length of dwork is nlat+2
!
   call zfini1 (nlat,nlon,imid,wzfin,wzfin(iw1),dwork,&
   &dwork(nlat/2+1))
   return
end
subroutine zfini1 (nlat,nlon,imid,z,abc,cz,work)
!
!     abc must have 3*((mmax-2)*(nlat+nlat-mmax-1))/2 locations
!     where mmax = min0(nlat,nlon/2+1)
!     cz and work must each have nlat+1 locations
!
   double precision z,abc
   dimension z(imid,nlat,2),abc(1)
   double precision pi,dt,th,zh,cz(*),work(*)
   pi = 4.*datan(1.d0)
   dt = pi/(nlat-1)
   do 160 mp1=1,2
      m = mp1-1
      do 160 np1=mp1,nlat
         n = np1-1
         call dnzfk(nlat,m,n,cz,work)
         do 165 i=1,imid
            th = (i-1)*dt
            call dnzft(nlat,m,n,th,cz,zh)
            z(i,np1,mp1) = zh
165      continue
         z(1,np1,mp1) = .5*z(1,np1,mp1)
160 continue
   call rabcp(nlat,nlon,abc)
   return
end
subroutine dnzfk(nlat,m,n,cz,work)
!
!     dnzfk computes the coefficients in the trigonometric
!     expansion of the z functions that are used in spherical
!     harmonic analysis.
!
   dimension  cz(1),work(1)
!
!     cz and work must both have nlat/2+1 locations
!
   double precision sum,sc1,t1,t2,work,cz
   lc = (nlat+1)/2
   sc1 = 2.d0/float(nlat-1)
   call dnlfk(m,n,work)
   nmod = mod(n,2)
   mmod = mod(m,2)
   if(nmod)1,1,2
1  if(mmod)3,3,4
!
!     n even, m even
!
3  kdo = n/2+1
   do 5 idx=1,lc
      i = idx+idx-2
      sum = work(1)/(1.d0-i*i)
      if(kdo.lt.2) go to 29
      do 6 kp1=2,kdo
         k = kp1-1
         t1 = 1.d0-(k+k+i)**2
         t2 = 1.d0-(k+k-i)**2
8        sum = sum+work(kp1)*(t1+t2)/(t1*t2)
6     continue
29    cz(idx) = sc1*sum
5  continue
   return
!
!     n even, m odd
!
4  kdo = n/2
   do 9 idx=1,lc
      i = idx+idx-2
      sum = 0.
      do 101 k=1,kdo
         t1 = 1.d0-(k+k+i)**2
         t2 = 1.d0-(k+k-i)**2
12       sum=sum+work(k)*(t1-t2)/(t1*t2)
101   continue
      cz(idx) = sc1*sum
9  continue
   return
2  if(mmod)13,13,14
!
!     n odd, m even
!
13 kdo = (n+1)/2
   do 15 idx=1,lc
      i = idx+idx-1
      sum = 0.
      do 16 k=1,kdo
         t1 = 1.d0-(k+k-1+i)**2
         t2 = 1.d0-(k+k-1-i)**2
18       sum=sum+work(k)*(t1+t2)/(t1*t2)
16    continue
      cz(idx)=sc1*sum
15 continue
   return
!
!     n odd, m odd
!
14 kdo = (n+1)/2
   do 19 idx=1,lc
      i = idx+idx-3
      sum=0.
      do 20 k=1,kdo
         t1 = 1.d0-(k+k-1+i)**2
         t2 = 1.d0-(k+k-1-i)**2
22       sum=sum+work(k)*(t1-t2)/(t1*t2)
20    continue
      cz(idx)=sc1*sum
19 continue
   return
end
subroutine dnzft(nlat,m,n,th,cz,zh)
   dimension cz(1)
   double precision cz,zh,th,cdt,sdt,cth,sth,chh
   zh = 0.
   cdt = dcos(th+th)
   sdt = dsin(th+th)
   lmod = mod(nlat,2)
   mmod = mod(m,2)
   nmod = mod(n,2)
   if(lmod)20,20,10
10 lc = (nlat+1)/2
   lq = lc-1
   ls = lc-2
   if(nmod)1,1,2
1  if(mmod)3,3,4
!
!     nlat odd n even m even
!
3  zh = .5*(cz(1)+cz(lc)*dcos(2*lq*th))
   cth = cdt
   sth = sdt
   do 201 k=2,lq
!     zh = zh+cz(k)*dcos(2*(k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
201 continue
   return
!
!     nlat odd n even m odd
!
4  cth = cdt
   sth = sdt
   do 202 k=1,ls
!     zh = zh+cz(k+1)*dsin(2*k*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
202 continue
   return
!
!     nlat odd n odd, m even
!
2  if(mmod)5,5,6
5  cth = dcos(th)
   sth = dsin(th)
   do 203 k=1,lq
!     zh = zh+cz(k)*dcos((2*k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
203 continue
   return
!
!     nlat odd n odd m odd
!
6  cth = dcos(th)
   sth = dsin(th)
   do 204 k=1,lq
!     zh = zh+cz(k+1)*dsin((2*k-1)*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
204 continue
   return
20 lc = nlat/2
   lq = lc-1
   if(nmod)30,30,80
30 if(mmod)40,40,60
!
!     nlat even n even m even
!
40 zh = .5*cz(1)
   cth = cdt
   sth = sdt
   do 50 k=2,lc
!     zh = zh+cz(k)*dcos(2*(k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
50 continue
   return
!
!     nlat even n even m odd
!
60 cth = cdt
   sth = sdt
   do 70 k=1,lq
!     zh = zh+cz(k+1)*dsin(2*k*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
70 continue
   return
!
!     nlat even n odd m even
!
80 if(mmod)90,90,110
90 zh = .5*cz(lc)*dcos((nlat-1)*th)
   cth = dcos(th)
   sth = dsin(th)
   do 100 k=1,lq
!     zh = zh+cz(k)*dcos((2*k-1)*th)
      zh = zh+cz(k)*cth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
100 continue
   return
!
!     nlat even n odd m odd
!
110 cth = dcos(th)
   sth = dsin(th)
   do 120 k=1,lq
!     zh = zh+cz(k+1)*dsin((2*k-1)*th)
      zh = zh+cz(k+1)*sth
      chh = cdt*cth-sdt*sth
      sth = sdt*cth+cdt*sth
      cth = chh
120 continue
   return
end
subroutine alin (isym,nlat,nlon,m,p,i3,walin)
   double precision p, walin
   dimension       p(1)        ,walin(1)
   imid = (nlat+1)/2
   lim = nlat*imid
   mmax = min0(nlat,nlon/2+1)
   labc = ((mmax-2)*(nlat+nlat-mmax-1))/2
   iw1 = lim+1
   iw2 = iw1+lim
   iw3 = iw2+labc
   iw4 = iw3+labc
!
!     the length of walin is ((5*l-7)*l+6)/2
!
   call alin1 (isym,nlat,m,p,imid,i3,walin,walin(iw1),walin(iw2),&
   &walin(iw3),walin(iw4))
   return
end
subroutine alin1 (isym,nlat,m,p,imid,i3,pz,p1,a,b,c)
   double precision p,pz,p1,a,b,c
   dimension       p(imid,nlat,3),pz(imid,1),p1(imid,1),&
   &a(1),b(1),c(1)
   save i1,i2
   ihold = i1
   i1 = i2
   i2 = i3
   i3 = ihold
   if(m-1)25,30,35
25 i1 = 1
   i2 = 2
   i3 = 3
   do 45 np1=1,nlat
      do 45 i=1,imid
         p(i,np1,i3) = pz(i,np1)
45 continue
   return
30 do 50 np1=2,nlat
      do 50 i=1,imid
         p(i,np1,i3) = p1(i,np1)
50 continue
   return
35 ns = ((m-2)*(nlat+nlat-m-1))/2+1
   if(isym .eq. 1) go to 36
   do 85 i=1,imid
      p(i,m+1,i3) = a(ns)*p(i,m-1,i1)-c(ns)*p(i,m+1,i1)
85 continue
36 if(m .eq. nlat-1) return
   if(isym .eq. 2) go to 71
   ns = ns+1
   do 70 i=1,imid
      p(i,m+2,i3) = a(ns)*p(i,m,i1)-c(ns)*p(i,m+2,i1)
70 continue
71 nstrt = m+3
   if(isym .eq. 1) nstrt = m+4
   if(nstrt .gt. nlat) go to 80
   nstp = 2
   if(isym .eq. 0) nstp = 1
   do 75 np1=nstrt,nlat,nstp
      ns = ns+nstp
      do 75 i=1,imid
         p(i,np1,i3) = a(ns)*p(i,np1-2,i1)+b(ns)*p(i,np1-2,i3)&
         &-c(ns)*p(i,np1,i1)
75 continue
80 return
end
subroutine alinit (nlat,nlon,walin,dwork)
   !dimension       walin(*)
   double precision walin
   dimension walin(*)
   double precision dwork(*)
   imid = (nlat+1)/2
   iw1 = 2*nlat*imid+1
!
!     the length of walin is 3*((l-3)*l+2)/2 + 2*l*imid
!     the length of work is nlat+1
!
   call alini1 (nlat,nlon,imid,walin,walin(iw1),dwork)
   return
end
subroutine alini1 (nlat,nlon,imid,p,abc,cp)
   double precision p,abc
   dimension p(imid,nlat,2),abc(1),cp(1)
   double precision pi,dt,th,cp,ph
   pi = 4.*datan(1.d0)
   dt = pi/(nlat-1)
   do 160 mp1=1,2
      m = mp1-1
      do 160 np1=mp1,nlat
         n = np1-1
         call dnlfk (m,n,cp)
         do 160 i=1,imid
            th = (i-1)*dt
            call dnlft (m,n,th,cp,ph)
            p(i,np1,mp1) = ph
160 continue
   call rabcp(nlat,nlon,abc)
   return
end
subroutine rabcp(nlat,nlon,abc)
!
!     subroutine rabcp computes the coefficients in the recurrence
!     relation for the associated legendre fuctions. array abc
!     must have 3*((mmax-2)*(nlat+nlat-mmax-1))/2 locations.
!
   double precision abc
   dimension abc(1)
   mmax = min0(nlat,nlon/2+1)
   labc = ((mmax-2)*(nlat+nlat-mmax-1))/2
   iw1 = labc+1
   iw2 = iw1+labc
   call rabcp1(nlat,nlon,abc,abc(iw1),abc(iw2))
   return
end
subroutine rabcp1(nlat,nlon,a,b,c)
!
!     coefficients a, b, and c for computing pbar(m,n,theta) are
!     stored in location ((m-2)*(nlat+nlat-m-1))/2+n+1
!
   double precision a,b,c
   dimension a(1),b(1),c(1)
   mmax = min0(nlat,nlon/2+1)
   do 215 mp1=3,mmax
      m = mp1-1
      ns = ((m-2)*(nlat+nlat-m-1))/2+1
      fm = float(m)
      tm = fm+fm
      temp = tm*(tm-1.)
      a(ns) = sqrt((tm+1.)*(tm-2.)/temp)
      c(ns) = sqrt(2./temp)
      if(m .eq. nlat-1) go to 215
      ns = ns+1
      temp = tm*(tm+1.)
      a(ns) = sqrt((tm+3.)*(tm-2.)/temp)
      c(ns) = sqrt(6./temp)
      mp3 = m+3
      if(mp3 .gt. nlat) go to 215
      do 210 np1=mp3,nlat
         n = np1-1
         ns = ns+1
         fn = float(n)
         tn = fn+fn
         cn = (tn+1.)/(tn-3.)
         fnpm = fn+fm
         fnmm = fn-fm
         temp = fnpm*(fnpm-1.)
         a(ns) = sqrt(cn*(fnpm-3.)*(fnpm-2.)/temp)
         b(ns) = sqrt(cn*fnmm*(fnmm-1.)/temp)
         c(ns) = sqrt((fnmm+1.)*(fnmm+2.)/temp)
210   continue
215 continue
   return
end

