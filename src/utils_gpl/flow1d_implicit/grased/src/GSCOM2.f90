subroutine gscom2(ngrid  ,nfrac  ,nbran  ,nnode  ,nboun  ,ibr    ,&
&dt     ,thexfa ,sedim  ,fdi    ,pdiacc ,branch ,&
&node   ,deltaa ,ptrla1 ,pexla1 ,p0la   ,dunel  ,&
&sedtr  ,deff1  ,deff2  ,wfs    ,ws     ,mbdpar ,&
&ptrla2 ,pexla2 ,a11    ,a12    ,a21    ,a22    ,&
&b1     ,b2     ,nunlay ,nonngp ,nvast  ,zbeps  ,&
&nrdzdl ,q2     ,deltar ,zbave  ,zbfl   ,deltai ,&
&wfsold ,sedpar ,dzr    ,time   ,jugralg)

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gscom2.F,v $
! Revision 1.4  1996/06/07  11:55:49  kuipe_j
! multi  +  fixed layer
!
! Revision 1.3  1996/01/08  13:29:46  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.2  1995/09/27  10:11:57  kuipe_j
! Maintenance
!
!
!***********************************************************************
!     Graded Sediment calculate COMposition in case of 2 layer model

!     Declaration of parameters
!
   integer    ngrid ,nfrac  ,nbran ,nnode ,nboun  ,ibr
   integer    nunlay,nonngp ,nvast ,jugralg
   integer    branch(4,nbran)      ,node  (4,nnode)       ,&
   &mbdpar(5,nboun)
   integer    nrdzdl(ngrid)
   double precision time, deltaa(ngrid,nfrac+1)
   real       zbeps
   real       dt    ,thexfa,fdi    ,dzunla
   real       ptrla1(ngrid,nfrac)  ,ptrla2(ngrid,nfrac)   ,&
   &pexla1(ngrid,nfrac)  ,pexla2(ngrid,nfrac)   ,&
   &p0la  (ngrid,nfrac,nunlay)  ,wfsold(ngrid)  ,&
   &wfs   (ngrid,2)      ,dzr(ngrid)            ,&
   &deff1 (ngrid)        ,deff2 (ngrid)         ,&
   &ws    (ngrid)        ,dunel (ngrid)         ,&
   &sedtr (ngrid,nfrac+2),deltar(ngrid,nfrac)   ,&
   &pdiacc(nfrac)        ,sedpar(*)             ,&
   &zbave (ngrid)        ,zbfl  (ngrid)         ,&
   &a11   (nfrac)        ,a12   (nfrac)         ,&
   &a21   (nfrac)        ,a22   (nfrac)         ,&
   &b1    (nfrac)        ,b2    (nfrac)         ,&
   &deltai(nfrac)
   double precision q2(ngrid)
   logical    sedim, nega
   logical    equal
   external   equal
!
!     Declaration of local parameters
!
   integer    igr   ,jf     ,il     ,ir     ,ibou  ,ibout  ,&
   &j     ,m      ,n      ,nster  ,nstera,jj     ,&
   &ncdef2
   real       dz    ,rwidth ,def2x  ,ddef   ,dex1  ,dex2   ,&
   &dex2x ,ddex   ,det    ,gamast ,gamst2,gamfdi ,&
   &wsact ,dz0e   ,dz0ef  ,dzrstr ,sum   ,epsda  ,&
   &thexfp,factb  ,factc  ,dzhulp ,deff2r
!
!     Declaration of constants
!
   real       gamma
   parameter (gamma=0.08)

   include '..\include\sobcon.i'
!
!     If left end of branch is a boundary with condition z=f(t) the
!     composition is given.
!
   dzunla= sedpar(11)
   epsda = 0.99
   thexfp= 1. + thexfa
   il    = 0
   ibout = node(1,branch(1,ibr))
   if (ibout .gt. 1) then
      ibou = node(4,branch(1,ibr))
      if (mbdpar(1,ibou) .eq. cmbzft) then
         igr = branch(3,ibr)
         if (q2(igr) .ge. 0.) then
            il = 1
         endif
      endif
   endif
!
!     If right end of branch is a boundary with condition z=f(t) the
!     composition is given.
!
   ir    = 0
   ibout = node(1,branch(2,ibr))
   if (ibout .gt. 1) then
      ibou = node(4,branch(2,ibr))
      if (mbdpar(1,ibou) .eq. cmbzft) then
         igr = branch(4,ibr)
         if (q2(igr) .le. 0.) then
            ir = 1
         endif
      endif
   endif

   nega = .false.

   do 60 igr = branch(3,ibr)+il,branch(4,ibr)-ir
!
!       Calculate the sediment transport width
!
      if (wfsold(igr) .gt. ws(igr)) then
!          if flow widht > sediment transporting width use ws
         rwidth = 1./ws(igr)
      else
         rwidth = 1./wfsold(igr)
      endif
      gamast = .5 * gamma * dt * sedtr(igr,nfrac+1) /&
      &dunel(igr) * rwidth
!
!       Transport layer
!
      ncdef2 = 0
5     continue
      dz    = - deltaa(igr,nfrac+1) * rwidth
      def2x = deff2(igr) + deff1(igr)
      ddef  = deff2(igr) - deff1(igr)
      if (dz .gt. ddef .or. sedim) then
!
!          Sedimentation
!
         gamst2 = gamast * 2.
         gamfdi = gamast * fdi
         do 10 jf=1,nfrac
            a11(jf) = .5 * (dz + def2x) + gamfdi
            a12(jf) = - gamast
            b1 (jf) = - deltaa(igr,jf) * rwidth&
            &- ptrla1(igr,jf) * (.5*(dz-def2x)+gamfdi)&
            &+ gamast * pexla1(igr,jf) - gamst2 * pdiacc(jf)
10       continue
      else
!
!         Degradation
!
         gamst2 = gamast * 2.
         gamfdi = gamast * fdi
         do 20 jf=1,nfrac
            a11(jf) = deff2(igr) + gamfdi
            a12(jf) = .5 * (dz - ddef) - gamast
            b1 (jf) = - deltaa(igr,jf) * rwidth&
            &+ ptrla1(igr,jf) * (deff1(igr) - gamfdi)&
            &+ pexla1(igr,jf) * (.5 * (ddef-dz) + gamast)&
            &- gamst2 * pdiacc(jf)
20       continue
      endif
!
!       Exchange layer
!
      dex1  = thexfa * deff1(igr)
      dex2  = thexfa * deff2(igr)
      dex2x = dex2 + dex1
      ddex  = dex2 - dex1
      if (dz .gt. ddef + ddex .or. sedim) then
!
!         Sedimentation
!
         do 30 jf=1,nfrac
            a21(jf) = deff2(igr)
            a22(jf) = .5 * (dz - ddef + dex2x)
            b2 (jf) = - deltaa(igr,jf) * rwidth&
            &+ ptrla1(igr,jf) * deff1(igr)&
            &+ pexla1(igr,jf) * .5 * (ddef+dex2x-dz)
30       continue
      else
!
!         Degradation
! vaste laag
         if (nvast.eq.1) then
            if (ncdef2.gt.0) goto 32
            factb = 1.0
            if (deff2(igr)*thexfp.lt.zbeps) then
               ncdef2 = ncdef2 + 1
               write(jugralg,*) 'gscom2 : layer thickness < zbeps at ',&
               &igr
               deff2(igr) = zbeps/thexfp
            endif
            if ( abs(dz).gt.deff1(igr))then
               write(jugralg,*) 'gscom2 : dz > layer thickness',&
               &igr,dz,deff1(igr)
            endif
            dzhulp= ( dz - deff2(igr)*thexfp ) / factb
            if ( zbave(igr) + dzhulp .lt. zbfl(igr)) then
               ncdef2 = ncdef2 + 1
               deff2r = (zbave(igr) + dz / factb&
               &- zbfl(igr))
               if (deff2r .lt. zbeps) then
                  deff2(igr) = zbeps/thexfp
                  dzhulp = (zbfl(igr) + zbeps - zbave(igr))*factb
                  write(jugralg,*) 'gscom2 : dz too large at ',&
                  &igr,dz,dzhulp
                  if (dzhulp .ge. 0.0)then
                     write(11,*) 'gscom2 : dz should be negative or 0'
                     factc = 0.0
                     dz    = 0.0
                  else
                     factc = dzhulp / dz
                     dz = dzhulp
                  endif
! test uitvoer 7-12-99
!                  if (dz .gt. (deff2(igr)-deff1(igr))*thexfp) then
!                     write(jugralg,*) 'gscom2 : erosion instead of '//
!    +                                 'sedim. by correction dz '
!                  endif
                  do 15 jf=1,nfrac+1
                     deltaa(igr,jf) = deltaa(igr,jf) * factc
15                continue
               else
                  deff2(igr) = deff2r/thexfp
               endif
            endif
            if (ncdef2.gt.0) goto 5
         endif
32       continue
!
! einde vaste laag
!
!***************************************************************
! underlayer model computation of average p0la
         dz0ef = ddef+ddex-dz
         if ((nunlay.gt.1).and.(dz0ef.gt.1.0e-15)) then
            dz0e  = -dz0ef /rwidth/ws(igr)
            m     = int((-dzr(igr)-dz0e)/dzunla  + 1.)
            n     = nrdzdl(igr)
            nster = n - m
            nstera= 0
            if (nster.lt.1) then
               nstera = 1 - nster
               nster  = 1
            endif
            dzrstr= m*dzunla + dzr(igr) + dz0e
            do 36 jf=1,nfrac
               sum= nstera*p0la(igr,jf,1)
               do 34 jj=nster,n-1
                  sum = sum + p0la(igr,jf,jj)
34             continue
               sum= -(p0la(igr,jf,n)*dzr(igr) + sum*dzunla -&
               &p0la(igr,jf,nster)*dzrstr)/dz0e
               deltai(jf)= sum * dz0ef
36          continue
         else
            do 38 jf=1,nfrac
               deltai(jf)= p0la(igr,jf,nrdzdl(igr)) * dz0ef
38          continue
         endif
!***************************************************************
         do 40 jf=1,nfrac
            a21(jf) = deff2(igr)
            a22(jf) = dex2
            b2 (jf) = - deltaa(igr,jf) * rwidth&
            &+ ptrla1(igr,jf) * deff1(igr)&
            &+ pexla1(igr,jf) * dex1&
            &+ deltai(jf)
40       continue
      endif


      do 50 jf=1,nfrac
         det            = a11(jf)*a22(jf) - a21(jf)*a12(jf)
         if (equal(det,0.)) then
            write (jugralg,*) 'gscom2 : Cannot calculate new mixture'
         else
            ptrla2(igr,jf) = (b1(jf)*a22(jf) - b2(jf)*a12(jf)) / det
            pexla2(igr,jf) = (b2(jf)*a11(jf) - b1(jf)*a21(jf)) / det
            if (ptrla2(igr,jf).lt.0.0) nega = .true.
            if (pexla2(igr,jf).lt.0.0) nega = .true.
         endif
50    continue
! update zbave
      if (nvast.eq.1) then
         factb = ws(igr) * rwidth
         zbave(igr) = zbave(igr) + dz / factb
         if (zbave(igr).lt.zbfl(igr)) then
            write(jugralg,*) 'gscom2 ', igr,zbave(igr),zbfl(igr),&
            &' not allowed , code error '
            zbave(igr) = zbfl(igr) + zbeps
         endif
      endif

60 continue
!
!     The available area per fraction in the transport layer
!     will be calculated to prevent negative frequecies
!
   if ((nunlay.eq.1).and.(nonngp.eq.1)) then
      do 80 igr = branch(3,ibr),branch(4,ibr)
!
!          Calculate the sediment transport width
!
         if (wfs(igr,1) .gt. ws(igr)) then
!             if flow widht > sediment transporting width use ws
            wsact  = ws(igr)
         else
            wsact  = wfs(igr,1)
         endif
         do 70 j=1,nfrac
            deltar(igr,j)= epsda*wsact*deff2(igr)*ptrla2(igr,j)
70       continue
80    continue
   endif
   if (nega) then
      write (jugralg,*) 'gscom2 : Negative frequencies at time ',time
   endif
end
