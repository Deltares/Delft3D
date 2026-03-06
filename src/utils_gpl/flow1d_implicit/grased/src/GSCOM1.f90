subroutine gscom1(ngrid  ,nfrac  ,nbran  ,nnode ,nboun  ,ibr    ,&
&branch ,node   ,deltaa ,ptrla1,p0la   ,deff1  ,&
&deff2  ,wfs    ,ws     ,mbdpar ,ptrla2,nunlay ,&
&nonngp ,nvast  ,zbeps  ,nrdzdl ,q2    ,deltar ,&
&zbave  ,zbfl   ,deltai ,wfsold ,sedpar,dzr    ,&
&time   ,jugralg)

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gscom1.F,v $
! Revision 1.4  1996/06/07  11:55:45  kuipe_j
! multi  +  fixed layer
!
! Revision 1.3  1996/01/08  13:29:44  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.2  1995/09/27  10:11:55  kuipe_j
! Maintenance
!
!
!***********************************************************************
!     Graded Sediment calculate COMposition in case of 1 layer model

!     Declaration of parameters
!
   integer    ngrid ,nfrac  ,nbran ,nnode   ,nboun ,ibr   ,&
   &nunlay,nonngp ,nvast ,jugralg
   integer    branch(4,nbran)      ,node  (4,nnode)       ,&
   &mbdpar(5,nboun)      ,nrdzdl(ngrid)
   double precision time, deltaa(ngrid,nfrac+1)
   real       zbeps ,dzunla
   real       ptrla1(ngrid,nfrac)  ,ptrla2(ngrid,nfrac)   ,&
   &p0la  (ngrid,nfrac,nunlay)  ,deltai(nfrac)  ,&
   &deltar(ngrid,nfrac)  ,&
   &wfs   (ngrid,2)      ,dzr   (ngrid)         ,&
   &deff1 (ngrid)        ,deff2 (ngrid)         ,&
   &ws    (ngrid)        ,&
   &zbave (ngrid)        ,zbfl  (ngrid)         ,&
   &wfsold(ngrid)        ,sedpar(*)
   double precision q2(ngrid)
!
!     Declaration of local parameters
!
   integer    igr  ,jf  ,il     ,j     ,ir     ,ibou   ,ibout,&
   &m    ,n   ,nster  ,nstera,jj
   real       dz   ,de   ,rwidth,factb ,dzhulp ,deff2r ,wsact,&
   &dz0e ,dz0ef,dzrstr,sum   ,epsda  ,factc
   logical    nega
   include '..\include\sobcon.i'
!
!     If left end of branch is a boundary with condition z=f(t) the
!     composition is given.
!
!
   dzunla= sedpar(11)
   epsda = 0.99
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

   do 30 igr = branch(3,ibr)+il,branch(4,ibr)-ir
!
!       Calculate the sediment transport width
!
      if (wfsold(igr) .gt. ws(igr)) then
!          if flow widht > sediment transporting width use ws
         rwidth = 1./ws(igr)
      else
         rwidth = 1./wfsold(igr)
      endif

      dz = - deltaa(igr,nfrac+1) * rwidth
      if (dz .gt. deff2(igr)-deff1(igr)) then
!
!          Sedimentation
!
         do 10 jf=1,nfrac
            de             = deff2(igr) + deff1(igr)
            ptrla2(igr,jf) = (- deltaa(igr,jf) * rwidth&
            &+ 0.5 * ptrla1(igr,jf) * ( de - dz )&
            &) / (0.5 * (dz + de))
            if(ptrla2(igr,jf).lt.0.0) then
               nega = .true.
            endif
10       continue
      else
!
!         Degradation
!
!
         if (nvast.eq.1) then
            factb = 1.0
            if (deff2(igr).lt.zbeps) then
               write(jugralg,*) 'gscom1 : layer thickness < zbeps at ',&
               &igr
               deff2(igr) = zbeps
            endif
            if ( abs(dz).gt.deff1(igr))then
               write(jugralg,*) 'gscom1 : dz > layer thickness',&
               &igr,dz,deff1(igr)
            endif
            dzhulp= ( dz - deff2(igr) ) / factb
            if ( zbave(igr) + dzhulp .lt. zbfl(igr)) then
               deff2r = zbave(igr) + dz / factb&
               &- zbfl(igr)
               if (deff2r .lt. zbeps) then
                  deff2(igr) = zbeps
                  dzhulp = (zbfl(igr) + deff2(igr) - zbave(igr))*factb
                  write(jugralg,*) 'gscom1 : dz too large at ',&
                  &igr,dz,dzhulp
                  if (dzhulp .ge. 0.0)then
                     write(jugralg,*)&
                     &'gscom1 : dz should be negative or 0'
                     factc = 0.0
                     dz    = 0.0
                  else
                     factc = dzhulp / dz
                     dz = dzhulp
                  endif
                  if (dz .gt. deff2(igr)-deff1(igr)) then
                     write(jugralg,*) 'gscom1 : erosion instead of '//&
                     &'sedim. by correction dz '
                  endif
                  do 15 jf=1,nfrac+1
                     deltaa(igr,jf) = deltaa(igr,jf) * factc
15                continue
               else
                  deff2(igr) = deff2r
               endif
            endif
         endif
!
!***************************************************************
! underlayer model computation of average p0la
         dz0ef = deff2(igr)-deff1(igr)-dz
         if ((nunlay.gt.1) .and. (dz0ef .gt. 1.0e-15)) then
            dz0e  = -dz0ef /rwidth/ws(igr)
            m     = int((-dzr(igr)-dz0e)/dzunla + 1.)
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
!
         do 20 jf=1,nfrac
            ptrla2(igr,jf) = (- deltaa(igr,jf) * rwidth + deltai(jf)&
            &+ ptrla1(igr,jf) * deff1(igr))&
            &/ deff2(igr)
            if(ptrla2(igr,jf).lt.0.0) then
               nega = .true.
            endif
20       continue
      endif
      if (nvast.eq.1) then
         factb = ws(igr) * rwidth
         zbave(igr) = zbave(igr) + dz / factb
         if (zbave(igr).lt.zbfl(igr)) then
            write(jugralg,*) 'gscom1 ', igr,zbave(igr),zbfl(igr),&
            &' not allowed , code error '
            zbave(igr) = zbfl(igr) + zbeps
         endif
      endif
30 continue

!
!  The available area per fraction in the transport layer
!  will be calculated to prevent negative frequecies
!
   if ((nunlay.eq.1).and.(nonngp.eq.1)) then
      do 40 igr = branch(3,ibr),branch(4,ibr)
!
!       Calculate the sediment transport width
!
         if (wfs(igr,1) .gt. ws(igr)) then
!             if flow widht > sediment transporting width use ws
            wsact  = ws(igr)
         else
            wsact  = wfs(igr,1)
         endif
         do 35 j=1,nfrac
            deltar(igr,j)= epsda*wsact*deff2(igr)*ptrla2(igr,j)
35       continue
40    continue
   endif
   if (nega) then
      write (jugralg,*) 'gscom1 : Negative frequencies at time ',time
   endif
end
