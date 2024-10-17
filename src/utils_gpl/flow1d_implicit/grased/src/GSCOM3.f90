subroutine gscom3(ngrid  ,nfrac  ,nbran  ,nnode  ,nboun  ,ngrain ,&
&submin ,subplus,ibr    ,branch ,node   ,deltaa ,&
&ptrla1 ,p0la   ,deff1  ,deff2  ,wfs    ,ws     ,&
&mbdpar ,ptrla2 ,thexfa ,pexla1 ,pexla2 ,dzr    ,&
&nrdzdl ,sedpar ,nunlay ,nlayer ,nonngp ,q2     ,&
&wfsold ,deltar ,deltai ,deltib ,deltiz ,levunl ,&
&zbfl   ,jugralg,lanrinbt       ,grain  ,ddis   ,&
&dfrac  ,grsizmun       ,juer   ,ker    )

!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gscom3.F,v $
! Revision 1.2  1996/06/07  11:55:52  kuipe_j
! multi  +  fixed layer
!
! Revision 1.1  1996/01/08  13:29:47  kuipe_j
! Multi layer option for under layer added
!
!
!***********************************************************************
!
!     Graded Sediment calculate COMposition in case of more sublayers
!
!     Declaration of parameters
!
   integer    ngrid ,nfrac  ,nbran ,nnode   ,nboun ,ibr   ,&
   &nunlay,nlayer ,nonngp,jugralg ,juer  ,ker   ,&
   &ngrain,submin ,subplus
   integer    branch(4,nbran)      ,node  (4,nnode)       ,&
   &mbdpar(5,nboun)      ,nrdzdl(ngrid)         ,&
   &lanrinbt(ngrid)      ,grain(4)
   real       ptrla1(ngrid,nfrac)  ,ptrla2(ngrid,nfrac)   ,&
   &p0la  (ngrid,nfrac,nunlay)                  ,&
   &ddis  (nfrac+1)      ,dfrac(nfrac)          ,&
   &grsizmun(ngrid,ngrain,submin:subplus)       ,&
   &deltar(ngrid,nfrac)   ,&
   &deltai(nfrac)        ,&
   &deltib(nfrac)        ,deltiz(nfrac)         ,&
   &wfs   (ngrid,2)      ,levunl(ngrid)         ,&
   &deff1 (ngrid)        ,deff2 (ngrid)         ,&
   &ws    (ngrid)        ,wfsold(ngrid)         ,&
   &thexfa               ,&
   &pexla1(ngrid,nfrac)  ,pexla2(ngrid,nfrac)   ,&
   &dzr   (ngrid)        ,sedpar(*)             ,&
   &zbfl(ngrid)
   double precision deltaa(ngrid,nfrac+1), q2(ngrid)
!
!     Declaration of local parameters
!
   integer    igr   ,il    ,ir    ,ibou   ,ibout  ,nnoutp  ,nold  ,&
   &n     ,m     ,j     ,nster  ,jj     ,nready ,nhulp  ,&
   &nstera,nneg  ,nnega ,ibrd   ,lbrnam ,nrdzold,&
   &ninbtold     ,lay1  ,lay2   ,lay3   ,lay4   ,lay1a  ,&
   &lay2a ,lay3a ,lay4a ,lay
   real       dz0   ,dz0e  ,wsactn ,wsact ,dzrstr ,sum   ,&
   &daa   ,dzunla,thexfp ,dz0ef ,dzreps ,epsda ,&
   &rwsact,dzres ,p0laii ,xc
   logical    shift ,shifta
   character*10      txtx
   character*8       txtdz
   character*40      branam
!
   include '..\include\sobcon.i'
   include '..\include\errcod.i'
!
!     If left end of branch is a boundary with condition z=f(t) the
!     composition is given.
!
   il    = 0
   ibout = node(1,branch(1,ibr))
   if (ibout .gt. 1) then
      ibou = node(4,branch(1,ibr))
      if (mbdpar(1,ibou) .eq. cmbzft) then
         igr = branch(3,ibr)
         if (q2(igr) .ge. 0.) then
            il = igr
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
            ir = igr
         endif
      endif
   endif
!
   dzunla = sedpar(11)
   dzreps = 0.0  * dzunla
   thexfp = 1.0
   epsda  = 0.99
   if (nlayer.eq.2) thexfp = 1.0 + thexfa
!
   do 200 igr = branch(3,ibr),branch(4,ibr)
!
!       Calculate the sediment transport width
!
      if (wfsold(igr) .gt. ws(igr)) then
!          if flow widht > sediment transporting width use transporting
!                          width ws. (previous time step)
         wsact = ws(igr)
      else
         wsact = wfsold(igr)
      endif
!
      if (wfs(igr,1) .gt. ws(igr)) then
!          if flow width > sediment transporting width use transporting
!                          width ws.
         wsactn = ws(igr)
      else
         wsactn = wfs(igr,1)
      endif
      rwsact = wsact / wsactn
!
!       Adapt P of transport and exchange layer at an inflowing
!       Z-boundary. P's are adapted in case of decreasing
!       sediment transporting width. In order to obtain
!       the given P's as output, the adaptation will be inverted
!       here.
!
      if(igr .eq. il .or. igr .eq. ir ) then
!
         if (wsactn .gt. wsact) then
            do 5 j=1,nfrac
               ptrla2(igr,j) = (ptrla2(igr,j)*wsactn +&
               &(wsactn-wsact) *&
               &p0la(igr,j,nrdzdl(igr)))/wsact
5           continue
            if (nlayer .eq. 2) then
               do 6 j=1,nfrac
                  pexla2(igr,j) = (pexla2(igr,j)*wsactn +&
                  &(wsactn-wsact) *&
                  &p0la(igr,j,nrdzdl(igr)))/wsact
6              continue
            endif
         endif
      endif
!
!-------------------------------------------------------------------------
!
!       Update Mixture of under layers due to change in height of top of
!       under layer. The diffence between actual flow width and sediment
!       width will be taken into account. The update process will be
!       done in 2 steps:
!       Step 1 : update due to change in height
!       Step 2 : update due to change in width

      nrdzold   = nrdzdl(igr)
      nold      = nrdzold
      ninbtold  = lanrinbt(igr)
      daa  = deltaa(igr,nfrac+1)
      dz0  = -daa/wsact + (deff1(igr) - deff2(igr))*thexfp
      dz0e = dz0 * wsact / ws(igr)
      dz0ef= (wsact-wsactn)*thexfp*deff2(igr)/ws(igr)
!
!   Bereken delta-Ib, delta-Iz en delta-I in de laag dz0e per fractie
!
      if (nlayer .eq. 1) then
!
!   Een laag model
!
         do 10 j=1,nfrac
            if (dz0 .gt. 0.)&
            &deltib(j) = 0.5*(ptrla1(igr,j)+ptrla2(igr,j))*wsact*dz0
            if (dz0ef .gt. 0.0)&
            &deltiz(j) = (wsact-wsactn)*deff2(igr)* ptrla2(igr,j)
10       continue
      else
!
!   Twee lagen model
!
         do 20 j=1,nfrac
            if (dz0 .gt. 0.0)&
            &deltib(j) = 0.5*(pexla1(igr,j)+pexla2(igr,j))*wsact*dz0
            if (dz0ef .gt. 0.0)&
            &deltiz(j) = (wsact-wsactn)*(deff2(igr)*ptrla2(igr,j)+&
            &thexfa*deff2(igr)*pexla2(igr,j))
20       continue
      endif
!
!       ** STEP 1 **
!       Updating sublayer data due to change in height of top of
!       under layer.
!
      nready = 0
      if (dz0e .gt. 0.) then
!
!          Sedimentation
!
         if (dz0ef .gt. 0.0) then
            nready = 1
            dz0e   = dz0e + dz0ef
            do 30 j=1,nfrac
               deltai(j) = (deltib(j)+deltiz(j))/ws(igr)
30          continue
         else
            do 35 j=1,nfrac
               deltai(j) = deltib(j)/ws(igr)
35          continue
         endif
         m      = int((dzr(igr)+dz0e)/dzunla)
         dzrstr = dzr(igr) + dz0e - m * dzunla
37       continue
         if (m.eq.0)then
            lay = nrdzdl(igr)
            do 38 j=1,nfrac
               p0la(igr,j,lay) = (p0la(igr,j,lay)*dzr(igr) +&
               &deltai(j)) / (dzr(igr)+dz0e)
38          continue
            call normunl (ngrid, nfrac, nunlay, igr, lay ,p0la)
         else
            if (dzrstr .lt. dzreps) then
               dzrstr = dzunla + dzrstr
               m = m-1
               if (m.eq.0) goto 37
            endif
            if ((nrdzdl(igr)+m) .gt. nunlay) then
               nhulp = nrdzdl(igr)+m-nunlay
               levunl(igr)   = levunl(igr)+ nhulp*dzunla
               lanrinbt(igr) = lanrinbt(igr) - nhulp
               do 40 j=1,nfrac
                  do 40 jj=nhulp+1,nrdzdl(igr)
                     p0la(igr,j,jj-nhulp)= p0la(igr,j,jj)
40             continue
               if ( zbfl(igr).gt.-1.0e+14)then
                  if ( zbfl(igr).lt.levunl(igr)+0.5*dzunla)then
!                     downward shift of layers not allowed because of
!                     fixed layer
                     write (jugralg,'(a,i5,a,i5)')&
                     &' Downward shift of layers'//&
                     &' not allowed because of fixed layer at'//&
                     &' grid point:',igr,', branch:',ibr
                     write (jugralg,'(a,e12.6)')&
                     &' Sedimentation dz0e = ',dz0e
                     write (jugralg,*)&
                     &' m,nrdzdl(igr),nunlay ,dzr(igr),dzunla',&
                     &m,nrdzdl(igr),nunlay ,dzr(igr),dzunla
                     ker = fatal
                     goto 1000
                  endif
               endif
               nrdzdl(igr) = nrdzdl(igr) - nhulp
               write(jugralg,*) ' nrdzdl > nunlay '
               write (jugralg,'(a,i2,a)')&
               &' Loss of ',nhulp,' layer(s) at lower side '
               write (jugralg,*) ' at grid point ',igr,' branch ',ibr
               write (jugralg,'(a,e12.6)')&
               &' Level of underlayer increased : ',levunl(igr)
            endif
!
            dzres = amin1(dz0e ,dzunla-dzr(igr))
            lay = nrdzdl(igr)
            do 50 j=1,nfrac
               p0laii = deltai(j)/dz0e
               p0la(igr,j,lay+1) = p0laii
               p0la(igr,j,lay) = (p0la(igr,j,lay)*dzr(igr)+&
               &p0laii*dzres) / (dzr(igr)+dzres)
!
! test uitvoer 7-12-99
!                if(p0la(igr,j,nrdzdl(igr)) .lt.0.0)then
!                  write(jugralg,*) ' g-p0la ',igr,j,nrdzdl(igr),
!    &                                p0la(igr,j,nrdzdl(igr)),
!    &                                dzr(igr), p0laii,dzres,
!    &                                dzr(igr),dz0e,deltai(j)
!                endif
!                if(p0laii .lt.0.0)
!    &             write(jugralg,*) ' h-p0la ',igr,j,p0laii,deltai(j),
!    &                                dz0e
! einde test uitvoer
!
50          continue
            call normunl (ngrid, nfrac, nunlay, igr, lay   ,p0la)
            call normunl (ngrid, nfrac, nunlay, igr, lay+1 ,p0la)
            do jj=2,m
               do j=1,nfrac
                  p0la(igr,j,lay+jj) = p0la(igr,j,lay+1)
               enddo
            enddo
         endif
         dzr(igr) = dzrstr
         nrdzdl(igr) = nrdzdl(igr) + m
!
      else
!
!  Degradation
!
!  First part of degradation has to be similar to GSCOM1/GSCOM2
         m      = int((-dzr(igr)-dz0e)/dzunla + 1.)
         n      = nrdzdl(igr)
         nster  = n - m
         if (nster .lt. 2) then
!  aantal sublayers wordt aan de onderzijde toegevoegd met de samenstelling
!  van de onderste laag
            levunl(igr)   = levunl(igr) - (2-nster)*dzunla
            lanrinbt(igr) = lanrinbt(igr) + 2 - nster
            do 60 j=1,nfrac
               p0la(igr,j,2)= p0la(igr,j,1)
60          continue
            write (jugralg,'(1x,i2,a,i4,a,i3,a,e12.6)')&
            &2-nster,' layers added at grid point ',   igr,&
            &' branch ',ibr, ' new level = ', levunl(igr)
            nster = 2
         endif
         dzr   (igr) = m*dzunla + dzr(igr) + dz0e
         nrdzdl(igr) = nster
      endif
!
!       Identify changed layers. The characteristic grain sizes
!       of these layers should be recalculated. These layers are
!       identified by the indices lay1, lay2 etc. in array grzismun.
!
!       Sub layer range due to change in height of top of under layer
      lay1 = nrdzold - ninbtold
      lay2 = nrdzdl(igr) - lanrinbt(igr)
!       Sub layer range due to shift of sub layers
      lay3 = - ninbtold
      lay4 = - lanrinbt(igr)
      shift = .true.
      if (lay3 .lt. lay4) then
         lay3 = lay3 + 1
      else if  (lay3 .gt. lay4) then
         lay4 = lay4 + 1
      else
         shift = .false.
      endif
      nrdzold   = nrdzdl(igr)
      ninbtold  = lanrinbt(igr)
!
!       ** STEP 2 **
!       Updating sublayer data due to change in actual flow width
!
!       Correctie tgv zijwaartse verbreding
!       dz0ef equivalente verticale verplaatsing tgv verandering wsact
!
      if (nready .eq. 0) then
         if (dz0ef .gt. 0.0) then
!              sedimentatie tgv versmalling transportbreedte
            m      = int((dzr(igr)+dz0ef)/dzunla)
            dzrstr = dzr(igr) + dz0ef - m * dzunla
64          continue
            if (m.eq.0) then
               lay = nrdzdl(igr)
               do 65 j=1,nfrac
                  p0laii = deltiz(j)/ws(igr)
                  p0la(igr,j,lay) = (p0la(igr,j,lay)*dzr(igr) +&
                  &p0laii) / (dzr(igr)+dz0ef)
65             continue
               call normunl (ngrid, nfrac, nunlay, igr, lay, p0la)
            else
               if (dzrstr .lt. dzreps) then
                  dzrstr = dzunla + dzrstr
                  m = m-1
                  if (m.eq.0) goto 64
               endif
!
               if ((nrdzdl(igr)+m) .gt. nunlay) then
                  nhulp = nrdzdl(igr)+m-nunlay
                  levunl(igr)   = levunl(igr)+ nhulp*dzunla
                  lanrinbt(igr) = lanrinbt(igr) - nhulp
                  do 70 j=1,nfrac
                     do 70 jj=nhulp+1,nrdzdl(igr)
                        p0la(igr,j,jj-nhulp)= p0la(igr,j,jj)
70                continue
                  if ( zbfl(igr).gt.-1.0e+14)then
                     if ( zbfl(igr).lt.levunl(igr)+0.5*dzunla)then
!                        downward shift of layers not allowed because of
!                        fixed layer
                        write (jugralg,'(a,i5,a,i5)')&
                        &' Downward shift of layers'//&
                        &' not allowed because of fixed layer at'//&
                        &' grid point:',igr,', branch:',ibr
                        write (jugralg,'(a,e12.6)')&
                        &' Sedimentation dz0ef = ',dz0ef
                        write (jugralg,*)&
                        &' m,nrdzdl(igr),nunlay ,dzr(igr),dzunla',&
                        &m,nrdzdl(igr),nunlay ,dzr(igr),dzunla
                        dz0e = dz0ef
                        ker  = fatal
                        goto 1000
                     endif
                  endif
                  nrdzdl(igr) = nrdzdl(igr) - nhulp
                  write(jugralg,*) ' nrdzdl > nunlay  '
                  write (jugralg,'(a,i2,a)')&
                  &' Loss of ',nhulp,' layer(s) at lower side '
                  write (jugralg,*) ' at grid point ',igr,' branch ',&
                  &ibr
                  write (jugralg,'(a,e12.6)')&
                  &' Level of underlayer increased : ',levunl(igr)
               endif
!
               dzres = amin1(dz0ef ,dzunla-dzr(igr))
               lay = nrdzdl(igr)
               do 80 j=1,nfrac
                  p0laii = deltiz(j)/dz0ef/ws(igr)
                  p0la(igr,j,lay+1) = p0laii
                  p0la(igr,j,lay) = (p0la(igr,j,lay)*dzr(igr)+&
                  &p0laii*dzres) / (dzr(igr)+dzres)
80             continue
               call normunl (ngrid, nfrac, nunlay, igr, lay   ,p0la)
               call normunl (ngrid, nfrac, nunlay, igr, lay+1 ,p0la)
               do jj=2,m
                  do j=1,nfrac
                     p0la(igr,j,lay+jj) = p0la(igr,j,lay+1)
                  enddo
               enddo
            endif
            dzr(igr) = dzrstr
            nrdzdl(igr) = nrdzdl(igr) + m
!
         elseif (dz0ef .lt. 0.0) then
!
!              erosie tgv verbreding transportbreedte
!
            m     = int((-dzr(igr)-dz0ef)/dzunla  + 1. )
            n     = nrdzdl(igr)
            nster = n - m
            nstera= 0
            if (nster.lt.1) then
               nstera = 1 - nster
               nster  = 1
            endif
            dzrstr= m*dzunla + dzr(igr) + dz0ef
            do 90 j=1,nfrac
               p0laii= nstera*p0la(igr,j,1)
               do 85 jj=nster,n-1
                  p0laii = p0laii + p0la(igr,j,jj)
85             continue
               p0laii=-(p0la(igr,j,n)*dzr(igr) + p0laii*dzunla -&
               &p0la(igr,j,nster)*dzrstr)/dz0ef
               ptrla2(igr,j) =&
               &ptrla2(igr,j)*rwsact + p0laii * (1.0 - rwsact)
               if (nlayer .eq. 2)&
               &pexla2(igr,j) =&
               &pexla2(igr,j)*rwsact + p0laii * (1.0 - rwsact)
90          continue
            if (nster.eq.1) nster= 1-nstera
            if (nster .lt. 2) then
!  aantal sublayers wordt aan de onderzijde toegevoegd met de samenstelling
!  van de onderste laag
               levunl(igr)   = levunl(igr) - (2-nster)*dzunla
               lanrinbt(igr) = lanrinbt(igr) + 2 -nster
               do 100 j=1,nfrac
                  p0la(igr,j,2)= p0la(igr,j,1)
100            continue
               write (jugralg,'(1x,a,i2,a,a)')&
               &'At lower side of underlayer ',2-nster,&
               &' layers are added'
               write (jugralg,'(1x,a,i4,a,i4,a,e12.6,a)')&
               &'(at grid point ',igr ,' branch ',ibr,&
               &' Level = ',levunl(igr),' )'
               nster = 2
            endif
            dzr   (igr) = dzrstr
            nrdzdl(igr) = nster
         endif
      endif
!
!       Identify for the second time changed layers.
!       The characteristic grain sizes
!       of these layers should be recalculated.
!
!       Sub layer range due to change in height of top of under layer
      lay1a = min(lay1,lay2)
      lay2a = max(lay1,lay2)
      lay1  = nrdzold - ninbtold
      lay2  = nrdzdl(igr) - lanrinbt(igr)
      lay1a = min(lay1a,lay1,lay2)
      lay2  = max(lay2a,lay1,lay2)
      lay1  = lay1a
!       Sub layer range due to change in actual flow width
      if (shift) then
         lay3a = min(lay3,lay4)
         lay4a = max(lay3,lay4)
      endif
      lay3 = - ninbtold
      lay4 = - lanrinbt(igr)
      shifta = .true.
      if (lay3 .lt. lay4) then
         lay3 = lay3 + 1
      else if  (lay3 .gt. lay4) then
         lay4 = lay4 + 1
      else
         shifta = .false.
      endif
      if (shift .and. shifta) then
         lay3a = min(lay3a,lay3,lay4)
         lay4  = max(lay4a,lay3,lay4)
         lay3  = lay3a
      else if (shift) then
         lay3 = lay3a
         lay4 = lay4a
      endif
!
!       Recalculate grain sizes due to change op top of under layer
!
      call gscharun (lay1   ,lay2   ,nfrac  ,nbran   ,ngrid  ,&
      &ngrain ,submin ,subplus,nunlay  ,grain  ,&
      &branch ,ddis   ,dfrac  ,p0la    ,nrdzdl ,&
      &lanrinbt       ,grsizmun        )
!
!       Recalculate grain sizes due to shift of sub under layers
!
      if (shift .or. shifta)&
      &call gscharun (lay3   ,lay4   ,nfrac  ,nbran   ,ngrid  ,&
      &ngrain ,submin ,subplus,nunlay  ,grain  ,&
      &branch ,ddis   ,dfrac  ,p0la    ,nrdzdl ,&
      &lanrinbt       ,grsizmun        )
!
      if (wsactn .gt. wsact) then
         sum = 0.
         do 130 j=1,nfrac
            sum = sum + ptrla2(igr,j)
130      continue
         do 140 j=1,nfrac
            ptrla2(igr,j) = ptrla2(igr,j)/sum
140      continue
         if (nlayer .eq. 2) then
            sum = 0.
            do 150 j=1,nfrac
               sum = sum + pexla2(igr,j)
150         continue
            do 160 j=1,nfrac
               pexla2(igr,j) = pexla2(igr,j)/sum
160         continue
         endif
      endif
!
      if (nonngp.eq.1) then
         do 170 j=1,nfrac
            deltar(igr,j)= epsda*deff2(igr)*wsactn*ptrla2(igr,j)
170      continue
      endif
200 continue
!
! output of frequencies and level if required or if fatal error occurs
! test uitvoer 7-12-99
   nnoutp = 0
   if (nnoutp.eq.1)then
      nneg= 0
      nnega= 0
      do 250 igr = branch(3,ibr),branch(4,ibr)
         do 240 j= 1,nfrac
            if ( ptrla2(igr,j).lt.0.0 ) nneg= nneg+1
            if ( p0la(igr,j,nrdzdl(igr)).lt.0.0 ) nnega= nnega+1
240      continue
250   continue
      write(jugralg,*) 'ptrla2',nneg
      do 300 igr = branch(3,ibr),branch(4,ibr)
         write(jugralg,'(1x,i3,6(1x,e10.4))') igr,&
         &(ptrla2(igr,j),j=1,nfrac),deff2(igr)
300   continue
      write(jugralg,*) 'p0la  ',nnega
      do 400 igr = branch(3,ibr),branch(4,ibr)
         jj = nrdzdl(igr)
         do 400 jj=1,nrdzdl(igr)
            write(jugralg,'(1x,i3,1x,i2,6(1x,e10.4))')&
            &igr,jj,(p0la(igr,j,jj),j=1,nfrac)
400   continue
      write(jugralg,*) 'igr nr  dzr '
      do 500 igr = branch(3,ibr),branch(4,ibr)
         write(jugralg,'(1x,i3,1x,i2,1x,e10.4)') igr,nrdzdl(igr),&
         &dzr(igr)
500   continue
   endif
! einde test uitvoer
!
   return
!
1000 ker = fatal
   call error ( juer,'GSCOM3 downward shift of layers not allowed',&
   &egfixs1, ker )
   call getloc (igr,ibrd,xc)
   write (txtx,'(f10.2)') xc
   call getbrn (ibrd,branam,lbrnam)
   write (txtdz,'(f8.4)') dz0e
   call error (juer,'GSCOM3 sed @'//txtdz//'@ @'&
   &//branam(:lbrnam)//'@ @'&
   &//txtx//'@' ,egfixs2, info )
end

subroutine normunl (ngrid, nfrac, nunlay, igr, lay ,p0la)
!
   integer             ngrid, nfrac, nunlay, igr ,lay
   real                p0la(ngrid,nfrac,nunlay)
!
   integer             i
   real                sum
!
   sum = 0.
   do  i=1,nfrac
      sum = sum + p0la(igr,i,lay)
   enddo
   do  i=1,nfrac
      p0la(igr,i,lay) = p0la(igr,i,lay)/sum
   enddo
end
