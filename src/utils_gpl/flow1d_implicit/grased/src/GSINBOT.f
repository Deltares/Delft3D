      subroutine gsinbot (nbran  ,ngrid  ,nfrac  ,nlayer ,g      ,
     &                    nunlay ,gsopts ,nvast  ,maxlev ,jugralg,
     &                    sedpar ,ddis   ,trform ,grsize ,duncon ,
     &                    dfrac  ,p0la   ,ptrla  ,deff   ,duneh  ,
     &                    dunel  ,levunl ,hlev   ,nrdzdl ,dzr    ,
     &                    cp     ,afwfqs ,nlev   ,ws     ,wft    ,
     &                    branch ,sedexp ,deltar ,zbave  ,zbfl   ,
     &                    nonall ,nnelvl ,nellvl ,lanrinbt       ,
     &                    juer   ,ker    )
c
c  Module:  GSINBOT (Graded Sediment INitial calculation of BOTtom height)
c
      include '..\include\sobdim.i'
      include '..\include\errcod.i'       
      include '..\include\sobcon.i'    
c
c     Declaration of parameters
c
      integer    nbran ,ngrid   ,nlayer  ,nfrac   ,nunlay  ,
     &           nvast ,maxlev  ,nnelvl  ,jugralg ,juer  ,ker   
      integer    nrdzdl(ngrid)  ,lanrinbt(ngrid)  ,nlev(ngrid)         ,
     &           branch(4,nbran),nonall(3,nbran)  ,gsopts(*)    
      real       g   
      real       sedpar(*)               ,
     &           duncon(*)               ,trform(3,nbran)              ,
     &           ddis  (nfrac+1)         ,dfrac (nfrac)                ,
     &           grsize(4,ngrid,nlayer+1),ptrla (ngrid,nfrac,2)        ,
     &           deltar(ngrid,nfrac)     ,sedexp(ngrid)                ,
     &           levunl(ngrid)                ,
     &           dzr   (ngrid)           ,p0la  (ngrid,nfrac,nunlay)   ,
     &           cp    (ngrid,4)         ,afwfqs(ngrid,8)              ,
     &           duneh (ngrid)           ,dunel (ngrid)                ,
     &           deff  (ngrid,2)         ,
     &           ws    (ngrid)           ,wft   (ngrid,maxlev)         ,
     &           zbave (ngrid)           ,zbfl  (ngrid)                ,
     &           nellvl(nnelvl)
      double precision hlev  (ngrid,maxlev)
c
c     Declaration of local parameters
c
      integer    ibr    ,igr    ,i      ,j      ,ind    ,ii     ,
     &           jj     ,mt     ,nfout  ,nrdigr ,klev   ,nbehou ,
     &           nlevft ,igra   ,neinde ,nrdz   ,nlyupd ,iunl   , 
     &           ilev   ,nstop  ,nlaagv ,iunlup ,lbrnam ,boterr ,
     &           iunl0  ,iunl1  
      real       relden ,kinvis ,thexfp , 
     &           dzunla ,ztot   ,thexfa ,dzrt   ,sum    ,dzrigr ,
     &           wsact  ,zsed   ,ased   ,bsed   ,sum1   ,
     &           ztops  ,redfac ,zbeps  ,deffcr ,dzref  ,dtref  ,
     &           dzt    ,hlevex ,bftex  ,tanalf ,zlev   ,hlevil ,
     &           bftil  ,bftunl ,zlev0  ,dzri   ,dzra   ,dzres  ,
     &           dzhulp ,xc     
      real       duncof (2)
      double precision hws
      character*10      txtx
      character*40      branam
c
c     Constants
c
      integer     d90   , dmed
      parameter  (d90=3 , dmed=4)
c
      kinvis    = sedpar(1)
      relden    = sedpar(2)
      duncof(1) = sedpar(6)
      duncof(2) = sedpar(7)
      thexfa    = sedpar(9)
      dzunla    = sedpar(11)
      redfac    = sedpar(12)
      zbeps     = sedpar(13)
c
c     Set initial flags for HIS files
c
c     Initialize parameters that are not used when the transport
c     formulas etc. are called in initial mode.
c
      nfout = 0 
c      
      nvast = 0
      if (nnelvl.gt.0) nvast= 1

      do ibr=1,nbran
c
         ind  = max(int(trform(2,ibr)),1)
         do igr= branch(3,ibr),branch(4,ibr)
c
            if (nonall(1,ibr).eq.0) then
               zbfl(igr)= -1.0e+15    
            else
               igra = nonall(3,ibr)-branch(3,ibr)
               if (igr+igra .gt. nnelvl) then
c                Fixed layer will be filled in probably later                
                 zbfl(igr)= -1.0e+15 
               else  
                 zbfl(igr)= nellvl(igr+igra)
               endif
            endif
            If (hlev(igr,1).LT.zbfl(igr)) Then
                write (jugralg,'(1x,a,a,i6)')
     +               ' Fixed layer above lowest cross-section'//
     +               ' level at grid pnt ',igr
            Endif
         enddo
      enddo
c
c     Bereken een eerste schatting van N(i) en dzr(i) op basis van
c     bekende bodemligging zb(i) in ieder roosterpunt.
c     Als het aantal onderlagen 1 is, rekenen met standaard Ribberink
c
c bepaling gemiddelde bodemligging zbave
      If (nunlay.le.1)then
         do 1005 igr=1,ngrid
c
c     Bereken   ztops = zsed - Ased / Bsed
c
            wsact = ws(igr)
            nbehou= 0
            if (nbehou.eq.1) then
               call momlev(igr,ngrid,maxlev,nlev,
     &                     wft,ws,klev,wsact)
               klev = min(klev,nlev(igr)-1)
               bsed = wft (igr,klev+1)
               zsed = hlev(igr,klev+1)
               ased = 0.
               do 1002 ii = 2,klev+1
                  ased = ased+(hlev(igr,ii)-hlev(igr,ii-1))*
     &                        (wft(igr,ii)+wft(igr,ii-1))*0.5
 1002          continue
            else
               call moseci(ngrid,igr,maxlev,nlev,wft,hlev,wsact,hws)
               bsed = ws(igr)
               zsed = hws
               ased = 0.
               do 1003 ii = 2,nlev(igr)
                  if (hlev(igr,ii) .gt. hws) goto 1004
                     ased = ased+(hlev(igr,ii)-hlev(igr,ii-1))*
     &                           (wft(igr,ii)+wft(igr,ii-1))*0.5
 1003          continue
c  
 1004          continue
               ased = ased+(hws-hlev(igr,ii-1))*
     .                     (wsact+wft(igr,ii-1))*0.5
            endif
c
            ztops       = zsed-ased/bsed
            zbave(igr)  = ztops
            write(jugralg,*) ' zbave ',igr,zbave(igr)
 1005    continue
      endif
c einde bepaling gemiddelde bodemligging
ctest nvast = 0
      if (nunlay .gt. 1) then

         nlevft = 0
         neinde = 0
         boterr = 0
         do 1 igr=1,ngrid
c
c        check on sum frequencies equal 1
c
            do 230 jj = 1,nunlay
               sum1 = 0.
               do 220 j = 1,nfrac
                  sum1 = sum1+ p0la(igr,j,jj)
  220          continue
               if (abs(sum1-1.0) .gt. 1.0e-4) then
                  nfout = 1
                  write(jugralg,*)
     .                ' Sum of supplied frequencies not equal 1 '
                  write(jugralg,*) ' at grid pnt ',igr, ' sublayer no ',
     .                  jj,' p_i= '
                  write(jugralg,'(1x,7(1x,e10.4))')
     .                 (p0la(igr,j,jj),j=1,nfrac)
               endif
c
  230       continue
c
c      fixed layer present ?
c
            zbfl(igr)   = -1.0e+15
            do 215 j= nunlay,1,-1
               if (p0la(igr,nfrac,j).gt. 0.999999) then
                  zbfl(igr) = levunl(igr) + j * dzunla + 0.1 * zbeps
                  nvast   = 1
                  if (j.eq.nunlay)then
                     write(jugralg,*) ' Fixed layer may not be '//
     .                    'specified at highest level of bed.'
                     neinde = 1
                     goto 2000
                  endif
                  goto 216
               endif
  215       continue
  216       continue

c
c     Bereken   ztops = zsed - Ased / Bsed
c
            wsact = ws(igr)
            nbehou= 0
            if (nbehou.eq.1) then
               call momlev(igr,ngrid,maxlev,nlev,
     &                     wft,ws,klev,wsact)
               bsed = wft (igr,klev+1)
               zsed = hlev(igr,klev+1)
               ased = 0.
               do 300 ii = 2,klev+1
                  ased = ased+(hlev(igr,ii)-hlev(igr,ii-1))*
     &                     (wft(igr,ii)+wft(igr,ii-1))*0.5
  300          continue
            else
               call moseci(ngrid,igr,maxlev,nlev,wft,hlev,wsact,hws)
               bsed = ws(igr)
               zsed = hws
               ased = 0.
               do 200 ii = 2,nlev(igr)
                  if (hlev(igr,ii) .gt. hws) goto 210
                  ased = ased+(hlev(igr,ii)-hlev(igr,ii-1))*
     &                        (wft(igr,ii)+wft(igr,ii-1))*0.5
  200          continue
c  
  210          continue
               ased = ased+(hws-hlev(igr,ii-1))*
     .                                  (wsact+wft(igr,ii-1))*0.5
            endif
c
            ztops       = zsed-ased/bsed
            ztot        = ztops-levunl(igr)
            nrdz        = int(ztot/dzunla) + 1
            dzr(igr)    = ztot-(nrdz-1)*dzunla
            zbave(igr)  = ztops
            write(jugralg,*) ' zbave ',igr,zbave(igr)
c
            if (nrdz .gt. nunlay) then
               nlevft = 1
               write (jugralg,'(1x,a,a,i6)')
     .               'Available number of layers too small ',
     .               'at grid point ',igr
               write (jugralg,'(1x,a,i3,a,i6)')
     .               'Required number of layers : ',nrdz,
     .               ' available : ',nunlay
               write (jugralg,'(1x,a,1x,e12.6)')
     .               'Averaged bed level = ', ztops
            elseif (nrdz .lt. 1) then
               nlevft = 1
               write (jugralg,'(1x,a,a,i6)')
     .               'Reference level of underlayer too high',
     .               ' at grid point i= ',igr
               write (jugralg,'(1x,a,1x,e12.6,a,e12.6)')
     .               'Averaged bed level = ', ztops ,
     .               ' underlayer reference level = ',levunl(igr)
            else
               lanrinbt(igr) = nrdz
               nrdzdl(igr)   = nrdz
            endif
c
c determine composition of upper layers with effect of cross section
c presence
c
            If (zbfl(igr).gt.hlev(igr,1)) Then
               nlyupd=0
               write(jugralg,*) ' No update of upper underlayer'//
     *                          ' composition at grid point ',igr
               write(jugralg,*) ' as fixed layer exceeds'//
     *                          ' lowest cross section level'
            Elseif (levunl(igr).gt.hlev(igr,1)) Then
               nlyupd=0
               write(jugralg,*) ' No update of upper underlayer'//
     *                          ' composition at grid point ',igr
               write(jugralg,*) ' as reference level of underlayer '//
     *                           'exceeds lowest cross section level'
               write(jugralg,*) ' lowest cross section level    : ',
     *                            hlev(igr,1)
               write(jugralg,*) ' lowest underlayer level       : ',
     *                            levunl(igr)
               boterr = 1
               goto 1
            Else
               nlyupd=1
               write(jugralg,*) ' Update of upper underlayer'//
     *                          ' composition at grid point ',igr
            Endif
            If (nlyupd.EQ.1) Then
               write(jugralg,*) ' igr   ilev    frequencies'
c
               ztot        = hlev(igr,1)-levunl(igr)
               iunl        = int(ztot/dzunla) + 1
               iunl0       = iunl
               iunl1       = iunl0
               dzt         = ztot-(iunl-1)*dzunla
c
               do 1200 j=1,nfrac
                  deltar(igr,j) = p0la(igr,j,iunl)*dzt
 1200          continue
               hlevex      = hlev(igr,1)
               bftex       = bsed - wft(igr,1)
               ilev        = 2
               if (ilev.gt.nlev(igr)) then
                  tanalf = 0.0
               else
                  tanalf = (wft(igr,ilev) - wft(igr,ilev-1))/
     .                     (hlev(igr,ilev)- hlev(igr,ilev-1))
               endif
               nstop    = 0
               nlaagv   = 0
               if (bftex.le.0) then
                  nstop = 1
                  goto 1350
               endif
               iunlup = 0
c
 1250          continue
c
               if (iunl.ge.nunlay) then
                  zlev = levunl(igr)+nunlay*dzunla+1.0e+10
               else
                  zlev = levunl(igr)+iunl*dzunla
               endif
               if (ilev.gt.nlev(igr)) then
c                  hlevil= hlev(igr,nlev(igr)) + 1.0e+10
                  bftil = bsed - wft(igr,nlev(igr))
c                  if (hlevil.gt.zsed) then
                     nstop  = 1
                     hlevil = zsed
c                  endif
               else
                  hlevil= hlev(igr,ilev)
                  bftil = bsed - wft(igr,ilev)
                  if (hlevil.gt.zsed) then
                     nstop  = 1
                     hlevil = zsed
                     bftil  = bftex - (zsed-hlevex)*tanalf
                  endif
               endif
c
               if (hlevil.gt.zlev) then
c
c cross-sectie level boven layer level
c
                  nstop  = 0
                  bftunl = bftex - (zlev-hlevex)*tanalf
                  if (bftunl.le.0.0) then
                     nstop = 1
                     if (bftunl.lt.bftex)then
                        zlev0 = hlevex + bftex / tanalf
                     else
                        zlev0 = hlevex
                     endif
                     ased  = 0.5*(zlev0-hlevex) * bftex
                  else
                     ased = (zlev-hlevex)*(bftunl+bftex)*0.5
                  endif
                  hlevex = zlev
                  bftex  = bftunl
                  iunlup = 1
c
               else
c oppervlakte tot cross-section level
                  if (bftil.le.0.0) then
                     nstop = 1
                     if (bftil.ge.bftex)then
                        hlevil = hlevex
                     else
                        hlevil = hlevex + bftex / tanalf
                     endif
                     bftil = 0.0
                  endif
                  ased = (hlevil-hlevex)*(bftil+bftex)*0.5
                  hlevex = hlevil
                  bftex  = bftil
                  ilev   = ilev + 1
                  if (ilev.gt.nlev(igr)) then
                     tanalf = 0.0
                  else
                     tanalf = (wft(igr,ilev) - wft(igr,ilev-1))/
     .                        (hlev(igr,ilev)- hlev(igr,ilev-1))
                  endif
               endif
c
               dzri = ased / bsed
c
 1280          continue
c
               if (dzt+dzri.ge.dzunla) then
                  nlaagv = 1
                  dzra   = dzunla-dzt
                  dzt    = 0.0
               else
                  nlaagv = 0
                  dzra   = dzri
                  dzt    = dzt + dzri
               endif
               dzres = dzri - dzra
c
               do 1300 j=1,nfrac
                  deltar(igr,j) = deltar(igr,j) + p0la(igr,j,iunl)*dzra
 1300          continue
c
 1350          continue
c
               if ((nlaagv.eq.1).or.(nstop.eq.1)) then
                  sum = 0.0
                  do 1400 j=1,nfrac
                     sum = sum + deltar(igr,j)
 1400             continue
                  if (abs(sum).lt.1.0e-20) goto 1600
                  do 1500 j=1,nfrac
                     p0la(igr,j,iunl0) = deltar(igr,j) / sum
                     deltar(igr,j)        = 0.0
 1500             continue
                  write(jugralg,'(1x,i5,1x,i2,2x,6(1x,e10.4))')
     .                  igr,iunl0,(p0la(igr,j,iunl0),j=1,nfrac)
                  iunl1 = iunl0
               endif
c
               if (nlaagv.eq.1) then
                  dzt = 0.0
                  iunl0  = iunl0 + 1
                  if (iunl0.gt.nunlay) then
                     write(jugralg,'(1x,a,a)')
     .                   ' Too much layers required'//
     .                   ' at initialization -gsini '
                     neinde = 3
                     goto 2000
                  endif
                  if (dzres.gt.0.0) then
                     dzri = dzres
                     goto 1280
                  endif
               endif
               nlaagv = 0
               if (iunlup.eq.1)then
                  iunl  = iunl + 1
                  iunlup= 0
               endif
c
               if (nstop.eq.0) goto 1250
c
 1600          continue
               do 1800 iunl=iunl1+1,nunlay
                  do 1700 i=1,nfrac
                     p0la(igr,i,iunl)= p0la(igr,i,iunl1)
 1700             continue
                  write(jugralg,'(1x,i5,1x,i2,2x,6(1x,e10.4))')
     .                  igr,iunl,(p0la(igr,j,iunl),j=1,nfrac)
 1800          continue
c
            Endif
c
    1    continue
c
         if (nfout.eq.1 .or. nlevft.eq.1 ) goto 2000
c         
c     Vul samenstelling van de transportlaag als eerste schatting uit
c     de samenstelling van de onderlaag direct onder zb
c
        do 3 igr=1,ngrid
          do 2 j=1,nfrac
            ptrla(igr,j,2) = p0la(igr,j,nrdzdl(igr))
    2     continue
    3   continue
c
        do ibr =1,nbran
          do igr = branch(3,ibr),branch(4,ibr)
c           Fill array grsize(dmed,igr,1) in subroutine GSDMED
c           for dune height computation
c
            call gsdmed (ngrid ,nfrac ,igr ,dfrac ,ptrla(1,1,2) ,
     &                   grsize(dmed,igr,1))
c
c           Fill array grsize(d90,igr,1) in subroutine GSCHAD for
c           Roughness computation
c
            call gschad (ngrid ,nfrac ,igr ,0.9 ,ddis ,ptrla(1,1,2) ,
     &                   grsize(d90,igr,1))
c
c           Calculate characteristic grainsizes which are not kept for
c           output but are used in the computation 
c           (for the dune height predictor) 
c
            if (nint(trform(1,ibr)) .eq. ctrfaw) then
               call gschad (ngrid ,nfrac ,igr ,0.35 ,ddis ,ptrla(1,1,2),
c                           D35 (temporarily)
     &                      sedexp(igr))
c                           after call of Gsdhgi the Sediment exponent
            endif
          enddo
        enddo
    
c
c       Calculate layer thickness and dune height and length 
c       using the user defined initial state.
c
        call gsdula (ngrid  ,nbran  ,g      ,gsopts ,branch ,
     &               sedpar ,cp     ,afwfqs ,trform ,duncon ,
     &               grsize(1,1,1)  ,sedexp ,duneh  ,dunel  ,deff    )
c
c     Indien er ook een exchangelaag is hier de dikte van bepalen.
c     De array deff(igr,1) wordt als tijdelijke opslag gebruikt.
c
        thexfp = 1.0
        if (nlayer .eq. 2) then
          thexfp = 1.0 + thexfa
          do 5 igr=1,ngrid
            deff(igr,1) = thexfa*deff(igr,2)
    5     continue
        endif
c
c     Berekend de dikte van de transportlaag. Bepaal hierbij de
c     definitieve waarde van N(igr) en dzr(igr) bij level zb(igr)-deff(igr,2)
c     en de nieuwe samenstelling van ptrla(igr,j,2).
c     Indien ook een exchangelaag aanwezig, dan deze procedure herhalen
c     en de nieuwe samenstelling van pexla(igr,j,2).
c
        do 18 igr=1,ngrid
           nrdigr= nrdzdl(igr)
           dzrigr= dzr(igr)
           if (afwfqs(igr,3) .lt. ws(igr)) then
              wsact = afwfqs(igr,3)
           else
              wsact = ws(igr)
           endif
c
            if (nvast.eq.1) then
               deffcr = thexfp*deff(igr,2)
               if (deffcr.lt.zbeps)then
                  write(jugralg,*) 'gsini : total layer thickness'//
     &                             ' < zbeps at grid point ',igr                  
                  deffcr = zbeps
               endif
               if (zbave(igr)-deffcr .lt. zbfl(igr) ) then
                  dzhulp        = (zbave(igr) - zbfl(igr))
                  if (dzhulp .lt.zbeps) then
                     write(jugralg,*) ' gsini : zbave - zbfl < zbeps',
     &                                ' at grid point ',igr
                     write(jugralg,*) ' zbave = ',zbave(igr)  
                     write(jugralg,*) ' zbfl  = ',zbfl (igr)  
                     dzhulp = zbeps
                     zbave(igr) = zbfl(igr) + zbeps
                  endif
                  deff(igr,2) = dzhulp / thexfp
                  if (nlayer .eq. 2)
     &                  deff(igr,1)= thexfa * deff(igr,2)
               endif
            endif
c
           dtref = deff(igr,2)*wsact/ws(igr)
c
            mt = 0
            do 6 i=1,nunlay
              dzrt = dzrigr-dtref+mt*dzunla
              if (dzrt .gt. 0.) goto 7
              mt = mt+1
    6       continue
    7       continue
c
            if (mt .gt. 0) then
              if (nrdigr-mt .lt. 1) then
c                too low top level of sublayer  
                 write(jugralg,*) ' '
                 write(jugralg,'(1x,a,e12.6,a,/,1x,a,i3)')
     .              'Occurring top of sublayer less than one step (= ',
     .              dzunla,' )',
     .              'above reference level at grid point i= ',igr
                 dzref = (nrdigr-mt-1)*dzunla + dzrt
                 if (nlayer.eq.2) dzref = dzref+ deff(igr,1)
                 write(jugralg,'(1x,a,1x,e12.6)')
     .              'Top of sublayer above ref. level= ',dzref
                 write(jugralg,'(1x,a,1x,e12.6)')
     .              'z of reference level = ',levunl(igr)
                 write(jugralg,*) ' dzrigr = ',dzrigr
                 write(jugralg,*) ' dtref  = ',dtref      
                 write(jugralg,*) ' nrdzdl = ',nrdzdl(igr)
                 write(jugralg,*) ' mt     = ',mt          
                 write(jugralg,*) ' zbave  = ',zbave(igr)  
                 write(jugralg,*) ' wsact  = ',wsact       
                 write(jugralg,*) ' ws     = ',ws(igr)     
                 write(jugralg,*) ' def1   = ',deff(igr,1)
                 write(jugralg,*) ' def2   = ',deff(igr,2)
                 neinde = 2
                 goto 2000
              endif                       
              sum1 = 0.
              do 9 j=1,nfrac
                sum = 0.
                do 8 jj=nrdigr-mt,nrdigr-1
                  sum = sum+p0la(igr,j,jj)
    8           continue
                ptrla(igr,j,2) = (dzrigr*p0la(igr,j,nrdigr) -
     &                            dzrt*p0la(igr,j,nrdigr-mt)  +
     &                            dzunla*sum) /dtref
                sum1 = sum1 + ptrla(igr,j,2)
    9         continue
              do 109 j=1,nfrac
                 ptrla(igr,j,2) = ptrla(igr,j,2) / sum1
  109         continue
            endif

   18   continue
      else
        do 19 igr=1,ngrid
           lanrinbt(igr) = 1
           nrdzdl(igr)   = 1
   19   continue
      endif
c
c   CAUTION : dzr and nrdzdl contain values corresponding to Zb and not
c             to lower side of transport or exchange layer, when leaving
c             the subroutine. In gsdula the values will be corresponding
c             to lower side of layers.
c
 
      if (nunlay.gt.1) then
         write(jugralg,*) ' '                                
         write(jugralg,*) ' Bed level '                                
         write(jugralg,*) ' igr, zbave , levunl, nrdzdl, dzr, ws, deff'
         do 140 igr=1,ngrid
            write(jugralg,'(1x,i3,2(1x,e12.6),1x,i2,3(1x,e12.6))')
     .            igr, zbave(igr), levunl(igr), nrdzdl(igr), dzr(igr),
     .            ws(igr),deff(igr,2)
  140    continue
c
      endif
      if (nvast.eq.1) then
         write(jugralg,*) 'zbave,   zbfl,   deff2,   zbave - zbfl '
         do 170 igr=1,ngrid
            write(jugralg,'(1x,i3,4(1x,e12.6))')
     .            igr,zbave(igr),zbfl(igr),deff(igr,2),
     .            zbave(igr)-zbfl(igr)
  170    continue
      endif
c
      return
   
 2000 continue
      ker = fatal
      if (nfout.eq.1) then 
         call error ( juer,'GSINI Sum of supplied frequencies not  '//
     +                     'equal 1 in all grid points',egfreq, ker )
      endif
      if (nlevft.eq.1) then 
         call error ( juer,'GSINI Incorrect underlayer level or too '//
     +                     'less layers in some grid points',egunla,ker)
      endif     
      if (boterr.eq.1) then 
              call getloc (igr,ibr,xc)
              write (txtx,'(f10.2)') xc
              call getbrn (ibr,branam,lbrnam)
              call error ( juer,'GSINI Bottom of underlayer too high ',
     +                           egsubbt, ker )
      endif
      if (neinde.eq.1) then 
         call getloc (igr,ibr,xc)
         write (txtx,'(f10.2)') xc
         call getbrn (ibr,branam,lbrnam)
         call error ( juer,'GSINI Fixed layer may not be '//
     +                    'specified at top multi under layer in @'//
     +                     branam(:lbrnam)//'@ @'//
     +                     txtx//'@' ,egfixtp, ker )
      else if (neinde.eq.2) then 
         call getloc (igr,ibr,xc)
         write (txtx,'(f10.2)') xc
         call getbrn (ibr,branam,lbrnam)
         call error ( juer,'GSINI Too low top level of sublayer in @'//
     +                      branam(:lbrnam)//'@ @'//
     +                      txtx//'@' ,egsubtp, ker )
      else if (neinde.eq.3) then
                   call getloc (igr,ibr,xc)
                   write (txtx,'(f10.2)') xc
                   call getbrn (ibr,branam,lbrnam)
                   call error ( juer,'GSINI Too much layers required '//
     +                               'at initialization in @'//
     +                                branam(:lbrnam)//'@ @'//
     +                                txtx//'@' ,egsubtm, ker )
      endif
c
      end
