      subroutine gsinmix(ngrid  ,nbran  ,nlayer ,g      ,nunlay ,nfrac ,
     &                   juer   ,jugralg,ker    ,gsopts ,branch ,sedpar,
     &                   ddis   ,dfrac  ,cp     ,afwfqs ,trform ,duncon,
     &                   duneh  ,dunel  ,deff   ,grsize ,dmed0  ,bfrict,
     &                   ptrla  ,pexla  ,p0la   ,dzr    ,levunl ,nrdzdl,
     &                   ws     ,sedexp ,deltar ,zbave  ,zbfl   )
c
c     Graded Sediment calculate DUne height and LAyer thickness
c
c     Declaration of parameters
c
      integer    ngrid ,nbran   ,nlayer ,nfrac ,nunlay,
     &           juer  ,jugralg ,ker     
      integer    branch(4,nbran) ,gsopts(*)    ,
     &           bfrict(3,nbran) ,nrdzdl(ngrid)
      real       g    
      real       grsize(4,ngrid,nlayer+1),cp    (ngrid,4)       ,
     &           afwfqs(ngrid,8)         ,deff  (ngrid,2)       ,
     &           dunel (ngrid)           ,duneh (ngrid)         ,
     &           trform(3,nbran)         ,
     &           sedpar(*)               ,duncon(*)             ,
     &           ddis  (nfrac+1)         ,dfrac (nfrac)         ,     
     &           ptrla (ngrid,nfrac,2)   ,pexla (ngrid,nfrac,2) ,
     &           p0la  (ngrid,nfrac,nunlay)   ,
     &           deltar(ngrid,nfrac)     ,
     &           levunl(ngrid)           ,ws    (ngrid)         ,
     &           zbave (ngrid)           ,zbfl  (ngrid)         ,
     &           sedexp(ngrid)           ,dzr   (ngrid)         ,
     &           dmed0 (ngrid) 
c
c     Declaration of local parameters
c
      integer    ibr    ,igr    ,rouopt ,mt     ,me     ,i      ,
     &           j      ,jj     ,nonngp ,nvast  ,lbrnam
      real       dzunla ,thexfa ,dzrt   ,sum    ,dzre   ,dzref  ,
     &           wsact  ,sum1   ,xc     ,deffcr ,thexfp ,dtref  ,
     &           dexef  ,zbeps  ,dzhulp
      character*10      txtx
      character*40      branam
c
      include '..\include\errcod.i'
      include '..\include\sobcon.i'

      integer    dmed
      parameter (dmed=4)
      integer    rouryn
      parameter (rouryn=1)

      thexfa    = sedpar(9)
      dzunla    = sedpar(11)
      zbeps     = sedpar(13)

      rouopt    = gsopts(3)
      nonngp    = gsopts(8)
      nvast     = gsopts(9)
c
      do ibr =1,nbran
         do igr = branch(3,ibr),branch(4,ibr)
c         
c           The mixture of the transport layer is determined on a 
c           rectangular cross-section. So fill array grsize(dmed,igr,1)
c           in subroutine GSDMED for the second time. 
c
            call gsdmed (ngrid ,nfrac ,igr ,dfrac ,ptrla(1,1,2) ,
     &                   grsize(dmed,igr,1))
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
c     Calculate layer thickness and dune height and length when a
c     steady flow has been calculated.
c
      call gsdula (ngrid  ,nbran  ,g      ,gsopts ,branch ,
     &             sedpar ,cp     ,afwfqs ,trform ,duncon ,
     &             grsize(1,1,1)  ,sedexp ,duneh  ,dunel  ,deff    )
c
c     De dikte van de transportlaag is juist bepaald. Bepaal hierbij de
c     definitieve waarde van N(i) en dzr(i) bij level zb(i)-deff(i,2)
c     en de nieuwe samenstelling van ptrla(i,j,2).
c     Indien ook een exchangelaag aanwezig, dan deze procedure herhalen
c     en de nieuwe samenstelling van pexla(i,j,2).
c
      if (nunlay .gt. 1) then
c
          thexfp = 1.0
          if (nlayer .eq. 2) then
             thexfp = 1.0 + thexfa
             do 5 i=1,ngrid
                deff(i,1) = thexfa*deff(i,2)
    5        continue
          endif
c
          do 18 igr=1,ngrid
             if (afwfqs(igr,3) .lt. ws(igr)) then
                wsact = afwfqs(igr,3)
             else
                wsact = ws(igr)
             endif
c
             if (nvast.eq.1) then
                deffcr = thexfp*deff(igr,2)
                if (deffcr.lt.zbeps) then
                   write (jugralg,*) 
     +                  ' gsdula : total layer thickness < zbeps'
                   deffcr = zbeps
                endif
                if (zbave(igr)-deffcr .lt. zbfl(igr) ) then
                   dzhulp      = (zbave(igr) - zbfl(igr))
                   if (dzhulp.lt.zbeps) then
                      write(jugralg,*) ' gsdula - deffec < zbeps ',igr
                      dzhulp     = zbeps
                      zbave(igr) = zbfl(igr) + zbeps
                   endif
                   deff(igr,2) = dzhulp / thexfp
                   if (nlayer .eq. 2)
     &                deff(igr,1)= thexfa * deff(igr,2)
                endif
             endif
c
             dtref = deff(igr,2)*wsact/ws(igr)
             mt = 0
             do 6 i=1,nunlay
                dzrt = dzr(igr)-dtref+mt*dzunla
                if (dzrt .gt. 0.) goto 7
                mt = mt+1
    6        continue
    7        continue
c
             if (mt .gt. 0) then
                if (nrdzdl(igr)-mt .lt. 1) then
c                   too low top level of sublayer
                    write(jugralg,*) ' '
                    write(jugralg,'(1x,a,e10.4,a,/,1x,a,i3)')
     .                 'Occurring top of sublayer less than one step'
     +                 //'(= ',dzunla,' )',
     .                 'above reference level at grid point i= ',igr
                    dzref = (nrdzdl(igr)-mt-1)*dzunla + dzrt
                    if (nlayer.eq.2) dzref = dzref+ deff(igr,1)
                    write(jugralg,'(1x,a,1x,e10.4)')
     .                 'Top of sublayer above ref. level= ',dzref
                    write(jugralg,'(1x,a,1x,e10.4)')
     .                 'z of reference level = ',levunl(igr)
                    goto 1000
                endif
                sum1 = 0.
                do 9 j=1,nfrac
                   sum = 0.
                   do 8 jj=nrdzdl(igr)-mt,nrdzdl(igr)-1
                      sum = sum+p0la(igr,j,jj)
    8              continue
                   ptrla(igr,j,2) = (dzr(igr)*p0la(igr,j,nrdzdl(igr)) -
     &                               dzrt*p0la(igr,j,nrdzdl(igr)-mt)  +
     &                               dzunla*sum) / dtref
                   sum1 = sum1 + ptrla(igr,j,2)
    9           continue
                do 109 j=1,nfrac
                   ptrla(igr,j,2) = ptrla(igr,j,2) / sum1
  109           continue
                nrdzdl(igr) = nrdzdl(igr)-mt
             endif
             dzr(igr) = dzrt
c
             if (nlayer .eq. 2) then
                dexef = deff(igr,1)*wsact/ws(igr)
                me = 0
                do 11 i=1,nunlay
                  dzre = dzr(igr)-dexef+me*dzunla
                  if (dzre .gt. 0.) goto 12
                     me = me+1
   11           continue
   12           continue
                if (me .gt. 0) then
                   if (nrdzdl(igr)-me .lt. 1) then
c                   too low top level of sublayer
                      write(jugralg,*) ' '
                      write(jugralg,'(1x,a,e10.4,a,/,1x,a,i3)')
     .                 'Occurring top of sublayer less than one step'
     +                 //'(= ',dzunla,' )',
     .                  'above reference level at grid point i= ',igr
                      write(jugralg,'(1x,a,1x,e10.4)')
     .                  'Top of sublayer above ref. level= ',
     .                  (nrdzdl(igr)-me-1)*dzunla + dzre
                      write(jugralg,'(1x,a,1x,e10.4)')
     .                  'z of reference level = ',levunl(igr)
                      goto 1000
                   endif
                   sum1 = 0.
                   do 14 j=1,nfrac
                      sum = 0.
                      do 13 jj=nrdzdl(igr)-me,nrdzdl(igr)-1
                         sum = sum+p0la(igr,j,jj)
   13                 continue
                      pexla(igr,j,2) = (dzr(igr)*p0la(igr,j,nrdzdl(igr)) 
     &                                 -dzre*p0la(igr,j,nrdzdl(igr)-me)+
     &                                 dzunla*sum) / dexef
                      sum1 = sum1 + pexla(igr,j,2)
   14              continue
                   do 16 j=1,nfrac
                      pexla(igr,j,2) = pexla(igr,j,2) / sum1
   16              continue
                   nrdzdl(igr) = nrdzdl(igr)-me
                else
                   do 17 j=1,nfrac
                      pexla(igr,j,2) = p0la(igr,j,nrdzdl(igr))
   17              continue
                endif
                dzr(igr) = dzre
             endif
   18     continue
c 
         write(jugralg,*) ' '
         write(jugralg,*) ' Topside of underlayer '
         write(jugralg,*) ' igr, nrdzdl, dzr, ws, deff'
         do 140 igr=1,ngrid
            write(jugralg,'(1x,i3,1x,i2,3(1x,e10.4))')
     .            igr, nrdzdl(igr),dzr(igr),ws(igr),deff(igr,2)
  140    continue
         write(jugralg,*) ' '
         write(jugralg,*) ' Frequency distribution of transport layer'//
     .                    ' at start'
         do 150 igr=1,ngrid
            write(jugralg,'(1x,i3,6(1x,e10.4))')
     .                      igr,(ptrla(igr,j,2),j=1,nfrac)          
  150    continue
         if (nlayer.eq.2) then
            write(jugralg,*) ' '
            write(jugralg,*) ' Frequency distribution of exchange layer'
     .                     //' at start'
            do 155 igr=1,ngrid
               write(jugralg,'(1x,i3,6(1x,e10.4))')
     .                         igr,(pexla(igr,j,2),j=1,nfrac)          
  155       continue
         endif
         write(jugralg,*) ' '
         write(jugralg,*)
     +       ' Frequency distribution of underlayer at start'
         do 160 igr=1,ngrid
         do 160 jj =1,nrdzdl(igr)
            write(jugralg,'(1x,i3,1x,i2,6(1x,e10.4))')
     .                      igr,jj,(p0la(igr,j,jj),j=1,nfrac)          
  160    continue
c 
      endif
c
      if (nonngp.eq.1) then
         do 190 igr=1,ngrid
            if (afwfqs(igr,3) .lt. ws(igr)) then
               wsact = afwfqs(igr,3)
            else
               wsact = ws(igr)
            endif
            do 180 j=1,nfrac
               deltar(igr,j)= wsact*deff(igr,2)*ptrla(igr,j,2)
  180       continue
  190    continue
      endif
c 
c     Finally the initial mixture is determined so calculate the characteristic
c     grain sizes.
c
      do ibr=1,nbran
         call gschar (ibr    ,nfrac  ,nlayer ,nbran  ,ngrid  ,branch ,
     &                ddis   ,dfrac  ,ptrla(1,1,2)   ,pexla(1,1,2)   ,
     &                grsize ,dmed0  ,p0la   ,nrdzdl ,trform ,sedexp ,
     &                nunlay )
      enddo
c
c     Set array with previous characteristic diameters equal to
c     array with current grain sizes.
c
      do 120 igr=1,ngrid
         dmed0(igr) = grsize(dmed,igr,1)
  120 continue
c
      do 30 ibr=1,nbran
         if ( rouopt .eq. rouryn) bfrict (1,ibr) = 0
   30 continue
c
      return

 1000 ker = fatal
      call getloc (igr,ibr,xc)
      write (txtx,'(f10.2)') xc
      call getbrn (ibr,branam,lbrnam)
      call error ( juer,'GSDULA too low top level of sublayer @'
     +                  //branam(:lbrnam)//'@ @'
     +                  //txtx//'@' ,egsubtp, ker )
      end
