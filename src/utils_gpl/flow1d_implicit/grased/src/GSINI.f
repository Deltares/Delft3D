      subroutine gsini (nbran  ,ngrid  ,nfrac  ,nlayer ,g      ,nunlay ,
     &                  maxlev ,jugraut,jugralg,itim   ,newres ,ncelse ,
     &                  gsopts ,sedpar ,sedtra ,ddis   ,dfrac  ,grsize ,
     &                  dmed0  ,nucoef ,uscoef ,trform ,forcon ,duncon ,
     &                  disgse ,p0la   ,ptrla  ,pexla  ,depos  ,deff   ,
     &                  duneh  ,dunel  ,levunl ,hlev   ,nrdzdl ,dzr    ,
     &                  cp     ,afwfqs ,nlev   ,ws     ,wft    ,branch ,
     &                  sedexp ,deltar ,zbave  ,zbfl   ,nonall ,nnelvl ,
     &                  nellvl ,sedini ,lanrinbt       ,fd_nefis_rst ,
     &                  fd_nefis_new ,bfrict ,juer   ,ker    )
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsini.F,v $
c Revision 1.5  1996/06/07  11:56:24  kuipe_j
c multi  +  fixed layer
c
c Revision 1.4  1996/01/08  13:29:55  kuipe_j
c Multi layer option for under layer added
c
c Revision 1.2  1995/09/27  10:12:29  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c  Module:  GSINI (Graded Sediment INItialization)
c
      include '..\include\sobdim.i'
      include '..\include\errcod.i'      
      include '..\include\filsim.i' 
c
c     Declaration of parameters
c
      integer    nbran ,ngrid   ,nlayer     ,nfrac  ,nunlay ,maxlev    ,
     &           nnelvl,jugraut ,jugralg    ,juer   ,ker    ,nucoef 
      integer    nrdzdl(ngrid)  ,lanrinbt(ngrid)    ,nlev(ngrid)       ,
     &           branch(4,nbran),nonall(3,nbran)    ,bfrict(3,nbran)   ,
     &           gsopts(*)    ,sedini(*)         ,ncelse(3)         , 
     &           itim(2), fd_nefis_rst , fd_nefis_new
      real       g   
      real       uscoef(nrcoefs,nucoef)  ,sedpar(*)                    ,
     &           duncon(*)               ,trform(3,nbran)              ,
     &           ddis  (nfrac+1)         ,dfrac (nfrac)                ,
     &           disgse(nfrac,2,nbran)   ,sedtra(nfrac)                ,
     &           grsize(4,ngrid,nlayer+1),forcon(nfrac+1,4,nbran)      ,
     &           ptrla (ngrid,nfrac,2)   ,pexla (ngrid,nfrac,2)        ,
     &           deltar(ngrid,nfrac)     ,sedexp(ngrid)                ,
     &           levunl(ngrid)           ,
     &           dzr   (ngrid)           ,p0la  (ngrid,nfrac,nunlay)   ,
     &           cp    (ngrid,4)         ,afwfqs(ngrid,8)              ,
     &           duneh (ngrid)           ,dunel (ngrid)                , 
     &           deff  (ngrid,2)         ,
     &           ws    (ngrid)           ,wft   (ngrid,maxlev)         ,
     &           zbave (ngrid)           ,zbfl  (ngrid)                ,
     &           dmed0 (ngrid)           ,nellvl(nnelvl)
      double precision hlev  (ngrid,maxlev)
      logical    newres
      logical    depos (ngrid)
c
c     Declaration of local parameters
c
      integer    ibr    ,igr    ,jf     ,i      ,j      ,k      ,
     &           heiopt ,ind    ,nonngp ,nvast  ,nfout  ,
     &           nchfla ,lbrnam ,ifil   ,rouopt
      real       velo   ,chezy  ,depth  ,hrad   ,frou2  ,alffl  ,
     &           pacfac ,relden ,kinvis ,dmedi  ,dzunla ,level  ,
     &           zbeps  ,xc
      real       duncof (2)
      logical    incall ,inires,first
      character*10       txtx
      character*40       branam
c
c     Constants
c
c     Dune height option Gill
      integer     dhgill
      parameter  (dhgill=1)
      integer    rouryn
      parameter (rouryn=1)
c
      kinvis    = sedpar(1)
      relden    = sedpar(2)
      pacfac    = sedpar(3)
      alffl     = sedpar(4)
      duncof(1) = sedpar(6)
      duncof(2) = sedpar(7)
      dzunla    = sedpar(11)
      zbeps     = sedpar(13)
      
      rouopt    = gsopts(3)
c
      nfout = 0 
      ker   = ok
      
      if (nrcoefs.ne.20)stop 'MDA/MDF files made with old TXT-parser'
c
      open ( unit = jugraut, file = graout )
      open ( unit = jugralg, file = gralog )
c
c     Set initial flags for HIS files
c
      do ifil=1,4
         sedini(ifil)=0
      enddo
c
c     Initialize Deposition flag. Flag will be overruled by restart
c
      do igr=1,ngrid
         depos(igr) = .true.
      enddo
c
      first = .true.
      call gsrsta (ngrid  ,nfrac  ,nunlay ,nlayer ,itim  ,ptrla(1,1,2) ,
     &             pexla(1,1,2)   ,p0la   ,nrdzdl ,lanrinbt     ,zbave ,
     &             zbfl   ,levunl ,dzr    ,deff(1,2)     ,dmed0 ,depos ,
     &             newres ,fd_nefis_rst, fd_nefis_new, juer  ,first ,
     &             ncelse(3) ,inires ,ker    )
      if (ker .eq. fatal) goto 1000
c
c     Check if sediment width was specified.
c
      do igr=1,ngrid
         if (ws(igr) .lt. 1.e-6) then
            nfout = 2
            goto 1000
         endif
      enddo 
c      
c     Calculate diameter per fraction from given distribution table
c
      do jf=1,nfrac
         dfrac(jf) = sqrt(ddis(jf)*ddis(jf+1))
      enddo
c
c     Probably not necessary
c
      do jf=1,nfrac
         do j=1,nbran
            do i=1,2
               disgse(jf,i,j) = 0.
            enddo
         enddo 
      enddo
c
c     Set time independent constants initialy to zero.
c     This avoids undefined values when not all constants are defined.
c
      do k=1,nbran
         do j=1,4
            do i=1,nfrac+1
               forcon(i,j,k) = 0.
            enddo
         enddo 
      enddo
c
      do i=1,nfrac
         sedtra(i) = 0.
      enddo
c
c     Set flags
c
      if (nunlay.gt.1) then
         if (zbeps .gt. 0.05*dzunla) then
            zbeps = 0.05*dzunla
            sedpar(13)= zbeps
         endif
      else
         if (zbeps.lt.0.0001) then
            zbeps = 0.0001     
            sedpar(13)= zbeps
         endif
      endif
c
      heiopt    = gsopts(1)
c      lathic    = gsopts(4)
      nonngp    = gsopts(8)
c
c     Initialize parameters that are not used when the transport
c     formulas etc. are called in initial mode.
c
      velo   = 0.
      frou2  = 0.
      chezy  = 0.
      depth  = 0.
      hrad   = 0.
      dmedi  = 0.
      nchfla = 2
      nvast  = 0
c
      do igr=1,ngrid
         sedexp(igr)= 0.
      enddo
c
      incall = .true.
      do 110 ibr=1,nbran
c
         ind  = max(int(trform(2,ibr)),1)
         do 105 igr= branch(3,ibr),branch(4,ibr)
            nchfla = nonall(2,ibr)

c
c           Calculate constants for all fractions
c           for transport formulae
c           1e parameter dfrac in gstrfo will be used as dummy
c
            call gstrfo (incall  ,nfrac ,g       ,pacfac  ,relden ,
c                       <Char. grain size (unused)>
     &                   kinvis  ,dfrac ,chezy   ,velo    ,depth  ,
     &                   hrad    ,uscoef(1,ind)  ,trform(1,ibr)   ,
     &                   dfrac   ,forcon(1,1,ibr),sedtra  ,
     &                   sedexp(igr)    ,nvast   ,alffl   ,
     &                   zbave(igr)     ,zbfl(igr)        ,nchfla ,
c                                       <ptrla2>
     &                   ngrid  ,igr    ,ptrla(1,1,2)  )
c
c           Calculate constants for dune height calculation
c
            if (heiopt .eq. dhgill) 
     &      call gsdhgi (incall ,g    ,relden ,kinvis ,dmedi       ,
     &                   chezy  ,velo ,depth  ,frou2  ,duncof(1)   ,
     &                   duncof(2)    ,trform (1,ibr) ,duncon(1)   ,
     &                   sedexp(igr)  ,duneh(igr)     )
      
  105    continue
  110 continue
c
c     Initial run
c     ===========
c     Bereken een eerste schatting van N(i) en dzr(i) op basis van
c     bekende bodemligging zb(i) in ieder roosterpunt.
c     Als het aantal onderlagen 1 is, rekenen met standaard Ribberink
c
c     bepaling gemiddelde bodemligging Zbave en hoogte vaste laag Zbfl

c     CAUTION: dzr and nrdzdl contain values corresponding to Zb and not
c              to lower side of transport or exchange layer, when leaving
c              the subroutine. In gsinmix the values will be corresponding
c              to lower side of layers.
c
      if (inires) then
         call gsinbot (nbran  ,ngrid  ,nfrac  ,nlayer ,g      ,
     &                 nunlay ,gsopts ,nvast  ,maxlev ,jugralg,
     &                 sedpar ,ddis   ,trform ,grsize ,duncon ,
     &                 dfrac  ,p0la   ,ptrla  ,deff   ,duneh  ,
     &                 dunel  ,levunl ,hlev   ,nrdzdl ,dzr    ,
     &                 cp     ,afwfqs ,nlev   ,ws     ,wft    ,
     &                 branch ,sedexp ,deltar ,zbave  ,zbfl   ,
     &                 nonall ,nnelvl ,nellvl ,lanrinbt       ,
     &                 juer   ,ker    )
         if (ker .eq. fatal) goto 1000
c    
         do igr=1,ngrid
            If (hlev(igr,1).LT.zbfl(igr)) Then
                write (jugralg,'(1x,a,a,i6)')
     +                 ' Fixed layer above lowest cross-section'//
     +                 ' level at grid pnt ',igr
            Endif 
         enddo
      else
c
c        Restart run
c        A.o. Zbave and Zbfl are read from restart.
c        Set flag for fixed layer.
c
         nvast = 0
         do igr=1,ngrid
            if (zbfl(igr).lt. -0.999e+15) nvast =1
         enddo
         do ibr=1,nbran
            if ( rouopt .eq. rouryn) bfrict (1,ibr) = 0
         enddo   
         
      endif
c
      gsopts(9) = nvast
c
      if (nunlay.gt.1) then
         write(jugralg,*) ' '
         write(jugralg,*) ' Position of sub under layer nr ZERO'
         write(jugralg,*) ' '
         write(jugralg,*) ' igr, layer nr ,level ' 
         do igr=1,ngrid
            level = (lanrinbt(igr)-1)*dzunla + levunl(igr)  
            write(jugralg,'(1x,2i5,3x,e12.6)')
     +                    igr, lanrinbt(igr), level
         enddo
      endif

      return
   
 1000 continue
      ker = fatal
      if (nfout.eq.2) then
         call getloc (igr,ibr,xc)
         write (txtx,'(f10.2)') xc
         call getbrn (ibr,branam,lbrnam)
         call error ( juer,'GSINI Sediment width = 0 @'//
     +                      branam(:lbrnam)//'@ @'//
     +                      txtx//'@' ,egsedw, ker )
      endif
c  
      end
