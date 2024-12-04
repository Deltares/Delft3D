subroutine gsini (nbran  ,ngrid  ,nfrac  ,nlayer ,g      ,nunlay ,&
&maxlev ,jugraut,jugralg,itim   ,newres ,ncelse ,&
&gsopts ,sedpar ,sedtra ,ddis   ,dfrac  ,grsize ,&
&dmed0  ,nucoef ,uscoef ,trform ,forcon ,duncon ,&
&disgse ,p0la   ,ptrla  ,pexla  ,depos  ,deff   ,&
&duneh  ,dunel  ,levunl ,hlev   ,nrdzdl ,dzr    ,&
&cp     ,afwfqs ,nlev   ,ws     ,wft    ,branch ,&
&sedexp ,deltar ,zbave  ,zbfl   ,nonall ,nnelvl ,&
&nellvl ,sedini ,lanrinbt       ,fd_nefis_rst ,&
&fd_nefis_new ,bfrict ,juer   ,ker    )
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsini.F,v $
! Revision 1.5  1996/06/07  11:56:24  kuipe_j
! multi  +  fixed layer
!
! Revision 1.4  1996/01/08  13:29:55  kuipe_j
! Multi layer option for under layer added
!
! Revision 1.2  1995/09/27  10:12:29  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!  Module:  GSINI (Graded Sediment INItialization)
!
   include '..\include\sobdim.i'
   include '..\include\errcod.i'
   include '..\include\filsim.i'
!
!     Declaration of parameters
!
   integer    nbran ,ngrid   ,nlayer     ,nfrac  ,nunlay ,maxlev    ,&
   &nnelvl,jugraut ,jugralg    ,juer   ,ker    ,nucoef
   integer    nrdzdl(ngrid)  ,lanrinbt(ngrid)    ,nlev(ngrid)       ,&
   &branch(4,nbran),nonall(3,nbran)    ,bfrict(3,nbran)   ,&
   &gsopts(*)    ,sedini(*)         ,ncelse(3)         ,&
   &itim(2), fd_nefis_rst , fd_nefis_new
   real       g
   real       uscoef(nrcoefs,nucoef)  ,sedpar(*)                    ,&
   &duncon(*)               ,trform(3,nbran)              ,&
   &ddis  (nfrac+1)         ,dfrac (nfrac)                ,&
   &disgse(nfrac,2,nbran)   ,sedtra(nfrac)                ,&
   &grsize(4,ngrid,nlayer+1),forcon(nfrac+1,4,nbran)      ,&
   &ptrla (ngrid,nfrac,2)   ,pexla (ngrid,nfrac,2)        ,&
   &deltar(ngrid,nfrac)     ,sedexp(ngrid)                ,&
   &levunl(ngrid)           ,&
   &dzr   (ngrid)           ,p0la  (ngrid,nfrac,nunlay)   ,&
   &cp    (ngrid,4)         ,afwfqs(ngrid,8)              ,&
   &duneh (ngrid)           ,dunel (ngrid)                ,&
   &deff  (ngrid,2)         ,&
   &ws    (ngrid)           ,wft   (ngrid,maxlev)         ,&
   &zbave (ngrid)           ,zbfl  (ngrid)                ,&
   &dmed0 (ngrid)           ,nellvl(nnelvl)
   double precision hlev  (ngrid,maxlev)
   logical    newres
   logical    depos (ngrid)
!
!     Declaration of local parameters
!
   integer    ibr    ,igr    ,jf     ,i      ,j      ,k      ,&
   &heiopt ,ind    ,nonngp ,nvast  ,nfout  ,&
   &nchfla ,lbrnam ,ifil   ,rouopt
   real       velo   ,chezy  ,depth  ,hrad   ,frou2  ,alffl  ,&
   &pacfac ,relden ,kinvis ,dmedi  ,dzunla ,level  ,&
   &zbeps  ,xc
   real       duncof (2)
   logical    incall ,inires,first
   character*10       txtx
   character*40       branam
!
!     Constants
!
!     Dune height option Gill
   integer     dhgill
   parameter  (dhgill=1)
   integer    rouryn
   parameter (rouryn=1)
!
   kinvis    = sedpar(1)
   relden    = sedpar(2)
   pacfac    = sedpar(3)
   alffl     = sedpar(4)
   duncof(1) = sedpar(6)
   duncof(2) = sedpar(7)
   dzunla    = sedpar(11)
   zbeps     = sedpar(13)

   rouopt    = gsopts(3)
!
   nfout = 0
   ker   = ok

   if (nrcoefs.ne.20)stop 'MDA/MDF files made with old TXT-parser'
!
   open ( unit = jugraut, file = graout )
   open ( unit = jugralg, file = gralog )
!
!     Set initial flags for HIS files
!
   do ifil=1,4
      sedini(ifil)=0
   enddo
!
!     Initialize Deposition flag. Flag will be overruled by restart
!
   do igr=1,ngrid
      depos(igr) = .true.
   enddo
!
   first = .true.
   call gsrsta (ngrid  ,nfrac  ,nunlay ,nlayer ,itim  ,ptrla(1,1,2) ,&
   &pexla(1,1,2)   ,p0la   ,nrdzdl ,lanrinbt     ,zbave ,&
   &zbfl   ,levunl ,dzr    ,deff(1,2)     ,dmed0 ,depos ,&
   &newres ,fd_nefis_rst, fd_nefis_new, juer  ,first ,&
   &ncelse(3) ,inires ,ker    )
   if (ker .eq. fatal) goto 1000
!
!     Check if sediment width was specified.
!
   do igr=1,ngrid
      if (ws(igr) .lt. 1.e-6) then
         nfout = 2
         goto 1000
      endif
   enddo
!
!     Calculate diameter per fraction from given distribution table
!
   do jf=1,nfrac
      dfrac(jf) = sqrt(ddis(jf)*ddis(jf+1))
   enddo
!
!     Probably not necessary
!
   do jf=1,nfrac
      do j=1,nbran
         do i=1,2
            disgse(jf,i,j) = 0.
         enddo
      enddo
   enddo
!
!     Set time independent constants initialy to zero.
!     This avoids undefined values when not all constants are defined.
!
   do k=1,nbran
      do j=1,4
         do i=1,nfrac+1
            forcon(i,j,k) = 0.
         enddo
      enddo
   enddo
!
   do i=1,nfrac
      sedtra(i) = 0.
   enddo
!
!     Set flags
!
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
!
   heiopt    = gsopts(1)
!      lathic    = gsopts(4)
   nonngp    = gsopts(8)
!
!     Initialize parameters that are not used when the transport
!     formulas etc. are called in initial mode.
!
   velo   = 0.
   frou2  = 0.
   chezy  = 0.
   depth  = 0.
   hrad   = 0.
   dmedi  = 0.
   nchfla = 2
   nvast  = 0
!
   do igr=1,ngrid
      sedexp(igr)= 0.
   enddo
!
   incall = .true.
   do 110 ibr=1,nbran
!
      ind  = max(int(trform(2,ibr)),1)
      do 105 igr= branch(3,ibr),branch(4,ibr)
         nchfla = nonall(2,ibr)

!
!           Calculate constants for all fractions
!           for transport formulae
!           1e parameter dfrac in gstrfo will be used as dummy
!
         call gstrfo (incall  ,nfrac ,g       ,pacfac  ,relden ,&
!                       <Char. grain size (unused)>
         &kinvis  ,dfrac ,chezy   ,velo    ,depth  ,&
         &hrad    ,uscoef(1,ind)  ,trform(1,ibr)   ,&
         &dfrac   ,forcon(1,1,ibr),sedtra  ,&
         &sedexp(igr)    ,nvast   ,alffl   ,&
         &zbave(igr)     ,zbfl(igr)        ,nchfla ,&
!                                       <ptrla2>
         &ngrid  ,igr    ,ptrla(1,1,2)  )
!
!           Calculate constants for dune height calculation
!
         if (heiopt .eq. dhgill)&
         &call gsdhgi (incall ,g    ,relden ,kinvis ,dmedi       ,&
         &chezy  ,velo ,depth  ,frou2  ,duncof(1)   ,&
         &duncof(2)    ,trform (1,ibr) ,duncon(1)   ,&
         &sedexp(igr)  ,duneh(igr)     )

105   continue
110 continue
!
!     Initial run
!     ===========
!     Bereken een eerste schatting van N(i) en dzr(i) op basis van
!     bekende bodemligging zb(i) in ieder roosterpunt.
!     Als het aantal onderlagen 1 is, rekenen met standaard Ribberink
!
!     bepaling gemiddelde bodemligging Zbave en hoogte vaste laag Zbfl

!     CAUTION: dzr and nrdzdl contain values corresponding to Zb and not
!              to lower side of transport or exchange layer, when leaving
!              the subroutine. In gsinmix the values will be corresponding
!              to lower side of layers.
!
   if (inires) then
      call gsinbot (nbran  ,ngrid  ,nfrac  ,nlayer ,g      ,&
      &nunlay ,gsopts ,nvast  ,maxlev ,jugralg,&
      &sedpar ,ddis   ,trform ,grsize ,duncon ,&
      &dfrac  ,p0la   ,ptrla  ,deff   ,duneh  ,&
      &dunel  ,levunl ,hlev   ,nrdzdl ,dzr    ,&
      &cp     ,afwfqs ,nlev   ,ws     ,wft    ,&
      &branch ,sedexp ,deltar ,zbave  ,zbfl   ,&
      &nonall ,nnelvl ,nellvl ,lanrinbt       ,&
      &juer   ,ker    )
      if (ker .eq. fatal) goto 1000
!
      do igr=1,ngrid
         If (hlev(igr,1).LT.zbfl(igr)) Then
            write (jugralg,'(1x,a,a,i6)')&
            &' Fixed layer above lowest cross-section'//&
            &' level at grid pnt ',igr
         Endif
      enddo
   else
!
!        Restart run
!        A.o. Zbave and Zbfl are read from restart.
!        Set flag for fixed layer.
!
      nvast = 0
      do igr=1,ngrid
         if (zbfl(igr).lt. -0.999e+15) nvast =1
      enddo
      do ibr=1,nbran
         if ( rouopt .eq. rouryn) bfrict (1,ibr) = 0
      enddo

   endif
!
   gsopts(9) = nvast
!
   if (nunlay.gt.1) then
      write(jugralg,*) ' '
      write(jugralg,*) ' Position of sub under layer nr ZERO'
      write(jugralg,*) ' '
      write(jugralg,*) ' igr, layer nr ,level '
      do igr=1,ngrid
         level = (lanrinbt(igr)-1)*dzunla + levunl(igr)
         write(jugralg,'(1x,2i5,3x,e12.6)')&
         &igr, lanrinbt(igr), level
      enddo
   endif

   return

1000 continue
   ker = fatal
   if (nfout.eq.2) then
      call getloc (igr,ibr,xc)
      write (txtx,'(f10.2)') xc
      call getbrn (ibr,branam,lbrnam)
      call error ( juer,'GSINI Sediment width = 0 @'//&
      &branam(:lbrnam)//'@ @'//&
      &txtx//'@' ,egsedw, ker )
   endif
!
end
