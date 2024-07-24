      subroutine gsrimo ( istep  ,time   ,itim   ,dtf    ,steady ,
     +                    juer   ,juresi ,jufrou ,juresd ,justrd ,
     +                    ker    ,inocon ,jusold ,lfrou  ,itstat ,
     +                    frobuf ,jugraut,jugralg,lrest)
c
c     Parameters
c
      integer  istep ,itim(2) ,ker   ,juer ,juresi ,jufrou ,
     +         juresd, justrd ,inocon,jusold,itstat(4)     ,
     +         jugraut,jugralg 
      real     frobuf (8)
      double   precision       time  ,dtf
      logical  steady         ,lfrou ,lrest
c
c     Pointers
c     --------
c
c     Pointers for flow, sediment and morphology module
c
      integer aft   ,afwfqs,alfab ,att   ,bfricp,bfrict,bgout ,
     +        branch,brnode,celer ,cpack ,engpar,forcon,flwdir,
     +        grid  ,grsize,hpack ,hlev  ,izwft ,maxlev,maxtab,
     +        mbdpar,mltpar,morcon,morpar,nboun ,nbran ,nbrnod,
     +        ngrid ,nlev  ,nmlat ,nnode ,node  ,nonall,nqlat ,
     +        ntab  ,ntabm ,nucoef,of    ,prslot,psltvr,qpack ,
     +        qlat  ,qltpar,rpack ,sdrdbf,sectc ,sectv ,seddb ,
     +        sedinf,sedpar,sedtr ,table ,trform,typcr ,uscoef,
     +        waoft ,wft   ,ws    ,wtt   ,x  
c
c     Pointers for graded sediment module
c
      integer celert,cela1 ,deff  ,depos ,deltaa,deltar,ddis  ,
     +        dfrac ,disgse,dmed0 ,duncon,duneh ,dunel ,dzr   ,
     +        grain ,gsopts,grsizmun     ,intgr ,lanrinbt     ,
     +        levunl,nfrac ,ngrain,nlayer,nrdzdl,ntmpfr,nunlay,
     +        pdiacc,pexla ,ptrla ,p0la  ,sedexp,sedini,source,
     +        submin,subplus      ,sumda ,tmpfr ,wfsold,zbave ,
     +        zbfl
c
c     Run parameters
c     --------------
c
c     Run parameters flow module
c
      real     g 
c
c     Run parameters morphology module
c
      real     alphcg
c
c     Local variables
c
      integer  ixpar  ,timnp1(2) ,moitmx ,
     +         cnt    ,m      ,filstp    ,cpredn ,igpmc 
      double   precision       tnp1      ,ldtf
      logical  lastts ,lsalt  ,lkalm     ,
c     mozart   variables 
     +         lmozad, lgrwt
      integer  nstepd      
      
      character*4      txt
      character*18     txt1
c
c     External functions
c
      integer  gtipnt, gtlpnt, gtrpnt, gtdpnt, soipar
      real     sorpar
      external gtipnt, gtlpnt, gtrpnt, gtdpnt, soipar, sorpar
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     Sediment module should process morphodynamic boundary conditions
c
c     Extract parameters from flwpar
c
      ixpar = gtrpnt ( 'FLWPAR' )
c
      g      = sorpar ( rp(ixpar), 1 )
c
c     Extract parameters from morpar
c
      ixpar  = gtrpnt ( 'MORPAR' )
c
      moitmx = soipar ( rp(ixpar), 2 )
      alphcg = sorpar ( rp(ixpar),12 )
c
c     Find starting addresses of working arrays
c
c     Single variables are read from the memory pool to simplify the
c     call for the Microsoft Fortran compiler v5
c
      aft    =     gtrpnt ( 'AFT'   )
      afwfqs =     gtrpnt ( 'AFWFQS')
      alfab  =     gtrpnt ( 'ALFAB' )
      att    =     gtrpnt ( 'ATT'   )
      bfricp =     gtrpnt ( 'BFRICP')
      bfrict =     gtipnt ( 'BFRICT')
      bgout  =     gtipnt ( 'BGOUT' )
      branch =     gtipnt ( 'BRANCH')
      brnode =     gtipnt ( 'BRNODE')
      celer  =     gtrpnt ( 'CELER' )
      cpack  =     gtrpnt ( 'CPACK' )
      engpar =     gtrpnt ( 'ENGPAR')
      flwdir =     gtipnt ( 'FLWDIR')
      forcon =     gtrpnt ( 'FORCON')
      grid   =     gtipnt ( 'GRID'  )
      grsize =     gtrpnt ( 'GRSIZE')
      hlev   =     gtdpnt ( 'HLEV'  )
      hpack  =     gtdpnt ( 'HPACK ')
      izwft  =     gtrpnt ( 'IZWFT' )
      maxlev = ip (gtipnt ( 'MAXLEV'))
      maxtab = ip (gtipnt ( 'MAXTAB'))
      mbdpar =     gtipnt ( 'MBDPAR')
      mltpar =     gtrpnt ( 'MLTPAR')
      morcon =     gtrpnt ( 'MORCON')
      morpar =     gtrpnt ( 'MORPAR')
      nboun  = ip (gtipnt ( 'NBOUN' ))
      nbran  = ip (gtipnt ( 'NBRAN' ))
      nbrnod = ip (gtipnt ( 'NBRNOD'))
      ngrid  = ip (gtipnt ( 'NGRID' ))
      nlev   =     gtipnt ( 'NLEV'  )
      nmlat  = ip (gtipnt ( 'NMLAT' ))
      nnode  = ip (gtipnt ( 'NNODE' ))
      node   =     gtipnt ( 'NODE'  )
      nonall =     gtipnt ( 'NONALL')
      nqlat  = ip (gtipnt ( 'NQLAT' ))
      ntab   =     gtipnt ( 'NTAB'  )
      ntabm  = ip (gtipnt ( 'NTABM' ))
      of     =     gtrpnt ( 'OF'    )
      prslot =     gtrpnt ( 'PRSLOT')
      psltvr =     gtrpnt ( 'PSLTVR')
      qlat   =     gtrpnt ( 'QLAT'  )
      qltpar =     gtrpnt ( 'QLTPAR')
      qpack  =     gtdpnt ( 'QPACK' )
      rpack  =     gtrpnt ( 'RPACK' )
      sdrdbf =     gtipnt ( 'SDRDBF')
      sectc  =     gtrpnt ( 'SECTC' )
      sectv  =     gtrpnt ( 'SECTV' )
      seddb  =     gtipnt ( 'SEDDB' )
      sedinf =     gtipnt ( 'SEDINF')
      sedpar =     gtrpnt ( 'SEDPAR')
      sedtr  =     gtrpnt ( 'SEDTR' )
      table  =     gtrpnt ( 'TABLE' )
      trform =     gtrpnt ( 'TRFORM')
      typcr  =     gtipnt ( 'TYPCR' )
      uscoef =     gtrpnt ( 'USCOEF')
      waoft  =     gtrpnt ( 'WAOFT' )
      wft    =     gtrpnt ( 'WFT'   )
      ws     =     gtrpnt ( 'WS'    )
      wtt    =     gtrpnt ( 'WTT'   )
      x      =     gtrpnt ( 'X'     )
c
c     Define pointers for graded sediment module
c
      celert =     gtrpnt ( 'CELERT')
      cela1  =     gtrpnt ( 'CELA1' )
      deff   =     gtrpnt ( 'DEFF'  )
      depos  =     gtlpnt ( 'DEPOS' )
      deltaa =     gtdpnt ( 'DELTAA')
      deltar =     gtrpnt ( 'DELTAR')
      ddis   =     gtrpnt ( 'DDIS'  )
      dfrac  =     gtrpnt ( 'DFRAC' )
      disgse =     gtrpnt ( 'DISSED')
      dmed0  =     gtrpnt ( 'DMED0' )
      duncon =     gtrpnt ( 'DUNCON')
      duneh  =     gtrpnt ( 'DUNEH' )
      dunel  =     gtrpnt ( 'DUNEL' )
      dzr    =     gtrpnt ( 'DZR'   )
      gsopts =     gtipnt ( 'GSOPTS')
      grsizmun =   gtrpnt ( 'GRSIZMUN')
      intgr  =     gtrpnt ( 'INTGR' )
      lanrinbt =   gtipnt ( 'LANRINBT')        
      levunl =     gtrpnt ( 'LEVUNL')
      nfrac  = ip (gtipnt ( 'NFRAC' ))
      ngrain = ip (gtipnt ( 'NGRAIN'))
      nlayer = ip (gtipnt ( 'NLAYER'))
      nrdzdl =     gtipnt ( 'NRDZDL')
      ntmpfr = ip (gtipnt ( 'NTMPFR'))
      nucoef = ip (gtipnt ( 'NUCOEF'))
      nunlay = ip (gtipnt ( 'NUNLAY'))
      pdiacc =     gtrpnt ( 'PDIACC')
      pexla  =     gtrpnt ( 'PEXLA' )
      ptrla  =     gtrpnt ( 'PTRLA' )
      p0la   =     gtrpnt ( 'P0LA'  )
      sedexp =     gtrpnt ( 'SEDEXP')
      sedini =     gtipnt ( 'SEDINI')
      source =     gtrpnt ( 'SOURCE')
      submin = ip (gtipnt ( 'SUBMIN'))
      subplus= ip (gtipnt ( 'SUBPLUS'))
      sumda  =     gtrpnt ( 'SUMDA' )
      tmpfr  =     gtrpnt ( 'TMPFR' )
      wfsold =     gtrpnt ( 'WFSOLD')
      zbave  =     gtrpnt ( 'ZBAVE' )
      zbfl   =     gtrpnt ( 'ZBFL'  )
c
c     Determine time N+1
c
      tnp1 = time + dtf
      timnp1(1) = itim(1)
      timnp1(2) = itim(2)
      call sotime ( timnp1, dtf )
      cnt  = 0
c
c     Repeat until time step is completed
c
c===================>
 100  continue
c
c        Calculate current delta time step
c
         ldtf = tnp1 - time
c
c        Determine courant number and max step morphology module
c

         call mocour (    nbran  ,   ngrid  ,
     +                 ip(branch),ip(typcr) ,
     +                 ip(grid)  ,rp(celert) ,rp(x)     ,
     +                    alphcg ,sngl(ldtf),
     +                    m      ,igpmc     )
CJK      WRITE (*,*) 'MOCOUR m=',m
c
c        Check if time step should be reduced
c
         if (m .gt. 1) then
c
c           Reduce time step
c
            ldtf    = ldtf / m
            time    = time + ldtf
            call sotime ( itim, ldtf )
            cnt     = cnt + 1
            lastts  = .false.
         else
            time    = tnp1
            itim(1) = timnp1(1)
            itim(2) = timnp1(2)
            lastts  = .true.
         endif
c
c        Shift variables of time n+1 to time n
c
         call gsshda(   ngrid  ,   nfrac  ,   nlayer ,
     +               rp(ptrla) ,rp(pexla) ,rp(deff)  )
c
c        Determine flow direction at each grid point
c
         call SOSDIR( dp(qpack) , ngrid , ip(flwdir) )
c
c        Calculate new bottom
c
         call gmorph (ngrid  ,   nbran  ,   nboun  ,   nnode  ,
     +                nbrnod ,   ntmpfr ,   nfrac  ,   time   ,
     +                ldtf   ,
     +             rp(prslot),
     +             rp(sedpar),rp(morpar),ip(gsopts),ip(flwdir),
     +             ip(grid)  ,ip(branch),ip(node)  ,
     +             ip(brnode),ip(bgout) ,rp(tmpfr) ,
     +                maxtab ,   ntabm  ,ip(ntab)  ,rp(table) ,
     +             ip(mbdpar),rp(x)     ,ip(typcr) ,   maxlev ,
     +             ip(nlev)  ,dp(hlev)  ,dp(hpack) ,rp(afwfqs),
     +             rp(wft)   ,rp(ws)    ,rp(celer) ,
     +             rp(celert),rp(sedtr) ,rp(disgse),rp(source),
     +             dp(deltaa),rp(dfrac) ,rp(cela1) ,rp(intgr) ,
     +                itim   ,   juer   ,   ker    ,rp(wfsold),
     +             rp(deltar),rp(zbave) ,rp(zbfl)  ,jugralg   ,
     +             rp(sumda) )
c
c        Test for fault
c
         if (ker .eq. fatal) then
            goto 900
         endif
c
c        Calculate derived variables
c
         lsalt = .false.
         call cstabl (    ngrid  ,   maxlev ,ip(nlev)  ,dp(hlev)  ,
     +                 rp(wft)   ,rp(aft)   ,rp(wtt)   ,rp(att)   ,
     +                 rp(of)    ,   lsalt  ,rp(izwft) ,   nbran  ,
     +                 ip(branch),ip(typcr) ,rp(sectc) ,rp(sectv) ,
     +                 rp(prslot),rp(psltvr),ip(bfrict),rp(bfricp),
     +                 rp(engpar),rp(grsize),   maxtab ,   ntabm  ,
     +                 ip(ntab)  ,rp(table) ,rp(ws)    ,   .true. ,
     +                    juer   ,   ker    )
c
c        Test for fault
c
         if (ker .eq. fatal) then
            goto 900
         endif

        filstp = 0
        cpredn = 0
        lkalm  = .false.
c       mozart parameters
        lmozad = .false.
        lgrwt  = .false.
        nstepd = 0
c
        call  SOFLOW ( istep  ,time   ,itim   ,dtf    ,filstp ,
     +                 cpredn ,steady ,lsalt  ,lkalm  ,
c                      mozart parameters
     +                 lmozad ,lgrwt  ,lrest  ,nstepd ,juresi ,     
     +                 jufrou ,juresd ,justrd ,juer   ,ker    ,
     +                 inocon ,jusold ,lfrou  ,itstat ,frobuf ) 
         if (ker.eq.fatal)  then
            goto 900
         endif

c        Calculate new composition
         grain = sedini+10
         call gscomp(   ngrid  ,   nfrac  ,   nbran  ,   nnode  ,
     +                  nboun  ,   nlayer ,   maxtab ,   ntabm  ,
     +                  ngrain ,   submin ,   subplus,   time   ,
     +                  ldtf   ,   g      ,   nunlay ,   jugraut,
     +                  jugralg,
     +               ip(gsopts),ip(branch),ip(node)  ,ip(mbdpar),
     +               rp(sedpar),dp(deltaa),rp(cpack) ,dp(qpack) ,
     +               rp(afwfqs),ip(ntab)  ,rp(table) ,ip(grain) ,
     +               rp(ddis)  ,rp(dfrac) ,rp(grsizmun)         , 
     +               rp(sedtr) ,rp(trform),rp(duncon),rp(p0la)  ,
     +               rp(pdiacc),rp(tmpfr) ,rp(duneh) ,rp(dunel) ,
     +               rp(deff)  ,rp(ptrla) ,rp(pexla) ,rp(grsize),
     +               rp(dmed0) ,rp(dzr)   ,ip(nrdzdl),rp(ws)    ,
     +               rp(wfsold),rp(deltar),rp(zbave) ,rp(zbfl)  ,
     +               rp(sedexp),rp(levunl),   lastts ,   istep  ,
     +               ip(lanrinbt)         ,   juer   ,   ker    )

         if (ker.eq.fatal)  then
            goto 900
         endif


c        Calculate sediment transport

         call gsedim (nbran  ,nnode  ,nbrnod ,nboun  ,maxlev ,
     +                nqlat  ,nmlat  ,ngrid ,maxtab  ,ntabm  ,
     +                nfrac  ,nlayer ,ntmpfr ,juer   ,itim   ,
     +                time   ,ldtf   ,g      ,nunlay ,jugraut,
     +                jugralg,
     +                ip(branch),ip(sedinf),rp(prslot),rp(psltvr),
     +                rp(disgse),ip(nonall),ip(seddb) ,ip(brnode),
     +                ip(bgout) ,ip(sdrdbf),ip(mbdpar),ip(ntab)  ,
     +                ip(gsopts),rp(afwfqs),rp(waoft) ,rp(ws)    ,
     +                dp(hlev)  ,ip(nlev)  ,rp(wft)   ,rp(sectv) ,
     +                rp(alfab) ,rp(rpack) ,rp(x)     ,rp(cpack) ,
     +                dp(qpack) ,dp(hpack) ,rp(grsize),rp(forcon),
     +                rp(source),rp(sedtr) ,rp(celer) ,rp(trform),
     +                   nucoef ,rp(uscoef),rp(duncon),rp(sedpar),
     +                rp(morcon),rp(mltpar),rp(qltpar),rp(qlat)  ,
     +                rp(dfrac) ,rp(pdiacc),rp(ptrla) ,rp(pexla) ,
     +                rp(p0la)  ,rp(dmed0) ,lp(depos) ,rp(duneh) ,
     +                rp(dunel) ,rp(deff)  ,rp(tmpfr) ,rp(table) ,
     +                rp(celert),ip(nrdzdl),rp(sedexp),rp(zbave) ,
     +                rp(zbfl)  ,   lastts ,   istep  ,   ker    )

c
c        Test for fault
c
         if (ker .eq. fatal) then
            goto 900
         endif

      if ((cnt .lt. moitmx) .and. (.not. lastts) ) then
c
c        If last time step not reached, try again
c
         goto 100
c <=================
      else if (cnt .eq. moitmx) then
         ker = fatal
         call error (juer,'GSRIMO Morp iteration failed',emrncv,ker)
c                                                         ?????
      else if (cnt .ge. 1) then
c
c        Inform the user
c
         write (txt,'(i4)') cnt
         call error ( juer,'GSRIMO @'//txt//
     +               '@ reductions of morphological time step',
     +                erdmst, info )
         write (txt1,'(2(1x,i8))') itim
         call error (juer,'GSRIMO timestep@'//txt1//'@',esames,info)

      endif
c
 900  continue
CJK   WRITE (*,*) 'MORP step ',istep,' klaar'
      end

