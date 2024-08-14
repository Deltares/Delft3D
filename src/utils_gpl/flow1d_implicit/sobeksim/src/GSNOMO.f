      subroutine GSNOMO ( istep  ,time   ,itim   ,dtf    ,steady ,
     +                    juer   ,juresi ,jufrou ,juresd ,justrd ,
     +                    ker    ,inocon ,jusold ,lfrou  ,itstat ,
     +                    frobuf ,jugraut,jugralg,lrest)
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SONOMO (SObek NO MOrphology)
c
c Module description: This routine iterates one time step for rivers or
c                     estuaries without the morphology module.
c
c                     First a flow step is calculated by a number of
c                     iteration steps. If the flow step has been calcu-
c                     lated routine FLNP1 will be called. This routine
c                     calculates a number of variables on time level
c                     n+1. This is necessary because other modules and
c                     results use these variables on this time level
c                     whereas the flow module uses these variables on
c                     time level n+1/2.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 dtf               I  Time step flow module
c  1 istep             I  Current time step number (t(n+1)).
c  3 itim              P  -
c  6 itp               I  Tidal period in whole numbers of flow step
c 10 juer              P  -
c 11 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  9 lmorp             P  -
c  7 lsalt             I  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c  8 lsedt             I  Switch to enable sediment transport module
c  5 steady            P  -
c  2 time              P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flnp1   FLow results on time N + 1
c flow    FLOW module
c gtdpnt  GeT Double PoiNTer
c gtipnt  GeT Integer PoiNTer
c gtlpnt  GeT Logical PoiNTer
c gtrpnt  GeT Real PoiNTer
c salt    SALT module
c sedim   SEDIMent module
c soconv  SObek CONVergence
c sodump
c soipar  SObek Integer PARameter
c sorpar  SObek Real PARameter
c=======================================================================
c

c
c     Parameters
c
      integer  itim(2),istep  ,juer   ,ker,  juresi ,jufrou ,
     +         juresd, justrd ,inocon,jusold,itstat(4)      ,
     +         jugraut,jugralg
      real     frobuf (8)
      double   precision       time   ,dtf
      logical  steady         ,lfrou  ,lrest
c
c     Local variables (pointers to arrays)
c
      integer afwfqs,alfab ,bfrict,branch,cpack ,grsize,hpack ,
     +        hlev  ,maxlev,maxtab, nbran,ngrid ,nlev  ,nnode ,
     +        nqlat ,ntab  ,ntabm ,prslot,psltvr,qpack ,qlat  ,
     +        qltpar,rpack ,sectv ,table ,waoft ,wft   ,x
c
c     Additional variables for the sediment module
c
      integer bgout ,brnode,celer ,forcon,mbdpar,mltpar,morcon,
     +        nboun ,nbrnod,nmlat ,nonall,sdrdbf,seddb ,sedinf,
     +        sedpar,sedtr ,trform,uscoef,ws
c
c     Additional pointers for graded sediment module
c
      integer celert,deff  ,deltar,depos ,ddis  ,dfrac ,dmed0   ,
     +        disgse,duncon,duneh ,dunel ,dzr   ,grain ,grsizmun,
     +        gsopts,lanrinbt     ,levunl,nfrac ,ngrain,nlayer  ,
     +        nrdzdl,ntmpfr,nucoef,nunlay,ptrla ,pexla ,pdiacc  ,
     +        p0la  ,sedexp,sedini,source,submin,subplus        ,
     +        tmpfr ,zbave ,zbfl
c
c     Single variables
c
      real    g 
c
      integer ixpar, filstp, cpredn
      logical lsalt, lkalm ,
c     mozart   variables 
     +         lmozad, lgrwt
      integer  nstepd
c
c     External functions
c
      integer  gtipnt, gtlpnt, gtrpnt, gtdpnt
      real     sorpar
      external gtipnt, gtlpnt, gtrpnt, gtdpnt, sorpar
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     Extract parameters from flwpar
c
      ixpar = gtrpnt ( 'FLWPAR' )
c
      g      = sorpar ( rp(ixpar), 1 )
c
c     Find starting addresses of working arrays
c
c     Single variables are read from the memory pool to simplify the
c     call for the Microsoft Fortran compiler v5
c
      afwfqs =     gtrpnt ( 'AFWFQS')
      alfab  =     gtrpnt ( 'ALFAB' )
      bfrict =     gtipnt ( 'BFRICT')
      branch =     gtipnt ( 'BRANCH')
      cpack  =     gtrpnt ( 'CPACK' )
      grsize =     gtrpnt ( 'GRSIZE')
      hpack  =     gtrpnt ( 'HPACK ')
      hlev   =     gtdpnt ( 'HLEV'  )
      maxlev = ip (gtipnt ( 'MAXLEV'))
      maxtab = ip (gtipnt ( 'MAXTAB'))
      nbran  = ip (gtipnt ( 'NBRAN' ))
      ngrid  = ip (gtipnt ( 'NGRID' ))
      nlev   =     gtipnt ( 'NLEV'  )
      nnode  = ip (gtipnt ( 'NNODE' ))
      nqlat  = ip (gtipnt ( 'NQLAT' ))
      ntab   =     gtipnt ( 'NTAB'  )
      ntabm  = ip (gtipnt ( 'NTABM' ))
      
      prslot =     gtrpnt ( 'PRSLOT')
      psltvr =     gtrpnt ( 'PSLTVR')
      qpack  =     gtrpnt ( 'QPACK' )
      qlat   =     gtrpnt ( 'QLAT'  )
      qltpar =     gtrpnt ( 'QLTPAR')
      rpack  =     gtrpnt ( 'RPACK' )
      sectv  =     gtrpnt ( 'SECTV' )
      table  =     gtrpnt ( 'TABLE' )
      waoft  =     gtrpnt ( 'WAOFT' )
      wft    =     gtrpnt ( 'WFT'   )
      x      =     gtrpnt ( 'X'     )
c
      filstp = 0
      cpredn = 0
      lkalm  = .false.
      lsalt  = .false.
c     mozart parameters
      lmozad = .false.
      lgrwt  = .false.
      nstepd = 0
c
      call  SOFLOW ( istep  ,time   ,itim   ,dtf    ,filstp ,
     +               cpredn ,steady ,lsalt  ,lkalm  ,
c                    mozart parameters
     +               lmozad ,lgrwt  ,lrest  ,nstepd ,juresi ,          
     +               jufrou ,juresd ,justrd ,juer   ,ker    ,
     +               inocon ,jusold ,lfrou  ,itstat ,frobuf )     
c
 
      if (ker.ne.fatal) then
c
c        Convergence obtained. Calculate sediment transport and
c        layer thickness.
c
c        Determine pointers to arrays in memory pools
c
         bgout  =     gtipnt ( 'BGOUT' )
         brnode =     gtipnt ( 'BRNODE')
         celer  =     gtrpnt ( 'CELER' )
         forcon =     gtrpnt ( 'FORCON')
         hpack  =     gtdpnt ( 'HPACK' )
         mbdpar =     gtipnt ( 'MBDPAR')
         mltpar =     gtrpnt ( 'MLTPAR')
         morcon =     gtrpnt ( 'MORCON')
         nboun  = ip (gtipnt ( 'NBOUN' ))
         nbrnod = ip (gtipnt ( 'NBRNOD'))
         nmlat  = ip (gtipnt ( 'NMLAT' ))
         nonall =     gtipnt ( 'NONALL')
         qpack  =     gtdpnt ( 'QPACK' )
         seddb  =     gtipnt ( 'SEDDB' )
         sedinf =     gtipnt ( 'SEDINF')
         sedpar =     gtrpnt ( 'SEDPAR')
         sedtr  =     gtrpnt ( 'SEDTR' )
         trform =     gtrpnt ( 'TRFORM')
         uscoef =     gtrpnt ( 'USCOEF')
         ws     =     gtrpnt ( 'WS'    )
c
c        Define pointers for graded sediment module
c
         celert =     gtrpnt ( 'CELERT')
         deff   =     gtrpnt ( 'DEFF'  )
         depos  =     gtlpnt ( 'DEPOS' )
         deltar =     gtrpnt ( 'DELTAR')
         ddis   =     gtrpnt ( 'DDIS'  )
         dfrac  =     gtrpnt ( 'DFRAC' )
         dmed0  =     gtrpnt ( 'DMED0' )
         duncon =     gtrpnt ( 'DUNCON')
         duneh  =     gtrpnt ( 'DUNEH' )
         dunel  =     gtrpnt ( 'DUNEL' )
         disgse =     gtrpnt ( 'DISSED')
         dzr    =     gtrpnt ( 'DZR'   )
         grsizmun =   gtrpnt ( 'GRSIZMUN')
         gsopts =     gtipnt ( 'GSOPTS')
         levunl =     gtrpnt ( 'LEVUNL')
         lanrinbt=    gtipnt ( 'LANRINBT')
         nfrac  = ip (gtipnt ( 'NFRAC' ))
         ngrain = ip (gtipnt ( 'NGRAIN'))
         nlayer = ip (gtipnt ( 'NLAYER'))
         nrdzdl =     gtipnt ( 'NRDZDL')
         ntmpfr = ip (gtipnt ( 'NTMPFR'))
         nunlay = ip (gtipnt ( 'NUNLAY'))
         nucoef = ip (gtipnt ( 'NUCOEF'))
         pdiacc =     gtrpnt ( 'PDIACC')
         pexla  =     gtrpnt ( 'PEXLA' )
         ptrla  =     gtrpnt ( 'PTRLA' )
         p0la   =     gtrpnt ( 'P0LA'  )
         sdrdbf =     gtipnt ('SDRDBF')
         sedexp =     gtrpnt ( 'SEDEXP')
         sedini =     gtipnt ( 'SEDINI')
         source =     gtrpnt ( 'SOURCE')
         submin = ip (gtipnt ( 'SUBMIN'))
         subplus= ip (gtipnt ( 'SUBPLUS'))
         tmpfr  =     gtrpnt ( 'TMPFR' )
         zbave  =     gtrpnt ( 'ZBAVE' )
         zbfl   =     gtrpnt ( 'ZBFL'  )
c
c        Calculate dune height and layer thickness
c        Switch roughness calculation in flow module off
c
         call gsinmix (   ngrid   ,   nbran   ,   nlayer  ,  g       ,
     &                    nunlay  ,   nfrac   ,   juer    ,  jugralg ,
     &                    ker     ,
     &                 ip(gsopts) ,ip(branch) ,rp(sedpar) ,rp(ddis)  ,
     &                 rp(dfrac)  ,rp(cpack)  ,rp(afwfqs) ,rp(trform),
     &                 rp(duncon) ,rp(duneh)  ,rp(dunel)  ,rp(deff)  ,
     &                 rp(grsize) ,rp(dmed0)  ,ip(bfrict) ,rp(ptrla) ,
     &                 rp(pexla)  ,rp(p0la)   ,rp(dzr)    ,rp(levunl),
     &                 ip(nrdzdl) ,rp(ws)     ,rp(sedexp) ,rp(deltar),
     &                 rp(zbave)  ,rp(zbfl)   )
c
         grain = sedini+10
         call gscharun(   submin  ,   subplus ,   nfrac   ,   nbran   ,
     &                    ngrid   ,   ngrain  ,   submin  ,   subplus ,
     &                    nunlay  ,
     &                 ip(grain)  ,ip(branch) ,rp(ddis)   ,rp(dfrac)  ,
     &                 rp(p0la)   ,ip(nrdzdl) ,ip(lanrinbt)           ,
     &                 rp(grsizmun)           )
c
c        Calculate sediment transport
c
         if (ker.ne.fatal) 
     +   call gsedim (nbran  ,nnode  ,nbrnod ,nboun  ,maxlev ,nqlat  ,
     +                nmlat  ,ngrid  ,maxtab ,ntabm  ,nfrac  ,nlayer ,
     +                ntmpfr ,juer   ,itim   ,time   ,dtf    ,g      ,
     +                nunlay ,jugraut,jugralg,
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
     +                rp(zbfl)  ,   .true. ,   istep  ,   ker    )
c
c           Test for fault
c
         if (ker .eq. fatal) then
            call error (juer,'GSNOMO Initialization error', eginit, ker)
         endif
      endif   
      return
      end
