subroutine SOFLOW(&
&g      , psi    , theta  , epsh   , epsq   ,&
&rhow   , omega  , epsqrl , lambda , relstr ,&
&dhstru , cflpse , resid  , overlp , omcfl  ,&
&dhtyp  , exrstp , flitmx                   ,&
&time   , dtf    , steady                   ,&
&ngrid  , ngridm , nbran  , maxlev , nnode  ,&
&nhstat , nqstat , maxtab , ntabm  , nbrnod ,&
&nlev                                       ,&
&branch , bfrict                            ,&
&bfricp , hpack  , qpack  ,x       ,waoft   ,&
&wft    , aft    ,wtt     ,att     , of     ,&
&hlev                                       ,&
&hbdpar , qbdpar                            ,&
&table  , ntab                              ,&
&node   , numnod ,nodnod                    ,&
&debug_wr                                   ,&
&juer&
&)
!*******
!    BEGIN original interface
!*******
!c      subroutine SOFLOW ( istep  ,time   ,itim   ,dtf    ,filstp ,
!c     +                    cpredn ,steady ,lsalt  ,lkalm  )
!c                         mozart parameters
!c     +                    lmoza  ,lgrwt  ,lrest  ,nstep  ,
!c     +                    juresi ,jufrou ,juresd ,justrd ,juer   ,ker  ,
!c     +                    inocon ,jusold ,lfrou  ,itstat ,frobuf )
!
!c=======================================================================
!c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!c                One Dimensional Modelling System
!c                           S O B E K
!c-----------------------------------------------------------------------
!c Subsystem:          Main module
!c
!c Programmer:         S.L. van der Woude
!c
!c Module:             SOFLOW (SObek FLOW main routine)
!c
!c Module description: Subroutine SOFLOW computes the waterlevels and
!c                     discharges for the next time level. If the Kalman
!c                     filter module is active also the uncertain model
!c                     parameters and covariances will be calculated.
!c
!c                     First the flow module is iterated until convergence
!c                     is reached. After convergence is reached, routine
!c                     KALMAN performs the filter step if requested.
!c
!c-----------------------------------------------------------------------
!c Parameters:
!c NR NAME              IO DESCRIPTION
!c  6 cpredn            P  -
!c  4 dtf               P  -
!c  5 filstp            P  -
!c 16 inocon            P  -
!c  1 istep             I  Current time step number (t(n+1)).
!c  3 itim              P  -
!c 19 itstat            P  -
!c 14 juer              P  -
!c 11 jufrou            I  Unit number of file froude
!c 10 juresi            P  -
!c 17 jusold            P  -
!c 12 justru            P  -
!c 15 ker               IO Error code:
!c                         ok     (0) : No error
!c                         info   (1) : Informative message
!c                         warnng (2) : Warning
!c                         fatal  (3) : Fatal error (processing stops)
!c 18 lfrou             P  -
!c  9 lkalm             I  -
!c  8 lsalt             P  -
!c  7 steady            P  -
!c  2 time              P  -
!c    conv                 Switch to indicate convergented solution
!c                         = 0 nog niet geconvergeerd
!c                         = 1 geconvergeerd
!c                         = 2 niet geconvergeerd, alle iteraties
!c                             verbruikt en doorgaan
!c-----------------------------------------------------------------------
!c Subprogram calls:
!c NAME    DESCRIPTION
!c error   write an ERROR to the error file.
!c flnp1   FLow results on time N + 1
!c flow    FLOW module
!c gtdpnt  GeT Double PoiNTer
!c gtipnt  GeT Integer PoiNTer
!c gtlpnt  GeT Logical PoiNTer
!c gtrpnt  GeT Real PoiNTer
!c kalman  KALman main routine
!c soconv  SObek CONVergence
!c soipar  SObek Integer PARameter
!c sorpar  SObek Real PARameter
!c sowrbf  SObel WRite BuFfer
!c=======================================================================
!c
!c
!c
!c**********************************************************************
!c CVS log information:
!c
!c $Id$
!c
!c History:
!c $Log: soflow.pf,v $
!c Revision 1.11  1999/03/15  15:03:29  kuipe_j
!c Improve Froude file and Dumpfiles
!c
!c Revision 1.10  1998/06/24  11:10:34  kuipe_j
!c Try direct solver if BICGST fails
!c
!c Revision 1.9  1998/06/11  11:47:41  kuipe_j
!c Estuary special integrated
!c
!c Revision 1.8  1998/06/08  13:15:36  kuipe_j
!c time lag hydr controller
!c
!c Revision 1.7  1998/04/10  09:22:23  kuipe_j
!c total area recalculated
!c
!c Revision 1.6  1997/05/26  07:37:00  kuipe_j
!c statistic of iteration improved
!c
!c Revision 1.5  1997/01/23  08:30:11  kuipe_j
!c Make flow module robust
!c
!c Revision 1.4  1996/12/02  10:03:48  kuipe_j
!c avoid negative pointers
!c
!c Revision 1.3  1996/09/03  14:33:42  kuipe_j
!c frequency time hist, run in time est. morp
!c
!c Revision 1.2  1996/04/12  13:06:05  kuipe_j
!c headers, minor changes
!c
!c Revision 1.1  1996/04/11  08:16:31  kuipe_j
!c Kalman module added
!c
!c
!c**********************************************************************
!c
!c     Parameters
!c
!      integer  itim(2),istep  ,filstp, cpredn ,
!     +         juresi ,jufrou ,juresd ,justrd ,
!     +         juer   ,ker    ,inocon ,jusold ,
!     +         itstat(4)
   integer juer
!      logical  lsalt  ,lkalm  ,steady ,lfrou  , lrest,
!c     mozart declaration plus groundwater switch
!     +         lmoza, lgrwt
!      integer  nstep
!      double   precision       time   ,dtf
!      real     frobuf(8)
!c
!c     Local variables (pointers to arrays)
!c
!      integer a1m   ,abcd1 ,abcd2 ,af2   ,aft   ,afwfqs,alfab ,
!     +        arex  ,arexcn,arexop,att   ,bfricp,bfrict,branch,
!     +        brnode,buflag,cnpflg,cnstrl,conhis,contrl,cpack ,
!     +        delh  ,deriva,
!     +        engpar,exres ,flwpar,gangle,grid  ,grsize,hpack ,
!     +        hbdpar,hlev  ,hstat ,indx  ,izwft ,kabcd1,kabcd2,
!     +        kalpar,kbeta ,kgain ,ksi   ,lagstm,lfilt ,mat   ,
!     +        maxlev,maxtab,nbran ,nbrnod,ncontr,ncsrel,nexres,
!     +        ngrid ,ngridm,nhstat,nlev  ,nlags ,nnc   ,nnf   ,
!     +        nnm   ,nnmu  ,nnn   ,nnode ,nns   ,node  ,nosdim,
!     +        np    ,nqlat ,nqstat,nsamp ,nstdb ,nstru ,ntab  ,
!     +        ntabm ,ntcrel,nodnod,ntrigr,
!     +        ntsam ,numnod,of    ,pfa   ,pmua
!      integer prslot,psltvr,pw    ,p1    ,p2    ,pcol  ,
!     +        qbdpar,qlat  ,qlatac,qlatgr,qltpar,qstat ,res   ,
!     +        rescov,rpack ,rfv1  ,rfv2  ,rho   ,rhsm  ,rhsvv ,
!     +        sample,scares,scceq ,scfric,scmeq ,scmu  ,scnode,
!     +        scqhs ,scifri,scimu ,sclceq,sclfri,sclmeq,sclmu ,
!     +        sclnod,sclqhs,sectc ,sectv ,smploc,smpns ,snceq ,
!     +        snfric,snmeq ,snmu  ,snnode,snqhs ,snwind,stdbq ,
!     +        strclo,strhis,strpar,strtyp,table ,
!     +        tauwi ,trcnrl,triger,typcr ,waoft ,wfrict  ,
!     +        wf2   ,wndpar,work  ,wshld ,x     ,
!     +        ibuf  ,resbuf,strbuf,solbuf, grhis
!
!c     mozart pointer, (Id's,names,storageWidth)
!     +        qlatid,qlatnm, gridnm, nodenm
!c
!c     Single variables
!c
!c      real    epsh ,epsq, overlp, epsqrl, qtyp, dhtyp
!      real    qtyp
!c
!      integer flitmx, iter, conv, miniter
!      logical lconv , bicg
!c     mozart declaration
!      integer istmoz, idmoz, istcnt
!c
!c     External functions
!c
!      integer  gtdpnt, gtipnt, gtlpnt, gtrpnt, gtcpnt, soipar
!      real     sorpar
!      logical  equal
!      external gtdpnt, gtipnt, gtlpnt, gtrpnt, gtcpnt, soipar,
!     +         sorpar, equal
!c
!c     Include memory pool
!c
!      include 'mempool.i'
!      include 'errcod.i'

!     maybe make a module of these parameters. Used also in <FLOWIT>
   integer    ok
   integer    info
   integer    warnng
   integer    fatal

   parameter (ok     =     0,&
   &info   =     1,&
   &warnng =     2,&
   &fatal  =     3)
!
!*******
!    END old interface
!*******
!
!     Include constants for array dimensions
!
!     Maybe better is to copy the code in <sobdim.i> here. But be aware
!     that this statement is in several parts of the code.
!
   include '../include/sobdim.i'
!
!     Extract parameters from flwpar
!
!      flwpar = gtrpnt ( 'FLWPAR' )
   real flwpar(20)
!
!      epsh   = sorpar ( rp(flwpar), 4 )
!      epsq   = sorpar ( rp(flwpar), 5 )
!      epsqrl = sorpar ( rp(flwpar), 9 )
!      flitmx = soipar ( rp(flwpar), 7 )
!      lconv  = soipar ( rp(flwpar), 17) .eq. 1
!
!
!    Passed from <SOFLOW_wrap>
!
   real     g      , psi    , theta  , epsh   , epsq,&
   &rhow   , omega  , epsqrl , lambda , relstr,&
   &dhstru , cflpse ,          overlp , omcfl,&
   &dhtyp  , exrstp

   double precision  dtf    , resid  , time

   integer  ngrid  , ngridm , nbran  , maxlev , nnode  ,&
   &nhstat , nqstat , maxtab , flitmx
!
!    Originally input but currently parameters
!
   logical lkalm, lmoza, lsalt, lfrou, steady, lgrwt, lrest, bicg
   logical lconv
   integer conv
   integer itstat(4), itim(2)
!
!     internal
!
   integer kgrid, kbran, knode, kbc, idx_bc
!
!     Originally read from memory pool
!
!     Using <gtdpnt> and <gtrpnt> one finds the starting addresses of working arrays.
!     This is changed into directly allocating memory for each variable. The ones that
!     are used are passed to <SOFLOW>. The ones that are not used are allocated here.
!
!     Single variables are read from the memory pool to simplify the
!     call for the Microsoft Fortran compiler v5
!
!
!      a1m    =     gtrpnt ( 'A1M'   )
!     used for salinity
   real    a1m(ngrid)

!      abcd1  =     gtdpnt ( 'ABCD1' )
!     output
   double precision abcd1(ngridm,5)

!      abcd2  =     gtdpnt ( 'ABCD2' )
!     output
   double precision abcd2(ngridm,5)

!      aft    =     gtrpnt ( 'AFT'   )
!     output if hydraulic parameters computed inside
   real aft(ngrid,maxlev)

!      afwfqs =     gtrpnt ( 'AFWFQS')
!     output
!     afwfqs(i,1) = afs(i,1)
!     afwfqs(1,3) = afs(i,2)
!     afs(ngrid,2)      O  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
   real    afwfqs(ngrid,8)

!      alfab  =     gtrpnt ( 'ALFAB' )
!     output
   real alfab(ngrid)

!      arex   =     gtrpnt ( 'AREX'  )
!     used for summerdikes
   real arex(ngrid,4)

!      arexcn =     gtipnt ( 'AREXCN')
!     output
   integer arexcn(ngrid,2)

!      arexop =     gtipnt ( 'AREXOP')
   integer arexop(2)

!      att    =     gtrpnt ( 'ATT'   )
!     output if hydraulic parameters computed inside
   real att(ngrid,maxlev)

!      bfricp =     gtrpnt ( 'BFRICP')
   real bfricp(6,ngrid)

!      bfrict =     gtipnt ( 'BFRICT')
   integer bfrict(3,nbran)

!      branch =     gtipnt ( 'BRANCH')
   integer branch(4,nbran)

!      brnode =     gtipnt ( 'BRNODE')
!     used for Kalman

!      buflag =     gtrpnt ( 'BUFLAG')
!     used for structures
!      real    buflag(lagstm,nlags)
   real    buflag(1,1)

!      cnpflg =     gtipnt ( 'CNPFLG')
!      integer cnpflg(dmcopr,*)
   integer cnpflg(1,1)

!      cnstrl =     gtipnt ( 'CNSTRL')
!      integer cnstrl(2,ncsrel)
   integer cnstrl(2,1)

!      conhis =     gtrpnt ( 'CONHIS')
!      real conhis(5,*)
   real conhis(1,1)

!      contrl =     gtrpnt ( 'CONTRL')
!      real    contrl(17,*)
   real    contrl(17,1)

!      cpack  =     gtrpnt ( 'CPACK' )
!     output
   real    cpack(ngrid,4)

!      delh   =     gtdpnt ( 'DELH'  )
   double precision delh(nnode)

!      engpar =     gtrpnt ( 'ENGPAR')
   real engpar(9)

!      exres  =     gtrpnt ( 'EXRES' )
   real exres(3,1)

!      gangle =     gtrpnt ( 'GANGLE')
   real gangle(ngrid)

!      grid   =     gtipnt ( 'GRID'  )
   integer grid(ngrid)

!      grsize =     gtrpnt ( 'GRSIZE')
   real grsize(4,ngrid,1)

!      hpack  =     gtdpnt ( 'HPACK')
   double precision hpack(ngrid,3)

!      hbdpar =     gtipnt ( 'HBDPAR')
!      integer hbdpar(3,*)
   integer hbdpar(3,nhstat)

!      hlev   =     gtdpnt ( 'HLEV'  )
   double precision hlev(ngrid,maxlev)

!      hstat  =     gtrpnt ( 'HSTAT' )
!     output
   real hstat(nhstat)

!      ibuf   =     gtipnt ( 'IBUF'  )
   integer ibuf(3)

!      indx   =     gtipnt ( 'INDX'  )
   integer indx(nnode)

!      izwft  =     gtrpnt ( 'IZWFT' )
   real izwft(ngrid,maxlev)

!      ksi    =     gtrpnt ( 'KSI'   )
   real    ksi(ngrid)

!      lagstm = ip (gtipnt ( 'LAGSTM'))
   integer lagstm

!      mat    =     gtdpnt ( 'MAT'   )
   double precision mat(nnode,nnode)

!      maxlev = ip (gtipnt ( 'MAXLEV'))

!      maxtab = ip (gtipnt ( 'MAXTAB'))

!      nbran  = ip (gtipnt ( 'NBRAN' ))

!      nbrnod = ip (gtipnt ( 'NBRNOD'))

!      ncontr = ip (gtipnt ( 'NCONTR'))
   integer ncontr

!      ncsrel = ip (gtipnt ( 'NCSREL'))
   integer ncsrel

!      nexres = ip (gtipnt ( 'NEXRES'))
   integer nexres

!      ngrid  = ip (gtipnt ( 'NGRID' ))
!     input

!      ngridm = ip (gtipnt ( 'NGRIDM'))
!     input

!      nhstat = ip (gtipnt ( 'NHSTAT'))

!      nlags  = ip (gtipnt ( 'NLAGS' ))
   integer nlags

!      nlev   =     gtipnt ( 'NLEV'  )
   integer nlev(ngrid)

!      nnc    = ip (gtipnt ( 'NNC'   ))
!     set as parameter because needed for allocating
   integer nnc
   parameter (nnc=1)

!      nnf    = ip (gtipnt ( 'NNF'   ))
!     set as parameter because needed for allocating
   integer nnf
   parameter (nnf=1)

!      nnm    = ip (gtipnt ( 'NNM'   ))
!     set as parameter because needed for allocating
   integer nnm
   parameter (nnm=1)

!      nnmu   = ip (gtipnt ( 'NNMU'  ))
!     set as parameter because needed for allocating
   integer nnmu
   parameter (nnmu=1)

!      nnn    = ip (gtipnt ( 'NNN'   ))
!     set as parameter because needed for allocating
   integer nnn
   parameter (nnn=1)

!      nns    = ip (gtipnt ( 'NNS'   ))
!     set as parameter because needed for allocating
   integer nns
   parameter (nns=1)

!      nnode  = ip (gtipnt ( 'NNODE' ))
!     input

!      node   =     gtipnt ( 'NODE'  )
   integer node(4,nnode)

!      nodnod =     gtipnt ( 'NODNOD')
   integer nodnod(nnode,nbrnod+1)

!      nosdim = ip (gtipnt ( 'NOSDIM'))
!     set as parameter because needed for allocating
   integer nosdim
   parameter (nosdim=1)

!      nqlat  = ip (gtipnt ( 'NQLAT' ))
!     set as parameter because needed for allocating
   integer nqlat
   parameter (nqlat=0)

!      nqstat = ip (gtipnt ( 'NQSTAT'))

!      nstdb  =     gtipnt ( 'NSTDB ')
!      if (nstdb.gt.0) then
!         nstdb = ip(nstdb)
!      else
!         nstdb = 0
!      endif
!     set as parameter because needed for allocating
   integer nstdb
   parameter (nstdb=0)

!      nstru  = ip (gtipnt ( 'NSTRU' ))
!     set as parameter because needed for allocating
   integer nstru
   parameter (nstru=0)

!      ntab   =     gtipnt ( 'NTAB'  )
   integer ntab(4,maxtab)

!      ntabm  = ip (gtipnt ( 'NTABM' ))

!      ntcrel = ip (gtipnt ( 'NTCREL'))
   integer ntcrel

!      ntrigr = ip (gtipnt ( 'NTRIGR'))
   integer ntrigr

!      numnod =     gtipnt ( 'NUMNOD')
   integer numnod(nnode)

!      of     =     gtrpnt ( 'OF'    )
!     output if hydraulic parameters computed inside
   real of(ngrid,maxlev)

!      pfa    =     gtrpnt ( 'PFA'   )
   real    pfa(nnf)
!      real pfa

!      pmua   =     gtrpnt ( 'PMUA'  )
   real    pmua(nnmu)
!      real pmua

!      prslot =     gtrpnt ( 'PRSLOT')
   real prslot(3,nbran)

!      psltvr =     gtrpnt ( 'PSLTVR')
   real psltvr(7,ngrid)

!      pw     =     gtrpnt ( 'PW'    )
   real pw(1)

!     qpack  =     gtdpnt ( 'QPACK' )
   double precision qpack(ngrid,3)

!      qbdpar =     gtipnt ( 'QBDPAR')
   integer qbdpar(3,nqstat)

!      qlat   =     gtrpnt ( 'QLAT'  )
   real qlat(1)

!      qlatgr =     gtrpnt ( 'QLATGR')
   real qlatgr(ngrid)

!      nodenm =     gtcpnt ('NODENM')

!      qlatid =     max(1,gtcpnt ('QLATID'))
   character(len=40) qlatid

!      qlatnm =     max(1,gtcpnt ('QLATNM'))
   character(len=40) qlatnm

!      gridnm =     gtcpnt ( 'GRIDNM')

!      qltpar =     gtrpnt ( 'QLTPAR')
   real    qltpar(9)

!      qstat  =     gtrpnt ( 'QSTAT' )
   real qstat(nhstat)

!      resbuf =     gtrpnt ( 'RESBUF')
   real     resbuf(dmbuf1,6)

!      rfv1   =     gtdpnt ( 'RFV1'  )
   double precision rfv1(ngrid,3)

!      rfv2   =     gtdpnt ( 'RFV2'  )
   double precision rfv2(ngrid,3)

!      rho    =     gtrpnt ( 'RHO'   )
   real    rho(ngrid)

!      rhsvv  =     gtdpnt ( 'RHSVV' )
   double precision rhsvv(nnode)

!      rhsm   =     gtrpnt ( 'RHSM'  )

!      rpack  =     gtrpnt ( 'RPACK' )
   real    rpack(ngrid,4)

!      scnode =     gtipnt ( 'SCNODE')
   integer scnode(1)

!      scceq  =     gtipnt ( 'SCCEQ' )
   integer scceq(1)

!      scifri =     gtipnt ( 'SCIFRI')
   integer scifri(ngrid)

!      scimu  =     gtipnt ( 'SCIMU' )
   integer scimu(nstru)

!      scmeq  =     gtipnt ( 'SCMEQ' )
   integer scmeq(1)

!      scqhs  =     gtipnt ( 'SCQHS' )
   integer scqhs(1)

!      sclceq =     gtipnt ( 'SCLCEQ')
   integer sclceq(nnc+1)

!      sclmeq =     gtipnt ( 'SCLMEQ')
   integer sclmeq(nnm+1)

!      sclnod =     gtipnt ( 'SCLNOD')
   integer sclnod(1)

!      sclqhs =     gtipnt ( 'SCLQHS')
   integer sclqhs(nns+1)

!      snceq  =     gtrpnt ( 'SNCEQ' )
   real    snceq(nosdim,nnc)

!      snfric =     gtrpnt ( 'SNFRIC')
   real    snfric(2,nnf)

!      snmeq  =     gtrpnt ( 'SNMEQ' )
   real snmeq(nosdim,nnm)

!      snmu   =     gtrpnt ( 'SNMU'  )
   real snmu(2,nnmu)

!      snnode =     gtrpnt ( 'SNNODE')
   real snnode(nosdim,nnn)

!      snqhs  =     gtrpnt ( 'SNQHS' )
   real snqhs(nosdim,nns)

!      snwind =     gtrpnt ( 'SNWIND')
   real snwind(2)

!      sectc  =     gtrpnt ( 'SECTC' )
   real sectc(ngrid,3)

!      sectv  =     gtrpnt ( 'SECTV' )
   real sectv(ngrid,dmsecv)

!      solbuf =     gtrpnt ( 'SOLBUF')
   real solbuf(dmbuf2,7,ngrid)

!      stdbq  =     max(gtrpnt ( 'STDBQ'),1)
   real    stdbq(nstdb)

!      strbuf =     gtrpnt ( 'STRBUF')
!      real    strbuf(dmbuf1,2,*)
   real strbuf(dmbuf1,2)

!      strclo =     gtlpnt ( 'STRCLO')
   logical strclo(1)

!      strhis =     gtrpnt ( 'STRHIS')
!      real    strhis(dmstrh,*)
   real    strhis(dmstrh)

!      strpar =     gtrpnt ( 'STRPAR')
!      real    strpar(dmstrpar,*)
   real    strpar(dmstrpar)

!      strtyp =     gtipnt ( 'STRTYP')
!      integer strtyp(10,*)
   integer strtyp(10)

!      table  =     gtrpnt ( 'TABLE' )
   real    table(ntabm)

!      tauwi  =     gtrpnt ( 'TAUWI' )
   real    tauwi(ngrid)

!      trcnrl =     gtipnt ( 'TRCNRL')
!      integer trcnrl(5,ntcrel)
   integer trcnrl(5)

!      triger =     gtipnt ( 'TRIGER')
!      integer triger(10,ntrigr)
   integer triger(10)

!      typcr  =     gtipnt ( 'TYPCR' )
   integer typcr(nbran)

!      waoft  =     gtrpnt ( 'WAOFT' )
!      real    waoft(ngrid,*)
   real    waoft(ngrid,14)

!      wfrict =     gtipnt ( 'WFRICT')
   integer wfrict(3,nbran)

!      wft    =     gtrpnt ( 'WFT'   )
   real wft(ngrid,maxlev)

!      wndpar =     gtrpnt ( 'WNDPAR')
   real wndpar(3)

!      wshld  =     gtrpnt ( 'WSHLD' )
   real    wshld(ngrid)

!      wtt    =     gtrpnt ( 'WTT'   )
   real wtt(ngrid,maxlev)

!      work   =     gtdpnt ( 'WORK'  )
   double precision work(nnode,7)

!      x      =     gtrpnt ( 'X'     )
   real x(ngrid)

!      grhis  =     gtrpnt ( 'GRHIS' )
   real grhis(1)

!    debug
   double precision dbg1
   integer debug_wr

!*******
!    END allocate
!*******
!
!c     Pointers to h and q:
!c     not needed any more?
!      h2 = hpack + ngrid * 2
!      q2 = qpack + ngrid * 2
!c     storage width = waoft(,2)
!c     not used?
!      storWidth = waoft + ngrid
!
!
!    summerdikes input (not used)
!
   arexop(1)=0
   arexop(2)=0
!
!     structures input (not used)
!
   ncontr=0
   nlags=1
   ncsrel=1
   do kgrid=1,ngrid
      grid(kgrid)=1
   enddo
!
!    only main channel
!
   do kgrid=1,ngrid
      sectc(kgrid,1)=0

!    I think this may need to be moved outside and compute based
!    on the actual water level? maybe only initialization?
      cpack(kgrid,1)=bfricp(1,kgrid)
      cpack(kgrid,2)=bfricp(1,kgrid)
      cpack(kgrid,3)=bfricp(3,kgrid)
      cpack(kgrid,4)=bfricp(5,kgrid)

      !waoft(kgrid,1)=wft(kgrid,1)
      !waoft(kgrid,2)=wtt(kgrid,1)
      !waoft(kgrid,3)=aft(kgrid,1)
      !waoft(kgrid,4)=att(kgrid,1)
      !waoft(kgrid,5)=of(kgrid,1)
   enddo
!
!    extra resistance parameters (not used)
!
   nexres=0
!
!    Kalman filter parameters (not used)
!
   lkalm=.false.
!
!    Salt parameters (not used)
!
   lsalt=.false.
!
!     Control parameters (not used)
!
   ntcrel=0
!
!      Mozart parameters (not used)
!
   lmoza=.false.
!
!      Groundwater parameters (not used)
!
   lgrwt=.false.
!
!     Branch input
!
   do kbran=1,nbran
      typcr(kbran)=1
      wfrict(1,kbran)=0
   enddo

!
!     Node input
   do knode=1,nnode
      delh(knode)=0
   enddo
!
!      Other
!
   lfrou=.false.
   lrest=.false.
   iterbc=100

!     I don't know what should the initial value be, but not 0.
   ibuf(1)=1
   ibuf(2)=1
   ibuf(3)=1
!
!     Create flwpar
!
!     I am not sure we can do this because of the different type
!     of array. The solution is to pass all variables or to change the
!     type of array to <real> for all variables.
!
   flwpar( 1 )      =g
   flwpar( 2 )      =psi
   flwpar( 3 )      =theta
   flwpar( 6 )      =rhow
   flwpar( 7 )      =flitmx
   flwpar( 8 )      =omega
   flwpar(10 )      =lambda
   flwpar(11 )      =relstr
   flwpar(12 )      =dhstru
   flwpar(13 )      =cflpse
   flwpar(14 )      =iterbc
   flwpar(15 )      =resid
   flwpar(16 )      =overlp
   flwpar(18 )      =omcfl
   flwpar(19 )      =dhtyp
   flwpar(20 )      =exrstp

!     Initialize BC.
   idx_bc=0
!         H-boundary
   do kbc=1,nhstat
      idx_bc=idx_bc+1
      hstat(kbc) = table(ntab(3,idx_bc))
   enddo
!         Q-boundary
   do kbc=1,nqstat
      idx_bc=idx_bc+1
      qstat(kbc) = table(ntab(3,idx_bc))
   enddo


#if !  defined (SHR_MEM)
! ====  shared memory  ====
! ====  niet voor SRS BOS
! Koppeling Mozart
!
!      if(lmoza .and. nqlat.gt.0)
!     +call MOZCONTROL (  istep   ,   ngrid  ,   nqlat  ,   dtf    ,
!     +                   juer    ,   istmoz ,   idmoz  ,   itim   ,
!     +                 rp(qltpar),cp(qlatid),rp(qlatgr),dp(hpack) )
#endif
!
!     Repeat until convergence
!
   iter   = 0
!
!     In case of automatic pseudo courant number adaptation
!     a minimum number of iteration steps will be carried out
!
!     <dhtyp> is here treated as a logical but it is actually a
!     real. Maybe previous compilers allowed this. We set <miniter>=3
!     as the default value is 0.1
!      if (equal(dhtyp,0.)) then
!         miniter = 0
!      else
   miniter = 3
!      endif
!
!     Start always with Bicgst method
!
   bicg = .true.

100 continue

   iter = iter + 1

!     FM1DIMP2DO: remove debug
   if (debug_wr>0) then
      write(42,*) 'in SOFLOW'
      write(42,*) iter
      write(42,*) 'h1'
      write(42,*) hpack(:,1)
      write(42,*) 'h2'
      write(42,*) hpack(:,2)
      write(42,*) 'h3'
      write(42,*) hpack(:,3)
      write(42,*) 'q1'
      write(42,*) qpack(:,1)
      write(42,*) 'q2'
      write(42,*) qpack(:,2)
      write(42,*) 'q3'
      write(42,*) qpack(:,3)
   endif

   call sre_flow (time ,dtf,steady,iter  ,istep ,itim  ,nbran,&
   &ngrid ,&
   &ncontr,ncsrel,ntcrel,ntrigr,lkalm ,nnc   ,&
   &nnm   ,nnn   ,nns     ,&
   &nnf   ,nnmu  ,nosdim,lagstm,nlags ,juer  ,&
!     Mozart parameters plus groundwater switch
   &lmoza ,istmoz,qlatid,qlatnm,lgrwt ,&
   &lrest ,flwpar,contrl,&
   &branch,typcr ,maxlev,nlev  ,hlev  ,wft   ,aft   ,&
   &wtt   ,att   ,arex  ,arexcn,arexop, of   ,&
   &bfrict,bfricp,maxtab,ntabm ,ntab  ,table ,&
   &sectc ,sectv ,grsize,engpar,gangle,wndpar,&
   &wfrict,wshld ,snceq ,snmeq ,snqhs ,snfric,&
   &snmu  ,snwind,sclceq,sclmeq,sclqhs,scceq ,&
   &scmeq ,scqhs ,scifri,scimu ,scnode,snnode,&
   &sclnod,pfa   ,pmua  ,pw    ,nexres,exres ,&
   &lsalt ,izwft ,nhstat,hbdpar,nqstat,qbdpar,&
   &nstru ,strtyp,strpar,nqlat ,qltpar,grid  ,&
   &x     ,grhis ,&
   &rho   ,ngridm,nnode ,node  ,   nbrnod    ,&
   &nodnod,numnod,prslot,psltvr,conhis,waoft ,&
   &cpack ,rpack ,alfab ,tauwi ,ksi   ,a1m   ,&
   &hstat ,qstat ,qlat  ,qlatgr,strclo,rfv1  ,&
   &rfv2  ,abcd1 ,abcd2 ,mat   ,rhsvv ,hpack ,&
   &qpack ,delh  ,work  ,cnstrl,strhis,trcnrl,&
   &triger,cnpflg,ker   ,qtyp  ,lfrou ,strbuf,&
   &ibuf  ,solbuf,buflag,indx  ,bicg  ,stdbq ,&
   &nstdb                                    ,&
   &debug_wr)




!
!    original call
!
!      call sre_flow (time ,dtf,steady,iter  ,istep ,itim  ,nbran  ,ngrid   ,
!     +ncontr,ncsrel,ntcrel,ntrigr,lkalm ,nnc   ,nnm   ,nnn    ,nns     ,
!     +nnf   ,nnmu  ,nosdim,lagstm,nlags ,juer  ,
!c     Mozart parameters plus groundwater switch
!     +  lmoza   ,  istmoz  ,cp(qlatid), cp(qlatnm), lgrwt   ,
!     +  lrest   ,rp(flwpar),rp(contrl),
!     +ip(branch),ip(typcr),maxlev,ip(nlev),dp(hlev) ,rp(wft),rp(aft)   ,
!     +rp(wtt)   ,rp(att)   ,rp(arex)  ,ip(arexcn),ip(arexop),rp(of)    ,
!     +ip(bfrict),rp(bfricp),   maxtab ,ntabm  ,  ip(ntab)   ,rp(table) ,
!     +rp(sectc) ,rp(sectv) ,rp(grsize),rp(engpar),rp(gangle),rp(wndpar),
!     +ip(wfrict),rp(wshld) ,rp(snceq) ,rp(snmeq) ,rp(snqhs) ,rp(snfric),
!     +rp(snmu)  ,rp(snwind),ip(sclceq),ip(sclmeq),ip(sclqhs),ip(scceq) ,
!     +ip(scmeq) ,ip(scqhs) ,ip(scifri),ip(scimu) ,ip(scnode),rp(snnode),
!     +ip(sclnod),rp(pfa)   ,rp(pmua)  ,rp(pw)    ,   nexres ,rp(exres) ,
!     +   lsalt  ,rp(izwft) ,   nhstat ,ip(hbdpar),   nqstat ,ip(qbdpar),
!     +   nstru  ,ip(strtyp),rp(strpar),   nqlat  ,rp(qltpar),ip(grid)  ,
!     +rp(x)     ,rp(grhis) ,
!     +rp(rho)   ,   ngridm ,   nnode  ,ip(node)  ,   nbrnod ,
!     +ip(nodnod),ip(numnod),rp(prslot),rp(psltvr),rp(conhis),rp(waoft) ,
!     +rp(cpack) ,rp(rpack) ,rp(alfab) ,rp(tauwi) ,rp(ksi)   ,rp(a1m)   ,
!     +rp(hstat) ,rp(qstat) ,rp(qlat)  ,rp(qlatgr),lp(strclo),dp(rfv1)  ,
!     +dp(rfv2)  ,dp(abcd1) ,dp(abcd2 ),dp(mat)   ,dp(rhsvv) ,dp(hpack) ,
!     +dp(qpack) ,dp(delh)  ,dp(work)  ,ip(cnstrl),rp(strhis),ip(trcnrl),
!     +ip(triger),ip(cnpflg),ker       ,qtyp      ,  lfrou   ,rp(strbuf),
!     +ip(ibuf)  ,rp(solbuf),rp(buflag),ip(indx)  ,   bicg   ,rp(stdbq) ,
!     +   nstdb  )

!
!        Check for convergence
!
!        Program stops at the moment in case of no convergence
!
!     FM1DIMP2DO: remove debug
   if (debug_wr>0) then
      write(42,*) 'FLOW'
      write(42,*) iter
      write(42,*) 'h1'
      write(42,*) hpack(:,1)
      write(42,*) 'h2'
      write(42,*) hpack(:,2)
      write(42,*) 'h3'
      write(42,*) hpack(:,3)
      write(42,*) 'q1'
      write(42,*) qpack(:,1)
      write(42,*) 'q2'
      write(42,*) qpack(:,2)
      write(42,*) 'q3'
      write(42,*) qpack(:,3)
   endif

   call soconv (ngrid ,epsh   ,epsq   ,hpack  ,&
   &qpack ,miniter,conv   ,juresi ,&
   &iter  ,epsqrl ,qtyp   ,juer   ,&
   &ker   ,flitmx ,lconv  ,inocon ,&
   &ibuf  ,resbuf ,itstat )

!
!     If convergence has not been reached try again
!
   if ( conv .eq. 0 .and. ker .ne. fatal ) then
      goto 100
   endif

!
!     Update info for data base structure warnings
!
!      call fltser (0      ,nstru  ,ngrid  ,rp(strpar) ,ip(strtyp) ,
!     +             dp(h2) ,ker    ,juer   )
!
!      if ( ker .ne. fatal ) then
!
!        if ( lkalm ) then
!
!         af2    =     gtrpnt ( 'AF2'   )
!         deriva =     gtrpnt ( 'DERIVA')
!         kabcd1 =     gtdpnt ( 'KABCD1')
!         kabcd2 =     gtdpnt ( 'KABCD2')
!         kalpar =     gtrpnt ( 'KALPAR')
!         kbeta  =     gtdpnt ( 'KBETA' )
!         kgain  =     gtrpnt ( 'KGAIN' )
!         lfilt  =     gtlpnt ( 'LFILT' )
!         np     = ip (gtipnt ( 'NP'    ))
!         nsamp  = ip (gtipnt ( 'NSAMP' ))
!         ntsam  = ip (gtipnt ( 'NTSAM' ))
!         p1     =     gtrpnt ( 'P1'    )
!         p2     =     gtrpnt ( 'P2'    )
!         pcol   =     gtrpnt ( 'PCOL'  )
!         qlatac =     gtrpnt ( 'QLATAC')
!         res    =     gtrpnt ( 'RES'   )
!         rescov =     gtrpnt ( 'RESCOV')
!         sample =     gtrpnt ( 'SAMPLE')
!         scares =     gtrpnt ( 'SCARES')
!         scfric =     gtipnt ( 'SCFRIC')
!         scmu   =     gtipnt ( 'SCMU'  )
!         sclfri =     gtipnt ( 'SCLFRI')
!         sclmu  =     gtipnt ( 'SCLMU' )
!         smploc =     gtipnt ( 'SMPLOC')
!         smpns  =     gtrpnt ( 'SMPNS' )
!         wf2    =     gtrpnt ( 'WF2'   )
!
!         call KALMAN   (maxlev ,maxtab ,nbran  ,ngrid ,ngridm ,nnc     ,
!     +        nnm      ,nnn    ,nnode  ,nbrnod ,nns   ,nstru  ,ip(nlev),
!     +        ntabm    ,nnf    ,nnmu   ,nsamp  ,ntsam ,np     ,nosdim  ,
!     +        cpredn   ,lp(lfilt)      ,filstp ,time  ,dtf    ,lsalt   ,
!     +        juer     ,nexres ,nqlat  ,rp(flwpar)    ,rp(kalpar)      ,
!     +dp(abcd1) ,rp(af2)   ,ip(branch),ip(grid)  ,ip(hbdpar),ip(typcr) ,
!     +dp(hpack) ,dp(hlev)  ,dp(kabcd1),dp(kabcd2),dp(kbeta) ,dp(mat)   ,
!     +ip(node)  ,ip(qbdpar),dp(rfv1)  ,dp(rfv2)  ,rp(rho)   ,dp(rhsvv) ,
!     +rp(wft)   ,rp(aft)   ,rp(wf2)   ,ip(wfrict),rp(of)    ,ip(bfrict),
!     +rp(bfricp),dp(qpack) ,ip(ntab)  ,rp(table) ,rp(sectc) ,rp(sectv) ,
!     +rp(grsize),rp(engpar),rp(x)     ,rp(exres) ,rp(prslot),rp(waoft) ,
!     +rp(cpack) ,rp(rpack) ,rp(alfab) ,rp(deriva),ip(strtyp),rp(strpar),
!     +rp(qltpar),rp(qlat)  ,lp(strclo),rp(qlatac),rp(tauwi) ,rp(arex)  ,
!     +ip(arexcn),ip(arexop),ip(sclnod),ip(scnode),rp(snnode),
!     +ip(scifri),ip(scimu) ,rp(snceq) ,rp(snmeq) ,rp(snqhs) ,
!     +rp(snfric),rp(snmu)  ,rp(snwind),ip(sclceq),ip(sclmeq),ip(sclqhs),
!     +ip(sclfri),ip(sclmu) ,ip(scceq) ,ip(scmeq) ,ip(scqhs) ,ip(scfric),
!     +ip(scmu)  ,ip(smploc),rp(p1)    ,rp(p2)    ,rp(pcol)  ,rp(rescov),
!     +rp(sample),rp(scares),rp(smpns) ,ip(indx)  ,ip(brnode),rp(rhsm)  ,
!     +rp(pfa)   ,rp(pmua)  ,rp(pw)    ,rp(res)   ,rp(kgain) ,   ker    ,
!     +rp(psltvr) )
!        endif
!
!      endif
!     FM1D2DO: remove debug
   dbg1=hpack(1,1)
!
!     Calculate variables for time level n+1
!
!     <h2> and <q2> are not computed because I skip subroutine
!     <fltser>. Hence, I call <flnp1> with <hpack> and <qpack>

!     FM1DIMP2DO: remove debug
!      write(42,*) 'SOCONV'
!      write(42,*) iter
!      write(42,*) 'h1'
!      write(42,*) hpack(:,1)
!      write(42,*) 'h2'
!      write(42,*) hpack(:,2)
!      write(42,*) 'h3'
!      write(42,*) hpack(:,3)

   if ( ker.ne.fatal ) then
      call flnp1 (lkalm ,nbran ,ngrid ,nnf   ,&
      &branch,typcr ,bfrict,bfricp,&
      &hpack(:,3)   ,qpack(:,3)   ,maxlev,nlev  ,&
      &hlev  ,wft   ,aft   ,overlp,&
      &arex  ,arexcn,arexop,of    ,&
      &maxtab,ntabm ,ntab  ,table ,&
      &sectc ,sectv ,prslot,psltvr,&
      &waoft ,grsize,engpar,scifri,&
      &pfa   ,juer  ,cpack ,rpack ,&
      &afwfqs,alfab ,&
      &wtt   ,att   ,ker    )
   endif

!     FM1DIMP2DO: remove debug
!      write(42,*) 'FLNP1'
!      write(42,*) iter
!      write(42,*) 'h1'
!      write(42,*) hpack(:,1)
!      write(42,*) 'h2'
!      write(42,*) hpack(:,2)
!      write(42,*) 'h3'
!      write(42,*) hpack(:,3)


!     FM1D2DO: remove debug
   dbg1=hpack(1,3)
!
! Original call
!
!      if ( ker.ne.fatal ) then
!         call flnp1 (lkalm ,nbran ,ngrid ,nnf   ,
!     +               branch,typcr ,bfrict,bfricp,
!     +               h2    ,q2    ,maxlev,nlev  ,
!     +               hlev  ,wft   ,aft   ,overlp,
!     +               arex  ,arexcn,arexop,of    ,
!     +               maxtab,ntabm ,ntab  ,table ,
!     +               sectc ,sectv ,prslot,psltvr,
!     +               waoft ,grsize,engpar,scifri,
!     +               pfa   ,juer  ,cpack ,rpack ,
!     +               afwfqs,alfab ,
!     +               wtt   ,att   ,ker    )
!      endif
!
! SObek WRite BuFfer
!
!      call sowrbf( juresd    , justrd    , jusold    ,
!     +             ip(ibuf)  , rp(resbuf), rp(strbuf),
!     +             nstru     , ip(strtyp), ngrid     ,
!     +             jufrou    , rp(solbuf), ker       ,
!     +             conv      , lfrou     , frobuf    )
!
! #if !  defined (SHR_MEM)
! ====  shared memory  ====
! ====  niet voor SRS BOS
! einde tijdsproces; Mozart-koppeling afsluiten
!      if (lmoza .and. nqlat .gt. 0 .and.
!     +    istep .eq. nstep) call ENDCT (idmoz, istcnt)
! #endif
!      return

end
