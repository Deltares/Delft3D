      subroutine SOFLOW(
     +                  g      , psi    , theta  , epsh   , epsq   , 
     +                  rhow   , omega  , epsqrl , lambda , relstr , 
     +                  dhstru , cflpse , resid  , overlp , omcfl  , 
     +                  dhtyp  , exrstp , flitmx                   ,
     +                  time   , dtf    , steady                   ,
     +                  ngrid  , ngridm , nbran  , maxlev , nnode  ,
     +                  nhstat , nqstat , maxtab , ntabm  , nbrnod ,
     +                  nlev                                       ,
     +                  branch , bfrict                            ,
     +                  bfricp , hpack  , qpack  ,x       ,waoft   ,
     +                  wft    , aft    ,wtt     ,att     , of     , 
     +                  hlev                                       , 
     +                  hbdpar , qbdpar                            , 
     +                  table  , ntab                              ,
     +                  node   , numnod ,nodnod                    ,
     +                  debug_wr                                   ,
     +                  juer
     +                  )
c*******
c    BEGIN original interface    
c*******
cc      subroutine SOFLOW ( istep  ,time   ,itim   ,dtf    ,filstp ,
cc     +                    cpredn ,steady ,lsalt  ,lkalm  )
cc                         mozart parameters
cc     +                    lmoza  ,lgrwt  ,lrest  ,nstep  , 
cc     +                    juresi ,jufrou ,juresd ,justrd ,juer   ,ker  ,
cc     +                    inocon ,jusold ,lfrou  ,itstat ,frobuf )
c
cc=======================================================================
cc            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
cc                One Dimensional Modelling System
cc                           S O B E K
cc-----------------------------------------------------------------------
cc Subsystem:          Main module
cc
cc Programmer:         S.L. van der Woude
cc
cc Module:             SOFLOW (SObek FLOW main routine)
cc
cc Module description: Subroutine SOFLOW computes the waterlevels and
cc                     discharges for the next time level. If the Kalman
cc                     filter module is active also the uncertain model
cc                     parameters and covariances will be calculated.
cc
cc                     First the flow module is iterated until convergence
cc                     is reached. After convergence is reached, routine
cc                     KALMAN performs the filter step if requested.
cc
cc-----------------------------------------------------------------------
cc Parameters:
cc NR NAME              IO DESCRIPTION
cc  6 cpredn            P  -
cc  4 dtf               P  -
cc  5 filstp            P  -
cc 16 inocon            P  -
cc  1 istep             I  Current time step number (t(n+1)).
cc  3 itim              P  -
cc 19 itstat            P  -
cc 14 juer              P  -
cc 11 jufrou            I  Unit number of file froude
cc 10 juresi            P  -
cc 17 jusold            P  -
cc 12 justru            P  -
cc 15 ker               IO Error code:
cc                         ok     (0) : No error
cc                         info   (1) : Informative message
cc                         warnng (2) : Warning
cc                         fatal  (3) : Fatal error (processing stops)
cc 18 lfrou             P  -
cc  9 lkalm             I  -
cc  8 lsalt             P  -
cc  7 steady            P  -
cc  2 time              P  -
cc    conv                 Switch to indicate convergented solution
cc                         = 0 nog niet geconvergeerd
cc                         = 1 geconvergeerd
cc                         = 2 niet geconvergeerd, alle iteraties
cc                             verbruikt en doorgaan
cc-----------------------------------------------------------------------
cc Subprogram calls:
cc NAME    DESCRIPTION
cc error   write an ERROR to the error file.
cc flnp1   FLow results on time N + 1
cc flow    FLOW module
cc gtdpnt  GeT Double PoiNTer
cc gtipnt  GeT Integer PoiNTer
cc gtlpnt  GeT Logical PoiNTer
cc gtrpnt  GeT Real PoiNTer
cc kalman  KALman main routine
cc soconv  SObek CONVergence
cc soipar  SObek Integer PARameter
cc sorpar  SObek Real PARameter
cc sowrbf  SObel WRite BuFfer
cc=======================================================================
cc
cc
cc
cc**********************************************************************
cc CVS log information:
cc
cc $Id$
cc
cc History:
cc $Log: soflow.pf,v $
cc Revision 1.11  1999/03/15  15:03:29  kuipe_j
cc Improve Froude file and Dumpfiles
cc
cc Revision 1.10  1998/06/24  11:10:34  kuipe_j
cc Try direct solver if BICGST fails
cc
cc Revision 1.9  1998/06/11  11:47:41  kuipe_j
cc Estuary special integrated
cc
cc Revision 1.8  1998/06/08  13:15:36  kuipe_j
cc time lag hydr controller
cc
cc Revision 1.7  1998/04/10  09:22:23  kuipe_j
cc total area recalculated
cc
cc Revision 1.6  1997/05/26  07:37:00  kuipe_j
cc statistic of iteration improved
cc
cc Revision 1.5  1997/01/23  08:30:11  kuipe_j
cc Make flow module robust
cc
cc Revision 1.4  1996/12/02  10:03:48  kuipe_j
cc avoid negative pointers
cc
cc Revision 1.3  1996/09/03  14:33:42  kuipe_j
cc frequency time hist, run in time est. morp
cc
cc Revision 1.2  1996/04/12  13:06:05  kuipe_j
cc headers, minor changes
cc
cc Revision 1.1  1996/04/11  08:16:31  kuipe_j
cc Kalman module added
cc
cc
cc**********************************************************************
cc
cc     Parameters
cc
c      integer  itim(2),istep  ,filstp, cpredn ,
c     +         juresi ,jufrou ,juresd ,justrd ,
c     +         juer   ,ker    ,inocon ,jusold ,
c     +         itstat(4)
      integer juer
c      logical  lsalt  ,lkalm  ,steady ,lfrou  , lrest,
cc     mozart declaration plus groundwater switch
c     +         lmoza, lgrwt
c      integer  nstep
c      double   precision       time   ,dtf
c      real     frobuf(8)
cc
cc     Local variables (pointers to arrays)
cc
c      integer a1m   ,abcd1 ,abcd2 ,af2   ,aft   ,afwfqs,alfab ,
c     +        arex  ,arexcn,arexop,att   ,bfricp,bfrict,branch,
c     +        brnode,buflag,cnpflg,cnstrl,conhis,contrl,cpack ,
c     +        delh  ,deriva,
c     +        engpar,exres ,flwpar,gangle,grid  ,grsize,hpack ,
c     +        hbdpar,hlev  ,hstat ,indx  ,izwft ,kabcd1,kabcd2,
c     +        kalpar,kbeta ,kgain ,ksi   ,lagstm,lfilt ,mat   ,
c     +        maxlev,maxtab,nbran ,nbrnod,ncontr,ncsrel,nexres,
c     +        ngrid ,ngridm,nhstat,nlev  ,nlags ,nnc   ,nnf   ,
c     +        nnm   ,nnmu  ,nnn   ,nnode ,nns   ,node  ,nosdim,
c     +        np    ,nqlat ,nqstat,nsamp ,nstdb ,nstru ,ntab  ,
c     +        ntabm ,ntcrel,nodnod,ntrigr,
c     +        ntsam ,numnod,of    ,pfa   ,pmua
c      integer prslot,psltvr,pw    ,p1    ,p2    ,pcol  ,
c     +        qbdpar,qlat  ,qlatac,qlatgr,qltpar,qstat ,res   ,
c     +        rescov,rpack ,rfv1  ,rfv2  ,rho   ,rhsm  ,rhsvv ,
c     +        sample,scares,scceq ,scfric,scmeq ,scmu  ,scnode,
c     +        scqhs ,scifri,scimu ,sclceq,sclfri,sclmeq,sclmu ,
c     +        sclnod,sclqhs,sectc ,sectv ,smploc,smpns ,snceq ,
c     +        snfric,snmeq ,snmu  ,snnode,snqhs ,snwind,stdbq ,
c     +        strclo,strhis,strpar,strtyp,table ,
c     +        tauwi ,trcnrl,triger,typcr ,waoft ,wfrict  ,
c     +        wf2   ,wndpar,work  ,wshld ,x     ,
c     +        ibuf  ,resbuf,strbuf,solbuf, grhis
c
cc     mozart pointer, (Id's,names,storageWidth)
c     +        qlatid,qlatnm, gridnm, nodenm
cc
cc     Single variables
cc
cc      real    epsh ,epsq, overlp, epsqrl, qtyp, dhtyp
c      real    qtyp
cc
c      integer flitmx, iter, conv, miniter
c      logical lconv , bicg      
cc     mozart declaration
c      integer istmoz, idmoz, istcnt
cc
cc     External functions
cc
c      integer  gtdpnt, gtipnt, gtlpnt, gtrpnt, gtcpnt, soipar
c      real     sorpar
c      logical  equal
c      external gtdpnt, gtipnt, gtlpnt, gtrpnt, gtcpnt, soipar,
c     +         sorpar, equal
cc
cc     Include memory pool
cc
c      include 'mempool.i'
c      include 'errcod.i'
      
c     maybe make a module of these parameters. Used also in <FLOWIT>
      integer    ok
      integer    info
      integer    warnng
      integer    fatal
      
      parameter (ok     =     0,       
     +           info   =     1,       
     +           warnng =     2,       
     +           fatal  =     3)
c
c*******
c    END old interface
c*******
c
c     Include constants for array dimensions
c
c     Maybe better is to copy the code in <sobdim.i> here. But be aware
c     that this statement is in several parts of the code. 
c
      include '../include/sobdim.i'
c
c     Extract parameters from flwpar
c
c      flwpar = gtrpnt ( 'FLWPAR' )
      real flwpar(20)
c      
c      epsh   = sorpar ( rp(flwpar), 4 )
c      epsq   = sorpar ( rp(flwpar), 5 )
c      epsqrl = sorpar ( rp(flwpar), 9 )
c      flitmx = soipar ( rp(flwpar), 7 )
c      lconv  = soipar ( rp(flwpar), 17) .eq. 1    
c
c      
c    Passed from <SOFLOW_wrap>      
c      
      real     g      , psi    , theta  , epsh   , epsq,   
     +         rhow   , omega  , epsqrl , lambda , relstr,
     +         dhstru , cflpse ,          overlp , omcfl,  
     +         dhtyp  , exrstp 
                    
      double precision  dtf    , resid  , time

      integer  ngrid  , ngridm , nbran  , maxlev , nnode  ,
     +                  nhstat , nqstat , maxtab , flitmx
c      
c    Originally input but currently parameters
c      
      logical lkalm, lmoza, lsalt, lfrou, steady, lgrwt, lrest, bicg
      logical lconv
      integer conv
      integer itstat(4), itim(2)
c     
c     internal
c
      integer kgrid, kbran, knode, kbc, idx_bc
c
c     Originally read from memory pool
c
c     Using <gtdpnt> and <gtrpnt> one finds the starting addresses of working arrays.
c     This is changed into directly allocating memory for each variable. The ones that 
c     are used are passed to <SOFLOW>. The ones that are not used are allocated here.
c
c     Single variables are read from the memory pool to simplify the
c     call for the Microsoft Fortran compiler v5
c
c
c      a1m    =     gtrpnt ( 'A1M'   ) 
c     used for salinity
      real    a1m(ngrid)
      
c      abcd1  =     gtdpnt ( 'ABCD1' )
c     output      
      double precision abcd1(ngridm,5)
      
c      abcd2  =     gtdpnt ( 'ABCD2' )
c     output
      double precision abcd2(ngridm,5)
      
c      aft    =     gtrpnt ( 'AFT'   )
c     output if hydraulic parameters computed inside      
      real aft(ngrid,maxlev)
      
c      afwfqs =     gtrpnt ( 'AFWFQS')
c     output
c     afwfqs(i,1) = afs(i,1) 
c     afwfqs(1,3) = afs(i,2)
c     afs(ngrid,2)      O  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
      real    afwfqs(ngrid,8)
      
c      alfab  =     gtrpnt ( 'ALFAB' )
c     output      
      real alfab(ngrid)

c      arex   =     gtrpnt ( 'AREX'  )
c     used for summerdikes
      real arex(ngrid,4)
      
c      arexcn =     gtipnt ( 'AREXCN')
c     output
      integer arexcn(ngrid,2)
      
c      arexop =     gtipnt ( 'AREXOP')
      integer arexop(2)
           
c      att    =     gtrpnt ( 'ATT'   )
c     output if hydraulic parameters computed inside      
      real att(ngrid,maxlev)
      
c      bfricp =     gtrpnt ( 'BFRICP')
      real bfricp(6,ngrid)
      
c      bfrict =     gtipnt ( 'BFRICT')
      integer bfrict(3,nbran)
      
c      branch =     gtipnt ( 'BRANCH')
      integer branch(4,nbran)
      
c      brnode =     gtipnt ( 'BRNODE')
c     used for Kalman
      
c      buflag =     gtrpnt ( 'BUFLAG')
c     used for structures
c      real    buflag(lagstm,nlags)
      real    buflag(1,1)
      
c      cnpflg =     gtipnt ( 'CNPFLG')
c      integer cnpflg(dmcopr,*)
      integer cnpflg(1,1)
      
c      cnstrl =     gtipnt ( 'CNSTRL')
c      integer cnstrl(2,ncsrel)
      integer cnstrl(2,1)
      
c      conhis =     gtrpnt ( 'CONHIS')
c      real conhis(5,*)
      real conhis(1,1)
      
c      contrl =     gtrpnt ( 'CONTRL')
c      real    contrl(17,*)
      real    contrl(17,1)
      
c      cpack  =     gtrpnt ( 'CPACK' )
c     output
      real    cpack(ngrid,4)
      
c      delh   =     gtdpnt ( 'DELH'  )
      double precision delh(nnode)
      
c      engpar =     gtrpnt ( 'ENGPAR')
      real engpar(9)
      
c      exres  =     gtrpnt ( 'EXRES' )
      real exres(3,1)
      
c      gangle =     gtrpnt ( 'GANGLE')
      real gangle(ngrid)
      
c      grid   =     gtipnt ( 'GRID'  )
      integer grid(ngrid)
      
c      grsize =     gtrpnt ( 'GRSIZE')
      real grsize(4,ngrid,1)
            
c      hpack  =     gtdpnt ( 'HPACK')
      double precision hpack(ngrid,3)
      
c      hbdpar =     gtipnt ( 'HBDPAR')
c      integer hbdpar(3,*)
      integer hbdpar(3,nhstat)
      
c      hlev   =     gtdpnt ( 'HLEV'  )
      double precision hlev(ngrid,maxlev)
      
c      hstat  =     gtrpnt ( 'HSTAT' )
c     output      
      real hstat(nhstat)
      
c      ibuf   =     gtipnt ( 'IBUF'  )
      integer ibuf(3) 
      
c      indx   =     gtipnt ( 'INDX'  )
      integer indx(nnode)
      
c      izwft  =     gtrpnt ( 'IZWFT' )
      real izwft(ngrid,maxlev)
      
c      ksi    =     gtrpnt ( 'KSI'   )
      real    ksi(ngrid)
      
c      lagstm = ip (gtipnt ( 'LAGSTM'))
      integer lagstm
      
c      mat    =     gtdpnt ( 'MAT'   )
      double precision mat(nnode,nnode)
      
c      maxlev = ip (gtipnt ( 'MAXLEV'))
      
c      maxtab = ip (gtipnt ( 'MAXTAB'))
      
c      nbran  = ip (gtipnt ( 'NBRAN' ))
      
c      nbrnod = ip (gtipnt ( 'NBRNOD'))
      
c      ncontr = ip (gtipnt ( 'NCONTR'))
      integer ncontr
      
c      ncsrel = ip (gtipnt ( 'NCSREL'))
      integer ncsrel
      
c      nexres = ip (gtipnt ( 'NEXRES'))
      integer nexres
      
c      ngrid  = ip (gtipnt ( 'NGRID' ))      
c     input
      
c      ngridm = ip (gtipnt ( 'NGRIDM'))      
c     input
      
c      nhstat = ip (gtipnt ( 'NHSTAT'))
      
c      nlags  = ip (gtipnt ( 'NLAGS' ))
      integer nlags
      
c      nlev   =     gtipnt ( 'NLEV'  )
      integer nlev(ngrid)
      
c      nnc    = ip (gtipnt ( 'NNC'   ))
c     set as parameter because needed for allocating
      integer nnc
      parameter (nnc=1)
      
c      nnf    = ip (gtipnt ( 'NNF'   ))
c     set as parameter because needed for allocating
      integer nnf
      parameter (nnf=1)
      
c      nnm    = ip (gtipnt ( 'NNM'   ))
c     set as parameter because needed for allocating
      integer nnm
      parameter (nnm=1)
      
c      nnmu   = ip (gtipnt ( 'NNMU'  ))
c     set as parameter because needed for allocating
      integer nnmu
      parameter (nnmu=1)
      
c      nnn    = ip (gtipnt ( 'NNN'   ))
c     set as parameter because needed for allocating
      integer nnn
      parameter (nnn=1)
      
c      nns    = ip (gtipnt ( 'NNS'   ))
c     set as parameter because needed for allocating
      integer nns
      parameter (nns=1)
      
c      nnode  = ip (gtipnt ( 'NNODE' ))
c     input
      
c      node   =     gtipnt ( 'NODE'  )
      integer node(4,nnode)
      
c      nodnod =     gtipnt ( 'NODNOD')
      integer nodnod(nnode,nbrnod+1)
      
c      nosdim = ip (gtipnt ( 'NOSDIM'))
c     set as parameter because needed for allocating
      integer nosdim
      parameter (nosdim=1)
      
c      nqlat  = ip (gtipnt ( 'NQLAT' ))
c     set as parameter because needed for allocating
      integer nqlat
      parameter (nqlat=0)
      
c      nqstat = ip (gtipnt ( 'NQSTAT'))
      
c      nstdb  =     gtipnt ( 'NSTDB ')
c      if (nstdb.gt.0) then
c         nstdb = ip(nstdb)
c      else
c         nstdb = 0
c      endif   
c     set as parameter because needed for allocating
      integer nstdb
      parameter (nstdb=0)
      
c      nstru  = ip (gtipnt ( 'NSTRU' ))
c     set as parameter because needed for allocating
      integer nstru
      parameter (nstru=0)
      
c      ntab   =     gtipnt ( 'NTAB'  )
      integer ntab(4,maxtab)
      
c      ntabm  = ip (gtipnt ( 'NTABM' ))
      
c      ntcrel = ip (gtipnt ( 'NTCREL'))
      integer ntcrel
      
c      ntrigr = ip (gtipnt ( 'NTRIGR'))
      integer ntrigr
      
c      numnod =     gtipnt ( 'NUMNOD')
      integer numnod(nnode)
      
c      of     =     gtrpnt ( 'OF'    )
c     output if hydraulic parameters computed inside      
      real of(ngrid,maxlev)
      
c      pfa    =     gtrpnt ( 'PFA'   )
      real    pfa(nnf)
c      real pfa
      
c      pmua   =     gtrpnt ( 'PMUA'  )
      real    pmua(nnmu)
c      real pmua
      
c      prslot =     gtrpnt ( 'PRSLOT')
      real prslot(3,nbran)
      
c      psltvr =     gtrpnt ( 'PSLTVR')
      real psltvr(7,ngrid)
      
c      pw     =     gtrpnt ( 'PW'    )
      real pw(1)
      
c     qpack  =     gtdpnt ( 'QPACK' )
      double precision qpack(ngrid,3)
      
c      qbdpar =     gtipnt ( 'QBDPAR')
      integer qbdpar(3,nqstat)
      
c      qlat   =     gtrpnt ( 'QLAT'  )
      real qlat(1) 
      
c      qlatgr =     gtrpnt ( 'QLATGR')
      real qlatgr(ngrid)
      
c      nodenm =     gtcpnt ('NODENM')
      
c      qlatid =     max(1,gtcpnt ('QLATID'))
      character(len=40) qlatid
      
c      qlatnm =     max(1,gtcpnt ('QLATNM'))
      character(len=40) qlatnm
      
c      gridnm =     gtcpnt ( 'GRIDNM')      
      
c      qltpar =     gtrpnt ( 'QLTPAR')
      real    qltpar(9)
      
c      qstat  =     gtrpnt ( 'QSTAT' )
      real qstat(nhstat)
      
c      resbuf =     gtrpnt ( 'RESBUF')
      real     resbuf(dmbuf1,6)
      
c      rfv1   =     gtdpnt ( 'RFV1'  )
      double precision rfv1(ngrid,3)  
      
c      rfv2   =     gtdpnt ( 'RFV2'  )
      double precision rfv2(ngrid,3)
      
c      rho    =     gtrpnt ( 'RHO'   )
      real    rho(ngrid)
      
c      rhsvv  =     gtdpnt ( 'RHSVV' )
      double precision rhsvv(nnode)
      
c      rhsm   =     gtrpnt ( 'RHSM'  )
      
c      rpack  =     gtrpnt ( 'RPACK' )
      real    rpack(ngrid,4)
      
c      scnode =     gtipnt ( 'SCNODE')
      integer scnode(1)
      
c      scceq  =     gtipnt ( 'SCCEQ' )
      integer scceq(1)
      
c      scifri =     gtipnt ( 'SCIFRI')
      integer scifri(ngrid)
      
c      scimu  =     gtipnt ( 'SCIMU' )
      integer scimu(nstru)
      
c      scmeq  =     gtipnt ( 'SCMEQ' )
      integer scmeq(1)
      
c      scqhs  =     gtipnt ( 'SCQHS' )
      integer scqhs(1)
      
c      sclceq =     gtipnt ( 'SCLCEQ')
      integer sclceq(nnc+1)         
      
c      sclmeq =     gtipnt ( 'SCLMEQ')
      integer sclmeq(nnm+1)
      
c      sclnod =     gtipnt ( 'SCLNOD')
      integer sclnod(1)
            
c      sclqhs =     gtipnt ( 'SCLQHS')
      integer sclqhs(nns+1)
      
c      snceq  =     gtrpnt ( 'SNCEQ' )
      real    snceq(nosdim,nnc)
      
c      snfric =     gtrpnt ( 'SNFRIC')
      real    snfric(2,nnf)
      
c      snmeq  =     gtrpnt ( 'SNMEQ' )
      real snmeq(nosdim,nnm)
      
c      snmu   =     gtrpnt ( 'SNMU'  )
      real snmu(2,nnmu)
      
c      snnode =     gtrpnt ( 'SNNODE')
      real snnode(nosdim,nnn)
      
c      snqhs  =     gtrpnt ( 'SNQHS' )
      real snqhs(nosdim,nns)
      
c      snwind =     gtrpnt ( 'SNWIND')
      real snwind(2)
      
c      sectc  =     gtrpnt ( 'SECTC' )
      real sectc(ngrid,3)
      
c      sectv  =     gtrpnt ( 'SECTV' )
      real sectv(ngrid,dmsecv)
      
c      solbuf =     gtrpnt ( 'SOLBUF')
      real solbuf(dmbuf2,7,ngrid)
      
c      stdbq  =     max(gtrpnt ( 'STDBQ'),1)
      real    stdbq(nstdb)
      
c      strbuf =     gtrpnt ( 'STRBUF')
c      real    strbuf(dmbuf1,2,*)
      real strbuf(dmbuf1,2)
      
c      strclo =     gtlpnt ( 'STRCLO')
      logical strclo(1)
      
c      strhis =     gtrpnt ( 'STRHIS')
c      real    strhis(dmstrh,*)
      real    strhis(dmstrh)
      
c      strpar =     gtrpnt ( 'STRPAR')
c      real    strpar(dmstrpar,*)
      real    strpar(dmstrpar)
      
c      strtyp =     gtipnt ( 'STRTYP')
c      integer strtyp(10,*)
      integer strtyp(10)
      
c      table  =     gtrpnt ( 'TABLE' )
      real    table(ntabm)
      
c      tauwi  =     gtrpnt ( 'TAUWI' )
      real    tauwi(ngrid)
      
c      trcnrl =     gtipnt ( 'TRCNRL')
c      integer trcnrl(5,ntcrel)
      integer trcnrl(5)
      
c      triger =     gtipnt ( 'TRIGER')
c      integer triger(10,ntrigr)
      integer triger(10)
      
c      typcr  =     gtipnt ( 'TYPCR' )
      integer typcr(nbran)
      
c      waoft  =     gtrpnt ( 'WAOFT' )
c      real    waoft(ngrid,*)
      real    waoft(ngrid,14)
      
c      wfrict =     gtipnt ( 'WFRICT')
      integer wfrict(3,nbran)
      
c      wft    =     gtrpnt ( 'WFT'   )
      real wft(ngrid,maxlev)
      
c      wndpar =     gtrpnt ( 'WNDPAR')
      real wndpar(3)
      
c      wshld  =     gtrpnt ( 'WSHLD' )
      real    wshld(ngrid)
      
c      wtt    =     gtrpnt ( 'WTT'   )      
      real wtt(ngrid,maxlev)
      
c      work   =     gtdpnt ( 'WORK'  )
      double precision work(nnode,7)
      
c      x      =     gtrpnt ( 'X'     )
      real x(ngrid)
      
c      grhis  =     gtrpnt ( 'GRHIS' )
      real grhis(1)     
      
      double precision h2(ngrid)
      double precision q2(ngrid)
      
c    debug
      double precision dbg1
      integer debug_wr
      
c*******
c    END allocate
c*******
c       
cc     Pointers to h and q:
cc     not needed any more?
c      h2 = hpack + ngrid * 2
c      q2 = qpack + ngrid * 2
cc     storage width = waoft(,2)
cc     not used?
c      storWidth = waoft + ngrid
c
c
c    summerdikes input (not used)
c
      arexop(1)=0
      arexop(2)=0
c
c     structures input (not used)
c
      ncontr=0
      nlags=1
      ncsrel=1
      do kgrid=1,ngrid
          grid(kgrid)=1
      enddo
c
c    only main channel
c
      do kgrid=1,ngrid 
          sectc(kgrid,1)=0
          
c    I think this may need to be moved outside and compute based
c    on the actual water level? maybe only initialization?
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
c
c    extra resistance parameters (not used)
c
      nexres=0
c     
c    Kalman filter parameters (not used)
c
      lkalm=.false.
c     
c    Salt parameters (not used)
c
      lsalt=.false.
c      
c     Control parameters (not used)      
c
      ntcrel=0
c      
c      Mozart parameters (not used)      
c
      lmoza=.false.
c
c      Groundwater parameters (not used)
c      
      lgrwt=.false.
c 
c     Branch input
c
      do kbran=1,nbran
          typcr(kbran)=1
          wfrict(1,kbran)=0
      enddo
      
c
c     Node input
      do knode=1,nnode
          delh(knode)=0
      enddo
c      
c      Other
c
      lfrou=.false.
      lrest=.false.
      iterbc=100
      
c     I don't know what should the initial value be, but not 0.       
      ibuf(1)=1
      ibuf(2)=1
      ibuf(3)=1
c
c     Create flwpar
c     
c     I am not sure we can do this because of the different type
c     of array. The solution is to pass all variables or to change the 
c     type of array to <real> for all variables. 
c
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
      
c     Initialize BC. 
      idx_bc=0
c         H-boundary
      do kbc=1,nhstat
          idx_bc=idx_bc+1
          hstat(kbc) = table(ntab(3,idx_bc))
      enddo
c         Q-boundary
      do kbc=1,nqstat
          idx_bc=idx_bc+1
          qstat(kbc) = table(ntab(3,idx_bc))
      enddo
      
      
#if !  defined (SHR_MEM)
c ====  shared memory  ====
c ====  niet voor SRS BOS
c Koppeling Mozart    
c
c      if(lmoza .and. nqlat.gt.0)
c     +call MOZCONTROL (  istep   ,   ngrid  ,   nqlat  ,   dtf    ,
c     +                   juer    ,   istmoz ,   idmoz  ,   itim   ,
c     +                 rp(qltpar),cp(qlatid),rp(qlatgr),dp(hpack) )
#endif     
c       
c     Repeat until convergence
c
      iter   = 0
c
c     In case of automatic pseudo courant number adaptation
c     a minimum number of iteration steps will be carried out
c
c     <dhtyp> is here treated as a logical but it is actually a 
c     real. Maybe previous compilers allowed this. We set <miniter>=3
c     as the default value is 0.1
c      if (equal(dhtyp,0.)) then
c         miniter = 0
c      else
         miniter = 3
c      endif
c
c     Start always with Bicgst method
c
       bicg = .true.  

 100  continue

      iter = iter + 1

c     FM1DIMP2DO: remove debug
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
      
      call sre_flow (time ,dtf,steady,iter  ,istep ,itim  ,nbran,
     +ngrid ,
     +ncontr,ncsrel,ntcrel,ntrigr,lkalm ,nnc   ,
     +nnm   ,nnn   ,nns     ,
     +nnf   ,nnmu  ,nosdim,lagstm,nlags ,juer  ,
c     Mozart parameters plus groundwater switch
     +lmoza ,istmoz,qlatid,qlatnm,lgrwt ,
     +lrest ,flwpar,contrl,
     +branch,typcr ,maxlev,nlev  ,hlev  ,wft   ,aft   ,
     +wtt   ,att   ,arex  ,arexcn,arexop, of   ,
     +bfrict,bfricp,maxtab,ntabm ,ntab  ,table ,
     +sectc ,sectv ,grsize,engpar,gangle,wndpar,
     +wfrict,wshld ,snceq ,snmeq ,snqhs ,snfric,
     +snmu  ,snwind,sclceq,sclmeq,sclqhs,scceq ,
     +scmeq ,scqhs ,scifri,scimu ,scnode,snnode,
     +sclnod,pfa   ,pmua  ,pw    ,nexres,exres ,
     +lsalt ,izwft ,nhstat,hbdpar,nqstat,qbdpar,
     +nstru ,strtyp,strpar,nqlat ,qltpar,grid  ,
     +x     ,grhis ,
     +rho   ,ngridm,nnode ,node  ,   nbrnod    ,
     +nodnod,numnod,prslot,psltvr,conhis,waoft ,
     +cpack ,rpack ,alfab ,tauwi ,ksi   ,a1m   ,
     +hstat ,qstat ,qlat  ,qlatgr,strclo,rfv1  ,
     +rfv2  ,abcd1 ,abcd2 ,mat   ,rhsvv ,hpack ,
     +qpack ,delh  ,work  ,cnstrl,strhis,trcnrl,
     +triger,cnpflg,ker   ,qtyp  ,lfrou ,strbuf,
     +ibuf  ,solbuf,buflag,indx  ,bicg  ,stdbq ,
     +nstdb                                    ,
     +debug_wr)
      


      
c     
c    original call
c
c      call sre_flow (time ,dtf,steady,iter  ,istep ,itim  ,nbran  ,ngrid   ,
c     +ncontr,ncsrel,ntcrel,ntrigr,lkalm ,nnc   ,nnm   ,nnn    ,nns     ,
c     +nnf   ,nnmu  ,nosdim,lagstm,nlags ,juer  ,
cc     Mozart parameters plus groundwater switch
c     +  lmoza   ,  istmoz  ,cp(qlatid), cp(qlatnm), lgrwt   ,
c     +  lrest   ,rp(flwpar),rp(contrl),
c     +ip(branch),ip(typcr),maxlev,ip(nlev),dp(hlev) ,rp(wft),rp(aft)   ,
c     +rp(wtt)   ,rp(att)   ,rp(arex)  ,ip(arexcn),ip(arexop),rp(of)    ,
c     +ip(bfrict),rp(bfricp),   maxtab ,ntabm  ,  ip(ntab)   ,rp(table) ,
c     +rp(sectc) ,rp(sectv) ,rp(grsize),rp(engpar),rp(gangle),rp(wndpar),
c     +ip(wfrict),rp(wshld) ,rp(snceq) ,rp(snmeq) ,rp(snqhs) ,rp(snfric),
c     +rp(snmu)  ,rp(snwind),ip(sclceq),ip(sclmeq),ip(sclqhs),ip(scceq) ,
c     +ip(scmeq) ,ip(scqhs) ,ip(scifri),ip(scimu) ,ip(scnode),rp(snnode),
c     +ip(sclnod),rp(pfa)   ,rp(pmua)  ,rp(pw)    ,   nexres ,rp(exres) ,
c     +   lsalt  ,rp(izwft) ,   nhstat ,ip(hbdpar),   nqstat ,ip(qbdpar),
c     +   nstru  ,ip(strtyp),rp(strpar),   nqlat  ,rp(qltpar),ip(grid)  ,
c     +rp(x)     ,rp(grhis) ,
c     +rp(rho)   ,   ngridm ,   nnode  ,ip(node)  ,   nbrnod ,
c     +ip(nodnod),ip(numnod),rp(prslot),rp(psltvr),rp(conhis),rp(waoft) ,
c     +rp(cpack) ,rp(rpack) ,rp(alfab) ,rp(tauwi) ,rp(ksi)   ,rp(a1m)   ,
c     +rp(hstat) ,rp(qstat) ,rp(qlat)  ,rp(qlatgr),lp(strclo),dp(rfv1)  ,
c     +dp(rfv2)  ,dp(abcd1) ,dp(abcd2 ),dp(mat)   ,dp(rhsvv) ,dp(hpack) ,
c     +dp(qpack) ,dp(delh)  ,dp(work)  ,ip(cnstrl),rp(strhis),ip(trcnrl),
c     +ip(triger),ip(cnpflg),ker       ,qtyp      ,  lfrou   ,rp(strbuf),
c     +ip(ibuf)  ,rp(solbuf),rp(buflag),ip(indx)  ,   bicg   ,rp(stdbq) ,
c     +   nstdb  )

c
c        Check for convergence
c
c        Program stops at the moment in case of no convergence
c
c     FM1DIMP2DO: remove debug
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
      
      call soconv (ngrid ,epsh   ,epsq   ,hpack  ,
     +             qpack ,miniter,conv   ,juresi ,
     +             iter  ,epsqrl ,qtyp   ,juer   ,
     +             ker   ,flitmx ,lconv  ,inocon ,
     +             ibuf  ,resbuf ,itstat )
  
c
c     If convergence has not been reached try again
c
      if ( conv .eq. 0 .and. ker .ne. fatal ) then
         goto 100
      endif

c
c     Update info for data base structure warnings
c
c      call fltser (0      ,nstru  ,ngrid  ,rp(strpar) ,ip(strtyp) ,
c     +             dp(h2) ,ker    ,juer   )
c
c      if ( ker .ne. fatal ) then
c
c        if ( lkalm ) then
c
c         af2    =     gtrpnt ( 'AF2'   )
c         deriva =     gtrpnt ( 'DERIVA')
c         kabcd1 =     gtdpnt ( 'KABCD1')
c         kabcd2 =     gtdpnt ( 'KABCD2')
c         kalpar =     gtrpnt ( 'KALPAR')
c         kbeta  =     gtdpnt ( 'KBETA' )
c         kgain  =     gtrpnt ( 'KGAIN' )
c         lfilt  =     gtlpnt ( 'LFILT' )
c         np     = ip (gtipnt ( 'NP'    ))
c         nsamp  = ip (gtipnt ( 'NSAMP' ))
c         ntsam  = ip (gtipnt ( 'NTSAM' ))
c         p1     =     gtrpnt ( 'P1'    )
c         p2     =     gtrpnt ( 'P2'    )
c         pcol   =     gtrpnt ( 'PCOL'  )
c         qlatac =     gtrpnt ( 'QLATAC')
c         res    =     gtrpnt ( 'RES'   )
c         rescov =     gtrpnt ( 'RESCOV')
c         sample =     gtrpnt ( 'SAMPLE')
c         scares =     gtrpnt ( 'SCARES')
c         scfric =     gtipnt ( 'SCFRIC')
c         scmu   =     gtipnt ( 'SCMU'  )
c         sclfri =     gtipnt ( 'SCLFRI')
c         sclmu  =     gtipnt ( 'SCLMU' )
c         smploc =     gtipnt ( 'SMPLOC')
c         smpns  =     gtrpnt ( 'SMPNS' )
c         wf2    =     gtrpnt ( 'WF2'   )
c
c         call KALMAN   (maxlev ,maxtab ,nbran  ,ngrid ,ngridm ,nnc     ,
c     +        nnm      ,nnn    ,nnode  ,nbrnod ,nns   ,nstru  ,ip(nlev),
c     +        ntabm    ,nnf    ,nnmu   ,nsamp  ,ntsam ,np     ,nosdim  ,
c     +        cpredn   ,lp(lfilt)      ,filstp ,time  ,dtf    ,lsalt   ,
c     +        juer     ,nexres ,nqlat  ,rp(flwpar)    ,rp(kalpar)      ,
c     +dp(abcd1) ,rp(af2)   ,ip(branch),ip(grid)  ,ip(hbdpar),ip(typcr) ,
c     +dp(hpack) ,dp(hlev)  ,dp(kabcd1),dp(kabcd2),dp(kbeta) ,dp(mat)   ,
c     +ip(node)  ,ip(qbdpar),dp(rfv1)  ,dp(rfv2)  ,rp(rho)   ,dp(rhsvv) ,
c     +rp(wft)   ,rp(aft)   ,rp(wf2)   ,ip(wfrict),rp(of)    ,ip(bfrict),
c     +rp(bfricp),dp(qpack) ,ip(ntab)  ,rp(table) ,rp(sectc) ,rp(sectv) ,
c     +rp(grsize),rp(engpar),rp(x)     ,rp(exres) ,rp(prslot),rp(waoft) ,
c     +rp(cpack) ,rp(rpack) ,rp(alfab) ,rp(deriva),ip(strtyp),rp(strpar),
c     +rp(qltpar),rp(qlat)  ,lp(strclo),rp(qlatac),rp(tauwi) ,rp(arex)  ,
c     +ip(arexcn),ip(arexop),ip(sclnod),ip(scnode),rp(snnode),
c     +ip(scifri),ip(scimu) ,rp(snceq) ,rp(snmeq) ,rp(snqhs) ,
c     +rp(snfric),rp(snmu)  ,rp(snwind),ip(sclceq),ip(sclmeq),ip(sclqhs),
c     +ip(sclfri),ip(sclmu) ,ip(scceq) ,ip(scmeq) ,ip(scqhs) ,ip(scfric),
c     +ip(scmu)  ,ip(smploc),rp(p1)    ,rp(p2)    ,rp(pcol)  ,rp(rescov),
c     +rp(sample),rp(scares),rp(smpns) ,ip(indx)  ,ip(brnode),rp(rhsm)  ,
c     +rp(pfa)   ,rp(pmua)  ,rp(pw)    ,rp(res)   ,rp(kgain) ,   ker    ,
c     +rp(psltvr) )
c        endif
c
c      endif
c     FM1D2DO: remove debug
      dbg1=hpack(1,1)
c
c     Calculate variables for time level n+1
c
c     <h2> and <q2> are not computed because I skip subroutine 
c     <fltser>. Hence, I call <flnp1> with <hpack> and <qpack>
      
c     FM1DIMP2DO: remove debug      
c      write(42,*) 'SOCONV'
c      write(42,*) iter
c      write(42,*) 'h1'
c      write(42,*) hpack(:,1)
c      write(42,*) 'h2'
c      write(42,*) hpack(:,2)
c      write(42,*) 'h3'
c      write(42,*) hpack(:,3)
      
      if ( ker.ne.fatal ) then
         call flnp1 (lkalm ,nbran ,ngrid ,nnf   ,
     +               branch,typcr ,bfrict,bfricp,
     +               hpack(:,3)   ,qpack(:,3)   ,maxlev,nlev  ,
     +               hlev  ,wft   ,aft   ,overlp,
     +               arex  ,arexcn,arexop,of    ,
     +               maxtab,ntabm ,ntab  ,table ,
     +               sectc ,sectv ,prslot,psltvr,
     +               waoft ,grsize,engpar,scifri,
     +               pfa   ,juer  ,cpack ,rpack ,
     +               afwfqs,alfab ,
     +               wtt   ,att   ,ker    )
      endif
      
c     FM1DIMP2DO: remove debug      
c      write(42,*) 'FLNP1'
c      write(42,*) iter
c      write(42,*) 'h1'
c      write(42,*) hpack(:,1)
c      write(42,*) 'h2'
c      write(42,*) hpack(:,2)
c      write(42,*) 'h3'
c      write(42,*) hpack(:,3)
      
      
c     FM1D2DO: remove debug
      dbg1=hpack(1,3)
c
c Original call
c
c      if ( ker.ne.fatal ) then
c         call flnp1 (lkalm ,nbran ,ngrid ,nnf   ,
c     +               branch,typcr ,bfrict,bfricp,
c     +               h2    ,q2    ,maxlev,nlev  ,
c     +               hlev  ,wft   ,aft   ,overlp,
c     +               arex  ,arexcn,arexop,of    ,
c     +               maxtab,ntabm ,ntab  ,table ,
c     +               sectc ,sectv ,prslot,psltvr,
c     +               waoft ,grsize,engpar,scifri,
c     +               pfa   ,juer  ,cpack ,rpack ,
c     +               afwfqs,alfab ,
c     +               wtt   ,att   ,ker    )
c      endif
c
c SObek WRite BuFfer
c
C      call sowrbf( juresd    , justrd    , jusold    ,
C     +             ip(ibuf)  , rp(resbuf), rp(strbuf),
C     +             nstru     , ip(strtyp), ngrid     ,
C     +             jufrou    , rp(solbuf), ker       ,
C     +             conv      , lfrou     , frobuf    )
c
c #if !  defined (SHR_MEM)
c ====  shared memory  ====
c ====  niet voor SRS BOS
c einde tijdsproces; Mozart-koppeling afsluiten
c      if (lmoza .and. nqlat .gt. 0 .and. 
c     +    istep .eq. nstep) call ENDCT (idmoz, istcnt)
c #endif    
c      return

       end
