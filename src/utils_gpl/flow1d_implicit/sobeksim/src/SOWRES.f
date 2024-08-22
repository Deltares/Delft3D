      subroutine SOWRES ( dtf    ,itim   ,time   ,istep  ,nstep  ,
     +                    wqagst ,restrt ,newres ,cpredn ,fflow  ,
     +                    fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,
     +                    lflow  ,lkalm  ,lsalt  ,lsedt  ,lmorp  ,
     +                    lgrad  ,iwqin  ,lgrwt  ,juer   ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOWRES (SObek Write RESults)
c
c Module description: After calculating a time step for all modules
c                     mentioned in the application this routine is cal-
c                     led to write results. The routine has been split-
c                     ted in different routines for the different modu-
c                     les to be able to sell sobek stripped.
c
c                     Depending on the user selected scenario and re-
c                     sults the output routines will be called.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 cpredn            P  -
c  1 dtf               P  -
c 10 fflow             P  -
c 11 fkalm             P  -
c 14 fmorp             P  -
c 12 fsalt             P  -
c 13 fsedt             P  -
c  4 istep             I  Current time step number (t(n+1)).
c  2 itim              P  -
c 21 juer              P  -
c 22 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 15 lflow             I  Switch to enable flow module
c 16 lkalm             I  -
c 19 lmorp             I  Logical indicator for morphology computation
c                         = .true.  : with morphology computation
c                         = .false. : without morphology computation
c 17 lsalt             I  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c 18 lsedt             I  Switch to enable sediment transport module
c 20 iwqin             P  -
c  8 newres            I  true, if a new restart file will be made
c  5 nstep             I  Last time step number in simulation.
c  7 restrt            I  Period of writing restart file (0 = write at
c                         end of run)
c  3 time              I  Actual time level tn+1. in sec.
c  6 wqagst            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flsdat  FLuSh buffers of DATa file
c flwres  FLow Write RESults
c gtipnt  GeT Integer PoiNTer
c gtlpnt  GeT Logical PoiNTer
c gtrpnt  GeT Real PoiNTer
c kawres  KAlman Write RESults
c motrou  MOrphology TRaject OUtput file(s)
c mowres  MOrphology Write RESults
c sawres  SAlt Write RESults
c sewres  SEdiment Write RESults
c sorpar  SObek Real PARameter
c
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sowres.pf,v $
c Revision 1.18  1999/03/15  15:01:03  kuipe_j
c IVBDOS
c
c Revision 1.17  1998/06/08  13:15:44  kuipe_j
c time lag hydr controller
c
c Revision 1.16  1997/11/04  14:20:01  kuipe_j
c Retention basin
c
c Revision 1.15  1997/06/17  11:29:25  kuipe_j
c output in history format
c
c Revision 1.14  1997/05/26  07:37:05  kuipe_j
c statistic of iteration improved
c
c Revision 1.13  1997/02/17  10:09:41  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.12  1997/01/23  08:30:19  kuipe_j
c Make flow module robust
c
c Revision 1.11  1996/12/03  09:03:59  kuipe_j
c Salt restart improved
c
c Revision 1.10  1996/04/12  13:06:15  kuipe_j
c headers, minor changes
c
c Revision 1.9  1996/04/11  08:16:42  kuipe_j
c Kalman module added
c
c Revision 1.8  1996/01/17  14:47:44  kuipe_j
c header update
c
c Revision 1.7  1996/01/16  15:01:58  kuipe_j
c Restart improvements
c
c Revision 1.6  1995/10/18  09:01:10  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.5  1995/09/12  08:11:36  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:37:38  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:57:11  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:10:06  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:31  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:28:48  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:10:14  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      double precision   dtf    ,time
      integer    itim(2),istep  ,nstep  ,wqagst ,restrt ,iwqin
      logical    lflow  ,lkalm  ,lsalt  ,lsedt  ,lmorp  ,lgrad  
      logical    fflow  ,fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad ,newres
      logical    lgrwt
      integer    juer   ,ker    ,cpredn
c
c     Variables
c
      logical    writim, wrirst
      integer    fd_nefis_res, fd_nefis_rst, fd_nefis_waq
c
c     Pointers to arrays and single integer values
c
      integer    afwfqs,arexcn,arexop,branch,buflag,cdcdx ,contrl,
     +           conhis,cpack ,csa   ,csd   ,deff  ,depos ,disgr ,
     +           disgse,dissed,dlwqts,dmed0 ,dsopt ,dzr   ,dzunla,
     +           fcpcpr,fgacpr,fhycpr,fpacpr,frcmap,frctim,frecpr,
     +           grain ,grsize,grsizmun,hlev, hlev0 ,hmax  , 
     +           hpack ,hycpre,hyrmap,hyrtim,kfgmap,kfhmap,kfhtim,
     +           kfmmap,kfpmap,kfptim,kfrmap,kfrtim,kgain ,kphmap,
     +           kphtim,kppmap,kpptim,lacpre,lagstm,lanrinbt     ,
     +           lattim,levunl,lfilt ,maxlev,mocpre,
     +           mormap,mortim,mouqpu,nboun ,nbran ,ncelfl,ncelmo,
     +           ncelsa,ncelse,nclfcp,nclfhy,nclfga,nclfpa,nclfre,
     +           ncllat,nclphy,nclppa,nclrst,nclstr,ncontr,nfcpmn,
     +           nfgamn,nfhymn,nfpamn,nfrac,nfrcmap,nfrctim      ,
     +           nfremn,ngrain,ngrid,nhyman,nhymap
      integer    nhytim,nkapar,nkfgmp,nkfhmp,nkfhtm,nkfmmp,nkfpmp,
     +           nkfptm,nkfrmp,nkfrtm,nkphmp,nkphtm,nkppmp,nkpptm,
     +           nlags ,nlaman,nlatim,nlayer,nlev  ,nmoman,nmomap,
     +           nmotim,nmouth,nnf   ,nnmu  ,nphymn,nppamn,np    ,
     +           nqlat ,nrdzdl,nsaman,nsamap,nsamp, nsatim,nsedrd,
     +           nseman,nsemap,nsetim,nstman,nstru ,nsttim,ntmpgr,
     +           nunlay,p0la  ,pexla ,ptrla ,pfa   ,phycpr,pmua  ,
     +           ppacpr,pw    ,p1    ,p2    ,qaggr ,qlaggr,qlat  ,
     +           qpack ,res   ,rescov,rho   ,rpack ,sacpre,
     +           salmap,saltim,sbdscr,sbdpar,scares,secpre,sedmap,
     +           sedpar,sectc ,sectv ,sedtim,sedtr ,stcpre,strhis,
     +           strtim,submin,subplus      ,sumda ,thasca,thcsum,
     +           timout,tmpgr ,trform,typcr ,waoft ,wft   ,x     ,
     +           zbave ,zbfl  ,nefhis,smploc,qltpar,grhis
      integer    flwini,gridnm,strunm,qlatnm,buffer,morini,salini,
     +           sedini,corrnm,kalini,hmin  ,tmaxh ,tminh ,
     +           qmin  ,qmax  ,tmaxq ,tminq ,hpack3,qpack3 
c
c     Single variables
c
      integer    ixpar ,errcod, kode
      real       psi   ,theta,  g    ,delta 
c
c     External functions
c
      integer    gtipnt, gtlpnt, gtrpnt, gtdpnt, flsdat, gtcpnt,soipar
      real       sorpar
      external   gtipnt, gtlpnt, gtrpnt, gtdpnt, flsdat, sorpar,
     +           gtcpnt
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     Workstations: flush true
c
      writim = .true.
c
c     Determine if it is time to write restart info
c
      if (restrt .gt. 0 .and. istep .gt. 0) then
         wrirst = mod(istep,restrt) .eq. 0
      else
         wrirst = .false.
      endif
      if (istep .eq. nstep) then
        wrirst = .true.
      endif
c
c     Find pointers of the different file descriptors
c
      if (newres) then
         fd_nefis_rst = gtipnt ('FD_NEFIS_NEW')
      else
         fd_nefis_rst = gtipnt ('FD_NEFIS_RST')
      endif
c
      fd_nefis_res = gtipnt ('FD_NEFIS_RES')
      fd_nefis_waq = gtipnt ('FD_NEFIS_WAQ')
c
      ixpar  = gtrpnt ( 'FLWPAR' )
      nefhis = soipar ( rp(ixpar), 22)
c
c     Output of FLOW results
c
      if ((lflow) .and. (ker .ne. fatal)) then
c
c        Extract parameters from flwpar
c
        ixpar = gtrpnt ( 'FLWPAR' )
c
        g      = sorpar ( rp(ixpar), 1 )
        psi    = sorpar ( rp(ixpar), 2 )
        theta  = sorpar ( rp(ixpar), 3 )
c
c        Fetch pointers of flow arrays
c
        afwfqs  =    gtrpnt('AFWFQS')
        arexcn  =    gtipnt('AREXCN')
        arexop  =    gtipnt('AREXOP')
        branch  =    gtipnt('BRANCH')
        buffer  =    gtrpnt('BUFFER')
        buflag  =    gtrpnt ('BUFLAG')
        conhis  =    gtrpnt('CONHIS')
        contrl  =    gtrpnt('CONTRL')
        cpack   =    gtrpnt('CPACK' )
        dlwqts  =    gtipnt('DLWQTS')
        flwini  =    gtipnt('FLWINI')
        gridnm  =    gtcpnt('GRIDNM')
        hmax    =    gtrpnt('HMAX'  )
        hpack   =    gtdpnt('HPACK')
        hycpre  =    gtipnt('HYCPRE')
        hyrmap  =    gtipnt('HYRMAP')
        hyrtim  =    gtipnt('HYRTIM')
        lacpre  =    gtipnt('LACPRE')
        lattim  =    gtipnt('LATTIM')
        lfilt   =    gtlpnt('LFILT' )
        lagstm  = ip(gtipnt('LAGSTM'))
        nbran   = ip(gtipnt('NBRAN' ))
        ncelfl  =    gtipnt('NCELFL')
        ncllat  =    gtipnt('NCLLAT')
        nclstr  =    gtipnt('NCLSTR')
        ncontr  = ip(gtipnt('NCONTR'))
        ngrid   = ip(gtipnt('NGRID' ))
        nhyman  = ip(gtipnt('NHYMAN'))
        nhymap  = ip(gtipnt('NHYMAP'))
        nhytim  = ip(gtipnt('NHYTIM'))
        nlags   = ip(gtipnt('NLAGS' ))
        nlaman  = ip(gtipnt('NLAMAN'))
        nlatim  = ip(gtipnt('NLATIM'))
        nqlat   = ip(gtipnt('NQLAT' ))
        nstman  = ip(gtipnt('NSTMAN'))
        nstru   = ip(gtipnt('NSTRU' ))
        nsttim  = ip(gtipnt('NSTTIM'))
        ntmpgr  = ip(gtipnt('NTMPGR'))
        qaggr   =    gtrpnt('QAGGR' )
        qlaggr  =    gtrpnt('QLAGGR')
        qlat    =    gtrpnt('QLAT'  )
        qltpar  =    gtrpnt('QLTPAR')
        qlatnm  =    max(1,gtcpnt('QLATNM'))
        qpack   =    gtdpnt('QPACK')
        rpack   =    gtrpnt('RPACK' )
        stcpre  =    gtipnt('STCPRE')
        strhis  =    gtrpnt('STRHIS')
        strtim  =    gtipnt('STRTIM')
        strunm  =    max(1,gtcpnt('STRUNM'))
        tmpgr   =    gtrpnt('TMPGR' )
        waoft   =    gtrpnt('WAOFT' )
        grhis   =    gtrpnt('GRHIS' )
        x       =    gtrpnt('X'     )
c  
        hmin    = hmax  + ngrid
        tmaxh   = hmin  + ngrid
        tminh   = tmaxh + ngrid
        qmax    = tminh + ngrid
        qmin    = qmax  + ngrid
        tmaxq   = qmin  + ngrid
        tminq   = tmaxq + ngrid
        

cARS 7786: Zelfde wijze als voor H zijn Q arrays gedefinieerd
c          Let ook op declaraties boven

        hpack3  = hpack + ngrid * 2
        qpack3  = qpack + ngrid * 2
c
        if (fflow) then 
           kode = 1
        else
           kode = 2
        endif
c
c       This call must before IVBDSM as in that routine also
c       maxima are detemined. In FLMINMAX also the time of
c       maximum will be determined. 
c
        call FLMINMAX (kode  ,ngrid  ,istep ,nstep ,dtf ,itim  ,
     &                 dp(hpack3)    ,rp(hmin)     ,rp(hmax)   ,
     &                 rp(tminh)     ,rp(tmaxh)    ,
     &                 dp(qpack3)    ,rp(qmin)     ,rp(qmax)   ,
     &                 rp(tminq)     ,rp(tmaxq)    ,cp(gridnm) )

cARS 7786 Gebruik routine om ook de
cARS 7786 qmin en qmax en de bijbehorende tijden te berekenen

c
c       IVBDOS call after wrirst is set and before first is made false.
c
        call IVBDSM(time   ,dtf    ,istep  ,nstep  ,fflow ,wrirst ,
     +              ngrid  ,nbran  ,ip(branch)     ,rp(x)         ,
     +              dp(qpack)      ,dp(hpack)      ,rp(hmax)      ,
     +              cp(gridnm)     )

        call flwres (    dtf     ,   psi     ,   theta   ,   fflow   ,
     +                    itim    ,sngl(time) ,   istep   ,   nstep   ,
     +                    wqagst  ,   writim  ,ip(dlwqts) ,   nstru   ,
     +                    ncontr  ,   ngrid   ,   nhyman  ,   nhymap  ,
     +                    nhytim  ,   nlaman  ,   nlatim  ,   nqlat   ,
     +                    nstman  ,   nsttim  ,   ntmpgr  ,   iwqin   ,
     +                    wrirst  ,   lagstm  ,   nlags   ,   g       ,
     +                 rp(afwfqs) ,rp(contrl) ,rp(conhis) ,rp(strhis) ,
     +                 rp(cpack)  ,rp(rpack)  ,ip(fd_nefis_res) ,
     +                 ip(fd_nefis_rst) ,ip(fd_nefis_waq) ,
     +                 dp(hpack)  ,ip(hycpre) ,ip(hyrmap) ,ip(hyrtim) ,
     +                 ip(lacpre) ,ip(strtim) ,ip(lattim) ,rp(qaggr)  ,
     +                 rp(qlaggr) ,rp(qlat)   ,dp(qpack)  ,ip(stcpre) ,
     +                 rp(tmpgr)  ,rp(waoft)  ,ip(ncelfl) ,ip(nclstr) ,
     +                 ip(ncllat) ,ip(arexop) ,ip(arexcn) ,lp(lfilt)  ,
     +                    juer    ,   ker     ,ip(flwini) ,cp(gridnm) ,
     +                 cp(strunm) ,cp(qlatnm) ,rp(buffer) ,nefhis     ,
     +                 rp(qltpar) ,rp(grhis)  ,lgrwt      ,rp(buflag) )
      endif
c
c     Output of KALMAN results
c
      if ((lkalm) .and. (ker .ne. fatal)) then
c
c       Fetch pointers of Kalman arrays
c
         hpack   =    gtdpnt('HPACK')
         fcpcpr  =    gtipnt('FCPCPR')
         fgacpr  =    gtipnt('FGACPR')
         fhycpr  =    gtipnt('FHYCPR')
         fpacpr  =    gtipnt('FPACPR')
         frecpr  =    gtipnt('FRECPR')
         kfgmap  =    gtipnt('KFGMAP')
         kfhmap  =    gtipnt('KFHMAP')
         kfhtim  =    gtipnt('KFHTIM')
         kfmmap  =    gtipnt('KFMMAP')
         kfpmap  =    gtipnt('KFPMAP')
         kfptim  =    gtipnt('KFPTIM')
         kfrmap  =    gtipnt('KFRMAP')
         kfrtim  =    gtipnt('KFRTIM')
         kgain   =    gtrpnt('KGAIN' )
         kphmap  =    gtipnt('KPHMAP')
         kphtim  =    gtipnt('KPHTIM')
         kppmap  =    gtipnt('KPPMAP')
         kpptim  =    gtipnt('KPPTIM')
         nclfcp  =    gtipnt('NCLFCP')
         nclfga  =    gtipnt('NCLFGA')
         nclfhy  =    gtipnt('NCLFHY')
         nclfpa  =    gtipnt('NCLFPA')
         nclfre  =    gtipnt('NCLFRE')
         nclphy  =    gtipnt('NCLPHY')
         nclppa  =    gtipnt('NCLPPA')
         nclrst  =    gtipnt('NCLRST')
         nfcpmn  = ip(gtipnt('NFCPMN'))
         nfgamn  = ip(gtipnt('NFGAMN'))
         nfhymn  = ip(gtipnt('NFHYMN'))
         nfpamn  = ip(gtipnt('NFPAMN'))
         nfremn  = ip(gtipnt('NFREMN'))
         ngrid   = ip(gtipnt('NGRID' ))
         nkapar  = ip(gtipnt('NKAPAR'))
         nkfgmp  = ip(gtipnt('NKFGMP'))
         nkfhmp  = ip(gtipnt('NKFHMP'))
         nkfhtm  = ip(gtipnt('NKFHTM'))
         nkfmmp  = ip(gtipnt('NKFMMP'))
         nkfpmp  = ip(gtipnt('NKFPMP'))
         nkfptm  = ip(gtipnt('NKFPTM'))
         nkfrmp  = ip(gtipnt('NKFRMP'))
         nkfrtm  = ip(gtipnt('NKFRTM'))
         nkphmp  = ip(gtipnt('NKPHMP'))
         nkphtm  = ip(gtipnt('NKPHTM'))
         nkppmp  = ip(gtipnt('NKPPMP'))
         nkpptm  = ip(gtipnt('NKPPTM'))
         nnf     = ip(gtipnt('NNF'   ))
         nnmu    = ip(gtipnt('NNMU'  ))
         nphymn  = ip(gtipnt('NPHYMN'))
         nppamn  = ip(gtipnt('NPPAMN'))
         np      = ip(gtipnt('NP'    ))
         nsamp   = ip(gtipnt('NSAMP' ))
         pfa     =    gtrpnt('PFA'   )
         pmua    =    gtrpnt('PMUA'  )
         pw      =    gtrpnt('PW'    )
         phycpr  =    gtipnt('PHYCPR')
         ppacpr  =    gtipnt('PPACPR')
         p1      =    gtrpnt('P1'    )
         p2      =    gtrpnt('P2'    )
         qpack   =    gtdpnt('QPACK')
         res     =    gtrpnt('RES'   )
         rescov  =    gtrpnt('RESCOV')
         scares  =    gtrpnt('SCARES')
         smploc  =    gtipnt('SMPLOC')
         gridnm  =    gtcpnt('GRIDNM')
         corrnm  =    gtcpnt('CORRNM')
         kalini  =    gtipnt('KALINI')
         call kawres (    itim    ,   istep   ,   nstep   ,   fkalm   ,
     &       writim   ,   juer    ,   ngrid   , rp(tmpgr ), wrirst    ,
     &     lp(lfilt)  ,ip(fd_nefis_res), ip(fd_nefis_rst)             ,
     &       nkphmp   ,  nkphtm   ,  nphymn   ,ip(kphmap) ,ip(kphtim) ,
     &     ip(phycpr) ,  nkppmp   ,  nkpptm   ,  nppamn   ,ip(kppmap) ,
     &                 ip(kpptim) ,ip(ppacpr) ,  nkfhmp   ,  nkfhtm   ,
     &                   nfhymn   ,ip(kfhmap) ,ip(kfhtim) ,ip(fhycpr) ,
     &                   nkfpmp   ,  nkfptm   ,  nfpamn   ,ip(kfpmap) ,
     &                 ip(kfptim) ,ip(fpacpr) ,  nkfrmp   ,  nkfrtm   ,
     &                   nfremn   ,ip(kfrmap) ,ip(kfrtim) ,ip(frecpr) ,
     &                   nkfgmp   ,  nfgamn   ,ip(kfgmap) ,ip(fgacpr) ,
     &                   nkfmmp   ,  nfcpmn   ,ip(kfmmap) ,ip(fcpcpr) ,
     &                   cpredn   ,  np       ,rp(p1)     ,rp(p2)     ,
     &                 dp(hpack)  ,dp(qpack)  ,  nnf      ,rp(pfa)    ,
     &                   nnmu     ,rp(pmua)   ,rp(pw)     ,  nkapar   ,
     &                   nsamp    ,rp(res)    ,rp(scares) ,rp(rescov) ,
     &                 rp(kgain)  ,ip(nclphy) ,ip(nclppa) ,ip(nclfhy) ,
     &                 ip(nclfpa) ,ip(nclfre) ,ip(nclfga) ,ip(nclfcp) ,
     &                 ip(nclrst) ,ip(smploc) ,cp(gridnm) ,cp(corrnm) ,
     &                 ip(kalini) ,nefhis     ,dtf        ,ker     )
c
      endif
c
c     Output of SALT results
c
      if ((lsalt) .and. (ker .ne. fatal)) then
c
c        Fetch pointers of salt arrays
c
         buffer  =    gtrpnt('BUFFER')
         cdcdx   =    gtrpnt('CDCDX' )
         csa     =    gtrpnt('CSA'   )
         csd     =    gtrpnt('CSD'   )
         disgr   =    gtrpnt('DISGR' )
         dsopt   = ip(gtipnt('DSOPT' ))
         gridnm  =    gtcpnt('GRIDNM')
         mouqpu  =    gtrpnt('MOUQPU')
         nboun   = ip(gtipnt('NBOUN' ))
         nbran   = ip(gtipnt('NBRAN' ))
         ncelsa  =    gtipnt('NCELSA')
         ngrid   = ip(gtipnt('NGRID' ))
         nmouth  = ip(gtipnt('NMOUTH'))
         nsaman  = ip(gtipnt('NSAMAN'))
         nsamap  = ip(gtipnt('NSAMAP'))
         nsatim  = ip(gtipnt('NSATIM'))
         ntmpgr  = ip(gtipnt('NTMPGR'))
         rho     =    gtrpnt('RHO'   )
         sacpre  =    gtipnt('SACPRE')
         salini  =    gtipnt('SALINI')
         salmap  =    gtipnt('SALMAP')
         saltim  =    gtipnt('SALTIM')
         sbdscr  =    gtrpnt('SBDSCR')
         sbdpar  =    gtrpnt('SBDPAR')
         thasca  =    gtrpnt('THASCA')
         thcsum  =    gtrpnt('THCSUM')
         timout  =    gtrpnt('TIMOUT')
         tmpgr   =    gtrpnt('TMPGR' )
c
         call sawres (    dsopt   ,   nmouth  , nboun     ,   nbran   ,
     +                    ngrid   ,   nsamap  , nsatim    ,   nsaman  ,
     +                    ntmpgr  ,   itim    , sngl(time),   istep   ,
     +                    nstep   ,   fsalt   , writim    ,   wrirst  ,
     +                    juer    ,
     +                 ip(fd_nefis_res), ip(fd_nefis_rst)             ,
     +                 ip(sacpre) ,ip(salmap) ,ip(saltim) ,rp(csa)    ,
     +                 rp(csd)    ,rp(disgr)  ,rp(cdcdx)  ,rp(thasca) ,
     +                 rp(mouqpu) ,rp(thcsum) ,rp(timout) ,rp(sbdscr) ,
     +                 rp(sbdpar) ,rp(rho)    ,rp(tmpgr)  ,ip(ncelsa) ,
     +                 ker        ,dtf        ,ip(salini) ,rp(buffer) ,
     +                 cp(gridnm) ,nefhis     )
      endif
c
c     Output of SEDIMENT results
c
      if ((lsedt) .and. (ker .ne. fatal)) then
c
c        Fetch pointers of sediment arrays
c
         ixpar   = gtrpnt ( 'FLWPAR' )
         g       = sorpar ( rp(ixpar), 1 )
         ixpar   = gtrpnt ( 'SEDPAR' )
         delta   = sorpar ( rp(ixpar), 2 )
c         
         afwfqs  =    gtrpnt('AFWFQS')
         cpack   =    gtrpnt('CPACK' )
         branch  =    gtipnt('BRANCH')
         dissed  =    gtrpnt('DISSED')
         gridnm  =    gtcpnt('GRIDNM')
         grsize  =    gtrpnt('GRSIZE')
         nbran   = ip(gtipnt('NBRAN' ))
         ncelse  =    gtipnt('NCELSE')
         ngrid   = ip(gtipnt('NGRID' ))
         nsedrd  = ip(gtipnt('NSEDRD'))
         nseman  = ip(gtipnt('NSEMAN'))
         nsemap  = ip(gtipnt('NSEMAP'))
         nsetim  = ip(gtipnt('NSETIM'))
         ntmpgr  = ip(gtipnt('NTMPGR'))
         secpre  =    gtipnt('SECPRE')
         sedini  =    gtipnt('SEDINI')
         sedmap  =    gtipnt('SEDMAP')
         sedtim  =    gtipnt('SEDTIM')
         sedtr   =    gtrpnt('SEDTR' )
         typcr   =    gtipnt('TYPCR' )
         trform  =    gtrpnt('TRFORM')
         tmpgr   =    gtrpnt('TMPGR' )
c
         call sewres (    nseman  ,   nsemap  ,   nsetim  ,   ngrid   ,
     +                    nbran   ,   nsedrd  ,   ntmpgr  ,   itim    ,
     +                    istep   ,   nstep   ,   fsedt   ,   writim  ,
     +                    juer    ,ip(branch) ,ip(typcr)  ,   lmorp   ,
     +                 ip(fd_nefis_res) ,ip(sedmap) ,ip(sedtim) ,
     +                 rp(sedtr)  ,rp(dissed) ,rp(afwfqs) ,rp(cpack)  ,
     +                 rp(trform) ,rp(grsize) ,ip(ncelse) ,ip(secpre) ,
     +                 rp(tmpgr)  ,   dtf     ,   delta   ,   g       ,
     +                 ip(sedini) ,rp(buffer) ,cp(gridnm) ,   nefhis  )
      endif
c
c     Output of MORPHOLOGY results
c
      if ((lmorp) .and. (ker .ne. fatal)) then
c
c        Fetch pointers of morphology arrays
c
         branch  =    gtipnt('BRANCH')
         buffer  =    gtrpnt('BUFFER')
         dissed  =    gtrpnt('DISSED')
         gridnm  =    gtcpnt('GRIDNM')
         hlev    =    gtdpnt('HLEV'  )
         hlev0   =    gtrpnt('HLEV0' )
         maxlev  = ip(gtipnt('MAXLEV'))
         mocpre  =    gtipnt('MOCPRE')
         morini  =    gtipnt('MORINI')
         mormap  =    gtipnt('MORMAP')
         mortim  =    gtipnt('MORTIM')
         nbran   = ip(gtipnt('NBRAN' ))
         ncelmo  =    gtipnt('NCELMO')
         ngrid   = ip(gtipnt('NGRID' ))
         nlev    =    gtipnt('NLEV'  )
         nmoman  = ip(gtipnt('NMOMAN'))
         nmomap  = ip(gtipnt('NMOMAP'))
         nmotim  = ip(gtipnt('NMOTIM'))
         ntmpgr  = ip(gtipnt('NTMPGR'))
         sectc   =    gtrpnt('SECTC' )
         sectv   =    gtrpnt('SECTV' )
         sedtr   =    gtrpnt('SEDTR' )
         sumda   =    gtrpnt('SUMDA' )
         tmpgr   =    gtrpnt('TMPGR' )
         typcr   =    gtipnt('TYPCR' )
c
         x       =    gtrpnt('X'     )
         hpack   =    gtdpnt('HPACK')
         wft     =    gtrpnt('WFT'   )
c
         call motrou (    ngrid   ,   istep   ,ip(nlev)   ,rp(x)      ,
     &                 dp(hpack)  ,ip(branch) ,rp(wft)    ,dp(hlev)   ,
     &                 sngl(time) ,itim       ,juer       ,   ker     )
c
         call mowres (    ngrid   ,   nbran   ,   maxlev  ,   nmomap  ,
     +                    nmotim  ,   nmoman  ,   ntmpgr  ,   itim    ,
     +                    istep   ,   nstep   ,   fmorp   ,   writim  ,
     +                    wrirst  ,   juer    ,ip(fd_nefis_res) ,
     +                 ip(fd_nefis_rst) ,ip(mocpre) ,ip(mormap) ,
     +                 ip(mortim) ,dp(hlev)   ,rp(sedtr)  ,rp(dissed) ,
     +                 ip(branch) ,ip(typcr)  ,rp(tmpgr)  ,ip(ncelmo) , 
     +                    dtf     ,ip(morini) ,rp(buffer) ,rp(sectc)  , 
     +                 rp(sectv)  ,rp(wft)    ,rp(sumda)  ,rp(hlev0)  ,
     +                 cp(gridnm) ,  nefhis   ,   ker     )
c     
      endif
c
c     Output of GRADED SEDIMENT results
c
      if ((lgrad) .and. (ker .ne. fatal)) then
c
c        Fetch pointers of sediment arrays
c
         branch  =    gtipnt('BRANCH')
         deff    =    gtrpnt('DEFF'  )
         depos   =    gtlpnt('DEPOS' )
         disgse  =    gtrpnt('DISSED')         
         dmed0   =    gtrpnt('DMED0' )
         dzr     =    gtrpnt('DZR'   )
         frcmap  =    gtipnt('FRCMAP')                 
         frctim  =    gtipnt('FRCTIM')      
         gridnm  =    gtcpnt('GRIDNM')
         grsize  =    gtrpnt('GRSIZE')
         grsizmun=    gtrpnt('GRSIZMUN')         
         lanrinbt=    gtipnt('LANRINBT')
         levunl =     gtrpnt('LEVUNL')         
         nbran   = ip(gtipnt('NBRAN' ))
         ncelse  =    gtipnt('NCELSE')
         nfrac   = ip(gtipnt('NFRAC' ))
         nfrcmap = ip(gtipnt('NFRCMAP'))              
         nfrctim = ip(gtipnt('NFRCTIM'))   
         ngrid   = ip(gtipnt('NGRID' ))
         ngrain  = ip(gtipnt('NGRAIN'))
         nlayer  = ip(gtipnt('NLAYER'))
         nrdzdl  =    gtipnt('NRDZDL')
         nunlay  = ip(gtipnt('NUNLAY'))
         nseman  = ip(gtipnt('NSEMAN'))
         nsemap  = ip(gtipnt('NSEMAP'))
         nsetim  = ip(gtipnt('NSETIM'))
         ntmpgr  = ip(gtipnt('NTMPGR'))
         p0la    =    gtrpnt('P0LA')
         pexla   =    gtrpnt('PEXLA')
         ptrla   =    gtrpnt('PTRLA')
         secpre  =    gtipnt('SECPRE')
         sedini  =    gtipnt('SEDINI')
         sedmap  =    gtipnt('SEDMAP')
         sedpar  =    gtrpnt('SEDPAR')
         sedtim  =    gtipnt('SEDTIM')
         sedtr   =    gtrpnt('SEDTR' )
         secpre  =    gtipnt('SECPRE')
         submin  = ip(gtipnt('SUBMIN'))
         subplus = ip(gtipnt('SUBPLUS'))
         tmpgr   =    gtrpnt('TMPGR' )                
         zbave   =    gtrpnt('ZBAVE' )
         zbfl    =    gtrpnt('ZBFL'  )
c
         dzunla = sedpar+10
         grain  = sedini+10
         call gswres (    nseman  ,   nsemap  ,   nsetim  ,   ngrid    ,
     +                    nlayer  ,   nunlay  ,   nbran   ,   ntmpgr   ,
     +                    nfrac   ,   ngrain  ,   submin  ,   subplus  ,
     +                    itim    ,   istep   ,   nstep   ,   fgrad    ,
     +                    writim  ,   wrirst  ,   dtf     , 
     +                 ip(branch) ,ip(fd_nefis_res) ,ip(fd_nefis_rst)  ,
     +                 ip(sedmap) ,ip(sedtim) ,rp(sedtr)   ,
     +                 rp(disgse) ,rp(grsize) ,ip(grain)  ,rp(grsizmun),
     +                 rp(dmed0)  ,rp(deff)   ,lp(depos)  ,ip(ncelse)  ,
     +                 ip(secpre) ,rp(tmpgr ) ,ip(sedini) ,rp(buffer)  ,
     +                 cp(gridnm) ,   nefhis  ,   nfrcmap ,   nfrctim  ,
     +                 ip(frcmap) ,ip(frctim) ,ip(lanrinbt)            ,
     +                 ip(nrdzdl) ,rp(zbave)  ,rp(zbfl)   ,rp(dzunla)  ,
     +                 rp(dzr)    ,rp(levunl) ,rp(p0la)   ,rp(ptrla)   ,
     +                 rp(pexla)  ,   juer    ,   ker     )
c
c        Fetch pointers of morphology arrays
c
         buffer  =    gtrpnt('BUFFER')
         hlev    =    gtdpnt('HLEV'  )
         hlev0   =    gtrpnt('HLEV0' )
         gridnm  =    gtcpnt('GRIDNM')
         maxlev  = ip(gtipnt('MAXLEV'))
         mocpre  =    gtipnt('MOCPRE')  
         morini  =    gtipnt('MORINI')
         mormap  =    gtipnt('MORMAP')
         mortim  =    gtipnt('MORTIM')
         ncelmo  =    gtipnt('NCELMO')
         ngrid   = ip(gtipnt('NGRID' ))
         nmoman  = ip(gtipnt('NMOMAN'))
         nmomap  = ip(gtipnt('NMOMAP'))
         nmotim  = ip(gtipnt('NMOTIM'))
         sectc   =    gtrpnt('SECTC' )
         sectv   =    gtrpnt('SECTV' )
         sumda   =    gtrpnt('SUMDA' )
         typcr   =    gtipnt('TYPCR' )
         wft     =    gtrpnt('WFT'   )
c 
         call mowres (    ngrid   ,   nbran   ,   maxlev  ,   nmomap  ,
     +                    nmotim  ,   nmoman  ,   ntmpgr  ,   itim    ,
     +                    istep   ,   nstep   ,   fmorp   ,   writim  ,
     +                    wrirst  ,   juer    ,ip(fd_nefis_res) ,
     +                 ip(fd_nefis_rst) ,ip(mocpre) ,ip(mormap) ,
     +                 ip(mortim) ,dp(hlev)   ,rp(sedtr)  ,rp(disgse) ,
     +                 ip(branch) ,ip(typcr)  ,rp(tmpgr)  ,ip(ncelmo) , 
     +                    dtf     ,ip(morini) ,rp(buffer) ,rp(sectc)  , 
     +                 rp(sectv)  ,rp(wft)    ,rp(sumda)  ,rp(hlev0)  ,
     +                 cp(gridnm) ,  nefhis   ,   ker     )
      endif
c
c     Flush results to result file if switch 'writim' is true
c
      if (writim) then
        errcod = flsdat (ip(fd_nefis_res))
      endif
c
      return
      end
