      subroutine SOINIT ( filnam ,itim  ,itp   ,dtf   ,lauto ,lflow  ,
     +                    lgrwt  ,
     +                    lkalm  ,lsalt ,lsedt ,lmorp ,lgrad ,lwqin  ,
     +                    lrest  ,newres,sbkrel,coding,juer  ,
     +                    juresi ,jufrou,juresd,justrd,ker   ,inocon ,
     +                    jusold ,lfrou ,itstat,lhisgp,frobuf,jugraut,
     +                    jugralg)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOINIT (SObek INITialise)
c
c Module description: Preceding the actual simulation, in subroutine
c                     SOINIT several auxiliary arrays will be computed
c                     to be used later on in the computational kernel.
c                     In case the auto start option has been selected a
c                     steady state computation will be exectuted on time
c                     t = 0.
c
c                     If the morphological module is included in the
c                     application first the initialisation routine of
c                     the morphological sub system will be called. If a
c                     restart file is available this will result in
c                     reading the adapted cross sections. The next step
c                     is calling CSTABL. This routine will insert preis-
c                     smann slots and calculates the auxiliary tables
c                     for areas and wetted perimeters. After this the
c                     initialisation routines of the flow module will be
c                     called. If the auto start option is selected a
c                     steady state computation will be executed by cal-
c                     ling SONOMO. After this the initialisation routi-
c                     nes of the salt and sediment module will be cal-
c                     led.
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 15 coding            P  -
c  4 dtf               I  Time step flow module
c  1 filnam            P  -
c 22 inocon            O  Number of timesteps continuated without
c                         convergence
c  2 itim              P  -
c  3 itp               I  Tidal period in whole numbers of flow step
c 25 itstat            P  -
c 16 juer              P  -
c 18 jufrou            P  -
c 17 juresi            P  -
c 23 jusold            P  -
c 19 justru            P  -
c 21 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  5 lauto             I  Switch to execute autostart procedure
c  6 lflow             I  Switch to enable flow module
c 24 lfrou             O  Flag if Froude numbers during simulation are
c                         high
c  7 lkalm             I  -
c 10 lmorp             I  Logical indicator for morphology computation
c                         = .true.  : with morphology computation
c                         = .false. : without morphology computation
c 12 lrest             O  Switch to indicate that restart file has been
c                         read
c  8 lsalt             I  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c  9 lsedt             I  Switch to enable sediment transport module
c 11 lwqin             I  Logical, = TRUE indicates the  water quality
c                         interface file must be written.
c 13 newres            I  true, if a new restart file will be made
c 14 sbkrel            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c cstabl  Cross Sectional TABLe module
c flini   FLow INItialise
c gtipnt  GeT Integer PoiNTer
c gtlpnt  GeT Logical PoiNTer
c gtrpnt  GeT Real PoiNTer
c kaini   KAlman INItialisation
c moini   MOrphology INItialise
c saini   SAlt INItialise
c seini   SEdiment INItialise
c soadna  SObek ADd NAmes of "lateral" structures
c socnam  SObek Create NAMe
c sonomo  SObek NO MOrphology
c soofil  SObek Open FILe
c sorpar  SObek Real PARameter
c sowrel  SObek Write RELease number
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
c $Log: soinit.pf,v $
c Revision 1.34  1999/07/23  15:10:37  kuipe_j
c improve restart
c
c Revision 1.33  1999/06/01  13:42:49  kuipe_j
c names in messages substituted + message template
c
c Revision 1.32  1999/04/22  08:45:19  kuipe_j
c remove initial ODA/ODF and WDA/WDF files
c
c Revision 1.31  1999/03/15  15:03:31  kuipe_j
c Improve Froude file and Dumpfiles
c
c Revision 1.30  1998/12/11  10:56:29  kuipe_j
c Throw existing NDA/NDF file away
c
c Revision 1.28  1998/06/11  11:47:43  kuipe_j
c Estuary special integrated
c
c Revision 1.27  1998/06/08  13:15:38  kuipe_j
c time lag hydr controller
c
c Revision 1.26  1998/02/25  12:49:06  kuipe_j
c Check on grain size added
c
c Revision 1.25  1998/02/13  13:22:35  kuipe_j
c Adapt to CMT ; SRS BOS
c
c Revision 1.24  1997/11/04  14:19:59  kuipe_j
c Retention basin
c
c Revision 1.23  1997/06/17  11:29:24  kuipe_j
c output in history format
c
c Revision 1.22  1997/06/04  11:20:17  kuipe_j
c Initialize arrays
c
c Revision 1.21  1997/05/26  07:37:01  kuipe_j
c statistic of iteration improved
c
c Revision 1.20  1997/02/17  10:09:39  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.19  1997/01/23  08:30:12  kuipe_j
c Make flow module robust
c
c Revision 1.18  1996/11/01  11:25:44  kuipe_j
c Update of Delwaq input file added
c
c Revision 1.17  1996/10/31  13:03:49  kuipe_j
c Extra resistance finished, Exchanges are calculated
c
c Revision 1.16  1996/09/03  14:33:43  kuipe_j
c frequency time hist, run in time est. morp
c
c Revision 1.15  1996/04/12  13:06:06  kuipe_j
c headers, minor changes
c
c Revision 1.14  1996/04/11  08:16:33  kuipe_j
c Kalman module added
c
c Revision 1.13  1996/03/07  10:44:30  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.12  1996/02/09  15:13:37  kuipe_j
c a.o. Restart improvements
c
c Revision 1.11  1996/01/17  14:47:38  kuipe_j
c header update
c
c Revision 1.10  1996/01/16  15:01:55  kuipe_j
c Restart improvements
c
c Revision 1.9  1995/10/18  09:01:04  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.8  1995/09/29  10:36:39  kuipe_j
c Improvement of autostart and simple weir
c
c Revision 1.7  1995/09/22  10:04:19  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:11:31  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:37:34  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:51  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:56:59  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:53  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:18  hoeks_a
c Initial check-in
c

c Revision 1.4  1994/12/02  13:33:29  kuipe_j
c Improvement of message handling.
c
c Revision 1.3  1994/11/28  08:28:39  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:09:59  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:43  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      use flow_in_datools
c
c     Parameters
c
      character*(*) filnam
      character*1   coding
#if defined (SHR_MEM)
c ====  shared memory  ====
      character*3   shmcod
#endif
      logical       lauto  ,lkalm  ,lflow  ,lsalt  ,lsedt  ,lmorp ,
     +              lgrad , lwqin  ,lrest  ,newres ,lfrou  ,lhisgp,
     +              lgrwt
      integer       juer   ,ker    ,itp    ,itim(2),sbkrel(3),
     +              juresi ,jufrou ,juresd ,justrd ,inocon   ,
     +              jusold ,itstat(4)      ,nefhis ,jugraut  ,jugralg
      double  precision     dtf
      real          frobuf (8)
c
c     Variables (pointers to arrays or single values from pool)
c
      integer  aft   ,af2   ,att   ,afwfqs,alfab ,arex  ,arexcn,arexop,
     +         bfricp,bfrict,bramrl,branch,brnode,buflag,cdcdx ,celer ,
     +         conhis,cnstrl,contrl,cpack ,csa   ,csd   ,deff  ,deltar,
     +         depos ,ddis  ,dfrac ,dmed0 ,duncon,duneh ,dunel ,dzr   ,
     +         disgse,delh  ,disgr ,dispf ,dissed,dsopt ,engpar,exres ,
     +         forcon,grid  ,grsize,gsopts,hbdpar,hlev  ,hlev0 ,hpack ,
     +         hstat,izwft,lanrinbt,lagstm,levunl,lfilt ,maxlev,maxtab,
     +         mltpar,moupar,mouqpu,mouth ,nboun ,nbran ,nbrnod,ncelfl,
     +         ncelmo,nclrst,ncelsa,ncelse,ncontr,ncsrel,nexres,nfrac ,
     +         ngrid ,nhstat,nlags ,nlayer,nlev  ,nmlat ,nmouth,nnc   ,
     +         nellvl,nnelvl,nnf   ,nnm   ,nnmu  ,nnn   ,nnode ,nns   ,
     +         np    ,nodnod,nonall,nqlat ,nqstat,nrdzdl,nsamp ,nosdim
     
      integer  nsedrd,nslat ,nstru ,ntab  ,ntabm ,ntmpgr,nucoef,numnod,
     +         nunlay,of    ,pfa   ,pmua  ,pw    ,prslot,psltvr,pexla ,
     +         ptrla ,p0la  ,p1    ,p2    ,qaggr ,qbdpar,
     +         qlaggr,qlat  ,qlatgr,qltpar,qpack ,qstat ,res   ,rescov,
     +         rpack ,rho   ,sbdpar,sbdscr,scares,scceq ,scmeq ,scqhs ,
     +         scfric,scmu  ,scifri,scimu ,sclceq,sclmeq,sclqhs,sclnod,
     +         sclfri,sclmu ,scnode,sectc ,sectv ,sedexp,sedtr ,sedinf,
     +         sedpar,sltpar,smpns ,snceq ,snmeq ,snqhs ,snnode,snfric,
     +         snmu  ,snwind,strclo,strhis,strpar,strtyp,sumda ,table ,
     +         thasca,thcsum,timout,tmpfr ,tmpgr ,trform,typcr ,tw    ,
     +         uscoef,waoft ,wft   ,wf2,ws,wtt   ,x     ,ibuf  ,resbuf,
     +         strbuf,solbuf,gridnm,strunm,qlatnm,morini,corrnm,zbave ,
     +         zbfl  ,flwini,salini,sedini,kalini,
     +         grhis ,strtim
c
c     Local variables
c
      integer         ixpar ,istep ,cpredn ,filstp, lutemp
      real            g     ,rhow  ,el
      real            flitmx,overlp,lambda ,theta, dhstru

      double  precision      time ,tp
      logical         inires,steady,tmpsal,tmpsed,tmpmor,tmpriv,tmpkal
c     mozart dummy
      logical         lmozad     
      integer         nstepd  
c
c     File variables
c
      character*256   defnam ,datnam
      character*4     ext
      integer         fd_nefis_rst, fd_nefis_new , fd_nefis_res ,
     +                fd_nefis_waq
c
c     External functions
c
      integer         gtrpnt,gtipnt,gtlpnt,gtdpnt,gtcpnt,soipar
      real            sorpar
      external        gtrpnt,gtipnt,gtlpnt,gtdpnt,gtcpnt,soipar,sorpar
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
      include '..\include\filsim.i'
c
      if ( da_running_in_da_tools() ) then
c        check timing inconsistency
         if ( .not. da_check_model_start_time(itim, juer) ) then
            ker = fatal
            return        
         endif
      endif
c
c     Set initial value of number of timesteps without convergence
c     to zero
c

      inocon = 0
c
c     Set Froude flag to false
c
      lfrou = .false.
c
c     Only open restart file in case at least flow module is included
c
      if (lflow) then
c
c       Find pointers of descriptors
c
        fd_nefis_rst = gtipnt ('FD_NEFIS_RST')
c
c       Open the restart files
c
        call soofil(coding, ip(fd_nefis_rst), nefrdf, nefrda, juer, ker)
c
c       Find pointers of descriptors of new restart file
c
        fd_nefis_new = gtipnt ('FD_NEFIS_NEW')

        if (newres) then
c
c          Open definition and data file
c          of a new restart file (the old file is still
c          present).
c
           lutemp = 31
           open (lutemp,file=nefnda)
           close (lutemp,status='delete')
           open (lutemp,file=nefndf)
           close (lutemp,status='delete')

           call soofil(coding, ip(fd_nefis_new), nefndf, nefnda,
     +                  juer    ,   ker     )
c
         endif
      endif
c
c     Morphology module, should be the first one to read adapted CS
c
      if ((lmorp.or.lgrad) .and. (ker .ne. fatal)) then
c
c        Read pointers of variables
c
         branch  =    gtipnt ('BRANCH')
         grid    =    gtipnt ('GRID'  )
         hlev    =    gtdpnt ('HLEV'  )
         hlev0   =    gtrpnt ('HLEV0'  )
         maxlev  = ip(gtipnt ('MAXLEV'))
         mltpar  =    gtrpnt ('MLTPAR')
         morini  =    gtipnt ('MORINI')
         nbran   = ip(gtipnt ('NBRAN' ))
         ncelmo  =    gtipnt ('NCELMO')
         ngrid   = ip(gtipnt ('NGRID' ))
         nmlat   = ip(gtipnt ('NMLAT' ))
         sumda   =    gtrpnt ('SUMDA' )

         call moini (   ngrid   ,   nbran   ,   nmlat   ,   maxlev  ,
     +                  itim    ,ip(branch) ,rp(mltpar) ,dp(hlev)   ,
     +               rp(hlev0)  ,ip(grid  ) ,   newres   , 
     +               ip(fd_nefis_rst) ,ip(fd_nefis_new) ,   juer    , 
     +               ip(ncelmo) ,   ker     ,ip(morini) ,rp(sumda)  ,
     +                  lgrad   )
      endif
c
c     Call CSTABL to calculate all tables
c
      if ((lflow) .and. (ker .ne. fatal)) then
c
c        Read pointers of variables
c
         aft     =    gtrpnt ('AFT'   )
         att     =    gtrpnt ('ATT'   )
         bfricp  =    gtrpnt ('BFRICP')
         bfrict  =    gtipnt ('BFRICT')
         cnstrl  =    gtipnt ('CNSTRL')
         branch  =    gtipnt ('BRANCH')
         engpar  =    gtrpnt ('ENGPAR')
         grsize  =    gtrpnt ('GRSIZE')
         hlev    =    gtdpnt ('HLEV'  )
         izwft   =    gtrpnt ('IZWFT' )
         maxlev  = ip(gtipnt ('MAXLEV'))
         maxtab  = ip(gtipnt ('MAXTAB'))
         nbran   = ip(gtipnt ('NBRAN' ))
         ngrid   = ip(gtipnt ('NGRID' ))
         nlev    =    gtipnt ('NLEV'  )
         ntab    =    gtipnt ('NTAB'  )
         ntabm   = ip(gtipnt ('NTABM' ))
         of      =    gtrpnt ('OF'    )
         prslot  =    gtrpnt ('PRSLOT')
         psltvr  =    gtrpnt ('PSLTVR')
         sectc   =    gtrpnt ('SECTC' )
         sectv   =    gtrpnt ('SECTV' )
         table   =    gtrpnt ('TABLE' )
         typcr   =    gtipnt ('TYPCR' )
         wft     =    gtrpnt ('WFT'   )
         wtt     =    gtrpnt ('WTT'   )
c        In case no sediment module is present ws points to scratch
c        array
         if (lsedt) then
            ws      =    gtrpnt ('WS'    )
         else   
            ws      =    gtrpnt ('TMPGR' )
         endif   
c

         call cstabl (   ngrid   ,   maxlev ,ip(nlev)  ,dp(hlev)  ,
     +                 rp(wft)   ,rp(aft)   ,rp(wtt)   ,rp(att)   ,
     +                 rp(of)    ,   lsalt  ,rp(izwft) ,   nbran  ,
     +                 ip(branch),ip(typcr) ,rp(sectc) ,rp(sectv) ,
     +                 rp(prslot),rp(psltvr),ip(bfrict),rp(bfricp),
     +                 rp(engpar),rp(grsize),   maxtab ,   ntabm  ,
     +                 ip(ntab)  ,rp(table) ,rp(ws)    ,
     +                    lsedt.or.lgrad    ,   juer   ,   ker    )
      endif
c
c     Initialise flow module
c
      if ((lflow) .and. (ker .ne. fatal)) then
c
c        Read pointers of variables
c
         aft     =    gtrpnt ('AFT'   )
         att     =    gtrpnt ('ATT'   )
         afwfqs  =    gtrpnt ('AFWFQS')
         alfab   =    gtrpnt ('ALFAB' )
         arex    =    gtrpnt ('AREX'  )
         arexcn  =    gtipnt ('AREXCN')
         arexop  =    gtipnt ('AREXOP')
         branch  =    gtipnt ('BRANCH')
         brnode  =    gtipnt ('BRNODE')
         bfricp  =    gtrpnt ('BFRICP')
         bfrict  =    gtipnt ('BFRICT')
         conhis  =    gtrpnt ('CONHIS')
         contrl  =    gtrpnt ('CONTRL')
         cpack   =    gtrpnt ('CPACK' )
         delh    =    gtdpnt ('DELH'  )
         engpar  =    gtrpnt ('ENGPAR')
         exres   =    gtrpnt ('EXRES' )
         flwini  =    gtipnt ('FLWINI')
         grid    =    gtipnt ('GRID'  )
         grsize  =    gtrpnt ('GRSIZE')
         hbdpar  =    gtipnt ('HBDPAR')
         hlev    =    gtdpnt ('HLEV'  )
         hpack   =    gtdpnt ('HPACK' )
         hstat   =    gtrpnt ('HSTAT' )
         ibuf    =    gtipnt ('IBUF'  )
         lfilt   =    gtlpnt ('LFILT' )
         maxlev  = ip(gtipnt ('MAXLEV'))
         maxtab  = ip(gtipnt ('MAXTAB'))
         nbran   = ip(gtipnt ('NBRAN' ))
         nbrnod  = ip(gtipnt ('NBRNOD'))
         ncelfl  =    gtipnt ('NCELFL')
         ncontr  = ip(gtipnt ('NCONTR'))
         ncsrel  = ip(gtipnt ('NCSREL'))
         nexres  = ip(gtipnt ('NEXRES'))
         ngrid   = ip(gtipnt ('NGRID' ))
         nhstat  = ip(gtipnt ('NHSTAT'))
         nlev    =    gtipnt ('NLEV'  )
         nnode   = ip(gtipnt ('NNODE' ))
         nodnod  =    gtipnt ('NODNOD')
         nqlat   = ip(gtipnt ('NQLAT' ))
         nqstat  = ip(gtipnt ('NQSTAT'))
         nstru   = ip(gtipnt ('NSTRU' ))
         ntab    =    gtipnt ('NTAB'  )
         ntabm   = ip(gtipnt ('NTABM' ))
         numnod  =    gtipnt ('NUMNOD')
         of      =    gtrpnt ('OF'    )
         pfa     =    gtrpnt ('PFA'   )
         pmua    =    gtrpnt ('PMUA'  )
         prslot  =    gtrpnt ('PRSLOT')
         psltvr  =    gtrpnt ('PSLTVR')
         pw      =    gtrpnt ('PW'    )
         qaggr   =    gtrpnt ('QAGGR' )
         qbdpar  =    gtipnt ('QBDPAR')
         qlaggr  =    gtrpnt ('QLAGGR')
         qlat    =    gtrpnt ('QLAT'  )
         qlatgr  =    gtrpnt ('QLATGR')
         qltpar  =    gtrpnt ('QLTPAR')
         qpack   =    gtdpnt ('QPACK' )
         qstat   =    gtrpnt ('QSTAT' )
         resbuf  =    gtrpnt ('RESBUF')
         rho     =    gtrpnt ('RHO'   )
         rpack   =    gtrpnt ('RPACK' )
         scceq   =    gtipnt ('SCCEQ' )
         scifri  =    gtipnt ('SCIFRI')
         scimu   =    gtipnt ('SCIMU' )
         sclceq  =    gtipnt ('SCLCEQ')
         sclmeq  =    gtipnt ('SCLMEQ')
         sclnod  =    gtipnt ('SCLNOD')
         sclqhs  =    gtipnt ('SCLQHS')
         scmeq   =    gtipnt ('SCMEQ' )
         scnode  =    gtipnt ('SCNODE')
         scqhs   =    gtipnt ('SCQHS' )
         sectc   =    gtrpnt ('SECTC' )
         sectv   =    gtrpnt ('SECTV' )
         solbuf  =    gtrpnt ('SOLBUF')
         strbuf  =    gtrpnt ('STRBUF')
         strclo  =    gtlpnt ('STRCLO')
         strhis  =    gtrpnt ('STRHIS')
         strpar  =    gtrpnt ('STRPAR')
         strtyp  =    gtipnt ('STRTYP')
         table   =    gtrpnt ('TABLE' )
         typcr   =    gtipnt ('TYPCR' )
         waoft   =    gtrpnt ('WAOFT' )
         wft     =    gtrpnt ('WFT'   )
         wtt     =    gtrpnt ('WTT'   )
         x       =    gtrpnt ('X'     )
         grhis   =    gtrpnt ('GRHIS' )
         gridnm  =    gtcpnt ('GRIDNM')
         strunm  =    max(1,gtcpnt ('STRUNM'))
         qlatnm  =    max(1,gtcpnt ('QLATNM'))
         strtim  =    gtipnt ('STRTIM')
c
c        Extract parameters from flwpar
c
         ixpar = gtrpnt ( 'FLWPAR' )
c
         g      = sorpar ( rp(ixpar), 1 )
         rhow   = sorpar ( rp(ixpar), 6 )
         lambda = sorpar ( rp(ixpar), 10)
         overlp = sorpar ( rp(ixpar), 16)
         dhstru = sorpar ( rp(ixpar), 12)
c
c        Determine size and declare time lag bugger
c
         call SOTLAG (ncontr ,rp(contrl),dtf   ,juer  ,ker  )
         if (ker.ne.fatal) then
c
         lagstm  =ip(gtipnt ('LAGSTM'))
         nlags   =ip(gtipnt ('NLAGS' ))
         buflag  =   gtrpnt ('BUFLAG')

cc        
c        Adjust structure names in case of retention basins
c

         call soadna( ip(strtim), nstru, cp(strunm), cp(qlatnm), 
     +                rp(qltpar), rp(strhis) )

         call flini(lkalm  ,lp(lfilt) ,   lwqin  ,   lauto  ,
     +        lgrwt,  g   ,rhow ,
     +   nqlat  ,   ncontr ,   nstru  ,   ngrid  ,   itim   ,   juer   ,
     +   newres ,   lambda ,   dtf    ,   dhstru ,   lagstm ,   nlags  ,
     +ip(fd_nefis_rst),ip(fd_nefis_new),dp(hpack),dp(qpack),
     +rp(conhis),rp(strpar),ip(strtyp),rp(contrl),lp(strclo),rp(x)     ,
     +rp(qaggr) ,rp(qlaggr),ip(ncelfl),   inires ,   nbran  ,ip(branch),
     +   nnode  ,   nbrnod ,ip(brnode),ip(nodnod),ip(numnod),ip(typcr) ,
     +rp(prslot),rp(psltvr),ip(bfrict),rp(bfricp),rp(grsize),rp(engpar),
     +rp(qltpar),rp(qlat)  ,rp(qlatgr),rp(cpack) ,rp(rpack) ,   maxlev ,
     +ip(nlev)  ,dp(hlev)  ,rp(wft)   ,rp(aft)   ,rp(wtt)   ,rp(att)   ,
     +   overlp ,rp(arex)  ,ip(arexcn),ip(arexop),rp(waoft) ,rp(afwfqs),
     +rp(sectc) ,rp(sectv) ,rp(of)    ,  maxtab  ,   ntabm  ,ip(ntab)  ,
     +rp(table) , nhstat   ,rp(hstat) ,ip(hbdpar),   nqstat ,rp(qstat) ,
     +ip(qbdpar),rp(alfab) ,rp(pfa)   ,rp(pmua)  ,rp(pw)    ,ip(scifri),
     +ip(scimu) ,ip(sclceq),ip(sclmeq),ip(sclqhs),ip(sclnod),ip(scceq) ,
     +ip(scmeq) ,ip(scqhs) ,ip(scnode),
     +rp(rho)   ,  ncsrel  ,rp(strhis),ip(cnstrl),ip(grid)  ,   nexres ,
     +rp(exres) ,  ker     ,ip(ibuf)  ,rp(resbuf),rp(strbuf),
     +rp(solbuf),  itstat  ,ip(flwini),   lhisgp ,cp(gridnm),cp(strunm),
     +cp(qlatnm),dp(delh)  ,rp(buflag),rp(grhis) )

         call soadna( ip(strtim), nstru, cp(strunm), cp(qlatnm), 
     +                rp(qltpar), rp(strhis) )
c
c        If inires = false a restart block was found
c
        lrest = .not. inires
      else
        lrest = .false.
      endif

      if (lauto) then
c
c        Autostart: save flwrun parameters and change itermax and
c
         ixpar = gtrpnt ( 'FLWPAR' )
c
         theta  = sorpar ( rp(ixpar), 3 )
         flitmx = sorpar ( rp(ixpar), 7 )
c
c        Change numerical parameters
c
         rp(ixpar+2) = 1.0
         rp(ixpar+6) = 999.
c
c        Set run parameters to steady and only flow module
c
         istep  = 0
         time   = 0D0
         steady = .true.
         tmpsal = .false.
         tmpsed = .false.
         tmpmor = .false.
         tmpriv = .false.
         tmpkal = .false.
         filstp = 0
         cpredn = 0
c        mozart dummy
         lmozad = .false.
         nstepd = 0
c
         call sonomo ( istep , time  , itim  , dtf  , filstp ,cpredn,
     +                 steady, itp   , tmpkal,tmpsal, tmpsed, tmpmor,
c                      mozart and groundwater dummy
     +                 lmozad,lgrwt  ,lrest  ,nstepd ,
     +                 tmpriv, juer  , juresi,jufrou ,juresd ,justrd,
     +                 ker   , inocon, jusold, lfrou ,itstat ,frobuf)

c
c        Restore original flow run parameters
c
         rp(ixpar+2) = theta
         rp(ixpar+6) = flitmx
c
      endif
c
c     Initialize time lag buffer
c
      if (.not. lrest)
     +call FLTLAG(2   ,ncontr ,rp(contrl) ,lagstm ,nlags, rp(buflag),
     +            dtf ,ngrid  ,dp(qpack+2*ngrid)  )
      endif
c
c     Initialise Kalman module
c
      if ((lkalm) .and. (ker .ne. fatal)) then
c
c        Fetch pointers of Kalman arrays
c
         af2     =    gtrpnt('AF2'   )
         lfilt   =    gtlpnt('LFILT' )
         nclrst  =    gtipnt('NCLRST')
         ngrid   = ip(gtipnt('NGRID' ))
         nnc     = ip(gtipnt('NNC'   ))
         nnf     = ip(gtipnt('NNF'   ))
         nnm     = ip(gtipnt('NNM'   ))
         nnmu    = ip(gtipnt('NNMU'  ))
         nnn     = ip(gtipnt('NNN'   ))
         nns     = ip(gtipnt('NNS'   ))
         nnode   = ip(gtipnt('NNODE' ))
         nosdim  = ip(gtipnt('NOSDIM'))
         np      = ip(gtipnt('NP'    ))
         nsamp   = ip(gtipnt('NSAMP' ))
         nstru   = ip(gtipnt('NSTRU' ))
         pfa     =    gtrpnt('PFA'   )
         pmua    =    gtrpnt('PMUA'  )
         pw      =    gtrpnt('PW'    )
         p1      =    gtrpnt('P1'    )
         p2      =    gtrpnt('P2'    )
         res     =    gtrpnt('RES'   )
         rescov  =    gtrpnt('RESCOV')
         scares  =    gtrpnt('SCARES')
         scfric  =    gtipnt('SCFRIC')
         scifri  =    gtipnt('SCIFRI')
         scimu   =    gtipnt('SCIMU' )
         sclnod  =    gtipnt('SCLNOD')
         scmu    =    gtipnt('SCMU'  )
         sclceq  =    gtipnt('SCLCEQ')
         sclfri  =    gtipnt('SCLFRI')
         sclmeq  =    gtipnt('SCLMEQ')
         sclmu   =    gtipnt('SCLMU' )
         sclqhs  =    gtipnt('SCLQHS')
         smpns   =    gtrpnt('SMPNS' )
         snceq   =    gtrpnt('SNCEQ' )
         snfric  =    gtrpnt('SNFRIC')
         snmeq   =    gtrpnt('SNMEQ' )
         snmu    =    gtrpnt('SNMU'  )
         snnode  =    gtrpnt('SNNODE')
         snqhs   =    gtrpnt('SNQHS' )
         snwind  =    gtrpnt('SNWIND')
         wf2     =    gtrpnt('WF2'   )
         corrnm  =    gtcpnt('CORRNM')
         kalini  =    gtipnt('KALINI')
c
         call KAINI (ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnn    ,
     &               nnf    ,nnmu   ,np     ,nsamp  ,itim   ,juer   ,
     &               ip(nclrst) ,inires     ,newres ,nosdim ,
     &               rp(af2)    ,rp(wf2)    ,
     &               ip(scifri) ,ip(scimu ) ,rp(snceq)  ,rp(snmeq)  ,
     &               rp(snqhs)  ,rp(snnode) ,rp(snfric) ,rp(snmu)   ,
     &               rp(snwind) ,rp(smpns)  ,ip(sclceq) ,ip(sclmeq) ,
     &               ip(sclqhs) ,ip(sclnod) ,ip(sclfri) ,ip(sclmu)  ,
     &               ip(scfric) ,ip(scmu)   ,
     &               ip(fd_nefis_rst) ,ip(fd_nefis_new) ,
     &               rp(p1)     ,rp(p2)     ,rp(pfa)    ,
     &               rp(pmua)   ,rp(pw)     ,rp(res)    ,rp(scares) ,
     &               rp(rescov) ,dp(hpack)  ,dp(qpack)  ,lp(lfilt)  ,
     &               cp(corrnm) ,ip(kalini) ,ker        )
      endif
c
c     Initialise salt module
c
      if ((lsalt) .and. (ker .ne. fatal)) then
c
c        Read pointers of variables
c
         bramrl  =    gtipnt ('BRAMRL')
         branch  =    gtipnt ('BRANCH')
         cdcdx   =    gtrpnt ('CDCDX' )
         cpack   =    gtrpnt ('CPACK' )
         csa     =    gtrpnt ('CSA'   )
         csd     =    gtrpnt ('CSD'   )
         disgr   =    gtrpnt ('DISGR' )
         dispf   =    gtipnt ('DISPF' )
         dsopt   = ip(gtipnt ('DSOPT' ))
         grid    =    gtipnt ('GRID'  )
         maxtab  = ip(gtipnt ('MAXTAB'))
         moupar  =    gtrpnt ('MOUPAR')
         mouqpu  =    gtrpnt ('MOUQPU')
         mouth   =    gtipnt ('MOUTH' )
         nboun   = ip(gtipnt ('NBOUN' ))
         nbran   = ip(gtipnt ('NBRAN' ))
         ncelsa  =    gtipnt ('NCELSA')
         ngrid   = ip(gtipnt ('NGRID' ))
         nmouth  = ip(gtipnt ('NMOUTH'))
         nslat   = ip(gtipnt ('NSLAT'))
         ntab    =    gtipnt ('NTAB'  )
         ntabm   = ip(gtipnt ('NTABM' ))
         ntmpgr  = ip(gtipnt ('NTMPGR'))
         salini  =    gtipnt ('SALINI')
         sbdpar  =    gtrpnt ('SBDPAR')
         sbdscr  =    gtrpnt ('SBDSCR')
         sltpar  =    gtrpnt ('SLTPAR')
         rho     =    gtrpnt ('RHO'   )
         table   =    gtrpnt ('TABLE' )
         thasca  =    gtrpnt ('THASCA')
         thcsum  =    gtrpnt ('THCSUM')
         timout  =    gtrpnt ('TIMOUT')
         tmpgr   =    gtrpnt ('TMPGR' )
         tw      =    gtrpnt ('TW'    )
         waoft   =    gtrpnt ('WAOFT' )
         x       =    gtrpnt ('X'     )
c
c        Extract parameters from flwpar
c
         ixpar = gtrpnt ( 'FLWPAR' )
c
         g    = sorpar ( rp(ixpar), 1 )
         rhow = sorpar ( rp(ixpar), 6 )
c
c        Extract parameters from salpar
c
         ixpar = gtrpnt ( 'SALPAR' )
c
         el   = sorpar ( rp(ixpar), 1 )
         tp   = real(itp) * dtf
         time = 0D0
c
         call saini (   dsopt   ,   nmouth  ,   nboun   ,   nbran  ,
     +                  ngrid   ,   ntabm   ,   maxtab  ,   nslat  ,
     +                  ntmpgr  ,   itim    ,   juer    ,   g      ,
     +                  rhow    ,   time    ,   tp      ,   el     ,
     +                  newres  ,
     +                ip(dispf) ,ip(mouth)  ,ip(branch) ,ip(bramrl),
     +                ip(ntab)  ,ip(fd_nefis_rst) ,ip(fd_nefis_new),
     +                rp(table)  ,rp(sbdpar) ,rp(sltpar),
     +                rp(moupar),rp(x)      ,rp(disgr)  ,rp(csa)   ,
     +                rp(csd)   ,rp(cpack)  ,dp(qpack)  ,rp(waoft) ,
     +                rp(cdcdx) ,rp(tmpgr)  ,rp(thasca) ,rp(rho  ) ,
     +                rp(mouqpu),rp(tw    ) ,rp(thcsum) ,rp(timout),
     +                rp(sbdscr),ip(ncelsa) ,ip(grid  ) ,   ker    ,
     +                ip(salini)          )
      endif
c
c     Initialise sediment module
c
      if ((lsedt) .and. (ker .ne. fatal)) then
c
c        Read pointers of variables
c
         branch  =    gtipnt ('BRANCH')
         celer   =    gtrpnt ('CELER' )
         dissed  =    gtrpnt ('DISSED')
         forcon  =    gtrpnt ('FORCON')
         grsize  =    gtrpnt ('GRSIZE')
         nbran   = ip(gtipnt ('NBRAN' ))
         ngrid   = ip(gtipnt ('NGRID' ))
         nsedrd  = ip(gtipnt ('NSEDRD'))
         nucoef  = ip(gtipnt ('NUCOEF'))
         sedinf  =    gtipnt ('SEDINF')
         sedini  =    gtipnt ('SEDINI')
         sedpar  =    gtrpnt ('SEDPAR')
         sedtr   =    gtrpnt ('SEDTR' )
         trform  =    gtrpnt ('TRFORM')
         uscoef  =    gtrpnt ('USCOEF')
c
c        Extract parameters from flwpar
c
         ixpar = gtrpnt ( 'FLWPAR' )
c
         g    = sorpar ( rp(ixpar), 1 )
c
         call seini (    nbran   ,   ngrid   ,   nsedrd  ,   g       ,
     +                rp(sedpar) ,ip(branch) ,ip(sedinf) ,   nucoef  ,
     +                rp(uscoef) ,rp(trform) ,rp(grsize) ,rp(forcon) ,
     +                rp(sedtr ) ,rp(celer ) ,rp(dissed) ,ip(sedini) ,
     +                   juer    ,   ker     )
       endif


      if ((lgrad) .and. (ker .ne. fatal)) then
c
c        Read pointers of variables
c
         afwfqs =     gtrpnt ( 'AFWFQS')
         branch =     gtipnt ( 'BRANCH')
         cpack  =     gtrpnt ( 'CPACK' )
         bfrict =     gtipnt ( 'BFRICT')
         deff   =     gtrpnt ( 'DEFF'  )
         deltar =     gtrpnt ( 'DELTAR')
         depos  =     gtlpnt ( 'DEPOS' )
         ddis   =     gtrpnt ( 'DDIS'  )
         dfrac  =     gtrpnt ( 'DFRAC' )
         dmed0  =     gtrpnt ( 'DMED0' )
         duncon =     gtrpnt ( 'DUNCON')
         duneh  =     gtrpnt ( 'DUNEH' )
         dunel  =     gtrpnt ( 'DUNEL' )
         dzr    =     gtrpnt ( 'DZR'   )
         disgse =     gtrpnt ( 'DISSED')
         forcon =     gtrpnt ( 'FORCON')
         grsize =     gtrpnt ( 'GRSIZE')
         gsopts =     gtipnt ( 'GSOPTS')
         hlev   =     gtdpnt ( 'HLEV'  )
         lanrinbt =   gtipnt ( 'LANRINBT')        
         levunl =     gtrpnt ( 'LEVUNL')
         maxlev = ip (gtipnt ( 'MAXLEV'))
         nbran  = ip (gtipnt ( 'NBRAN' ))
         ncelse =     gtipnt ( 'NCELSE')
         nellvl =     gtrpnt ( 'NELLVL')         
         nfrac  = ip (gtipnt ( 'NFRAC' ))
         ngrid  = ip (gtipnt ( 'NGRID' ))
         nlayer = ip (gtipnt ( 'NLAYER'))
         nlev   =     gtipnt ( 'NLEV'  )
         nnelvl = ip( gtipnt ( 'NNELVL'))         
         nonall =     gtipnt ( 'NONALL') 
         nrdzdl =     gtipnt ( 'NRDZDL')
         nucoef = ip (gtipnt ( 'NUCOEF'))
         nunlay = ip (gtipnt ( 'NUNLAY'))
         p0la   =     gtrpnt ( 'P0LA'  )
         pexla  =     gtrpnt ( 'PEXLA' )
         ptrla  =     gtrpnt ( 'PTRLA' )
         sedexp =     gtrpnt ( 'SEDEXP')
         sedini  =    gtipnt ( 'SEDINI')
         sedpar =     gtrpnt ( 'SEDPAR')
         tmpfr  =     gtrpnt ( 'TMPFR' )
         trform =     gtrpnt ( 'TRFORM')
         uscoef =     gtrpnt ( 'USCOEF')
         ws     =     gtrpnt ( 'WS'    )
         wft    =     gtrpnt ( 'WFT'   )
         zbave  =     gtrpnt ( 'ZBAVE' )
         zbfl   =     gtrpnt ( 'ZBFL'  )
c
c        Extract parameters from flwpar
c
         ixpar = gtrpnt ( 'FLWPAR' )
c
         g    = sorpar ( rp(ixpar), 1 )
c
         call gsini (  nbran  ,   ngrid  ,   nfrac  ,  nlayer  ,
     &                 g      ,   nunlay ,   maxlev ,  jugraut ,
     &                 jugralg,   itim   ,   newres ,ip(ncelse),
     &              ip(gsopts),rp(sedpar),rp(tmpfr ),rp(ddis)  ,
     &              rp(dfrac) ,rp(grsize),rp(dmed0) ,   nucoef ,
     &              rp(uscoef),rp(trform),rp(forcon),rp(duncon),
     &              rp(disgse),rp(p0la)  ,rp(ptrla ),rp(pexla ),
     &              lp(depos) ,rp(deff)  ,rp(duneh ),rp(dunel ),
     &              rp(levunl),dp(hlev)  ,ip(nrdzdl),rp(dzr)   ,
     &              rp(cpack) ,rp(afwfqs),ip(nlev)  ,rp(ws)    ,
     &              rp(wft)   ,ip(branch),rp(sedexp),rp(deltar),
     &              rp(zbave) ,rp(zbfl)  ,ip(nonall),   nnelvl ,
     &              rp(nellvl),ip(sedini),ip(lanrinbt)         ,
     &              ip(fd_nefis_rst),ip(fd_nefis_new),
     &              ip(bfrict),   juer   ,   ker    ) 
c
      endif
c
c     Create names for definition and data file (RESULT)
c     Only if at least flow module will be used
c
      ixpar  = gtrpnt ( 'FLWPAR' )
      nefhis = soipar ( rp(ixpar), 22)
c
      if ((lflow) .and. nefhis.ge.1 .and. (ker .ne. fatal)) then
c
        ext = '.odf'
        call socnam ( filnam, defnam, ext )
        ext = '.oda'
        call socnam ( filnam, datnam, ext )
c
c       Find pointers of descriptors
c
        fd_nefis_res = gtipnt ('FD_NEFIS_RES')
c
c        Open the result files
c
#if defined (SHR_MEM)
c ====  shared memory for SRS-BOS ====
        defnam = "SODF"
        datnam = "SODA"
        shmcod = "NSC"
c       shmcod = coding
        call soofil (  shmcod  ,
     +                 ip(fd_nefis_res) ,
     +                 defnam  ,   datnam  ,
     +                 juer    ,   ker     )
#else
        ext = '.odf'
        call socnam ( filnam, defnam, ext )
        ext = '.oda'
        call socnam ( filnam, datnam, ext )
c
c       Open definition and data file
c       of output file (an old file will be deleted)
c
        lutemp = 31
        open (lutemp,file=datnam)
        close (lutemp,status='delete')
        open (lutemp,file=defnam)
        close (lutemp,status='delete')

        call soofil (  coding  ,
     +                 ip(fd_nefis_res) ,
     +                 defnam  ,   datnam  ,
     +                 juer    ,   ker     )
#endif
c
c       Write sobek release number to file
c
        if (ker .ne. fatal) then
            call sowrel ( ip(fd_nefis_res), sbkrel,
     +                       juer   , ker       )
        endif

      endif
c
c     Create names for definition and data file (INTERFACE FILE)
c     Only if aggregation option will be used
c
      if ((lflow) .and. (lwqin) .and. (ker .ne. fatal)) then
c
c       Determine exchanges
c
        call SOWQIN ( juer   ,ker )
c
c        Find pointers of descriptors
c
        fd_nefis_waq = gtipnt ('FD_NEFIS_WAQ')
c
c       Open the water quality interface files
c       (an old file will be deleted)
c
        lutemp = 31
        open (lutemp,file=nefwda)
        close (lutemp,status='delete')
        open (lutemp,file=nefwdf)
        close (lutemp,status='delete')
c
        call soofil (  coding  ,
     +                 ip(fd_nefis_waq) ,
     +                 nefwdf  ,   nefwda  ,
     +                 juer    ,   ker     )
      endif

      return

      end
