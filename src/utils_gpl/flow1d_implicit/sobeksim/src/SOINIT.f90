subroutine SOINIT ( filnam ,itim  ,itp   ,dtf   ,lauto ,lflow  ,&
&lgrwt  ,&
&lkalm  ,lsalt ,lsedt ,lmorp ,lgrad ,lwqin  ,&
&lrest  ,newres,sbkrel,coding,juer  ,&
&juresi ,jufrou,juresd,justrd,ker   ,inocon ,&
&jusold ,lfrou ,itstat,lhisgp,frobuf,jugraut,&
&jugralg)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOINIT (SObek INITialise)
!
! Module description: Preceding the actual simulation, in subroutine
!                     SOINIT several auxiliary arrays will be computed
!                     to be used later on in the computational kernel.
!                     In case the auto start option has been selected a
!                     steady state computation will be exectuted on time
!                     t = 0.
!
!                     If the morphological module is included in the
!                     application first the initialisation routine of
!                     the morphological sub system will be called. If a
!                     restart file is available this will result in
!                     reading the adapted cross sections. The next step
!                     is calling CSTABL. This routine will insert preis-
!                     smann slots and calculates the auxiliary tables
!                     for areas and wetted perimeters. After this the
!                     initialisation routines of the flow module will be
!                     called. If the auto start option is selected a
!                     steady state computation will be executed by cal-
!                     ling SONOMO. After this the initialisation routi-
!                     nes of the salt and sediment module will be cal-
!                     led.
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 15 coding            P  -
!  4 dtf               I  Time step flow module
!  1 filnam            P  -
! 22 inocon            O  Number of timesteps continuated without
!                         convergence
!  2 itim              P  -
!  3 itp               I  Tidal period in whole numbers of flow step
! 25 itstat            P  -
! 16 juer              P  -
! 18 jufrou            P  -
! 17 juresi            P  -
! 23 jusold            P  -
! 19 justru            P  -
! 21 ker               I  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  5 lauto             I  Switch to execute autostart procedure
!  6 lflow             I  Switch to enable flow module
! 24 lfrou             O  Flag if Froude numbers during simulation are
!                         high
!  7 lkalm             I  -
! 10 lmorp             I  Logical indicator for morphology computation
!                         = .true.  : with morphology computation
!                         = .false. : without morphology computation
! 12 lrest             O  Switch to indicate that restart file has been
!                         read
!  8 lsalt             I  Logical indicator for salt computation
!                         = .true.  : with salt computation
!                         = .false. : no salt computation
!  9 lsedt             I  Switch to enable sediment transport module
! 11 lwqin             I  Logical, = TRUE indicates the  water quality
!                         interface file must be written.
! 13 newres            I  true, if a new restart file will be made
! 14 sbkrel            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! cstabl  Cross Sectional TABLe module
! flini   FLow INItialise
! gtipnt  GeT Integer PoiNTer
! gtlpnt  GeT Logical PoiNTer
! gtrpnt  GeT Real PoiNTer
! kaini   KAlman INItialisation
! moini   MOrphology INItialise
! saini   SAlt INItialise
! seini   SEdiment INItialise
! soadna  SObek ADd NAmes of "lateral" structures
! socnam  SObek Create NAMe
! sonomo  SObek NO MOrphology
! soofil  SObek Open FILe
! sorpar  SObek Real PARameter
! sowrel  SObek Write RELease number
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: soinit.pf,v $
! Revision 1.34  1999/07/23  15:10:37  kuipe_j
! improve restart
!
! Revision 1.33  1999/06/01  13:42:49  kuipe_j
! names in messages substituted + message template
!
! Revision 1.32  1999/04/22  08:45:19  kuipe_j
! remove initial ODA/ODF and WDA/WDF files
!
! Revision 1.31  1999/03/15  15:03:31  kuipe_j
! Improve Froude file and Dumpfiles
!
! Revision 1.30  1998/12/11  10:56:29  kuipe_j
! Throw existing NDA/NDF file away
!
! Revision 1.28  1998/06/11  11:47:43  kuipe_j
! Estuary special integrated
!
! Revision 1.27  1998/06/08  13:15:38  kuipe_j
! time lag hydr controller
!
! Revision 1.26  1998/02/25  12:49:06  kuipe_j
! Check on grain size added
!
! Revision 1.25  1998/02/13  13:22:35  kuipe_j
! Adapt to CMT ; SRS BOS
!
! Revision 1.24  1997/11/04  14:19:59  kuipe_j
! Retention basin
!
! Revision 1.23  1997/06/17  11:29:24  kuipe_j
! output in history format
!
! Revision 1.22  1997/06/04  11:20:17  kuipe_j
! Initialize arrays
!
! Revision 1.21  1997/05/26  07:37:01  kuipe_j
! statistic of iteration improved
!
! Revision 1.20  1997/02/17  10:09:39  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.19  1997/01/23  08:30:12  kuipe_j
! Make flow module robust
!
! Revision 1.18  1996/11/01  11:25:44  kuipe_j
! Update of Delwaq input file added
!
! Revision 1.17  1996/10/31  13:03:49  kuipe_j
! Extra resistance finished, Exchanges are calculated
!
! Revision 1.16  1996/09/03  14:33:43  kuipe_j
! frequency time hist, run in time est. morp
!
! Revision 1.15  1996/04/12  13:06:06  kuipe_j
! headers, minor changes
!
! Revision 1.14  1996/04/11  08:16:33  kuipe_j
! Kalman module added
!
! Revision 1.13  1996/03/07  10:44:30  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.12  1996/02/09  15:13:37  kuipe_j
! a.o. Restart improvements
!
! Revision 1.11  1996/01/17  14:47:38  kuipe_j
! header update
!
! Revision 1.10  1996/01/16  15:01:55  kuipe_j
! Restart improvements
!
! Revision 1.9  1995/10/18  09:01:04  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.8  1995/09/29  10:36:39  kuipe_j
! Improvement of autostart and simple weir
!
! Revision 1.7  1995/09/22  10:04:19  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:11:31  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:37:34  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:51  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:56:59  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:53  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:18  hoeks_a
! Initial check-in
!

! Revision 1.4  1994/12/02  13:33:29  kuipe_j
! Improvement of message handling.
!
! Revision 1.3  1994/11/28  08:28:39  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:09:59  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:43  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   use flow_in_datools
!
!     Parameters
!
   character*(*) filnam
   character*1   coding
#if defined (SHR_MEM)
! ====  shared memory  ====
   character*3   shmcod
#endif
   logical       lauto  ,lkalm  ,lflow  ,lsalt  ,lsedt  ,lmorp ,&
   &lgrad , lwqin  ,lrest  ,newres ,lfrou  ,lhisgp,&
   &lgrwt
   integer       juer   ,ker    ,itp    ,itim(2),sbkrel(3),&
   &juresi ,jufrou ,juresd ,justrd ,inocon   ,&
   &jusold ,itstat(4)      ,nefhis ,jugraut  ,jugralg
   double  precision     dtf
   real          frobuf (8)
!
!     Variables (pointers to arrays or single values from pool)
!
   integer  aft   ,af2   ,att   ,afwfqs,alfab ,arex  ,arexcn,arexop,&
   &bfricp,bfrict,bramrl,branch,brnode,buflag,cdcdx ,celer ,&
   &conhis,cnstrl,contrl,cpack ,csa   ,csd   ,deff  ,deltar,&
   &depos ,ddis  ,dfrac ,dmed0 ,duncon,duneh ,dunel ,dzr   ,&
   &disgse,delh  ,disgr ,dispf ,dissed,dsopt ,engpar,exres ,&
   &forcon,grid  ,grsize,gsopts,hbdpar,hlev  ,hlev0 ,hpack ,&
   &hstat,izwft,lanrinbt,lagstm,levunl,lfilt ,maxlev,maxtab,&
   &mltpar,moupar,mouqpu,mouth ,nboun ,nbran ,nbrnod,ncelfl,&
   &ncelmo,nclrst,ncelsa,ncelse,ncontr,ncsrel,nexres,nfrac ,&
   &ngrid ,nhstat,nlags ,nlayer,nlev  ,nmlat ,nmouth,nnc   ,&
   &nellvl,nnelvl,nnf   ,nnm   ,nnmu  ,nnn   ,nnode ,nns   ,&
   &np    ,nodnod,nonall,nqlat ,nqstat,nrdzdl,nsamp ,nosdim

   integer  nsedrd,nslat ,nstru ,ntab  ,ntabm ,ntmpgr,nucoef,numnod,&
   &nunlay,of    ,pfa   ,pmua  ,pw    ,prslot,psltvr,pexla ,&
   &ptrla ,p0la  ,p1    ,p2    ,qaggr ,qbdpar,&
   &qlaggr,qlat  ,qlatgr,qltpar,qpack ,qstat ,res   ,rescov,&
   &rpack ,rho   ,sbdpar,sbdscr,scares,scceq ,scmeq ,scqhs ,&
   &scfric,scmu  ,scifri,scimu ,sclceq,sclmeq,sclqhs,sclnod,&
   &sclfri,sclmu ,scnode,sectc ,sectv ,sedexp,sedtr ,sedinf,&
   &sedpar,sltpar,smpns ,snceq ,snmeq ,snqhs ,snnode,snfric,&
   &snmu  ,snwind,strclo,strhis,strpar,strtyp,sumda ,table ,&
   &thasca,thcsum,timout,tmpfr ,tmpgr ,trform,typcr ,tw    ,&
   &uscoef,waoft ,wft   ,wf2,ws,wtt   ,x     ,ibuf  ,resbuf,&
   &strbuf,solbuf,gridnm,strunm,qlatnm,morini,corrnm,zbave ,&
   &zbfl  ,flwini,salini,sedini,kalini,&
   &grhis ,strtim
!
!     Local variables
!
   integer         ixpar ,istep ,cpredn ,filstp, lutemp
   real            g     ,rhow  ,el
   real            flitmx,overlp,lambda ,theta, dhstru

   double  precision      time ,tp
   logical         inires,steady,tmpsal,tmpsed,tmpmor,tmpriv,tmpkal
!     mozart dummy
   logical         lmozad
   integer         nstepd
!
!     File variables
!
   character*256   defnam ,datnam
   character*4     ext
   integer         fd_nefis_rst, fd_nefis_new , fd_nefis_res ,&
   &fd_nefis_waq
!
!     External functions
!
   integer         gtrpnt,gtipnt,gtlpnt,gtdpnt,gtcpnt,soipar
   real            sorpar
   external        gtrpnt,gtipnt,gtlpnt,gtdpnt,gtcpnt,soipar,sorpar
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
   include '..\include\filsim.i'
!
   if ( da_running_in_da_tools() ) then
!        check timing inconsistency
      if ( .not. da_check_model_start_time(itim, juer) ) then
         ker = fatal
         return
      endif
   endif
!
!     Set initial value of number of timesteps without convergence
!     to zero
!

   inocon = 0
!
!     Set Froude flag to false
!
   lfrou = .false.
!
!     Only open restart file in case at least flow module is included
!
   if (lflow) then
!
!       Find pointers of descriptors
!
      fd_nefis_rst = gtipnt ('FD_NEFIS_RST')
!
!       Open the restart files
!
      call soofil(coding, ip(fd_nefis_rst), nefrdf, nefrda, juer, ker)
!
!       Find pointers of descriptors of new restart file
!
      fd_nefis_new = gtipnt ('FD_NEFIS_NEW')

      if (newres) then
!
!          Open definition and data file
!          of a new restart file (the old file is still
!          present).
!
         lutemp = 31
         open (lutemp,file=nefnda)
         close (lutemp,status='delete')
         open (lutemp,file=nefndf)
         close (lutemp,status='delete')

         call soofil(coding, ip(fd_nefis_new), nefndf, nefnda,&
         &juer    ,   ker     )
!
      endif
   endif
!
!     Morphology module, should be the first one to read adapted CS
!
   if ((lmorp.or.lgrad) .and. (ker .ne. fatal)) then
!
!        Read pointers of variables
!
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

      call moini (   ngrid   ,   nbran   ,   nmlat   ,   maxlev  ,&
      &itim    ,ip(branch) ,rp(mltpar) ,dp(hlev)   ,&
      &rp(hlev0)  ,ip(grid  ) ,   newres   ,&
      &ip(fd_nefis_rst) ,ip(fd_nefis_new) ,   juer    ,&
      &ip(ncelmo) ,   ker     ,ip(morini) ,rp(sumda)  ,&
      &lgrad   )
   endif
!
!     Call CSTABL to calculate all tables
!
   if ((lflow) .and. (ker .ne. fatal)) then
!
!        Read pointers of variables
!
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
!        In case no sediment module is present ws points to scratch
!        array
      if (lsedt) then
         ws      =    gtrpnt ('WS'    )
      else
         ws      =    gtrpnt ('TMPGR' )
      endif
!

      call cstabl (   ngrid   ,   maxlev ,ip(nlev)  ,dp(hlev)  ,&
      &rp(wft)   ,rp(aft)   ,rp(wtt)   ,rp(att)   ,&
      &rp(of)    ,   lsalt  ,rp(izwft) ,   nbran  ,&
      &ip(branch),ip(typcr) ,rp(sectc) ,rp(sectv) ,&
      &rp(prslot),rp(psltvr),ip(bfrict),rp(bfricp),&
      &rp(engpar),rp(grsize),   maxtab ,   ntabm  ,&
      &ip(ntab)  ,rp(table) ,rp(ws)    ,&
      &lsedt.or.lgrad    ,   juer   ,   ker    )
   endif
!
!     Initialise flow module
!
   if ((lflow) .and. (ker .ne. fatal)) then
!
!        Read pointers of variables
!
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
!
!        Extract parameters from flwpar
!
      ixpar = gtrpnt ( 'FLWPAR' )
!
      g      = sorpar ( rp(ixpar), 1 )
      rhow   = sorpar ( rp(ixpar), 6 )
      lambda = sorpar ( rp(ixpar), 10)
      overlp = sorpar ( rp(ixpar), 16)
      dhstru = sorpar ( rp(ixpar), 12)
!
!        Determine size and declare time lag bugger
!
      call SOTLAG (ncontr ,rp(contrl),dtf   ,juer  ,ker  )
      if (ker.ne.fatal) then
!
         lagstm  =ip(gtipnt ('LAGSTM'))
         nlags   =ip(gtipnt ('NLAGS' ))
         buflag  =   gtrpnt ('BUFLAG')

!c
!        Adjust structure names in case of retention basins
!

         call soadna( ip(strtim), nstru, cp(strunm), cp(qlatnm),&
         &rp(qltpar), rp(strhis) )

         call flini(lkalm  ,lp(lfilt) ,   lwqin  ,   lauto  ,&
         &lgrwt,  g   ,rhow ,&
         &nqlat  ,   ncontr ,   nstru  ,   ngrid  ,   itim   ,   juer   ,&
         &newres ,   lambda ,   dtf    ,   dhstru ,   lagstm ,   nlags  ,&
         &ip(fd_nefis_rst),ip(fd_nefis_new),dp(hpack),dp(qpack),&
         &rp(conhis),rp(strpar),ip(strtyp),rp(contrl),lp(strclo),rp(x)     ,&
         &rp(qaggr) ,rp(qlaggr),ip(ncelfl),   inires ,   nbran  ,ip(branch),&
         &nnode  ,   nbrnod ,ip(brnode),ip(nodnod),ip(numnod),ip(typcr) ,&
         &rp(prslot),rp(psltvr),ip(bfrict),rp(bfricp),rp(grsize),rp(engpar),&
         &rp(qltpar),rp(qlat)  ,rp(qlatgr),rp(cpack) ,rp(rpack) ,   maxlev ,&
         &ip(nlev)  ,dp(hlev)  ,rp(wft)   ,rp(aft)   ,rp(wtt)   ,rp(att)   ,&
         &overlp ,rp(arex)  ,ip(arexcn),ip(arexop),rp(waoft) ,rp(afwfqs),&
         &rp(sectc) ,rp(sectv) ,rp(of)    ,  maxtab  ,   ntabm  ,ip(ntab)  ,&
         &rp(table) , nhstat   ,rp(hstat) ,ip(hbdpar),   nqstat ,rp(qstat) ,&
         &ip(qbdpar),rp(alfab) ,rp(pfa)   ,rp(pmua)  ,rp(pw)    ,ip(scifri),&
         &ip(scimu) ,ip(sclceq),ip(sclmeq),ip(sclqhs),ip(sclnod),ip(scceq) ,&
         &ip(scmeq) ,ip(scqhs) ,ip(scnode),&
         &rp(rho)   ,  ncsrel  ,rp(strhis),ip(cnstrl),ip(grid)  ,   nexres ,&
         &rp(exres) ,  ker     ,ip(ibuf)  ,rp(resbuf),rp(strbuf),&
         &rp(solbuf),  itstat  ,ip(flwini),   lhisgp ,cp(gridnm),cp(strunm),&
         &cp(qlatnm),dp(delh)  ,rp(buflag),rp(grhis) )

         call soadna( ip(strtim), nstru, cp(strunm), cp(qlatnm),&
         &rp(qltpar), rp(strhis) )
!
!        If inires = false a restart block was found
!
         lrest = .not. inires
      else
         lrest = .false.
      endif

      if (lauto) then
!
!        Autostart: save flwrun parameters and change itermax and
!
         ixpar = gtrpnt ( 'FLWPAR' )
!
         theta  = sorpar ( rp(ixpar), 3 )
         flitmx = sorpar ( rp(ixpar), 7 )
!
!        Change numerical parameters
!
         rp(ixpar+2) = 1.0
         rp(ixpar+6) = 999.
!
!        Set run parameters to steady and only flow module
!
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
!        mozart dummy
         lmozad = .false.
         nstepd = 0
!
         call sonomo ( istep , time  , itim  , dtf  , filstp ,cpredn,&
         &steady, itp   , tmpkal,tmpsal, tmpsed, tmpmor,&
!                      mozart and groundwater dummy
         &lmozad,lgrwt  ,lrest  ,nstepd ,&
         &tmpriv, juer  , juresi,jufrou ,juresd ,justrd,&
         &ker   , inocon, jusold, lfrou ,itstat ,frobuf)

!
!        Restore original flow run parameters
!
         rp(ixpar+2) = theta
         rp(ixpar+6) = flitmx
!
      endif
!
!     Initialize time lag buffer
!
      if (.not. lrest)&
      &call FLTLAG(2   ,ncontr ,rp(contrl) ,lagstm ,nlags, rp(buflag),&
      &dtf ,ngrid  ,dp(qpack+2*ngrid)  )
   endif
!
!     Initialise Kalman module
!
   if ((lkalm) .and. (ker .ne. fatal)) then
!
!        Fetch pointers of Kalman arrays
!
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
!
      call KAINI (ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnn    ,&
      &nnf    ,nnmu   ,np     ,nsamp  ,itim   ,juer   ,&
      &ip(nclrst) ,inires     ,newres ,nosdim ,&
      &rp(af2)    ,rp(wf2)    ,&
      &ip(scifri) ,ip(scimu ) ,rp(snceq)  ,rp(snmeq)  ,&
      &rp(snqhs)  ,rp(snnode) ,rp(snfric) ,rp(snmu)   ,&
      &rp(snwind) ,rp(smpns)  ,ip(sclceq) ,ip(sclmeq) ,&
      &ip(sclqhs) ,ip(sclnod) ,ip(sclfri) ,ip(sclmu)  ,&
      &ip(scfric) ,ip(scmu)   ,&
      &ip(fd_nefis_rst) ,ip(fd_nefis_new) ,&
      &rp(p1)     ,rp(p2)     ,rp(pfa)    ,&
      &rp(pmua)   ,rp(pw)     ,rp(res)    ,rp(scares) ,&
      &rp(rescov) ,dp(hpack)  ,dp(qpack)  ,lp(lfilt)  ,&
      &cp(corrnm) ,ip(kalini) ,ker        )
   endif
!
!     Initialise salt module
!
   if ((lsalt) .and. (ker .ne. fatal)) then
!
!        Read pointers of variables
!
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
!
!        Extract parameters from flwpar
!
      ixpar = gtrpnt ( 'FLWPAR' )
!
      g    = sorpar ( rp(ixpar), 1 )
      rhow = sorpar ( rp(ixpar), 6 )
!
!        Extract parameters from salpar
!
      ixpar = gtrpnt ( 'SALPAR' )
!
      el   = sorpar ( rp(ixpar), 1 )
      tp   = real(itp) * dtf
      time = 0D0
!
      call saini (   dsopt   ,   nmouth  ,   nboun   ,   nbran  ,&
      &ngrid   ,   ntabm   ,   maxtab  ,   nslat  ,&
      &ntmpgr  ,   itim    ,   juer    ,   g      ,&
      &rhow    ,   time    ,   tp      ,   el     ,&
      &newres  ,&
      &ip(dispf) ,ip(mouth)  ,ip(branch) ,ip(bramrl),&
      &ip(ntab)  ,ip(fd_nefis_rst) ,ip(fd_nefis_new),&
      &rp(table)  ,rp(sbdpar) ,rp(sltpar),&
      &rp(moupar),rp(x)      ,rp(disgr)  ,rp(csa)   ,&
      &rp(csd)   ,rp(cpack)  ,dp(qpack)  ,rp(waoft) ,&
      &rp(cdcdx) ,rp(tmpgr)  ,rp(thasca) ,rp(rho  ) ,&
      &rp(mouqpu),rp(tw    ) ,rp(thcsum) ,rp(timout),&
      &rp(sbdscr),ip(ncelsa) ,ip(grid  ) ,   ker    ,&
      &ip(salini)          )
   endif
!
!     Initialise sediment module
!
   if ((lsedt) .and. (ker .ne. fatal)) then
!
!        Read pointers of variables
!
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
!
!        Extract parameters from flwpar
!
      ixpar = gtrpnt ( 'FLWPAR' )
!
      g    = sorpar ( rp(ixpar), 1 )
!
      call seini (    nbran   ,   ngrid   ,   nsedrd  ,   g       ,&
      &rp(sedpar) ,ip(branch) ,ip(sedinf) ,   nucoef  ,&
      &rp(uscoef) ,rp(trform) ,rp(grsize) ,rp(forcon) ,&
      &rp(sedtr ) ,rp(celer ) ,rp(dissed) ,ip(sedini) ,&
      &juer    ,   ker     )
   endif


   if ((lgrad) .and. (ker .ne. fatal)) then
!
!        Read pointers of variables
!
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
!
!        Extract parameters from flwpar
!
      ixpar = gtrpnt ( 'FLWPAR' )
!
      g    = sorpar ( rp(ixpar), 1 )
!
      call gsini (  nbran  ,   ngrid  ,   nfrac  ,  nlayer  ,&
      &g      ,   nunlay ,   maxlev ,  jugraut ,&
      &jugralg,   itim   ,   newres ,ip(ncelse),&
      &ip(gsopts),rp(sedpar),rp(tmpfr ),rp(ddis)  ,&
      &rp(dfrac) ,rp(grsize),rp(dmed0) ,   nucoef ,&
      &rp(uscoef),rp(trform),rp(forcon),rp(duncon),&
      &rp(disgse),rp(p0la)  ,rp(ptrla ),rp(pexla ),&
      &lp(depos) ,rp(deff)  ,rp(duneh ),rp(dunel ),&
      &rp(levunl),dp(hlev)  ,ip(nrdzdl),rp(dzr)   ,&
      &rp(cpack) ,rp(afwfqs),ip(nlev)  ,rp(ws)    ,&
      &rp(wft)   ,ip(branch),rp(sedexp),rp(deltar),&
      &rp(zbave) ,rp(zbfl)  ,ip(nonall),   nnelvl ,&
      &rp(nellvl),ip(sedini),ip(lanrinbt)         ,&
      &ip(fd_nefis_rst),ip(fd_nefis_new),&
      &ip(bfrict),   juer   ,   ker    )
!
   endif
!
!     Create names for definition and data file (RESULT)
!     Only if at least flow module will be used
!
   ixpar  = gtrpnt ( 'FLWPAR' )
   nefhis = soipar ( rp(ixpar), 22)
!
   if ((lflow) .and. nefhis.ge.1 .and. (ker .ne. fatal)) then
!
      ext = '.odf'
      call socnam ( filnam, defnam, ext )
      ext = '.oda'
      call socnam ( filnam, datnam, ext )
!
!       Find pointers of descriptors
!
      fd_nefis_res = gtipnt ('FD_NEFIS_RES')
!
!        Open the result files
!
#if defined (SHR_MEM)
! ====  shared memory for SRS-BOS ====
      defnam = "SODF"
      datnam = "SODA"
      shmcod = "NSC"
!       shmcod = coding
      call soofil (  shmcod  ,&
      &ip(fd_nefis_res) ,&
      &defnam  ,   datnam  ,&
      &juer    ,   ker     )
#else
      ext = '.odf'
      call socnam ( filnam, defnam, ext )
      ext = '.oda'
      call socnam ( filnam, datnam, ext )
!
!       Open definition and data file
!       of output file (an old file will be deleted)
!
      lutemp = 31
      open (lutemp,file=datnam)
      close (lutemp,status='delete')
      open (lutemp,file=defnam)
      close (lutemp,status='delete')

      call soofil (  coding  ,&
      &ip(fd_nefis_res) ,&
      &defnam  ,   datnam  ,&
      &juer    ,   ker     )
#endif
!
!       Write sobek release number to file
!
      if (ker .ne. fatal) then
         call sowrel ( ip(fd_nefis_res), sbkrel,&
         &juer   , ker       )
      endif

   endif
!
!     Create names for definition and data file (INTERFACE FILE)
!     Only if aggregation option will be used
!
   if ((lflow) .and. (lwqin) .and. (ker .ne. fatal)) then
!
!       Determine exchanges
!
      call SOWQIN ( juer   ,ker )
!
!        Find pointers of descriptors
!
      fd_nefis_waq = gtipnt ('FD_NEFIS_WAQ')
!
!       Open the water quality interface files
!       (an old file will be deleted)
!
      lutemp = 31
      open (lutemp,file=nefwda)
      close (lutemp,status='delete')
      open (lutemp,file=nefwdf)
      close (lutemp,status='delete')
!
      call soofil (  coding  ,&
      &ip(fd_nefis_waq) ,&
      &nefwdf  ,   nefwda  ,&
      &juer    ,   ker     )
   endif

   return

end
