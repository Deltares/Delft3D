subroutine SOFLOW ( istep  ,time   ,itim   ,dtf    ,filstp ,&
&cpredn ,steady ,lsalt  ,lkalm  ,&
!                         mozart parameters
&lmoza  ,lgrwt  ,lrest  ,nstep  ,&
&juresi ,jufrou ,juresd ,justrd ,juer   ,ker  ,&
&inocon ,jusold ,lfrou  ,itstat ,frobuf )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOFLOW (SObek FLOW main routine)
!
! Module description: Subroutine SOFLOW computes the waterlevels and
!                     discharges for the next time level. If the Kalman
!                     filter module is active also the uncertain model
!                     parameters and covariances will be calculated.
!
!                     First the flow module is iterated until convergence
!                     is reached. After convergence is reached, routine
!                     KALMAN performs the filter step if requested.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 cpredn            P  -
!  4 dtf               P  -
!  5 filstp            P  -
! 16 inocon            P  -
!  1 istep             I  Current time step number (t(n+1)).
!  3 itim              P  -
! 19 itstat            P  -
! 14 juer              P  -
! 11 jufrou            I  Unit number of file froude
! 10 juresi            P  -
! 17 jusold            P  -
! 12 justru            P  -
! 15 ker               IO Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 18 lfrou             P  -
!  9 lkalm             I  -
!  8 lsalt             P  -
!  7 steady            P  -
!  2 time              P  -
!    conv                 Switch to indicate convergented solution
!                         = 0 nog niet geconvergeerd
!                         = 1 geconvergeerd
!                         = 2 niet geconvergeerd, alle iteraties
!                             verbruikt en doorgaan
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flnp1   FLow results on time N + 1
! flow    FLOW module
! gtdpnt  GeT Double PoiNTer
! gtipnt  GeT Integer PoiNTer
! gtlpnt  GeT Logical PoiNTer
! gtrpnt  GeT Real PoiNTer
! kalman  KALman main routine
! soconv  SObek CONVergence
! soipar  SObek Integer PARameter
! sorpar  SObek Real PARameter
! sowrbf  SObel WRite BuFfer
!=======================================================================
!
!
!
!**********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: soflow.pf,v $
! Revision 1.11  1999/03/15  15:03:29  kuipe_j
! Improve Froude file and Dumpfiles
!
! Revision 1.10  1998/06/24  11:10:34  kuipe_j
! Try direct solver if BICGST fails
!
! Revision 1.9  1998/06/11  11:47:41  kuipe_j
! Estuary special integrated
!
! Revision 1.8  1998/06/08  13:15:36  kuipe_j
! time lag hydr controller
!
! Revision 1.7  1998/04/10  09:22:23  kuipe_j
! total area recalculated
!
! Revision 1.6  1997/05/26  07:37:00  kuipe_j
! statistic of iteration improved
!
! Revision 1.5  1997/01/23  08:30:11  kuipe_j
! Make flow module robust
!
! Revision 1.4  1996/12/02  10:03:48  kuipe_j
! avoid negative pointers
!
! Revision 1.3  1996/09/03  14:33:42  kuipe_j
! frequency time hist, run in time est. morp
!
! Revision 1.2  1996/04/12  13:06:05  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:16:31  kuipe_j
! Kalman module added
!
!
!**********************************************************************
!
!     Parameters
!
   integer  itim(2),istep  ,filstp, cpredn ,&
   &juresi ,jufrou ,juresd ,justrd ,&
   &juer   ,ker    ,inocon ,jusold ,&
   &itstat(4)
   logical  lsalt  ,lkalm  ,steady ,lfrou  , lrest,&
!     mozart declaration plus groundwater switch
   &lmoza, lgrwt
   integer  nstep
   double   precision       time   ,dtf
   real     frobuf(8)
!
!     Local variables (pointers to arrays)
!
   integer a1m   ,abcd1 ,abcd2 ,af2   ,aft   ,afwfqs,alfab ,&
   &arex  ,arexcn,arexop,att   ,bfricp,bfrict,branch,&
   &brnode,buflag,cnpflg,cnstrl,conhis,contrl,cpack ,&
   &delh  ,deriva,&
   &engpar,exres ,flwpar,gangle,grid  ,grsize,hpack ,&
   &hbdpar,hlev  ,hstat ,indx  ,izwft ,kabcd1,kabcd2,&
   &kalpar,kbeta ,kgain ,ksi   ,lagstm,lfilt ,mat   ,&
   &maxlev,maxtab,nbran ,nbrnod,ncontr,ncsrel,nexres,&
   &ngrid ,ngridm,nhstat,nlev  ,nlags ,nnc   ,nnf   ,&
   &nnm   ,nnmu  ,nnn   ,nnode ,nns   ,node  ,nosdim,&
   &np    ,nqlat ,nqstat,nsamp ,nstdb ,nstru ,ntab  ,&
   &ntabm ,ntcrel,nodnod,ntrigr,&
   &ntsam ,numnod,of    ,pfa   ,pmua
   integer prslot,psltvr,pw    ,p1    ,p2    ,pcol  ,qpack ,&
   &qbdpar,qlat  ,qlatac,qlatgr,qltpar,qstat ,res   ,&
   &rescov,rpack ,rfv1  ,rfv2  ,rho   ,rhsm  ,rhsvv ,&
   &sample,scares,scceq ,scfric,scmeq ,scmu  ,scnode,&
   &scqhs ,scifri,scimu ,sclceq,sclfri,sclmeq,sclmu ,&
   &sclnod,sclqhs,sectc ,sectv ,smploc,smpns ,snceq ,&
   &snfric,snmeq ,snmu  ,snnode,snqhs ,snwind,stdbq ,&
   &strclo,strhis,strpar,strtyp,table ,&
   &tauwi ,trcnrl,triger,typcr ,waoft ,wfrict,wft   ,&
   &wf2   ,wndpar,work  ,wshld ,wtt   ,x     ,&
   &ibuf  ,resbuf,strbuf,solbuf, grhis
   integer h2    ,q2    ,&
!     mozart pointer, (Id's,names,storageWidth)
   &qlatid,qlatnm, gridnm, storWidth, nodenm
!
!     Single variables
!
   real    epsh ,epsq, overlp, epsqrl, qtyp, dhtyp
!
   integer flitmx, iter, conv, miniter
   logical lconv , bicg
!     mozart declaration
   integer istmoz, idmoz, istcnt
!
!     External functions
!
   integer  gtdpnt, gtipnt, gtlpnt, gtrpnt, gtcpnt, soipar
   real     sorpar
   logical  equal
   external gtdpnt, gtipnt, gtlpnt, gtrpnt, gtcpnt, soipar,&
   &sorpar, equal
!
!     Include memory pool
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
!
!     Extract parameters from flwpar
!
   flwpar = gtrpnt ( 'FLWPAR' )
!
   epsh   = sorpar ( rp(flwpar), 4 )
   epsq   = sorpar ( rp(flwpar), 5 )
   epsqrl = sorpar ( rp(flwpar), 9 )
   flitmx = soipar ( rp(flwpar), 7 )
   lconv  = soipar ( rp(flwpar), 17) .eq. 1
   overlp = sorpar ( rp(flwpar), 16)
   dhtyp  = sorpar ( rp(flwpar), 19)
!
!     Find starting addresses of working arrays
!
!     Single variables are read from the memory pool to simplify the
!     call for the Microsoft Fortran compiler v5
!
   a1m    =     gtrpnt ( 'A1M'   )
   abcd1  =     gtdpnt ( 'ABCD1' )
   abcd2  =     gtdpnt ( 'ABCD2' )
   aft    =     gtrpnt ( 'AFT'   )
   afwfqs =     gtrpnt ( 'AFWFQS')
   alfab  =     gtrpnt ( 'ALFAB' )
   arex   =     gtrpnt ( 'AREX'  )
   arexcn =     gtipnt ( 'AREXCN')
   arexop =     gtipnt ( 'AREXOP')
   att    =     gtrpnt ( 'ATT'   )
   bfricp =     gtrpnt ( 'BFRICP')
   bfrict =     gtipnt ( 'BFRICT')
   branch =     gtipnt ( 'BRANCH')
   brnode =     gtipnt ( 'BRNODE')
   buflag =     gtrpnt ( 'BUFLAG')
   cnpflg =     gtipnt ( 'CNPFLG')
   cnstrl =     gtipnt ( 'CNSTRL')
   conhis =     gtrpnt ( 'CONHIS')
   contrl =     gtrpnt ( 'CONTRL')
   cpack  =     gtrpnt ( 'CPACK' )
   delh   =     gtdpnt ( 'DELH'  )
   engpar =     gtrpnt ( 'ENGPAR')
   exres  =     gtrpnt ( 'EXRES' )
   gangle =     gtrpnt ( 'GANGLE')
   grid   =     gtipnt ( 'GRID'  )
   grsize =     gtrpnt ( 'GRSIZE')
   hpack  =     gtdpnt ( 'HPACK')
   hbdpar =     gtipnt ( 'HBDPAR')
   hlev   =     gtdpnt ( 'HLEV'  )
   hstat  =     gtrpnt ( 'HSTAT' )
   ibuf   =     gtipnt ( 'IBUF'  )
   indx   =     gtipnt ( 'INDX'  )
   izwft  =     gtrpnt ( 'IZWFT' )
   ksi    =     gtrpnt ( 'KSI'   )
   lagstm = ip (gtipnt ( 'LAGSTM'))
   mat    =     gtdpnt ( 'MAT'   )
   maxlev = ip (gtipnt ( 'MAXLEV'))
   maxtab = ip (gtipnt ( 'MAXTAB'))
   nbran  = ip (gtipnt ( 'NBRAN' ))
   nbrnod = ip (gtipnt ( 'NBRNOD'))
   ncontr = ip (gtipnt ( 'NCONTR'))
   ncsrel = ip (gtipnt ( 'NCSREL'))
   nexres = ip (gtipnt ( 'NEXRES'))
   ngrid  = ip (gtipnt ( 'NGRID' ))
   ngridm = ip (gtipnt ( 'NGRIDM'))
   nhstat = ip (gtipnt ( 'NHSTAT'))
   nlags  = ip (gtipnt ( 'NLAGS' ))
   nlev   =     gtipnt ( 'NLEV'  )
   nnc    = ip (gtipnt ( 'NNC'   ))
   nnf    = ip (gtipnt ( 'NNF'   ))
   nnm    = ip (gtipnt ( 'NNM'   ))
   nnmu   = ip (gtipnt ( 'NNMU'  ))
   nnn    = ip (gtipnt ( 'NNN'   ))
   nns    = ip (gtipnt ( 'NNS'   ))
   nnode  = ip (gtipnt ( 'NNODE' ))
   node   =     gtipnt ( 'NODE'  )
   nodnod =     gtipnt ( 'NODNOD')
   nosdim = ip (gtipnt ( 'NOSDIM'))
   nqlat  = ip (gtipnt ( 'NQLAT' ))
   nqstat = ip (gtipnt ( 'NQSTAT'))
   nstdb  =     gtipnt ( 'NSTDB ')
   if (nstdb.gt.0) then
      nstdb = ip(nstdb)
   else
      nstdb = 0
   endif
   nstru  = ip (gtipnt ( 'NSTRU' ))
   ntab   =     gtipnt ( 'NTAB'  )
   ntabm  = ip (gtipnt ( 'NTABM' ))
   ntcrel = ip (gtipnt ( 'NTCREL'))
   ntrigr = ip (gtipnt ( 'NTRIGR'))
   numnod =     gtipnt ( 'NUMNOD')
   of     =     gtrpnt ( 'OF'    )
   pfa    =     gtrpnt ( 'PFA'   )
   pmua   =     gtrpnt ( 'PMUA'  )
   prslot =     gtrpnt ( 'PRSLOT')
   psltvr =     gtrpnt ( 'PSLTVR')
   pw     =     gtrpnt ( 'PW'    )
   qpack  =     gtdpnt ( 'QPACK' )
   qbdpar =     gtipnt ( 'QBDPAR')
   qlat   =     gtrpnt ( 'QLAT'  )
   qlatgr =     gtrpnt ( 'QLATGR')
!     Nodenm pointer
   nodenm =     gtcpnt ('NODENM')
!     Mozart pointer
   qlatid =     max(1,gtcpnt ('QLATID'))
   qlatnm =     max(1,gtcpnt ('QLATNM'))
   gridnm =     gtcpnt ( 'GRIDNM')
   qltpar =     gtrpnt ( 'QLTPAR')
   qstat  =     gtrpnt ( 'QSTAT' )
   resbuf =     gtrpnt ( 'RESBUF')
   rfv1   =     gtdpnt ( 'RFV1'  )
   rfv2   =     gtdpnt ( 'RFV2'  )
   rho    =     gtrpnt ( 'RHO'   )
   rhsvv  =     gtdpnt ( 'RHSVV' )
   rhsm   =     gtrpnt ( 'RHSM'  )
   rpack  =     gtrpnt ( 'RPACK' )
   scnode =     gtipnt ( 'SCNODE')
   scceq  =     gtipnt ( 'SCCEQ' )
   scifri =     gtipnt ( 'SCIFRI')
   scimu  =     gtipnt ( 'SCIMU' )
   scmeq  =     gtipnt ( 'SCMEQ' )
   scqhs  =     gtipnt ( 'SCQHS' )
   sclceq =     gtipnt ( 'SCLCEQ')
   sclmeq =     gtipnt ( 'SCLMEQ')
   sclnod =     gtipnt ( 'SCLNOD')
   sclqhs =     gtipnt ( 'SCLQHS')
   snceq  =     gtrpnt ( 'SNCEQ' )
   snfric =     gtrpnt ( 'SNFRIC')
   snmeq  =     gtrpnt ( 'SNMEQ' )
   snmu   =     gtrpnt ( 'SNMU'  )
   snnode =     gtrpnt ( 'SNNODE')
   snqhs  =     gtrpnt ( 'SNQHS' )
   snwind =     gtrpnt ( 'SNWIND')
   sectc  =     gtrpnt ( 'SECTC' )
   sectv  =     gtrpnt ( 'SECTV' )
   solbuf =     gtrpnt ( 'SOLBUF')
   stdbq  =     max(gtrpnt ( 'STDBQ'),1)
   strbuf =     gtrpnt ( 'STRBUF')
   strclo =     gtlpnt ( 'STRCLO')
   strhis =     gtrpnt ( 'STRHIS')
   strpar =     gtrpnt ( 'STRPAR')
   strtyp =     gtipnt ( 'STRTYP')
   table  =     gtrpnt ( 'TABLE' )
   tauwi  =     gtrpnt ( 'TAUWI' )
   trcnrl =     gtipnt ( 'TRCNRL')
   triger =     gtipnt ( 'TRIGER')
   typcr  =     gtipnt ( 'TYPCR' )
   waoft  =     gtrpnt ( 'WAOFT' )
   wfrict =     gtipnt ( 'WFRICT')
   wft    =     gtrpnt ( 'WFT'   )
   wndpar =     gtrpnt ( 'WNDPAR')
   wshld  =     gtrpnt ( 'WSHLD' )
   wtt    =     gtrpnt ( 'WTT'   )
   work   =     gtdpnt ( 'WORK'  )
   x      =     gtrpnt ( 'X'     )
   grhis  =     gtrpnt ( 'GRHIS' )
!     Pointers to h and q:
   h2 = hpack + ngrid * 2
   q2 = qpack + ngrid * 2
!     storage width = waoft(,2)
   storWidth = waoft + ngrid
!
#if !  defined (SHR_MEM)
! ====  shared memory  ====
! ====  niet voor SRS BOS
! Koppeling Mozart
!
   if(lmoza .and. nqlat.gt.0)&
   &call MOZCONTROL (  istep   ,   ngrid  ,   nqlat  ,   dtf    ,&
   &juer    ,   istmoz ,   idmoz  ,   itim   ,&
   &rp(qltpar),cp(qlatid),rp(qlatgr),dp(hpack) )
#endif
!
!     Repeat until convergence
!
   iter   = 0
!
!     In case of automatic pseudo courant number adaptation
!     a minimum number of iteration steps will be carried out
!
   if (equal(dhtyp,0.)) then
      miniter = 0
   else
      miniter = 3
   endif
!
!     Start always with Bicgst method
!
   bicg = .true.

100 continue

   iter = iter + 1

   call FLOW (time ,dtf,steady,iter  ,istep ,itim  ,nbran  ,ngrid   ,&
   &ncontr,ncsrel,ntcrel,ntrigr,lkalm ,nnc   ,nnm   ,nnn    ,nns     ,&
   &nnf   ,nnmu  ,nosdim,lagstm,nlags ,juer  ,&
!     Mozart parameters plus groundwater switch
   &lmoza   ,  istmoz  ,cp(qlatid), cp(qlatnm), lgrwt   ,&
   &lrest   ,rp(flwpar),rp(contrl),&
   &ip(branch),ip(typcr),maxlev,ip(nlev),dp(hlev) ,rp(wft),rp(aft)   ,&
   &rp(wtt)   ,rp(att)   ,rp(arex)  ,ip(arexcn),ip(arexop),rp(of)    ,&
   &ip(bfrict),rp(bfricp),   maxtab ,ntabm  ,  ip(ntab)   ,rp(table) ,&
   &rp(sectc) ,rp(sectv) ,rp(grsize),rp(engpar),rp(gangle),rp(wndpar),&
   &ip(wfrict),rp(wshld) ,rp(snceq) ,rp(snmeq) ,rp(snqhs) ,rp(snfric),&
   &rp(snmu)  ,rp(snwind),ip(sclceq),ip(sclmeq),ip(sclqhs),ip(scceq) ,&
   &ip(scmeq) ,ip(scqhs) ,ip(scifri),ip(scimu) ,ip(scnode),rp(snnode),&
   &ip(sclnod),rp(pfa)   ,rp(pmua)  ,rp(pw)    ,   nexres ,rp(exres) ,&
   &lsalt  ,rp(izwft) ,   nhstat ,ip(hbdpar),   nqstat ,ip(qbdpar),&
   &nstru  ,ip(strtyp),rp(strpar),   nqlat  ,rp(qltpar),ip(grid)  ,&
   &rp(x)     ,rp(grhis) ,&
   &rp(rho)   ,   ngridm ,   nnode  ,ip(node)  ,   nbrnod ,&
   &ip(nodnod),ip(numnod),rp(prslot),rp(psltvr),rp(conhis),rp(waoft) ,&
   &rp(cpack) ,rp(rpack) ,rp(alfab) ,rp(tauwi) ,rp(ksi)   ,rp(a1m)   ,&
   &rp(hstat) ,rp(qstat) ,rp(qlat)  ,rp(qlatgr),lp(strclo),dp(rfv1)  ,&
   &dp(rfv2)  ,dp(abcd1) ,dp(abcd2 ),dp(mat)   ,dp(rhsvv) ,dp(hpack) ,&
   &dp(qpack) ,dp(delh)  ,dp(work)  ,ip(cnstrl),rp(strhis),ip(trcnrl),&
   &ip(triger),ip(cnpflg),ker       ,qtyp      ,  lfrou   ,rp(strbuf),&
   &ip(ibuf)  ,rp(solbuf),rp(buflag),ip(indx)  ,   bicg   ,rp(stdbq) ,&
   &nstdb  )

!
!        Check for convergence
!
!        Program stops at the moment in case of no convergence
!
   call soconv (   ngrid  ,   epsh   ,   epsq   ,dp(hpack) ,&
   &dp(qpack) ,   miniter,   conv   ,   juresi ,&
   &iter   ,   epsqrl ,   qtyp   ,   juer   ,&
   &ker    ,   flitmx ,   lconv  ,   inocon ,&
   &ip(ibuf)  ,rp(resbuf),   itstat )

!
!     If convergence has not been reached try again
!
   if ( conv .eq. 0 .and. ker .ne. fatal ) then
      goto 100
   endif

!
!     Update info for data base structure warnings
!
   call fltser (0      ,nstru  ,ngrid  ,rp(strpar) ,ip(strtyp) ,&
   &dp(h2) ,ker    ,juer   )
!
   if ( ker .ne. fatal ) then

      if ( lkalm ) then
!
         af2    =     gtrpnt ( 'AF2'   )
         deriva =     gtrpnt ( 'DERIVA')
         kabcd1 =     gtdpnt ( 'KABCD1')
         kabcd2 =     gtdpnt ( 'KABCD2')
         kalpar =     gtrpnt ( 'KALPAR')
         kbeta  =     gtdpnt ( 'KBETA' )
         kgain  =     gtrpnt ( 'KGAIN' )
         lfilt  =     gtlpnt ( 'LFILT' )
         np     = ip (gtipnt ( 'NP'    ))
         nsamp  = ip (gtipnt ( 'NSAMP' ))
         ntsam  = ip (gtipnt ( 'NTSAM' ))
         p1     =     gtrpnt ( 'P1'    )
         p2     =     gtrpnt ( 'P2'    )
         pcol   =     gtrpnt ( 'PCOL'  )
         qlatac =     gtrpnt ( 'QLATAC')
         res    =     gtrpnt ( 'RES'   )
         rescov =     gtrpnt ( 'RESCOV')
         sample =     gtrpnt ( 'SAMPLE')
         scares =     gtrpnt ( 'SCARES')
         scfric =     gtipnt ( 'SCFRIC')
         scmu   =     gtipnt ( 'SCMU'  )
         sclfri =     gtipnt ( 'SCLFRI')
         sclmu  =     gtipnt ( 'SCLMU' )
         smploc =     gtipnt ( 'SMPLOC')
         smpns  =     gtrpnt ( 'SMPNS' )
         wf2    =     gtrpnt ( 'WF2'   )

         call KALMAN   (maxlev ,maxtab ,nbran  ,ngrid ,ngridm ,nnc     ,&
         &nnm      ,nnn    ,nnode  ,nbrnod ,nns   ,nstru  ,ip(nlev),&
         &ntabm    ,nnf    ,nnmu   ,nsamp  ,ntsam ,np     ,nosdim  ,&
         &cpredn   ,lp(lfilt)      ,filstp ,time  ,dtf    ,lsalt   ,&
         &juer     ,nexres ,nqlat  ,rp(flwpar)    ,rp(kalpar)      ,&
         &dp(abcd1) ,rp(af2)   ,ip(branch),ip(grid)  ,ip(hbdpar),ip(typcr) ,&
         &dp(hpack) ,dp(hlev)  ,dp(kabcd1),dp(kabcd2),dp(kbeta) ,dp(mat)   ,&
         &ip(node)  ,ip(qbdpar),dp(rfv1)  ,dp(rfv2)  ,rp(rho)   ,dp(rhsvv) ,&
         &rp(wft)   ,rp(aft)   ,rp(wf2)   ,ip(wfrict),rp(of)    ,ip(bfrict),&
         &rp(bfricp),dp(qpack) ,ip(ntab)  ,rp(table) ,rp(sectc) ,rp(sectv) ,&
         &rp(grsize),rp(engpar),rp(x)     ,rp(exres) ,rp(prslot),rp(waoft) ,&
         &rp(cpack) ,rp(rpack) ,rp(alfab) ,rp(deriva),ip(strtyp),rp(strpar),&
         &rp(qltpar),rp(qlat)  ,lp(strclo),rp(qlatac),rp(tauwi) ,rp(arex)  ,&
         &ip(arexcn),ip(arexop),ip(sclnod),ip(scnode),rp(snnode),&
         &ip(scifri),ip(scimu) ,rp(snceq) ,rp(snmeq) ,rp(snqhs) ,&
         &rp(snfric),rp(snmu)  ,rp(snwind),ip(sclceq),ip(sclmeq),ip(sclqhs),&
         &ip(sclfri),ip(sclmu) ,ip(scceq) ,ip(scmeq) ,ip(scqhs) ,ip(scfric),&
         &ip(scmu)  ,ip(smploc),rp(p1)    ,rp(p2)    ,rp(pcol)  ,rp(rescov),&
         &rp(sample),rp(scares),rp(smpns) ,ip(indx)  ,ip(brnode),rp(rhsm)  ,&
         &rp(pfa)   ,rp(pmua)  ,rp(pw)    ,rp(res)   ,rp(kgain) ,   ker    ,&
         &rp(psltvr) )
      endif
!
   endif
!
!     Calculate variables for time level n+1
!
   if ( ker.ne.fatal ) then
      call flnp1 (   lkalm  ,   nbran  ,   ngrid  ,   nnf    ,&
      &ip(branch),ip(typcr) ,ip(bfrict),rp(bfricp),&
      &dp(h2 )   ,dp(q2)    ,   maxlev ,ip(nlev)  ,&
      &dp(hlev)  ,rp(wft),rp(aft)   ,   overlp ,&
      &rp(arex)  ,ip(arexcn),ip(arexop),rp(of)    ,&
      &maxtab ,   ntabm  ,ip(ntab)  ,rp(table) ,&
      &rp(sectc) ,rp(sectv) ,rp(prslot),rp(psltvr),&
      &rp(waoft) ,rp(grsize),rp(engpar),ip(scifri),&
      &rp(pfa)   ,   juer   ,rp(cpack) ,rp(rpack) ,&
      &rp(afwfqs),rp(alfab) ,&
      &rp(wtt)   ,rp(att)   ,ker    )
   endif
!
   call sowrbf( juresd    , justrd    , jusold    ,&
   &ip(ibuf)  , rp(resbuf), rp(strbuf),&
   &nstru     , ip(strtyp), ngrid     ,&
   &jufrou    , rp(solbuf), ker       ,&
   &conv      , lfrou     , frobuf    )
!
#if !  defined (SHR_MEM)
! ====  shared memory  ====
! ====  niet voor SRS BOS
! einde tijdsproces; Mozart-koppeling afsluiten
   if (lmoza .and. nqlat .gt. 0 .and.&
   &istep .eq. nstep) call ENDCT (idmoz, istcnt)
#endif
   return

end
