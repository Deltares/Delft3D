subroutine FLOWIT(g      ,istep  ,time   ,dt1    ,steady ,iter   ,&
&psi    ,theta  ,urelax ,nbran  ,ngrid  ,nnc    ,nnf    ,&
&nnm    ,nns    ,nnmu   ,nnn    ,nosdim ,maxlev ,nlev   ,&
&lkalm  ,lsalt  ,overlp ,maxtab ,ntabm  ,rhow   ,juer   ,&
!               mozart parameters plus groundwater switch
&lmoza  ,nstmoz ,qlatid ,qlatnm ,lgrwt  ,&
&hlev   ,wft    ,aft    ,wtt    ,att    ,arex   ,arexcn ,&
&arexop ,of     ,bfrict ,bfricp ,ntab   ,table  ,sectc  ,&
&sectv  ,grsize ,engpar ,gangle ,wndpar ,wfrict ,wshld  ,&
&snceq  ,snmeq  ,snqhs  ,snfric ,snmu   ,snwind ,sclceq ,&
&sclmeq ,sclqhs ,scceq  ,scmeq  ,scqhs  ,scifri ,scimu  ,&
&scnode ,snnode ,sclnod ,pfa    ,pmua   ,pw     ,nexres ,&
&exres  ,izwft  ,nhstat ,hbdpar ,nqstat ,qbdpar ,nstru  ,&
&strtyp ,strpar ,nqlat  ,qltpar ,grid   ,x      ,grhis  ,&
&rho    ,&
&ngridm ,branch ,typcr  ,nnode  ,node   ,nbrnod ,nodnod ,&
&numnod ,prslot ,psltvr ,waoft  ,cpa    ,rpa    ,alfab  ,&
&tauwi  ,ksi    ,a1m    ,hstat  ,qstat  ,qlat   ,qlatgr ,&
&strhis ,strclo ,rfv1   ,rfv2   ,abcd1  ,abcd2  ,mat    ,&
&rhsvv  ,hp     ,qp     ,theta2 ,exrstp ,omalfa ,omc    ,&
&omr    ,omw    ,cflpse ,iterbc ,resid  ,delh   ,work   ,&
&lambda ,relstr ,dhstru ,omcfl  ,dhtyp  ,ker    ,omboun ,&
&omqlat ,ibuf   ,lfrou  ,qtyp   ,indx   ,bicg   ,solbuf ,&
&stdbq  ,nstdb  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLOWIT (FLow ITeration step)
!
! Module description: Subroutine FLOWIT computes h(i)n+1 and Q(i)n+1 ,
!                     for a certain iteration level i.
!
!                     First the water level dependent hydrodynamic para-
!                     meters are calculated by calling routine FLHYPA.
!
!                     In subroutine FLBOUN the boundary conditions will
!                     be determined at the new time level tn+1.
!
!                     In subroutine FLQLAT the lateral discharges will
!                     be computed in the user selected discharge stati-
!                     ons at the new time level tn+1/2. Notice that the
!                     Preissmann scheme requires boundary conditions and
!                     lateral discharges at different time levels.
!
!                     After this the ABCDE coefficients are calculated
!                     in routine FLDSCO. This routine delivers the re-
!                     sulting f, r and v result arrays which are used by
!                     routine FLSOEQ. See functional design for the
!                     water flow module for a description of the double
!                     sweep method.
!
!                     Subroutine FLSOEQ computes a nodal administration
!                     matrix and solves this matrix. The results are
!                     used to calculate the flow in the branches.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 96 a1m               P  -
! 29 aft               P  -
! 93 alfab             P  -
! 32 arex              P  -
! 33 arexcn            P  -
! 34 arexop            P  -
! 31 att               P  -
! 37 bfricp            P  -
! 36 bfrict            P  -
! 81 branch            P  -
! 91 cpa               P  -
!  4 dt1               P  -
! 43 engpar            P  -
! 66 exres             P  -
!  1 g                 P  -
! 44 gangle            P  -
! 77 grid              P  -
! 42 grsize            P  -
! 69 hbdpar            P  -
! 27 hlev              P  -
! 97 hstat             P  -
!  2 istep             P  -
!  6 iter              I  Iteration step.
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 67 izwft             P  -
! 26 juer              P  -
! 95 ksi               P  -
! 20 lkalm             I  -
! 21 lsalt             P  -
! 18 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 23 maxtab            I  Maximum number of defined tables.
! 10 nbran             I  Number of branches.
! 85 nbrnod            I  Maximum number of connected branches to one
!                         node.
! 65 nexres            P  -
! 11 ngrid             I  Number of grid points in network.
! 80 ngridm            I  Maximum number of gridpoints in a branch.
! 68 nhstat            P  -
! 19 nlev              P  -
! 12 nnc               I  Number of uncorrelated random noise processes
!                         for the continity equation.
! 13 nnf               I  Number of uncertain bed friction parameters.
! 14 nnm               I  Number of uncorrelated random noise processes
!                         for the momentum equation.
! 16 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
! 17 nnn               I  Number of uncorrelated random noise processes
!                         for nodal (= boundary) equations.
! 83 nnode             I  Number of nodes.
! 15 nns               I  Number of uncorrelated random noise processes
!                         for the Q-H relations of structures.
! 84 node              P  -
! 86 nodnod            P  -
! 75 nqlat             P  -
! 70 nqstat            P  -
! 72 nstru             I  Number of structures.
! 38 ntab              P  -
! 24 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 87 numnod            P  -
! 35 of                P  -
! 22 overlp            P  -
! 62 pfa               P  -
! 63 pmua              P  -
! 88 prslot            P  -
!  7 psi               P  -
! 89 psltvr            P  -
! 64 pw                P  -
! 71 qbdpar            P  -
! 99 qlat              P  -
! 76 qltpar            P  -
! 98 qstat             P  -
! 79 rho               P  -
! 25 rhow              P  -
! 92 rpa               P  -
! 54 scceq             P  -
! 57 scifri            P  -
! 55 scmeq             P  -
! 58 scimu             P  -
! 56 scqhs             P  -
! 61 sclnod            P  -
! 59 scnode            P  -
! 40 sectc             P  -
! 41 sectv             P  -
! 48 snceq             P  -
! 51 snfric            P  -
! 49 snmeq             P  -
! 52 snmu              P  -
! 60 snnode            P  -
! 50 snqhs             P  -
! 53 snwind            P  -
!  5 steady            P  -
! 74 strpar            P  -
! 73 strtyp            P  -
! 39 table             P  -
! 94 tauwi             P  -
!  8 theta             P  -
!  3 time              P  -
! 82 typcr             P  -
!  9 urelax            P  -
! 90 waoft             P  -
! 46 wfrict            P  -
! 28 wft               P  -
! 45 wndpar            P  -
! 47 wshld             P  -
! 30 wtt               P  -
! 78 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flboun  FLow BOUNdary conditions
! fldsco  FLow Double Sweep COefficients
! flhypa  FLow HYdraulic PArameters
! flkapa  FLow KAlman correction PArameter
! flqlat  FLow Q LATeral
! flsoeq  FLow SOlve EQuations
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flowit.pf,v $
! Revision 1.19  1999/03/15  14:21:45  kuipe_j
! Improve writing Froude file
!
! Revision 1.18  1998/06/24  11:10:22  kuipe_j
! Try direct solver if BICGST fails
!
! Revision 1.17  1998/05/25  19:12:08  kuipe_j
! Wendy structures
!
! Revision 1.16  1997/11/04  14:17:25  kuipe_j
! Retention basin
!
! Revision 1.15  1997/05/26  07:41:31  kuipe_j
! dicretization Q(H), H(Q) boundaries improved
!
! Revision 1.14  1997/01/23  08:29:14  kuipe_j
! Make flow module robust
!
! Revision 1.13  1996/11/01  15:04:16  kuipe_j
! Improve contoller messages
!
! Revision 1.12  1996/10/31  10:30:22  kuipe_j
! Extra resistance finished
!
! Revision 1.11  1996/09/03  14:52:06  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.10  1996/04/12  13:04:11  kuipe_j
! headers, minor changes
!
! Revision 1.9  1996/04/11  08:23:45  kuipe_j
! Kalman module added
!
! Revision 1.8  1995/11/21  11:07:58  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic pseudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.7  1995/09/22  10:02:02  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:10:59  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:36:45  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:25  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:55:16  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:16  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:57  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  13:19:33  kuipe_j
! Prevention against devide by zero.
!
! Revision 1.3  1994/11/28  08:37:42  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:18  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:53  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   !DEC$ IF DEFINED (_DLL)
   use SobekRE_OpenMI
   !DEC$ ENDIF

   include '..\include\sobdim.i'
   include '..\include\mempool.i'

!
!     Declaration of parameters:
!
   integer iter  , maxlev, maxtab, nbran , nexres, ngrid , ngridm
   integer nhstat, nbrnod, nnode , nqstat, nqlat , nstru , ntabm
   integer nnc, nnm, nnn, nns, nnf, nnmu, nosdim
   integer exrstp, juer, ker
   integer iterbc, istep, nstdb
   integer node(4,nnode), indx(nnode)
   integer numnod(nnode), nodnod(nnode,nbrnod+1)
   integer branch(4,nbran), bfrict(3,nbran), wfrict(3,nbran)
   integer typcr(nbran), ntab(4,maxtab), nlev(ngrid)
   integer arexcn(ngrid,2), arexop(2)
   integer grid(ngrid), strtyp(10,nstru)
   integer hbdpar(3,*), qbdpar(3,*)
   integer sclceq(nnc+1),   sclmeq(nnm+1), sclqhs(nns+1)
   integer scifri(ngrid),   scimu(nstru)
   integer scceq(*), scmeq(*), scqhs(*)
   integer sclnod(nnn+1), scnode(*)
   integer ibuf(*)
!
   real    g, psi , theta, urelax, rhow   ,overlp,omqlat
   real    lambda ,relstr, dhstru, omcfl  ,dhtyp ,omboun
   real    theta2 ,omalfa ,omc    ,omr    ,omw   ,cflpse, qtyp
   real    table(ntabm),stdbq(nstdb)
   real    wft(ngrid,maxlev), aft(ngrid,maxlev)
   real    wtt(ngrid,maxlev), att(ngrid,maxlev), arex(ngrid,4)
   real    of (ngrid,maxlev), izwft(ngrid,maxlev)
   real    cpa(ngrid,4), rpa(ngrid,4), alfab(ngrid), gangle(ngrid)
   real    wshld(ngrid), bfricp(6,ngrid)
   real    grsize(4,ngrid,*), engpar(9), wndpar(3), exres(3,*)
   real    sectc(ngrid,3), sectv(ngrid,dmsecv)
   real    ksi(ngrid), x(ngrid)
   real    grhis(*)
   real    a1m(ngrid), tauwi(ngrid)
   real    qltpar(9,*), qlat(nqlat,9), qlatgr(ngrid)
   real    strpar(dmstrpar,*), strhis(dmstrh,*)
   real    rho(ngrid), prslot(3,nbran), psltvr(7,ngrid)
   real    waoft(ngrid,*)
   real    hstat(*), qstat(*)
   real    pfa(nnf), pmua(nnmu), pw(1)
   real    snceq(nosdim,nnc), snmeq(nosdim,nnm), snqhs(nosdim,nns)
   real    snfric(2,nnf), snmu(2,nnmu), snwind(2)
   real    snnode(nosdim,nnn)
   real    solbuf(dmbuf2,7,ngrid)
!
   double precision time ,dt1 ,resid, hlev(ngrid,maxlev)
   double precision mat(nnode,nnode), rhsvv(nnode)
   double precision abcd1(ngridm,5) , abcd2(ngridm,5)
   double precision rfv1(ngrid,3)   , rfv2(ngrid,3)
   double precision delh(nnode),work(nnode,*)
   double precision hp(ngrid,3), qp(ngrid,3)
!
   logical lkalm, lsalt, steady, bicg
   logical strclo(*)
   logical lfrou
!     mozart declarations
   character*40  qlatid(*), qlatnm(*)
   logical       lmoza, lgrwt
   integer       nstmoz
!
!     Declaration of local parameters:
!

   integer plrec, pllrec, plave, plold, pbal, grwpar
   integer mugr, kdgr, grcgr, hbal
   integer strunm
   real    grdh
!
!     External functions
!
   integer gtrpnt, soipar
   real sorpar
   external gtrpnt, soipar, sorpar
   integer  gtcpnt
   external gtcpnt
!
!     Include sobek error codes:
!
   include '..\include\errcod.i'

   !DEC$ IF DEFINED (_DLL)

   if (initdone .and. OpenMIactive()) then
      ! Fill structure changes

      strunm   =    gtcpnt('STRUNM')

      call GetStruct(strtyp, cp(strunm), strpar, nstru)
   endif
   !DEC$ ENDIF
!
!     Calculate water level dependent hydraulic parameters
!
   call FLHYPA(time    ,iter    ,nbran   ,ngrid   ,branch ,typcr  ,&
!                 <h_n>    <h_*>
   &hp(1,2) ,hp(1,1) ,maxlev  ,nlev    ,hlev   ,wft    ,&
   &aft     ,wtt     ,att     ,overlp  ,arex   ,arexcn ,&
!                                                    <Q_n>    <Q_*>
   &arexop , of      ,bfrict  ,bfricp ,qp(1,1) ,qp(1,2),&
   &maxtab  ,ntabm   ,ntab    ,table  ,sectc   ,sectv  ,&
   &grsize  ,engpar  ,gangle  ,wndpar ,wfrict  ,wshld  ,&
   &x       ,nexres  ,exres   ,lsalt  ,izwft   ,juer   ,&
   &prslot  ,psltvr  ,waoft   ,cpa    ,rpa     ,alfab  ,&
   &tauwi   ,ksi     ,a1m     ,ker    ,dt1     ,theta2 ,&
   &exrstp  ,omalfa  ,omc     ,omr    ,omw    )
!
   if (ker .eq. fatal) goto 1000
!
!
!     Implement effect of correction parameters.
!
   if ( lkalm ) then
      call FLKAPA (istep  ,ngrid  ,nnf    ,nstru  ,nnmu   ,nbran  ,&
      &cpa(1,1),scifri ,pfa   ,strpar ,scimu  ,pmua   ,&
      &branch ,wfrict ,tauwi  ,pw     )
   endif

!     Determine boundary conditions at the new time level t(n+1)
!     Only the conditions which are defined as a function of time
!
   call FLBOUN(time   ,&
   &maxtab ,ntabm  ,ntab   ,table   ,&
   &nhstat ,hstat  ,hbdpar ,&
   &nqstat ,qstat  ,qbdpar ,omboun  ,iter)
!
!     Compute lateral discharges in the user selected discharge stations
!     at the new time level t(n+1/2)
!
   call FLQLAT(g       ,time   ,ngrid  ,lambda ,&
!                         <h_n>    <h_*>
   &x       ,hp(1,1),hp(1,2),&
   &strtyp  ,strpar ,maxtab ,ntabm  ,ntab    ,&
!     mozart parameters
   &lmoza   ,nstmoz ,qlatid ,&
   &table   ,nqlat   ,qltpar ,juer   ,qlat   ,qlatgr ,&
   &strclo  ,strhis  ,theta2 ,dt1    ,ker    ,omqlat ,&
   &dhstru  ,relstr  ,iter)
!
   if (ker .eq. fatal) goto 1000

   if (lgrwt) then
!
!     Compute lateral discharge caused by flux with groundwater
!
      mugr   =     gtrpnt ( 'MU' )
      kdgr   =     gtrpnt ( 'KD' )
      grcgr  =     gtrpnt ( 'GRC' )
      grwpar =     gtrpnt ( 'GRWPAR' )
      hbal   =     gtrpnt ( 'HBAL' )

      grdh = sorpar(rp(grwpar),2)
      plrec = soipar(rp(grwpar),3)
      pllrec = soipar(rp(grwpar),4)
      plave = soipar(rp(grwpar),5)
      plold = soipar(rp(grwpar),6)
      pbal = soipar(rp(grwpar),7)

      call FLQLGR(iter,istep,dt1,ngrid,rp(mugr),rp(kdgr),&
      &rp(grcgr),grhis, qlatgr,&
      &x,  hp, grdh, plrec, pllrec, plave,&
      &plold,pbal,rp(hbal),juer,ker)

   endif
!
!     Calculate ABCDE coefficients
!
   call FLDSCO(g      ,iter   ,dt1    ,steady ,psi    ,theta  ,&
   &exrstp ,nbran  ,branch ,ngrid  ,lambda ,relstr ,&
   &dhstru ,hp     ,qp     ,qlatgr ,grid   ,&
   &x      ,waoft  ,cpa    ,rpa    ,alfab  ,tauwi  ,&
   &ksi    ,lsalt  ,rho    ,rhow   ,a1m    ,nstru  ,&
   &strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,&
   &ngridm ,strclo ,strhis ,abcd1  ,abcd2  ,rfv1   ,&
   &rfv2   ,theta2 ,omr    ,cflpse ,omcfl  ,dhtyp  ,&
   &ibuf   ,istep  ,lfrou  ,qlat   ,qltpar ,nqlat  ,&
   &hlev   ,maxlev ,solbuf ,stdbq  ,nstdb  ,juer   ,&
   &ker    )
!
!     Compute nodal administration matrix and solve this matrix
!
   call FLSOEQ(lkalm  ,ngrid  ,nstru  ,nnc    ,nnm    ,nns   ,&
   &nnf    ,nnmu   ,nnn    ,nosdim ,&
   &qtyp   ,qlatnm ,sclceq ,sclmeq ,sclqhs ,&
   &scceq  ,scmeq  ,scqhs  ,scifri ,scimu  ,snceq ,&
   &snmeq  ,snqhs  ,snfric ,snmu   ,snwind ,strtyp,&
   &wfrict ,scnode ,snnode ,sclnod ,nnode  ,node  ,&
   &nbrnod ,nodnod ,numnod ,nbran  ,branch ,maxtab,&
   &ntabm  ,ntab   ,table  ,hstat  ,hbdpar ,qstat ,&
   &qbdpar ,urelax ,rfv1   ,rfv2   ,mat    ,rhsvv ,&
   &hp     ,qp     ,iterbc ,resid  ,delh   ,work  ,&
   &ker    ,steady ,nqlat  ,qlat   ,qltpar ,strhis,&
   &relstr ,theta  ,dt1    ,indx   ,juer   ,bicg  )
!
!     Exception
!
1000 continue

end
