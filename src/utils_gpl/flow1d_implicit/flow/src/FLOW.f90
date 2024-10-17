subroutine FLOW (time   ,dt1    ,steady ,iter   ,istep  ,itim   ,&
&nbran  ,ngrid  ,ncontr ,ncsrel ,ntcrel ,ntrigr ,lkalm  ,nnc    ,&
&nnm    ,nnn    ,nns    ,nnf    ,nnmu   ,nosdim ,lagstm ,nlags  ,&
&juer   ,&
!              mozart parameters (extra qlatnm) plus groundwater switch
&lmoza  ,nstmoz ,qlatid ,qlatnm ,lgrwt  ,lrest  ,&
&flwpar ,contrl ,branch ,typcr  ,maxlev ,nlev   ,hlev   ,&
&wft    ,aft    ,wtt    ,att    ,arex   ,arexcn ,arexop ,of     ,&
&bfrict ,bfricp ,maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,&
&grsize ,engpar ,gangle ,wndpar ,wfrict ,wshld  ,snceq  ,snmeq  ,&
&snqhs  ,snfric ,snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,&
&scceq  ,scmeq  ,scqhs  ,scifri ,scimu  ,scnode ,snnode ,&
&sclnod ,pfa    ,pmua   ,pw     ,nexres ,exres  ,lsalt  ,&
&izwft  ,nhstat ,hbdpar ,nqstat ,qbdpar ,nstru  ,strtyp ,&
&strpar ,nqlat  ,qltpar ,grid   ,x      ,grhis  ,&
&rho    ,ngridm ,&
&nnode  ,node   ,nbrnod ,nodnod ,numnod ,prslot ,psltvr ,&
&conhis ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,ksi    ,&
&a1m    ,hstat  ,qstat  ,qlat   ,qlatgr ,strclo ,rfv1   ,&
&rfv2   ,abcd1  ,abcd2  ,mat    ,rhsvv  ,hp     ,&
&qp     ,delh   ,work   ,cnstrl ,strhis ,trcnrl ,triger ,&
&cnpflg ,ker    ,qtyp   ,lfrou  ,strbuf ,ibuf   ,solbuf ,&
&buflag ,indx   ,bicg   ,stdbq  ,nstdb  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLOW (FLOW main routine)
!
! Module description: Subroutine FLOW will perform the next tasks:
!
!                     -   Initialisation of water levels and discharges
!                         for every iteration step;
!                     -   Computation of triggers and controllers in the
!                         first iteration step of a new time step;
!                     -   Control of the flow iteration for computation
!                         of the water flow hn+1 and Qn+1.
!
!                     Every first iteration step routine FLCSTR is cal-
!                     led to calculate triggers and controllers. After
!                     this always routine FLOWIT is called to calculate
!                     a new iteration step.
!
!                     Starting from the water flow h(i)n+1 and Q(i)n+1
!                     from the previous iteration level i, subroutine
!                     FLOWIT will compute the water flow h(i+1)n+1 and
!                     Q(i+1)n+1 for the new iteration level i+1.
!
!                     From the control module two delta times are pas-
!                     sed. In case the flow module is calculating unste-
!                     ady delta t2 will have the same value as delta t1.
!                     Delta t2 must be used when calculating the ABCDE
!                     coefficients. In all other cases delta t1 is used.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 98 a1m               P  -
! 29 aft               P  -
! 95 alfab             P  -
! 32 arex              P  -
! 33 arexcn            P  -
! 34 arexop            P  -
! 31 att               P  -
! 37 bfricp            P  -
! 36 bfrict            P  -
! 23 branch            P  -
! 91 conhis            P  -
! 22 contrl            P  -
! 93 cp                P  -
!  2 dt1               P  -
! 45 engpar            P  -
! 68 exres             P  -
! 21 flwpar            I  -
! 46 gangle            P  -
! 80 grid              P  -
! 44 grsize            P  -
! 72 hbdpar            P  -
! 27 hlev              P  -
! 99 hstat             P  -
!  5 istep             P  -
!  4 iter              I  Iteration step.
!  6 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 70 izwft             P  -
! 20 juer              P  -
! 97 ksi               P  -
! 13 lkalm             P  -
! 69 lsalt             P  -
! 25 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 38 maxtab            I  Maximum number of defined tables.
!  7 nbran             I  Number of branches.
! 86 nbrnod            I  Maximum number of connected branches to one
!                         node.
!  9 ncontr            P  -
! 10 ncsrel            I  number of controller structure relations.
! 67 nexres            P  -
!  8 ngrid             I  Number of grid points in network.
! 83 ngridm            I  Maximum number of gridpoints in a branch.
! 71 nhstat            P  -
! 26 nlev              P  -
! 14 nnc               I  Number of uncorrelated random noise processes
!                         for the continity equation.
! 18 nnf               I  Number of uncertain bed friction parameters.
! 15 nnm               I  Number of uncorrelated random noise processes
!                         for the momentum equation.
! 19 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
! 16 nnn               I  Number of uncorrelated random noise processes
!                         for nodal (= boundary) equations.
! 84 nnode             I  Number of nodes.
! 17 nns               I  Number of uncorrelated random noise processes
!                         for the Q-H relations of structures.
! 85 node              P  -
! 87 nodnod            P  -
! 78 nqlat             P  -
! 73 nqstat            P  -
! 75 nstru             I  Number of structures.
! 40 ntab              P  -
! 39 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 11 ntcrel            I  Is the number of trigger controller relations.
! 12 ntrigr            I  Number of triggers.
! 88 numnod            P  -
! 35 of                P  -
! 64 pfa               P  -
! 65 pmua              P  -
! 89 prslot            P  -
! 90 psltvr            I  Preissmann slot variables for every grid point
!                         i (assuring positive water depths):
!                         (1,i) = Value for C**2*R for positive flow.
!                         (2,i) = Value for C**2*R for negative flow.
!                         (3,i) = Bottom of slot (funnel)
!                         (4,i) = Division level between trapezium and
!                                 rectangle of slot (top of rectangle
!                                 and bottom of trapezium)
!                         (5,i) = Top of slot
!                         (6,i) = Bottom width of slot (width of
!                                 rectangle)
!                         (7,i) = Top width of slot (top of trapezium)
! 66 pw                P  -
! 74 qbdpar            P  -
! 79 qltpar            P  -
! 82 rho               P  -
! 94 rp                P  -
! 56 scceq             P  -
! 59 scifri            P  -
! 57 scmeq             P  -
! 60 scimu             P  -
! 58 scqhs             P  -
! 63 sclnod            P  -
! 61 scnode            P  -
! 42 sectc             P  -
! 43 sectv             P  -
! 50 snceq             P  -
! 53 snfric            P  -
! 51 snmeq             P  -
! 54 snmu              P  -
! 62 snnode            P  -
! 52 snqhs             P  -
! 55 snwind            P  -
!  3 steady            P  -
! 77 strpar            P  -
! 76 strtyp            P  -
! 41 table             P  -
! 96 tauwi             P  -
!  1 time              P  -
! 24 typcr             P  -
! 92 waoft             P  -
! 48 wfrict            P  -
! 28 wft               P  -
! 47 wndpar            P  -
! 49 wshld             P  -
! 30 wtt               P  -
! 81 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! flchkh  FLow CHeck H (Water levels > bottom)
! flcstr  FLow Controlled STRuctures
! flins   FLow Initialise Next Step
! flowit  FLow ITeration step
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flow.pf,v $
! Revision 1.23  1999/03/15  14:21:43  kuipe_j
! Improve writing Froude file
!
! Revision 1.22  1998/12/11  13:06:59  kuipe_j
! improve annotation in dumps
!
! Revision 1.21  1998/06/24  11:10:20  kuipe_j
! Try direct solver if BICGST fails
!
! Revision 1.20  1998/06/08  12:29:36  kuipe_j
! time lag hydr controller
!
! Revision 1.19  1997/11/04  14:08:32  kuipe_j
! dhstru in lat structures,theta set for steady flow
!
! Revision 1.18  1997/05/26  07:41:29  kuipe_j
! dicretization Q(H), H(Q) boundaries improved
!
! Revision 1.17  1997/01/23  08:29:12  kuipe_j
! Make flow module robust
!
! Revision 1.16  1996/11/01  15:04:14  kuipe_j
! Improve contoller messages
!
! Revision 1.15  1996/09/03  14:52:05  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.14  1996/04/12  13:04:09  kuipe_j
! headers, minor changes
!
! Revision 1.13  1996/04/11  08:23:44  kuipe_j
! Kalman module added
!
! Revision 1.12  1996/01/17  14:38:40  kuipe_j
! header update
!
! Revision 1.11  1995/11/21  11:07:57  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.10  1995/10/18  10:49:55  hoeks_a
! Some small changes
!
! Revision 1.9  1995/10/18  08:59:23  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.8  1995/10/11  12:24:06  kuipe_j
! Remove aux output temp
!
! Revision 1.7  1995/09/22  10:01:58  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:10:58  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:36:44  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:24  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:55:15  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:15  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:57  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  13:30:39  kuipe_j
! Improved message handling
!
! Revision 1.3  1994/11/28  08:37:40  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:17  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:52  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
   include '..\include\sobcon.i'
!
!     Declaration of parameters:
!
   integer iter  , istep , maxlev, maxtab, nbran , ncontr, nexres
   integer ngrid , ngridm, nhstat, nnode , nqstat, nqlat , nstru
   integer ntabm , ncsrel, ntcrel, ntrigr, nbrnod, nstdb
   integer nnc   , nnm   , nnn   , nns   , nnf   , nnmu  , nosdim
   integer lagstm ,nlags , juer  , ker
   integer node(4,nnode) , indx(nnode)
   integer numnod(nnode), nodnod(nnode,nbrnod+1)
   integer branch(4,nbran), bfrict(3,nbran), wfrict(3,nbran)
   integer typcr(nbran), ntab(4,maxtab), nlev(ngrid)
   integer grid(ngrid), strtyp(10,*)
   integer hbdpar(3,*), qbdpar(3,*)
   integer scifri(ngrid), scimu(nstru)
   integer sclceq(nnc+1),   sclmeq(nnm+1), sclqhs(nns+1)
   integer scceq(*), scmeq(*), scqhs(*)
   integer sclnod(nnn+1), scnode(*)
   integer itim(2), arexcn(ngrid,2), arexop(2), cnpflg(dmcopr,*)
   integer cnstrl(2,ncsrel),trcnrl(5,ntcrel),triger(10,ntrigr)
   integer ibuf(*)
!
   real    table(ntabm),stdbq(nstdb)
   real    arex(ngrid,4)
   real    wft(ngrid,maxlev), aft(ngrid,maxlev)
   real    wtt(ngrid,maxlev), att(ngrid,maxlev)
   real    of (ngrid,maxlev), izwft(ngrid,maxlev)
   real    cp(ngrid,4), rp(ngrid,4), alfab(ngrid), gangle(ngrid)
   real    wshld(ngrid), bfricp(6,ngrid)
   real    grsize(4,ngrid,*), engpar(9), wndpar(3), exres(3,*)
   real    sectc(ngrid,3), sectv(ngrid,dmsecv)
   real    ksi(ngrid), x(ngrid)
   real    grhis(*)
   real    a1m(ngrid), tauwi(ngrid)
   real    prslot(3,nbran), psltvr(7,ngrid)
   real    contrl(17,*), conhis(5,*) ,strhis(dmstrh,*)
   real    qltpar(9,*), qlat(*), qlatgr(ngrid)
   real    strpar(dmstrpar,*)
   real    rho(ngrid)
   real    waoft(ngrid,*)
   real    hstat(*), qstat(*)
   real    pfa(nnf), pmua(nnmu), pw(1)
   real    snceq(nosdim,nnc), snmeq(nosdim,nnm), snqhs(nosdim,nns)
   real    snfric(2,nnf), snmu(2,nnmu), snwind(2)
   real    snnode(nosdim,nnn)
   real    flwpar(*)
   real    qtyp
   real    strbuf(dmbuf1,2,*), solbuf(dmbuf2,7,ngrid)
   real    buflag(lagstm,nlags)
!
   double precision mat(nnode,nnode), rhsvv(nnode)
   double precision abcd1(ngridm,5) , abcd2(ngridm,5)
   double precision rfv1(ngrid,3)   , rfv2(ngrid,3)
   double precision delh(nnode)     , work(nnode,*)
   double precision time ,dt1, hlev(ngrid,maxlev)
   double precision hp(ngrid,3), qp(ngrid,3)
!
   logical lkalm ,lsalt, steady, bicg
   logical strclo(*)
   logical lfrou
!     mozart declarations
   character*40  qlatid(*), qlatnm(*)
   logical       lmoza, lgrwt ,lrest
   integer       nstmoz
!
!     Declaration of local variables
!
   integer ibuf1,ibuf2
   integer iterbc ,exrstp ,istru, igrid
   real    theta2 ,omalfa ,omc    ,omr    ,omw    ,omboun ,omqlat
   real    g      ,psi    ,theta  ,rhow   ,omega  ,lambda ,relstr
   real    dhstru ,cflpse ,overlp ,omcfl  ,dhtyp  ,urelax
   double precision        resid
   character*18            txt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   g      =  flwpar( 1 )
   psi    =  flwpar( 2 )
   theta  =  flwpar( 3 )
   rhow   =  flwpar( 6 )
   omega  =  flwpar( 8 )
   lambda =  flwpar(10 )
   relstr =  flwpar(11 )
   dhstru =  flwpar(12 )
   cflpse =  flwpar(13 )
   iterbc =  int (flwpar(14 ))
   resid  =  dble(flwpar(15 ))
   overlp =  flwpar(16 )
   omcfl  =  flwpar(18 )
   dhtyp  =  flwpar(19 )
   exrstp =  int (flwpar(20 ))
   if ( steady ) then
      theta = 1.0
   endif
!
!     omega instead of urelax, urelax dummy now
!
   urelax = 1.
!
!     Reduction of numerical parameters for the user
!
   omalfa = omega
   omc    = omega
   omr    = omega
   omw    = omega
   omboun = omega
   omqlat = omega
   theta2 = theta
!
!     Put last computed approximations of new h and q in positions *
!                                              <h_n>    <h_*>
   call FLINS (steady    ,iter    ,ngrid   ,hp(1,1) ,hp(1,2)    ,&
!                 <h_n+1>    <Q_n>    <Q_*>    <Q_n+1>  <At_n>
   &hp(1,3)   ,qp(1,1) ,qp(1,2) ,qp(1,3) ,waoft(1,5) ,&
!                 <At_n+1>
   &waoft(1,4),qtyp)
!
   if ( iter .eq. 1 .or. steady ) then

      call FLCSTR (time   ,dt1     ,istep   ,g          ,rho    ,&
      &ncontr ,contrl  ,strtyp  ,strpar     ,ngrid  ,&
!                      <h_*>    <Q_*>    <Af>
      &hp(1,2) ,qp(1,2) ,waoft(1,3) ,maxtab ,ntabm  ,&
      &ntab    ,table   ,ncsrel ,cnstrl     ,ntrigr ,&
      &triger  ,ntcrel  ,trcnrl ,nstru      ,strhis ,&
      &conhis  ,cnpflg  ,lagstm ,nlags      ,buflag ,&
      &nqlat   ,qltpar  ,juer   ,lrest      ,ker    )

   endif

   if (ker .eq. fatal) goto 1000
!
   call FLOWIT (g      ,istep  ,time   ,dt1    ,steady ,iter   ,&
   &psi    ,theta  ,urelax ,nbran  ,ngrid  ,nnc    ,nnf    ,&
   &nnm    ,nns    ,nnmu   ,nnn    ,nosdim ,maxlev ,nlev   ,&
   &lkalm  ,lsalt  ,overlp ,maxtab ,ntabm  ,rhow   ,juer   ,&
!          mozart parameters plus groundwater switch
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
   &numnod ,prslot ,psltvr ,waoft  ,cp     ,rp     ,alfab  ,&
   &tauwi  ,ksi    ,a1m    ,hstat  ,qstat  ,qlat   ,qlatgr ,&
   &strhis ,strclo ,rfv1   ,rfv2   ,abcd1  ,abcd2  ,mat    ,&
   &rhsvv  ,hp     ,qp     ,theta2 ,exrstp ,omalfa ,omc    ,&
   &omr    ,omw    ,cflpse ,iterbc ,resid  ,delh   ,work   ,&
   &lambda ,relstr ,dhstru ,omcfl  ,dhtyp  ,ker    ,omboun ,&
   &omqlat ,ibuf   ,lfrou  ,qtyp   ,indx   ,bicg   ,solbuf ,&
   &stdbq  ,nstdb  )
!
   if (ker .eq. fatal) goto 1000
!
   call FLCHKH (ngrid  ,nbran  ,branch ,typcr  ,maxlev ,hlev   ,&
   &hp(1,3),juer   ,ker    ,prslot ,psltvr )
!
!     Fill buffers
!
   ibuf1 = ibuf(1)
   do 1010 istru = 1 , nstru
      strbuf(ibuf1,1,istru) = strhis(4,istru)
      if (strtyp(2,istru) .eq. cstbra) then
         strbuf(ibuf1,2,istru) = hp(strtyp(3,istru),3)-&
         &hp(strtyp(4,istru),3)
      else
         strbuf(ibuf1,2,istru) = -999.
      endif
1010 continue
   ibuf2 = ibuf(2)
   do 1020 igrid = 1 , ngrid
      solbuf(ibuf2,1,igrid) = hp(igrid,3)
      solbuf(ibuf2,2,igrid) = qp(igrid,3)
      solbuf(ibuf2,3,igrid) = hp(igrid,3)-psltvr(5,igrid)
      solbuf(ibuf2,4,igrid) = waoft(igrid,1)
      solbuf(ibuf2,5,igrid) = waoft(igrid,2)
      solbuf(ibuf2,6,igrid) = 1/waoft(igrid,8)
1020 continue
!
!        Set minus sign to define that buffer data has been stored
   ibuf(1) = -ibuf(1)
1000 continue

   if (ker .ne. ok) then
      write (txt,'(2(1x,i8))') itim
      call error (juer,'FLOW timestep@'//txt//'@',eflmes,info)
      if (ker .ne. fatal) ker = ok
   endif
!
end
