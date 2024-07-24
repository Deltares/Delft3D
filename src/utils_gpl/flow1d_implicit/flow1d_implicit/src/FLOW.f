      subroutine sre_FLOW (time   ,dt1    ,steady ,iter   ,istep  ,itim,
     + nbran  ,ngrid  ,ncontr ,ncsrel ,ntcrel ,ntrigr ,lkalm  ,nnc    ,
     + nnm    ,nnn    ,nns    ,nnf    ,nnmu   ,nosdim ,lagstm ,nlags  ,
     + juer   ,
c              mozart parameters (extra qlatnm) plus groundwater switch
     +         lmoza  ,nstmoz ,qlatid ,qlatnm ,lgrwt  ,lrest  , 
     + flwpar ,contrl ,branch ,typcr  ,maxlev ,nlev   ,hlev   ,
     + wft    ,aft    ,wtt    ,att    ,arex   ,arexcn ,arexop ,of     ,
     + bfrict ,bfricp ,maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,
     + grsize ,engpar ,gangle ,wndpar ,wfrict ,wshld  ,snceq  ,snmeq  ,
     +         snqhs  ,snfric ,snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,
     +         scceq  ,scmeq  ,scqhs  ,scifri ,scimu  ,scnode ,snnode ,
     +         sclnod ,pfa    ,pmua   ,pw     ,nexres ,exres  ,lsalt  ,
     +         izwft  ,nhstat ,hbdpar ,nqstat ,qbdpar ,nstru  ,strtyp ,
     +         strpar ,nqlat  ,qltpar ,grid   ,x      ,grhis  ,
     +         rho    ,ngridm ,
     +         nnode  ,node   ,nbrnod ,nodnod ,numnod ,prslot ,psltvr ,
     +         conhis ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,ksi    ,
     +         a1m    ,hstat  ,qstat  ,qlat   ,qlatgr ,strclo ,rfv1   ,
     +         rfv2   ,abcd1  ,abcd2  ,mat    ,rhsvv  ,hp     ,
     +         qp     ,delh   ,work   ,cnstrl ,strhis ,trcnrl ,triger ,
     +         cnpflg ,ker    ,qtyp   ,lfrou  ,strbuf ,ibuf   ,solbuf ,
     +         buflag ,indx   ,bicg   ,stdbq  ,nstdb                  ,
     +         debug_wr)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLOW (FLOW main routine)
c
c Module description: Subroutine FLOW will perform the next tasks:
c
c                     -   Initialisation of water levels and discharges
c                         for every iteration step;
c                     -   Computation of triggers and controllers in the
c                         first iteration step of a new time step;
c                     -   Control of the flow iteration for computation
c                         of the water flow hn+1 and Qn+1.
c
c                     Every first iteration step routine FLCSTR is cal-
c                     led to calculate triggers and controllers. After
c                     this always routine FLOWIT is called to calculate
c                     a new iteration step.
c
c                     Starting from the water flow h(i)n+1 and Q(i)n+1
c                     from the previous iteration level i, subroutine
c                     FLOWIT will compute the water flow h(i+1)n+1 and
c                     Q(i+1)n+1 for the new iteration level i+1.
c
c                     From the control module two delta times are pas-
c                     sed. In case the flow module is calculating unste-
c                     ady delta t2 will have the same value as delta t1.
c                     Delta t2 must be used when calculating the ABCDE
c                     coefficients. In all other cases delta t1 is used.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 98 a1m               P  -
c 29 aft               P  -
c 95 alfab             P  -
c 32 arex              P  -
c 33 arexcn            P  -
c 34 arexop            P  -
c 31 att               P  -
c 37 bfricp            P  -
c 36 bfrict            P  -
c 23 branch            P  -
c 91 conhis            P  -
c 22 contrl            P  -
c 93 cp                P  -
c  2 dt1               P  -
c 45 engpar            P  -
c 68 exres             P  -
c 21 flwpar            I  -
c 46 gangle            P  -
c 80 grid              P  -
c 44 grsize            P  -
c 72 hbdpar            P  -
c 27 hlev              P  -
c 99 hstat             P  -
c  5 istep             P  -
c  4 iter              I  Iteration step.
c  6 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 70 izwft             P  -
c 20 juer              P  -
c 97 ksi               P  -
c 13 lkalm             P  -
c 69 lsalt             P  -
c 25 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 38 maxtab            I  Maximum number of defined tables.
c  7 nbran             I  Number of branches.
c 86 nbrnod            I  Maximum number of connected branches to one
c                         node.
c  9 ncontr            P  -
c 10 ncsrel            I  number of controller structure relations.
c 67 nexres            P  -
c  8 ngrid             I  Number of grid points in network.
c 83 ngridm            I  Maximum number of gridpoints in a branch.
c 71 nhstat            P  -
c 26 nlev              P  -
c 14 nnc               I  Number of uncorrelated random noise processes
c                         for the continity equation.
c 18 nnf               I  Number of uncertain bed friction parameters.
c 15 nnm               I  Number of uncorrelated random noise processes
c                         for the momentum equation.
c 19 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c 16 nnn               I  Number of uncorrelated random noise processes
c                         for nodal (= boundary) equations.
c 84 nnode             I  Number of nodes.
c 17 nns               I  Number of uncorrelated random noise processes
c                         for the Q-H relations of structures.
c 85 node              P  -
c 87 nodnod            P  -
c 78 nqlat             P  -
c 73 nqstat            P  -
c 75 nstru             I  Number of structures.
c 40 ntab              P  -
c 39 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 11 ntcrel            I  Is the number of trigger controller relations.
c 12 ntrigr            I  Number of triggers.
c 88 numnod            P  -
c 35 of                P  -
c 64 pfa               P  -
c 65 pmua              P  -
c 89 prslot            P  -
c 90 psltvr            I  Preissmann slot variables for every grid point
c                         i (assuring positive water depths):
c                         (1,i) = Value for C**2*R for positive flow.
c                         (2,i) = Value for C**2*R for negative flow.
c                         (3,i) = Bottom of slot (funnel)
c                         (4,i) = Division level between trapezium and
c                                 rectangle of slot (top of rectangle
c                                 and bottom of trapezium)
c                         (5,i) = Top of slot
c                         (6,i) = Bottom width of slot (width of
c                                 rectangle)
c                         (7,i) = Top width of slot (top of trapezium)
c 66 pw                P  -
c 74 qbdpar            P  -
c 79 qltpar            P  -
c 82 rho               P  -
c 94 rp                P  -
c 56 scceq             P  -
c 59 scifri            P  -
c 57 scmeq             P  -
c 60 scimu             P  -
c 58 scqhs             P  -
c 63 sclnod            P  -
c 61 scnode            P  -
c 42 sectc             P  -
c 43 sectv             P  -
c 50 snceq             P  -
c 53 snfric            P  -
c 51 snmeq             P  -
c 54 snmu              P  -
c 62 snnode            P  -
c 52 snqhs             P  -
c 55 snwind            P  -
c  3 steady            P  -
c 77 strpar            P  -
c 76 strtyp            P  -
c 41 table             P  -
c 96 tauwi             P  -
c  1 time              P  -
c 24 typcr             P  -
c 92 waoft             P  -
c 48 wfrict            P  -
c 28 wft               P  -
c 47 wndpar            P  -
c 49 wshld             P  -
c 30 wtt               P  -
c 81 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flchkh  FLow CHeck H (Water levels > bottom)
c flcstr  FLow Controlled STRuctures
c flins   FLow Initialise Next Step
c flowit  FLow ITeration step
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flow.pf,v $
c Revision 1.23  1999/03/15  14:21:43  kuipe_j
c Improve writing Froude file
c
c Revision 1.22  1998/12/11  13:06:59  kuipe_j
c improve annotation in dumps
c
c Revision 1.21  1998/06/24  11:10:20  kuipe_j
c Try direct solver if BICGST fails
c
c Revision 1.20  1998/06/08  12:29:36  kuipe_j
c time lag hydr controller
c
c Revision 1.19  1997/11/04  14:08:32  kuipe_j
c dhstru in lat structures,theta set for steady flow
c
c Revision 1.18  1997/05/26  07:41:29  kuipe_j
c dicretization Q(H), H(Q) boundaries improved
c
c Revision 1.17  1997/01/23  08:29:12  kuipe_j
c Make flow module robust
c
c Revision 1.16  1996/11/01  15:04:14  kuipe_j
c Improve contoller messages
c
c Revision 1.15  1996/09/03  14:52:05  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.14  1996/04/12  13:04:09  kuipe_j
c headers, minor changes
c
c Revision 1.13  1996/04/11  08:23:44  kuipe_j
c Kalman module added
c
c Revision 1.12  1996/01/17  14:38:40  kuipe_j
c header update
c
c Revision 1.11  1995/11/21  11:07:57  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.10  1995/10/18  10:49:55  hoeks_a
c Some small changes
c
c Revision 1.9  1995/10/18  08:59:23  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.8  1995/10/11  12:24:06  kuipe_j
c Remove aux output temp
c
c Revision 1.7  1995/09/22  10:01:58  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:10:58  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:36:44  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:24  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:55:15  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:15  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:57  hoeks_a
c Initial check-in
c
c Revision 1.4  1994/12/02  13:30:39  kuipe_j
c Improved message handling
c
c Revision 1.3  1994/11/28  08:37:40  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:17  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:52  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
      include '../include/sobcon.i'
c
c     Declaration of parameters:
c
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
c
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
c
      double precision mat(nnode,nnode), rhsvv(nnode)
      double precision abcd1(ngridm,5) , abcd2(ngridm,5)
      double precision rfv1(ngrid,3)   , rfv2(ngrid,3)
      double precision delh(nnode)     , work(nnode,*)
      double precision time ,dt1, hlev(ngrid,maxlev)
      double precision hp(ngrid,3), qp(ngrid,3)
c
      logical lkalm ,lsalt, steady, bicg
      logical strclo(*)
      logical lfrou
c     mozart declarations
      character*40  qlatid(*), qlatnm(*)
      logical       lmoza, lgrwt ,lrest
      integer       nstmoz
c
c     Declaration of local variables
c
      integer ibuf1,ibuf2
      integer iterbc ,exrstp ,istru, igrid
      real    theta2 ,omalfa ,omc    ,omr    ,omw    ,omboun ,omqlat
      real    g      ,psi    ,theta  ,rhow   ,omega  ,lambda ,relstr
      real    dhstru ,cflpse ,overlp ,omcfl  ,dhtyp  ,urelax
      double precision        resid
      character*18            txt
c
c     FM1DIMP2DO: remove debug
      real dbg1
      integer debug_wr
c
c     Include sobek error code file
c
      include '../include/errcod.i'
c
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
c
c     omega instead of urelax, urelax dummy now
c
      urelax = 1.
c
c     Reduction of numerical parameters for the user
c
      omalfa = omega
      omc    = omega
      omr    = omega
      omw    = omega
      omboun = omega
      omqlat = omega
      theta2 = theta
      
c     FM1DIMP2DO: remove debug
      dbg1=hp(1,1)
      if (debug_wr>0) then
      write(42,*) 'in FLOW'
      write(42,*) 'h1'
      write(42,*) hp(:,1)
      write(42,*) 'h2'
      write(42,*) hp(:,2)
      write(42,*) 'h3'
      write(42,*) hp(:,3)
      write(42,*) 'q1'
      write(42,*) qp(:,1)
      write(42,*) 'q2'
      write(42,*) qp(:,2)
      write(42,*) 'q3'
      write(42,*) qp(:,3)
      endif
c
c     Put last computed approximations of new h and q in positions *
c                                              <h_n>    <h_*>
      call FLINS (steady    ,iter    ,ngrid   ,hp(1,1) ,hp(1,2)    ,
c                 <h_n+1>    <Q_n>    <Q_*>    <Q_n+1>  <At_n>
     +            hp(1,3)   ,qp(1,1) ,qp(1,2) ,qp(1,3) ,waoft(1,5) ,
c                 <At_n+1>
     +            waoft(1,4),qtyp)
      
c     FM1DIMP2DO: remove debug
      dbg1=hp(1,1)
      if (debug_wr>0) then
      write(42,*) 'FLINS'
      write(42,*) 'h1'
      write(42,*) hp(:,1)
      write(42,*) 'h2'
      write(42,*) hp(:,2)
      write(42,*) 'h3'
      write(42,*) hp(:,3)
      write(42,*) 'q1'
      write(42,*) qp(:,1)
      write(42,*) 'q2'
      write(42,*) qp(:,2)
      write(42,*) 'q3'
      write(42,*) qp(:,3)
      endif
c
c
c Neglect structures for now
c
c      if ( iter .eq. 1 .or. steady ) then
c
c      call FLCSTR (time   ,dt1     ,istep   ,g          ,rho    ,
c     +                ncontr ,contrl  ,strtyp  ,strpar     ,ngrid  ,
cc                      <h_*>    <Q_*>    <Af>
c     +                hp(1,2) ,qp(1,2) ,waoft(1,3) ,maxtab ,ntabm  ,
c     +                ntab    ,table   ,ncsrel ,cnstrl     ,ntrigr ,
c     +                triger  ,ntcrel  ,trcnrl ,nstru      ,strhis ,
c     +                conhis  ,cnpflg  ,lagstm ,nlags      ,buflag ,
c     +                nqlat   ,qltpar  ,juer   ,lrest      ,ker    )
c
c      endif

      if (ker .eq. fatal) goto 1000
c
      call FLOWIT (g      ,istep  ,time   ,dt1    ,steady ,iter   ,
     +     psi    ,theta  ,urelax ,nbran  ,ngrid  ,nnc    ,nnf    ,
     +     nnm    ,nns    ,nnmu   ,nnn    ,nosdim ,maxlev ,nlev   ,
     +     lkalm  ,lsalt  ,overlp ,maxtab ,ntabm  ,rhow   ,juer   ,
c          mozart parameters plus groundwater switch
     +     lmoza  ,nstmoz ,qlatid ,qlatnm ,lgrwt  ,
     +     hlev   ,wft    ,aft    ,wtt    ,att    ,arex   ,arexcn ,
     +     arexop ,of     ,bfrict ,bfricp ,ntab   ,table  ,sectc  ,
     +     sectv  ,grsize ,engpar ,gangle ,wndpar ,wfrict ,wshld  ,
     +     snceq  ,snmeq  ,snqhs  ,snfric ,snmu   ,snwind ,sclceq ,
     +     sclmeq ,sclqhs ,scceq  ,scmeq  ,scqhs  ,scifri ,scimu  ,
     +     scnode ,snnode ,sclnod ,pfa    ,pmua   ,pw     ,nexres ,
     +     exres  ,izwft  ,nhstat ,hbdpar ,nqstat ,qbdpar ,nstru  ,
     +     strtyp ,strpar ,nqlat  ,qltpar ,grid   ,x      ,grhis  ,
     +     rho    ,
     +     ngridm ,branch ,typcr  ,nnode  ,node   ,nbrnod ,nodnod ,
     +     numnod ,prslot ,psltvr ,waoft  ,cp     ,rp     ,alfab  ,
     +     tauwi  ,ksi    ,a1m    ,hstat  ,qstat  ,qlat   ,qlatgr ,
     +     strhis ,strclo ,rfv1   ,rfv2   ,abcd1  ,abcd2  ,mat    ,
     +     rhsvv  ,hp     ,qp     ,theta2 ,exrstp ,omalfa ,omc    ,
     +     omr    ,omw    ,cflpse ,iterbc ,resid  ,delh   ,work   ,
     +     lambda ,relstr ,dhstru ,omcfl  ,dhtyp  ,ker    ,omboun ,
     +     omqlat ,ibuf   ,lfrou  ,qtyp   ,indx   ,bicg   ,solbuf ,
     +     stdbq  ,nstdb                                          ,
     +     debug_wr)
c
      if (ker .eq. fatal) goto 1000
cc
cc Check done in FM
cc
c      call FLCHKH (ngrid  ,nbran  ,branch ,typcr  ,maxlev ,hlev   ,
c     +             hp(1,3),juer   ,ker    ,prslot ,psltvr )
cc
cc     Fill buffers
cc
c      ibuf1 = ibuf(1)
c      do 1010 istru = 1 , nstru
c         strbuf(ibuf1,1,istru) = strhis(4,istru)
c         if (strtyp(2,istru) .eq. cstbra) then
c            strbuf(ibuf1,2,istru) = hp(strtyp(3,istru),3)-
c     +                              hp(strtyp(4,istru),3)
c         else
c            strbuf(ibuf1,2,istru) = -999.
c         endif
 1010 continue
cc      ibuf2 = ibuf(2)
cc      do 1020 igrid = 1 , ngrid
cc         solbuf(ibuf2,1,igrid) = hp(igrid,3)
cc         solbuf(ibuf2,2,igrid) = qp(igrid,3)
cc         solbuf(ibuf2,3,igrid) = hp(igrid,3)-psltvr(5,igrid)
cc         solbuf(ibuf2,4,igrid) = waoft(igrid,1)
cc         solbuf(ibuf2,5,igrid) = waoft(igrid,2)
cc         solbuf(ibuf2,6,igrid) = 1/waoft(igrid,8)
 1020 continue
cc
cc        Set minus sign to define that buffer data has been stored
c         ibuf(1) = -ibuf(1)
 1000 continue

c      if (ker .ne. ok) then
c         write (txt,'(2(1x,i8))') itim
c         call sre_error (juer,'FLOW timestep@'//txt//'@',eflmes,info)
c         if (ker .ne. fatal) ker = ok
c      endif
c
      end
