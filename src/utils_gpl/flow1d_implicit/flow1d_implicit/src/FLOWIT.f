      subroutine FLOWIT(g      ,istep  ,time   ,dt1    ,steady ,iter   ,
     +          psi    ,theta  ,urelax ,nbran  ,ngrid  ,nnc    ,nnf    ,
     +          nnm    ,nns    ,nnmu   ,nnn    ,nosdim ,maxlev ,nlev   ,
     +          lkalm  ,lsalt  ,overlp ,maxtab ,ntabm  ,rhow   ,juer   ,
c               mozart parameters plus groundwater switch
     +          lmoza  ,nstmoz ,qlatid ,qlatnm ,lgrwt  ,
     +          hlev   ,wft    ,aft    ,wtt    ,att    ,arex   ,arexcn ,
     +          arexop ,of     ,bfrict ,bfricp ,ntab   ,table  ,sectc  ,
     +          sectv  ,grsize ,engpar ,gangle ,wndpar ,wfrict ,wshld  ,
     +          snceq  ,snmeq  ,snqhs  ,snfric ,snmu   ,snwind ,sclceq ,
     +          sclmeq ,sclqhs ,scceq  ,scmeq  ,scqhs  ,scifri ,scimu  ,
     +          scnode ,snnode ,sclnod ,pfa    ,pmua   ,pw     ,nexres ,
     +          exres  ,izwft  ,nhstat ,hbdpar ,nqstat ,qbdpar ,nstru  ,
     +          strtyp ,strpar ,nqlat  ,qltpar ,grid   ,x      ,grhis  ,
     +          rho    ,
     +          ngridm ,branch ,typcr  ,nnode  ,node   ,nbrnod ,nodnod ,
     +          numnod ,prslot ,psltvr ,waoft  ,cpa    ,rpa    ,alfab  ,
     +          tauwi  ,ksi    ,a1m    ,hstat  ,qstat  ,qlat   ,qlatgr ,
     +          strhis ,strclo ,rfv1   ,rfv2   ,abcd1  ,abcd2  ,mat    ,
     +          rhsvv  ,hp     ,qp     ,theta2 ,exrstp ,omalfa ,omc    ,
     +          omr    ,omw    ,cflpse ,iterbc ,resid  ,delh   ,work   ,
     +          lambda ,relstr ,dhstru ,omcfl  ,dhtyp  ,ker    ,omboun ,
     +          omqlat ,ibuf   ,lfrou  ,qtyp   ,indx   ,bicg   ,solbuf ,
     +          stdbq  ,nstdb                                          ,
     +          debug_wr)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLOWIT (FLow ITeration step)
c
c Module description: Subroutine FLOWIT computes h(i)n+1 and Q(i)n+1 ,
c                     for a certain iteration level i.
c
c                     First the water level dependent hydrodynamic para-
c                     meters are calculated by calling routine FLHYPA.
c
c                     In subroutine FLBOUN the boundary conditions will
c                     be determined at the new time level tn+1.
c
c                     In subroutine FLQLAT the lateral discharges will
c                     be computed in the user selected discharge stati-
c                     ons at the new time level tn+1/2. Notice that the
c                     Preissmann scheme requires boundary conditions and
c                     lateral discharges at different time levels.
c
c                     After this the ABCDE coefficients are calculated
c                     in routine FLDSCO. This routine delivers the re-
c                     sulting f, r and v result arrays which are used by
c                     routine FLSOEQ. See functional design for the
c                     water flow module for a description of the double
c                     sweep method.
c
c                     Subroutine FLSOEQ computes a nodal administration
c                     matrix and solves this matrix. The results are
c                     used to calculate the flow in the branches.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 96 a1m               P  -
c 29 aft               P  -
c 93 alfab             P  -
c 32 arex              P  -
c 33 arexcn            P  -
c 34 arexop            P  -
c 31 att               P  -
c 37 bfricp            P  -
c 36 bfrict            P  -
c 81 branch            P  -
c 91 cpa               P  -
c  4 dt1               P  -
c 43 engpar            P  -
c 66 exres             P  -
c  1 g                 P  -
c 44 gangle            P  -
c 77 grid              P  -
c 42 grsize            P  -
c 69 hbdpar            P  -
c 27 hlev              P  -
c 97 hstat             P  -
c  2 istep             P  -
c  6 iter              I  Iteration step.
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 67 izwft             P  -
c 26 juer              P  -
c 95 ksi               P  -
c 20 lkalm             I  -
c 21 lsalt             P  -
c 18 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 23 maxtab            I  Maximum number of defined tables.
c 10 nbran             I  Number of branches.
c 85 nbrnod            I  Maximum number of connected branches to one
c                         node.
c 65 nexres            P  -
c 11 ngrid             I  Number of grid points in network.
c 80 ngridm            I  Maximum number of gridpoints in a branch.
c 68 nhstat            P  -
c 19 nlev              P  -
c 12 nnc               I  Number of uncorrelated random noise processes
c                         for the continity equation.
c 13 nnf               I  Number of uncertain bed friction parameters.
c 14 nnm               I  Number of uncorrelated random noise processes
c                         for the momentum equation.
c 16 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c 17 nnn               I  Number of uncorrelated random noise processes
c                         for nodal (= boundary) equations.
c 83 nnode             I  Number of nodes.
c 15 nns               I  Number of uncorrelated random noise processes
c                         for the Q-H relations of structures.
c 84 node              P  -
c 86 nodnod            P  -
c 75 nqlat             P  -
c 70 nqstat            P  -
c 72 nstru             I  Number of structures.
c 38 ntab              P  -
c 24 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 87 numnod            P  -
c 35 of                P  -
c 22 overlp            P  -
c 62 pfa               P  -
c 63 pmua              P  -
c 88 prslot            P  -
c  7 psi               P  -
c 89 psltvr            P  -
c 64 pw                P  -
c 71 qbdpar            P  -
c 99 qlat              P  -
c 76 qltpar            P  -
c 98 qstat             P  -
c 79 rho               P  -
c 25 rhow              P  -
c 92 rpa               P  -
c 54 scceq             P  -
c 57 scifri            P  -
c 55 scmeq             P  -
c 58 scimu             P  -
c 56 scqhs             P  -
c 61 sclnod            P  -
c 59 scnode            P  -
c 40 sectc             P  -
c 41 sectv             P  -
c 48 snceq             P  -
c 51 snfric            P  -
c 49 snmeq             P  -
c 52 snmu              P  -
c 60 snnode            P  -
c 50 snqhs             P  -
c 53 snwind            P  -
c  5 steady            P  -
c 74 strpar            P  -
c 73 strtyp            P  -
c 39 table             P  -
c 94 tauwi             P  -
c  8 theta             P  -
c  3 time              P  -
c 82 typcr             P  -
c  9 urelax            P  -
c 90 waoft             P  -
c 46 wfrict            P  -
c 28 wft               P  -
c 45 wndpar            P  -
c 47 wshld             P  -
c 30 wtt               P  -
c 78 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flboun  FLow BOUNdary conditions
c fldsco  FLow Double Sweep COefficients
c flhypa  FLow HYdraulic PArameters
c flkapa  FLow KAlman correction PArameter
c flqlat  FLow Q LATeral
c flsoeq  FLow SOlve EQuations
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flowit.pf,v $
c Revision 1.19  1999/03/15  14:21:45  kuipe_j
c Improve writing Froude file
c
c Revision 1.18  1998/06/24  11:10:22  kuipe_j
c Try direct solver if BICGST fails
c
c Revision 1.17  1998/05/25  19:12:08  kuipe_j
c Wendy structures
c
c Revision 1.16  1997/11/04  14:17:25  kuipe_j
c Retention basin
c
c Revision 1.15  1997/05/26  07:41:31  kuipe_j
c dicretization Q(H), H(Q) boundaries improved
c
c Revision 1.14  1997/01/23  08:29:14  kuipe_j
c Make flow module robust
c
c Revision 1.13  1996/11/01  15:04:16  kuipe_j
c Improve contoller messages
c
c Revision 1.12  1996/10/31  10:30:22  kuipe_j
c Extra resistance finished
c
c Revision 1.11  1996/09/03  14:52:06  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.10  1996/04/12  13:04:11  kuipe_j
c headers, minor changes
c
c Revision 1.9  1996/04/11  08:23:45  kuipe_j
c Kalman module added
c
c Revision 1.8  1995/11/21  11:07:58  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic pseudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.7  1995/09/22  10:02:02  kuipe_j
c variable dimensions, new headers
c
c Revision 1.6  1995/09/12  08:10:59  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.5  1995/08/30  12:36:45  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:25  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:55:16  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:16  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:57  hoeks_a
c Initial check-in
c
c Revision 1.4  1994/12/02  13:19:33  kuipe_j
c Prevention against devide by zero.
c
c Revision 1.3  1994/11/28  08:37:42  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:18  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:53  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
c      !DEC$ IF DEFINED (_DLL)
c      use SobekRE_OpenMI
c      !DEC$ ENDIF

      include '../include/sobdim.i'
c      include '../include/mempool.i'
      
      integer    ok
      integer    info
      integer    warnng
      integer    fatal
      
      parameter (ok     =     0,       
     +           info   =     1,       
     +           warnng =     2,       
     +           fatal  =     3)

c
c     Declaration of parameters:
c
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
c
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
c      
      double precision time ,dt1 ,resid, hlev(ngrid,maxlev)
      double precision mat(nnode,nnode), rhsvv(nnode)
      double precision abcd1(ngridm,5) , abcd2(ngridm,5)
      double precision rfv1(ngrid,3)   , rfv2(ngrid,3)
      double precision delh(nnode),work(nnode,*)
      double precision hp(ngrid,3), qp(ngrid,3)
c
      logical lkalm, lsalt, steady, bicg
      logical strclo(*)
      logical lfrou
c     mozart declarations
      character(len=40) qlatid(*), qlatnm(*)
      logical       lmoza, lgrwt
      integer       nstmoz
c 
      double precision dbg1
      real dbg2
      integer debug_wr
c
c      !DEC$ IF DEFINED (_DLL)

c      if (initdone .and. OpenMIactive()) then
c         ! Fill structure changes
c
c         strunm   =    gtcpnt('STRUNM')         
c
c         call GetStruct(strtyp, cp(strunm), strpar, nstru)
c      endif
c      !DEC$ ENDIF
c
c     Calculate water level dependent hydraulic parameters
c
c     Debug variables FM1DIMP2DO: remove
      dbg2=waoft(100,3)
     
c      write(42,*) 'in FLOWIT'
c      write(42,*) 'h1'
c      write(42,*) hp(:,1)
c      write(42,*) 'h2'
c      write(42,*) hp(:,2)
c      write(42,*) 'h3'
c      write(42,*) hp(:,3)
      
      call FLHYPA(time    ,iter    ,nbran   ,ngrid   ,branch ,typcr  ,
c                 <h_n>    <h_*>
     +            hp(1,2) ,hp(1,1) ,maxlev  ,nlev    ,hlev   ,wft    ,
     +            aft     ,wtt     ,att     ,overlp  ,arex   ,arexcn ,
c                                                    <Q_n>    <Q_*>
     +            arexop , of      ,bfrict  ,bfricp ,qp(1,1) ,qp(1,2),
     +            maxtab  ,ntabm   ,ntab    ,table  ,sectc   ,sectv  ,
     +            grsize  ,engpar  ,gangle  ,wndpar ,wfrict  ,wshld  ,
     +            x       ,nexres  ,exres   ,lsalt  ,izwft   ,juer   ,
     +            prslot  ,psltvr  ,waoft   ,cpa    ,rpa     ,alfab  ,
     +            tauwi   ,ksi     ,a1m     ,ker    ,dt1     ,theta2 ,
     +            exrstp  ,omalfa  ,omc     ,omr    ,omw    )
c
c     Debug variables FM1DIMP2DO: remove
      dbg2=waoft(100,3)      
      if (debug_wr>0) then
      write(42,*) 'FLYPA'
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
      
      if (ker .eq. fatal) goto 1000
c
c
c     Implement effect of correction parameters.
c
c      if ( lkalm ) then
c         call FLKAPA (istep  ,ngrid  ,nnf    ,nstru  ,nnmu   ,nbran  ,
c     +                cpa(1,1),scifri ,pfa   ,strpar ,scimu  ,pmua   ,
c     +                branch ,wfrict ,tauwi  ,pw     )
c      endif

c     Determine boundary conditions at the new time level t(n+1)
c     Only the conditions which are defined as a function of time
c
      call FLBOUN(time   ,
     +            maxtab ,ntabm  ,ntab   ,table   ,
     +            nhstat ,hstat  ,hbdpar ,
     +            nqstat ,qstat  ,qbdpar ,omboun  ,iter)
c
c     Compute lateral discharges in the user selected discharge stations
c     at the new time level t(n+1/2)
c
      call FLQLAT(g       ,time   ,ngrid  ,lambda ,
c                         <h_n>    <h_*>
     +            x       ,hp(1,1),hp(1,2),
     +            strtyp  ,strpar ,maxtab ,ntabm  ,ntab    ,
c     mozart parameters
     +            lmoza   ,nstmoz ,qlatid ,
     +            table   ,nqlat   ,qltpar ,juer   ,qlat   ,qlatgr ,
     +            strclo  ,strhis  ,theta2 ,dt1    ,ker    ,omqlat ,
     +            dhstru  ,relstr  ,iter)
      
      if (debug_wr>0) then
      write(42,*) 'FLQLAT'
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
      
      if (ker .eq. fatal) goto 1000

c      if (lgrwt) then
cc
cc     Compute lateral discharge caused by flux with groundwater
cc
c         mugr   =     gtrpnt ( 'MU' )
c         kdgr   =     gtrpnt ( 'KD' )
c         grcgr  =     gtrpnt ( 'GRC' )
c         grwpar =     gtrpnt ( 'GRWPAR' )
c         hbal   =     gtrpnt ( 'HBAL' ) 
c
c         grdh = sorpar(rp(grwpar),2)
c         plrec = soipar(rp(grwpar),3)
c         pllrec = soipar(rp(grwpar),4)
c         plave = soipar(rp(grwpar),5)
c         plold = soipar(rp(grwpar),6)
c         pbal = soipar(rp(grwpar),7)
c
c         call FLQLGR(iter,istep,dt1,ngrid,rp(mugr),rp(kdgr),
c     +                  rp(grcgr),grhis, qlatgr,
c     +                  x,  hp, grdh, plrec, pllrec, plave,
c     +                  plold,pbal,rp(hbal),juer,ker)
c
c      endif
c
c     Calculate ABCDE coefficients
c
      call FLDSCO(g      ,iter   ,dt1    ,steady ,psi    ,theta  ,
     +            exrstp ,nbran  ,branch ,ngrid  ,lambda ,relstr ,
     +            dhstru ,hp     ,qp     ,qlatgr ,grid   ,
     +            x      ,waoft  ,cpa    ,rpa    ,alfab  ,tauwi  ,
     +            ksi    ,lsalt  ,rho    ,rhow   ,a1m    ,nstru  ,
     +            strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,
     +            ngridm ,strclo ,strhis ,abcd1  ,abcd2  ,rfv1   ,
     +            rfv2   ,theta2 ,omr    ,cflpse ,omcfl  ,dhtyp  ,
     +            ibuf   ,istep  ,lfrou  ,qlat   ,qltpar ,nqlat  ,
     +            hlev   ,maxlev ,solbuf ,stdbq  ,nstdb  ,juer   ,
     +            ker    )
      
      if (debug_wr>0) then
      write(42,*) 'FLDSCO'
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
c     Compute nodal administration matrix and solve this matrix
c
      call FLSOEQ(lkalm  ,ngrid  ,nstru  ,nnc    ,nnm    ,nns   ,
     +            nnf    ,nnmu   ,nnn    ,nosdim ,
     +            qtyp   ,qlatnm ,sclceq ,sclmeq ,sclqhs ,
     +            scceq  ,scmeq  ,scqhs  ,scifri ,scimu  ,snceq ,
     +            snmeq  ,snqhs  ,snfric ,snmu   ,snwind ,strtyp,
     +            wfrict ,scnode ,snnode ,sclnod ,nnode  ,node  ,
     +            nbrnod ,nodnod ,numnod ,nbran  ,branch ,maxtab,
     +            ntabm  ,ntab   ,table  ,hstat  ,hbdpar ,qstat ,
     +            qbdpar ,urelax ,rfv1   ,rfv2   ,mat    ,rhsvv ,
     +            hp     ,qp     ,iterbc ,resid  ,delh   ,work  ,
     +            ker    ,steady ,nqlat  ,qlat   ,qltpar ,strhis,
     +            relstr ,theta  ,dt1    ,indx   ,juer   ,bicg  ,
     +            debug_wr)
      
      dbg1=hp(1,1)
      
      if (debug_wr>0) then
      write(42,*) 'FLSOEQ'
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
c     Exception
c
 1000 continue

      end
