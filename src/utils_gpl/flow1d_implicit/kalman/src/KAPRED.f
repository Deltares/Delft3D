      subroutine KAPRED(nbran  ,ngrid  ,branch ,typcr  ,maxlev ,nlev   ,
     +                  hlev   ,wft    ,aft    ,of     ,bfrict ,bfricp ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,
     +                  grsize ,engpar ,nexres ,exres  ,juer   ,prslot ,
     +                  cp     ,rp     ,alfab  ,deriva ,
     +                  g      ,time   ,strtyp ,strpar ,nqlat  ,qltpar ,
     +                  qlat   ,strclo ,qlatac ,np     ,p1     ,p2     ,
     +                  pcol   ,indx   ,pfa    ,pmua   ,pw     ,dt1    ,
     +                  psi    ,theta  ,hp     ,qp     ,grid   ,x      ,
     +                  waoft  ,tauwi  ,lsalt  ,rho    ,rhow   ,nstru  ,
     +                  ngridm ,nnode  ,node   ,nbrnod ,brnode ,hbdpar ,
     +                  qbdpar ,af2    ,wf2    ,abcd1  ,kabcd1 ,kabcd2 ,
     +                  scnode ,snnode ,nnn    ,sclnod ,wfrict ,rfv1   ,
     +                  rfv2   ,kbeta  ,mat    ,nnc    ,
     +                  nnm    ,nns    ,nnf    ,nnmu   ,nosdim ,
     +                  scifri ,scimu  ,snceq  ,snmeq  ,snqhs  ,
     +                  snfric ,snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,
     +                  sclfri ,sclmu  ,scceq  ,scmeq  ,scqhs  ,scfric ,
     +                  scmu   ,rhs    ,lambda ,dhstru ,theta2 ,overlp ,
     +                  arex   ,arexcn ,arexop ,ker    ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAPRED (KAlman PREDiction step)
c
c Module description: Subroutine KAPRED computes the covariance P(n+1|n)
c                     for a time step.
c
c                     Starting from the covariance P(n|n) from time step
c                     n, subroutine KAPRED computes the predicted P(n+1|n)
c                     for the next time step.
c
c                     First a.o. the derivatives of water level dependent
c                     hydrodynamic parameters are calculated by calling
c                     routine KAHYPA. Then subroutine KADQLT computes the
c                     derivative of the lateral discharges.  After that
c                     the prediction is done by subroutine KAPRNT.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 68 abcd1             P  -
c 66 af2               P  -
c  9 aft               P  -
c 27 alfab             P  -
c 12 bfricp            P  -
c 11 bfrict            P  -
c  3 branch            P  -
c 63 brnode            P  -
c 25 cp                P  -
c 28 deriva            P  -
c 46 dt1               P  -
c 20 engpar            P  -
c 22 exres             P  -
c 29 g                 P  -
c 51 grid              P  -
c 19 grsize            P  -
c 64 hbdpar            P  -
c  7 hlev              P  -
c 49 hp                P  -
c 42 indx              P  -
c 23 juer              P  -
c 69 kabcd1            P  -
c 70 kabcd2            P  -
c 78 kbeta             P  -
c 55 lsalt             P  -
c 79 mat               P  -
c  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 13 maxtab            I  Maximum number of defined tables.
c  1 nbran             I  Number of branches.
c 62 nbrnod            I  Maximum number of connected branches to one
c                         node.
c 21 nexres            P  -
c  2 ngrid             I  Number of grid points in network.
c 59 ngridm            I  Maximum number of gridpoints in a branch.
c  6 nlev              P  -
c 80 nnc               I  Number of uncorrelated random noise processes
c                         for the continity equation.
c 83 nnf               I  Number of uncertain bed friction parameters.
c 81 nnm               I  Number of uncorrelated random noise processes
c                         for the momentum equation.
c 84 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c 73 nnn               I  Number of uncorrelated random noise processes
c                         for nodal (= boundary) equations.
c 60 nnode             I  Number of nodes.
c 82 nns               I  Number of uncorrelated random noise processes
c                         for the Q-H relations of structures.
c 61 node              P  -
c 38 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 33 nqlat             P  -
c 58 nstru             I  Number of structures.
c 15 ntab              P  -
c 14 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 10 of                P  -
c 39 p1                P  -
c 40 p2                P  -
c 41 pcol              P  -
c 43 pfa               P  -
c 44 pmua              P  -
c 24 prslot            P  -
c 47 psi               P  -
c    psltvr            P  -
c 45 pw                P  -
c 65 qbdpar            P  -
c 35 qlat              P  -
c 37 qlatac            P  -
c 34 qltpar            P  -
c 50 qp                P  -
c 76 rfv1              P  -
c 77 rfv2              P  -
c 56 rho               P  -
c 57 rhow              P  -
c 26 rp                P  -
c 87 scifri            P  -
c 88 scimu             P  -
c 95 sclceq            P  -
c 98 sclfri            P  -
c 96 sclmeq            P  -
c 99 sclmu             P  -
c 74 sclnod            P  -
c 97 sclqhs            P  -
c 71 scnode            P  -
c 17 sectc             P  -
c 18 sectv             P  -
c 89 snceq             P  -
c 92 snfric            P  -
c 90 snmeq             P  -
c 93 snmu              P  -
c 72 snnode            P  -
c 91 snqhs             P  -
c 94 snwind            P  -
c 36 strclo            P  -
c 32 strpar            P  -
c 31 strtyp            P  -
c 16 table             P  -
c 54 tauwi             P  -
c 48 theta             P  -
c 30 time              P  -
c  4 typcr             P  -
c 53 waoft             P  -
c 67 wf2               P  -
c 75 wfrict            P  -
c  8 wft               P  -
c 52 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c kadqlt  KAlman Derivative of Q LaTeral
c kahypa  KAlman HYdraulic PArameters
c kaprnt  KAlman PRedict Next Time step
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kapred.pf,v $
c Revision 1.5  1997/06/17  11:23:48  kuipe_j
c Initialize vars
c
c Revision 1.4  1997/01/23  08:29:42  kuipe_j
c Make flow module robust
c
c Revision 1.3  1996/11/05  13:48:07  kuipe_j
c Error in declaration hlev fixed
c
c Revision 1.2  1996/04/12  13:05:16  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:49  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
      include '..\include\sobdim.i'
c
c     Declaration of Parameters:
c
      integer nbran, ngrid, maxlev, nexres, juer, ker
      integer ngridm, nstru, maxtab, ntabm, nosdim
      integer nnode, nnn ,nbrnod,
     +        branch(4,nbran), bfrict(3,nbran),
     +        typcr(nbran),
     +        ntab(4,maxtab),
     +        nlev(ngrid),
     +        arexcn(ngrid,2), arexop(2)
      integer grid(ngrid)
      integer strtyp(10,*)
      integer hbdpar(3,*), qbdpar(3,*)
      integer node(4,nnode), indx(nnode)
      integer wfrict(3,nbran)
      integer sclnod(nnn+1), scnode(*), brnode(nbrnod+1,nnode)
      integer nnc, nnm, nns, nnf, nnmu, np
      integer scifri(ngrid), scimu(nstru)
      integer sclceq(nnc+1), sclqhs(nns+1), sclfri(nnf+1),
     +        sclmu(nnmu+1), sclmeq(nnm+1)
      integer scceq(*), scmeq(*), scqhs(*),
     +        scfric(ngrid), scmu(nstru)
c
      logical lsalt, strclo(*)
c
      real    table(ntabm),
     +        wft(ngrid,maxlev), aft(ngrid,maxlev),
     +        of (ngrid,maxlev),
     +        waoft(ngrid,dmwaof),
     +        cp(ngrid,4), rp(ngrid,4), alfab(ngrid),
     +        bfricp(6,ngrid),
     +        grsize(4,ngrid,*), engpar(9), exres(3,*),
     +        sectc(ngrid,3), sectv(ngrid,dmsecv),
     +        x(ngrid), arex(ngrid,4),
     +        prslot(3,nbran), psltvr(7,ngrid)
c
      real    g, psi, theta, rhow, pw
      real    lambda, theta2, overlp, dhstru
      real    tauwi(ngrid), rho(ngrid)
      real    strpar(dmstrpar,*)
      real    qltpar(9,*), af2(ngrid), wf2(ngrid)
      real    pfa(nnf), pmua(nnmu)
      real    deriva(ngrid,11)
      real    snnode(nosdim,nnn)
      real    snceq(nosdim,nnc), snmeq(nosdim,nnm), snqhs(nosdim,nns),
     +        snfric(2,nnf), snmu(2,nnmu), snwind(2)
      real    p1(np,np), p2(np,np), pcol(np)
c
      double precision dt1, hlev(ngrid,maxlev),hp(ngrid,3), qp(ngrid,3)
      double precision abcd1(ngridm,5)
      double precision kabcd1(ngrid,8), kabcd2(ngrid,14)
      double precision rfv1(ngrid,3), rfv2(ngrid,3)
      double precision mat(nnode,nnode), rhs(nnode)
      double precision kbeta(2,nbran)
c
      integer nqlat
      real    qlat(*), qlatac(*)
      double  precision  time
c
c                                                           <h_n>
      call KAHYPA (nbran  ,ngrid  ,nnf    ,branch ,typcr  ,hp(1,1),
c                   <h_*>   <h_n+1>
     +             hp(1,2),hp(1,3),maxlev ,nlev   ,hlev   ,wft    ,
c                                                   <Q_n>   <Q_*>
     +             aft    ,of     ,bfrict ,bfricp ,qp(1,1),qp(1,2),
     +             maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,
     +             grsize ,engpar ,x      ,nexres ,exres  ,juer   ,
     +             prslot ,waoft  ,af2    ,wf2    ,grid   ,cp     ,
     +             rp     ,alfab  ,scifri ,pfa    ,
c                   <afacc>         <oacc>          <dwfdh>
     +             deriva(1,10)   ,deriva(1,11)   ,deriva(1, 5)   ,
c                   <dcdh>          <drdh>          <dalfdh>
     +             deriva(1, 1)   ,deriva(1, 3)   ,deriva(1, 4)   ,
c                   <dcdq>          <eta>           <detadh>
     +             deriva(1, 2)   ,deriva(1, 8)   ,deriva(1, 6)   ,
     +             theta2 ,overlp ,arex   ,arexcn ,arexop ,   ker ,
     +             psltvr )
c
c                                                  <h_n>   <h_*>
      call KADQLT (g      ,time   ,ngrid  ,x      ,hp(1,1),hp(1,2),
     +             strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,
     +             nqlat  ,qltpar ,juer   ,lambda ,theta2 ,dt1    ,
c                                         <dqltdh>
     +             qlat   ,strclo ,qlatac ,deriva(1, 7)   ,ker    )
c
      call KAPRNT (g      ,dt1    ,psi    ,theta  ,nbran  ,branch ,
     +             ngrid  ,lambda ,dhstru ,hp     ,qp     ,grid   ,
     +             x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,
     +             lsalt  ,rho    ,rhow   ,nstru  ,nosdim ,
     +             strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,
     +             ngridm ,prslot ,strclo ,nnode  ,
     +             node   ,nbrnod ,brnode ,hbdpar ,qbdpar ,juer   ,
c                  <eta>          <dqltdh>        <dalfdh>
     +             deriva(1, 8)   ,deriva(1, 7)   ,deriva(1, 4)   ,
c                  <dcdh>         <drdh>          <dcdq>
     +             deriva(1, 1)   ,deriva(1, 3)   ,deriva(1, 2)   ,
c                  <dwfdh>        <detadh>
     +             deriva(1, 5)   ,deriva(1, 6)   ,
     +             af2    ,wf2    ,pw     ,pfa    ,pmua   ,abcd1  ,
     +             kabcd1 ,kabcd2 ,indx   ,scnode ,snnode ,nnn    ,
     +             sclnod ,wfrict ,rfv1   ,rfv2   ,kbeta  ,mat    ,
     +             nnc    ,nnm    ,nns    ,nnf    ,nnmu   ,scifri ,
     +             scimu  ,snceq  ,snmeq  ,snqhs  ,snfric ,snmu   ,
     +             snwind ,sclceq ,sclmeq ,sclqhs ,sclfri ,sclmu  ,
     +             scceq  ,scmeq  ,scqhs  ,scfric ,scmu   ,
     +             rhs    ,np     ,p1     ,p2     ,pcol   ,ker    ,
     +             psltvr )
c
      end
