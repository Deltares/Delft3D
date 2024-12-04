subroutine KAPRED(nbran  ,ngrid  ,branch ,typcr  ,maxlev ,nlev   ,&
&hlev   ,wft    ,aft    ,of     ,bfrict ,bfricp ,&
&maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,&
&grsize ,engpar ,nexres ,exres  ,juer   ,prslot ,&
&cp     ,rp     ,alfab  ,deriva ,&
&g      ,time   ,strtyp ,strpar ,nqlat  ,qltpar ,&
&qlat   ,strclo ,qlatac ,np     ,p1     ,p2     ,&
&pcol   ,indx   ,pfa    ,pmua   ,pw     ,dt1    ,&
&psi    ,theta  ,hp     ,qp     ,grid   ,x      ,&
&waoft  ,tauwi  ,lsalt  ,rho    ,rhow   ,nstru  ,&
&ngridm ,nnode  ,node   ,nbrnod ,brnode ,hbdpar ,&
&qbdpar ,af2    ,wf2    ,abcd1  ,kabcd1 ,kabcd2 ,&
&scnode ,snnode ,nnn    ,sclnod ,wfrict ,rfv1   ,&
&rfv2   ,kbeta  ,mat    ,nnc    ,&
&nnm    ,nns    ,nnf    ,nnmu   ,nosdim ,&
&scifri ,scimu  ,snceq  ,snmeq  ,snqhs  ,&
&snfric ,snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,&
&sclfri ,sclmu  ,scceq  ,scmeq  ,scqhs  ,scfric ,&
&scmu   ,rhs    ,lambda ,dhstru ,theta2 ,overlp ,&
&arex   ,arexcn ,arexop ,ker    ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAPRED (KAlman PREDiction step)
!
! Module description: Subroutine KAPRED computes the covariance P(n+1|n)
!                     for a time step.
!
!                     Starting from the covariance P(n|n) from time step
!                     n, subroutine KAPRED computes the predicted P(n+1|n)
!                     for the next time step.
!
!                     First a.o. the derivatives of water level dependent
!                     hydrodynamic parameters are calculated by calling
!                     routine KAHYPA. Then subroutine KADQLT computes the
!                     derivative of the lateral discharges.  After that
!                     the prediction is done by subroutine KAPRNT.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 68 abcd1             P  -
! 66 af2               P  -
!  9 aft               P  -
! 27 alfab             P  -
! 12 bfricp            P  -
! 11 bfrict            P  -
!  3 branch            P  -
! 63 brnode            P  -
! 25 cp                P  -
! 28 deriva            P  -
! 46 dt1               P  -
! 20 engpar            P  -
! 22 exres             P  -
! 29 g                 P  -
! 51 grid              P  -
! 19 grsize            P  -
! 64 hbdpar            P  -
!  7 hlev              P  -
! 49 hp                P  -
! 42 indx              P  -
! 23 juer              P  -
! 69 kabcd1            P  -
! 70 kabcd2            P  -
! 78 kbeta             P  -
! 55 lsalt             P  -
! 79 mat               P  -
!  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
! 13 maxtab            I  Maximum number of defined tables.
!  1 nbran             I  Number of branches.
! 62 nbrnod            I  Maximum number of connected branches to one
!                         node.
! 21 nexres            P  -
!  2 ngrid             I  Number of grid points in network.
! 59 ngridm            I  Maximum number of gridpoints in a branch.
!  6 nlev              P  -
! 80 nnc               I  Number of uncorrelated random noise processes
!                         for the continity equation.
! 83 nnf               I  Number of uncertain bed friction parameters.
! 81 nnm               I  Number of uncorrelated random noise processes
!                         for the momentum equation.
! 84 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
! 73 nnn               I  Number of uncorrelated random noise processes
!                         for nodal (= boundary) equations.
! 60 nnode             I  Number of nodes.
! 82 nns               I  Number of uncorrelated random noise processes
!                         for the Q-H relations of structures.
! 61 node              P  -
! 38 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 33 nqlat             P  -
! 58 nstru             I  Number of structures.
! 15 ntab              P  -
! 14 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 10 of                P  -
! 39 p1                P  -
! 40 p2                P  -
! 41 pcol              P  -
! 43 pfa               P  -
! 44 pmua              P  -
! 24 prslot            P  -
! 47 psi               P  -
!    psltvr            P  -
! 45 pw                P  -
! 65 qbdpar            P  -
! 35 qlat              P  -
! 37 qlatac            P  -
! 34 qltpar            P  -
! 50 qp                P  -
! 76 rfv1              P  -
! 77 rfv2              P  -
! 56 rho               P  -
! 57 rhow              P  -
! 26 rp                P  -
! 87 scifri            P  -
! 88 scimu             P  -
! 95 sclceq            P  -
! 98 sclfri            P  -
! 96 sclmeq            P  -
! 99 sclmu             P  -
! 74 sclnod            P  -
! 97 sclqhs            P  -
! 71 scnode            P  -
! 17 sectc             P  -
! 18 sectv             P  -
! 89 snceq             P  -
! 92 snfric            P  -
! 90 snmeq             P  -
! 93 snmu              P  -
! 72 snnode            P  -
! 91 snqhs             P  -
! 94 snwind            P  -
! 36 strclo            P  -
! 32 strpar            P  -
! 31 strtyp            P  -
! 16 table             P  -
! 54 tauwi             P  -
! 48 theta             P  -
! 30 time              P  -
!  4 typcr             P  -
! 53 waoft             P  -
! 67 wf2               P  -
! 75 wfrict            P  -
!  8 wft               P  -
! 52 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! kadqlt  KAlman Derivative of Q LaTeral
! kahypa  KAlman HYdraulic PArameters
! kaprnt  KAlman PRedict Next Time step
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kapred.pf,v $
! Revision 1.5  1997/06/17  11:23:48  kuipe_j
! Initialize vars
!
! Revision 1.4  1997/01/23  08:29:42  kuipe_j
! Make flow module robust
!
! Revision 1.3  1996/11/05  13:48:07  kuipe_j
! Error in declaration hlev fixed
!
! Revision 1.2  1996/04/12  13:05:16  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:49  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
   include '..\include\sobdim.i'
!
!     Declaration of Parameters:
!
   integer nbran, ngrid, maxlev, nexres, juer, ker
   integer ngridm, nstru, maxtab, ntabm, nosdim
   integer nnode, nnn ,nbrnod,&
   &branch(4,nbran), bfrict(3,nbran),&
   &typcr(nbran),&
   &ntab(4,maxtab),&
   &nlev(ngrid),&
   &arexcn(ngrid,2), arexop(2)
   integer grid(ngrid)
   integer strtyp(10,*)
   integer hbdpar(3,*), qbdpar(3,*)
   integer node(4,nnode), indx(nnode)
   integer wfrict(3,nbran)
   integer sclnod(nnn+1), scnode(*), brnode(nbrnod+1,nnode)
   integer nnc, nnm, nns, nnf, nnmu, np
   integer scifri(ngrid), scimu(nstru)
   integer sclceq(nnc+1), sclqhs(nns+1), sclfri(nnf+1),&
   &sclmu(nnmu+1), sclmeq(nnm+1)
   integer scceq(*), scmeq(*), scqhs(*),&
   &scfric(ngrid), scmu(nstru)
!
   logical lsalt, strclo(*)
!
   real    table(ntabm),&
   &wft(ngrid,maxlev), aft(ngrid,maxlev),&
   &of (ngrid,maxlev),&
   &waoft(ngrid,dmwaof),&
   &cp(ngrid,4), rp(ngrid,4), alfab(ngrid),&
   &bfricp(6,ngrid),&
   &grsize(4,ngrid,*), engpar(9), exres(3,*),&
   &sectc(ngrid,3), sectv(ngrid,dmsecv),&
   &x(ngrid), arex(ngrid,4),&
   &prslot(3,nbran), psltvr(7,ngrid)
!
   real    g, psi, theta, rhow, pw
   real    lambda, theta2, overlp, dhstru
   real    tauwi(ngrid), rho(ngrid)
   real    strpar(dmstrpar,*)
   real    qltpar(9,*), af2(ngrid), wf2(ngrid)
   real    pfa(nnf), pmua(nnmu)
   real    deriva(ngrid,11)
   real    snnode(nosdim,nnn)
   real    snceq(nosdim,nnc), snmeq(nosdim,nnm), snqhs(nosdim,nns),&
   &snfric(2,nnf), snmu(2,nnmu), snwind(2)
   real    p1(np,np), p2(np,np), pcol(np)
!
   double precision dt1, hlev(ngrid,maxlev),hp(ngrid,3), qp(ngrid,3)
   double precision abcd1(ngridm,5)
   double precision kabcd1(ngrid,8), kabcd2(ngrid,14)
   double precision rfv1(ngrid,3), rfv2(ngrid,3)
   double precision mat(nnode,nnode), rhs(nnode)
   double precision kbeta(2,nbran)
!
   integer nqlat
   real    qlat(*), qlatac(*)
   double  precision  time
!
!                                                           <h_n>
   call KAHYPA (nbran  ,ngrid  ,nnf    ,branch ,typcr  ,hp(1,1),&
!                   <h_*>   <h_n+1>
   &hp(1,2),hp(1,3),maxlev ,nlev   ,hlev   ,wft    ,&
!                                                   <Q_n>   <Q_*>
   &aft    ,of     ,bfrict ,bfricp ,qp(1,1),qp(1,2),&
   &maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,&
   &grsize ,engpar ,x      ,nexres ,exres  ,juer   ,&
   &prslot ,waoft  ,af2    ,wf2    ,grid   ,cp     ,&
   &rp     ,alfab  ,scifri ,pfa    ,&
!                   <afacc>         <oacc>          <dwfdh>
   &deriva(1,10)   ,deriva(1,11)   ,deriva(1, 5)   ,&
!                   <dcdh>          <drdh>          <dalfdh>
   &deriva(1, 1)   ,deriva(1, 3)   ,deriva(1, 4)   ,&
!                   <dcdq>          <eta>           <detadh>
   &deriva(1, 2)   ,deriva(1, 8)   ,deriva(1, 6)   ,&
   &theta2 ,overlp ,arex   ,arexcn ,arexop ,   ker ,&
   &psltvr )
!
!                                                  <h_n>   <h_*>
   call KADQLT (g      ,time   ,ngrid  ,x      ,hp(1,1),hp(1,2),&
   &strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,&
   &nqlat  ,qltpar ,juer   ,lambda ,theta2 ,dt1    ,&
!                                         <dqltdh>
   &qlat   ,strclo ,qlatac ,deriva(1, 7)   ,ker    )
!
   call KAPRNT (g      ,dt1    ,psi    ,theta  ,nbran  ,branch ,&
   &ngrid  ,lambda ,dhstru ,hp     ,qp     ,grid   ,&
   &x      ,waoft  ,cp     ,rp     ,alfab  ,tauwi  ,&
   &lsalt  ,rho    ,rhow   ,nstru  ,nosdim ,&
   &strtyp ,strpar ,maxtab ,ntabm  ,ntab   ,table  ,&
   &ngridm ,prslot ,strclo ,nnode  ,&
   &node   ,nbrnod ,brnode ,hbdpar ,qbdpar ,juer   ,&
!                  <eta>          <dqltdh>        <dalfdh>
   &deriva(1, 8)   ,deriva(1, 7)   ,deriva(1, 4)   ,&
!                  <dcdh>         <drdh>          <dcdq>
   &deriva(1, 1)   ,deriva(1, 3)   ,deriva(1, 2)   ,&
!                  <dwfdh>        <detadh>
   &deriva(1, 5)   ,deriva(1, 6)   ,&
   &af2    ,wf2    ,pw     ,pfa    ,pmua   ,abcd1  ,&
   &kabcd1 ,kabcd2 ,indx   ,scnode ,snnode ,nnn    ,&
   &sclnod ,wfrict ,rfv1   ,rfv2   ,kbeta  ,mat    ,&
   &nnc    ,nnm    ,nns    ,nnf    ,nnmu   ,scifri ,&
   &scimu  ,snceq  ,snmeq  ,snqhs  ,snfric ,snmu   ,&
   &snwind ,sclceq ,sclmeq ,sclqhs ,sclfri ,sclmu  ,&
   &scceq  ,scmeq  ,scqhs  ,scfric ,scmu   ,&
   &rhs    ,np     ,p1     ,p2     ,pcol   ,ker    ,&
   &psltvr )
!
end
