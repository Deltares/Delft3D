subroutine KALMAN (maxlev ,maxtab ,nbran  ,ngrid  ,ngridm ,nnc   ,&
&nnm    ,nnn    ,nnode  ,nbrnod ,nns    ,nstru  ,nlev  ,&
&ntabm  ,nnf    ,nnmu   ,nsamp  ,ntsam  ,np     ,nosdim,&
&cpredn ,lfilt  ,filstp ,time   ,dt1    ,lsalt  ,juer  ,&
&nexres ,nqlat  ,flwpar ,kalpar ,&
&abcd1  ,af2    ,branch ,grid   ,hbdpar ,typcr  ,hp    ,&
&hlev   ,kabcd1 ,kabcd2 ,kbeta  ,mat    ,node   ,qbdpar,&
&rfv1   ,rfv2   ,rho    ,rhs    ,wft    ,aft    ,wf2   ,&
&wfrict ,of     ,bfrict ,bfricp ,qp     ,ntab   ,table ,&
&sectc  ,sectv  ,grsize ,engpar ,x      ,exres  ,prslot,&
&waoft  ,cp     ,rp     ,alfab  ,deriva ,strtyp ,strpar,&
&qltpar ,qlat   ,strclo ,qlatac ,tauwi  ,arex   ,arexcn,&
&arexop ,sclnod ,scnode ,snnode ,&
&scifri ,scimu  ,snceq  ,snmeq  ,snqhs  ,snfric ,&
&snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,sclfri ,&
&sclmu  ,scceq  ,scmeq  ,scqhs  ,scfric ,scmu   ,&
&smploc ,p1     ,p2     ,pcol   ,rescov ,&
&sample ,scares ,smpns  ,indx   ,brnode ,rhsm   ,&
&pfa    ,pmua   ,pw     ,res    ,kgain  ,ker    ,&
&psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KALMAN (KALman main routine)
!
! Module description: Determines the filtered model state for the next
!                     time step.
!
!                     Subroutine KALMAN will perform the following tasks:
!                     -    Predict the covariance of the model state if
!                          filtering is time dependent.
!                     -    Update both flow (from flow module) and cov
!                          -ariance prediction using the measurements for
!                          the current time step.
!
!                     Routine KAINS1 shifts arrays to the current time
!                     level. KAPRED predicts the covariance. KAUPPR uses
!                     the predicted mean (flow) and covariance to calcu-
!                     late the filtered model state and covariance.
!
!                     During Kalman filtering also the uncertain
!                     parameters are estimated.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 31 abcd1             P  -
! 32 af2               P  -
! 50 aft               P  -
! 69 alfab             P  -
! 78 arex              P  -
! 79 arexcn            P  -
! 80 arexop            P  -
! 55 bfricp            P  -
! 54 bfrict            P  -
! 33 branch            P  -
! 67 cp                P  -
! 20 cpredn            P  -
! 70 deriva            P  -
! 24 dt1               P  -
! 62 engpar            P  -
! 64 exres             P  -
! 22 filstp            IO filter step number
! 29 flwpar            I  -
! 34 grid              P  -
! 61 grsize            P  -
! 35 hbdpar            P  -
! 38 hlev              P  -
! 37 hp                P  -
! 26 juer              P  -
! 39 kabcd1            P  -
! 40 kabcd2            P  -
! 30 kalpar(5)         I  Kalman parameters.
!                         (1) Prediction interval (number of steps).
!                         (2) Time dependent (1) or time independent (2)
! 41 kbeta             P  -
! 21 lfilt             I  = True if a filter step must be performed.
! 25 lsalt             P  -
! 42 mat               P  -
!  1 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  2 maxtab            I  Maximum number of defined tables.
!  3 nbran             I  Number of branches.
! 10 nbrnod            I  Maximum number of connected branches to one
!                         node.
! 27 nexres            P  -
!  4 ngrid             I  Number of grid points in network.
!  5 ngridm            I  Maximum number of gridpoints in a branch.
! 13 nlev              P  -
!  6 nnc               I  Number of uncorrelated random noise processes
!                         for the continity equation.
! 15 nnf               I  Number of uncertain bed friction parameters.
!  7 nnm               I  Number of uncorrelated random noise processes
!                         for the momentum equation.
! 16 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  8 nnn               I  Number of uncorrelated random noise processes
!                         for nodal (= boundary) equations.
!  9 nnode             I  Number of nodes.
! 11 nns               I  Number of uncorrelated random noise processes
!                         for the Q-H relations of structures.
! 43 node              P  -
! 19 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 28 nqlat             P  -
! 17 nsamp             I  Number of hydrodynamic samples (measurements)
! 12 nstru             I  Number of structures.
! 57 ntab              P  -
! 14 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 18 ntsam             I  Number of filter steps
! 53 of                P  -
! 65 prslot            P  -
!    psltvr            P  -
! 44 qbdpar            P  -
! 74 qlat              P  -
! 76 qlatac            P  -
! 73 qltpar            P  -
! 56 qp                P  -
! 45 rfv1              P  -
! 46 rfv2              P  -
! 47 rho               P  -
! 48 rhs               P  -
! 68 rp                P  -
! 99 scceq             P  -
! 86 scifri            P  -
! 87 scimu             P  -
! 94 sclceq            P  -
! 97 sclfri            P  -
! 95 sclmeq            P  -
! 98 sclmu             P  -
! 81 sclnod            P  -
! 96 sclqhs            P  -
! 82 scnode            P  -
! 59 sectc             P  -
! 60 sectv             P  -
! 88 snceq             P  -
! 91 snfric            P  -
! 89 snmeq             P  -
! 92 snmu              P  -
! 83 snnode            P  -
! 90 snqhs             P  -
! 93 snwind            P  -
! 75 strclo            P  -
! 72 strpar            P  -
! 71 strtyp            P  -
! 58 table             P  -
! 77 tauwi             P  -
! 23 time              P  -
! 36 typcr             P  -
! 66 waoft             P  -
! 51 wf2               P  -
! 52 wfrict            P  -
! 49 wft               P  -
! 63 x                 P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! kaflsp  Kalman Flow Structure correction Parameter
! kains1  KAlman Initialize Next Step 1
! kains2  KAlman Initialize Next Step 2
! kains3  KAlman Initialize Next Step 3
! kapred  KAlman PREDiction step
! kauppr  KAlman UPdate PRediction
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kalman.pf,v $
! Revision 1.8  1999/03/15  15:51:59  kuipe_j
! tabs removed
!
! Revision 1.7  1997/08/21  11:11:06  kuipe_j
! Check for negative areas and wetted per.
!
! Revision 1.6  1997/01/23  08:29:39  kuipe_j
! Make flow module robust
!
! Revision 1.5  1996/12/05  10:00:02  kuipe_j
! Smoothing kgain,linearization,limit covariance,etc
!
! Revision 1.4  1996/11/05  13:48:03  kuipe_j
! Error in declaration hlev fixed
!
! Revision 1.3  1996/05/28  13:31:51  kuipe_j
! Smoothing  added
!
! Revision 1.2  1996/04/12  13:05:10  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:43  kuipe_j
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
   integer ngridm, nstru, maxtab, ntabm
   integer nnode, nbrnod, nnn, cpredn, filstp, nosdim
!
   integer branch(4,nbran), bfrict(3,nbran), typcr(nbran),&
   &ntab(4,maxtab), nlev(ngrid), grid(ngrid)
   integer strtyp(10,*), hbdpar(3,*), qbdpar(3,*)
   integer node(4,nnode), indx(nnode), brnode(nbrnod+1,nnode)
   integer sclnod(nnn+1), scnode(*)
   integer wfrict(3,nbran), arexcn(ngrid,2), arexop(2)
!
   integer nnc, nnm, nns, nnf, nnmu, np
   integer scifri(ngrid), scimu(nstru)
   integer sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),&
   &sclfri(nnf+1), sclmu(nnmu+1)
   integer scceq(*) , scmeq(*) , scqhs(*),&
   &scfric(ngrid), scmu(nstru)
!
   logical lsalt, lfilt, strclo(*)
!
   real    table(ntabm),&
   &wft(ngrid,maxlev), aft(ngrid,maxlev),&
   &of (ngrid,maxlev),&
   &waoft(ngrid,dmwaof),&
   &cp(ngrid,4), rp(ngrid,4), alfab(ngrid),&
   &bfricp(6,ngrid),&
   &grsize(4,ngrid,*), engpar(9), exres(3,*),&
   &sectc(ngrid,3), sectv(ngrid,dmsecv),&
   &x(ngrid), prslot(3,nbran), flwpar(*),arex(ngrid,4),&
   &psltvr(7,ngrid)
!
   real    pw
   real    tauwi(ngrid), rho(ngrid)
   real    strpar(dmstrpar,*)
   real    qltpar(9,*), af2(ngrid), wf2(ngrid)
   real    pfa(nnf), pmua(nnmu)
   real    deriva(ngrid,11)
   real    snnode(nosdim,nnn)
   real    snceq(nosdim,nnc), snmeq(nosdim,nnm), snqhs(nosdim,nns),&
   &snfric(2,nnf), snmu(2,nnmu), snwind(2)
   real    p1(np,np), p2(np,np), pcol(np), kalpar(5)
!
   double precision dt1, hlev(ngrid,maxlev),hp(ngrid,3),qp(ngrid,3)
   double precision abcd1(ngridm,5)
   double precision kabcd1(ngrid,8), kabcd2(ngrid,14)
   double precision rfv1(ngrid,3), rfv2(ngrid,3)
   double precision mat(nnode,nnode), rhs(nnode)
   double precision kbeta(2,nbran)
!
   real    qlat(*), qlatac(*)
   double  precision  time
!
   integer nqlat
!
   integer nsamp, ntsam
   integer smploc(nsamp)
   real    res(nsamp), scares(nsamp), sample(nsamp,ntsam),&
   &smpns(nosdim,nsamp)
   real    rescov(nsamp,nsamp), rhsm(nsamp), kgain(np,nsamp)
!
!     Declaration of local variables
!
   integer predin, filtyp
   real    g, psi, theta, theta2, rhow, lambda,overlp, dhstru
!
   predin = int(kalpar(1))
   filtyp = int(kalpar(2))
   g      = flwpar( 1 )
   psi    = flwpar( 2 )
   theta  = flwpar( 3 )
   rhow   = flwpar( 6 )
   lambda = flwpar(10 )
   dhstru = flwpar(12 )
   overlp = flwpar(16 )
   theta2 = theta

   call KAINS1 (cpredn ,predin ,lfilt  ,ngrid  ,&
!                   <wt1>          <wt>
   &deriva(1,9)    ,waoft(1,2)     )
!
   if ( filtyp .eq. 1 ) then
!
!        Time dependent filtering
!
      call KAPRED (nbran  ,ngrid  ,branch ,typcr  ,maxlev ,nlev   ,&
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

   endif
!
!     Remove correction for free gate contraction
!
   call KAFLSP (nstru  ,nnmu   ,strpar ,scimu  ,pmua   )
!
   if ( lfilt ) then
      filstp = filstp + 1

      call KAUPPR (ngrid  ,nsamp  ,nnf    ,nnmu   ,np     ,nosdim ,&
      &smploc ,kalpar ,p1     ,p2     ,rescov ,&
      &sample(1,filstp)       ,scares ,smpns  ,&
!                                          <hn+1|n+1> <Qn+1|n+1>
      &indx   ,rhsm   ,juer   ,hp(1,2),qp(1,2),&
!                    <hn+1|n> <qn+1|n>
      &hp(1,3),qp(1,3),pfa    ,pmua   ,pw     ,&
      &res    ,kgain  ,ker    ,nbran  , branch,&
      &pcol   ,x      )
!
      call FLCHKH (ngrid  ,nbran  ,branch ,typcr  ,maxlev ,hlev   ,&
      &hp(1,2),juer   ,ker    ,prslot ,psltvr )
!
      call KAINS2 (ngrid  ,hp     ,qp     )
   else
      call KAINS3 (np     ,p1     ,p2     )
   endif
!
end
