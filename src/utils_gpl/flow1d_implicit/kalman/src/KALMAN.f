      subroutine KALMAN (maxlev ,maxtab ,nbran  ,ngrid  ,ngridm ,nnc   ,
     +           nnm    ,nnn    ,nnode  ,nbrnod ,nns    ,nstru  ,nlev  ,
     +           ntabm  ,nnf    ,nnmu   ,nsamp  ,ntsam  ,np     ,nosdim,
     +           cpredn ,lfilt  ,filstp ,time   ,dt1    ,lsalt  ,juer  ,
     +           nexres ,nqlat  ,flwpar ,kalpar ,
     +           abcd1  ,af2    ,branch ,grid   ,hbdpar ,typcr  ,hp    ,
     +           hlev   ,kabcd1 ,kabcd2 ,kbeta  ,mat    ,node   ,qbdpar,
     +           rfv1   ,rfv2   ,rho    ,rhs    ,wft    ,aft    ,wf2   ,
     +           wfrict ,of     ,bfrict ,bfricp ,qp     ,ntab   ,table ,
     +           sectc  ,sectv  ,grsize ,engpar ,x      ,exres  ,prslot,
     +           waoft  ,cp     ,rp     ,alfab  ,deriva ,strtyp ,strpar,
     +           qltpar ,qlat   ,strclo ,qlatac ,tauwi  ,arex   ,arexcn,
     +           arexop ,sclnod ,scnode ,snnode ,
     +           scifri ,scimu  ,snceq  ,snmeq  ,snqhs  ,snfric ,
     +           snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,sclfri ,
     +           sclmu  ,scceq  ,scmeq  ,scqhs  ,scfric ,scmu   ,
     +           smploc ,p1     ,p2     ,pcol   ,rescov ,
     +           sample ,scares ,smpns  ,indx   ,brnode ,rhsm   ,
     +           pfa    ,pmua   ,pw     ,res    ,kgain  ,ker    ,
     +           psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KALMAN (KALman main routine)
c
c Module description: Determines the filtered model state for the next
c                     time step.
c
c                     Subroutine KALMAN will perform the following tasks:
c                     -    Predict the covariance of the model state if
c                          filtering is time dependent.
c                     -    Update both flow (from flow module) and cov
c                          -ariance prediction using the measurements for
c                          the current time step.
c
c                     Routine KAINS1 shifts arrays to the current time
c                     level. KAPRED predicts the covariance. KAUPPR uses
c                     the predicted mean (flow) and covariance to calcu-
c                     late the filtered model state and covariance.
c
c                     During Kalman filtering also the uncertain
c                     parameters are estimated.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 31 abcd1             P  -
c 32 af2               P  -
c 50 aft               P  -
c 69 alfab             P  -
c 78 arex              P  -
c 79 arexcn            P  -
c 80 arexop            P  -
c 55 bfricp            P  -
c 54 bfrict            P  -
c 33 branch            P  -
c 67 cp                P  -
c 20 cpredn            P  -
c 70 deriva            P  -
c 24 dt1               P  -
c 62 engpar            P  -
c 64 exres             P  -
c 22 filstp            IO filter step number
c 29 flwpar            I  -
c 34 grid              P  -
c 61 grsize            P  -
c 35 hbdpar            P  -
c 38 hlev              P  -
c 37 hp                P  -
c 26 juer              P  -
c 39 kabcd1            P  -
c 40 kabcd2            P  -
c 30 kalpar(5)         I  Kalman parameters.
c                         (1) Prediction interval (number of steps).
c                         (2) Time dependent (1) or time independent (2)
c 41 kbeta             P  -
c 21 lfilt             I  = True if a filter step must be performed.
c 25 lsalt             P  -
c 42 mat               P  -
c  1 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  2 maxtab            I  Maximum number of defined tables.
c  3 nbran             I  Number of branches.
c 10 nbrnod            I  Maximum number of connected branches to one
c                         node.
c 27 nexres            P  -
c  4 ngrid             I  Number of grid points in network.
c  5 ngridm            I  Maximum number of gridpoints in a branch.
c 13 nlev              P  -
c  6 nnc               I  Number of uncorrelated random noise processes
c                         for the continity equation.
c 15 nnf               I  Number of uncertain bed friction parameters.
c  7 nnm               I  Number of uncorrelated random noise processes
c                         for the momentum equation.
c 16 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  8 nnn               I  Number of uncorrelated random noise processes
c                         for nodal (= boundary) equations.
c  9 nnode             I  Number of nodes.
c 11 nns               I  Number of uncorrelated random noise processes
c                         for the Q-H relations of structures.
c 43 node              P  -
c 19 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 28 nqlat             P  -
c 17 nsamp             I  Number of hydrodynamic samples (measurements)
c 12 nstru             I  Number of structures.
c 57 ntab              P  -
c 14 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 18 ntsam             I  Number of filter steps
c 53 of                P  -
c 65 prslot            P  -
c    psltvr            P  -
c 44 qbdpar            P  -
c 74 qlat              P  -
c 76 qlatac            P  -
c 73 qltpar            P  -
c 56 qp                P  -
c 45 rfv1              P  -
c 46 rfv2              P  -
c 47 rho               P  -
c 48 rhs               P  -
c 68 rp                P  -
c 99 scceq             P  -
c 86 scifri            P  -
c 87 scimu             P  -
c 94 sclceq            P  -
c 97 sclfri            P  -
c 95 sclmeq            P  -
c 98 sclmu             P  -
c 81 sclnod            P  -
c 96 sclqhs            P  -
c 82 scnode            P  -
c 59 sectc             P  -
c 60 sectv             P  -
c 88 snceq             P  -
c 91 snfric            P  -
c 89 snmeq             P  -
c 92 snmu              P  -
c 83 snnode            P  -
c 90 snqhs             P  -
c 93 snwind            P  -
c 75 strclo            P  -
c 72 strpar            P  -
c 71 strtyp            P  -
c 58 table             P  -
c 77 tauwi             P  -
c 23 time              P  -
c 36 typcr             P  -
c 66 waoft             P  -
c 51 wf2               P  -
c 52 wfrict            P  -
c 49 wft               P  -
c 63 x                 P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c kaflsp  Kalman Flow Structure correction Parameter
c kains1  KAlman Initialize Next Step 1
c kains2  KAlman Initialize Next Step 2
c kains3  KAlman Initialize Next Step 3
c kapred  KAlman PREDiction step
c kauppr  KAlman UPdate PRediction
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kalman.pf,v $
c Revision 1.8  1999/03/15  15:51:59  kuipe_j
c tabs removed
c
c Revision 1.7  1997/08/21  11:11:06  kuipe_j
c Check for negative areas and wetted per.
c
c Revision 1.6  1997/01/23  08:29:39  kuipe_j
c Make flow module robust
c
c Revision 1.5  1996/12/05  10:00:02  kuipe_j
c Smoothing kgain,linearization,limit covariance,etc
c
c Revision 1.4  1996/11/05  13:48:03  kuipe_j
c Error in declaration hlev fixed
c
c Revision 1.3  1996/05/28  13:31:51  kuipe_j
c Smoothing  added
c
c Revision 1.2  1996/04/12  13:05:10  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:43  kuipe_j
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
      integer ngridm, nstru, maxtab, ntabm
      integer nnode, nbrnod, nnn, cpredn, filstp, nosdim
c
      integer branch(4,nbran), bfrict(3,nbran), typcr(nbran),
     +        ntab(4,maxtab), nlev(ngrid), grid(ngrid)
      integer strtyp(10,*), hbdpar(3,*), qbdpar(3,*)
      integer node(4,nnode), indx(nnode), brnode(nbrnod+1,nnode)
      integer sclnod(nnn+1), scnode(*)
      integer wfrict(3,nbran), arexcn(ngrid,2), arexop(2)
c
      integer nnc, nnm, nns, nnf, nnmu, np
      integer scifri(ngrid), scimu(nstru)
      integer sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),
     +        sclfri(nnf+1), sclmu(nnmu+1)
      integer scceq(*) , scmeq(*) , scqhs(*),
     +        scfric(ngrid), scmu(nstru)
c
      logical lsalt, lfilt, strclo(*)
c
      real    table(ntabm),
     +        wft(ngrid,maxlev), aft(ngrid,maxlev),
     +        of (ngrid,maxlev),
     +        waoft(ngrid,dmwaof),
     +        cp(ngrid,4), rp(ngrid,4), alfab(ngrid),
     +        bfricp(6,ngrid),
     +        grsize(4,ngrid,*), engpar(9), exres(3,*),
     +        sectc(ngrid,3), sectv(ngrid,dmsecv),
     +        x(ngrid), prslot(3,nbran), flwpar(*),arex(ngrid,4),
     +        psltvr(7,ngrid)
c
      real    pw
      real    tauwi(ngrid), rho(ngrid)
      real    strpar(dmstrpar,*)
      real    qltpar(9,*), af2(ngrid), wf2(ngrid)
      real    pfa(nnf), pmua(nnmu)
      real    deriva(ngrid,11)
      real    snnode(nosdim,nnn)
      real    snceq(nosdim,nnc), snmeq(nosdim,nnm), snqhs(nosdim,nns),
     +        snfric(2,nnf), snmu(2,nnmu), snwind(2)
      real    p1(np,np), p2(np,np), pcol(np), kalpar(5)
c
      double precision dt1, hlev(ngrid,maxlev),hp(ngrid,3),qp(ngrid,3)
      double precision abcd1(ngridm,5)
      double precision kabcd1(ngrid,8), kabcd2(ngrid,14)
      double precision rfv1(ngrid,3), rfv2(ngrid,3)
      double precision mat(nnode,nnode), rhs(nnode)
      double precision kbeta(2,nbran)
c
      real    qlat(*), qlatac(*)
      double  precision  time
c
      integer nqlat
c
      integer nsamp, ntsam
      integer smploc(nsamp)
      real    res(nsamp), scares(nsamp), sample(nsamp,ntsam),
     +        smpns(nosdim,nsamp)
      real    rescov(nsamp,nsamp), rhsm(nsamp), kgain(np,nsamp)
c
c     Declaration of local variables
c
      integer predin, filtyp
      real    g, psi, theta, theta2, rhow, lambda,overlp, dhstru
c
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

      call KAINS1 (cpredn ,predin ,lfilt  ,ngrid  ,
c                   <wt1>          <wt>
     &             deriva(1,9)    ,waoft(1,2)     )
c
      if ( filtyp .eq. 1 ) then
c
c        Time dependent filtering
c
         call KAPRED (nbran  ,ngrid  ,branch ,typcr  ,maxlev ,nlev   ,
     +                hlev   ,wft    ,aft    ,of     ,bfrict ,bfricp ,
     +                maxtab ,ntabm  ,ntab   ,table  ,sectc  ,sectv  ,
     +                grsize ,engpar ,nexres ,exres  ,juer   ,prslot ,
     +                cp     ,rp     ,alfab  ,deriva ,
     +                g      ,time   ,strtyp ,strpar ,nqlat  ,qltpar ,
     +                qlat   ,strclo ,qlatac ,np     ,p1     ,p2     ,
     +                pcol   ,indx   ,pfa    ,pmua   ,pw     ,dt1    ,
     +                psi    ,theta  ,hp     ,qp     ,grid   ,x      ,
     +                waoft  ,tauwi  ,lsalt  ,rho    ,rhow   ,nstru  ,
     +                ngridm ,nnode  ,node   ,nbrnod ,brnode ,hbdpar ,
     +                qbdpar ,af2    ,wf2    ,abcd1  ,kabcd1 ,kabcd2 ,
     +                scnode ,snnode ,nnn    ,sclnod ,wfrict ,rfv1   ,
     +                rfv2   ,kbeta  ,mat    ,nnc    ,
     +                nnm    ,nns    ,nnf    ,nnmu   ,nosdim ,
     +                scifri ,scimu  ,snceq  ,snmeq  ,snqhs  ,
     +                snfric ,snmu   ,snwind ,sclceq ,sclmeq ,sclqhs ,
     +                sclfri ,sclmu  ,scceq  ,scmeq  ,scqhs  ,scfric ,
     +                scmu   ,rhs    ,lambda ,dhstru ,theta2 ,overlp ,
     +                arex   ,arexcn ,arexop ,ker    ,psltvr )

      endif
c
c     Remove correction for free gate contraction
c
      call KAFLSP (nstru  ,nnmu   ,strpar ,scimu  ,pmua   )
c
      if ( lfilt ) then
         filstp = filstp + 1

         call KAUPPR (ngrid  ,nsamp  ,nnf    ,nnmu   ,np     ,nosdim ,
     +                smploc ,kalpar ,p1     ,p2     ,rescov ,
     +                sample(1,filstp)       ,scares ,smpns  ,
c                                          <hn+1|n+1> <Qn+1|n+1>
     +                indx   ,rhsm   ,juer   ,hp(1,2),qp(1,2),
c                    <hn+1|n> <qn+1|n>
     +                hp(1,3),qp(1,3),pfa    ,pmua   ,pw     ,
     +                res    ,kgain  ,ker    ,nbran  , branch,
     +                pcol   ,x      )
c
         call FLCHKH (ngrid  ,nbran  ,branch ,typcr  ,maxlev ,hlev   ,
     +                hp(1,2),juer   ,ker    ,prslot ,psltvr )
c
         call KAINS2 (ngrid  ,hp     ,qp     )
      else
         call KAINS3 (np     ,p1     ,p2     )
      endif
c
      end
