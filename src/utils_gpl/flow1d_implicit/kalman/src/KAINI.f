      subroutine KAINI (ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnn    ,
     &                  nnf    ,nnmu   ,np     ,nsamp  ,itim   ,juer   ,
     &                  ncelst ,inires ,newres ,nosdim ,af2    ,
     &                  wf2    ,scifri ,scimu  ,
     &                  snceq  ,snmeq  ,snqhs  ,snnode ,snfric ,snmu   ,
     &                  snwind ,smpns  ,sclceq ,sclmeq ,sclqhs ,sclnod ,
     &                  sclfri ,sclmu  ,scfric ,scmu   ,
     &                  fd_nefis_rst, fd_nefis_new ,p1     ,p2     ,
     &                  pfa    ,pmua   ,pw     ,res    ,scares ,rescov ,
     &                  hp     ,qp     ,lfilt  ,corrnm ,kalini ,ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAINI (KAlman INItialisation)
c
c Module description: In subroutine KAINI the initial Kalman filter
c                     conditions (correction parameters, covariances and
c                     noises at the begin time of the simulation period)
c                     will be assigned.
c                     For the Kalman filter there are two options:
c                     1: Initial conditions will be read from the restart
c                        file created in a previous SOBEK run;
c                     2: Initial conditions are available from the user.
c                     If the filtering is time independent, the the user
c                     has to provide the Kalman gain.
c
c                     In subroutine KAINS1 the initial Kalman conditions
c                     will be assigned for the first option mentioned
c                     above. The values will be read and assigned in
c                     subroutine KARSTA.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 16 af2(ngrid)        O  Flow area in every grid point i on time n+1.
c                         Only values at grid points around structures are
c                         defined.
c 45 dafdrn            P  -
c 43 dafdst            P  -
c 44 defdrn            P  -
c 42 defdst            P  -
c 53 hp(ngrid,3)       IO (i,1) = h1(i) (t=n)
c                         (i,2) = h(i)  (*)
c                         (i,3) = h2(i) (t=n+1)
c 14 inires            P  -
c 11 itim              P  -
c 12 juer              P  -
c 56 ker               P  -
c 55 lfilt             O  = True if a filter step must be performed.
c 13 ncelst            P  -
c 15 newres            P  -
c  1 ngrid             I  Number of grid points in network.
c  3 nnc               I  Number of uncorrelated random noise processes
c                         for the continity equation.
c  7 nnf               I  Number of uncertain bed friction parameters.
c  4 nnm               I  Number of uncorrelated random noise processes
c                         for the momentum equation.
c  8 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  6 nnn               I  Number of uncorrelated random noise processes
c                         for nodal (= boundary) equations.
c  5 nns               I  Number of uncorrelated random noise processes
c                         for the Q-H relations of structures.
c  9 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 10 nsamp             I  Number of hydrodynamic samples (measurements)
c  2 nstru             I  Number of structures.
c 46 p1                P  -
c 47 pfa               P  -
c 48 pmua              P  -
c 49 pw                P  -
c 54 qp(ngrid,3)       IO (i,1) = q1(i) (t=n)
c                         (i,2) = q(i)  (*)
c                         (i,3) = q2(i) (t=n+1)
c 50 res(nsamp)        O  Residual vector
c 52 rescov(nsamp,     O  Matrix with covariances of residuals.
c      ,nsamp)
c 51 scares(nsamp)     O  Scaled residual vector
c 40 scfric            P  -
c 21 scifri            P  -
c 22 scimu             P  -
c 31 sclceq            P  -
c 35 sclfri            P  -
c 32 sclmeq            P  -
c 36 sclmu             P  -
c 34 sclnod            P  -
c 33 sclqhs            P  -
c 41 scmu              P  -
c 30 smpns             P  -
c 23 snceq             P  -
c 27 snfric            P  -
c 24 snmeq             P  -
c 28 snmu              P  -
c 26 snnode            P  -
c 25 snqhs             P  -
c 29 snwind            P  -
c 17 wf2(ngrid)        O  Flow width in every grid point i on time n+1.
c                         Only values at grid points around structures are
c                         defined.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c kainsn  KAlman INitialse System noise
c karsta  KAlman read or write ReSTArt information
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kaini.pf,v $
c Revision 1.5  1999/03/15  15:51:53  kuipe_j
c tabs removed
c
c Revision 1.4  1997/06/17  11:23:46  kuipe_j
c Initialize vars
c
c Revision 1.3  1997/06/04  11:18:15  kuipe_j
c Initialize arrays
c
c Revision 1.2  1996/04/12  13:05:03  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:37  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer      ngrid, nstru, nnc, nnm, nns, nnn, nnf, nnmu,nsamp,
     +             nosdim
      integer      scifri(ngrid), scimu(nstru)
      integer      sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),
     +             sclnod(nnn+1), sclfri(nnf+1), sclmu(nnmu+1)
      integer      scfric(ngrid), scmu(nstru)
      integer      kalini(*)
c
      real         af2(ngrid)   , wf2(ngrid)  
      double precision            hp (ngrid,3) , qp (ngrid,3)
      real         snceq(nosdim,nnc) , snmeq(nosdim,nnm) ,
     +             snqhs(nosdim,nns) , snnode(nosdim,nnn),
     +             snfric(2,nnf), snmu(2,nnmu),
     +             snwind(2)    , smpns(nosdim,nsamp)
      real         res   (nsamp), scares(nsamp), rescov(nsamp,nsamp)
      integer      np     ,ncelst ,
     &             juer   ,ker
      integer      fd_nefis_rst, fd_nefis_new, itim(2)
      real         p1   (np,np)   ,p2   (np,np)   ,
     &             pfa(nnf)       ,pmua(nnmu) ,pw
      logical      inires, lfilt  ,newres
      character*40 corrnm(*)
c
c     Declaration of local variables
c
      integer      i    ,j , ipar , k
c
      lfilt  = .true.
c
      do 10 i = 1, ngrid
         af2(i) = 0.
         wf2(i) = 0.
   10 continue
c
      do 15 i=1,10
         kalini(i) = 0
   15 continue
c
      call KAINSN (ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnn    ,
     &             nnf    ,nnmu   ,nsamp  ,nosdim ,scifri ,scimu  ,
     &             snceq  ,snmeq  ,snqhs  ,snnode ,snfric ,snmu   ,
     &             snwind ,sclceq ,sclmeq ,sclqhs ,sclnod ,sclfri ,
     &             sclmu  ,scfric ,scmu   ,smpns  )
c
      call KARSTA (np     ,nnf    ,nnmu   ,itim   ,juer   ,.true. ,
     &             newres , fd_nefis_rst, fd_nefis_new, p1     ,
     &             pfa    ,pmua   ,pw     ,ncelst ,inires ,ker    )
c
c     Initialize for output purposes on time step 0
c
c     Initialize filtered values
c
      do 20 i = 1, ngrid
         hp(i,2) = hp(i,3)
         qp(i,2) = qp(i,3)
   20 continue

      do 40 i = 1, nsamp
         res   (i) = 0.
         scares(i) = 0.
         do 30 j = 1, nsamp
            rescov(i,j) = 0.
   30    continue
   40 continue

      do 60 i = 1, np
         do 50 j = 1, np
            p2(i,j) = p1(i,j)
   50    continue
   60 continue
c
c     Generate identifiers for correction parameters
c     - bed friction
c     - energy loss coefficient mu
c     - wind
c
      k=1
      do 70 ipar=1,nnf
         write(corrnm(k),'(a,i2)') 'Bed friction par.',ipar
         k=k+1
   70 continue
c
      do 80 ipar=1,nnmu
         write(corrnm(k),'(a,i2)') 'Energy loss par.',ipar
         k=k+1
   80 continue

      write(corrnm(k),'(a)') 'Wind stress par.'

      end
