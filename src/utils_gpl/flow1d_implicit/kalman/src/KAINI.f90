subroutine KAINI (ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnn    ,&
&nnf    ,nnmu   ,np     ,nsamp  ,itim   ,juer   ,&
&ncelst ,inires ,newres ,nosdim ,af2    ,&
&wf2    ,scifri ,scimu  ,&
&snceq  ,snmeq  ,snqhs  ,snnode ,snfric ,snmu   ,&
&snwind ,smpns  ,sclceq ,sclmeq ,sclqhs ,sclnod ,&
&sclfri ,sclmu  ,scfric ,scmu   ,&
&fd_nefis_rst, fd_nefis_new ,p1     ,p2     ,&
&pfa    ,pmua   ,pw     ,res    ,scares ,rescov ,&
&hp     ,qp     ,lfilt  ,corrnm ,kalini ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAINI (KAlman INItialisation)
!
! Module description: In subroutine KAINI the initial Kalman filter
!                     conditions (correction parameters, covariances and
!                     noises at the begin time of the simulation period)
!                     will be assigned.
!                     For the Kalman filter there are two options:
!                     1: Initial conditions will be read from the restart
!                        file created in a previous SOBEK run;
!                     2: Initial conditions are available from the user.
!                     If the filtering is time independent, the the user
!                     has to provide the Kalman gain.
!
!                     In subroutine KAINS1 the initial Kalman conditions
!                     will be assigned for the first option mentioned
!                     above. The values will be read and assigned in
!                     subroutine KARSTA.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 16 af2(ngrid)        O  Flow area in every grid point i on time n+1.
!                         Only values at grid points around structures are
!                         defined.
! 45 dafdrn            P  -
! 43 dafdst            P  -
! 44 defdrn            P  -
! 42 defdst            P  -
! 53 hp(ngrid,3)       IO (i,1) = h1(i) (t=n)
!                         (i,2) = h(i)  (*)
!                         (i,3) = h2(i) (t=n+1)
! 14 inires            P  -
! 11 itim              P  -
! 12 juer              P  -
! 56 ker               P  -
! 55 lfilt             O  = True if a filter step must be performed.
! 13 ncelst            P  -
! 15 newres            P  -
!  1 ngrid             I  Number of grid points in network.
!  3 nnc               I  Number of uncorrelated random noise processes
!                         for the continity equation.
!  7 nnf               I  Number of uncertain bed friction parameters.
!  4 nnm               I  Number of uncorrelated random noise processes
!                         for the momentum equation.
!  8 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  6 nnn               I  Number of uncorrelated random noise processes
!                         for nodal (= boundary) equations.
!  5 nns               I  Number of uncorrelated random noise processes
!                         for the Q-H relations of structures.
!  9 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 10 nsamp             I  Number of hydrodynamic samples (measurements)
!  2 nstru             I  Number of structures.
! 46 p1                P  -
! 47 pfa               P  -
! 48 pmua              P  -
! 49 pw                P  -
! 54 qp(ngrid,3)       IO (i,1) = q1(i) (t=n)
!                         (i,2) = q(i)  (*)
!                         (i,3) = q2(i) (t=n+1)
! 50 res(nsamp)        O  Residual vector
! 52 rescov(nsamp,     O  Matrix with covariances of residuals.
!      ,nsamp)
! 51 scares(nsamp)     O  Scaled residual vector
! 40 scfric            P  -
! 21 scifri            P  -
! 22 scimu             P  -
! 31 sclceq            P  -
! 35 sclfri            P  -
! 32 sclmeq            P  -
! 36 sclmu             P  -
! 34 sclnod            P  -
! 33 sclqhs            P  -
! 41 scmu              P  -
! 30 smpns             P  -
! 23 snceq             P  -
! 27 snfric            P  -
! 24 snmeq             P  -
! 28 snmu              P  -
! 26 snnode            P  -
! 25 snqhs             P  -
! 29 snwind            P  -
! 17 wf2(ngrid)        O  Flow width in every grid point i on time n+1.
!                         Only values at grid points around structures are
!                         defined.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! kainsn  KAlman INitialse System noise
! karsta  KAlman read or write ReSTArt information
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kaini.pf,v $
! Revision 1.5  1999/03/15  15:51:53  kuipe_j
! tabs removed
!
! Revision 1.4  1997/06/17  11:23:46  kuipe_j
! Initialize vars
!
! Revision 1.3  1997/06/04  11:18:15  kuipe_j
! Initialize arrays
!
! Revision 1.2  1996/04/12  13:05:03  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:37  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer      ngrid, nstru, nnc, nnm, nns, nnn, nnf, nnmu,nsamp,&
   &nosdim
   integer      scifri(ngrid), scimu(nstru)
   integer      sclceq(nnc+1), sclmeq(nnm+1), sclqhs(nns+1),&
   &sclnod(nnn+1), sclfri(nnf+1), sclmu(nnmu+1)
   integer      scfric(ngrid), scmu(nstru)
   integer      kalini(*)
!
   real         af2(ngrid)   , wf2(ngrid)
   double precision            hp (ngrid,3) , qp (ngrid,3)
   real         snceq(nosdim,nnc) , snmeq(nosdim,nnm) ,&
   &snqhs(nosdim,nns) , snnode(nosdim,nnn),&
   &snfric(2,nnf), snmu(2,nnmu),&
   &snwind(2)    , smpns(nosdim,nsamp)
   real         res   (nsamp), scares(nsamp), rescov(nsamp,nsamp)
   integer      np     ,ncelst ,&
   &juer   ,ker
   integer      fd_nefis_rst, fd_nefis_new, itim(2)
   real         p1   (np,np)   ,p2   (np,np)   ,&
   &pfa(nnf)       ,pmua(nnmu) ,pw
   logical      inires, lfilt  ,newres
   character*40 corrnm(*)
!
!     Declaration of local variables
!
   integer      i    ,j , ipar , k
!
   lfilt  = .true.
!
   do 10 i = 1, ngrid
      af2(i) = 0.
      wf2(i) = 0.
10 continue
!
   do 15 i=1,10
      kalini(i) = 0
15 continue
!
   call KAINSN (ngrid  ,nstru  ,nnc    ,nnm    ,nns    ,nnn    ,&
   &nnf    ,nnmu   ,nsamp  ,nosdim ,scifri ,scimu  ,&
   &snceq  ,snmeq  ,snqhs  ,snnode ,snfric ,snmu   ,&
   &snwind ,sclceq ,sclmeq ,sclqhs ,sclnod ,sclfri ,&
   &sclmu  ,scfric ,scmu   ,smpns  )
!
   call KARSTA (np     ,nnf    ,nnmu   ,itim   ,juer   ,.true. ,&
   &newres , fd_nefis_rst, fd_nefis_new, p1     ,&
   &pfa    ,pmua   ,pw     ,ncelst ,inires ,ker    )
!
!     Initialize for output purposes on time step 0
!
!     Initialize filtered values
!
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
!
!     Generate identifiers for correction parameters
!     - bed friction
!     - energy loss coefficient mu
!     - wind
!
   k=1
   do 70 ipar=1,nnf
      write(corrnm(k),'(a,i2)') 'Bed friction par.',ipar
      k=k+1
70 continue
!
   do 80 ipar=1,nnmu
      write(corrnm(k),'(a,i2)') 'Energy loss par.',ipar
      k=k+1
80 continue

   write(corrnm(k),'(a)') 'Wind stress par.'

end
