subroutine sanorm (ngrid  ,ngridm ,i1     ,i2     ,dt     ,psi   ,&
&theta  ,q1     ,q2     ,qltgim ,csa1   ,csd1  ,&
&source ,disgr  ,x      ,at1    ,at2    ,af    ,&
&aa     ,ba     ,da     ,ea     ,fd     ,gd    ,&
&md     ,nd     ,ra     ,rd     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SANORM (SAlt NORMal a,b,d,e-etc. coeff. calc.)
!
! Module description: Calculate the A,B,D,E-etc. coefficients for all
!                     grid cells in a branch.
!
!                     The coefficients apply to normal cells as wel to
!                     structures. The obtained coefficients for structu-
!                     re cells are wrong and will be recalculated in
!                     SASTRU.
!
!                     Because in the functional design [S-FO-001.5KV /
!                     par. 22.1.1 ] option 1 has been chosen for P1 and
!                     P2 the term P1 has a constant value of 1 and
!                     P2=Af*D.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 19 aa(ngridm)        O  a-coefficient [cs(i)] in diff.-advection equa-
!                         tion for every grid point in a branch.
! 18 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 16 at1(ngrid)        I  Total area at every cross section for time
!                         level t(n).
! 17 at2(ngrid)        I  Total area at every cross section for time
!                         level t(n+1).
! 20 ba(ngridm)        IO b-coefficient [c (i)] in diff.-advection equa-
!                         tion for every grid point in a branch.
! 11 csa1(ngrid)       I  Salt concentration in every grid point at time
!                         t(n).
! 12 csd1(ngrid)       I  Diffusion (c s) in every grid point at time
!                         t(n).
! 21 da(ngridm)        O  d-coefficient [cs(i+1)] in diff.-advection
!                         equation for every grid point in a branch.
! 14 disgr(ngrid)      I  Dispersion coefficient in every grid point at
!                         time t(n+1).
!  5 dt                I  Computational time step dt [sec].
! 22 ea(ngridm)        O  e-coefficient [c s(i+1)] in diff.-advection
!                         equation for every grid point in a branch.
! 23 fd(ngridm)        O  f-coefficient [cs(i)] in diffusion equation
!                         for every grid point in a branch.
! 24 gd(ngridm)        O  g-coefficient [c (i)] in diffusion equation
!                         for every grid point in a branch.
!  3 i1                I  Index of first grid point in actual branch.
!  4 i2                I  Index of last grid point in actual branch.
! 25 md(ngridm)        O  m-coefficient [cs(i+1)] in diffusion equation
!                         for every grid point in a branch.
! 26 nd(ngridm)        O  n-coefficient [c s(i+1)] in diffusion equation
!                         for every grid point in a branch.
!  1 ngrid             I  Number of grid points in network.
!  2 ngridm            I  Maximum number of gridpoints in a branch.
!  6 psi               I  Space weight factor in Preissmann scheme.
!  8 q1(ngrid)         I  Discharge in every grid point at time t(n).
!  9 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 10 qltgim(ngrid)     I  Sum of only outgoing lateral discharges in
!                         every grid point at time t(n+1/2).
! 27 ra(ngridm)        O  Right-hand-side of advection-diffusion equa-
!                         tion for every grid point in a branch.
! 28 rd(ngridm)        O  Right-hand-side of diffusion equation for
!                         every grid point in a branch.
! 13 source(ngrid)     I  Load due to inflow at salt stations, wast
!                         loads and connected salt stations. One value
!                         for every grid point at time t(n+1).
!  7 theta             I  Time weight factor in Preissmann scheme.
! 15 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sanorm.pf,v $
! Revision 1.4  1997/02/17  10:27:48  kuipe_j
! Lateral  Q  in m3/s in cont equation
!
! Revision 1.3  1995/10/18  09:00:27  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:06:11  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:53  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  09:17:20  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    ngrid  ,ngridm ,i1     ,i2
   real       psi    ,theta
   real       qltgim(ngrid)  ,&
   &csa1 (ngrid)  ,csd1 (ngrid)  ,source(ngrid)  ,&
   &disgr(ngrid)  ,x    (ngrid)  ,at1   (ngrid)  ,&
   &at2  (ngrid)  ,af   (ngrid)
   double precision dt
   double precision&
   &q1   (ngrid ) ,q2   (ngrid ) ,&
   &aa   (ngridm) ,ba   (ngridm) ,da    (ngridm) ,&
   &ea   (ngridm) ,fd   (ngridm) ,gd    (ngridm) ,&
   &md   (ngridm) ,nd   (ngridm) ,ra    (ngridm) ,&
   &rd   (ngridm)
!
!     Declaration of local variables
!
   integer    i      ,ip     ,j
   real       psi1   ,theta1 ,dx     ,dts   ,qltdtx
   double precision    fdjd   ,gdjd
!
!     Remark: The coefficients on a grid point that is a structure are
!             also calculated although this is not necessary. These
!             coefficients will be redefined in SASTRU.
!
   dts    = sngl(dt)
   theta1 = 1. - theta
   psi1   = 1. - psi
   j      = 0
   do 10 i = i1 , i2-1
      ip     =  i + 1
      j      =  j + 1
      dx     =  x(ip) - x(i)
      qltdtx =  qltgim(i)*dts
!
!        Calculation of coefficients in 'advection equation'.
!        [ Doc. S-FO-001.5KV / Eq. 22-4/22-25 ]
!
      aa(j) = dble(  psi1*at2(i)*dx - theta*q2(i)*dts&
      &- theta*psi1*qltdtx )
      ba(j) = dble(  theta*dts)
      da(j) = dble(  psi*at2(ip)*dx + theta*q2(ip)*dts&
      &- theta*psi*qltdtx )
      ea(j) =      - ba(j)
      ra(j) = dble(  csa1(i) * (psi1*at1(i)*dx + theta1*q1(i)*dts&
      &+ theta1*psi1*qltdtx)&
      &- csd1(i)*theta1*dts&
      &+ csa1(ip) * (psi*at1(ip)*dx - theta1*q1(ip)*dts&
      &+ theta1*psi*qltdtx)&
      &+ csd1(ip)*theta1*dts&
      &+ source(i)*dts )
!
!        Calculation of coefficients in diffusion equation.
!        [ Doc. S-FO-001.5KV / Eq. 22-5/22-25 ]
!
      fdjd  = dble(-(af(i)*disgr(i) + af(ip)*disgr(ip)) )
      fd(j) =        fdjd
      gdjd  = dble(- dx )
      gd(j) =        gdjd
      md(j) =      - fdjd
      nd(j) =        gdjd
      rd(j) =        0.d0
10 continue
!
end
