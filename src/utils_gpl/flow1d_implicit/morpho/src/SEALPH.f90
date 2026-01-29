subroutine sealph (igr    ,igp    ,igm    ,ngrid  ,maxlev ,g     ,&
&relden ,u12    ,c12    ,d5012  ,rc     ,x     ,&
&wf     ,hlev   ,h2     ,qs     ,eb     ,tanalf)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEALPH (SEdiment ALPHa calculation)
!
! Module description: Calculate the tangent of the angle ALPHA of the
!                     sediment exchange in a grid point. ALPHA is the
!                     angle between the direction of the sediment trans-
!                     port in the exchange region and the axis of the
!                     branch.
!
!                     The calculation is according to:
!                     [ Doc. S-FO-002.2KV / Eq. 6.15 - 6.17 ]
!
! Precondition:       Equal grid step sizes.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 c12               I  Chezy value in exchange region
! 10 d5012             I  D50 in exchange region
! 17 eb(7)             I  E coefficients for a sedredge branch:
!                         (1) = E1 coefficient.
!                         (2) = E2 coefficient.
!                         (3) = E3 coefficient.
!                         (4) = E4 coefficient.
!                         (5) = E5 coefficient.
!                         (6) = E6 coefficient.
!                         (7) = E7 coefficient.
!  6 g                 I  Acceleration of gravity.
! 15 h2(ngrid)         I  Water level in every grid point at time
!                         t(n+1).
! 14 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  3 igm               I  igr-1 (at branch begin = igr)
!  2 igp               I  igr+1 (at branch end = igr)
!  1 igr               I  Actual grid point number.
!  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  4 ngrid             I  Number of grid points in network.
! 16 qs(ngrid,2)       I  Flow in every grid point per section:
!                         (i,1) = Through grid point i of main channel.
!                         (i,2) = Through grid point i of sub section 1.
! 11 rc(nrc)           I  For sedredge branches this array contains the
!                         river bend curvature for each gridpoint in the
!                         sedredge branches (positive for a right bend).
!                         For each sedredge branch the starting address
!                         of that branch is stored in the array sedinf.
!  7 relden            I  relative density
! 18 tanalf            O  Tangent of angle between transport in exchange
!                         region and branch axis.
!  8 u12               I  velocity in exchange region
! 13 wf(ngrid)         I  Actual flow width at every grid point.
! 12 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sealph.pf,v $
! Revision 1.2  1995/05/30  07:07:08  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:12  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:27  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:18  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    igr   ,igp    ,igm     ,ngrid     ,maxlev
   real       g     ,relden ,u12     ,c12       ,d5012  ,rc
   real       wf(ngrid)     ,x   (ngrid)        ,&
   &qs  (ngrid,2) ,eb(7)
!
   double precision tanalf, hlev(ngrid,maxlev), h2(ngrid)
!
!     Declaration of local parameters
!
   double precision d12   ,d12m  ,dz12  ,dz12p  ,dz12m ,&
   &dq1dx ,d2qdx2,dd12dx,d2zdx2,dzbdy  ,dxi   ,&
   &dx2i  ,p     ,shiel
   double precision term1 ,term2 ,term3 ,term4 ,term5  ,term6
!
!     Constants
!
   double precision karman
   data             karman /0.4D0/

!
!     Calculate depths ,z1-z2  and length step.
!
   d12   = dble(h2(igr) - (hlev(igr,1) + hlev(igr,2))*.5)
   d12m  = dble(h2(igm) - (hlev(igm,1) + hlev(igm,2))*.5)
   dz12  = dble(hlev(igr,1) - hlev(igr,2))
!
!     Second derivatives: D2(q1)/(Dx)2 ,D2(z1-z2)/(Dx)2
!
   if (igp-igm .eq. 1) then
!
!        Derivatives are zero for first and last gridpoints in branch.
!
      dxi    = dble(1.D0 / (x(igp) - x(igm)))
      dq1dx  = dble((qs(igp,1) - qs(igm,1)) * dxi)
      d2qdx2 = 0.D0
      d2zdx2 = 0.D0
   else
!
!        Equal grid sizes are assumed !
!
      dxi    = dble(1.D0 / (x(igr) - x(igm)))
      dx2i   = dble(dxi*dxi)
      dq1dx  = dble((qs(igr,1) - qs(igm,1)) * dxi)
      d2qdx2 = dble((qs(igp,1) - 2.D0*qs(igr,1) + qs(igm,1)) * dx2i)
      dz12p  = dble(hlev(igp,1) - hlev(igp,2))
      dz12m  = dble(hlev(igm,1) - hlev(igm,2))
      d2zdx2 = dble((dz12p - 2.D0*dz12 + dz12m) * dx2i)
   endif
!
!     First deriviates: Dd12/Dx
!
   dd12dx = (d12 - d12m) * dxi
!
!     Calculate DzDy and factor P.
!
   dzbdy = dble(2.D0 * dz12 / (wf(igr) * eb(7)))
!
   p = dble((2.D0/(karman*karman)) * (1.D0-sqrt (g) / (c12*karman)))
!
!     Calculate the terms of factor tangens(Alpha).
!
   term1 = dble (1.d0 / (u12 * d12) * dq1dx)
   term2 = dble (eb(1) * p * d12 * rc)
   term3 = dble (eb(1) * p / u12)
   term4 = dble (d2qdx2 - dd12dx * dq1dx / d12)
   shiel = dble ((u12/c12)**2. / (relden * d5012))
   term5 = dble (eb(2)/(shiel**eb(3) * (d12 / d5012)**eb(4)) * dzbdy)
   term6 = dble (eb(6) * wf(igr) * c12*c12 / g * d2zdx2)
!
!     Assemble tangens(Alpha)
!
   tanalf = term1 - term2 + term3 * term4 - term5 + term6
!
end
