subroutine sedfl2 (ibr    ,linc   ,nbran  ,ngrid  ,maxlev ,g     ,&
&branch ,afs    ,wfs    ,hlev   ,x      ,h2    ,&
&q2     ,cs     ,rs     ,qs     )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEDFL2 (SEdiment Distribute FLows 2)
!
! Module description: Distribute total flow over both channels in all
!                     grid points but the first upstream point, accor-
!                     ding to the Sedredge option.
!
!                     For this, the following equations are solved:
!                     [ Doc. S-FO-002.2KV / Eq. 6.1 and 6.2 ]
!
! Precondition:       No change of flow direction or zero flow in any
!                     grid point.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 afs(ngrid,2)      I  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
!  7 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 14 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
!                         sub 1, sub 2) for every grid point.
!  6 g                 I  Acceleration of gravity.
! 12 h2(ngrid)         I  Water level in every grid point at time
!                         t(n+1).
! 10 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  1 ibr               I  Number of actual branch.
!  2 linc              I  Indicates the direction of looping through a
!                         branch:
!                         +1 : Positive
!                         -1 : Negative
!  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  3 nbran             I  Number of branches.
!  4 ngrid             I  Number of grid points in network.
! 13 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 16 qs(ngrid,2)       IO Flow in every grid point per section:
!                         (i,1) = Through grid point i of main channel.
!                         (i,2) = Through grid point i of sub section 1.
! 15 rs(ngrid,3)       I  Actual hydraulic radius in a section (main,
!                         sub 1, sub 2) for every grid point.
!  9 wfs(ngrid,2)      I  Actual flow width per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
! 11 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sedfl2.pf,v $
! Revision 1.2  1995/05/30  07:07:17  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:19  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:42  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer    ibr   ,linc   ,nbran   ,ngrid  ,maxlev
   integer    branch(4,nbran)
   real       g
   real       afs   (ngrid,2 ) ,wfs(ngrid,2)  ,x     (ngrid),&
   &cs    (ngrid,3)  ,rs (ngrid,3)  ,qs  (ngrid,2)
   double precision hlev(ngrid,maxlev),h2(ngrid),q2(ngrid)
!
!     Declaration of local parameters
!
   integer    ig1   ,ig2   ,iga   ,igb
   real       alf1a ,alf1b ,alf2a ,alf2b ,con2  ,&
   &con3  ,c1    ,c2    ,a1    ,a2     ,w1    ,w2    ,&
   &d1a   ,d1b   ,d2a   ,d2b   ,d1     ,d2    ,dd1dx ,&
   &dd2dx ,dw1dx ,dw2dx ,dxi   ,d1dx   ,d1ow1 ,d2ow2 ,&
   &a1a22 ,q1a   ,qtiph  ,q1iph ,qti   ,r1    ,r2    ,&
   &y     ,d2dx  ,gdacr1 ,gdacr2,da122 ,n

   double precision x1, x2, a, b, c, disc, term1, term2, term3,&
   &term4, alpha1 ,alpha2
!
!     Constants
!
   real       karman
   data       karman /0.4/
!
!     Processing will be done from upstream to down stream.
!     determine loop indices.
!
   if (linc .eq. 1) then
      ig1 = branch(3,ibr)+1
      ig2 = branch(4,ibr)
   else
      ig1 = branch(4,ibr)-1
      ig2 = branch(3,ibr)
   endif

!
!     Compute alpha 1 and alpha 2 for first gridpoint.
!     (main and subsection)
!     [Doc. S-FO-002.2KV  Eq. 6.1, 6.2 ]
!
   con2 = 3. * (sqrt(g) / karman) ** 2.
   con3 = 2. * (sqrt(g) / karman) ** 3.
!
   igb   = ig1 - linc
   alf1b = 1. + (con2 - con3/cs(igb,1)) / cs(igb,1)**2.
   alf2b = 1. + (con2 - con3/cs(igb,2)) / cs(igb,2)**2.
!
   do 10 igb = ig1,ig2,linc
!
!        igb = current grid point (downstream)
!        iga = previous grid point (upstream)
!
      iga = igb - linc
!
!        Compute alpha 1 and alpha 2 for gridpoint (a and) b.
!        (main and subsection)
!
      alf1a  = alf1b
      alf2a  = alf2b
      alf1b  = 1. + (con2 - con3/cs(igb,1)) / cs(igb,1)**2.
      alf2b  = 1. + (con2 - con3/cs(igb,2)) / cs(igb,2)**2.
!
!        Calculate depths
!
      d1a = h2(iga)-hlev(iga,1)
      d2a = h2(iga)-hlev(iga,2)
      d1b = h2(igb)-hlev(igb,1)
      d2b = h2(igb)-hlev(igb,2)
!
!        Compute variables at level (a+b)/2.
!
      alpha1 = dble ( (alf1b + alf1a) *.5 * real(linc) )
      alpha2 = dble ( (alf2b + alf2a) *.5 * real(linc) )
!
!        Alpha is multiplied by sign of discharge.
!
      d1 = (d1b + d1a) * .5
      d2 = (d2b + d2a) * .5
!
      r1 = (rs(igb,1) + rs(iga,1)) * .5
      r2 = (rs(igb,2) + rs(iga,2)) * .5
!
      c1 = (cs(igb,1) + cs(iga,1)) * .5
      c2 = (cs(igb,2) + cs(iga,2)) * .5
!
      a1 = (afs(igb,1) + afs(iga,1)) * .5
      a2 = (afs(igb,2) + afs(iga,2)) * .5
!
      w1 = (wfs(igb,1) + wfs(iga,1)) * .5
      w2 = (wfs(igb,2) + wfs(iga,2)) * .5
!
!        Get discharges and calculate Qt on level (a+b)/2.
!
      q1a    = qs(iga,1)
      qti    = q2(iga)
      qtiph  = (q2(iga)+q2(igb)) *.5
!
!        Computation of derivatives.
!
      dxi    = 1. / (x(igb) - x(iga))
      dd1dx  = (d1b - d1a) * dxi
      dd2dx  = (d2b - d2a) * dxi
!
      dw1dx  = (wfs(igb,1) - wfs(iga,1)) * dxi
      dw2dx  = (wfs(igb,2) - wfs(iga,2)) * dxi
!
!        Computation of A,B,C
!        [Doc. S-FO-002.2KV  Eq. 6.3, 6.6 ]
!        Calculate auxiliary variables first.
!
      a1a22  = (a1 / a2)**2.
      y      = 2. * (1. - a1a22)
      d1dx   = d1 * dxi
      d2dx   = d2 * dxi
!
      n      = (w1*d2) + (w2*d1)
!
      gdacr1 = (g * d1) / (alpha1*c1*c1*r1)
      gdacr2 = (g * d2) / (alpha2*c2*c2*r2)
!
      d1ow1  = d1 / w1
      d2ow2  = d2 / w2
!
      da122  = (d1/d2) * a1a22
!
!        TERM a
!
      a = dble ( y*d1dx - d1ow1*dw1dx + gdacr1 - dd1dx +&
      &da122 * ( d2ow2 * dw2dx - gdacr2 + dd2dx )&
      &)
!
!        TERM b
!
      term1 = dble ( (d2*a2*a2*dxi) / (a1*n) +&
      &(d2*a2   *dxi) / n      +&
      &(2.0d0*d2*dxi)&
      &)
!
      term2 = dble ( -1.0d0/n - a1/(a2*n) - a1/(a2*a2))
!
      b = dble ( 2.d0*da122 * ( - (d2ow2*dw2dx) + gdacr2 -&
      &dd2dx + term1 ) * qtiph  +&
      &2.d0*d1dx * ( q1a * ( a1a22 - 1.0d0 ) +&
      &qti * a1 * term2 )&
      &)
!
!        TERM C
!
      term1 = dble ( 2.0d0 * d2 * a2 * dxi / n )
!
      term2 = dble ( 2.0d0 * d1 * a1 * a1 * dxi )
!
      term3 = dble ( (qtiph*qti - qtiph*q1a) / (a2*a2) )
!
      term4 = dble ( (qtiph*qti) / (n*a2) )
!
      c = dble ( da122 * ( d2ow2*dw2dx - gdacr2 + dd2dx -&
      &2.0d0*d2dx - term1) * qtiph**2 +&
      &term2 * ( term3 + term4 )&
      &)

!
!                      2
!        Solution of Ax  + Bx + C = 0
!
      if (abs(a) .lt. 1.d-10) then
         x1 = - c / b
         x2 = x1
      else
         disc = sqrt(b*b - 4.d0*a*c)
         x1   = (-b + disc) / (2.d0*a)
         x2   = (-b - disc) / (2.d0*a)
      endif
!
!        Solution is smallest difference with calculated value in prior
!        space step.
!
      if (abs(x1 - q1a) .lt. abs(x2 - q1a)) then
         q1iph = real(x1)
      else
         q1iph = real(x2)
      endif
!
!        Calculate new distribution on level b for channel 1 and 2.
!        [Doc. S-FO-002.2KV  Eq. 6.4, 6.5 ]
!
      qs(igb,1) = 2.*q1iph - q1a
      qs(igb,2) = q2(igb) - qs(igb,1)
!
10 continue
!
end
