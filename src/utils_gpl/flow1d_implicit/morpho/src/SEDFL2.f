      subroutine sedfl2 (ibr    ,linc   ,nbran  ,ngrid  ,maxlev ,g     ,
     &                   branch ,afs    ,wfs    ,hlev   ,x      ,h2    ,
     &                   q2     ,cs     ,rs     ,qs     )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEDFL2 (SEdiment Distribute FLows 2)
c
c Module description: Distribute total flow over both channels in all
c                     grid points but the first upstream point, accor-
c                     ding to the Sedredge option.
c
c                     For this, the following equations are solved:
c                     [ Doc. S-FO-002.2KV / Eq. 6.1 and 6.2 ]
c
c Precondition:       No change of flow direction or zero flow in any
c                     grid point.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 afs(ngrid,2)      I  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c  7 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 14 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
c                         sub 1, sub 2) for every grid point.
c  6 g                 I  Acceleration of gravity.
c 12 h2(ngrid)         I  Water level in every grid point at time
c                         t(n+1).
c 10 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  1 ibr               I  Number of actual branch.
c  2 linc              I  Indicates the direction of looping through a
c                         branch:
c                         +1 : Positive
c                         -1 : Negative
c  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  3 nbran             I  Number of branches.
c  4 ngrid             I  Number of grid points in network.
c 13 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 16 qs(ngrid,2)       IO Flow in every grid point per section:
c                         (i,1) = Through grid point i of main channel.
c                         (i,2) = Through grid point i of sub section 1.
c 15 rs(ngrid,3)       I  Actual hydraulic radius in a section (main,
c                         sub 1, sub 2) for every grid point.
c  9 wfs(ngrid,2)      I  Actual flow width per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c 11 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sedfl2.pf,v $
c Revision 1.2  1995/05/30  07:07:17  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:19  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:42  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:19  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    ibr   ,linc   ,nbran   ,ngrid  ,maxlev
      integer    branch(4,nbran)
      real       g
      real       afs   (ngrid,2 ) ,wfs(ngrid,2)  ,x     (ngrid), 
     &           cs    (ngrid,3)  ,rs (ngrid,3)  ,qs  (ngrid,2)
	double precision hlev(ngrid,maxlev),h2(ngrid),q2(ngrid) 
c
c     Declaration of local parameters
c
      integer    ig1   ,ig2   ,iga   ,igb
      real       alf1a ,alf1b ,alf2a ,alf2b ,con2  ,
     &           con3  ,c1    ,c2    ,a1    ,a2     ,w1    ,w2    ,
     &           d1a   ,d1b   ,d2a   ,d2b   ,d1     ,d2    ,dd1dx ,
     &           dd2dx ,dw1dx ,dw2dx ,dxi   ,d1dx   ,d1ow1 ,d2ow2 ,
     &           a1a22 ,q1a   ,qtiph  ,q1iph ,qti   ,r1    ,r2    ,
     &           y     ,d2dx  ,gdacr1 ,gdacr2,da122 ,n

      double precision x1, x2, a, b, c, disc, term1, term2, term3, 
     &                 term4, alpha1 ,alpha2
c
c     Constants
c
      real       karman
      data       karman /0.4/
c
c     Processing will be done from upstream to down stream.
c     determine loop indices.
c
      if (linc .eq. 1) then
         ig1 = branch(3,ibr)+1
         ig2 = branch(4,ibr)
      else
         ig1 = branch(4,ibr)-1
         ig2 = branch(3,ibr)
      endif

c
c     Compute alpha 1 and alpha 2 for first gridpoint.
c     (main and subsection)
c     [Doc. S-FO-002.2KV  Eq. 6.1, 6.2 ]
c
      con2 = 3. * (sqrt(g) / karman) ** 2.
      con3 = 2. * (sqrt(g) / karman) ** 3.
c
      igb   = ig1 - linc
      alf1b = 1. + (con2 - con3/cs(igb,1)) / cs(igb,1)**2.
      alf2b = 1. + (con2 - con3/cs(igb,2)) / cs(igb,2)**2.
c
      do 10 igb = ig1,ig2,linc
c
c        igb = current grid point (downstream)
c        iga = previous grid point (upstream)
c
         iga = igb - linc
c
c        Compute alpha 1 and alpha 2 for gridpoint (a and) b.
c        (main and subsection)
c
         alf1a  = alf1b
         alf2a  = alf2b
         alf1b  = 1. + (con2 - con3/cs(igb,1)) / cs(igb,1)**2.
         alf2b  = 1. + (con2 - con3/cs(igb,2)) / cs(igb,2)**2.
c
c        Calculate depths
c
         d1a = h2(iga)-hlev(iga,1)
         d2a = h2(iga)-hlev(iga,2)
         d1b = h2(igb)-hlev(igb,1)
         d2b = h2(igb)-hlev(igb,2)
c
c        Compute variables at level (a+b)/2.
c
         alpha1 = dble ( (alf1b + alf1a) *.5 * real(linc) )
         alpha2 = dble ( (alf2b + alf2a) *.5 * real(linc) )
c
c        Alpha is multiplied by sign of discharge.
c
         d1 = (d1b + d1a) * .5
         d2 = (d2b + d2a) * .5
c
         r1 = (rs(igb,1) + rs(iga,1)) * .5
         r2 = (rs(igb,2) + rs(iga,2)) * .5
c
         c1 = (cs(igb,1) + cs(iga,1)) * .5
         c2 = (cs(igb,2) + cs(iga,2)) * .5
c
         a1 = (afs(igb,1) + afs(iga,1)) * .5
         a2 = (afs(igb,2) + afs(iga,2)) * .5
c
         w1 = (wfs(igb,1) + wfs(iga,1)) * .5
         w2 = (wfs(igb,2) + wfs(iga,2)) * .5
c
c        Get discharges and calculate Qt on level (a+b)/2.
c
         q1a    = qs(iga,1)
         qti    = q2(iga)
         qtiph  = (q2(iga)+q2(igb)) *.5
c
c        Computation of derivatives.
c
         dxi    = 1. / (x(igb) - x(iga))
         dd1dx  = (d1b - d1a) * dxi
         dd2dx  = (d2b - d2a) * dxi
c
         dw1dx  = (wfs(igb,1) - wfs(iga,1)) * dxi
         dw2dx  = (wfs(igb,2) - wfs(iga,2)) * dxi
c
c        Computation of A,B,C
c        [Doc. S-FO-002.2KV  Eq. 6.3, 6.6 ]
c        Calculate auxiliary variables first.
c
         a1a22  = (a1 / a2)**2.
         y      = 2. * (1. - a1a22)
         d1dx   = d1 * dxi
         d2dx   = d2 * dxi
c
         n      = (w1*d2) + (w2*d1)
c
         gdacr1 = (g * d1) / (alpha1*c1*c1*r1)
         gdacr2 = (g * d2) / (alpha2*c2*c2*r2)
c
         d1ow1  = d1 / w1
         d2ow2  = d2 / w2
c
         da122  = (d1/d2) * a1a22
c
c        TERM a
c
         a = dble ( y*d1dx - d1ow1*dw1dx + gdacr1 - dd1dx +
     +              da122 * ( d2ow2 * dw2dx - gdacr2 + dd2dx )
     +            )
c
c        TERM b
c
         term1 = dble ( (d2*a2*a2*dxi) / (a1*n) +
     +                  (d2*a2   *dxi) / n      +
     +                  (2.0d0*d2*dxi)
     +                )
c
         term2 = dble ( -1.0d0/n - a1/(a2*n) - a1/(a2*a2))
c
         b = dble ( 2.d0*da122 * ( - (d2ow2*dw2dx) + gdacr2 -
     +                             dd2dx + term1 ) * qtiph  +
     +              2.d0*d1dx * ( q1a * ( a1a22 - 1.0d0 ) +
     +                            qti * a1 * term2 )
     +            )
c
c        TERM C
c
         term1 = dble ( 2.0d0 * d2 * a2 * dxi / n )
c
         term2 = dble ( 2.0d0 * d1 * a1 * a1 * dxi )
c
         term3 = dble ( (qtiph*qti - qtiph*q1a) / (a2*a2) )
c
         term4 = dble ( (qtiph*qti) / (n*a2) )
c
         c = dble ( da122 * ( d2ow2*dw2dx - gdacr2 + dd2dx -
     +                        2.0d0*d2dx - term1) * qtiph**2 +
     +              term2 * ( term3 + term4 )
     +            )

c
c                      2
c        Solution of Ax  + Bx + C = 0
c
         if (abs(a) .lt. 1.d-10) then
            x1 = - c / b
            x2 = x1
         else
            disc = sqrt(b*b - 4.d0*a*c)
            x1   = (-b + disc) / (2.d0*a)
            x2   = (-b - disc) / (2.d0*a)
         endif
c
c        Solution is smallest difference with calculated value in prior
c        space step.
c
         if (abs(x1 - q1a) .lt. abs(x2 - q1a)) then
            q1iph = real(x1)
         else
            q1iph = real(x2)
         endif
c
c        Calculate new distribution on level b for channel 1 and 2.
c        [Doc. S-FO-002.2KV  Eq. 6.4, 6.5 ]
c
         qs(igb,1) = 2.*q1iph - q1a
         qs(igb,2) = q2(igb) - qs(igb,1)
c
   10 continue
c
      end
