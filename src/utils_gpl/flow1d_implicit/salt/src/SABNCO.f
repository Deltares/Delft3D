      subroutine sabnco(nx     ,s      ,ix     ,nnode  ,nboun ,ngrid ,
     &                  node   ,q2     ,sbdpar ,sbdscr ,r1    ,f1    ,
     &                  v1     ,r2     ,f2     ,v2     ,alfa  ,beta  ,
     &                  gamma )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SABNCO (SAlt BouNdary COefficients)
c
c Module description: The boundary coefficients ALPHA, BETA and GAMMA
c                     will be computed for internal boundaries (=nodes)
c                     and external boundaries.
c
c                     The boundary conditions are expressed in following
c                     form:
c
c                     ALPHA * cs + BETA * c's = GAMMA
c
c                     In the formulas 22-18 and 22-19 of the functional
c                     design [S-FO-001.5KV] a term P1 occurs. Because in
c                     the functional design (par. 22.1.1) option 1 has
c                     been chosen for P1 and P2 the term P1 has a con-
c                     stant value of 1.
c
c Precondition:       The concentrations at inflow boundaries (option 1
c                     or 2) are already calculated by SABOUN.
c
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 17 alfa              O  Contribution of one branch in the coefficient
c                         of dh or cs in the current node equation.
c 18 beta              O  Contribution of one branch in the coefficient
c                         of dq or c's in the current node equation.
c 12 f1(ngrid)         I  f1-coefficients (2*i-1,N) of set of branch
c                         equations. One value per grid point.
c 15 f2(ngrid)         I  f2-coefficients (2*i,N) of set of branch equa-
c                         tions. One value per grid point.
c 19 gamma             O  Contribution of one branch in the right hand
c                         side of the current node equation.
c  3 ix                I  -
c  5 nboun             I  Number of boundary nodes.
c  6 ngrid             I  Number of grid points in network.
c  4 nnode             I  Number of nodes.
c  7 node(4,nnode)     I  Definition of nodes:
c                         (1,i) = Type of node i:
c                                 cintnd (1) : Internal node
c                                 chbou  (2) : H-boundary
c                                 cqbou  (3) : Q-boundary
c                                 cqhbou (4) : QH-boundary
c                                 chqbou (5) : HQ-boundary
c                         (2,i) = Gridpoint in case of boundary, else
c                                 undefined.
c                         (3,i) = Station number for boundary, undefined
c                                 for internal nodes:
c                                 HQ, H-boundary: station nr H-station.
c                                 QH, Q-boundary: station nr Q-station.
c                         (4,i) = Boundary number in case of boundary.
c  1 nx                I  Node number at begin or end of branch.
c  8 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 11 r1(ngrid)         I  r1-coefficients (2*i-1,1) of set of branch
c                         equations. One value per grid point.
c 14 r2(ngrid)         I  r2-coefficients (2*i,1) of set of branch equa-
c                         tions. One value per grid point.
c  2 s                 I  Indicates position of node:
c                         +1  :   Begin of branch
c                         -1  :   End of branch
c  9 sbdpar(5,nboun)   I  Definition of salt boundary conditions:
c                         (1,i) = Option for boundary condition:
c                                 csbusr (1) : User specified concen-
c                                              tration at inflow
c                                 csbthh (2) : Thatcher-Harleman for-
c                                              mulation at inflow
c                                 csbflx (3) : Zero flux
c                         (2,i) = Location (node number).
c                         (3,i) = Branch number that is connected.
c                         (4,i) = Table number (Options 1 and 2).
c                         (5,i) = T0 period for option 2, else undefi-
c                                 ned.
c 10 sbdscr(3,nboun)   I  Intermediate results at salt boundaries:
c                         (1,i) = Last time of outflow (option = 2)
c                         (2,i) = Concentration at last time of outflow
c                                 (option = 2)
c                         (3,i) = Concentration at inflow (time n+1) if
c                                 option is 1 or 2
c 13 v1(ngrid)         I  Right-hand-sides (2*i-1) of set of branch
c                         equations. One value per grid point.
c 16 v2(ngrid)         I  Right-hand-sides (2*i) of set of branch equa-
c                         tions. One value per grid point.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sabnco.pf,v $
c Revision 1.8  1999/03/15  15:53:17  kuipe_j
c tabs removed
c
c Revision 1.7  1996/12/04  12:00:38  kuipe_j
c declarations / undefined vars
c
c Revision 1.6  1995/10/18  09:00:12  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.5  1995/08/30  12:37:12  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.4  1995/08/23  14:29:36  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:56:02  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:05:51  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:32  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:33:20  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:11  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer nx  ,ix     ,nnode  ,nboun ,ngrid
      integer node(4,nnode)
      real    s
      real    sbdpar(5,nboun) ,sbdscr(3,nboun)
      double precision     alfa   ,beta    ,gamma
      double precision
     &        q2  (ngrid) ,
     &        r1  (ngrid) ,f1    (ngrid)   ,v1    (ngrid)  ,
     &        r2  (ngrid) ,f2    (ngrid)   ,v2    (ngrid)
c
c     Declaration of local variables
c
      integer  iboun ,ix1
      logical  DEQUAL
      external DEQUAL
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     ix  = grid point number at node nx
c     ix1 = number of next grid cel in same branch
c
c     Update coefficients for node nx.
c
      if (node(1,nx) .eq. cintnd) then
c
c        Internal boundary.
c        [ Doc: S-FO-001.5KV  Eq. 22-17 ]
c
         alfa  = 0.d0
         beta  = dble(s)
         gamma = 0.d0
      else
c
c        External boundary.
c
         iboun = node(4,nx)
         if (int(sbdpar(1,iboun)) .eq. csbflx) then
c
c           Zero flux condition.
c           [ Doc: S-FO-001.5KV  Eq. 22-23 ]
c
            alfa  = dble(q2(ix))
            beta  = -1.0d0
            gamma = 0.d0
         else if (q2(ix)*s .gt. 0.) then
c
c           Inflow at branch begin or end.
c           [ Doc: S-FO-001.5KV  Eq.  22-20 ]
c
            alfa  = 1.d0
            beta  = 0.d0
            gamma = dble(sbdscr(3,iboun))
         else if (s .gt. 0.) then
c
c           Outflow at branch begin.
c           [ Doc: S-FO-001.5KV  Eq.  22-18 ]
c
            ix1   = ix+1
            if (DEQUAL(f1(ix),0.d0)) then
               alfa  = r1(ix) - r1(ix1)
               beta  = 0.d0
               gamma = v1(ix) - v1(ix1)
            else
               alfa  = f1(ix1)*r1(ix) - f1(ix)*r1(ix1)
               beta  = f1(ix1)-f1(ix)
               gamma = f1(ix1)*v1(ix) - f1(ix)*v1(ix1)
            endif
         else if (s .lt. 0.) then
c
c           Outflow at branch end.
c           [ Doc: S-FO-001.5KV  Eq.  22-19 ]
c
            ix1   = ix-1
            if (DEQUAL(r2(ix1),0.d0)) then
               alfa  = f2(ix1) - f1(ix1)
               beta  = 0.d0
               gamma = v2(ix1) - v1(ix1)
            else
               alfa  = r1(ix1)*f2(ix1) - r2(ix1)*f1(ix1)
               beta  = r1(ix1)-r2(ix1)
               gamma = r1(ix1)*v2(ix1) - r2(ix1)*v1(ix1)
            endif
         endif
      endif
c
      end
