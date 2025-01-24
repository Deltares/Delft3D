subroutine sabnco(nx     ,s      ,ix     ,nnode  ,nboun ,ngrid ,&
&node   ,q2     ,sbdpar ,sbdscr ,r1    ,f1    ,&
&v1     ,r2     ,f2     ,v2     ,alfa  ,beta  ,&
&gamma )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SABNCO (SAlt BouNdary COefficients)
!
! Module description: The boundary coefficients ALPHA, BETA and GAMMA
!                     will be computed for internal boundaries (=nodes)
!                     and external boundaries.
!
!                     The boundary conditions are expressed in following
!                     form:
!
!                     ALPHA * cs + BETA * c's = GAMMA
!
!                     In the formulas 22-18 and 22-19 of the functional
!                     design [S-FO-001.5KV] a term P1 occurs. Because in
!                     the functional design (par. 22.1.1) option 1 has
!                     been chosen for P1 and P2 the term P1 has a con-
!                     stant value of 1.
!
! Precondition:       The concentrations at inflow boundaries (option 1
!                     or 2) are already calculated by SABOUN.
!
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 17 alfa              O  Contribution of one branch in the coefficient
!                         of dh or cs in the current node equation.
! 18 beta              O  Contribution of one branch in the coefficient
!                         of dq or c's in the current node equation.
! 12 f1(ngrid)         I  f1-coefficients (2*i-1,N) of set of branch
!                         equations. One value per grid point.
! 15 f2(ngrid)         I  f2-coefficients (2*i,N) of set of branch equa-
!                         tions. One value per grid point.
! 19 gamma             O  Contribution of one branch in the right hand
!                         side of the current node equation.
!  3 ix                I  -
!  5 nboun             I  Number of boundary nodes.
!  6 ngrid             I  Number of grid points in network.
!  4 nnode             I  Number of nodes.
!  7 node(4,nnode)     I  Definition of nodes:
!                         (1,i) = Type of node i:
!                                 cintnd (1) : Internal node
!                                 chbou  (2) : H-boundary
!                                 cqbou  (3) : Q-boundary
!                                 cqhbou (4) : QH-boundary
!                                 chqbou (5) : HQ-boundary
!                         (2,i) = Gridpoint in case of boundary, else
!                                 undefined.
!                         (3,i) = Station number for boundary, undefined
!                                 for internal nodes:
!                                 HQ, H-boundary: station nr H-station.
!                                 QH, Q-boundary: station nr Q-station.
!                         (4,i) = Boundary number in case of boundary.
!  1 nx                I  Node number at begin or end of branch.
!  8 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 11 r1(ngrid)         I  r1-coefficients (2*i-1,1) of set of branch
!                         equations. One value per grid point.
! 14 r2(ngrid)         I  r2-coefficients (2*i,1) of set of branch equa-
!                         tions. One value per grid point.
!  2 s                 I  Indicates position of node:
!                         +1  :   Begin of branch
!                         -1  :   End of branch
!  9 sbdpar(5,nboun)   I  Definition of salt boundary conditions:
!                         (1,i) = Option for boundary condition:
!                                 csbusr (1) : User specified concen-
!                                              tration at inflow
!                                 csbthh (2) : Thatcher-Harleman for-
!                                              mulation at inflow
!                                 csbflx (3) : Zero flux
!                         (2,i) = Location (node number).
!                         (3,i) = Branch number that is connected.
!                         (4,i) = Table number (Options 1 and 2).
!                         (5,i) = T0 period for option 2, else undefi-
!                                 ned.
! 10 sbdscr(3,nboun)   I  Intermediate results at salt boundaries:
!                         (1,i) = Last time of outflow (option = 2)
!                         (2,i) = Concentration at last time of outflow
!                                 (option = 2)
!                         (3,i) = Concentration at inflow (time n+1) if
!                                 option is 1 or 2
! 13 v1(ngrid)         I  Right-hand-sides (2*i-1) of set of branch
!                         equations. One value per grid point.
! 16 v2(ngrid)         I  Right-hand-sides (2*i) of set of branch equa-
!                         tions. One value per grid point.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sabnco.pf,v $
! Revision 1.8  1999/03/15  15:53:17  kuipe_j
! tabs removed
!
! Revision 1.7  1996/12/04  12:00:38  kuipe_j
! declarations / undefined vars
!
! Revision 1.6  1995/10/18  09:00:12  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.5  1995/08/30  12:37:12  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:36  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:56:02  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:05:51  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:32  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:33:20  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:11  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer nx  ,ix     ,nnode  ,nboun ,ngrid
   integer node(4,nnode)
   real    s
   real    sbdpar(5,nboun) ,sbdscr(3,nboun)
   double precision     alfa   ,beta    ,gamma
   double precision&
   &q2  (ngrid) ,&
   &r1  (ngrid) ,f1    (ngrid)   ,v1    (ngrid)  ,&
   &r2  (ngrid) ,f2    (ngrid)   ,v2    (ngrid)
!
!     Declaration of local variables
!
   integer  iboun ,ix1
   logical  DEQUAL
   external DEQUAL
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     ix  = grid point number at node nx
!     ix1 = number of next grid cel in same branch
!
!     Update coefficients for node nx.
!
   if (node(1,nx) .eq. cintnd) then
!
!        Internal boundary.
!        [ Doc: S-FO-001.5KV  Eq. 22-17 ]
!
      alfa  = 0.d0
      beta  = dble(s)
      gamma = 0.d0
   else
!
!        External boundary.
!
      iboun = node(4,nx)
      if (int(sbdpar(1,iboun)) .eq. csbflx) then
!
!           Zero flux condition.
!           [ Doc: S-FO-001.5KV  Eq. 22-23 ]
!
         alfa  = dble(q2(ix))
         beta  = -1.0d0
         gamma = 0.d0
      else if (q2(ix)*s .gt. 0.) then
!
!           Inflow at branch begin or end.
!           [ Doc: S-FO-001.5KV  Eq.  22-20 ]
!
         alfa  = 1.d0
         beta  = 0.d0
         gamma = dble(sbdscr(3,iboun))
      else if (s .gt. 0.) then
!
!           Outflow at branch begin.
!           [ Doc: S-FO-001.5KV  Eq.  22-18 ]
!
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
!
!           Outflow at branch end.
!           [ Doc: S-FO-001.5KV  Eq.  22-19 ]
!
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
!
end
