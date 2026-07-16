subroutine FLBNCO(nx     ,ix     ,s      ,nnode  ,node   ,&
&ngrid  ,h1     ,h      ,q1     ,q      ,&
&maxtab ,ntabm  ,ntab   ,table  ,&
&hstat  ,hbdpar ,qstat  ,qbdpar ,&
&qtyp   ,alfa   ,beta   ,gamma  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLBNCO (FLow BouNdary COefficients)
!
! Module description: In subroutine FLBNCO the boundary coefficients
!                     alfa, beta and gamma will be computed for internal
!                     boundaries (=nodes) and external boundaries.
!
!                     The boundary conditions are expressed in following
!                     form:
!
!                     alfa * delta H + beta * delta Q = gamma
!
!                     For the external boundary conditions these coeffi-
!                     cients follow from formulaes (8-2) until (8-4) in
!                     S-FO-001.5KV. Values for h and Q are already cal-
!                     culated by FLBOUN.
!
!                     For internal boundary conditions (at nodes) the
!                     coefficients can be computed from formulae (8-5)
!                     in S-FO-001.5KV.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 19 alfa              IO Contribution of one branch in the coefficient
!                         of dh or cs in the current node equation.
! 20 beta              IO Contribution of one branch in the coefficient
!                         of dq or c's in the current node equation.
! 21 gamma             O  Contribution of one branch in the right hand
!                         side of the current node equation.
!  7 h1(ngrid)         I  Water level in every grid point at time t(n).
!  8 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
! 16 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
!                         (1,i) = Location [grid point] for H-station.
!                         (2,i) = Type of condition
!                                 cbftim (1) : h = f(t)
!                                 cbfqoh (2) : h = h(Q)
!                                 cbfour (3) : h = fourier
!                                 cbtidl (4) : h = tidal components
!                         (3,i) = Table number for f(t), h(Q), fourier
!                                 or tidal components table.
! 15 hstat(nhstat)     I  Actual water level in every H-station.
!  2 ix                I  Grid point at begin or end of branch.
! 11 maxtab            I  Maximum number of defined tables.
!  6 ngrid             I  Number of grid points in network.
!  4 nnode             I  Number of nodes.
!  5 node(4,nnode)     I  Definition of nodes:
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
! 13 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
! 12 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  1 nx                I  Node number at begin or end of branch.
!  9 q1(ngrid)         I  Discharge in every grid point at time t(n).
! 10 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
! 18 qbdpar(3,nqstat)  I  Hydrodynamic conditions for Q-stations:
!                         (1,i) = Location [grid point] for Q-station.
!                         (2,i) = Type of condition
!                                 cbftim (1) : Q = f(t)
!                                 cbfqoh (2) : Q = Q(h)
!                                 cbfour (3) : Q = fourier
!                                 cbtidl (4) : Q = tidal components
!                         (3,i) = Table number for f(t), Q(h), fourier
!                                 or tidal components table.
! 17 qstat(nqstat)     I  Actual discharge in every Q-station.
!  3 s                 I  Indicates position of node:
!                         +1  :   Begin of branch
!                         -1  :   End of branch
! 14 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flbnco.pf,v $
! Revision 1.9  1999/03/15  15:49:33  kuipe_j
! tabs removed
!
! Revision 1.8  1997/05/26  07:44:43  kuipe_j
! Small changes
!
! Revision 1.7  1996/05/28  13:34:11  kuipe_j
! Q(n) instead of Q(*)
!
! Revision 1.6  1995/11/21  11:07:45  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.5  1995/09/22  10:00:58  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:10:49  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.3  1994/11/28  08:37:17  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:30:32  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Declaration of Parameters:
!
   integer nx, ix, nnode, ngrid, maxtab, ntabm
   integer node(4,nnode), ntab(4,maxtab)
   integer hbdpar(3,*), qbdpar(3,*)
   real    qtyp, s
   double precision h1(ngrid), h(ngrid), q1(ngrid), q(ngrid)
   real    hstat(*), qstat(*)
   real    table(ntabm)
   double precision alfa, beta, gamma
!
!     Declaration of local variables
!
   integer igr, istat, itab
   real    dh, dq, hqit, hqit1, hqit2, qhit, qhit1 ,qhit2
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
!     nx = node number at begin or end of branch
!     ix = grid point number at node nx
!
!     *************
!     *  NODE nx  *
!     *************
!
!     *********************
!     * Internal boundary *
!     *********************
!
   if (node(1,nx) .eq. cintnd) then
!
!        Update coefficients for node nx
!
!        Remark:
!        Positive branch direction goes from node n1 to node n2.
!        So, for directional unit vector [s] along branch holds : s = +1
!
!        Doc: S-FO-001.5KV  Eq. 8-6
!
      alfa  = 0.0D0
      beta  = dble (s)
      gamma = dble (-s * q1(ix))
!
!     *********************
!     * External boundary *
!     *********************
!
   else if (node(1,nx) .eq. chbou) then
!
!        H-boundary
!
      istat = node(3,nx)
!
!        Doc: S-FO-001.5KV  Eq. 8-2
!
      alfa  = 1.0D0
      beta  = 0.0D0
      gamma = dble (hstat(istat) - h1(ix))

   else if (node(1,nx) .eq. cqbou) then
!
!        Q-boundary
!
!        Doc: S-FO-001.5KV  Eq. 8-3
!
      istat = node(3,nx)
!
      alfa  = 0.0D0
      beta  = 1.0D0
      gamma = dble (qstat(istat) - q1(ix))

   else if (node(1,nx) .eq. cqhbou) then
!
!        QH-boundary
!
!        Doc: S-FO-001.5KV  Eq. 8-4
!
      igr   = node(2,nx)
      istat = node(3,nx)
      itab  = qbdpar(3,istat)
!
!        Determine differentiation increment
!
      dh = max(min(.0001*qtyp,.01),.001)
!
!        Determine qhit1 = Q(h* - dh)
!
      call INTTAB (ntab(1,itab), ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &dble(h(igr)-dh), qhit1      )
!
!        Determine qhit2 = Q(h* + dh)
!
      call INTTAB (ntab(1,itab), ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &dble(h(igr)+dh), qhit2   )
!
      qhit  = 0.5 * (qhit1+qhit2)
      alfa  = dble (( qhit2 - qhit1 ) / (dh*2.) )
      beta  = -1.0D0
      gamma = dble ( -qhit + (h(igr) - h1(igr)) * alfa + q1(igr))

   else if (node(1,nx) .eq. chqbou) then
!
!        HQ-boundary
!
!        Doc: S-FO-001.5KV  Eq. 8-4
!
      igr   = node(2,nx)
      istat = node(3,nx)
      itab  = hbdpar(3,istat)
!
!        Determine differentiation increment
!
      dq = max(min(.001*qtyp,1.0),.001)
!
!        Determine hqit1 = h(Q*-dq)
!
      call INTTAB (ntab(1,itab), ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &dble(q(igr)-dq), hqit1      )
!
!        Determine hqit2 = h(Q* + dQ)
!
      call INTTAB (ntab(1,itab), ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &dble(q(igr)+dq), hqit2  )
!
      hqit  = 0.5 * (hqit1+hqit2)
!
      alfa  = -1.0D0
      beta  = dble (( hqit2 - hqit1 ) / (dq*2.) )
      gamma = dble ( -hqit + (q(igr) - q1(igr)) * beta + h1(igr))

   endif
!
end
