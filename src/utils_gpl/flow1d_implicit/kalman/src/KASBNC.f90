subroutine KASBNC(nx     ,s      ,nnode  ,node   ,&
&ngrid  ,h2     ,q2     ,&
&maxtab ,ntabm  ,ntab   ,table  ,&
&hbdpar ,qbdpar ,&
&alfa   ,beta   ,gamma  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KASBNC (KALman Substitute BouNdary Coefficients)
!
! Module description: In subroutine KASBNC the boundary coefficients
!                     ALFA, BETA and GAMMA will be computed for internal
!                     boundaries (=nodes) and external boundaries.
!
!                     For the external boundary conditions these coef-
!                     ficients follow from formulae (5-1) thr. (5-6) in
!                     [S-FO-004].
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 14 alfa              O  -
! 15 beta              O  -
! 16 gamma             O  -
!  6 h2(ngrid)         I  Water level in every grid point at time
!                         t(n+1).
! 12 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
!                         (1,i) = Location [grid point] for H-station.
!                         (2,i) = Type of condition
!                                 cbftim (1) : h = f(t)
!                                 cbfqoh (2) : h = h(Q)
!                                 cbfour (3) : h = fourier
!                                 cbtidl (4) : h = tidal components
!                         (3,i) = Table number for f(t), h(Q), fourier
!                                 or tidal components table.
!  8 maxtab            I  Maximum number of defined tables.
!  5 ngrid             I  Number of grid points in network.
!  3 nnode             I  Number of nodes.
!  4 node(4,nnode)     I  Definition of nodes:
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
! 10 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  9 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  1 nx                I  -
!  7 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 13 qbdpar(3,nqstat)  I  Hydrodynamic conditions for Q-stations:
!                         (1,i) = Location [grid point] for Q-station.
!                         (2,i) = Type of condition
!                                 cbftim (1) : Q = f(t)
!                                 cbfqoh (2) : Q = Q(h)
!                                 cbfour (3) : Q = fourier
!                                 cbtidl (4) : Q = tidal components
!                         (3,i) = Table number for f(t), Q(h), fourier
!                                 or tidal components table.
!  2 s                 I  -
! 11 table             P  -
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
! $Log: kasbnc.pf,v $
! Revision 1.3  1999/03/15  15:52:16  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:26  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:58  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Declaration of Parameters:
!
   integer nx, nnode, ngrid, maxtab, ntabm
   integer node(4,nnode), ntab(4,maxtab)
   integer hbdpar(3,*), qbdpar(3,*)
   real    s, table(ntabm)

   double precision h2(ngrid), q2(ngrid)
   double precision alfa, beta, gamma
!
!     Declaration of local variables
!
   integer igr, istat, itab
   real    dh, dq, hqit, hqitdq, qhit, qhitdh
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
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
!        Doc: S-FO-004.2PB  Eq. 6-4
!
      alfa  = 0.0D0
      beta  = s
      gamma = 0.0D0
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
!        Doc: S-FO-004.2PB  Eq. 5-3
!
      alfa  = 1.0D0
      beta  = 0.0D0
      gamma = 0.0D0

   else if (node(1,nx) .eq. cqbou) then
!
!        Q-boundary
!
!        Doc: S-FO-004.2PB  Eq. 5-4
!
      istat = node(3,nx)
!
      alfa  = 0.0D0
      beta  = 1.0D0
      gamma = 0.0D0

   else if (node(1,nx) .eq. cqhbou) then
!
!        QH-boundary
!
!        Doc: S-FO-004.2PB  Eq. 5-5
!
      igr   = node(2,nx)
      istat = node(3,nx)
      itab  = qbdpar(3,istat)
!
!        Determine qhit = Q(h2(igr))
!
      call INTTAB (ntab(1,itab), ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &h2(igr), qhit       )
!
!        Determine qhitdh = Q(h2(igr) + dh)
!
!        >>>>>> dh moet ergens een waarde meekrijgen !!!!!! <<<<<<
      dh = 0.01
      call INTTAB (ntab(1,itab), ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &(h2(igr)+dh), qhitdh  )
!
      alfa  = dble (( qhitdh - qhit ) / dh )
      beta  = -1.0D0
      gamma = 0.0D0

   else if (node(1,nx) .eq. chqbou) then
!
!        HQ-boundary
!
!        Doc: S-FO-004.2PB  Eq. 5-6
!
      igr   = node(2,nx)
      istat = node(3,nx)
      itab  = hbdpar(3,istat)
!
!        Determine hqit = h(Q2(igr))
!
      call INTTAB (ntab(1,itab), ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &q2(igr), hqit       )
!
!        Determine hqitdq = h(Q2(igr) + dQ)
!
!        >>>>>> dQ moet ergens een waarde meekrijgen !!!!!! <<<<<<
      dq = 0.01
      call INTTAB (ntab(1,itab), ntab(4,itab),&
      &table(ntab(2,itab)),&
      &table(ntab(3,itab)),&
      &(q2(igr)+dq), hqitdq  )
!
      alfa  = -1.0D0
      beta  = dble (( hqitdq - hqit ) / dq )
      gamma = 0.0D0

   endif
!
end
