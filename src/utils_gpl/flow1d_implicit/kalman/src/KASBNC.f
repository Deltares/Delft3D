      subroutine KASBNC(nx     ,s      ,nnode  ,node   ,
     +                  ngrid  ,h2     ,q2     ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,
     +                  hbdpar ,qbdpar ,
     +                  alfa   ,beta   ,gamma  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KASBNC (KALman Substitute BouNdary Coefficients)
c
c Module description: In subroutine KASBNC the boundary coefficients
c                     ALFA, BETA and GAMMA will be computed for internal
c                     boundaries (=nodes) and external boundaries.
c
c                     For the external boundary conditions these coef-
c                     ficients follow from formulae (5-1) thr. (5-6) in
c                     [S-FO-004].
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 14 alfa              O  -
c 15 beta              O  -
c 16 gamma             O  -
c  6 h2(ngrid)         I  Water level in every grid point at time
c                         t(n+1).
c 12 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
c                         (1,i) = Location [grid point] for H-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : h = f(t)
c                                 cbfqoh (2) : h = h(Q)
c                                 cbfour (3) : h = fourier
c                                 cbtidl (4) : h = tidal components
c                         (3,i) = Table number for f(t), h(Q), fourier
c                                 or tidal components table.
c  8 maxtab            I  Maximum number of defined tables.
c  5 ngrid             I  Number of grid points in network.
c  3 nnode             I  Number of nodes.
c  4 node(4,nnode)     I  Definition of nodes:
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
c 10 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c  9 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  1 nx                I  -
c  7 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 13 qbdpar(3,nqstat)  I  Hydrodynamic conditions for Q-stations:
c                         (1,i) = Location [grid point] for Q-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : Q = f(t)
c                                 cbfqoh (2) : Q = Q(h)
c                                 cbfour (3) : Q = fourier
c                                 cbtidl (4) : Q = tidal components
c                         (3,i) = Table number for f(t), Q(h), fourier
c                                 or tidal components table.
c  2 s                 I  -
c 11 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kasbnc.pf,v $
c Revision 1.3  1999/03/15  15:52:16  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:26  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:58  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Declaration of Parameters:
c
      integer nx, nnode, ngrid, maxtab, ntabm
      integer node(4,nnode), ntab(4,maxtab)
      integer hbdpar(3,*), qbdpar(3,*)
      real    s, table(ntabm)

      double precision h2(ngrid), q2(ngrid)
      double precision alfa, beta, gamma
c
c     Declaration of local variables
c
      integer igr, istat, itab
      real    dh, dq, hqit, hqitdq, qhit, qhitdh
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     nx = node number at begin or end of branch
c     ix = grid point number at node nx
c
c     *************
c     *  NODE nx  *
c     *************
c
c     *********************
c     * Internal boundary *
c     *********************
c
      if (node(1,nx) .eq. cintnd) then
c
c        Update coefficients for node nx
c
c        Remark:
c        Positive branch direction goes from node n1 to node n2.
c        So, for directional unit vector [s] along branch holds : s = +1
c
c        Doc: S-FO-004.2PB  Eq. 6-4
c
         alfa  = 0.0D0
         beta  = s
         gamma = 0.0D0
c
c     *********************
c     * External boundary *
c     *********************
c
      else if (node(1,nx) .eq. chbou) then
c
c        H-boundary
c
         istat = node(3,nx)
c
c        Doc: S-FO-004.2PB  Eq. 5-3
c
         alfa  = 1.0D0
         beta  = 0.0D0
         gamma = 0.0D0

      else if (node(1,nx) .eq. cqbou) then
c
c        Q-boundary
c
c        Doc: S-FO-004.2PB  Eq. 5-4
c
         istat = node(3,nx)
c
         alfa  = 0.0D0
         beta  = 1.0D0
         gamma = 0.0D0

      else if (node(1,nx) .eq. cqhbou) then
c
c        QH-boundary
c
c        Doc: S-FO-004.2PB  Eq. 5-5
c
         igr   = node(2,nx)
         istat = node(3,nx)
         itab  = qbdpar(3,istat)
c
c        Determine qhit = Q(h2(igr))
c
         call INTTAB (ntab(1,itab), ntab(4,itab),
     +                table(ntab(2,itab)),
     +                table(ntab(3,itab)),
     +                h2(igr), qhit       )
c
c        Determine qhitdh = Q(h2(igr) + dh)
c
c        >>>>>> dh moet ergens een waarde meekrijgen !!!!!! <<<<<<
         dh = 0.01
         call INTTAB (ntab(1,itab), ntab(4,itab),
     +                table(ntab(2,itab)),
     +                table(ntab(3,itab)),
     +                (h2(igr)+dh), qhitdh  )
c
         alfa  = dble (( qhitdh - qhit ) / dh )
         beta  = -1.0D0
         gamma = 0.0D0

      else if (node(1,nx) .eq. chqbou) then
c
c        HQ-boundary
c
c        Doc: S-FO-004.2PB  Eq. 5-6
c
         igr   = node(2,nx)
         istat = node(3,nx)
         itab  = hbdpar(3,istat)
c
c        Determine hqit = h(Q2(igr))
c
         call INTTAB (ntab(1,itab), ntab(4,itab),
     +                table(ntab(2,itab)),
     +                table(ntab(3,itab)),
     +                q2(igr), hqit       )
c
c        Determine hqitdq = h(Q2(igr) + dQ)
c
c        >>>>>> dQ moet ergens een waarde meekrijgen !!!!!! <<<<<<
         dq = 0.01
         call INTTAB (ntab(1,itab), ntab(4,itab),
     +                table(ntab(2,itab)),
     +                table(ntab(3,itab)),
     +                (q2(igr)+dq), hqitdq  )
c
         alfa  = -1.0D0
         beta  = dble (( hqitdq - hqit ) / dq )
         gamma = 0.0D0

      endif
c
      end
