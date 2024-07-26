      subroutine FLBNCO(nx     ,ix     ,s      ,nnode  ,node   ,
     +                  ngrid  ,h1     ,h      ,q1     ,q      ,
     +                  maxtab ,ntabm  ,ntab   ,table  ,
     +                  hstat  ,hbdpar ,qstat  ,qbdpar ,
     +                  qtyp   ,alfa   ,beta   ,gamma  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLBNCO (FLow BouNdary COefficients)
c
c Module description: In subroutine FLBNCO the boundary coefficients
c                     alfa, beta and gamma will be computed for internal
c                     boundaries (=nodes) and external boundaries.
c
c                     The boundary conditions are expressed in following
c                     form:
c
c                     alfa * delta H + beta * delta Q = gamma
c
c                     For the external boundary conditions these coeffi-
c                     cients follow from formulaes (8-2) until (8-4) in
c                     S-FO-001.5KV. Values for h and Q are already cal-
c                     culated by FLBOUN.
c
c                     For internal boundary conditions (at nodes) the
c                     coefficients can be computed from formulae (8-5)
c                     in S-FO-001.5KV.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 19 alfa              IO Contribution of one branch in the coefficient
c                         of dh or cs in the current node equation.
c 20 beta              IO Contribution of one branch in the coefficient
c                         of dq or c's in the current node equation.
c 21 gamma             O  Contribution of one branch in the right hand
c                         side of the current node equation.
c  7 h1(ngrid)         I  Water level in every grid point at time t(n).
c  8 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c 16 hbdpar(3,nhstat)  I  Hydrodynamic conditions for H-stations:
c                         (1,i) = Location [grid point] for H-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : h = f(t)
c                                 cbfqoh (2) : h = h(Q)
c                                 cbfour (3) : h = fourier
c                                 cbtidl (4) : h = tidal components
c                         (3,i) = Table number for f(t), h(Q), fourier
c                                 or tidal components table.
c 15 hstat(nhstat)     I  Actual water level in every H-station.
c  2 ix                I  Grid point at begin or end of branch.
c 11 maxtab            I  Maximum number of defined tables.
c  6 ngrid             I  Number of grid points in network.
c  4 nnode             I  Number of nodes.
c  5 node(4,nnode)     I  Definition of nodes:
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
c 13 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c 12 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  1 nx                I  Node number at begin or end of branch.
c  9 q1(ngrid)         I  Discharge in every grid point at time t(n).
c 10 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c 18 qbdpar(3,nqstat)  I  Hydrodynamic conditions for Q-stations:
c                         (1,i) = Location [grid point] for Q-station.
c                         (2,i) = Type of condition
c                                 cbftim (1) : Q = f(t)
c                                 cbfqoh (2) : Q = Q(h)
c                                 cbfour (3) : Q = fourier
c                                 cbtidl (4) : Q = tidal components
c                         (3,i) = Table number for f(t), Q(h), fourier
c                                 or tidal components table.
c 17 qstat(nqstat)     I  Actual discharge in every Q-station.
c  3 s                 I  Indicates position of node:
c                         +1  :   Begin of branch
c                         -1  :   End of branch
c 14 table             P  -
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
c $Log: flbnco.pf,v $
c Revision 1.9  1999/03/15  15:49:33  kuipe_j
c tabs removed
c
c Revision 1.8  1997/05/26  07:44:43  kuipe_j
c Small changes
c
c Revision 1.7  1996/05/28  13:34:11  kuipe_j
c Q(n) instead of Q(*)
c
c Revision 1.6  1995/11/21  11:07:45  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.5  1995/09/22  10:00:58  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:10:49  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.3  1994/11/28  08:37:17  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:30:32  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Declaration of Parameters:
c
      integer nx, ix, nnode, ngrid, maxtab, ntabm
      integer node(4,nnode), ntab(4,maxtab)
      integer hbdpar(3,*), qbdpar(3,*)
      real    qtyp, s
      double precision h1(ngrid), h(ngrid), q1(ngrid), q(ngrid)
      real    hstat(*), qstat(*)
      real    table(ntabm)
      double precision alfa, beta, gamma
c
c     Declaration of local variables
c
      integer igr, istat, itab
      real    dh, dq, hqit, hqit1, hqit2, qhit, qhit1 ,qhit2
c
c     Include sobek constants
c
      include '../include/sobcon.i'
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
c        Doc: S-FO-001.5KV  Eq. 8-6
c
         alfa  = 0.0D0
         beta  = dble (s)
         gamma = dble (-s * q1(ix))
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
c        Doc: S-FO-001.5KV  Eq. 8-2
c
         alfa  = 1.0D0
         beta  = 0.0D0
         gamma = dble (hstat(istat) - h1(ix))

      else if (node(1,nx) .eq. cqbou) then
c
c        Q-boundary
c
c        Doc: S-FO-001.5KV  Eq. 8-3
c
         istat = node(3,nx)
c
         alfa  = 0.0D0
         beta  = 1.0D0
         gamma = dble (qstat(istat) - q1(ix))

      else if (node(1,nx) .eq. cqhbou) then
c
c        QH-boundary
c
c        Doc: S-FO-001.5KV  Eq. 8-4
c
         igr   = node(2,nx)
         istat = node(3,nx)
         itab  = qbdpar(3,istat)
c
c        Determine differentiation increment
c
         dh = max(min(.0001*qtyp,.01),.001)
c
c        Determine qhit1 = Q(h* - dh)
c
         call INTTAB (ntab(1,itab), ntab(4,itab),
     +                table(ntab(2,itab)),
     +                table(ntab(3,itab)),
     +                dble(h(igr)-dh), qhit1      )
c
c        Determine qhit2 = Q(h* + dh)
c
         call INTTAB (ntab(1,itab), ntab(4,itab),
     +                table(ntab(2,itab)),
     +                table(ntab(3,itab)),
     +                dble(h(igr)+dh), qhit2   )
c
         qhit  = 0.5 * (qhit1+qhit2)
         alfa  = dble (( qhit2 - qhit1 ) / (dh*2.) )
         beta  = -1.0D0
         gamma = dble ( -qhit + (h(igr) - h1(igr)) * alfa + q1(igr))

      else if (node(1,nx) .eq. chqbou) then
c
c        HQ-boundary
c
c        Doc: S-FO-001.5KV  Eq. 8-4
c
         igr   = node(2,nx)
         istat = node(3,nx)
         itab  = hbdpar(3,istat)
c
c        Determine differentiation increment
c
         dq = max(min(.001*qtyp,1.0),.001)
c
c        Determine hqit1 = h(Q*-dq)
c
         call INTTAB (ntab(1,itab), ntab(4,itab),
     +                table(ntab(2,itab)),
     +                table(ntab(3,itab)),
     +                dble(q(igr)-dq), hqit1      )
c
c        Determine hqit2 = h(Q* + dQ)
c
         call INTTAB (ntab(1,itab), ntab(4,itab),
     +                table(ntab(2,itab)),
     +                table(ntab(3,itab)),
     +                dble(q(igr)+dq), hqit2  )
c
         hqit  = 0.5 * (hqit1+hqit2)
c
         alfa  = -1.0D0
         beta  = dble (( hqit2 - hqit1 ) / (dq*2.) )
         gamma = dble ( -hqit + (q(igr) - q1(igr)) * beta + h1(igr))

      endif
c
      end
