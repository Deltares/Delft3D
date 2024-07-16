      subroutine seboun (nboun ,nbran ,ngrid ,maxtab ,ntabm  ,time   ,
     &                   q2    ,qs    ,sedtr ,mbdpar ,branch ,sedinf ,
     &                   ntab  ,table ,dissed)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SEBOUN (SEdiment BOUNdaries)
c
c Module description: Determine the inflowing sediment transport at
c                     boundary stations.
c
c                     When the morphology module is included the sedi-
c                     ment transport at the boundaries is determined by
c                     reading the boundary conditions. When the morpho-
c                     logy module is not included the calculated sedi-
c                     ment transports remain valid.
c
c                     In case discharge is flowing in the user defined
c                     boundary conditions will be read. For a Sedredge
c                     branch two values will be read. In stead of sedi-
c                     ment transport also the bed level can be given (in
c                     the morphology module). In the latter case and in
c                     case of outflow the transports will be calculated.
c
c                     The sign of the given sediment transport is deter-
c                     mined by the branch direction.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 11 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 15 dissed(4,nbran)   O  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c  4 maxtab            I  Maximum number of defined tables.
c 10 mbdpar(5,nboun)   I  Morphodynamic boundary conditions:
c                         (1,i) = Type of boundary condition:
c                                 cmbsft (1) : Condition S=f(t).
c                                 cmbsfq (2) : Condition S=f(Q).
c                                 cmbzft (3) : Condition z=f(t).
c                         (2,i) = Location (node number).
c                         (3,i) = Branch number that is connected.
c                         (4,i) = Table pointer for boundary table. In
c                                 case of a connected sedredge branch
c                                 the pointer will be assigned to the
c                                 left channel.
c                         (5,i) = Table pointer for boundary table. In
c                                 case of a connected sedredge branch
c                                 the pointer will be assigned to the
c                                 right channel. In other cases undefi-
c                                 ned.
c  1 nboun             I  Number of boundary nodes.
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
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
c  5 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  7 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c  8 qs(ngrid,2)       I  Flow in every grid point per section:
c                         (i,1) = Through grid point i of main channel.
c                         (i,2) = Through grid point i of sub section 1.
c 12 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
c                         sedredge branch or not.
c                         (1,j) = Sedredge branch number (1..nsedrd)
c                                 else 0.
c                         (2,j) = Starting index in Rc array (River bend
c                                 curvature).
c  9 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c 14 table             P  -
c  6 time              P  -
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
c $Log: seboun.pf,v $
c Revision 1.4  1995/10/18  09:00:36  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.3  1995/05/30  09:56:24  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:07:10  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:13  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:47:28  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:34:31  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:18  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nboun ,nbran    ,ngrid ,maxtab  ,ntabm
      integer branch(4,nbran) ,sedinf(2,nbran),ntab (4,maxtab) ,
     &        mbdpar(5,*)
      real    qs   (ngrid,2) ,sedtr(ngrid,*)  ,
     &        dissed(4,nbran) ,table(ntabm)
      double  precision  time, q2(ngrid)
c
c     Declaration of local variables
c
      integer ibn  ,ibr  ,igr  ,ind ,itab, i1, i2
      real    s    ,sedtra
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     First copy sedtr at begin and end points to dissed buffer
c     This because outflowing boundaries without a boundary condition
c     become zero otherwise.
c
      do 10 ibr = 1, nbran
c
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
c
         if (sedinf(1,ibr).ne.0) then
c
c           Sedredge branch
c
            dissed(1,ibr) = sedtr(i1,1)
            dissed(2,ibr) = sedtr(i1,2)
            dissed(3,ibr) = sedtr(i2,1)
            dissed(4,ibr) = sedtr(i2,2)
         else
c
c           Normal branch
c
            dissed(1,ibr) = sedtr(i1,1)
            dissed(3,ibr) = sedtr(i2,1)
         endif
 10   continue
c
c     Now process boundaries
c
      do 20 ibn = 1,nboun
c
c        Determine if bound is at begin or end of branch.
c
         ibr = mbdpar(3,ibn)
c
         if (ibr .ne. 0) then
c
c           Boundary condition defined
c
            if (branch(1,ibr) .eq. mbdpar(2,ibn)) then
c              Begin of branch
               s     = 1.
               ind   = 0
            else
c              End of branch
               s     = -1.
               ind   = 1
            endif
            igr = branch(ind+3,ibr)
c
            if (q2(igr)*s .ge. 0.) then
c
c              Inflow
c
               if (sedinf(1,ibr).ne.0) then
c
c                 Sedredge branch
c
                  if (mbdpar(1,ibn) .eq. cmbsft) then
c
c                    User defined sediment table S=f(t) for left channel
c
c                    itab       : TABLE number sedtra=f(t)
c                    time       : t(n+1)
c
                     itab  = mbdpar(4,ibn)
c
                     call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                            table(ntab(2,itab)),
     &                            table(ntab(3,itab)),
     &                            time               ,sedtra      )
c
                     dissed(ind*2+1,ibr) = sedtra
c
c                    User defined sediment table S=f(t) for right channel
c
                     itab  = mbdpar(5,ibn)
c
                     call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                            table(ntab(2,itab)),
     &                            table(ntab(3,itab)),
     &                            time               ,sedtra      )
c
                     dissed(ind*2+2,ibr) = sedtra
c
                  else if (mbdpar(1,ibn) .eq. cmbsfq) then
c
c                    User defined sediment table S=f(Q) for left channel.
c
c                    itab       : TABLE number sedtra=f(Q)
c                    Qs         : q(n+1) in channel
c
                     itab  = mbdpar(4,ibn)
c
                     call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                            table(ntab(2,itab)),
     &                            table(ntab(3,itab)),
     &                            dble(qs(igr,1))    ,sedtra      )
c
                     dissed(ind*2+1,ibr) = sedtra
c
c                    User defined sediment table S=f(Q) for right channel.
c
                     itab  = mbdpar(5,ibn)
c
                     call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                            table(ntab(2,itab)),
     &                            table(ntab(3,itab)),
     &                            dble(qs(igr,2))    ,sedtra      )
c
                     dissed(ind*2+2,ibr) = sedtra
c
                  else if (mbdpar(1,ibn) .eq. cmbzft) then
c
c                    Calculated sediment transport is stored.
c
                     dissed(ind*2+1,ibr) = sedtr(igr,1)
                     dissed(ind*2+2,ibr) = sedtr(igr,2)
c
                  endif
               else
c
c                 Normal branch
c
                  if (mbdpar(1,ibn) .eq. cmbsft) then
c
c                    User defined sediment table S=f(t).
c
c                    itab       : TABLE number sedtra=f(t)
c                    time       : t(n+1)
c
                     itab  = mbdpar(4,ibn)
c
                     call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                            table(ntab(2,itab)),
     &                            table(ntab(3,itab)),
     &                            time               ,sedtra      )
c
                     dissed(ind*2+1,ibr) = sedtra
c
                  else if (mbdpar(1,ibn) .eq. cmbsfq) then
c
c                    User defined sediment table S=f(Q).
c
c                    itab       : TABLE number sedtra=f(Q)
c                    Q2         : q(n+1) in main section
c
                     itab  = mbdpar(4,ibn)
c
                     call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                            table(ntab(2,itab)),
     &                            table(ntab(3,itab)),
     &                            dble(q2(igr))      ,sedtra      )
c
                     dissed(ind*2+1,ibr) = sedtra
c
                  else if (mbdpar(1,ibn) .eq. cmbzft) then
c
c                    Calculated sediment transport is stored.
c
                     dissed(ind*2+1,ibr) = sedtr(igr,1)
c
                  endif
               endif
            else
c
c              Outflow
c              Calculated sediment transport is stored.
c
               dissed(ind*2+1,ibr) = sedtr(igr,1)
               if (sedinf(1,ibr).ne.0) then
                  dissed(ind*2+2,ibr) = sedtr(igr,2)
               endif
c
            endif
         endif
   20 continue
c
      end
