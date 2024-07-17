      subroutine gsboun (nnode ,nboun ,nbran ,ngrid ,nbrnod ,maxtab ,
     &                   ntabm ,nfrac ,time  ,q2    ,sedtr  ,mbdpar ,
     &                   branch,brnode,ntab  ,table ,disgse )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Sediment Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             GSBOUN (Graded Sediment BOUNdaries)
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
c 15 disgse(4,nbran)   O  Redistributed sediment transport at begin and
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
c  8 qs                P  -
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
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: gsboun.F,v $
c Revision 1.2  1995/09/27  10:11:48  kuipe_j
c Maintenance
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nboun ,nbran    ,ngrid ,maxtab  ,ntabm  ,nfrac ,
     &        nnode ,nbrnod 
      integer branch(4,nbran) ,ntab (4,maxtab) ,
     &        mbdpar(5,*)     ,brnode(nbrnod+1,nnode)
      real    sedtr(ngrid,nfrac+2) ,
     &        disgse(nfrac,2,nbran) ,table(ntabm)
      double precision    time
      double precision     q2(ngrid)
c
c     Declaration of local variables
c
      integer ibn  ,ibr  ,igr  ,ind ,itab, jf ,itabtb ,
     &        inode
      real    s    ,sedtra
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     First copy sedtr at boundary points to disgse buffer
c     This because outflowing boundaries without a boundary condition
c     become zero otherwise.
c
      do 20 inode = 1, nnode
      if (brnode(1,inode) .eq. 1) then
      ibr = brnode(2,inode)
      if (branch (1,ibr) .eq. inode) then
c             Begin of branch
         igr = branch (3,ibr)
         ind = 1
           else
c             Begin of branch
         igr = branch (4,ibr)
         ind = 2
           endif  
      do 10 jf=1,nfrac
         disgse(jf,ind,ibr) = sedtr(igr,jf)
  10       continue
         endif
  20  continue
c
c     Now process boundaries
c
      do 70 ibn = 1,nboun
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
               ind   = 1
            else
c              End of branch
               s     = -1.
               ind   = 2
            endif
            igr = branch(ind+2,ibr)
c
            if (q2(igr)*s .ge. 0.) then
c
c              Inflow
c
               if (mbdpar(1,ibn) .eq. cmbsft) then
c
c                 User defined sediment table S=f(t).
c                 First get table with table numbers
c
c                 itabtb     : TABLE number of table with
c                               table numbers

                  itabtb  = mbdpar(4,ibn)

                  do 30 jf=1,nfrac

                     itab = int(table(ntab(3,itabtb) + jf - 1))

c                    itab       : TABLE number sedtra=f(t) for
c                                 fraction jf
c                    time       : t(n+1)
c
c
                     call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                            table(ntab(2,itab)),
     &                            table(ntab(3,itab)),
     &                            time               ,sedtra      )
c
                     disgse(jf,ind,ibr) = sedtra
c
   30             continue
               else if (mbdpar(1,ibn) .eq. cmbsfq) then
c
c                 User defined sediment table S=f(Q).
c
c                 First get table with table numbers
c
c                 itabtb     : TABLE number of table with
c                               table numbers

                  itabtb  = mbdpar(4,ibn)

                  do 40 jf=1,nfrac

                     itab = int(table(ntab(3,itabtb) + jf - 1))

c                    itab       : TABLE number Sedtra=f(Q) for
c                                 fraction jf
c                    time       : t(n+1)
c
c
                     call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                            table(ntab(2,itab)),
     &                            table(ntab(3,itab)),
     &                            q2(igr)      ,sedtra      )
c
                     disgse(jf,ind,ibr) = sedtra
c
   40             continue
               else if (mbdpar(1,ibn) .eq. cmbzft) then
c
c                 Calculated sediment transport is stored.
c
                  do 50 jf=1,nfrac
                     disgse(jf,ind,ibr) = sedtr(igr,jf)
   50             continue
c
               endif
            else
c
c              Outflow
c              Calculated sediment transport is stored.
c
               do 60 jf=1,nfrac
                   disgse(jf,ind,ibr) = sedtr(igr,jf)
   60          continue
c
            endif
         endif
   70 continue
c
      end
