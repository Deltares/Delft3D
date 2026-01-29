subroutine gsboun (nnode ,nboun ,nbran ,ngrid ,nbrnod ,maxtab ,&
&ntabm ,nfrac ,time  ,q2    ,sedtr  ,mbdpar ,&
&branch,brnode,ntab  ,table ,disgse )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             GSBOUN (Graded Sediment BOUNdaries)
!
! Module description: Determine the inflowing sediment transport at
!                     boundary stations.
!
!                     When the morphology module is included the sedi-
!                     ment transport at the boundaries is determined by
!                     reading the boundary conditions. When the morpho-
!                     logy module is not included the calculated sedi-
!                     ment transports remain valid.
!
!                     In case discharge is flowing in the user defined
!                     boundary conditions will be read. For a Sedredge
!                     branch two values will be read. In stead of sedi-
!                     ment transport also the bed level can be given (in
!                     the morphology module). In the latter case and in
!                     case of outflow the transports will be calculated.
!
!                     The sign of the given sediment transport is deter-
!                     mined by the branch direction.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
! 15 disgse(4,nbran)   O  Redistributed sediment transport at begin and
!                         end of branches. At the outflow side of the
!                         branch the calculated transports are stored.
!                         At the inflow side the redistributed trans-
!                         ports are stored.
!                         (1,i)   Transport at section 1 (main or left
!                                 channel)  at begin of branch.
!                         (2,i)   Transport at section 2 (right channel)
!                                 at begin of branch.
!                         (3,i)   Transport at section 1 (main or left
!                                 channel)  at end of branch.
!                         (4,i)   Transport at section 2 (right channel)
!                                 at end of branch.
!  4 maxtab            I  Maximum number of defined tables.
! 10 mbdpar(5,nboun)   I  Morphodynamic boundary conditions:
!                         (1,i) = Type of boundary condition:
!                                 cmbsft (1) : Condition S=f(t).
!                                 cmbsfq (2) : Condition S=f(Q).
!                                 cmbzft (3) : Condition z=f(t).
!                         (2,i) = Location (node number).
!                         (3,i) = Branch number that is connected.
!                         (4,i) = Table pointer for boundary table. In
!                                 case of a connected sedredge branch
!                                 the pointer will be assigned to the
!                                 left channel.
!                         (5,i) = Table pointer for boundary table. In
!                                 case of a connected sedredge branch
!                                 the pointer will be assigned to the
!                                 right channel. In other cases undefi-
!                                 ned.
!  1 nboun             I  Number of boundary nodes.
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
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
!  5 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  7 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
!  8 qs                P  -
! 12 sedinf(2,nbran)   I  Defines for each branch whether a branch is a
!                         sedredge branch or not.
!                         (1,j) = Sedredge branch number (1..nsedrd)
!                                 else 0.
!                         (2,j) = Starting index in Rc array (River bend
!                                 curvature).
!  9 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
! 14 table             P  -
!  6 time              P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gsboun.F,v $
! Revision 1.2  1995/09/27  10:11:48  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nboun ,nbran    ,ngrid ,maxtab  ,ntabm  ,nfrac ,&
   &nnode ,nbrnod
   integer branch(4,nbran) ,ntab (4,maxtab) ,&
   &mbdpar(5,*)     ,brnode(nbrnod+1,nnode)
   real    sedtr(ngrid,nfrac+2) ,&
   &disgse(nfrac,2,nbran) ,table(ntabm)
   double precision    time
   double precision     q2(ngrid)
!
!     Declaration of local variables
!
   integer ibn  ,ibr  ,igr  ,ind ,itab, jf ,itabtb ,&
   &inode
   real    s    ,sedtra
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     First copy sedtr at boundary points to disgse buffer
!     This because outflowing boundaries without a boundary condition
!     become zero otherwise.
!
   do 20 inode = 1, nnode
      if (brnode(1,inode) .eq. 1) then
         ibr = brnode(2,inode)
         if (branch (1,ibr) .eq. inode) then
!             Begin of branch
            igr = branch (3,ibr)
            ind = 1
         else
!             Begin of branch
            igr = branch (4,ibr)
            ind = 2
         endif
         do 10 jf=1,nfrac
            disgse(jf,ind,ibr) = sedtr(igr,jf)
10       continue
      endif
20 continue
!
!     Now process boundaries
!
   do 70 ibn = 1,nboun
!
!        Determine if bound is at begin or end of branch.
!
      ibr = mbdpar(3,ibn)
!
      if (ibr .ne. 0) then
!
!           Boundary condition defined
!
         if (branch(1,ibr) .eq. mbdpar(2,ibn)) then
!              Begin of branch
            s     = 1.
            ind   = 1
         else
!              End of branch
            s     = -1.
            ind   = 2
         endif
         igr = branch(ind+2,ibr)
!
         if (q2(igr)*s .ge. 0.) then
!
!              Inflow
!
            if (mbdpar(1,ibn) .eq. cmbsft) then
!
!                 User defined sediment table S=f(t).
!                 First get table with table numbers
!
!                 itabtb     : TABLE number of table with
!                               table numbers

               itabtb  = mbdpar(4,ibn)

               do 30 jf=1,nfrac

                  itab = int(table(ntab(3,itabtb) + jf - 1))

!                    itab       : TABLE number sedtra=f(t) for
!                                 fraction jf
!                    time       : t(n+1)
!
!
                  call inttab (ntab (1,itab)      ,ntab(4,itab),&
                  &table(ntab(2,itab)),&
                  &table(ntab(3,itab)),&
                  &time               ,sedtra      )
!
                  disgse(jf,ind,ibr) = sedtra
!
30             continue
            else if (mbdpar(1,ibn) .eq. cmbsfq) then
!
!                 User defined sediment table S=f(Q).
!
!                 First get table with table numbers
!
!                 itabtb     : TABLE number of table with
!                               table numbers

               itabtb  = mbdpar(4,ibn)

               do 40 jf=1,nfrac

                  itab = int(table(ntab(3,itabtb) + jf - 1))

!                    itab       : TABLE number Sedtra=f(Q) for
!                                 fraction jf
!                    time       : t(n+1)
!
!
                  call inttab (ntab (1,itab)      ,ntab(4,itab),&
                  &table(ntab(2,itab)),&
                  &table(ntab(3,itab)),&
                  &q2(igr)      ,sedtra      )
!
                  disgse(jf,ind,ibr) = sedtra
!
40             continue
            else if (mbdpar(1,ibn) .eq. cmbzft) then
!
!                 Calculated sediment transport is stored.
!
               do 50 jf=1,nfrac
                  disgse(jf,ind,ibr) = sedtr(igr,jf)
50             continue
!
            endif
         else
!
!              Outflow
!              Calculated sediment transport is stored.
!
            do 60 jf=1,nfrac
               disgse(jf,ind,ibr) = sedtr(igr,jf)
60          continue
!
         endif
      endif
70 continue
!
end
