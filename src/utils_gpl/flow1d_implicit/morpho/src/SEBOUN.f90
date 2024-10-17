subroutine seboun (nboun ,nbran ,ngrid ,maxtab ,ntabm  ,time   ,&
&q2    ,qs    ,sedtr ,mbdpar ,branch ,sedinf ,&
&ntab  ,table ,dissed)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Sediment Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SEBOUN (SEdiment BOUNdaries)
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
! 15 dissed(4,nbran)   O  Redistributed sediment transport at begin and
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
!  8 qs(ngrid,2)       I  Flow in every grid point per section:
!                         (i,1) = Through grid point i of main channel.
!                         (i,2) = Through grid point i of sub section 1.
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
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: seboun.pf,v $
! Revision 1.4  1995/10/18  09:00:36  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:56:24  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:07:10  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:13  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:47:28  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:34:31  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:18  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nboun ,nbran    ,ngrid ,maxtab  ,ntabm
   integer branch(4,nbran) ,sedinf(2,nbran),ntab (4,maxtab) ,&
   &mbdpar(5,*)
   real    qs   (ngrid,2) ,sedtr(ngrid,*)  ,&
   &dissed(4,nbran) ,table(ntabm)
   double  precision  time, q2(ngrid)
!
!     Declaration of local variables
!
   integer ibn  ,ibr  ,igr  ,ind ,itab, i1, i2
   real    s    ,sedtra
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     First copy sedtr at begin and end points to dissed buffer
!     This because outflowing boundaries without a boundary condition
!     become zero otherwise.
!
   do 10 ibr = 1, nbran
!
      i1 = branch(3,ibr)
      i2 = branch(4,ibr)
!
      if (sedinf(1,ibr).ne.0) then
!
!           Sedredge branch
!
         dissed(1,ibr) = sedtr(i1,1)
         dissed(2,ibr) = sedtr(i1,2)
         dissed(3,ibr) = sedtr(i2,1)
         dissed(4,ibr) = sedtr(i2,2)
      else
!
!           Normal branch
!
         dissed(1,ibr) = sedtr(i1,1)
         dissed(3,ibr) = sedtr(i2,1)
      endif
10 continue
!
!     Now process boundaries
!
   do 20 ibn = 1,nboun
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
            ind   = 0
         else
!              End of branch
            s     = -1.
            ind   = 1
         endif
         igr = branch(ind+3,ibr)
!
         if (q2(igr)*s .ge. 0.) then
!
!              Inflow
!
            if (sedinf(1,ibr).ne.0) then
!
!                 Sedredge branch
!
               if (mbdpar(1,ibn) .eq. cmbsft) then
!
!                    User defined sediment table S=f(t) for left channel
!
!                    itab       : TABLE number sedtra=f(t)
!                    time       : t(n+1)
!
                  itab  = mbdpar(4,ibn)
!
                  call inttab (ntab (1,itab)      ,ntab(4,itab),&
                  &table(ntab(2,itab)),&
                  &table(ntab(3,itab)),&
                  &time               ,sedtra      )
!
                  dissed(ind*2+1,ibr) = sedtra
!
!                    User defined sediment table S=f(t) for right channel
!
                  itab  = mbdpar(5,ibn)
!
                  call inttab (ntab (1,itab)      ,ntab(4,itab),&
                  &table(ntab(2,itab)),&
                  &table(ntab(3,itab)),&
                  &time               ,sedtra      )
!
                  dissed(ind*2+2,ibr) = sedtra
!
               else if (mbdpar(1,ibn) .eq. cmbsfq) then
!
!                    User defined sediment table S=f(Q) for left channel.
!
!                    itab       : TABLE number sedtra=f(Q)
!                    Qs         : q(n+1) in channel
!
                  itab  = mbdpar(4,ibn)
!
                  call inttab (ntab (1,itab)      ,ntab(4,itab),&
                  &table(ntab(2,itab)),&
                  &table(ntab(3,itab)),&
                  &dble(qs(igr,1))    ,sedtra      )
!
                  dissed(ind*2+1,ibr) = sedtra
!
!                    User defined sediment table S=f(Q) for right channel.
!
                  itab  = mbdpar(5,ibn)
!
                  call inttab (ntab (1,itab)      ,ntab(4,itab),&
                  &table(ntab(2,itab)),&
                  &table(ntab(3,itab)),&
                  &dble(qs(igr,2))    ,sedtra      )
!
                  dissed(ind*2+2,ibr) = sedtra
!
               else if (mbdpar(1,ibn) .eq. cmbzft) then
!
!                    Calculated sediment transport is stored.
!
                  dissed(ind*2+1,ibr) = sedtr(igr,1)
                  dissed(ind*2+2,ibr) = sedtr(igr,2)
!
               endif
            else
!
!                 Normal branch
!
               if (mbdpar(1,ibn) .eq. cmbsft) then
!
!                    User defined sediment table S=f(t).
!
!                    itab       : TABLE number sedtra=f(t)
!                    time       : t(n+1)
!
                  itab  = mbdpar(4,ibn)
!
                  call inttab (ntab (1,itab)      ,ntab(4,itab),&
                  &table(ntab(2,itab)),&
                  &table(ntab(3,itab)),&
                  &time               ,sedtra      )
!
                  dissed(ind*2+1,ibr) = sedtra
!
               else if (mbdpar(1,ibn) .eq. cmbsfq) then
!
!                    User defined sediment table S=f(Q).
!
!                    itab       : TABLE number sedtra=f(Q)
!                    Q2         : q(n+1) in main section
!
                  itab  = mbdpar(4,ibn)
!
                  call inttab (ntab (1,itab)      ,ntab(4,itab),&
                  &table(ntab(2,itab)),&
                  &table(ntab(3,itab)),&
                  &dble(q2(igr))      ,sedtra      )
!
                  dissed(ind*2+1,ibr) = sedtra
!
               else if (mbdpar(1,ibn) .eq. cmbzft) then
!
!                    Calculated sediment transport is stored.
!
                  dissed(ind*2+1,ibr) = sedtr(igr,1)
!
               endif
            endif
         else
!
!              Outflow
!              Calculated sediment transport is stored.
!
            dissed(ind*2+1,ibr) = sedtr(igr,1)
            if (sedinf(1,ibr).ne.0) then
               dissed(ind*2+2,ibr) = sedtr(igr,2)
            endif
!
         endif
      endif
20 continue
!
end
