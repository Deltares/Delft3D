      subroutine saboun (nboun  ,nbran ,ngrid ,maxtab ,ntabm ,time   ,
     &                   q2     ,csa1  ,sbdpar,branch ,ntab  ,table  ,
     &                   sbdscr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SABOUN (SAlt BOUNdaries)
c
c Module description: Determine the salt boundary conditions.
c
c                     In the salt module boundary conditions are used
c                     for solving the nodal administration matrix. The
c                     concentrations at inflow boundaries (option 1 or
c                     2) are determined. For a Thatcher-Harleman bounda-
c                     ry also the time and concentration at the last
c                     outflow step are stored.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  8 csa1(ngrid)       I  Salt concentration in every grid point at time
c                         t(n).
c  4 maxtab            I  Maximum number of defined tables.
c  1 nboun             I  Number of boundary nodes.
c  2 nbran             I  Number of branches.
c  3 ngrid             I  Number of grid points in network.
c 11 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c 13 sbdscr(3,nboun)   IO Intermediate results at salt boundaries:
c                         (1,i) = Last time of outflow (option = 2)
c                         (2,i) = Concentration at last time of outflow
c                                 (option = 2)
c                         (3,i) = Concentration at inflow (time n+1) if
c                                 option is 1 or 2
c 12 table             P  -
c  6 time              I  Actual time level tn+1. in sec.
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
c $Log: saboun.pf,v $
c Revision 1.5  1999/03/15  15:53:19  kuipe_j
c tabs removed
c
c Revision 1.4  1996/04/11  08:25:20  kuipe_j
c Kalman module added
c
c Revision 1.3  1995/05/30  09:56:03  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:05:52  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:33  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:17:02  kuipe_j
c Time , timestep and period in double precision.
c
c Revision 1.2  1993/11/26  15:33:23  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:11  kuipe_j
c Initial version
c
c
c***********************************************************************

!DEC$ IF DEFINED (_DLL)
      use SobekRE_OpenMI
!DEC$ ENDIF
c
c     Declaration of parameters
c
      integer nboun ,nbran    ,ngrid ,maxtab  ,ntabm, nodenm
      integer branch(4,nbran) ,ntab  (4,maxtab)
      real    csa1 (ngrid)  ,
     &        sbdpar(5,nboun) ,sbdscr(3,nboun) ,table(ntabm)
      double  precision        time            ,q2   (ngrid)
c
c     Declaration of local variables
c
      integer ibn  ,ibr  ,igr  ,itab ,iopt
      real    s    ,con  ,tout ,t0   ,cout  ,pi   ,timdif
c
c     External functions
c
      integer gtcpnt


c
c     Include sobek constants
c
      include '..\include\sobcon.i'
      include '..\include\mempool.i'
c

!DEC$ IF DEFINED (_DLL)

   if (OpenMIActive()) then
         nodenm =     gtcpnt ( 'NODENM')
      endif
!DEC$ ENDIF
      
      do 10 ibn = 1,nboun
c
         iopt = int (sbdpar(1,ibn))
c
         if (iopt .eq. csbusr .or. iopt .eq. csbthh) then
c
c           User specified concentration (option=1) or
c           Thatcher Harleman (option=2).
c           Determine if bound is at begin or end of branch.
c
            ibr = int(sbdpar(3,ibn))
            if (branch(1,ibr) .eq. int(sbdpar(2,ibn))) then
               igr   = branch(3,ibr)
               s     = 1.
            else
               igr   = branch(4,ibr)
               s     = -1.
            endif
c
            if (q2(igr)*s .le. 0. .and. iopt .eq. csbthh) then
c
c              There was outflow on a Thatcher Harleman boundary.
c              Store time and concentration of this time step.
c
               sbdscr(1,ibn) = sngl(time)
               sbdscr(2,ibn) = csa1(igr)
c
            else if (q2(igr)*s .gt. 0.) then
c
c              Inflow at current time (step n+1).
c              Get concentration from a user defined table.
c
c              itab       : TABLE number con=f(t)
c              time     . : t(n+1)
c
               itab  = int(sbdpar(4,ibn))
c
               call inttab (ntab (1,itab)      ,ntab(4,itab),
     &                      table(ntab(2,itab)),
     &                      table(ntab(3,itab)),
     &                      time               ,con         )

!DEC$ IF DEFINED (_DLL)

            if (OpenMIActive()) then
                  call GetBoundariesSalt(cp(nodenm), ibn, nboun, con)
               endif
!DEC$ ENDIF
c
               if (iopt .eq. csbusr) then
c
c                 User defined concentration
c
                  sbdscr(3,ibn) = con
               else
c
c                 Thatcher Harleman.
c                 [ Doc. S-FO-001.5KV / Eq 22-21 ]
c
                  tout = sbdscr(1,ibn)
                  t0   = sbdpar(5,ibn)
                  if (sngl(time) .ge. tout+t0) then
c
c                    Adaptation time exceeded. Use user defined
c                    concentration.
c
                     sbdscr(3,ibn) = con
                  else
c
c                    Adaptation time not exceeded.Interpolate
c                    between last outflow concentration and the
c                    user defined concentration.
c
                     cout  = sbdscr(2,ibn)
                     pi    = 4.*atan(1.)
                     timdif= sngl(time-dble(tout))
                     sbdscr(3,ibn) = cout + (con - cout) * .5 *
     &                               (1. - cos(pi*timdif/t0))
                  endif
               endif
            endif
         endif
   10 continue
c
      end
