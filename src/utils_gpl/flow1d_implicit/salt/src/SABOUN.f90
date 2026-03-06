subroutine saboun (nboun  ,nbran ,ngrid ,maxtab ,ntabm ,time   ,&
&q2     ,csa1  ,sbdpar,branch ,ntab  ,table  ,&
&sbdscr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SABOUN (SAlt BOUNdaries)
!
! Module description: Determine the salt boundary conditions.
!
!                     In the salt module boundary conditions are used
!                     for solving the nodal administration matrix. The
!                     concentrations at inflow boundaries (option 1 or
!                     2) are determined. For a Thatcher-Harleman bounda-
!                     ry also the time and concentration at the last
!                     outflow step are stored.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  8 csa1(ngrid)       I  Salt concentration in every grid point at time
!                         t(n).
!  4 maxtab            I  Maximum number of defined tables.
!  1 nboun             I  Number of boundary nodes.
!  2 nbran             I  Number of branches.
!  3 ngrid             I  Number of grid points in network.
! 11 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
! 13 sbdscr(3,nboun)   IO Intermediate results at salt boundaries:
!                         (1,i) = Last time of outflow (option = 2)
!                         (2,i) = Concentration at last time of outflow
!                                 (option = 2)
!                         (3,i) = Concentration at inflow (time n+1) if
!                                 option is 1 or 2
! 12 table             P  -
!  6 time              I  Actual time level tn+1. in sec.
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
! $Log: saboun.pf,v $
! Revision 1.5  1999/03/15  15:53:19  kuipe_j
! tabs removed
!
! Revision 1.4  1996/04/11  08:25:20  kuipe_j
! Kalman module added
!
! Revision 1.3  1995/05/30  09:56:03  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:05:52  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:33  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:17:02  kuipe_j
! Time , timestep and period in double precision.
!
! Revision 1.2  1993/11/26  15:33:23  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:11  kuipe_j
! Initial version
!
!
!***********************************************************************

!DEC$ IF DEFINED (_DLL)
   use SobekRE_OpenMI
!DEC$ ENDIF
!
!     Declaration of parameters
!
   integer nboun ,nbran    ,ngrid ,maxtab  ,ntabm, nodenm
   integer branch(4,nbran) ,ntab  (4,maxtab)
   real    csa1 (ngrid)  ,&
   &sbdpar(5,nboun) ,sbdscr(3,nboun) ,table(ntabm)
   double  precision        time            ,q2   (ngrid)
!
!     Declaration of local variables
!
   integer ibn  ,ibr  ,igr  ,itab ,iopt
   real    s    ,con  ,tout ,t0   ,cout  ,pi   ,timdif
!
!     External functions
!
   integer gtcpnt


!
!     Include sobek constants
!
   include '..\include\sobcon.i'
   include '..\include\mempool.i'
!

!DEC$ IF DEFINED (_DLL)

   if (OpenMIActive()) then
   nodenm =     gtcpnt ( 'NODENM')
endif
!DEC$ ENDIF

do 10 ibn = 1,nboun
!
   iopt = int (sbdpar(1,ibn))
!
   if (iopt .eq. csbusr .or. iopt .eq. csbthh) then
!
!           User specified concentration (option=1) or
!           Thatcher Harleman (option=2).
!           Determine if bound is at begin or end of branch.
!
      ibr = int(sbdpar(3,ibn))
      if (branch(1,ibr) .eq. int(sbdpar(2,ibn))) then
         igr   = branch(3,ibr)
         s     = 1.
      else
         igr   = branch(4,ibr)
         s     = -1.
      endif
!
      if (q2(igr)*s .le. 0. .and. iopt .eq. csbthh) then
!
!              There was outflow on a Thatcher Harleman boundary.
!              Store time and concentration of this time step.
!
         sbdscr(1,ibn) = sngl(time)
         sbdscr(2,ibn) = csa1(igr)
!
      else if (q2(igr)*s .gt. 0.) then
!
!              Inflow at current time (step n+1).
!              Get concentration from a user defined table.
!
!              itab       : TABLE number con=f(t)
!              time     . : t(n+1)
!
         itab  = int(sbdpar(4,ibn))
!
         call inttab (ntab (1,itab)      ,ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &time               ,con         )

!DEC$ IF DEFINED (_DLL)

         if (OpenMIActive()) then
            call GetBoundariesSalt(cp(nodenm), ibn, nboun, con)
         endif
!DEC$ ENDIF
!
         if (iopt .eq. csbusr) then
!
!                 User defined concentration
!
            sbdscr(3,ibn) = con
         else
!
!                 Thatcher Harleman.
!                 [ Doc. S-FO-001.5KV / Eq 22-21 ]
!
            tout = sbdscr(1,ibn)
            t0   = sbdpar(5,ibn)
            if (sngl(time) .ge. tout+t0) then
!
!                    Adaptation time exceeded. Use user defined
!                    concentration.
!
               sbdscr(3,ibn) = con
            else
!
!                    Adaptation time not exceeded.Interpolate
!                    between last outflow concentration and the
!                    user defined concentration.
!
               cout  = sbdscr(2,ibn)
               pi    = 4.*atan(1.)
               timdif= sngl(time-dble(tout))
               sbdscr(3,ibn) = cout + (con - cout) * .5 *&
               &(1. - cos(pi*timdif/t0))
            endif
         endif
      endif
   endif
10 continue
!
end
