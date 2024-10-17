subroutine sarlti (add   , nboun , sbdpar , sbdscr ,&
&dsopt , thasca, nmouth , timout            )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Module
!
! Programmer:         A.W.J.Koster
!
! Module:             SARLTI (SAlt adapt ReL. TIme for Thatcher-Harleman)
!
! Module description: Add a time increment to
!                     - the last time of outflow for all Thatcher-Harleman
!                       boundaries
!                     - end time of current tide
!-----------------------------------------------------------------------
! Parameters:
!  1 add               I  time increment
!  2 nboun             I  Number of boundary nodes.
!  3 sbdpar(5,nboun)   I  Definition of salt boundary conditions:
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
!  4 sbdscr(3,nboun)   IO Intermediate results at salt boundaries:
!                         (1,i) = Last time of outflow (option = 2)
!                         (2,i) = Concentration at last time of outflow
!                                 (option = 2)
!                         (3,i) = Concentration at inflow (time n+1) if
!                                 option is 1 or 2
!  5 dsopt             I  Option for dispersion for the whole network:
!                           cds1fu (1) : One function of place or time
!                           cds2fu (2) : Two functions of place or time
!                           cdsthh (3) : Thatcher-Harleman formulation
!                           cdsemp (4) : Empirical formulation
!  6 thasca(3)         IO Administration for the calculation of
!                         <c/c0*dc/dx>:
!                         (1) =   End time of current tide.
!                         (2) =   Number of time steps that contribute
!                                 in current sum.
!                         (3) =   0 : First tidal period not started yet.
!                                 1 : First tidal period has started.
!  7 nmouth            I  Maximum number of mouths in the network.
!  8 timout(2,nmouth)  O  Administration for the calculation of fresh
!                         water discharge, flood volume and maximum
!                         flood velocity for every mouth:
!                         (1,i) = Starting time of current tide.
!                         (2,i) = 0 : Fist tidal period not started yet.
!                                 1 : Fist tidal period has started.
!
! NR NAME              IO DESCRIPTION
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sarlti.pf,v $
! Revision 1.4  1999/03/15  15:53:25  kuipe_j
! tabs removed
!
! Revision 1.3  1996/12/04  12:00:39  kuipe_j
! declarations / undefined vars
!
! Revision 1.2  1996/12/03  09:08:42  kuipe_j
! cvs keys
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer dsopt ,nmouth   ,nboun
   real    add
   real    sbdpar(5,nboun) ,sbdscr(3,nboun), thasca(3) ,&
   &timout(2,nmouth)
!
!     Declaration of local variables
!
   integer ibn  ,iopt ,im
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   if (dsopt .eq. cdsthh .or. dsopt .eq. cdsemp) then
      thasca(1) = thasca(1) + add
      do 10 im=1,nmouth
         timout(1,im) = timout(1,im) + add
10    continue
   endif
!
   do 20 ibn = 1,nboun
!
      iopt = int (sbdpar(1,ibn))
!
      if (iopt .eq. csbthh) then
         sbdscr(1,ibn) = sbdscr(1,ibn) + add
      endif
20 continue
!
   return
end
