      subroutine sarlti (add   , nboun , sbdpar , sbdscr , 
     *                   dsopt , thasca, nmouth , timout            )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Module
c
c Programmer:         A.W.J.Koster 
c
c Module:             SARLTI (SAlt adapt ReL. TIme for Thatcher-Harleman)
c
c Module description: Add a time increment to 
c                     - the last time of outflow for all Thatcher-Harleman 
c                       boundaries
c                     - end time of current tide
c-----------------------------------------------------------------------
c Parameters:
c  1 add               I  time increment 
c  2 nboun             I  Number of boundary nodes.
c  3 sbdpar(5,nboun)   I  Definition of salt boundary conditions:
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
c  4 sbdscr(3,nboun)   IO Intermediate results at salt boundaries:
c                         (1,i) = Last time of outflow (option = 2)
c                         (2,i) = Concentration at last time of outflow
c                                 (option = 2)
c                         (3,i) = Concentration at inflow (time n+1) if
c                                 option is 1 or 2
c  5 dsopt             I  Option for dispersion for the whole network:
c                           cds1fu (1) : One function of place or time
c                           cds2fu (2) : Two functions of place or time
c                           cdsthh (3) : Thatcher-Harleman formulation
c                           cdsemp (4) : Empirical formulation
c  6 thasca(3)         IO Administration for the calculation of
c                         <c/c0*dc/dx>:
c                         (1) =   End time of current tide.
c                         (2) =   Number of time steps that contribute
c                                 in current sum.
c                         (3) =   0 : First tidal period not started yet.
c                                 1 : First tidal period has started.
c  7 nmouth            I  Maximum number of mouths in the network.
c  8 timout(2,nmouth)  O  Administration for the calculation of fresh
c                         water discharge, flood volume and maximum
c                         flood velocity for every mouth:
c                         (1,i) = Starting time of current tide.
c                         (2,i) = 0 : Fist tidal period not started yet.
c                                 1 : Fist tidal period has started.
c
c NR NAME              IO DESCRIPTION
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sarlti.pf,v $
c Revision 1.4  1999/03/15  15:53:25  kuipe_j
c tabs removed
c
c Revision 1.3  1996/12/04  12:00:39  kuipe_j
c declarations / undefined vars
c
c Revision 1.2  1996/12/03  09:08:42  kuipe_j
c cvs keys
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer dsopt ,nmouth   ,nboun
      real    add
      real    sbdpar(5,nboun) ,sbdscr(3,nboun), thasca(3) ,
     &        timout(2,nmouth)   
c
c     Declaration of local variables
c
      integer ibn  ,iopt ,im
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      if (dsopt .eq. cdsthh .or. dsopt .eq. cdsemp) then
         thasca(1) = thasca(1) + add
         do 10 im=1,nmouth
            timout(1,im) = timout(1,im) + add
  10     continue
      endif
c
      do 20 ibn = 1,nboun
c
         iopt = int (sbdpar(1,ibn))
c
         if (iopt .eq. csbthh) then
            sbdscr(1,ibn) = sbdscr(1,ibn) + add
         endif
   20 continue
c
      return
      end
