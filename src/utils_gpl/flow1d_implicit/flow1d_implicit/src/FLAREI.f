      subroutine FLAREI (hact  ,htop  ,daext ,overlp,delA  ,iart  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLAREI (FLow correction for ARea Extra Initial)
c
c Module description: The initial calculated flow or total area must be
c                     corrected with the extra area.
c
c                     Depending on the option the area is calculated in
c                     the concerning grid point.
c                     As well as, both "artop" and the extra area are
c                     determined.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 daext             I  Extra area (flow or total).
c  5 dela              O  Computed extra area depending on option.
c  1 hact              I  Actual water level at gridpoint in branch.
c  2 htop              I  top-level
c  6 iart              O  Status-variable indicating rising or falling
c                         water level.
c  4 overlp            I  adaptation height.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flarei.pf,v $
c Revision 1.4  1999/03/15  15:49:24  kuipe_j
c tabs removed
c
c Revision 1.3  1996/04/12  13:03:35  kuipe_j
c headers, minor changes
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer  iart
      real     hact  ,htop  ,daext ,overlp,delA
c
c     Declaration of local variables:
c
      real htopp
c
      htopp = htop + overlp
c
      if (hact .gt. htopp) then
         iart  = 0
         delA  = daext
      else
         iart  = 1
         delA  = 0.
      endif
c
      end
