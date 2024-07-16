      subroutine FLGA03 (strpar, istru , nstru , af    , wf    ,
     &                   h     , ngrid , teken , q     , iup   ,
     &                   idn   , npier , wn    , le    , cg    ,
     &                   leg   , ksii  , kp    , ka    , lw    ,
     &                   li    , lo    , ksa   , ksi   , kso   ,
     &                   wp    , alpha , beta  , bot   , qin   )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLGA03 (FLow Get Arguments for structure type 03)
c
c Module description: Unpack the array strpar for a sluice with bottom
c                     hinged gate.
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flga03.pf,v $
c Revision 1.2  1998/06/08  12:35:24  kuipe_j
c log added
c
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c declare arguments
      integer teken , istru , nstru , ngrid , iup   , idn   ,
     &        npier
      real    strpar(dmstrpar,nstru)
      real    af(ngrid), wf(ngrid)
      double precision  q(ngrid), h(ngrid)
      real    wn    , le    , cg    , leg   , ksii  , kp    ,
     &        ka    , lw    , li    , lo    , ksa   , ksi   ,
     &        kso   , wp    , alpha , beta  , bot   , qin
c
c unpack array
      wn    = strpar(1,istru)
      le    = strpar(2,istru)
      cg    = strpar(3,istru)
      leg   = strpar(4,istru)
      ksii  = strpar(5,istru)
      npier = int(strpar(6,istru))
      kp    = strpar(7,istru)
      ka    = strpar(8,istru)
      lw    = strpar(9,istru)
      li    = strpar(10,istru)
      lo    = strpar(11,istru)
      ksa   = strpar(12,istru)
      ksi   = strpar(13,istru)
      kso   = strpar(14,istru)
      wp    = strpar(15,istru)
      alpha = strpar(16,istru)
      beta  = strpar(17,istru)
c
      if (teken .gt. 0) then
        bot = h(iup) - af(iup) / wf(iup)
        qin = q(iup)
      else
        bot = h(idn) - af(idn) / wf(idn)
        qin = q(idn)
      endif
c
      return
      end
