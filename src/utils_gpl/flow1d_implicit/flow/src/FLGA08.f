      subroutine FLGA08 (strpar, istru , nstru , af    , wf    ,
     &                   h     , ngrid , q     , iup   , idn   ,
     &                   teken , npier , wn    , kp    , ka    ,
     &                   le    , go    , gu    , lw    ,
     &                   li    , lo    , c2    , c3    , wp    ,
     &                   ksa   , ks    , alpha , ksii  , cga   ,
     &                   beta  , bot   , qin   )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLGA08 (FLow Get Arguments for structure type 08)
c
c Module description: Unpack the array strpar for a sluice with over/
c                     underflow gate.
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
c $Log: flga08.pf,v $
c Revision 1.2  1998/06/08  12:35:30  kuipe_j
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
      integer istru , nstru , iup   , idn   , ngrid , teken ,
     &        npier
      real    strpar(dmstrpar,nstru)
      real    wn    , kp    , ka    , le    , go    ,
     &        gu    , lw    , li    , lo    , c2    , c3    ,
     &        wp    , ksa   , ks    , alpha , ksii  , cga   ,
     &        beta  , bot   , 
     &        af(ngrid), wf(ngrid)
      double precision h(ngrid), q(ngrid),qin
c
c unpack array
      wn    = strpar(1,istru)
      npier = INT(strpar(2,istru))
      kp    = strpar(3,istru)
      ka    = strpar(4,istru)
      le    = strpar(5,istru)
      go    = strpar(6,istru)
      gu    = strpar(7,istru)
      lw    = strpar(8,istru)
      li    = strpar(9,istru)
      lo    = strpar(10,istru)
      c2    = strpar(11,istru)
      c3    = strpar(12,istru)
      wp    = strpar(13,istru)
      ksa   = strpar(14,istru)
      ks    = strpar(15,istru)
      alpha = strpar(16,istru)
      ksii  = strpar(17,istru)
      cga   = INT(strpar(18,istru))
      beta  = strpar(19,istru)
c
      if (teken .gt. 0) then
        bot   = h(iup) - af(iup) / wf(iup)
        qin   = q(iup)
      else
        bot   = h(idn) - af(idn) / wf(idn)
        qin   = q(idn)
      endif
c
      return
      end
