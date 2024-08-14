      subroutine FLGA07 (strpar, istru , nstru , ngrid , npier ,
     &                   q     , wn    , kp    , ka    , level ,
     &                   hgate , lw    , li    , lo    , c2    ,
     &                   c3    , ksa   , ks    , ksii  , wp    ,
     &                   alpha , qin   , bot   , hlev  , maxlev,
     &                   beta  , iup   , idn   , teken )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLGA07 (FLow Get Arguments for structure type 07)
c
c Module description: Unpack the array strpar for a sluice with 
c                     underflow gate
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
c $Log: flga07.pf,v $
c Revision 1.2  1998/06/08  12:35:28  kuipe_j
c log added
c
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c declare arguments
      integer teken , istru , nstru , ngrid , npier , maxlev,
     &        iup   , idn 
      real    strpar(dmstrpar,nstru)
      double precision  q(ngrid)
      real    wn    , kp    , ka    , level , hgate , lw    ,
     &        li    , lo    , c2    , c3    , ksa   , ks    ,
     &        ksii  , wp    , alpha , qin   , beta  , bot 
      double precision hlev(ngrid,maxlev) 
c
c unpack array
      wn    = strpar(1,istru)
      npier = INT(strpar(2,istru))
      kp    = strpar(3,istru)
      ka    = strpar(4,istru)
      level = strpar(5,istru)
      hgate = strpar(6,istru)
      lw    = strpar(7,istru)
      li    = strpar(8,istru)
      lo    = strpar(9,istru)
      c2    = strpar(10,istru)
      c3    = strpar(11,istru)
      ksa   = strpar(12,istru)
      ks    = strpar(13,istru)
      ksii  = strpar(14,istru)
      wp    = strpar(15,istru)
      alpha = strpar(16,istru)
      beta  = strpar(17,istru)
c
      if (teken .gt. 0) then
        qin   = q(iup)
        bot   = MIN(hlev(iup,1),hlev(iup,2))
      else
        qin   = q(idn)
        bot   = MIN(hlev(idn,1),hlev(idn,2))
      endif
c
      return
      end
