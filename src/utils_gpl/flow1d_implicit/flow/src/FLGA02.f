      subroutine FLGA02 (strpar, istru , nstru , hlev  , maxlev, 
     &                   ngrid , teken , width , le    , z     ,
     &                   length, alpha , lw    , ksa   , ks    ,
     &                   beta  , lbvin , lbvout, bot   , qin   ,
     &                   rloc  , wloc  , theta , cc    , q     ,
     &                   iup   , idn   )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLGA02 (FLow Get Arguments for structure type 02)
c
c Module description: Unpack the array strpar for an open flume.
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
c $Log: flga02.pf,v $
c Revision 1.2  1998/06/08  12:35:23  kuipe_j
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
      integer teken , istru , nstru , ngrid , iup   , idn
      integer maxlev
      real    strpar(dmstrpar,nstru)
      double precision   q(ngrid) 
      real    width , le    , z     , length, alpha , lw    ,
     &        ksa   , ks    , beta  , bot   , qin   , rloc  ,
     &        wloc  , theta
      real    cc(3)
      double precision hlev(ngrid,maxlev)
      logical lbvin , lbvout
c
c declare local variables
      integer ihulp
c
c unpack array
      width  = strpar(1,istru)
      le     = strpar(2,istru)
      z      = strpar(3,istru)
      length = strpar(4,istru)
      alpha  = strpar(5,istru)
      lw     = strpar(6,istru)
      ksa    = strpar(7,istru)
      ks     = strpar(7,istru)
      beta   = strpar(20,istru)
      lbvin  = .false.
      lbvout = .false.
c
      if (teken .gt. 0) then
        bot   = MIN(hlev(iup,1),hlev(iup,2)) 
        qin   = q(iup)
        rloc  = strpar(8,istru)
        wloc  = strpar(9,istru)
        theta = strpar(10,istru)
        cc(1) = strpar(11,istru)
        cc(2) = strpar(12,istru)
        cc(3) = strpar(13,istru)
        ihulp = INT(strpar(21,istru))/10
        if (ihulp .eq. 1) lbvin = .true.
      else
        bot   = MIN(hlev(idn,1),hlev(idn,2)) 
        qin   = q(idn)
        rloc  = strpar(16,istru)
        wloc  = strpar(14,istru)
        theta = strpar(15,istru)
        cc(1) = strpar(17,istru)
        cc(2) = strpar(18,istru)
        cc(3) = strpar(19,istru)
        ihulp = MOD(INT(strpar(21,istru)),10)
        if (ihulp .eq. 1) lbvout = .true.
      endif
c
      return
      end
