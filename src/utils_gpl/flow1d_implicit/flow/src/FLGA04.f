      subroutine FLGA04 (strpar, istru , nstru , af    , wf    ,
     &                   h     , ngrid , teken , width , heidia,
     &                   cullen, le    , z     , alpha , ks    ,
     &                   ksa   , rloc  , theta , beta  , bot   ,
     &                   qin   , wloc  , cc    , q     , iup   ,
     &                   idn   , applen, rle   )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLGA04 (FLow Get Arguments for structure type 04)
c
c Module description: Unpack the array strpar for a culvert
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
c $Log: flga04.pf,v $
c Revision 1.2  1998/06/08  12:35:26  kuipe_j
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
      integer istru , nstru , iup   , idn   , ngrid , teken
      real    width , heidia, cullen, le    , z     , alpha ,
     &        ks    , ksa   , rloc  , theta , wloc  , bot   ,
     &        qin   , beta  , applen, rle
      real    strpar(dmstrpar,nstru), cc(6), af(ngrid), wf(ngrid)
      double precision h(ngrid), q(ngrid)
c
c unpack array
      width  = strpar(1,istru)
      heidia = strpar(2,istru)
      cullen = strpar(3,istru)
      le     = strpar(4,istru)
      alpha  = strpar(6,istru)
      ks     = strpar(7,istru)
      ksa    = ks
      rloc   = 0.15
      theta  = 30.
      beta   = 1.0
      applen = 5 * width
c
      if (teken .gt. 0) then
        bot   = h(iup) - af(iup) / wf(iup)
        qin   = q(iup)
        rle   = strpar(4,istru)
        z     = strpar(5,istru)
        wloc  = strpar(8,istru)
        cc(1) = strpar(9,istru)
        cc(2) = strpar(10,istru)
        cc(3) = strpar(11,istru)
        cc(4) = strpar(12,istru)
        cc(5) = strpar(13,istru)
        cc(6) = strpar(14,istru)
      else
        bot   = h(idn) - af(idn) / wf(idn)
        qin   = q(idn)
        rle   = strpar(4,istru) - strpar(5,istru)
        z     = -strpar(5,istru)
        wloc  = strpar(15,istru)
        cc(1) = strpar(16,istru)
        cc(2) = strpar(17,istru)
        cc(3) = strpar(18,istru)
        cc(4) = strpar(19,istru)
        cc(5) = strpar(20,istru)
        cc(6) = strpar(21,istru)
      endif
c
      return
      end
