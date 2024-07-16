      subroutine FLGA09 (strpar, istru , nstru , q     , ngrid ,
     &                   qin   , iup   , idn   , teken , wn    ,
     &                   wp    , le    , shapco)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLGA08 (FLow Get Arguments for structure type 09)
c
c Module description: Unpack the array strpar for bridge piers
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
c $Log: flga09.pf,v $
c Revision 1.2  1998/06/08  12:35:31  kuipe_j
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
     &        itype
      real    strpar(dmstrpar,nstru)
      real    wn    , wp    , le    , shapco, qin
      double precision  q(ngrid)
      logical  equal
      external equal
c
c unpack array
      wn     = strpar(1,istru)
      wp     = strpar(2,istru)
      le     = strpar(3,istru)
      shapco = strpar(4,istru)
      itype  = INT(strpar(5,istru))
c
      if (EQUAL(shapco,0.)) then
        if (itype .eq. 1) shapco = 1.25
        if (itype .eq. 2) shapco = 0.9
        if (itype .eq. 3) shapco = 1.05
      endif
   
      if (teken .gt. 0) then
        qin   = q(iup)
      else
        qin   = q(idn)
      endif
c
      return
      end
