      subroutine FLGA10 (strpar, le    , appwid, gapwid, gaplen,
     &                   ks    , c1    , c3    , ccr   , applen,
     &                   itype , q     , ngrid , qin   , iup   ,
     &                   idn   , teken , istru , nstru )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Parse New Model Database (V2.0)
c
c Programmer:         P.R. Evans
c
c Module:             FLow Get Arguments structure type 10 (Abutments)
c
c Module description: Unpack array strpar for structure type 10
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
c $Log: flga10.pf,v $
c Revision 1.2  1998/06/08  12:35:33  kuipe_j
c log added
c
c
c
c***********************************************************************
c
      implicit none
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c declare arguments
      integer istru , nstru
      real    strpar(dmstrpar,nstru)
      real    le    , appwid, gapwid, gaplen, ks    ,
     &        c1    , c3    , ccr   , applen, qin
      integer itype , iup   , idn   , ngrid , teken
      double precision  q(ngrid)
c
c unpack array
      le     = strpar(1,istru)
      appwid = strpar(2,istru)
      gapwid = strpar(3,istru)
      gaplen = strpar(4,istru)
      ks     = strpar(5,istru)
      c1     = strpar(6,istru)
      c3     = strpar(7,istru)
      ccr    = strpar(8,istru)
      applen = strpar(9,istru)
      itype  = INT(strpar(10,istru))
c
      if (teken .gt. 0) then
        qin   = q(iup)
      else
        qin   = q(idn)
      endif
c
      return
      end
