      subroutine wqtbou ( iboun , segmnt , nsegmt , igrid )
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         J.Kuipers      
c
c Module:             WQTBOU (Water Quality geT BOUndary)
c
c Module description: Get segment number for a specified boundary 
c                     grid point.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 igrid             I  boundary gridpoint number
c  1 iboun             O  segment number
c  3 nsegtb            I  nr of entries in segtab
c  2 segmnt(3,nsegmt)  I  Segment definition:
c                         (1,i) = segment number (if > 0)
c                         (2,i) = first entry in segtab for this segment
c                         (3,i) = nr.of entries in segtab for this
c                                 segment
c                         (1,i) = negative boundary number (if < 0)
c                         (2,i) = grdpoint where boundary is situated
c                         (3,i) = should have value 0!
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqtbou.pf,v $
c Revision 1.1  1996/10/31  09:51:47  kuipe_j
c Calculation of exchanges added
c
c
c***********************************************************************
c

c     Arguments

      integer      igrid ,
     j             nsegmt,
     j             iboun
      integer      segmnt(3,nsegmt)

c     Locals

      integer      isegmt

      iboun = 0
      do 100 isegmt = 1,nsegmt

         if ( segmnt(1,isegmt) .lt. 0 ) then
            if ( segmnt(2,isegmt) .eq. igrid ) then
               iboun = segmnt(1,isegmt)
               goto 110
            endif
         endif

  100 continue
  110 continue

      end

