      subroutine wqtcns ( igrid , nsegmt, isegto, isegfr, 
     j                    nsegtb, segmnt, segtab )
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         J.A.G. van Gils
c
c Module:             WQTCNS (Water Quality geT CoNnected Segments)
c
c Module description: Get connected segments for requested grid point.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 ngrid             I  nr of gridpoints
c  2 nsegmt            I  nr of entries in segmnt(segments + boundaries)
c  3 isegto            O  segment index to
c  4 isegfr            O  segment index from
c  5 nsegtb            I  nr of entries in segtab
c  6 segmnt(3,nsegmt)  I  Segment definition:
c                         (1,i) = segment number (if > 0)
c                         (2,i) = first entry in segtab for this segment
c                         (3,i) = nr.of entries in segtab for this
c                                 segment
c                         (1,i) = negative boundary number (if < 0)
c                         (2,i) = grdpoint where boundary is situated
c                         (3,i) = should have value 0!
c  7 segtab(5,nsegtb)  I  This table contains for each segment the en-
c                         closed grid-cells together with the length
c                         factors and section indication.
c                         (1,j) = Gridpoint 1
c                         (2,j) = Gridpoint 2
c                         (3,j) = Length factor Lb
c                         (4,j) = Length factor Le
c                         (5,j) = Section
c                                 cnopar (0) : No parallel sections
c                                 cmainc (1) : Main channel
c                                 csub1  (2) : Sub section 1
c                                 csub2  (3) : Sub section 2
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
c $Log: wqtcns.pf,v $
c Revision 1.2  1999/03/12  12:42:28  kuipe_j
c parallel segments added
c
c Revision 1.1  1996/10/31  09:51:48  kuipe_j
c Calculation of exchanges added
c
c
c***********************************************************************
c

c     Arguments

      integer      igrid ,
     j             nsegmt,
     j             isegto ,
     j             isegfr ,
     j             nsegtb
      integer      segmnt(3,nsegmt)
      real         segtab(5,nsegtb)
c
c     Locals
c
      integer      isegm ,
     j             isgtb ,
     j             nsgtb ,
     j             isgtb0

      isegto = 0
      isegfr = 0
      do 20 isegm = 1,nsegmt
c     ARS 6809 Bug fix
      if ( segmnt(1,isegm) .gt. 0 ) then
        isgtb0 = segmnt(2,isegm)
c
c       Search only for the main segment, as sub segments
c       do have adjacent numbers
c
        if (nint(segtab(5,isgtb0)) .le. 1 )  then
          nsgtb  = segmnt(3,isegm)
          do 10 isgtb = isgtb0,isgtb0+nsgtb-1
            if ( igrid .eq. nint(segtab(1,isgtb)) .and.
     j           segtab(3,isgtb) .eq. 0.0) then
               isegto = isegm
            endif
            if ( igrid .eq. nint(segtab(2,isgtb)) .and.
     j           segtab(4,isgtb) .eq. 1.0) then
               isegfr = isegm
            endif
   10     continue
        endif
      endif
   20 continue
      return
      end
