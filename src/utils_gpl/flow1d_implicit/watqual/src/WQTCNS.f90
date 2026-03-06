subroutine wqtcns ( igrid , nsegmt, isegto, isegfr,&
&nsegtb, segmnt, segtab )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         J.A.G. van Gils
!
! Module:             WQTCNS (Water Quality geT CoNnected Segments)
!
! Module description: Get connected segments for requested grid point.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 ngrid             I  nr of gridpoints
!  2 nsegmt            I  nr of entries in segmnt(segments + boundaries)
!  3 isegto            O  segment index to
!  4 isegfr            O  segment index from
!  5 nsegtb            I  nr of entries in segtab
!  6 segmnt(3,nsegmt)  I  Segment definition:
!                         (1,i) = segment number (if > 0)
!                         (2,i) = first entry in segtab for this segment
!                         (3,i) = nr.of entries in segtab for this
!                                 segment
!                         (1,i) = negative boundary number (if < 0)
!                         (2,i) = grdpoint where boundary is situated
!                         (3,i) = should have value 0!
!  7 segtab(5,nsegtb)  I  This table contains for each segment the en-
!                         closed grid-cells together with the length
!                         factors and section indication.
!                         (1,j) = Gridpoint 1
!                         (2,j) = Gridpoint 2
!                         (3,j) = Length factor Lb
!                         (4,j) = Length factor Le
!                         (5,j) = Section
!                                 cnopar (0) : No parallel sections
!                                 cmainc (1) : Main channel
!                                 csub1  (2) : Sub section 1
!                                 csub2  (3) : Sub section 2
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: wqtcns.pf,v $
! Revision 1.2  1999/03/12  12:42:28  kuipe_j
! parallel segments added
!
! Revision 1.1  1996/10/31  09:51:48  kuipe_j
! Calculation of exchanges added
!
!
!***********************************************************************
!

!     Arguments

   integer      igrid ,&
   &nsegmt,&
   &isegto ,&
   &isegfr ,&
   &nsegtb
   integer      segmnt(3,nsegmt)
   real         segtab(5,nsegtb)
!
!     Locals
!
   integer      isegm ,&
   &isgtb ,&
   &nsgtb ,&
   &isgtb0

   isegto = 0
   isegfr = 0
   do 20 isegm = 1,nsegmt
!     ARS 6809 Bug fix
      if ( segmnt(1,isegm) .gt. 0 ) then
         isgtb0 = segmnt(2,isegm)
!
!       Search only for the main segment, as sub segments
!       do have adjacent numbers
!
         if (nint(segtab(5,isgtb0)) .le. 1 )  then
            nsgtb  = segmnt(3,isegm)
            do 10 isgtb = isgtb0,isgtb0+nsgtb-1
               if ( igrid .eq. nint(segtab(1,isgtb)) .and.&
               &segtab(3,isgtb) .eq. 0.0) then
                  isegto = isegm
               endif
               if ( igrid .eq. nint(segtab(2,isgtb)) .and.&
               &segtab(4,isgtb) .eq. 1.0) then
                  isegfr = isegm
               endif
10          continue
         endif
      endif
20 continue
   return
end
