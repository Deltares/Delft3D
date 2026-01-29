subroutine wqtbou ( iboun , segmnt , nsegmt , igrid )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         J.Kuipers
!
! Module:             WQTBOU (Water Quality geT BOUndary)
!
! Module description: Get segment number for a specified boundary
!                     grid point.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 igrid             I  boundary gridpoint number
!  1 iboun             O  segment number
!  3 nsegtb            I  nr of entries in segtab
!  2 segmnt(3,nsegmt)  I  Segment definition:
!                         (1,i) = segment number (if > 0)
!                         (2,i) = first entry in segtab for this segment
!                         (3,i) = nr.of entries in segtab for this
!                                 segment
!                         (1,i) = negative boundary number (if < 0)
!                         (2,i) = grdpoint where boundary is situated
!                         (3,i) = should have value 0!
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
! $Log: wqtbou.pf,v $
! Revision 1.1  1996/10/31  09:51:47  kuipe_j
! Calculation of exchanges added
!
!
!***********************************************************************
!

!     Arguments

   integer      igrid ,&
   &nsegmt,&
   &iboun
   integer      segmnt(3,nsegmt)

!     Locals

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

