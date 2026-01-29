subroutine CSPERI (ngrid ,&
&maxlev ,&
&nlev   ,hlev    , width  ,&
&of                            )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Cross Sectional Table Module
!
! Programmer:         J.Brouwer
!
! Module:             CSPERI (Cross Sections computation of PERImeter)
!
! Module description: Subroutine CSPERI generates a table of wetted
!                     perimeters.
!
!                     In subroutine CSPERI a table of wetted perimeters
!                     will be computed provided a user supplied table of
!                     total widths.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  2 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 ngrid             I  Number of grid points in network.
!  3 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
!  6 of(ngrid)         IO Actual wetted perimeter at every cross secti-
!                         on.
!  5 width(ngrid,      I  Flow/total width.
!        maxlev)
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: csperi.pf,v $
! Revision 1.3  1996/05/30  09:59:51  kuipe_j
! comment char
!
! Revision 1.2  1995/05/30  06:55:29  hoeks_a
! files changed from dos-file to unix-files
!
! Revision 1.1  1995/04/13  06:58:31  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:29:50  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:42  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer  ngrid, maxlev
   integer  nlev(ngrid)
   double precision hlev(ngrid,maxlev)
   real     width(ngrid,maxlev) ,of(ngrid,maxlev)
!
!     Declaration of local variables:
!
   integer i, j
   real    z1, z2, w1, w2, dz, dw, do
!
!     Loop over network
!
   do 100 i = 1, ngrid
!
      of(i,1) = width(i,1)
!
      z2  = hlev(i,1)
      w2  = width(i,1)
!
!        Compute wetted perimeter Of
!
      do 10 j = 2, nlev(i)
         z1  = z2
         w1  = w2
!
         z2  = hlev(i,j)
         w2  = width(i,j)
!
         dw  = (w2-w1) / 2.0
         dz  = z2-z1
         do  = sqrt( dw*dw + dz*dz )
!
         of(i,j) = of(i,j-1) + 2.0 * do
10    continue
!
100 continue
!
end
