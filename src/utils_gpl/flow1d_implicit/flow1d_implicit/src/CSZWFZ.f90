subroutine CSZWFZ (ngrid  ,&
&maxlev ,nlev   ,hlev   ,&
&width  ,izwft  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Cross Sectional Table Module
!
! Programmer:         J.Brouwer
!
! Module:             CSZWFZ (Cross Section z Wf(z) dz)
!
! Module description: Compute a table for each defined water level con-
!                     taining integrated widths.
!
!                     Compute a table for each defined water level con-
!                     taining integrated widths. This table is used to
!                     compute the A1m integral in case of salt. For this
!                     routine the table with flow widths is required.
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
!  6 izwft(ngrid,      IO Table containing integrated widths.
!       maxlev)
!  2 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  1 ngrid             I  Number of grid points in network.
!  3 nlev(ngrid)       I  Number of h-levels for every cross section.
!                         (For a circle cross section   : 1 ;
!                          For a sedredge cross section : 2 )
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
! $Log: cszwfz.pf,v $
! Revision 1.4  1999/03/15  15:49:11  kuipe_j
! tabs removed
!
! Revision 1.3  1996/05/30  09:59:56  kuipe_j
! comment char
!
! Revision 1.2  1995/05/30  06:55:33  hoeks_a
! files changed from dos-file to unix-files
!
! Revision 1.1  1995/04/13  06:58:36  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:29:57  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:41  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer ngrid, maxlev
   integer nlev(ngrid)
   real width(ngrid,maxlev), izwft(ngrid,maxlev)
   double precision hlev (ngrid,maxlev)
!
!     Declaration of local variables:
!
   integer i, j
   real    h1, h2, w1, w2
!
!     Loop over network
!
   do 100 i = 1, ngrid
!
!        Numerical integration by trapezoidal rule
!
      h2  = hlev(i,1)
      w2  = width(i,1)
!
      izwft(i,1) = 0.0
!
      do 10 j = 2, nlev(i)
         h1  = h2
         w1  = w2
!
         h2  = hlev(i,j)
         w2  = width(i,j)
!
         izwft(i,j) = izwft(i,j-1) +&
         &( h2*h2*h2/6. + h1*h1*h1/3. - h2*h1*h1/2. ) /&
         &( h2 - h1 ) * w1 +&
         &( h1*h1*h1/6. + h2*h2*h2/3. - h2*h2*h1/2. ) /&
         &( h2 - h1 ) * w2
!
10    continue
!
100 continue
!
end
