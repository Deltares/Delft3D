subroutine CSINTG (ngrid,&
&maxlev, nlev  , hlev  ,&
&width , area  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Cross Sectional Table Module
!
! Programmer:         J.Brouwer
!
! Module:             CSINTG (Cross Section INTeGration of tables)
!
! Module description: Subroutine CSINTG performs the numerical integra-
!                     tion of tables.
!
!                     In subroutine CSINTG from the passed table of
!                     flow/total widths, tables will be computed for
!                     flow/total areas. As a consequence of this in the
!                     SOBEK time integration the actual flow/ total
!                     areas can easily be computed from the generated
!                     tables.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 area(ngrid,       IO Flow/total area.
!       maxlev)
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
! $Log: csintg.pf,v $
! Revision 1.3  1996/05/30  09:59:50  kuipe_j
! comment char
!
! Revision 1.2  1995/05/30  06:55:26  hoeks_a
! files changed from dos-file to unix-files
!
! Revision 1.1  1995/04/13  06:58:30  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:29:48  kuipe_j
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
   real    width(ngrid,maxlev), area(ngrid,maxlev)
   double precision hlev (ngrid,maxlev)
!
!     Declaration of local variables:
!
   integer i, j
   real    z1, z2, w1, w2
!
!     Loop over network
!
   do 100 i = 1, ngrid
!
!        Numerical integration by trapezoidal rule
!
      z2  = hlev(i,1)
      w2  = width(i,1)
!
      area(i,1) = 0.0
!
      do 10 j = 2, nlev(i)
         z1  = z2
         w1  = w2
!
         z2  = hlev(i,j)
         w2  = width(i,j)
!
         area(i,j) = area(i,j-1) + 0.5*(z2-z1)*(w1+w2)
10    continue
!
100 continue
!
end
