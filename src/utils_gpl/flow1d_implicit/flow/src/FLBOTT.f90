function FLBOTT (igr    ,ngrid  ,maxlev ,hlev   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLBOTT (FLow determine BOTTom)
!
! Module description: Determine the lowest bed level for a sedredge
!                     branch.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 flbott            O  Function value.
!  4 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  1 igr               I  Gridpoint index.
!  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  2 ngrid             I  Number of grid points in network.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flbott.pf,v $
! Revision 1.3  1995/05/30  09:54:47  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:44  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:30  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:30:35  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:47  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Function declaration:
!
   real FLBOTT
!
!     Declaration of Parameters:
!
   integer igr, ngrid, maxlev
   double precision hlev(ngrid,maxlev)
!
   FLBOTT = min ( hlev(igr,1), hlev(igr,2) )
!
end
