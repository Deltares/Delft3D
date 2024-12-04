subroutine wqfbgp ( ngrid  ,qaggr  ,igpfrm ,igpto  ,&
&isecwq ,intpol ,qex    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQFBGP (Water Quality Flow Between GridPoints)
!
! Module description: This routine calculates the exchange flow between
!                     two gridpoints.
!
!                     First the left and right flows are calculated and
!                     an interpolation is done to calculate the exchange
!                     flow between the gridpoints. The sign of the cal-
!                     culated exchange flow will be changed if the
!                     pointer direction is opposite to the branch direc-
!                     tion.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 igpfrm            I  Gridpoint from.
!  4 igpto             I  Gridpoint to.
!  6 intpol            I  Interpolation factor.
!  5 isecwq            P  -
!  1 ngrid             I  Number of grid points in network.
!  2 qaggr             P  -
!  7 qex               IO Calculated exchange flow.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqgpfl  Water Quality GridPoint FLow
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
! $Log: wqfbgp.pf,v $
! Revision 1.3  1999/03/15  15:53:52  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:08:26  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:46  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:32  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:27  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer  ngrid,&
   &igpfrm,&
   &igpto,&
   &isecwq

   real     intpol, qex

   real     qaggr (ngrid,3)

!
!     Variables
!
   real     q1, q2
!
!     Calculate flow on left side
!
   call wqgpfl ( ngrid  ,qaggr  ,igpfrm ,&
   &isecwq ,q1     )
!
!     Calculate flow on right side
!
   call wqgpfl ( ngrid  ,qaggr  ,igpto ,&
   &isecwq ,q2     )
!
!     Calculate exchange flow by interpolation
!
   qex = ( (1.-intpol) * q1 ) + ( intpol * q2 )
!
!     Change sign if from -> to is negative
!
   if (igpto .lt. igpfrm) then
      qex = -qex
   endif
!
   return
end
