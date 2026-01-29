subroutine wqaigp ( ngrid  ,af     ,afs    ,igp    ,&
&isecfr ,isecto ,exarea )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQAIGP (Water Quality Area In GridPoint)
!
! Module description: This routine calculates the exchange area in a
!                     particular gridpoint/section.
!
!                     The routine calculates two areas (A1 and A2) and
!                     determines the minimum of these values. This mini-
!                     mum is the exchange area in the gridpoint/section.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 af                P  -
!  3 afs               P  -
!  7 exarea            O  Calculated exchange area.
!  4 igp               P  -
!  5 isecfr            P  -
!  6 isecto            P  -
!  1 ngrid             I  Number of grid points in network.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! wqgpar  Water Quality GridPoint ARea
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
! $Log: wqaigp.pf,v $
! Revision 1.3  1999/03/15  15:53:45  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:08:19  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:40  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:35:20  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:26  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer  igp    ,isecfr ,isecto ,ngrid
   real     exarea
   real     af  (ngrid),&
   &afs (ngrid,2)

!
!     Variables
!
   real     area1 ,area2

!
!     Calculate "from" area
!
   call wqgpar ( ngrid  ,af     ,afs    ,igp    ,&
   &isecfr ,area1  )
!
!     Calculate "to" area
!
   call wqgpar ( ngrid  ,af     ,afs    ,igp    ,&
   &isecto ,area2  )

!
!     Exchange area is minimum surface of "from" and "to" area
!
   exarea = min ( area1, area2 )

   return
end
