subroutine wqabgp ( ngrid  ,af     ,afs    ,igpfrm ,&
&igpto  ,isecwq ,intpol ,exarea&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQABGP (Water Quality Area Between GridPoints)
!
! Module description: This routine calculates the exchange area between
!                     two gridpoints.
!
!                     First the left and right area are calculated and
!                     an interpolation is done to calculate the exchange
!                     area between the gridpoints.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 af                P  -
!  3 afs               P  -
!  8 exarea            O  Calculated exchange area.
!  4 igpfrm            P  -
!  5 igpto             P  -
!  7 intpol            I  Interpolation factor.
!  6 isecwq            P  -
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
! $Log: wqabgp.pf,v $
! Revision 1.2  1995/05/30  07:08:17  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:39  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:26  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer  igpfrm ,igpto ,isecwq ,ngrid

   real     intpol ,exarea

   real     af (ngrid),&
   &afs(ngrid,2)

!
!       Variables
!
   real     area1  ,area2

!
!     Calculate "from" area
!
   call wqgpar ( ngrid  ,af     ,afs    ,igpfrm ,&
   &isecwq ,area1  )
!
!     Calculate "to" area
!
   call wqgpar ( ngrid  ,af     ,afs    ,igpto  ,&
   &isecwq ,area2  )

!
!     Exchange area by interpolation between "from" and "to" area
!
   exarea = ( (1.-intpol) * area1 ) + ( intpol * area2 )

   return
end
