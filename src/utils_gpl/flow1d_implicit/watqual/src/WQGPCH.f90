subroutine wqgpch ( ngrid ,c     ,igp   ,isecwq  ,chezy )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQGPCH (Water Quality GridPoint CHezy)
!
! Module description: This routine calculates the Chezy coefficient in a
!                     particular gridpoint.
!
!                     The requested Chezy coefficient is extracted. When
!                     the whole channel is indicated the Chezy coeffi-
!                     cient for the whole channel is returned, else the
!                     Chezy coefficient for the requested section.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 c(ngrid)          I  Actual Chezy coefficient for total channel in
!                         every grid point.
!  5 chezy             O  Calculated chezy coefficient.
!  3 igp               I  Gridpoint.
!  4 isecwq            I  Section:
!                         0 = Channel including parallel sections
!                         1 = Main channel
!                         2 = Sub section 1
!                         3 = Sub section 2
!  1 ngrid             I  Number of grid points in network.
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
! $Log: wqgpch.pf,v $
! Revision 1.3  1999/03/15  15:54:00  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:08:35  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:56  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer  igp,&
   &isecwq,&
   &ngrid

   real     chezy

   real     c(ngrid,4)

!
!     Determine chezy coefficient
!
   if (isecwq .eq. 0) then
      chezy = c (igp,1)
   else
      chezy = c (igp,isecwq+1)
   endif

   return
end
