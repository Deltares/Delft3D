subroutine wqfdir ( dir, qcalc, qdir )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Water Quality Interface Module
!
! Programmer:         S.L. van der Woude
!
! Module:             WQFDIR (Water Quality Flow DIRection)
!
! Module description: Determine flow direction
!
!                     This routine determines if a flow is entering or
!                     leaving a node.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 dir               I  Direction of gridpoint (begin or end of
!                         branch)
!                         1 = begin of branch
!                         2 = end of branch
!  2 qcalc             I  Calculated flow value.
!  3 qdir              O  Calculated direction of flow:
!                         0 = direction could not be determined.
!                         1 = entering the node.
!                         2 = leaving the node.
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
! $Log: wqfdir.pf,v $
! Revision 1.3  1999/03/15  15:53:53  kuipe_j
! tabs removed
!
! Revision 1.2  1995/05/30  07:08:27  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:10:48  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:27  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!       Parameters
!
   integer  dir,&
   &qdir

   real     qcalc

!
!       Determine if flow is entering or leaving
!
   if (dir .eq. 1) then
      if (qcalc .lt. 0) then
         qdir = 1
      else
         qdir = 2
      endif
   elseif (dir .eq. 2) then
      if (qcalc .gt. 0) then
         qdir = 1
      else
         qdir = 2
      endif
   else
      qdir = 0
   endif

   return
end
