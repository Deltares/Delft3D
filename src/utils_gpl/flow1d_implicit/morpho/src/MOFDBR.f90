subroutine mofdbr ( nbran  ,branch ,igp    ,ibr    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOFDBR (MOrphology FinD BRanch number for a gridpoint)
!
! Module description: Find branch number for a particular grid point
!
!                     For history information grid numbers are defined
!                     where output should be given. To determine if a
!                     preissmann slot is defined the branch number for
!                     this gridpoint should be found. This function is
!                     performed by this routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  4 ibr               O  Branch number
!  3 igp               I  Gridpoint number
!  1 nbran             I  Number of branches.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: mofdbr.pf,v $
! Revision 1.4  1999/03/15  15:52:51  kuipe_j
! tabs removed
!
! Revision 1.3  1995/10/18  08:59:57  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:04:45  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:14  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/12/12  08:06:20  kuipe_j
! Wrong index in loop
!
! Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer   nbran, ibr, igp
   integer   branch(4,nbran)
!
!     Local variables
!
   logical   lfound
   integer   i
!
   do 100 i = 1, nbran
      lfound = igp .ge. branch(3,i) .and. igp .le. branch(4,i)
      if (lfound) then
         ibr = i
         goto 200
      endif
100 continue

200 continue

end
