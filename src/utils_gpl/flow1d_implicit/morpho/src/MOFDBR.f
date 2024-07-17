      subroutine mofdbr ( nbran  ,branch ,igp    ,ibr    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOFDBR (MOrphology FinD BRanch number for a gridpoint)
c
c Module description: Find branch number for a particular grid point
c
c                     For history information grid numbers are defined
c                     where output should be given. To determine if a
c                     preissmann slot is defined the branch number for
c                     this gridpoint should be found. This function is
c                     performed by this routine.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  4 ibr               O  Branch number
c  3 igp               I  Gridpoint number
c  1 nbran             I  Number of branches.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: mofdbr.pf,v $
c Revision 1.4  1999/03/15  15:52:51  kuipe_j
c tabs removed
c
c Revision 1.3  1995/10/18  08:59:57  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:04:45  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:14  hoeks_a
c Initial check-in
c
c Revision 1.2  1994/12/12  08:06:20  kuipe_j
c Wrong index in loop
c
c Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer   nbran, ibr, igp
      integer   branch(4,nbran)
c
c     Local variables
c
      logical   lfound
      integer   i
c
      do 100 i = 1, nbran
         lfound = igp .ge. branch(3,i) .and. igp .le. branch(4,i)
         if (lfound) then
            ibr = i
            goto 200
         endif
 100  continue

 200  continue

      end
