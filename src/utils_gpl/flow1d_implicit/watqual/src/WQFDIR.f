      subroutine wqfdir ( dir, qcalc, qdir )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQFDIR (Water Quality Flow DIRection)
c
c Module description: Determine flow direction
c
c                     This routine determines if a flow is entering or
c                     leaving a node.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 dir               I  Direction of gridpoint (begin or end of
c                         branch)
c                         1 = begin of branch
c                         2 = end of branch
c  2 qcalc             I  Calculated flow value.
c  3 qdir              O  Calculated direction of flow:
c                         0 = direction could not be determined.
c                         1 = entering the node.
c                         2 = leaving the node.
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: wqfdir.pf,v $
c Revision 1.3  1999/03/15  15:53:53  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:08:27  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:48  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:27  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c       Parameters
c
      integer  dir,
     +         qdir

      real     qcalc

c
c       Determine if flow is entering or leaving
c
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
