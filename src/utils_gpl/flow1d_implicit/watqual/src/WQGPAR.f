      subroutine wqgpar ( ngrid ,aft   ,afs   ,igp   ,isecwq  ,carea )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQGPAR (Water Quality GridPoint ARea)
c
c Module description: This routine calculates the area in a particular
c                     gridpoint/section.
c
c                     The total area for the requested gridpoint/section
c                     is extracted. When the whole channel is indicated,
c                     including parallel sections, all areas are summed
c                     to one area.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 aft(ngrid)        I  Flow ot total area at every grid point
c  3 afs(ngrid,2)      I  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c  6 carea             O  Calculated area.
c  4 igp               I  Gridpoint.
c  5 isecwq            I  Section:
c                         0 = Channel including parallel sections
c                         1 = Main channel
c                         2 = Sub section 1
c                         3 = Sub section 2
c  1 ngrid             I  Number of grid points in network.
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
c $Log: wqgpar.pf,v $
c Revision 1.3  1999/03/12  12:42:22  kuipe_j
c parallel segments added
c
c Revision 1.2  1995/05/30  07:08:34  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:56  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:28  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c       Parameters
c
      integer  igp,
     +         isecwq,
     +         ngrid

      real     carea

      real     aft  (ngrid),
     +         afs (ngrid,2)

      if     (isecwq .eq. 0) then
         carea = aft(igp)
      elseif (isecwq .eq. 1) then
         carea = afs(igp,1)
      elseif (isecwq .eq. 2) then
         carea = afs(igp,2)
      else
         carea = aft(igp) - afs(igp,1) - afs(igp,2)
      endif
      carea = max(carea,0.0)

      return
      end
