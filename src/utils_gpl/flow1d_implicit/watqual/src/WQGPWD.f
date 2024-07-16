      subroutine wqgpwd ( ngrid ,wf    ,wfs   ,igp   ,isecwq  ,cwidth )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQGPWD (Water Quality GridPoint WiDth)
c
c Module description: This routine calculates the width in a particular
c                     gridpoint/section.
c
c                     The total width for the requested gridpoint/secti-
c                     on is extracted. When the whole channel is indica-
c                     ted, including parallel sections, all widths are
c                     summed to one width.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 cwidth            O  Calculated widths.
c  4 igp               I  Gridpoint.
c  5 isecwq            I  Section:
c                         0 = Channel including parallel sections
c                         1 = Main channel
c                         2 = Sub section 1
c                         3 = Sub section 2
c  1 ngrid             I  Number of grid points in network.
c  2 wf(ngrid)         I  Actual flow width at every grid point.
c  3 wfs(ngrid,2)      I  Actual flow width per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
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
c $Log: wqgpwd.pf,v $
c Revision 1.3  1999/03/15  15:54:02  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:08:37  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:58  hoeks_a
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

      real     cwidth

      real     wf  (ngrid),
     +         wfs (ngrid,2)

      if (isecwq .eq. 0) then
         cwidth = wf(igp)
      elseif (isecwq .eq. 1) then
         cwidth = wfs(igp,1)
      elseif (isecwq .eq. 2) then
         cwidth = wfs(igp,2)
      else
         cwidth = wf(igp) - wfs(igp,1) - wfs(igp,2)
      endif

      return
      end
