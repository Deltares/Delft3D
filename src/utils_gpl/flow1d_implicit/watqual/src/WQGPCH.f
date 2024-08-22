      subroutine wqgpch ( ngrid ,c     ,igp   ,isecwq  ,chezy )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Water Quality Interface Module
c
c Programmer:         S.L. van der Woude
c
c Module:             WQGPCH (Water Quality GridPoint CHezy)
c
c Module description: This routine calculates the Chezy coefficient in a
c                     particular gridpoint.
c
c                     The requested Chezy coefficient is extracted. When
c                     the whole channel is indicated the Chezy coeffi-
c                     cient for the whole channel is returned, else the
c                     Chezy coefficient for the requested section.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 c(ngrid)          I  Actual Chezy coefficient for total channel in
c                         every grid point.
c  5 chezy             O  Calculated chezy coefficient.
c  3 igp               I  Gridpoint.
c  4 isecwq            I  Section:
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
c $Log: wqgpch.pf,v $
c Revision 1.3  1999/03/15  15:54:00  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:08:35  hoeks_a
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

      real     chezy

      real     c(ngrid,4)

c
c     Determine chezy coefficient
c
      if (isecwq .eq. 0) then
         chezy = c (igp,1)
      else
         chezy = c (igp,isecwq+1)
      endif

      return
      end
