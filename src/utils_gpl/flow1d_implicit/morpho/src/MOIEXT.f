      subroutine MOIEXT ( igpbou ,dir    ,isec   ,ngrid  ,
     +                    dtm    ,x      ,celer  ,sedtr  ,iextra )


c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         J. Kuipers
c
c Module:             MOGREP (MOrphology Integeral EXTra)
c
c Module description: Calculate extra integral on point n-1/2
c                     for outflowing part of branch.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 31 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
c               1|2       - Normal branches:
c                         (i,1) = Celerity in gridpoint i in main secti-
c                                 on.
c                         - Sedredge branches:
c                         (i,1) = Celerity in gridpoint i,
c                                 left channel.
c                         (i,2) = Celerity in gridpoint i,
c                                 right channel.
c 29 dtm               I  Morphology time step.
c  1 igpbou            I  Calculated integral value on boundary
c  3 isec              I  Section number (1 or 2)
c  4 ngrid             I  Number of grid points in network.
c 32 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c 27 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moiext.pf,v $
c Revision 1.2  1998/06/18  13:26:42  kuipe_j
c sign change
c
c Revision 1.1  1998/06/12  07:59:00  kuipe_j
c Estuary special integrated
c
c
c***********************************************************************
c
c     Parameters
c
      integer    igpbou ,dir, isec ,ngrid 

      real       iextra

      real       x      (ngrid),
     +           celer  (ngrid,*),
     +           sedtr  (ngrid,*)

      double     precision  dtm
c
c     Local variables
c
      real       sigma, dtms, sboun
c
      dtms   = sngl(dtm)
c
      sigma  = celer(igpbou,isec)*dtms/
     +        (x(igpbou+dir)-x(igpbou))*real(dir)
      sboun  = sedtr(igpbou,isec)
      iextra = (sboun + sigma * (sboun - sedtr(igpbou+dir,isec))*
     +         0.5 * real(dir)) * dtms
      return
      end
