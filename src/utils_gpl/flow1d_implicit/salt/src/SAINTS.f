      subroutine saints (ngrid  ,csa1  ,csa2   ,csd1  ,csd2)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAINTS (SAlt INItialise)
c
c Module description: SAlt Initialise Next Time Step
c
c                     The resulting concentrations (Cs) and diffusion
c                     (C's) of time level n+1 will be copied to time
c                     level n.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 csa1(ngrid)       O  Salt concentration in every grid point at time
c                         t(n).
c  3 csa2(ngrid)       I  Salt concentration in every grid point at time
c                         t(n+1).
c  4 csd1(ngrid)       O  Diffusion (c s) in every grid point at time
c                         t(n).
c  5 csd2(ngrid)       I  Diffusion (c s) in every grid point at time
c                         t(n+1).
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
c $Log: saints.pf,v $
c Revision 1.3  1995/10/18  09:00:25  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.2  1995/05/30  07:06:07  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:49  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:14  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    ngrid
      real       csa1   (ngrid) ,csa2  (ngrid) ,csd1  (ngrid)  ,
     &           csd2   (ngrid)
c
c     Declaration of local variables
c
      integer    igr
c
      do 10 igr = 1,ngrid
         csa1(igr) =csa2(igr)
         csd1(igr) =csd2(igr)
   10 continue
c
      end
