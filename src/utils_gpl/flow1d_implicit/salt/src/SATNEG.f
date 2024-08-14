      subroutine satneg (ngrid ,ceps ,csa2 ,filter)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SATNEG (SAlt Test NEGative concentrations)
c
c Module description: Test for negative concentrations in network
c
c                     Test if all concentrations do have a value greater
c                     then cs,eps. If this is not the case negative
c                     concentrations occur.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 ceps              I  Minimum allowed concentration.
c  3 csa2(ngrid)       I  Salt concentration in every grid point at time
c                         t(n+1).
c  4 filter            O  = True if there are salt concentrations with a
c                         value lower than the minimum value (ceps).
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
c $Log: satneg.pf,v $
c Revision 1.2  1995/05/30  07:06:22  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:10:03  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    ngrid
      real       ceps
      real       csa2(ngrid)
      logical    filter
c
c     Declaration of local variables
c
      integer    igr
c
      filter  = .false.
      do 10 igr = 1,ngrid
         if (csa2(igr) .lt. ceps) then
            filter  = .true.
         endif
   10 continue
c
      end
