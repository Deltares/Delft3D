      subroutine saforf (nbran ,ngrid ,ceps ,branch ,csa2  ,filc )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SAFORF (SAlt FORester Filter)
c
c Module description: Execute the filter on the calculated
c                     concentration field (was in older versions a 
c                     Forrester filter) Now negative concentrations are
c                     simply made equal to EPSILON.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  3 ceps              I  Minimum allowed concentration.
c  5 csa2(ngrid)       IO Salt concentration in every grid point at time
c                         t(n+1).
c  6 filc(ngrid)       O  1  = negative concentration is filterred away
c                         -1 = calculated concentration is positive
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
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
c $Log: saforf.pf,v $
c Revision 1.5  1995/10/18  09:00:22  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/09/22  10:03:18  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/08/23  14:29:40  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.2  1995/05/30  07:06:03  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:45  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:44:13  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer    nbran  ,ngrid
      integer    branch(4,nbran)
      real       ceps
      real       csa2(ngrid),filc(ngrid)
c
c     Declaration of local variables
c
      integer    i1    ,i2    ,ibr  ,igr
c
c     Filter in branches (only at inner points)
c
      do 40 ibr = 1,nbran
         i1     = branch(3,ibr)
         i2     = branch(4,ibr)
c
c        Determine filter flag in every grid point (also at
c        grid ends).
c
         do 10 igr = i1,i2
            if (csa2(igr) .lt. abs(ceps)) then
               csa2(igr) =    abs(ceps)
               filc(igr) =    1.0 
            else   
               filc(igr) =    -1.0 
            endif
   10    continue
c
   40 continue
c
      end
