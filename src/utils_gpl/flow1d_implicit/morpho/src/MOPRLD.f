      subroutine moprld ( igp,
     +                    hws,
     +                    k,
     +                    ngrid,
     +                    maxlev,
     +                    hlev,
     +                    deltaz
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOPRLD (MORPHology erosion/sedimentation PRoportional to Local Depth)
c
c Module description: Calculate new cross section
c
c                     This routine calculates new cross section dimensi-
c                     ons. The algoritm used will distribute the erosion
c                     and and sedimentation proportional to the local
c                     water depth in the cross section. The delta bed
c                     level is used to adapt the levels in the cross
c                     section below the found highest bed level.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 deltaz            I  Calculated change in cross section level
c  6 hlev(ngrid,       IO (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  2 hws               I  Water level for sediment transporting width
c  1 igp               I  Gridpoint number
c  3 k                 I  Cross section level which is morphodynamic
c                         active
c  5 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  4 ngrid             I  Number of grid points in network.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moprld.pf,v $
c Revision 1.4  1999/03/15  15:53:02  kuipe_j
c tabs removed
c
c Revision 1.3  1996/05/31  12:57:03  kuipe_j
c keep mimimum level for proportional distr.
c
c Revision 1.2  1995/05/30  07:04:56  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:23  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:54  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:09  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c       Parameters
c
      integer   igp,
     +          k,
     +          maxlev,
     +          ngrid
c
      double precision hws,
     +                 hlev (ngrid,maxlev),
     +                 deltaz

c
c     Variables
c
      double precision denum, alpha
c
      integer   i

c
c     Denumerator fixed in loop
c
      denum = dmax1(hws - hlev(igp,1) , 1d-6 )
c
c     Calculate adapted bed levels
c
      do 100 i = 1, k
         alpha = (hws - hlev(igp,i)) / denum
         hlev(igp,i) = hlev(igp,i) - (alpha * deltaz)
 100  continue

      return
      end
