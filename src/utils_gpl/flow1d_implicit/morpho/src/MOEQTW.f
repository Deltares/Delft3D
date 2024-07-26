      subroutine moeqtw ( igp,
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
c Module:             MOEQTW (MORPHology erosion/sedimentation spread EQually over Transport Width)
c
c
c
c Module description: This routine calculates new cross section dimensi-
c                     ons. The algoritm used will spread out the erosion
c                     and sedimentation equally over the transport width
c                     of the cross section. The delta bed level is used
c                     to adapt the levels in the cross section below the
c                     found highest bed level.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 deltaz            I  Calculated change in cross section level
c  5 hlev(ngrid,       IO (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  1 igp               I  Gridpoint number
c  2 k                 I  Cross section level which is morphodynamic
c                         active
c  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  3 ngrid             I  Number of grid points in network.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moeqtw.pf,v $
c Revision 1.2  1995/05/30  07:04:44  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:12  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:39  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:07  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer   igp,
     +          k,
     +          maxlev,
     +          ngrid

      double precision hlev (ngrid,maxlev),
     +                 deltaz

c
c     Variables
c
      integer   i

      do 100 i = 1, k
         hlev(igp,i) = hlev(igp,i) - deltaz
 100  continue

      return
      end
