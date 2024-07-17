      subroutine moprdz ( igp,
     +                    hws,
     +                    k,
     +                    ngrid,
     +                    maxlev,
     +                    nlev,
     +                    hlev,
     +                    wft,
     +                    deltaa,
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
c Module:             MOPRDZ (MORPHology PRoportional Delta Z calculation)
c
c Module description: Calculate delta z if morphodynamic option is pro-
c                     portional to local depth.
c
c                     With the level found (MOMLEV) a delta bed level is
c                     calculated. The delta bed level is used to adapt
c                     the levels in the cross section below the found
c                     highest bed level.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 deltaa            I  Calculated change in area
c 10 deltaz            O  Calculated change in cross section level
c  7 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
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
c  6 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c  8 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
c                                 point i.
c                         - For a circle cross section:
c                         (i,1) = Radius of the circle.
c                         - For a sedredge cross section:
c                         (i,1) = Width of main section (i.e. left chan-
c                                 nel).
c                         (i,2) = Width of sub section 1 (i.e. right
c                                 channel).
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: moprdz.pf,v $
c Revision 1.4  1999/03/15  15:53:00  kuipe_j
c tabs removed
c
c Revision 1.3  1996/05/31  12:57:01  kuipe_j
c keep mimimum level for proportional distr.
c
c Revision 1.2  1995/05/30  07:04:54  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:22  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:53  kuipe_j
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
     +          ngrid,
     +          nlev (ngrid)

      real      wft  (ngrid,maxlev)

      double precision hlev (ngrid,maxlev),
     +                 hws, deltaa, deltaz
c
c       Variables
c
      integer   i
      double precision alpha1,
     +                 alpha2,
     +                 sum1,
     +                 sum2,
     +                 denum 

c
c     Initialise counters
c
      alpha1 = 1d0
      sum1   = 0d0
      sum2   = 0d0

c
c     Denumerator fixed in loop
c
      denum = dmax1(hws - hlev(igp,1) , 1.0d-6 )

c
c     Calculate sum1 and sum2
c
      do 100 i = 1, k-1
         alpha2 = (hws - hlev(igp,i+1)) / denum
         sum1 = sum1 + (alpha1 * wft(igp,i+1))
         sum2 = sum2 + (alpha2 * wft(igp,i))

         alpha1 = alpha2
 100  continue

c
c     Calculate last contribution sum1
c
      if (k .eq. nlev(igp)) then
         sum1 = sum1 + (alpha1 * wft(igp,k))
      else
         sum1 = sum1 + (alpha1 * wft(igp,k+1))
      endif

c
c     Calculate delta z
c
      deltaz = (2. * deltaa) / (wft(igp,1) + sum1 - sum2)

      return
      end
