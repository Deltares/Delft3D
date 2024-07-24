      subroutine moeqdz ( igp,
     +                    k,
     +                    ngrid,
     +                    maxlev,
     +                    nlev,
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
c Module:             MOEQDZ (MORPHology EQually over Transport Width Delta Z calculation)
c
c Module description: Calculate delta z if morphodynamic option is equ-
c                     ally over transport width.
c
c                     With the level found (MOMLEV) a delta bed level is
c                     calculated. The delta bed level is used to adapt
c                     the levels in the cross section below the found
c                     highest bed level.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  7 deltaa            I  Calculated change in area
c  8 deltaz            O  Calculated change in cross section level
c  1 igp               I  Gridpoint number
c  2 k                 I  Cross section level which is morphodynamic
c                         active
c  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  3 ngrid             I  Number of grid points in network.
c  5 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c  6 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
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
c $Log: moeqdz.pf,v $
c Revision 1.2  1995/05/30  07:04:43  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:12  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:38  kuipe_j
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
     +          ngrid,
     +          nlev (ngrid)

      real      wft  (ngrid,maxlev)
      
	double precision deltaa, deltaz


c
c     Determine delta z
c
      if (k .eq. nlev(igp)) then
         deltaz = deltaa / wft(igp,k)
      else
         deltaz = (2. * deltaa) / (wft(igp,k) + wft(igp,k+1))
      endif

      return
      end
