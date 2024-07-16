      subroutine gmabnd ( igp,   k,     ngrid, maxlev, nlev,  wft,
     +                    deltaa,deltaz)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         J.Kuipers
c
c Module:             GMABND (Graded Morphology Area for z-BouND) 
c
c Module description: Calculate delta A if morphodynamic boundary
c                     condition is z=f(t). Deltaz-Z  is given.
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
c $Log: gmabnd.F,v $
c Revision 1.2  1996/06/07  11:55:04  kuipe_j
c multi  +  fixed layer
c
c Revision 1.1  1996/01/08  13:29:27  kuipe_j
c Multi layer option for under layer added
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
      double precision  deltaa, deltaz


c
c     Determine delta a
c
      if (k .eq. nlev(igp)) then
         deltaa = deltaz * wft(igp,k)
      else
         deltaa = deltaz * (wft(igp,k)+wft(igp,k+1)) * .5
      endif
      end
