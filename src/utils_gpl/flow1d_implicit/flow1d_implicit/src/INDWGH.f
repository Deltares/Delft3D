      subroutine INDWGH (ngrid , i     ,
     +                   maxlev, nlev  , hlev  ,
     +                   hact  , inter , wght  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             INDWGH (Compute INDex and WeiGHt factor)
c
c Module description: Computing of the index in array hlev and a weight
c                     factor.
c                     Explanation:
c                     hlev(i,j) : j-th water level in table for grid-
c                     point i.
c                     wtab(i,j) : j-th width/area in table for gridpoint
c                     i
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  5 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  2 i                 I  Gridpoint index in branch.
c  7 inter             O  Level in table hlev(i,j).
c  3 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  1 ngrid             I  Number of grid points in network.
c  4 nlev(ngrid)       I  Number of h-levels for every cross section.
c                         (For a circle cross section   : 1 ;
c                          For a sedredge cross section : 2 )
c  8 wght              O  Weight factor.
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
c $Log: indwgh.pf,v $
c Revision 1.3  1999/03/15  15:51:19  kuipe_j
c tabs removed
c
c Revision 1.2  1995/05/30  07:02:25  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:29  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:32:01  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
c Initial version
c
c
c***********************************************************************
c
C     declaration of Parameters:
c
      integer ngrid, i, maxlev, inter
      integer nlev(ngrid)
	double precision hlev(ngrid,maxlev), wght, hact
c
C     declaration of local variables:
c
      integer j
      double precision h1, h2
      logical inner
c
      if ( hact .gt. hlev(i,nlev(i)) ) then
         inter = -1
      else
c
c        1. Find interval inter (j = current interval)
c
         j = 0
         inner = .false.
   10    continue
         if (.not. inner .and. j .lt. (nlev(i)-1) ) then

               j     = j + 1

               h1    = hlev(i,j)
               h2    = hlev(i,j+1)
               inner = hact.ge.h1 .and. hact.le.h2

               go to 10
         endif
c
         if (inner) then
            inter = j
c
c           2. Compute weight factor
c
c           >> Definition weight factor :
c              w(h) := (1-wght) * w(h1) + wght * w(h2)
c
            wght = (hact-h1) / (h2-h1)
            
         else

c           write(*,*) 'Internal failure: h<bodem', i, j,h, hlev(i,1)

            inter = -1
         endif
 
      endif
      end
