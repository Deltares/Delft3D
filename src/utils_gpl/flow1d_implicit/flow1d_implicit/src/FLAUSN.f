      subroutine flausn (nbran  ,ngrid  ,branch  ,af  ,hq2, alfa)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLAUSM (FLow AUtostart SMoothing)
c
c Module description: Smoothing of water levels or discharges
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  4 af(ngrid)         IO (to be) smoothed array
c  5 hq2(ngrid)        IO scratch array
c  1 nbran             I  Number of branches.
c  2 ngrid             I  Number of grid points in network.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flausm.pf,v $
c Revision 1.2  1995/10/18  08:59:14  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.1  1995/09/22  10:00:52  kuipe_j
c variable dimensions, new headers
c
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      integer    nbran ,ngrid 
      integer    branch(4,nbran)
      real       alfa, af(ngrid)
      double precision hq2(ngrid)
c
c     Declaration of local variables
c
      integer    i1  ,i2  ,ibr   ,igr
      real       alfa1

      alfa1 = (1.-alfa)*.5
      do 30 ibr = 1, nbran
         i1 = branch(3,ibr)
         i2 = branch(4,ibr)
         do 10 igr = i1+1, i2-1
            hq2(igr) = dble( alfa1*af(igr-1) + alfa*af(igr)
     +                     + alfa1*af(igr+1) )
 10      continue
         do 20 igr = i1+1, i2-1
            af(igr) = sngl(hq2(igr))
 20      continue
 30   continue

      end

