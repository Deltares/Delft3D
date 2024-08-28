      subroutine FLA1MS(i1     ,i2     ,ngrid  ,h1     ,h      ,maxlev ,
     +                  hlev   ,wft    ,a1m    ,theta2 )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLA1MS (FLow A1M Sedredge)
c
c Module description: Compute first order momentum cross section A1m for
c                     each grid point in a sedredge branch. General de-
c                     scription
c
c                     For a sedredge cross section the first order mo-
c                     mentum will be calculated by the ZWENDL formulati-
c                     on: 1/2 d * Af.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 a1m               O  parameter a1m
c  4 h1(ngrid)         I  Water level in every grid point at time t(n).
c  5 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  7 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  1 i1                I  Index of first grid point in actual branch.
c  2 i2                I  Index of last grid point in actual branch.
c  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c  3 ngrid             I  Number of grid points in network.
c 10 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
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
c $Log: fla1ms.pf,v $
c Revision 1.5  1995/09/22  10:00:41  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:10:41  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.2  1993/11/26  15:30:21  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:45  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Declaration of Parameters:
c
      integer   i1, i2, ngrid, maxlev
      double precision h1(ngrid), h(ngrid)
      double precision hlev(ngrid,maxlev)
	real      wft(ngrid,maxlev), a1m(ngrid)
      real      theta2
c
c     Declaration of local variables:
c
      integer  i
      real      zbmain, zbsub1, wmain, wsub1
      real      zb1, zb2, w1, w2
	double precision hint
c
c     Do for each gridpoint in branch
c
      do 100 i = i1, i2

c       hint = ( h1(i) + h(i) ) / 2.
        hint = dble( theta2*h(i) + (1.-theta2)*h1(i) )

        zbmain = real( hlev(i,1), kind=kind(zbmain) )
        wmain  = wft (i,1)
        zbsub1 = real( hlev(i,2), kind=kind(zbsub1) )
        wsub1  = wft (i,2)

        if ( zbmain .lt. zbsub1 ) then
           zb1 = zbmain
           w1  = wmain
           zb2 = zbsub1
           w2  = wsub1
        else
           zb1 = zbsub1
           w1  = wsub1
           zb2 = zbmain
           w2  = wmain
        endif
         a1m(i) = 0.5 * (hint-zb2) * (hint-zb2) * w2 +
     +            0.5 * (hint-zb1) * (hint-zb1) * w1
 100  continue
c
      end
