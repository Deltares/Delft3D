      subroutine SOSDIR( qpack , ngrid , flwdir )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOSDIR (SObek Sediment flow DIRection)
c
c Module description: This routine fills the array flwdir
c
c                     Depending on the discharge at each gridpoint
c                     the integer value of flwdir(igp) is determined
c                     as follows:
c                     Positive discharge -> flwdir(igp) = 1
c                     Zero discharge     -> flwdir(igp) = 0
c                     Negative discharge -> flwdir(igp) = -1
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 flwdir(ngrid)     O  Indicator for flow direction at each grid
c                         point      1 = positive flow
c                                    0 = zero flow
c                                   -1 = negative flow
c  2 ngrid             I  Number of grid points in network.
c  1 qpack(ngrid,3)    I  (i,1) = q1(i) (t=n)
c                         (i,2) = q(i)  (*)
c                         (i,3) = q2(i) (t=n+1)
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c epsequ  EQUal test with interval EPSilon
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sosdir.pf,v $
c Revision 1.2  1996/03/08  09:40:52  kuipe_j
c Headers
c
c
c***********************************************************************
c
c
c     Parameters
c
      integer ngrid, flwdir(ngrid)

      double precision qpack(ngrid,3)
	double precision help
c
c     Local variables
c
      integer ip

      logical, external    :: dpsequ
c
c     Loop over gridpoints and determine flow direction depending
c     on the discharge at each gridpoint
c
      do 100 ip = 1,ngrid
	   help = qpack(ip,3)
         if (DPSEQU(help,0.0d0,1.d-10)) then
            flwdir(ip) = 0
         elseif (help .gt. 0.) then
            flwdir(ip) = 1
         else
            flwdir(ip) = -1
         endif
  100 continue

      end
