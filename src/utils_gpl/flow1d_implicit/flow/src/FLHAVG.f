      subroutine FLHAVG (ngrid, x, h1, h, lbt, let, i1, i2, havg,theta2)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLHAVG (FLow AVeraGed waterlevel H)
c
c Module description: Compute averaged water level over a trajectory.
c
c                     The possibility for a Q(h) table can be point
c                     lateral or on a trajectory. In case a trajectory
c                     has been specified an averaged water level or
c                     depth over the trajectory must be computed.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 h1(ngrid)         I  Water level in every grid point at time t(n).
c  4 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  9 havg              O  Averaged water level over a trajectory.
c  7 i1                I  Index of first grid point in actual branch.
c  8 i2                I  Index of last grid point in actual branch.
c  5 lbt               I  Begin point trajectory lateral discharge.
c  6 let               I  End point trajectory lateral discharge.
c  1 ngrid             I  Number of grid points in network.
c 10 theta2            I  parameter for the time level t(n)+theta2*dt on
c                         which hydraulic parameters are to be evaluated
c  2 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flhavg.pf,v $
c Revision 1.5  1995/09/22  10:01:23  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:10:54  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer ngrid, i1, i2

      real    lbt, let, havg, theta2
      real    x(ngrid)

      double precision h1(ngrid), h(ngrid)
      
c
c     Declaration of local variables
c
      integer i
      real    x1, x2, xm, dx, f1, f2, f3
      real    hin, hin1, hin2

c     --- Check for situation
c		(Added on 2007-12-5, Niek Praagman, SEPRA BV)

	if ( i2==i1 ) then


c        --- Trajectory inside one cel:

c        --- First find coordinate for avarage waterlevel:         

         xm = ( lbt + let ) / 2. 

         x1 = x(i1  )
         x2 = x(i1+1)

	   dx = x2 - x1

c        --- Use simple linear interpolation
         
	   hin1 = theta2 * h(i1  ) + (1. - theta2) * h1(i1  )
         hin2 = theta2 * h(i1+1) + (1. - theta2) * h1(i1+1)

         havg = hin1 + ( xm - x1 ) * ( hin2 - hin1 ) / dx

	else

c        --- Total redesign 2007-12-10 (Niek Praagman, SEPRA)
c            Compute contribution to total sum and take avarage value
c            afterwards

c        --- Length first cel is:

         dx = x(i1+1) - x(i1)

c        --- Length covered by trajectory in first cel:

         x1 = x(i1+1) - lbt

	   xm = ( x(i1+1) + lbt ) / 2.

c        --- Length not used in first cel:

         x2 = dx - x1

c        --- Find waterlevels in i1 and i1+1:

         hin1 = theta2 * h(i1  ) + (1. - theta2) * h1(i1  )
         hin2 = theta2 * h(i1+1) + (1. - theta2) * h1(i1+1)

c        --- Compute mean waterlevel i.e. in point xm:

	   f1 = hin1 + ( xm - x(i1) ) * ( hin2 - hin1 ) / dx 

c        --- Finally compute contribution of first cel for part with
c            length x1:

         f1 = f1 * x1
c
c        --- Next compute contributions to sum of all enclosed cells
c
         f2 = 0.

         do i = i1+1, i2-1

c           --- Length of cel:

            dx = x(i+1) - x(i)

c           --- Mean waterlevelvalue in this cel:

            hin = theta2 * (h(i) + h(i+1)) / 2. +
     +           (1. - theta2) *(h1(i) + h1(i+1)) / 2.

c           --- Contribution:

            f2 = f2 + hin * dx

         end do
c
c        --- Finally contribution of last cell to trajectory
c

c        --- Length cel is:

         dx = x(i2+1) - x(i2)

c        --- Length covered by trajectory in first cel:

         x1 = let - x(i2)

	   xm = ( x(i2) + let ) / 2.

c        --- Length not used in first cel:

         x2 = dx - x1

c        --- Find waterlevels in i2 and i2+1:

         hin1 = theta2 * h(i2  ) + (1. - theta2) * h1(i2  )
         hin2 = theta2 * h(i2+1) + (1. - theta2) * h1(i2+1)

c        --- Compute mean waterlevel i.e. in point xm:

	   f3 = hin1 + ( xm - x(i2) ) * ( hin2 - hin1 ) / dx 

c        --- Finally compute contribution of first cel for part with
c            length x1:

         f3 = f3 * x1
c
c        compute avaraged value: sum divided by length trajectory:
c
         havg = (f1 + f2 + f3) / (let - lbt)

	endif
c
      end
