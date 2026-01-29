subroutine FLHAVG (ngrid, x, h1, h, lbt, let, i1, i2, havg,theta2)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLHAVG (FLow AVeraGed waterlevel H)
!
! Module description: Compute averaged water level over a trajectory.
!
!                     The possibility for a Q(h) table can be point
!                     lateral or on a trajectory. In case a trajectory
!                     has been specified an averaged water level or
!                     depth over the trajectory must be computed.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 h1(ngrid)         I  Water level in every grid point at time t(n).
!  4 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  9 havg              O  Averaged water level over a trajectory.
!  7 i1                I  Index of first grid point in actual branch.
!  8 i2                I  Index of last grid point in actual branch.
!  5 lbt               I  Begin point trajectory lateral discharge.
!  6 let               I  End point trajectory lateral discharge.
!  1 ngrid             I  Number of grid points in network.
! 10 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  2 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flhavg.pf,v $
! Revision 1.5  1995/09/22  10:01:23  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:10:54  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.1.1.1  1993/07/21  14:43:49  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer ngrid, i1, i2

   real    lbt, let, havg, theta2
   real    x(ngrid)

   double precision h1(ngrid), h(ngrid)

!
!     Declaration of local variables
!
   integer i
   real    x1, x2, xm, dx, f1, f2, f3
   real    hin, hin1, hin2

!     --- Check for situation
!		(Added on 2007-12-5, Niek Praagman, SEPRA BV)

   if ( i2==i1 ) then


!        --- Trajectory inside one cel:

!        --- First find coordinate for avarage waterlevel:

      xm = ( lbt + let ) / 2.

      x1 = x(i1  )
      x2 = x(i1+1)

      dx = x2 - x1

!        --- Use simple linear interpolation

      hin1 = theta2 * h(i1  ) + (1. - theta2) * h1(i1  )
      hin2 = theta2 * h(i1+1) + (1. - theta2) * h1(i1+1)

      havg = hin1 + ( xm - x1 ) * ( hin2 - hin1 ) / dx

   else

!        --- Total redesign 2007-12-10 (Niek Praagman, SEPRA)
!            Compute contribution to total sum and take avarage value
!            afterwards

!        --- Length first cel is:

      dx = x(i1+1) - x(i1)

!        --- Length covered by trajectory in first cel:

      x1 = x(i1+1) - lbt

      xm = ( x(i1+1) + lbt ) / 2.

!        --- Length not used in first cel:

      x2 = dx - x1

!        --- Find waterlevels in i1 and i1+1:

      hin1 = theta2 * h(i1  ) + (1. - theta2) * h1(i1  )
      hin2 = theta2 * h(i1+1) + (1. - theta2) * h1(i1+1)

!        --- Compute mean waterlevel i.e. in point xm:

      f1 = hin1 + ( xm - x(i1) ) * ( hin2 - hin1 ) / dx

!        --- Finally compute contribution of first cel for part with
!            length x1:

      f1 = f1 * x1
!
!        --- Next compute contributions to sum of all enclosed cells
!
      f2 = 0.

      do i = i1+1, i2-1

!           --- Length of cel:

         dx = x(i+1) - x(i)

!           --- Mean waterlevelvalue in this cel:

         hin = theta2 * (h(i) + h(i+1)) / 2. +&
         &(1. - theta2) *(h1(i) + h1(i+1)) / 2.

!           --- Contribution:

         f2 = f2 + hin * dx

      end do
!
!        --- Finally contribution of last cell to trajectory
!

!        --- Length cel is:

      dx = x(i2+1) - x(i2)

!        --- Length covered by trajectory in first cel:

      x1 = let - x(i2)

      xm = ( x(i2) + let ) / 2.

!        --- Length not used in first cel:

      x2 = dx - x1

!        --- Find waterlevels in i2 and i2+1:

      hin1 = theta2 * h(i2  ) + (1. - theta2) * h1(i2  )
      hin2 = theta2 * h(i2+1) + (1. - theta2) * h1(i2+1)

!        --- Compute mean waterlevel i.e. in point xm:

      f3 = hin1 + ( xm - x(i2) ) * ( hin2 - hin1 ) / dx

!        --- Finally compute contribution of first cel for part with
!            length x1:

      f3 = f3 * x1
!
!        compute avaraged value: sum divided by length trajectory:
!
      havg = (f1 + f2 + f3) / (let - lbt)

   endif
!
end
