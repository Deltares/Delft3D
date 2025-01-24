subroutine FLA1MS(i1     ,i2     ,ngrid  ,h1     ,h      ,maxlev ,&
&hlev   ,wft    ,a1m    ,theta2 )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLA1MS (FLow A1M Sedredge)
!
! Module description: Compute first order momentum cross section A1m for
!                     each grid point in a sedredge branch. General de-
!                     scription
!
!                     For a sedredge cross section the first order mo-
!                     mentum will be calculated by the ZWENDL formulati-
!                     on: 1/2 d * Af.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 a1m               O  parameter a1m
!  4 h1(ngrid)         I  Water level in every grid point at time t(n).
!  5 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  7 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
!       maxlev)           - For a circle cross section:
!                         (i,1) = Reference level.
!                         - For a sedredge cross section:
!                         (i,1) = Bed level of main section (i.e. left
!                                 channel).
!                         (i,2) = Bed level of sub section 1 (i.e. right
!                                 channel).
!  1 i1                I  Index of first grid point in actual branch.
!  2 i2                I  Index of last grid point in actual branch.
!  6 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  3 ngrid             I  Number of grid points in network.
! 10 theta2            I  parameter for the time level t(n)+theta2*dt on
!                         which hydraulic parameters are to be evaluated
!  8 wft(ngrid,maxlev) I  (i,j) = flow width at h = hlev(i,j) for grid
!                                 point i.
!                         - For a circle cross section:
!                         (i,1) = Radius of the circle.
!                         - For a sedredge cross section:
!                         (i,1) = Width of main section (i.e. left chan-
!                                 nel).
!                         (i,2) = Width of sub section 1 (i.e. right
!                                 channel).
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: fla1ms.pf,v $
! Revision 1.5  1995/09/22  10:00:41  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:10:41  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.2  1993/11/26  15:30:21  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:45  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Declaration of Parameters:
!
   integer   i1, i2, ngrid, maxlev
   double precision h1(ngrid), h(ngrid)
   double precision hlev(ngrid,maxlev)
   real      wft(ngrid,maxlev), a1m(ngrid)
   real      theta2
!
!     Declaration of local variables:
!
   integer  i
   real      zbmain, zbsub1, wmain, wsub1
   real      zb1, zb2, w1, w2
   double precision hint
!
!     Do for each gridpoint in branch
!
   do 100 i = i1, i2

!       hint = ( h1(i) + h(i) ) / 2.
      hint = dble( theta2*h(i) + (1.-theta2)*h1(i) )

      zbmain = sngl( hlev(i,1) )
      wmain  = wft (i,1)
      zbsub1 = sngl( hlev(i,2) )
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
      a1m(i) = 0.5 * (hint-zb2) * (hint-zb2) * w2 +&
      &0.5 * (hint-zb1) * (hint-zb1) * w1
100 continue
!
end
