subroutine SOSDIR( qpack , ngrid , flwdir )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOSDIR (SObek Sediment flow DIRection)
!
! Module description: This routine fills the array flwdir
!
!                     Depending on the discharge at each gridpoint
!                     the integer value of flwdir(igp) is determined
!                     as follows:
!                     Positive discharge -> flwdir(igp) = 1
!                     Zero discharge     -> flwdir(igp) = 0
!                     Negative discharge -> flwdir(igp) = -1
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 flwdir(ngrid)     O  Indicator for flow direction at each grid
!                         point      1 = positive flow
!                                    0 = zero flow
!                                   -1 = negative flow
!  2 ngrid             I  Number of grid points in network.
!  1 qpack(ngrid,3)    I  (i,1) = q1(i) (t=n)
!                         (i,2) = q(i)  (*)
!                         (i,3) = q2(i) (t=n+1)
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! epsequ  EQUal test with interval EPSilon
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sosdir.pf,v $
! Revision 1.2  1996/03/08  09:40:52  kuipe_j
! Headers
!
!
!***********************************************************************
!
!
!     Parameters
!
   integer ngrid, flwdir(ngrid)

   double precision qpack(ngrid,3)
   double precision help
!
!     Local variables
!
   integer ip

   logical, external    :: dpsequ
!
!     Loop over gridpoints and determine flow direction depending
!     on the discharge at each gridpoint
!
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
