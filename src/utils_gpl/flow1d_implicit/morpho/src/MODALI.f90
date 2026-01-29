subroutine MODALI ( igpm1  ,igp    ,igpp1  ,&
&isec   , ngrid ,&
&dtm    ,&
&intiph ,intnmh ,&
&slat   ,x      ,&
&deltaa&
&)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MODALI (MOrphology Delta Area for Last Internal point)
!
! Module description: This subroutine calculates the change in area for
!                     the last internal point. This point uses the cal-
!                     culated integral values Ii+1/2 (last value of
!                     MODAIP) and In-1/2 (value from MODAEP or MODAST).
!                     This is shown below:
!
!                     |         |         |
!                     -----x---------x-----
!                     |         |         |
!                        i+1/2 n-1  n-1/2 n
!                         /     |     \
!                        /      |      \
!                     MODAIP    MODALI   MODAEP
!
!                     The lateral sediment on point n-1/2 will be assig-
!                     ned to the point n-1.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 deltaa            O  Calculated change in area
!  6 dtm               I  Morphology time step.
!  2 igp               I  Gridpoint number
!  1 igpm1             I  Gridpoint number - 1
!  3 igpp1             I  Gridpoint number + 1
!  7 intiph            I  Calculated integral value on i + 1/2
!  8 intnmh            I  Calculated integral value on n - 1/2
!  4 isec              I  Section number (1 or 2)
!  5 ngrid             I  Number of grid points in network.
!  9 slat(ngrid,*)     I  Actual lateral sediment transport in grid
!              1|2        point i+1/2 for:
!                         (i,1) = Main or Left channel.
!                         (i,2) = Right channel.
! 10 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: modali.pf,v $
! Revision 1.5  1997/02/17  10:23:13  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.4  1996/03/08  09:39:02  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.3  1995/10/18  08:59:55  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:04:39  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:08  hoeks_a
! Initial check-in
!
! Revision 1.2  1994/11/28  08:52:29  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.1.1.1  1993/07/21  14:44:06  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer  igpm1  ,igp    ,igpp1  ,isec   , ngrid
   real     intiph ,intnmh
   real     slat (ngrid,*),&
   &x    (ngrid)
   double  precision  dtm, deltaa
!
!     Local variables
!
   real     dx,&
   &ili

!
!     Calculate dx
!
   dx = x(igpp1) - x(igpm1)
!
!     Calculate lateral sediment integral
!
   ili = slat(igp,isec) * sngl(dtm)
!
!     Calculate delta A
!
   deltaa = ( intnmh - intiph - ili ) / ( 0.5 * dx )

   return
end
