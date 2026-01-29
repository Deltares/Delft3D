subroutine MODAIP ( igp    ,isec   ,ngrid  ,&
&alphac ,dtm    ,alphad ,&
&celer  ,sedtr  ,slat   ,x     ,&
&intiph ,deltaa&
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
! Module:             MODAIP (MOrphology Delta Area for Internal Points)
!
! Module description: This subroutine calculates the change in area for
!                     an internal point. If a branch has grid points
!                     numbered from 1..n internal grid points will be
!                     located between 2 1/2, .., n-3/2.
!                     The integral value I i+1/2 will be returned to
!                     the calling routine.
!                     This integral value will be used in the
!                     next call as Ii-1/2. Notice that on the first
!                     internal point the lateral sediment from i-1/2
!                     will be assigned completely.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 alphac            P  -
!  6 alphad            P  -
!  7 celer             P  -
! 12 deltaa            O  Calculated change in area
!  5 dtm               I  Morphology time step.
!  1 igp               I  Gridpoint number
! 11 intiph            IO Calculated integral value on i + 1/2
!  2 isec              I  Section number (1 or 2)
!  3 ngrid             I  Number of grid points in network.
!  8 sedtr             P  -
!  9 slat(ngrid,*)     I  Actual lateral sediment transport in grid
!              1|2        point i+1/2 for:
!                         (i,1) = Main or Left channel.
!                         (i,2) = Right channel.
! 10 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! moitip  MOrphology InTegral on Intermediate grid Point
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: modaip.pf,v $
! Revision 1.7  1997/02/17  10:23:12  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.6  1996/05/28  13:30:06  kuipe_j
! Error message courant nr added
!
! Revision 1.5  1996/03/08  09:39:01  kuipe_j
! Headers + moptf temporarily = false
!
! Revision 1.4  1996/03/07  10:44:13  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.3  1995/10/18  08:59:54  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:04:38  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:07  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:52:27  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:32  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:06  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer  igp    ,isec   , ngrid
   real     intiph ,alphac , alphad
   real     celer (ngrid,*),&
   &sedtr (ngrid,*),&
   &slat  (ngrid,*),&
   &x     (ngrid)
   double  precision     dtm, deltaa
!
!     Local variables
!
   real     inthp,&
   &dx,&
   &ili,&
   &dtms

   dtms = sngl(dtm)
!
!     Calculate integral on i+1/2
!
   CALL MOITIP ( igp    ,isec   ,ngrid  ,&
   &x      ,dtm    ,alphac ,&
   &celer  ,sedtr  ,&
   &alphad ,inthp&
   &)
!
!     Calculate dx for lateral sediment and delta A calculation
!
   dx = x(igp+1) - x(igp-1)
!
   ili = slat(igp,isec) * dtms
!
!     Calculate delta A
!
   deltaa = ( inthp - intiph - ili ) / ( 0.5 * dx )
!
!     return value of I at i+1/2
!
   intiph = inthp
!
end
