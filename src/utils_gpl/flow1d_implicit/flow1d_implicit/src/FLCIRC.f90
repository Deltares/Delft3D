subroutine FLCIRC ( h, reflev, radius, juer, af, wf, o, ker)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLCIRC (FLow CIRCLe cross section)
!
! Module description: Calculate flow area, flow width and wetted perime-
!                     ter for a circle cross section.
!
!                     A circle cross section is represented by a bed
!                     level h0 and a radius r. To calculate the widths
!                     and areas for a given water level h goniometric
!                     functions have to be used.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 af(ngrid)         O  Flow area at every grid point at time t(n+1)
!  1 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  4 juer              P  -
!  8 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  7 o(ngrid)          O  Wetted perimeter for total cross section.
!  3 radius            I  Radius for circle cross section.
!  2 reflev            I  Reference level.
!  6 wf(ngrid)         O  Actual flow width at every grid point.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flcirc.pf,v $
! Revision 1.4  1999/03/15  15:49:40  kuipe_j
! tabs removed
!
! Revision 1.3  1995/05/30  09:54:53  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:50  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:36  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:30:43  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   real    radius, af, wf, o
   integer juer, ker
   double precision h, reflev
!
!     Declaration of local variables:
!
   real    alpha, phi, x, y
!
   double precision pi
!
!     Include sobek error code file
!
   include '../include/errcod.i'
!
!     Function: Calculate Af, Wf and O for circle cross section
!
   pi = atan (1.D0) * 4.D0
!
!     Check if water level > reference + radius
!
   if ( h .ge. reflev + radius ) then

      af = real(( 0.5 * pi * radius * radius ) +&
      &( 2. * radius ) * ( h - reflev - radius ), kind=kind(af))

      wf = 2. * radius

      o = real(( pi * radius ) + ( 2. * ( h - reflev - radius )))

   else if ( h .lt. reflev ) then
      ker = fatal
      call sre_error (juer,'FLCIRC', eflhci, ker)
   else
!
!        Calculate X en Y
!
      x = radius + reflev - h
      y = sqrt ( radius * radius - x * x )
!
!        Calculate corner phi
!
      phi = atan ( y/x )
!
!        alpha equals twice phi
!
      alpha = 2. * phi

      af = 0.5 * radius * radius  * ( alpha - sin (alpha) )

      wf = 2. * y

      o  = radius * ( alpha )

   endif
end
