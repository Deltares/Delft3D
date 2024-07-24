      subroutine FLCIRC ( h, reflev, radius, juer, af, wf, o, ker)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLCIRC (FLow CIRCLe cross section)
c
c Module description: Calculate flow area, flow width and wetted perime-
c                     ter for a circle cross section.
c
c                     A circle cross section is represented by a bed
c                     level h0 and a radius r. To calculate the widths
c                     and areas for a given water level h goniometric
c                     functions have to be used.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  5 af(ngrid)         O  Flow area at every grid point at time t(n+1)
c  1 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  4 juer              P  -
c  8 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  7 o(ngrid)          O  Wetted perimeter for total cross section.
c  3 radius            I  Radius for circle cross section.
c  2 reflev            I  Reference level.
c  6 wf(ngrid)         O  Actual flow width at every grid point.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flcirc.pf,v $
c Revision 1.4  1999/03/15  15:49:40  kuipe_j
c tabs removed
c
c Revision 1.3  1995/05/30  09:54:53  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:50  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:36  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:30:43  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      real    radius, af, wf, o
      integer juer, ker
      double precision h, reflev
c
c     Declaration of local variables:
c
      real    alpha, phi, x, y
c
      double precision pi
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     Function: Calculate Af, Wf and O for circle cross section
c
      pi = atan (1.D0) * 4.D0
c
c     Check if water level > reference + radius
c
      if ( h .ge. reflev + radius ) then

         af = sngl(( 0.5 * pi * radius * radius ) +
     +        ( 2. * radius ) * ( h - reflev - radius ))

         wf = 2. * radius

         o  = sngl(( pi * radius ) + ( 2. * ( h - reflev - radius )))

      else if ( h .lt. reflev ) then
         ker = fatal
         call ERROR (juer,'FLCIRC', eflhci, ker)
      else
c
c        Calculate X en Y
c
         x = radius + reflev - h
         y = sqrt ( radius * radius - x * x )
c
c        Calculate corner phi
c
         phi = atan ( y/x )
c
c        alpha equals twice phi
c
         alpha = 2. * phi

         af = 0.5 * radius * radius  * ( alpha - sin (alpha) )

         wf = 2. * y

         o  = radius * ( alpha )

      endif
      end
