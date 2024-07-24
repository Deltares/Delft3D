      subroutine SERIES (time, iopt, xar, lxar, y)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             SERIES (SERIES of fourier or tidal component)
c
c Module description: Calculate serie
c
c                     Boundary conditions can be defined as tables of
c                     time or as series. The series are also stored in
c                     the standard datastructure TABLE and are processed
c                     by this standard routine.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 iopt              I  Component option:
c                           cbfour (3) : Fourier components
c                           cbtidl (4) : Tidal components
c  4 lxar              I  Length of array with X-values.
c  1 time              I  Actual time level tn+1. in sec.
c  3 xar               I  Array with X-values.
c  5 y                 IO Interpolated Y value.
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: series.pf,v $
c Revision 1.4  1999/03/15  15:51:24  kuipe_j
c tabs removed
c
c Revision 1.3  1995/09/22  10:03:11  kuipe_j
c variable dimensions, new headers
c
c Revision 1.2  1995/05/30  07:02:30  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:33  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  09:22:49  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:04  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:00  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer iopt, lxar
      real    xar(lxar), y
      double  precision  time
c
c     Declaration of local variables:
c
      integer i, ip, n
      real    a0, ai, w0, wi, gammai
      double  precision  arg
c
      include '../include/sobcon.i'
c
      if (iopt .eq. cbfour) then
c
c        Fourier components
c
         a0 = xar(1)
         n  = int(xar(2))
         w0 = xar(3)
         y  = a0

         do 100 i = 1, n
            ip     = 3 + (i-1)*2 + 1
            ai     = xar(ip)
            gammai = xar(ip+1)
            arg    = dble(w0) * time * i - dble(gammai)
            y      = y + ai * sngl(dcos( arg ))
  100    continue
c
      else if (iopt .eq. cbtidl) then
c
c        Tidal components
c
         a0 = xar(1)
         n  = int(xar(2))
         y  = a0
c
         do 200 i = 1, n
            ip     = 2 + (i-1)*3 + 1
            wi     = xar(ip)
            ai     = xar(ip+1)
            gammai = xar(ip+2)
            arg    = dble(wi) * time - dble(gammai)
            y      = y + ai * sngl(dcos( arg ))
  200    continue
      endif
c
      end
