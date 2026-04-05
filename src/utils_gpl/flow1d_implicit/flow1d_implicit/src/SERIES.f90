subroutine SERIES (time, iopt, xar, lxar, y)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             SERIES (SERIES of fourier or tidal component)
!
! Module description: Calculate serie
!
!                     Boundary conditions can be defined as tables of
!                     time or as series. The series are also stored in
!                     the standard datastructure TABLE and are processed
!                     by this standard routine.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 iopt              I  Component option:
!                           cbfour (3) : Fourier components
!                           cbtidl (4) : Tidal components
!  4 lxar              I  Length of array with X-values.
!  1 time              I  Actual time level tn+1. in sec.
!  3 xar               I  Array with X-values.
!  5 y                 IO Interpolated Y value.
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: series.pf,v $
! Revision 1.4  1999/03/15  15:51:24  kuipe_j
! tabs removed
!
! Revision 1.3  1995/09/22  10:03:11  kuipe_j
! variable dimensions, new headers
!
! Revision 1.2  1995/05/30  07:02:30  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:33  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  09:22:49  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:04  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:00  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer iopt, lxar
   real    xar(lxar), y
   double  precision  time
!
!     Declaration of local variables:
!
   integer i, ip, n
   real    a0, ai, w0, wi, gammai
   double  precision  arg
!
   include '../include/sobcon.i'
!
   if (iopt .eq. cbfour) then
!
!        Fourier components
!
      a0 = xar(1)
      n  = int(xar(2))
      w0 = xar(3)
      y  = a0

      do 100 i = 1, n
         ip     = 3 + (i-1)*2 + 1
         ai     = xar(ip)
         gammai = xar(ip+1)
         arg    = dble(w0) * time * i - dble(gammai)
         y      = y + ai * real(cos( arg ), kind=kind(y))
100   continue
!
   else if (iopt .eq. cbtidl) then
!
!        Tidal components
!
      a0 = xar(1)
      n  = int(xar(2))
      y  = a0
!
      do 200 i = 1, n
         ip     = 2 + (i-1)*3 + 1
         wi     = xar(ip)
         ai     = xar(ip+1)
         gammai = xar(ip+2)
         arg    = dble(wi) * time - dble(gammai)
         y      = y + ai * real(cos( arg ), kind=kind(y))
200   continue
   endif
!
end
