subroutine INTTAB ( nxy, access, xar, yar, xs, ys )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          General routines Module
!
! Programmer:         J.Brouwer
!
! Module:             INTTAB (INTerpolate in TABle)
!
! Module description: Interpolate in table (Continuous or Discrete)
!
!                     This routine interpolates in tables which are
!                     stored in the standard datastructure TABLE.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 access            I  Access method xy :
!                         x = ctbnpf (0) : no period defined
!                         x = ctbpfu (1) : period defined
!                         y = ctbico (0) : continuous interpolation
!                         y = ctbidi (1) : discrete interpolation
!  1 nxy               I  Length of the arrays xar and yar.
!  3 xar               I  Array with X-values.
!  5 xs                I  Offered X-value.
!  4 yar               I  Array with Y-values.
!  6 ys                O  Interpolated Y-value.
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
! $Log: inttab.pf,v $
! Revision 1.5  1999/03/15  15:51:20  kuipe_j
! tabs removed
!
! Revision 1.4  1998/03/02  19:31:59  kuipe_j
! improve interpolation for negative times and period
!
! Revision 1.3  1995/09/22  10:03:01  kuipe_j
! variable dimensions, new headers
!
! Revision 1.2  1995/05/30  07:02:26  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:30  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  13:11:15  kuipe_j
! Negative argument are possible now.
! Improvements for discrete and periodic interpolation.
!
! Revision 1.3  1994/11/28  09:22:47  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:32:02  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer nxy, access
   real xar(nxy), yar(nxy), xxs ,ys
   double  precision        xs  ,bgn ,fween
   logical period
!
!     Declaration of local variables:
!
   integer i, ind, sa, perp1 ,perp2
   real    lt,rint
!
!     Include sobek constant file
!
   include '../include/sobcon.i'
!
   xxs = real(xs, kind=kind(xxs))
!     Period defined ?
!
   period = access/10 .eq. ctbpfu
   if ( period ) then
      if ( xar(2) .lt. xar(nxy) ) then
!
!           Increasing arguments
         perp1 = 2
         perp2 = nxy
      else
!
!           Decreasing arguments
         perp1 = nxy
         perp2 = 2
      endif
      fween = dble(xar(perp1))
      bgn = dble( xar(perp2) - xar(1) )
      if (bgn .lt. fween) then
         bgn = fween
      else
!           interpolation itself is not periodical
         period = .false.
      endif
      if (xs.gt.xar(perp2)) then
!           periodical; x > 0
         lt = real(dmod(xs-bgn,dble(xar(1))) + bgn)
      elseif (xs.lt.xar(perp1) .and. bgn .lt. fween+1.0D-2 ) then
!           periodical; x < 0
         lt = real(dmod(xs-bgn,dble(xar(1))) + bgn + dble(xar(1)))
      else
!           run-in part (also for x < 0)
         lt = xxs
      endif
      sa  = 2
   else
      lt  = xxs
      sa  = 1
   endif
!
!     For discrete interpolation add small number to find proper
!     table point if xs=xar(i)
!
   if ( mod(access,10) .eq. ctbidi ) then
      lt = lt*1.0000005
   endif
!
!     Index table and find X value
!
   ind = 0
   if ( xar(sa) .lt. xar(nxy) ) then
      do 100 i = sa, nxy
         if ( lt .ge. xar(i) ) then
            ind = i
         else
            goto 300
         endif
100   continue
   else
      do 200 i = nxy, sa, -1
         if ( lt .ge. xar(i) ) then
            ind = i
         else
            goto 300
         endif
200   continue
   endif
300 continue
!
!     Check for continuous or discrete interpolation
!
   if ( mod(access,10) .eq. ctbico ) then
!
!        Interpolate in X using ind and find Y value
!
      if ( xar(sa) .lt. xar(nxy) ) then
         if ( ind .eq. nxy ) then
            if ( period ) then
               rint = ( lt - xar(nxy) ) /&
               &( xar(sa) + xar(1) - xar(nxy) )
               ys   = yar(nxy) + rint * ( yar(sa) - yar(nxy) )
            else
               ys = yar(nxy)
            endif
         else if ( ind .eq. 0 ) then
            if ( period ) then
               rint = ( lt - xar(sa) ) /&
               &( xar(nxy) + xar(1) - xar(sa) )
               ys   = yar(sa) + rint * ( yar(nxy) - yar(sa) )
            else
               ys = yar(sa)
            endif
         else
            rint = ( lt - xar(ind) ) / ( xar(ind+1) - xar(ind) )
            ys   = yar(ind) + rint * ( yar(ind+1) - yar(ind) )
         endif
      else
         if ( ind .eq. sa ) then
            ys = yar(sa)
         else if ( ind .eq. 0 ) then
            ys = yar(nxy)
         else
            rint = ( lt - xar(ind) ) / ( xar(ind-1) - xar(ind) )
            ys   = yar(ind) + rint * ( yar(ind-1) - yar(ind) )
         endif
      endif
   else
!
!        Find Y value using ind
!
      if ( xar(sa) .lt. xar(nxy) ) then
         if ( ind .eq. 0 ) then
            ys = yar(sa)
         else
            ys = yar(ind)
         endif
      else
         if ( ind .eq. 0 ) then
            ys = yar(nxy)
         else
            ys = yar(ind)
         endif
      endif
   endif
!
end
