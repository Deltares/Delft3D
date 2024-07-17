      subroutine INTTAB ( nxy, access, xar, yar, xs, ys )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          General routines Module
c
c Programmer:         J.Brouwer
c
c Module:             INTTAB (INTerpolate in TABle)
c
c Module description: Interpolate in table (Continuous or Discrete)
c
c                     This routine interpolates in tables which are
c                     stored in the standard datastructure TABLE.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 access            I  Access method xy :
c                         x = ctbnpf (0) : no period defined
c                         x = ctbpfu (1) : period defined
c                         y = ctbico (0) : continuous interpolation
c                         y = ctbidi (1) : discrete interpolation
c  1 nxy               I  Length of the arrays xar and yar.
c  3 xar               I  Array with X-values.
c  5 xs                I  Offered X-value.
c  4 yar               I  Array with Y-values.
c  6 ys                O  Interpolated Y-value.
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
c $Log: inttab.pf,v $
c Revision 1.5  1999/03/15  15:51:20  kuipe_j
c tabs removed
c
c Revision 1.4  1998/03/02  19:31:59  kuipe_j
c improve interpolation for negative times and period
c
c Revision 1.3  1995/09/22  10:03:01  kuipe_j
c variable dimensions, new headers
c
c Revision 1.2  1995/05/30  07:02:26  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:30  hoeks_a
c Initial check-in
c
c Revision 1.4  1994/12/02  13:11:15  kuipe_j
c Negative argument are possible now.
c Improvements for discrete and periodic interpolation.
c
c Revision 1.3  1994/11/28  09:22:47  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:32:02  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:59  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer nxy, access
      real xar(nxy), yar(nxy), xxs ,ys
      double  precision        xs  ,bgn ,fween
      logical period
c
c     Declaration of local variables:
c
      integer i, ind, sa, perp1 ,perp2
      real    lt,rint
c
c     Include sobek constant file
c
      include '../include/sobcon.i'
c
      xxs = sngl(xs)
c     Period defined ?
c
      period = access/10 .eq. ctbpfu 
      if ( period ) then
         if ( xar(2) .lt. xar(nxy) ) then
c
c           Increasing arguments
            perp1 = 2
            perp2 = nxy
         else
c
c           Decreasing arguments
            perp1 = nxy
            perp2 = 2
         endif
         fween = dble(xar(perp1))
         bgn = dble( xar(perp2) - xar(1) )
         if (bgn .lt. fween) then
            bgn = fween 
         else
c           interpolation itself is not periodical
            period = .false.
         endif
         if (xs.gt.xar(perp2)) then
c           periodical; x > 0
            lt = sngl(dmod(xs-bgn,dble(xar(1))) + bgn)
         elseif (xs.lt.xar(perp1) .and. bgn .lt. fween+1.0D-2 ) then
c           periodical; x < 0
            lt = sngl(dmod(xs-bgn,dble(xar(1))) + bgn + dble(xar(1)))
         else
c           run-in part (also for x < 0)
            lt = xxs
         endif
         sa  = 2
      else
         lt  = xxs
         sa  = 1
      endif
c
c     For discrete interpolation add small number to find proper
c     table point if xs=xar(i)
c
      if ( mod(access,10) .eq. ctbidi ) then
         lt = lt*1.0000005
      endif
c
c     Index table and find X value
c
      ind = 0
      if ( xar(sa) .lt. xar(nxy) ) then
         do 100 i = sa, nxy
            if ( lt .ge. xar(i) ) then
               ind = i
            else
               goto 300
            endif
 100     continue
      else
         do 200 i = nxy, sa, -1
            if ( lt .ge. xar(i) ) then
               ind = i
            else
               goto 300
            endif
 200     continue
      endif
 300  continue
c
c     Check for continuous or discrete interpolation
c
      if ( mod(access,10) .eq. ctbico ) then
c
c        Interpolate in X using ind and find Y value
c
         if ( xar(sa) .lt. xar(nxy) ) then
            if ( ind .eq. nxy ) then
               if ( period ) then
                 rint = ( lt - xar(nxy) ) / 
     &                  ( xar(sa) + xar(1) - xar(nxy) )
                 ys   = yar(nxy) + rint * ( yar(sa) - yar(nxy) )
               else
                 ys = yar(nxy)
               endif
            else if ( ind .eq. 0 ) then
               if ( period ) then
                 rint = ( lt - xar(sa) ) / 
     &                  ( xar(nxy) + xar(1) - xar(sa) )
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
c
c        Find Y value using ind
c
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
c
      end
