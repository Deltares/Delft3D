subroutine gmiflh ( igpbou ,igpcel ,ngrid ,nfrac ,alphac ,alphad ,&
&alphae ,dtm    ,celer ,celert,sedtr  ,source ,&
&x      ,dfrac ,ds     ,spredc,cela1  ,intiph ,&
&jugralg)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:
!
! Module:             GMIFLH (Graded MOrphology Integral on First
!                             or Last Half point)
!
! Module description: This subroutine calculates the change in area for
!                     an internal point. If a branch has grid points
!                     numbered from 1..n internal grid points will be
!                     located between 2 1/2, .., n-3/2. Sometimes the
!                     points 1/2 and n-1/2 are internal too. This will
!                     be evaluated on a higher level. Notice that the
!                     integral Ii+1/2 will be returned to the calling
!                     routine. This integral value will be used in the
!                     next call as Ii-1/2. Notice that on the first
!                     internal point the lateral sediment from i-1/2
!                     will be assigned completely.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 alphac            I  Stability factor for bottom scheme (>1)
!  8 celer(ngrid,*)    I  Sediment celerity for each gridpoint.
!               1|2       - Normal branches:
!                         (i,1) = Celerity in gridpoint i in main secti-
!                                 on.
!                         - Sedredge branches:
!                         (i,1) = Celerity in gridpoint i,
!                                 left channel.
!                         (i,2) = Celerity in gridpoint i,
!                                 right channel.
! 13 deltaa            O  Calculated change in area
!  7 dtm               I  Morphology time step
!  2 igp               I  Gridpoint number
!  1 igpm1             I  Gridpoint number - 1
!  3 igpp1             I  Gridpoint number + 1
! 12 intiph            IO Calculated integral value on i + 1/2
!  5 ngrid             I  Number of grid points in network.
!  9 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
!               1|2       (At first transports per unit width, finaly
!                         total transports)
!                         - Normal branches:
!                         (i,1) = Sediment transport in gridpoint i of
!                                 main section.
!                         - Sedredge branches:
!                         (i,1) = Sediment transport in gridpoint i,
!                                 left channel.
!                         (i,2) = Sediment transport in gridpoint i,
!                                 right channel.
!                         (i,3) = Sediment exchange in gridpoint i.
! 11 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: gmiflh.F,v $
! Revision 1.3  1996/01/05  15:43:24  kuipe_j
! Lateral sediment and structures
!
! Revision 1.2  1995/09/27  10:11:35  kuipe_j
! Maintenance
!
!
!***********************************************************************
!
!     Parameters
!
   integer  igpbou ,igpcel ,ngrid  ,nfrac  ,jugralg
   real     alphac ,alphad ,alphae ,dtm
   real     celer (ngrid,nfrac,5)  ,celert (ngrid)       ,&
   &sedtr (ngrid,nfrac+2)  ,&
   &source(ngrid,nfrac+2)  ,&
   &x     (ngrid)          ,cela1  (nfrac,nfrac) ,&
   &ds    (nfrac)          ,spredc (nfrac)       ,&
   &intiph(nfrac)          ,&
   &dfrac (nfrac)
!
!     Local variables
!
   integer  igp   ,igpp1  ,jf
   real     dx    ,dtm2   ,sediph
!
!     Calculate dx
!
   if (igpcel .gt. igpbou) then
      igp   = igpbou
      igpp1 = igpcel
   else
      igp   = igpcel
      igpp1 = igpbou
   endif

   dx = abs(x(igpp1) - x(igp))
!
!     Calculate predicted transport
!
   call gmpred (ngrid  ,nfrac  ,igp    ,dx     ,dtm    ,alphac ,&
!                                                 <cela0>
   &alphad ,alphae ,sedtr  ,source ,celer(1,1,1)   ,&
!                 <cela1a>        <cela1b>        <cela2>
   &celer(1,1,2)   ,celer(1,1,3)   ,celer(1,1,4)   ,&
!                 <cela3>
   &celer(1,1,5)   ,dfrac  ,cela1  ,ds     ,celert ,&
   &spredc         ,jugralg)
!
!     Calculate dx for lateral sediment and delta A calculation
!
   dtm2 = dtm * .5

   do 10 jf=1,nfrac
!
!        Calculate sediment on i+1/2
!
      sediph = (sedtr(igp,jf) + sedtr (igpp1,jf)) / 2.
!
!        Calculate integral on i+1/2
!
      intiph(jf) = ( sediph + spredc(jf)) * dtm2
!
10 continue
!
end
