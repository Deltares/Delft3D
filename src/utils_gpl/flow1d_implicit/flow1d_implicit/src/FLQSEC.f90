subroutine FLQSEC(ngrid  ,q2     ,af     ,asubsc ,c      ,r      ,&
&cs     ,rs     ,afs    ,q2s    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLQSEC (FLow Q in SECtions)
!
! Module description: This subroutine distributes the calculated dis-
!                     charge over the different sections.
!
!                     A cross section may be sub divided into more than
!                     one section. The algorithm to calculate discharges
!                     and water levels calculate only one discharge. For
!                     several modules the flow in the sections must be
!                     known. Therefore the calculated discharge will be
!                     distributed over the different sections.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 af(ngrid)         I  Flow area at every grid point at time t(n+1)
!  9 afs(ngrid,2)      I  Actual flow area per section:
!                         (i,1) = For main section of grid point i.
!                         (i,2) = For sub section 1 of grid point i.
!  4 asubsc(ngrid)     I  Defines the actual number of sub sections for
!                         avery cross section (depending on the actual
!                         water level):
!                         c1sec (0) : Main section only (0 sub sections)
!                         c2sec (1) : 1 sub section
!                         c3sec (2) : 2 sub sections
!  5 c(ngrid)          I  Actual Chezy coefficient for total channel in
!                         every grid point.
!  7 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
!                         sub 1, sub 2) for every grid point.
!  1 ngrid             I  Number of grid points in network.
!  2 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 10 q2s(ngrid,2)      IO Flow through main and sub section 1 at time
!                         t(n+1).
!  6 r(ngrid)          I  Actual hydraulic radius for total channel in
!                         every grid point.
!  8 rs(ngrid,3)       I  Actual hydraulic radius in a section (main,
!                         sub 1, sub 2) for every grid point.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flqsec.pf,v $
! Revision 1.4  1999/03/15  15:50:39  kuipe_j
! tabs removed
!
! Revision 1.3  1995/05/30  09:55:24  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:25  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:05  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:31:29  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:54  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer ngrid
   real    asubsc(ngrid)
   real    af(ngrid)
   real    cs(ngrid,3), rs(ngrid,3), q2s(ngrid,2)
   real    afs(ngrid,2)
   real    c(ngrid), r(ngrid)
   double precision q2(ngrid)
!
!     Declaration of local variables:
!
   integer i
   real    acrtot
!
   do 100 i = 1, ngrid
!
!        Do for each gridpoint and section:
!
      if     ( int( asubsc(i) ) .eq. 0 ) then
!
!           Flow in main channel only
!
         q2s(i,1) = q2(i)
         q2s(i,2) = 0.
!
      elseif ( int ( asubsc(i) ) .eq. 1 ) then
!
!           Flow in main channel and in sub section 1
!
         if ( r(i)<0d0 .or. rs(i,1)<0d0 ) then
            r (i  ) = max( 0.0, r (i  ))
            rs(i,1) = max( 0.0, rs(i,1))
         end if
         acrtot = af(i) * c(i) * sqrt(r(i))
         q2s(i,1) = afs(i,1) * cs(i,1) * sqrt(rs(i,1)) * q2(i)/acrtot
         q2s(i,2) = q2(i) - q2s(i,1)
!
      else
!
!           Flow in main channel and both sub sections
!
         if ( r(i)<0.0 .or. rs(i,1)<0.0 .or. rs(i,2)<0.0 ) then
            r (i  ) = max( 0.0, r (i  ))
            rs(i,1) = max( 0.0, rs(i,1))
            rs(i,2) = max( 0.0, rs(i,2))
         end if
         acrtot = af(i) * c(i) * sqrt(r(i))
         q2s(i,1) = afs(i,1) * cs(i,1) * sqrt(rs(i,1)) * q2(i)/acrtot
         q2s(i,2) = afs(i,2) * cs(i,2) * sqrt(rs(i,2)) * q2(i)/acrtot
      endif

100 continue
!
end
