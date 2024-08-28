      subroutine FLQSEC(ngrid  ,q2     ,af     ,asubsc ,c      ,r      ,
     +                  cs     ,rs     ,afs    ,q2s    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLQSEC (FLow Q in SECtions)
c
c Module description: This subroutine distributes the calculated dis-
c                     charge over the different sections.
c
c                     A cross section may be sub divided into more than
c                     one section. The algorithm to calculate discharges
c                     and water levels calculate only one discharge. For
c                     several modules the flow in the sections must be
c                     known. Therefore the calculated discharge will be
c                     distributed over the different sections.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c  9 afs(ngrid,2)      I  Actual flow area per section:
c                         (i,1) = For main section of grid point i.
c                         (i,2) = For sub section 1 of grid point i.
c  4 asubsc(ngrid)     I  Defines the actual number of sub sections for
c                         avery cross section (depending on the actual
c                         water level):
c                         c1sec (0) : Main section only (0 sub sections)
c                         c2sec (1) : 1 sub section
c                         c3sec (2) : 2 sub sections
c  5 c(ngrid)          I  Actual Chezy coefficient for total channel in
c                         every grid point.
c  7 cs(ngrid,3)       I  Actual Chezy coefficient in a section (main,
c                         sub 1, sub 2) for every grid point.
c  1 ngrid             I  Number of grid points in network.
c  2 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 10 q2s(ngrid,2)      IO Flow through main and sub section 1 at time
c                         t(n+1).
c  6 r(ngrid)          I  Actual hydraulic radius for total channel in
c                         every grid point.
c  8 rs(ngrid,3)       I  Actual hydraulic radius in a section (main,
c                         sub 1, sub 2) for every grid point.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flqsec.pf,v $
c Revision 1.4  1999/03/15  15:50:39  kuipe_j
c tabs removed
c
c Revision 1.3  1995/05/30  09:55:24  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:25  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:05  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:31:29  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:54  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer ngrid
      real    asubsc(ngrid)
      real    af(ngrid)
      real    cs(ngrid,3), rs(ngrid,3), q2s(ngrid,2)
      real    afs(ngrid,2)
      real    c(ngrid), r(ngrid)
	double precision q2(ngrid)
c
c     Declaration of local variables:
c
      integer i
      real    acrtot
c
      do 100 i = 1, ngrid
c
c        Do for each gridpoint and section:
c
         if     ( int( asubsc(i) ) .eq. 0 ) then
c
c           Flow in main channel only
c
            q2s(i,1) = q2(i)
            q2s(i,2) = 0.
c
         elseif ( int ( asubsc(i) ) .eq. 1 ) then
c
c           Flow in main channel and in sub section 1
c
            if ( r(i)<0d0 .or. rs(i,1)<0d0 ) then
	         r (i  ) = max( 0.0, r (i  ))
	         rs(i,1) = max( 0.0, rs(i,1))
	      end if
            acrtot = af(i) * c(i) * sqrt(r(i))
            q2s(i,1) = afs(i,1) * cs(i,1) * sqrt(rs(i,1)) * q2(i)/acrtot
            q2s(i,2) = q2(i) - q2s(i,1)
c
         else
c
c           Flow in main channel and both sub sections
c
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
c
      end
