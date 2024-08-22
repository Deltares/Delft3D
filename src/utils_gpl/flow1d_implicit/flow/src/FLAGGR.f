      subroutine FLAGGR(istep  ,wqagst ,theta  ,ngrid  ,nqlat  ,q1     ,
     &                  q2     ,q1s    ,q2s    ,qlat   ,qaggr  ,qlaggr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLAGGR (FLow AGGRegate)
c
c Module description: Aggregation of hydrodynamic results to have a
c                     small interface file from water flow subsystem to
c                     water quality subsystem.
c
c                     To have a small interface file to the water qual-
c                     ity subsystem flow results will be aggregated
c                     using the time step of the water quality process
c                     run. The following variables are aggregated:
c
c                     -   discharges;
c                     -   lateral discharges.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 istep             I  Current time step number (t(n+1)).
c  4 ngrid             I  Number of grid points in network.
c  5 nqlat             I  Number of lateral discharge stations.
c  6 q1(ngrid)         I  Discharge in every grid point at time t(n).
c  8 q1s(ngrid,2)      IO Flow through main and sub section 1 at time
c                         t(n).
c  7 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c  9 q2s(ngrid,2)      I  Flow through main and sub section 1 at time
c                         t(n+1).
c 11 qaggr(ngrid,3)    IO Aggregated flow through main and subsections 1
c                         and 2, using time step of the water quality
c                         process run.
c 12 qlaggr(nqlat)     IO Aggregated lateral discharge using time step
c                         of the water quality process run.
c 10 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
c                         time level n+1/2.
c  3 theta             I  Time weight factor in Preissmann scheme.
c  2 wqagst            I  Time step of the water quality process run.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flaggr.pf,v $
c Revision 1.4  1999/03/15  14:19:30  kuipe_j
c improve writing Aggr-file
c
c Revision 1.3  1995/05/30  09:54:41  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:37  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:24  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:30:24  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:45  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer istep, wqagst, ngrid, nqlat
      real    theta, q1s(ngrid,2), q2s(ngrid,2)
      real    qlat(*), qaggr(ngrid,3), qlaggr(*)

	double precision q1(ngrid), q2(ngrid)
c
c     Declaration of local variables
c
      integer i   , j
c
      if (istep .gt. 0) then
      if ( wqagst .eq. 1 ) then
         do 10 i = 1, ngrid
            qaggr(i,1) = (1.0-theta)*q1(i)    + theta*q2(i)
            qaggr(i,2) = (1.0-theta)*q1s(i,1) + theta*q2s(i,1)
            qaggr(i,3) = (1.0-theta)*q1s(i,2) + theta*q2s(i,2)
   10    continue
c
         do 20 i = 1, nqlat
            qlaggr(i) = qlat(i)
   20    continue
c
      else
         if ( mod(istep,wqagst) .eq. 1 ) then
c
c           First step of aggregation period
c
            do 30 i = 1, ngrid
               qaggr(i,1) = (1.0-theta)*q1(i)    + q2(i)
               qaggr(i,2) = (1.0-theta)*q1s(i,1) + q2s(i,1)
               qaggr(i,3) = (1.0-theta)*q1s(i,2) + q2s(i,2)
   30       continue
c
            do 40 i = 1, nqlat
               qlaggr(i) = qlat(i)
   40       continue
c
         else if ( mod(istep,wqagst) .eq. 0 ) then
c
c           Last step of aggregation period
c
            do 50 i = 1, ngrid
               qaggr(i,1) = ( qaggr(i,1) + theta*q2(i)    ) / wqagst
               qaggr(i,2) = ( qaggr(i,2) + theta*q2s(i,1) ) / wqagst
               qaggr(i,3) = ( qaggr(i,3) + theta*q2s(i,2) ) / wqagst
   50       continue
c
            do 60 i = 1, nqlat
               qlaggr(i) = ( qlaggr(i) + qlat(i) ) / wqagst
   60       continue
         else
            do 70 i = 1, ngrid
               qaggr(i,1) = qaggr(i,1) + q2(i)
               qaggr(i,2) = qaggr(i,2) + q2s(i,1)
               qaggr(i,3) = qaggr(i,3) + q2s(i,2)
   70       continue
c
            do 80 i = 1, nqlat
               qlaggr(i) = qlaggr(i) + qlat(i)
   80       continue
         endif
      endif
      endif
c
c     Move Q section from n+1 to time n
c
      do 100 i = 1, ngrid
         do 90 j = 1, 2
            q1s(i,j) = q2s(i,j)
   90    continue
  100 continue
c
      end
