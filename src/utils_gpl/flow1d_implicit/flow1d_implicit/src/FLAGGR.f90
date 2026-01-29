subroutine FLAGGR(istep  ,wqagst ,theta  ,ngrid  ,nqlat  ,q1     ,&
&q2     ,q1s    ,q2s    ,qlat   ,qaggr  ,qlaggr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLAGGR (FLow AGGRegate)
!
! Module description: Aggregation of hydrodynamic results to have a
!                     small interface file from water flow subsystem to
!                     water quality subsystem.
!
!                     To have a small interface file to the water qual-
!                     ity subsystem flow results will be aggregated
!                     using the time step of the water quality process
!                     run. The following variables are aggregated:
!
!                     -   discharges;
!                     -   lateral discharges.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 istep             I  Current time step number (t(n+1)).
!  4 ngrid             I  Number of grid points in network.
!  5 nqlat             I  Number of lateral discharge stations.
!  6 q1(ngrid)         I  Discharge in every grid point at time t(n).
!  8 q1s(ngrid,2)      IO Flow through main and sub section 1 at time
!                         t(n).
!  7 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
!  9 q2s(ngrid,2)      I  Flow through main and sub section 1 at time
!                         t(n+1).
! 11 qaggr(ngrid,3)    IO Aggregated flow through main and subsections 1
!                         and 2, using time step of the water quality
!                         process run.
! 12 qlaggr(nqlat)     IO Aggregated lateral discharge using time step
!                         of the water quality process run.
! 10 qlat(nqlat)       I  (i) = Actual lateral discharge in station i on
!                         time level n+1/2.
!  3 theta             I  Time weight factor in Preissmann scheme.
!  2 wqagst            I  Time step of the water quality process run.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flaggr.pf,v $
! Revision 1.4  1999/03/15  14:19:30  kuipe_j
! improve writing Aggr-file
!
! Revision 1.3  1995/05/30  09:54:41  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:37  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:24  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:30:24  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:45  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer istep, wqagst, ngrid, nqlat
   real    theta, q1s(ngrid,2), q2s(ngrid,2)
   real    qlat(*), qaggr(ngrid,3), qlaggr(*)

   double precision q1(ngrid), q2(ngrid)
!
!     Declaration of local variables
!
   integer i   , j
!
   if (istep .gt. 0) then
      if ( wqagst .eq. 1 ) then
         do 10 i = 1, ngrid
            qaggr(i,1) = (1.0-theta)*q1(i)    + theta*q2(i)
            qaggr(i,2) = (1.0-theta)*q1s(i,1) + theta*q2s(i,1)
            qaggr(i,3) = (1.0-theta)*q1s(i,2) + theta*q2s(i,2)
10       continue
!
         do 20 i = 1, nqlat
            qlaggr(i) = qlat(i)
20       continue
!
      else
         if ( mod(istep,wqagst) .eq. 1 ) then
!
!           First step of aggregation period
!
            do 30 i = 1, ngrid
               qaggr(i,1) = (1.0-theta)*q1(i)    + q2(i)
               qaggr(i,2) = (1.0-theta)*q1s(i,1) + q2s(i,1)
               qaggr(i,3) = (1.0-theta)*q1s(i,2) + q2s(i,2)
30          continue
!
            do 40 i = 1, nqlat
               qlaggr(i) = qlat(i)
40          continue
!
         else if ( mod(istep,wqagst) .eq. 0 ) then
!
!           Last step of aggregation period
!
            do 50 i = 1, ngrid
               qaggr(i,1) = ( qaggr(i,1) + theta*q2(i)    ) / wqagst
               qaggr(i,2) = ( qaggr(i,2) + theta*q2s(i,1) ) / wqagst
               qaggr(i,3) = ( qaggr(i,3) + theta*q2s(i,2) ) / wqagst
50          continue
!
            do 60 i = 1, nqlat
               qlaggr(i) = ( qlaggr(i) + qlat(i) ) / wqagst
60          continue
         else
            do 70 i = 1, ngrid
               qaggr(i,1) = qaggr(i,1) + q2(i)
               qaggr(i,2) = qaggr(i,2) + q2s(i,1)
               qaggr(i,3) = qaggr(i,3) + q2s(i,2)
70          continue
!
            do 80 i = 1, nqlat
               qlaggr(i) = qlaggr(i) + qlat(i)
80          continue
         endif
      endif
   endif
!
!     Move Q section from n+1 to time n
!
   do 100 i = 1, ngrid
      do 90 j = 1, 2
         q1s(i,j) = q2s(i,j)
90    continue
100 continue
!
end
