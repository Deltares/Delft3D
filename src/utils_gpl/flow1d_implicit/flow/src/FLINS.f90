subroutine FLINS (steady ,iter   ,ngrid  ,h1     ,h      ,h2     ,&
&q1     ,q      ,q2     ,at1    ,at     ,qtyp )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLINS (FLow Initialise Next Step)
!
! Module description: Initialise areas, discharges and water levels for
!                     next step.
!
!                     Depending on the run option for the flow module,
!                     steady or unsteady, every iteration or first time
!                     step, areas, discharges and water levels from time
!                     level n+1 have to be used as discharges and water
!                     levels from time level n. This routine will copy
!                     the iterated values to the appropriate arrays.
!
!                     The following symbols are used:
!
!                     1     =     array from time level n
!                     2     =     array from time level n+1
!                     *     =     last iterated value
!
!                     The arrays Q2 and h2 are computed by FLBRAN. The
!                     next time the flow module is called the values of
!                     Q2 and h2 are stored in Q* and h*.
!
!                     A typical value for the discharges in the model
!                     is determined as the maximum absolute discharge.
!                     This value is used in the relative discharge
!                     convergence-criterion.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 at1(ngrid)        O  Total area at every cross section for time
!                         level t(n).
! 11 at(ngrid)         I  Actual total area at every grid point.
!  4 h1(ngrid)         O  Water level in every grid point at time t(n).
!  6 h2(ngrid)         IO Water level in every grid point at time
!                         t(n+1).
!  5 h(ngrid)          IO Water level in every grid point at the latest
!                         iteration.
!  2 iter              I  Iteration step.
!  3 ngrid             I  Number of grid points in network.
!  7 q1(ngrid)         O  Discharge in every grid point at time t(n).
!  8 q(ngrid)          IO Discharge in every grid point at the latest
!                         iteration.
!  9 q2(ngrid)         IO Discharge in every grid point at time t(n+1).
! 12 qtyp              O  Typical value for the discharges (maximum|Q|)
!  1 steady            I  Calculation mode (0 or 1)
!                         0 = steady calculation
!                         1 = unsteady calculation
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flins.pf,v $
! Revision 1.9  1999/03/15  15:50:08  kuipe_j
! tabs removed
!
! Revision 1.8  1997/01/23  08:29:09  kuipe_j
! Make flow module robust
!
! Revision 1.7  1996/01/17  14:38:33  kuipe_j
! header update
!
! Revision 1.6  1995/11/21  11:07:54  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.5  1995/09/22  10:01:48  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/23  14:29:22  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:55:12  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:11  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:54  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:43:52  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer iter, ngrid
   double precision    h1 (ngrid), h (ngrid), h2(ngrid)
   double precision    q1 (ngrid), q (ngrid), q2(ngrid)
   real    at1(ngrid), at(ngrid), qtyp
!
!     Declaration of local variable:
!
   logical steady
   integer i
   double precision temp
!
   if ( iter .eq. 1 .or. steady ) then
      qtyp = 0.0
      do 10 i = 1, ngrid
         h1 (i) = h2(i)
         q1 (i) = q2(i)
         at1(i) = at(i)
         qtyp = sngl( max( qtyp , sngl( abs(q1(i))) ) )
10    continue
   endif
   do 20 i = 1, ngrid
      temp  =  q(i)
      q(i)  = q2(i)
      q2(i) = temp
      temp  =  h(i)
      h(i)  = h2(i)
      h2(i) = temp
20 continue
end
