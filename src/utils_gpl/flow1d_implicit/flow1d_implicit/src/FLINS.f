      subroutine FLINS (steady ,iter   ,ngrid  ,h1     ,h      ,h2     ,
     +                  q1     ,q      ,q2     ,at1    ,at     ,qtyp )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLINS (FLow Initialise Next Step)
c
c Module description: Initialise areas, discharges and water levels for
c                     next step.
c
c                     Depending on the run option for the flow module,
c                     steady or unsteady, every iteration or first time
c                     step, areas, discharges and water levels from time
c                     level n+1 have to be used as discharges and water
c                     levels from time level n. This routine will copy
c                     the iterated values to the appropriate arrays.
c
c                     The following symbols are used:
c
c                     1     =     array from time level n
c                     2     =     array from time level n+1
c                     *     =     last iterated value
c
c                     The arrays Q2 and h2 are computed by FLBRAN. The
c                     next time the flow module is called the values of
c                     Q2 and h2 are stored in Q* and h*.
c
c                     A typical value for the discharges in the model 
c                     is determined as the maximum absolute discharge.
c                     This value is used in the relative discharge 
c                     convergence-criterion.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 at1(ngrid)        O  Total area at every cross section for time
c                         level t(n).
c 11 at(ngrid)         I  Actual total area at every grid point.
c  4 h1(ngrid)         O  Water level in every grid point at time t(n).
c  6 h2(ngrid)         IO Water level in every grid point at time
c                         t(n+1).
c  5 h(ngrid)          IO Water level in every grid point at the latest
c                         iteration.
c  2 iter              I  Iteration step.
c  3 ngrid             I  Number of grid points in network.
c  7 q1(ngrid)         O  Discharge in every grid point at time t(n).
c  8 q(ngrid)          IO Discharge in every grid point at the latest
c                         iteration.
c  9 q2(ngrid)         IO Discharge in every grid point at time t(n+1).
c 12 qtyp              O  Typical value for the discharges (maximum|Q|) 
c  1 steady            I  Calculation mode (0 or 1)
c                         0 = steady calculation
c                         1 = unsteady calculation
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flins.pf,v $
c Revision 1.9  1999/03/15  15:50:08  kuipe_j
c tabs removed
c
c Revision 1.8  1997/01/23  08:29:09  kuipe_j
c Make flow module robust
c
c Revision 1.7  1996/01/17  14:38:33  kuipe_j
c header update
c
c Revision 1.6  1995/11/21  11:07:54  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.5  1995/09/22  10:01:48  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/23  14:29:22  overmar
c Lelystad juli 95 ingebracht
c
c Revision 1.3  1995/05/30  09:55:12  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:11  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:54  hoeks_a
c Initial check-in
c
c Revision 1.1.1.1  1993/07/21  14:43:52  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer iter, ngrid
      double precision    h1 (ngrid), h (ngrid), h2(ngrid)
      double precision    q1 (ngrid), q (ngrid), q2(ngrid)
      real    at1(ngrid), at(ngrid), qtyp
c
c     Declaration of local variable:
c
      logical steady
      integer i
	double precision temp
c
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
