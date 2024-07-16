      subroutine KAINS2(ngrid  ,h      ,q      )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAINS2 (KAlman Initialize Next Step 2)
c
c Module description: Exchange the filtered and predicted discharges and
c                     water levels if it is a filter step. So in a next
c                     step the filtered values will become the values of
c                     the previous time step (h2 --> h1, q2 --> q1). i
c                     As a result also the predicted values are kept (h*,
c                     q*) for output purposes.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 h(ngrid)          IO Contains water levels in every grid point. It is
c                         stored on index 2 of the packed array hpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c  1 ngrid             I  Number of grid points in network.
c  3 q(ngrid)          IO Contains discharges in every grid point. It is
c                         stored on index 2 of the packed array qpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kains2.pf,v $
c Revision 1.3  1999/03/15  15:51:56  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:06  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:39  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer ngrid
      double precision h(ngrid,3), q(ngrid,3)
c
c     Declaration of local variable:
c
      integer i
      real    rdum
c
c     Exchange:  h2 <----> h*  and  q2 <----> q*
c
      do 10 i = 1, ngrid
         rdum   = h(i,2)
         h(i,2) = h(i,3)
         h(i,3) = rdum
         rdum   = q(i,2)
         q(i,2) = q(i,3)
         q(i,3) = rdum
   10 continue
      end
