      subroutine KAUPST(ngrid  ,nsamp  ,nnf    ,nnmu   ,np     ,h      ,
     +                  q      ,h2     ,q2     ,pfa    ,pmua   ,pw     ,
     +                  res    ,kgain  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAUPST (KAlman UPdate model STate)
c
c Module description: This routine updates the prediction of water levels,
c                     discharges and uncertain correction parameters due
c                     to measurements using the Kalman gain matrix.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  6 h(ngrid)          IO Contains water levels in every grid point. It is
c                         stored on index 2 of the packed array hpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c  8 h2(ngrid)         I  Water level in every grid point at time
c                         t(n+1).
c 14 kgain(np,nsamp)   I  Kalman gain matrix
c  1 ngrid             I  Number of grid points in network.
c  3 nnf               I  Number of uncertain bed friction parameters.
c  4 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  2 nsamp             I  Number of hydrodynamic samples (measurements)
c 10 pfa(nnf)          IO Uncertain bed friction parameters of all
c 11 pmua(nnmu)        IO Uncertain energy loss parameters in case of
c 12 pw                IO Uncertain wind stress parameter.
c  9 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c  7 q(ngrid)          IO Contains discharges in every grid point. It is
c                         stored on index 2 of the packed array qpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c 13 res(nsamp)        I  Residual vector
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kaupst.pf,v $
c Revision 1.3  1999/03/15  15:52:29  kuipe_j
c tabs removed
c
c Revision 1.2  1996/04/12  13:05:36  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:10  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Declaration of parameters
c
      integer ngrid, nsamp, nnf, nnmu, np
      real    pfa(nnf), pmua(nnmu), pw, res(nsamp)
      real    kgain(np,nsamp)
      double precision h(ngrid), q(ngrid), h2(ngrid), q2(ngrid)
c
c     Declaration of local variables
c
      integer i, j
c
c     Update of the hydrodynamic variables.
c
      do 20 i = 1, ngrid
         h(i) = h2(i)
         q(i) = q2(i)
         do 10 j = 1, nsamp
            h(i) = h(i) + dble( kgain(i,j) * res(j) )
            q(i) = q(i) + dble( kgain(ngrid+i,j) * res(j) )
   10    continue
   20 continue
c
c     Update of the correction parameters for the bottom friction.
c
      do 40 i = 1, nnf
         do 30 j = 1, nsamp
            pfa(i) = pfa(i) + kgain(2*ngrid+i,j) * res(j)
   30    continue
   40 continue
c
c     Update of the correction parameters for contraction at free gate
c     flow.
c
      do 60 i = 1, nnmu
         do 50 j = 1, nsamp
            pmua(i) = pmua(i) + kgain(2*ngrid+nnf+i,j) * res(j)
   50    continue
   60 continue
c
c     Update of the correction parameter for the wind stress.
c
      do 70 j = 1, nsamp
         pw = pw + kgain(np,j) * res(j)
   70 continue
c
      end
