subroutine KAUPST(ngrid  ,nsamp  ,nnf    ,nnmu   ,np     ,h      ,&
&q      ,h2     ,q2     ,pfa    ,pmua   ,pw     ,&
&res    ,kgain  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAUPST (KAlman UPdate model STate)
!
! Module description: This routine updates the prediction of water levels,
!                     discharges and uncertain correction parameters due
!                     to measurements using the Kalman gain matrix.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 h(ngrid)          IO Contains water levels in every grid point. It is
!                         stored on index 2 of the packed array hpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
!  8 h2(ngrid)         I  Water level in every grid point at time
!                         t(n+1).
! 14 kgain(np,nsamp)   I  Kalman gain matrix
!  1 ngrid             I  Number of grid points in network.
!  3 nnf               I  Number of uncertain bed friction parameters.
!  4 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  2 nsamp             I  Number of hydrodynamic samples (measurements)
! 10 pfa(nnf)          IO Uncertain bed friction parameters of all
! 11 pmua(nnmu)        IO Uncertain energy loss parameters in case of
! 12 pw                IO Uncertain wind stress parameter.
!  9 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
!  7 q(ngrid)          IO Contains discharges in every grid point. It is
!                         stored on index 2 of the packed array qpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
! 13 res(nsamp)        I  Residual vector
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kaupst.pf,v $
! Revision 1.3  1999/03/15  15:52:29  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:36  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:10  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Declaration of parameters
!
   integer ngrid, nsamp, nnf, nnmu, np
   real    pfa(nnf), pmua(nnmu), pw, res(nsamp)
   real    kgain(np,nsamp)
   double precision h(ngrid), q(ngrid), h2(ngrid), q2(ngrid)
!
!     Declaration of local variables
!
   integer i, j
!
!     Update of the hydrodynamic variables.
!
   do 20 i = 1, ngrid
      h(i) = h2(i)
      q(i) = q2(i)
      do 10 j = 1, nsamp
         h(i) = h(i) + dble( kgain(i,j) * res(j) )
         q(i) = q(i) + dble( kgain(ngrid+i,j) * res(j) )
10    continue
20 continue
!
!     Update of the correction parameters for the bottom friction.
!
   do 40 i = 1, nnf
      do 30 j = 1, nsamp
         pfa(i) = pfa(i) + kgain(2*ngrid+i,j) * res(j)
30    continue
40 continue
!
!     Update of the correction parameters for contraction at free gate
!     flow.
!
   do 60 i = 1, nnmu
      do 50 j = 1, nsamp
         pmua(i) = pmua(i) + kgain(2*ngrid+nnf+i,j) * res(j)
50    continue
60 continue
!
!     Update of the correction parameter for the wind stress.
!
   do 70 j = 1, nsamp
      pw = pw + kgain(np,j) * res(j)
70 continue
!
end
