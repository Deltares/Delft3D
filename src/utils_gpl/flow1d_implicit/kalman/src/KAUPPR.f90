subroutine KAUPPR(ngrid  ,nsamp  ,nnf    ,nnmu   ,np     ,nosdim ,&
&smploc ,kalpar ,p1     ,p2     ,rescov ,sample ,&
&scares ,smpns  ,indx   ,rhsm   ,juer   ,h      ,&
&q      ,h2     ,q2     ,pfa    ,pmua   ,pw     ,&
&res    ,kgain  ,ker    ,nbran  ,branch ,kg2    ,&
&x      )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAUPPR (KAlman UPdate PRediction)
!
! Module description: This routine first computes the Kalman gain from
!                     the covariance and the measurements of the predict-
!                     ion time. Then it updates the prediction of both
!                     mean and covariance.
!
!                     Determine the weight matrix H. Update for non
!                     available measurements.
!
!                     If filtering is time dependent the Kalman gain will
!                     be calculated. In case of time independent filter-
!                     ing, the Kalman gain has been read from file.
!
!                     The computation of the Kalman gain is combined with
!                     the determination of the scaled residuals using a
!                     Cholesky decomposition.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 33 branch(4,nbran)   I
! 19 h2(ngrid)         I  Water level in every grid point at time
!                         t(n+1).
! 17 h                 P  -
! 14 indx              P  -
! 16 juer              P  -
!  7 kalpar(5)         I  Kalman parameters.
!                         (1) Prediction interval (number of steps).
!                         (2) Time dependent (1) or time independent (2)
! 26 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
! 25 kgain(np,nsamp)   IO Kalman gain matrix
!  3 nbran             I  Number of branches in network
!  1 ngrid             I  Number of grid points in network.
!  3 nnf               I  Number of uncertain bed friction parameters.
!  4 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
!  2 nsamp             I  Number of hydrodynamic samples (measurements)
!  8 p1(np,np)         IO Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n+1 (filtered
!                         values) or n|n (previous time step).
!  9 p2(np,np)         I  Matrix with covariances of waterlevels,
!                         discharges and uncertain correction parameters
!                         (bed friction, contraction in case of free gate
!                         flow and wind) on time level n+1|n (predicted
!                         values).
! 21 pfa               P  -
! 22 pmua              P  -
! 23 pw                P  -
! 20 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
! 18 q                 P  -
! 24 res(nsamp)        IO Residual vector
! 10 rescov(nsamp,     IO Matrix with covariances of residuals.
!      ,nsamp)
! 15 rhsm(nsamp)       IO Right hand side vector measurement system
! 11 sample(nsamp,     I  Measured discharges or water levels:
!      ,ntsam)            (i,j) = value at location i on filterstep j
! 12 scares(nsamp)     O  Scaled residual vector
!  6 smploc(nsamp)     I  location of samples:
!                         water levels: smploc(i) = grid point of location
!                         i.
!                         discharges: smploc(i) = j + ngrid if location i
!                         is on grid point j.
! 13 smpns(nosdim,nsamp)I Measurement noise
!                         (1,i) mean
!                         (2,i) deviation (input) or variance
!                         (3,i) indicator for absolute(0) or relative
!                               (1) noise variance
!                         (4,i) time correlation
!                         (5,i) current variance
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! chbksb  CHolesky decomp.: BacKSuBstitution
! chdcmp  CHolesky DeCoMPosition
! error   write an ERROR to the error file.
! kacvsm  KALman make CoVariance SyMmetric
! kaupst  KAlman UPdate model STate
! lubkss  LU decomp. BacKSubst. Single Prec.
! ludcms  LU DeCoMposition Single precision
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kauppr.pf,v $
! Revision 1.5  1999/03/15  15:52:28  kuipe_j
! tabs removed
!
! Revision 1.4  1996/12/05  10:00:08  kuipe_j
! Smoothing kgain,linearization,limit covariance,etc
!
! Revision 1.3  1996/05/28  13:31:52  kuipe_j
! Smoothing  added
!
! Revision 1.2  1996/04/12  13:05:35  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:09  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!
!     Function declaration:
!
   logical EQUAL
!
!     Declaration of parameters
!
   integer ngrid, nsamp, nnf, nnmu, np, juer, ker, nosdim
   integer smploc(nsamp), indx(nsamp)
   integer nbran
   integer branch(4,nbran)
   real    res(nsamp), scares(nsamp), sample(nsamp),&
   &smpns(nosdim,nsamp)
   real    rescov(nsamp,nsamp), rhsm(nsamp), kgain(np,nsamp)
   real    x(ngrid)
   real    kalpar(5), p1(np,np), p2(np,np)
   real    pfa(nnf), pmua(nnmu), pw
   real    kg2(np)
   double precision h(ngrid), q(ngrid), h2(ngrid), q2(ngrid)

!
!     Declaration of local variables
!
   integer i, j, ip, jp, igr, kerlu, m, mp ,ii
   real    d, scalvr,mxvarh  ,mxvarq
   real    nu,mxdevh ,mxdevq
   logical  EPSEQU
   external EPSEQU
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
   nu     = kalpar(3)
   mxdevh = kalpar(4)
   mxdevq = kalpar(5)
!
!     Check on correct measurement
!
!     call KACKME()
!
!     Calculate the residual vector: res(1...nsamp)
!
   do 10 i = 1, nsamp
      igr = smploc(i)
      if ( igr .le. ngrid ) then
         res(i) = sample(i) - h2(igr) - smpns(1,i)
      else
         res(i) = sample(i) - q2(igr-ngrid) - smpns(1,i)
      endif
10 continue
!
   if ( int(kalpar(2)) .eq. 1 ) then
!
!        Time dependent filtering
!
!        In order to avoid an exponential growth of the variances,
!        the predicted model state covariance is scaled down such
!        that the water level variance and discharge variance
!        are within user specified limits.
!
      mxvarh = mxdevh**2
      mxvarq = mxdevq**2
      do 16 i=1, ngrid
!           limitation of water level variances
         if (.not.EPSEQU(p2(i,i),0.0,mxvarh)) then
            scalvr  = sqrt(mxvarh/abs(p2(i,i)))
            p2(i,i) = p2(i,i)*scalvr
!
            do 12 j=1,2*ngrid
               p2(i,j) = p2(i,j)*scalvr
               p2(j,i) = p2(i,j)
12          continue
         endif
!
!           limitation of discharge variances
         ii = ngrid+i
         if (.not.EPSEQU(p2(ii,ii),0.0,mxvarq)) then
            scalvr  = sqrt(mxvarq/abs(p2(ii,ii)))
            p2(ii,ii) = p2(ii,ii)*scalvr
!
            do 14 j=1,2*ngrid
               p2(ii,j) = p2(ii,j)*scalvr
               p2(j,ii) = p2(ii,j)
14          continue
         endif
16    continue
!
!        Calculate covariance of residuals
!
!        a. Contributions of the predicted covariance
!
      do 30 i = 1, nsamp
         ip = smploc(i)
         do 20 j = 1, nsamp
            jp = smploc(j)
            rescov(i,j) = p2(ip,jp)
20       continue
30    continue
!
!        Overwrite for missing values
!
      do 50 i = 1, nsamp
         if ( EQUAL(sample(i), -999.999 ) ) then
            do 40 j = 1, nsamp
               rescov(i,j) = 0.
               rescov(j,i) = 0.
40          continue
         endif
50    continue
!
!        b. Contributions of the measurement noise covariance
!
      do 60 i = 1, nsamp
         if ( EQUAL(sample(i), -999.999 ) ) then
            rescov(i,i) = 1.
         else
            if (nosdim.le.2 .or. int(smpns(3,i)).eq.0) then
               rescov(i,i) = rescov(i,i) + smpns(2,i)
            else
!                 Relative noise
               rescov(i,i) = rescov(i,i) + smpns(2,i) * sample(i)**2
            endif
         endif
60    continue
!
!        Array rescov must be kept a.o. for output purposes so, store
!        rescov temporarily in P1.
!
!        Copy rescov ====> P1
!
      do 80 i = 1, nsamp
         do 70 j = 1, nsamp
            p1(i,j) = rescov(i,j)
70       continue
80    continue
!
!        Make LU decomposition
!
      call LUDCMS (rescov ,nsamp  ,nsamp  ,indx   ,d      ,rhsm   ,&
      &kerlu  )
!
      if ( kerlu .ne. 0 ) then
!
!           Matrix singular
!
         ker = fatal
         call ERROR (juer ,'KAUPPR Matrix singular' ,ekamat ,ker)
         goto 1000
      endif
!
!        Computation of the Kalman gain: Kgain(np,nsamp)
!
      do 110 j = 1, np
         do 90 i = 1, nsamp
            ip = smploc(i)
            if ( EQUAL(sample(i), -999.999 ) ) then
               rhsm(i) = 0.
            else
               rhsm(i) = p2(ip,j)
            endif
90       continue

         call LUBKSS (rescov ,nsamp  ,nsamp  ,indx   ,rhsm   )

         do 100 i = 1, nsamp
            kgain(j,i) = rhsm(i)
100      continue
110   continue
!
!        intermezzo to test the smoothing of the columns of kgain
!
      if (nu.gt.(0.0001)) then
         call KASMCV (nu, np, nsamp, x, branch, nbran, ngrid,&
         &kgain, kg2)
      endif
!
!        Now load rescov back from P1
!        Copy P1 ====> rescov
!
      do 130 i = 1, nsamp
         do 120 j = 1, nsamp
            rescov(i,j) = p1(i,j)
120      continue
130   continue
!
!        Computation of the scaled residual: scares(nsamp)
!
      call CHDCMP (rescov, nsamp)

      do 140 i = 1, nsamp
         scares(i) = res(i)
140   continue
!
      call CHBKSB (rescov ,nsamp  ,scares )
!
!        Now load rescov back from P1
!        Copy P1 ====> rescov
!
      do 160 i = 1, nsamp
         do 150 j = 1, nsamp
            rescov(i,j) = p1(i,j)
150      continue
160   continue
!
!        Update model state
!
      call KAUPST (ngrid  ,nsamp  ,nnf    ,nnmu   ,np     ,h      ,&
      &q      ,h2     ,q2     ,pfa    ,pmua   ,pw     ,&
      &res    ,kgain  )
!
!        Update covariance
!
      do 190 i = 1, np
         do 180 j = 1, np
            p1(i,j) = p2(i,j)
            do 170 m = 1, nsamp
               mp = smploc(m)
               p1(i,j) = p1(i,j) - kgain(i,m) * p2(mp,j)
170         continue
180      continue
190   continue
!
!        Make filtered covariances synmetric
!
      call KACVSM (np     ,p1     )
   else
      call KAUPST (ngrid  ,nsamp  ,nnf    ,nnmu   ,np     ,h      ,&
      &q      ,h2     ,q2     ,pfa    ,pmua   ,pw     ,&
      &res    ,kgain  )
   endif

1000 continue
end
