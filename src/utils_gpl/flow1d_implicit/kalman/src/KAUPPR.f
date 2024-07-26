      subroutine KAUPPR(ngrid  ,nsamp  ,nnf    ,nnmu   ,np     ,nosdim ,
     +                  smploc ,kalpar ,p1     ,p2     ,rescov ,sample ,
     +                  scares ,smpns  ,indx   ,rhsm   ,juer   ,h      ,
     +                  q      ,h2     ,q2     ,pfa    ,pmua   ,pw     ,
     +                  res    ,kgain  ,ker    ,nbran  ,branch ,kg2    ,
     +                  x      )
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAUPPR (KAlman UPdate PRediction)
c
c Module description: This routine first computes the Kalman gain from
c                     the covariance and the measurements of the predict-
c                     ion time. Then it updates the prediction of both
c                     mean and covariance.
c
c                     Determine the weight matrix H. Update for non
c                     available measurements.
c
c                     If filtering is time dependent the Kalman gain will
c                     be calculated. In case of time independent filter-
c                     ing, the Kalman gain has been read from file.
c
c                     The computation of the Kalman gain is combined with
c                     the determination of the scaled residuals using a
c                     Cholesky decomposition.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 33 branch(4,nbran)   I
c 19 h2(ngrid)         I  Water level in every grid point at time
c                         t(n+1).
c 17 h                 P  -
c 14 indx              P  -
c 16 juer              P  -
c  7 kalpar(5)         I  Kalman parameters.
c                         (1) Prediction interval (number of steps).
c                         (2) Time dependent (1) or time independent (2)
c 26 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 25 kgain(np,nsamp)   IO Kalman gain matrix
c  3 nbran             I  Number of branches in network
c  1 ngrid             I  Number of grid points in network.
c  3 nnf               I  Number of uncertain bed friction parameters.
c  4 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c  2 nsamp             I  Number of hydrodynamic samples (measurements)
c  8 p1(np,np)         IO Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n+1 (filtered
c                         values) or n|n (previous time step).
c  9 p2(np,np)         I  Matrix with covariances of waterlevels,
c                         discharges and uncertain correction parameters
c                         (bed friction, contraction in case of free gate
c                         flow and wind) on time level n+1|n (predicted
c                         values).
c 21 pfa               P  -
c 22 pmua              P  -
c 23 pw                P  -
c 20 q2(ngrid)         I  Discharge in every grid point at time t(n+1).
c 18 q                 P  -
c 24 res(nsamp)        IO Residual vector
c 10 rescov(nsamp,     IO Matrix with covariances of residuals.
c      ,nsamp)
c 15 rhsm(nsamp)       IO Right hand side vector measurement system
c 11 sample(nsamp,     I  Measured discharges or water levels:
c      ,ntsam)            (i,j) = value at location i on filterstep j
c 12 scares(nsamp)     O  Scaled residual vector
c  6 smploc(nsamp)     I  location of samples:
c                         water levels: smploc(i) = grid point of location
c                         i.
c                         discharges: smploc(i) = j + ngrid if location i
c                         is on grid point j.
c 13 smpns(nosdim,nsamp)I Measurement noise
c                         (1,i) mean
c                         (2,i) deviation (input) or variance
c                         (3,i) indicator for absolute(0) or relative
c                               (1) noise variance
c                         (4,i) time correlation
c                         (5,i) current variance
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c chbksb  CHolesky decomp.: BacKSuBstitution
c chdcmp  CHolesky DeCoMPosition
c error   write an ERROR to the error file.
c kacvsm  KALman make CoVariance SyMmetric
c kaupst  KAlman UPdate model STate
c lubkss  LU decomp. BacKSubst. Single Prec.
c ludcms  LU DeCoMposition Single precision
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kauppr.pf,v $
c Revision 1.5  1999/03/15  15:52:28  kuipe_j
c tabs removed
c
c Revision 1.4  1996/12/05  10:00:08  kuipe_j
c Smoothing kgain,linearization,limit covariance,etc
c
c Revision 1.3  1996/05/28  13:31:52  kuipe_j
c Smoothing  added
c
c Revision 1.2  1996/04/12  13:05:35  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:09  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c
c     Function declaration:
c
      logical EQUAL
c
c     Declaration of parameters
c
      integer ngrid, nsamp, nnf, nnmu, np, juer, ker, nosdim
      integer smploc(nsamp), indx(nsamp)
      integer nbran
      integer branch(4,nbran)
      real    res(nsamp), scares(nsamp), sample(nsamp),
     +        smpns(nosdim,nsamp)
      real    rescov(nsamp,nsamp), rhsm(nsamp), kgain(np,nsamp)
      real    x(ngrid)
      real    kalpar(5), p1(np,np), p2(np,np)
      real    pfa(nnf), pmua(nnmu), pw
      real    kg2(np)      
      double precision h(ngrid), q(ngrid), h2(ngrid), q2(ngrid)

c
c     Declaration of local variables
c
      integer i, j, ip, jp, igr, kerlu, m, mp ,ii
      real    d, scalvr,mxvarh  ,mxvarq
      real    nu,mxdevh ,mxdevq
      logical  EPSEQU
      external EPSEQU
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
      nu     = kalpar(3)
      mxdevh = kalpar(4)
      mxdevq = kalpar(5)
c
c     Check on correct measurement
c
c     call KACKME()
c
c     Calculate the residual vector: res(1...nsamp)
c
      do 10 i = 1, nsamp
         igr = smploc(i)
         if ( igr .le. ngrid ) then
            res(i) = sample(i) - h2(igr) - smpns(1,i)
         else
            res(i) = sample(i) - q2(igr-ngrid) - smpns(1,i)
         endif
   10 continue
c
      if ( int(kalpar(2)) .eq. 1 ) then
c
c        Time dependent filtering
c
c        In order to avoid an exponential growth of the variances,
c        the predicted model state covariance is scaled down such
c        that the water level variance and discharge variance
c        are within user specified limits.
c
         mxvarh = mxdevh**2
         mxvarq = mxdevq**2
         do 16 i=1, ngrid
c           limitation of water level variances
            if (.not.EPSEQU(p2(i,i),0.0,mxvarh)) then
               scalvr  = sqrt(mxvarh/abs(p2(i,i)))
               p2(i,i) = p2(i,i)*scalvr
c
               do 12 j=1,2*ngrid
                  p2(i,j) = p2(i,j)*scalvr
                  p2(j,i) = p2(i,j)
  12           continue
            endif
c
c           limitation of discharge variances
            ii = ngrid+i
            if (.not.EPSEQU(p2(ii,ii),0.0,mxvarq)) then
               scalvr  = sqrt(mxvarq/abs(p2(ii,ii)))
               p2(ii,ii) = p2(ii,ii)*scalvr
c
               do 14 j=1,2*ngrid
                  p2(ii,j) = p2(ii,j)*scalvr
                  p2(j,ii) = p2(ii,j)
  14           continue
            endif
  16     continue
c
c        Calculate covariance of residuals
c
c        a. Contributions of the predicted covariance
c
         do 30 i = 1, nsamp
            ip = smploc(i)
            do 20 j = 1, nsamp
               jp = smploc(j)
               rescov(i,j) = p2(ip,jp)
   20       continue
   30    continue
c
c        Overwrite for missing values
c
         do 50 i = 1, nsamp
            if ( EQUAL(sample(i), -999.999 ) ) then
               do 40 j = 1, nsamp
                  rescov(i,j) = 0.
                  rescov(j,i) = 0.
   40          continue
            endif
   50    continue
c
c        b. Contributions of the measurement noise covariance
c
         do 60 i = 1, nsamp
            if ( EQUAL(sample(i), -999.999 ) ) then
               rescov(i,i) = 1.
            else
               if (nosdim.le.2 .or. int(smpns(3,i)).eq.0) then
                  rescov(i,i) = rescov(i,i) + smpns(2,i)
               else
c                 Relative noise
                  rescov(i,i) = rescov(i,i) + smpns(2,i) * sample(i)**2
               endif
            endif
   60    continue
c
c        Array rescov must be kept a.o. for output purposes so, store
c        rescov temporarily in P1.
c
c        Copy rescov ====> P1
c
         do 80 i = 1, nsamp
            do 70 j = 1, nsamp
               p1(i,j) = rescov(i,j)
   70       continue
   80    continue
c
c        Make LU decomposition
c
         call LUDCMS (rescov ,nsamp  ,nsamp  ,indx   ,d      ,rhsm   ,
     +                kerlu  )
c
         if ( kerlu .ne. 0 ) then
c
c           Matrix singular
c
            ker = fatal
            call ERROR (juer ,'KAUPPR Matrix singular' ,ekamat ,ker)
            goto 1000
         endif
c
c        Computation of the Kalman gain: Kgain(np,nsamp)
c
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
  100       continue
  110    continue
c
c        intermezzo to test the smoothing of the columns of kgain
c
         if (nu.gt.(0.0001)) then
            call KASMCV (nu, np, nsamp, x, branch, nbran, ngrid,
     +                   kgain, kg2)
         endif
c
c        Now load rescov back from P1
c        Copy P1 ====> rescov
c
         do 130 i = 1, nsamp
            do 120 j = 1, nsamp
               rescov(i,j) = p1(i,j)
  120       continue
  130    continue
c
c        Computation of the scaled residual: scares(nsamp)
c
         call CHDCMP (rescov, nsamp)

         do 140 i = 1, nsamp
            scares(i) = res(i)
  140    continue
c
         call CHBKSB (rescov ,nsamp  ,scares )
c
c        Now load rescov back from P1
c        Copy P1 ====> rescov
c
         do 160 i = 1, nsamp
            do 150 j = 1, nsamp
               rescov(i,j) = p1(i,j)
  150       continue
  160    continue
c
c        Update model state
c
         call KAUPST (ngrid  ,nsamp  ,nnf    ,nnmu   ,np     ,h      ,
     +                q      ,h2     ,q2     ,pfa    ,pmua   ,pw     ,
     +                res    ,kgain  )
c
c        Update covariance
c
         do 190 i = 1, np
            do 180 j = 1, np
               p1(i,j) = p2(i,j)
               do 170 m = 1, nsamp
                  mp = smploc(m)
                  p1(i,j) = p1(i,j) - kgain(i,m) * p2(mp,j)
  170          continue
  180       continue
  190    continue
c
c        Make filtered covariances synmetric
c
         call KACVSM (np     ,p1     )
      else
         call KAUPST (ngrid  ,nsamp  ,nnf    ,nnmu   ,np     ,h      ,
     +                q      ,h2     ,q2     ,pfa    ,pmua   ,pw     ,
     +                res    ,kgain  )
      endif

 1000 continue
      end
