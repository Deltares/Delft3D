subroutine KANORM(ngrid  ,ngridm ,nbran  ,ibr    ,i1     ,i2     ,&
&g      ,dt1    ,psi    ,theta  ,&
&h1     ,q1     ,h      ,q      ,grid   ,x      ,&
&wf     ,af     ,&
&chz    ,r      ,alfab  ,tauwi  ,eta    ,lsalt  ,&
&rho    ,rhow   ,prslot ,dqltdh ,dalfdh ,dcdh   ,&
&drdh   ,dcdq   ,dwfdh  ,detadh ,&
&scifri ,nnf    ,pfa    ,pw     ,&
&a1     ,b1     ,c1     ,d1     ,ea1    ,eb1    ,&
&ec1    ,ed1    ,a2     ,b2     ,c2     ,d2     ,&
&f2     ,w2     ,m2     ,ea2    ,eb2    ,ec2    ,&
&ed2    ,ef2    ,ew2    ,em2    ,psltvr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KANORM (KAlman abcde coefficients 'NORMal' gridpoints)
!
! Module description: Subroutine KANORM computes the matrix coefficients
!                     for 'normal' gridpoints. These coefficients are
!                     A1-D1, EA1-ED1 and A2-F2, EA2-EF2.
!
!                     For normal gridpoints, coefficients for both the
!                     continuity equation and the momentum equation are
!                     determined. For structure grid points only the
!                     coefficients of the continuity equations are deter-
!                     mined. The other coefficients (of the stage
!                     discharge equation) for structure gridpoints are
!                     determined in routine KASTRU.
!                     The coefficients are described in S-FO-004 Par 3.3
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 43 a1(ngridm)        IO A1-coefficient of continuity eq. per gridpnt.
! 51 a2(ngridm)        IO A2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 20 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 25 alfab(ngrid)      I  Actual Bousinessq coefficient in grid point i.
! 44 b1(ngridm)        IO B1-coefficient of continuity eq. per gridpnt.
! 52 b2(ngridm)        IO B2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 45 c1(ngridm)        IO C1-coefficient of continuity eq. per gridpnt.
! 53 c2(ngridm)        IO C2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 23 chz               I  -
! 46 d1(ngridm)        IO D1-coefficient of continuity eq. per gridpnt.
! 54 d2(ngridm)        IO D2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 33 dalfdh(ngrid)     I  Derivative of Bousinesq constant ALFAb to
!                         waterlevel in every grid point i on time n+1/2
!                         (d(ALFAb)/dh).
! 34 dcdh(ngrid)       I  Derivative of Chezy value to waterlevel in every
!                         grid point i on time n+1/2 (dC/dh).
! 36 dcdq(ngrid)       I  Derivative of Chezy value to discharge in every
!                         grid point i on time n+1/2 (dC/dQ).
! 38 detadh(ngrid)     I  Derivative of extra resistance coefficient ETA
!                         to waterlevel in every grid point i+1/2 on time
!                         n+1/2 (d(ETA)/dh). Value at i+1/2 is stored at
!                         index i.
! 32 dqltdh(ngrid)     I  Derivative of lateral discharge to waterlevel in
!                         every grid point i+1/2 on time n+1/2
!                         (d(Qlat)/dh). Value at i+1/2 is stored at index
!                         i.
! 35 drdh(ngrid)       I  Derivative of hydraulic radius to waterlevel in
!                         every grid point i on time n+1/2 (dR/dh).
!  8 dt1               I  -
! 37 dwfdh(ngrid)      I  Derivative of flow width to waterlevel in every
!                         grid point i on time n+1/2 (dWf/dh).
! 47 ea1(ngrid)        IO EA1 right hand side coefficient of continuity
! 58 ea2(ngrid)        IO EA2 right hand side coefficient of momentum
! 48 eb1(ngrid)        IO EB1 right hand side coefficient of continuity
! 59 eb2(ngrid)        IO EB2 right hand side coefficient of momentum
! 49 ec1(ngrid)        IO EC1 right hand side coefficient of continuity
! 60 ec2(ngrid)        IO EC2 right hand side coefficient of momentum
! 50 ed1(ngrid)        IO ED1 right hand side coefficient of continuity
! 61 ed2(ngrid)        IO ED2 right hand side coefficient of momentum
! 62 ef2(ngrid)        IO EF2 right hand side coefficient of momentum
! 64 em2(ngrid)        O  EM2 right hand side coefficient of Q-h relation
! 27 eta(ngrid)        I  Extra resistance coefficient ETA in every grid
!                         point i+1/2 on time n+1/2. Value at i+1/2 is
!                         stored at index i.
! 63 ew2(ngrid)        IO EW2 right hand side coefficient of momentum
! 55 f2(ngrid)         IO F2 coefficient of momentum equation
!  7 g                 I  Acceleration of gravity.
! 17 grid(ngrid)       I  Grid cell type definition:
!                         cgrdcl (1) : Normal grid cell
!                         cstrcl (2) : Structure cell
! 13 h1(ngrid)         I  Water level in every grid point at time t(n).
! 15 h(ngrid)          I  Contains water levels in every grid point. It is
!                         stored on index 2 of the packed array hpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
!  5 i1                I  Index of first grid point in actual branch.
!  6 i2                I  Index of last grid point in actual branch.
!  4 ibr               I  -
! 28 lsalt             I  Logical indicator for salt computation
!                         = .true.  : with salt computation
!                         = .false. : no salt computation
! 57 m2(ngrid)         O  M2 coefficient of Q-h relation
!  3 nbran             I  Number of branches.
!  1 ngrid             I  Number of grid points in network.
!  2 ngridm            I  Maximum number of gridpoints in a branch.
! 40 nnf               I  Number of uncertain bed friction parameters.
! 41 pfa(nnf)          I  Uncertain bed friction parameters of all
! 31 prslot(3,nbran)   I  Contains information concerning Preissmann
!                         slot (assuring positive water depths).
!                         (1,i) = Create slot indicator
!                                 csldis (0) : No slot in this branch.
!                                 cslena (1) : Create slot in branch.
!                         (2,i) = Reference area for slot generation.
!                         (3,i) = Depth of slot below lowest bottom of
!                                 branch.
! 63 psltvr            I  Preissmann slot variables for every grid point
!                         i (assuring positive water depths):
!                         (1,i) = Value for C**2*R for positive flow.
!                         (2,i) = Value for C**2*R for negative flow.
!                         (3,i) = Bottom of slot (funnel)
!                         (4,i) = Division level between trapezium and
!                                 rectangle of slot (top of rectangle
!                                 and bottom of trapezium)
!                         (5,i) = Top of slot
!                         (6,i) = Bottom width of slot (width of
!                                 rectangle)
!                         (7,i) = Top width of slot (top of trapezium)
!  9 psi               I  Space weight factor in Preissmann scheme.
! 42 pw                I  Uncertain wind stress parameter.
! 16 q(ngrid)          I  Contains discharges in every grid point. It is
!                         stored on index 2 of the packed array qpack.
!                         Flow:        current values during iteration
!                         (h*).
!                         Prediction:  last iterated value.
!                         Update:      filtered value (n+1|n+1)
! 14 q1(ngrid)         I  Discharge in every grid point at time t(n).
! 24 r(ngrid)          I  Actual hydraulic radius for total channel in
!                         every grid point.
! 29 rho(ngrid)        I  Density of diluted water per grid point.
! 30 rhow              I  Density of fresh water.
! 39 scifri(ngrid)     I  Contains the number of the uncorrelated r.n.
!                         process for bed friction (group nr. or correc-
!                         tion parameter nr.) of every normal grid cell,
!                         otherwise zero.
! 26 tauwi(ngrid)      I  Calculated wind friction for each gridpoint.
! 10 theta             I  Time weight factor in Preissmann scheme.
! 56 w2(ngrid)         IO W2 coefficient of momentum equation
! 56 w2(ngrid)         IO Total width at (n+theta2) in every grid point.
! 19 wf(ngrid)         I  Actual flow width at every grid point.
! 18 x(ngrid)          I  x(i) = X-coordinate of grid point i.
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kanorm.pf,v $
! Revision 1.11  1999/03/15  15:52:01  kuipe_j
! tabs removed
!
! Revision 1.10  1997/06/17  11:23:47  kuipe_j
! Initialize vars
!
! Revision 1.9  1997/06/04  11:18:16  kuipe_j
! Initialize arrays
!
! Revision 1.8  1997/02/17  10:22:36  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.7  1997/01/23  08:29:40  kuipe_j
! Make flow module robust
!
! Revision 1.6  1996/12/20  15:46:42  kuipe_j
! error in friction corr. par
!
! Revision 1.5  1996/12/05  10:00:04  kuipe_j
! Smoothing kgain,linearization,limit covariance,etc
!
! Revision 1.4  1996/12/02  10:04:58  kuipe_j
! avoid negative pointers
!
! Revision 1.3  1996/09/03  14:54:25  kuipe_j
! frequency time hist,etc
!
! Revision 1.2  1996/04/12  13:05:11  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:44  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer ngrid, ngridm, nbran, ibr, i1, i2, nnf
   real    g, psi, rhow, theta, pw
   real    x(ngrid), wf(ngrid), af(ngrid)
   real    chz(ngrid), r(ngrid), alfab(ngrid)
   real    prslot(3,nbran), psltvr(7,ngrid)
   real    tauwi(ngrid), eta(ngrid)
   real    rho(ngrid)
   real    pfa(nnf)
   real    dqltdh(*), dalfdh(*), dcdh(*), drdh(*), dcdq(*), dwfdh(*)
   real    detadh(*)
   integer grid(ngrid), scifri(ngrid)
   logical lsalt
!
   double precision dt1
   double precision h(ngrid), h1(ngrid), q1(ngrid), q(ngrid)
   double precision a1(ngridm), ea1(ngridm),&
   &b1(ngridm), eb1(ngridm),&
   &c1(ngridm), ec1(ngridm),&
   &d1(ngridm), ed1(ngridm)
   double precision a2(ngridm), ea2(ngridm),&
   &b2(ngridm), eb2(ngridm),&
   &c2(ngridm), ec2(ngridm),&
   &d2(ngridm), ed2(ngridm),&
   &f2(ngridm), ef2(ngridm),&
   &w2(ngridm), ew2(ngridm),&
   &m2(ngridm), em2(ngridm)
!
!     Declaration of local variables
!
   integer          i, j, k,  idx
   logical          lslot, wetlef, wetrig
   real             pf
   double precision a(7), b(7), c(7), d(7)
   double precision ea(7), eb(7), ec(7), ed(7)
   double precision ompsi, dxodt, dt2, dx, ltheta
   double precision ta, tb, tq, tnr, t1, t2, t3, t4
   double precision tm, hit, hi1t, qit, qi1t, theta1
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   dt2    = dble (dt1)
   ltheta = dble (theta)
!
!     Calculate 1 - psi
!
   ompsi  = dble (1.0 - psi)
   theta1 = 1.0D0 - ltheta
!
!     Preissmann slot active in branch ?
!
   lslot = int(prslot(1,ibr)) .eq. cslena
!
!     Loop over grid points in branch ibr
!
   k = 0
   do 100 i = i1, i2-1
      dx   = dble (x(i+1) - x(i))
      hit  = ltheta*h(i)   + theta1*h1(i)
      hi1t = ltheta*h(i+1) + theta1*h1(i+1)
      qit  = ltheta*q(i)   + theta1*q1(i)
      qi1t = ltheta*q(i+1) + theta1*q1(i+1)
!
!        Calculate dx / dt
!
      dxodt = dx / dt2
!
      k  = k + 1
!
!********************************
!        Continuity equation *
!********************************
!
!        1. First term continuity equation
!        [ Doc. S-FO-004.2PB / Eq. 3-9]
!
      t1    = wf (i) * dxodt
      t2    = wf (i+1) * dxodt
      a(1)  = dble (ompsi * t1)
      b(1)  = 0.0D0
      c(1)  = dble (psi * t2)
      d(1)  = 0.0D0
      ea(1) = dble (ompsi * t1)
      eb(1) = 0.0D0
      ec(1) = dble (psi * t2)
      ed(1) = 0.0D0
!
!        2. Second term continuity equation
!        [ Doc. S-FO-004.2PB / Eq. 3-10]
!
      a(2)  = 0.0D0
      b(2)  = -ltheta
      c(2)  = 0.0D0
      d(2)  = ltheta
      ea(2) = 0.0D0
      eb(2) = 1.0D0 - ltheta
      ec(2) = 0.0D0
      ed(2) = - (1.0D0 - ltheta)
!
!
!        3. Last term continuity equation
!        [ Doc. S-FO-004.2PB / Eq. 3-11]
!
      a(3)  = dble (-0.25 * dqltdh(i))
      b(3)  = 0.0D0
      c(3)  = a(3)
      d(3)  = 0.0D0
      ea(3) = -a(3)
      eb(3) = 0.0D0
      ec(3) = -a(3)
      ed(3) = 0.0D0
!
!        Evaluation of coefficients a1,b1,c1,d1 and ea1,eb1,ec1,ed1
!
      a1(k)  = 0.0D0
      b1(k)  = 0.0D0
      c1(k)  = 0.0D0
      d1(k)  = 0.0D0
      ea1(k) = 0.0D0
      eb1(k) = 0.0D0
      ec1(k) = 0.0D0
      ed1(k) = 0.0D0
      do 20 j = 1, 3
         a1(k)  = a1(k) + a(j)
         b1(k)  = b1(k) + b(j)
         c1(k)  = c1(k) + c(j)
         d1(k)  = d1(k) + d(j)
         ea1(k) = ea1(k) + ea(j)
         eb1(k) = eb1(k) + eb(j)
         ec1(k) = ec1(k) + ec(j)
         ed1(k) = ed1(k) + ed(j)
20    continue
!
!        Normal grid cell
!
      if (grid(i) .eq. cgrdcl) then
         idx= scifri(i)
         if (idx.eq.0) then
            pf =  1.
         else
            pf = pfa(idx)
         endif
!
!***********************************
!           Momentum equation      *
!***********************************
!
!           Normal gridpoint
!
!           1. Acceleration term momentum equation
!           [ Doc. S-FO-004.2PB / Eq. 3-15]
!
         a(1)  = 0.0D0
         b(1)  = dble (ompsi * dxodt)
         c(1)  = 0.0D0
         d(1)  = dble (psi * dxodt)
         ea(1) = 0.0D0
         eb(1) = b(1)
         ec(1) = 0.0D0
         ed(1) = d(1)
!
!           2. Convective term momentum equation
!           [ Doc. S-FO-004.2PB / Eq. 3-16]
!
         ta    = dble ( qit**2  / af(i))
         tb    = dble ( qi1t**2 / af(i+1))
!
         t1    = dble (ta * alfab(i) / af(i) * wf(i))
         t2    = dble (ta * dalfdh(i))
         t3    = dble (tb * alfab(i+1) / af(i+1) * wf(i+1))
         t4    = dble (tb * dalfdh(i+1))
!
         a(2)  =  ltheta * (t1 - t2)
         ea(2) =  theta1 * (t2 - t1)
         tm    =  dble ( 2.0D0 * alfab(i) / af(i) * qit)
         b(2)  = -ltheta * tm
         eb(2) =  theta1 * tm
         c(2)  =  ltheta * (t4 - t3)
         ec(2) =  theta1 * (t3 - t4)
         tm    =  dble ( 2.0D0 * alfab(i+1) / af(i+1) * qi1t)
         d(2)  =  ltheta * tm
         ed(2) = -theta1 * tm
!
!           3. Hydrostatic pressure term in momentum equation
!           [ Doc. S-FO-004.2PB / Eq. 3-17]
!
         t1    = dble (0.5D0 * g * (af(i) + af(i+1)))
         t2    = dble (0.5D0 * g * (wf(i) + wf(i+1))) * (hi1t - hit)
!
         a(3)  =  ltheta * (t2 - t1)
         b(3)  =  0.0D0
         c(3)  =  ltheta * (t1 + t2)
         d(3)  =  0.0D0
         ea(3) =  theta1 * (t1 - t2)
         eb(3) =  0.0D0
         ec(3) = -theta1 * (t1 + t2)
         ed(3) =  0.0D0
!
!           4. Bottom friction term in momentum equation
!           [ Doc. S-FO-004.2PB / Eqs. 3-18,3-19,3-20,3-21,3-22]
!
         t1     = dble (g * ompsi * dx /&
         &(chz(i  ) * chz(i  ) * r(i  ) * af(i  )))
         t2     = dble (g * psi * dx /&
         &(chz(i+1) * chz(i+1) * r(i+1) * af(i+1)))
         t3     = abs(qit) * qit * t1
         t4     = abs(qi1t) * qi1t * t2
!
         wetlef = .true.
         wetrig = .true.

         if ( lslot ) then
            if ( hit .lt. psltvr(5,i) ) then
               wetlef= .false.
               tm    =  dble (pf * t3 * wf(i) / af(i))
               a(4)  = -ltheta * tm
               ea(4) =  theta1 * tm
               tm    =  dble (&
               &(1.0D0 +  (qit * qit/(qit * qit + 1.0D0)))&
               &* pf * t1 * abs(qit))
               b(4)  =  ltheta * tm
               eb(4) = -theta1 * tm
            endif
            if ( hi1t.lt. psltvr(5,i+1) ) then
               wetrig= .false.
               tm    =  dble (pf * t4 * wf(i+1) / af(i+1))
               c(4)  = -ltheta * tm
               ec(4) =  theta1 * tm
               tm    =  dble (&
               &(1.0D0 + (qi1t * qi1t/(qi1t * qi1t + 1.0D0)))&
               &* pf * t2 * abs(qi1t))
               d(4)  =  ltheta * tm
               ed(4) = -theta1 * tm
            endif
         endif
!
         if (wetlef) then
            tm    =  dble (pf * t3 *&
            &(-wf(i  ) / af(i  )&
            &-2.0D0 / chz(i  ) * dcdh(i  )&
            &-drdh(i  ) / r(i  ) ))
            a(4)  =  ltheta * tm
            ea(4) = -theta1 * tm
!
            tm    =  dble (pf * t1 * abs(qit) * (1.0D0 +&
            &(qit * qit / (qit * qit + 1.0D0))&
            &- qit / chz(i) * dcdq(i)))
            b(4)  =  ltheta * tm
            eb(4) = -theta1 * tm
         endif
!
         if (wetrig) then
            tm    =  dble (pf * t4 *&
            &(-wf(i+1) / af(i+1)&
            &-2.0D0 / chz(i+1) * dcdh(i+1)&
            &-drdh(i+1) / r(i+1) ))
            c(4)  =  ltheta * tm
            ec(4) = -theta1 * tm
!
            tm    =  dble (pf * t2 * abs(qi1t) * (1.0D0 +&
            &(qi1t * qi1t / (qi1t * qi1t + 1.0D0))&
            &- qi1t / chz(i+1) * dcdq(i+1)) )
            d(4)  =  ltheta * tm
            ed(4) = -theta1 * tm
         endif

         f2(k)  = 0.5D0 * ( t3 + t4 )
         ef2(k) = - f2(k)
!
!           5. Wind friction term in momentum equation
!           [ Doc. S-FO-004.2PB / Eq. 3-23]
!
         tm    = dble ( ompsi * dx * pw * dwfdh(i) *&
         &tauwi(i) / rhow)
         a(5)  = -ltheta * tm
         ea(5) =  theta1 * tm
         b(5)  = 0.0D0
         eb(5) = 0.D0
         tm    = dble ( psi * dx * pw * dwfdh(i+1) *&
         &tauwi(i+1) / rhow)
         c(5)  = -ltheta * tm
         ec(5) =  theta1 * tm
         d(5)  = 0.0D0
         ed(5) = 0.D0
         w2(k)  = dble (-0.5D0 * ompsi * dx * wf(i) * tauwi(i)/rhow&
         &-0.5D0 * psi * dx * wf(i+1) * tauwi(i+1)/rhow)
         ew2(k) = -w2(k)
!
!           6. Extra resistance term in momentum equation
!           [ Doc. S-FO-004.2PB / Eq. 3-24]
!
         tq    = dble (0.25 * (q1(i) + q(i) + q1(i+1) + q(i+1)))
         tnr   = abs(tq) * tq
         t1    =  (ompsi * abs(qit) * qit +&
         &psi * abs(qi1t) * qi1t) / tnr
         tm    = dble(g * dx * t1 *&
         &(wf(i) * eta(i) + af(i) * detadh(i)))
         a(6)  =  ltheta * tm
         ea(6) = -theta1 * tm
         b(6)  =  0.0D0
         eb(6) =  0.0D0
         tm    = dble(g * dx * t1 *&
         &(wf(i+1) * eta(i+1) + af(i+1) * detadh(i+1)))
         c(6)  =  ltheta * tm
         ec(6) = -theta1 * tm
         d(6)  = 0.0D0
         ed(6) = 0.0D0
!
!           7. Density term in momentum equation
!           [ Doc. S-FO-004.2PB / Eq. 3-25]
!
!           Remark: Af = dA1m/dh
!
         if (lsalt) then
            tm    =  dble ( g * ompsi * af(i) *&
            &( rho(i+1)-rho(i) ) / rhow )
            a(7)  =  ltheta * tm
            ea(7) = -theta1 * tm
            tm    =  dble ( g * psi * af(i+1) *&
            &( rho(i+1)-rho(i) ) / rhow )
            c(7)  =  ltheta * tm
            ec(7) = -theta1 * tm
         else
            a(7)  = 0.0D0
            ea(7) = 0.0D0
            c(7)  = 0.0D0
            ec(7) = 0.0D0
         endif
         b(7)  = 0.0D0
         d(7)  = 0.0D0
         eb(7) = 0.0D0
         ed(7) = 0.0D0
!
!           Evaluation of coefficients a2,b2,c2,d2 and e2.
!
         a2(k)  = 0.0D0
         b2(k)  = 0.0D0
         c2(k)  = 0.0D0
         d2(k)  = 0.0D0
         ea2(k) = 0.0D0
         eb2(k) = 0.0D0
         ec2(k) = 0.0D0
         ed2(k) = 0.0D0
         do 40 j = 1, 7
            a2(k)  = a2(k) + a(j)
            b2(k)  = b2(k) + b(j)
            c2(k)  = c2(k) + c(j)
            d2(k)  = d2(k) + d(j)
            ea2(k) = ea2(k) + ea(j)
            eb2(k) = eb2(k) + eb(j)
            ec2(k) = ec2(k) + ec(j)
            ed2(k) = ed2(k) + ed(j)
40       continue
!
         ef2(k) = ef2(k) - f2(k)
         ew2(k) = ew2(k) - w2(k)
         em2(k) = 0.D0
         m2(k)  = 0.D0

         f2(k)  = 0.D0
         w2(k)  = 0.D0
         m2(k)  = 0.D0
!
      else
         a2(k)  = 0.0D0
         b2(k)  = 0.0D0
         c2(k)  = 0.0D0
         d2(k)  = 0.0D0
         f2(k)  = 0.D0
         w2(k)  = 0.D0
         m2(k)  = 0.D0
         ea2(k) = 0.0D0
         eb2(k) = 0.0D0
         ec2(k) = 0.0D0
         ed2(k) = 0.0D0
         ef2(k) = 0.D0
         ew2(k) = 0.D0
         em2(k) = 0.D0
      endif
100 continue
   ea1(k+1) = 0.D0
   eb1(k+1) = 0.D0
   ec1(k+1) = 0.D0
   ed1(k+1) = 0.D0
   ea2(k+1) = 0.D0
   eb2(k+1) = 0.D0
   ec2(k+1) = 0.D0
   ed2(k+1) = 0.D0
end
