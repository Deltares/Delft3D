      subroutine KANORM(ngrid  ,ngridm ,nbran  ,ibr    ,i1     ,i2     ,
     +                  g      ,dt1    ,psi    ,theta  ,
     +                  h1     ,q1     ,h      ,q      ,grid   ,x      ,
     +                  wf     ,af     ,
     +                  chz    ,r      ,alfab  ,tauwi  ,eta    ,lsalt  ,
     +                  rho    ,rhow   ,prslot ,dqltdh ,dalfdh ,dcdh   ,
     +                  drdh   ,dcdq   ,dwfdh  ,detadh ,
     +                  scifri ,nnf    ,pfa    ,pw     ,
     +                  a1     ,b1     ,c1     ,d1     ,ea1    ,eb1    ,
     +                  ec1    ,ed1    ,a2     ,b2     ,c2     ,d2     ,
     +                  f2     ,w2     ,m2     ,ea2    ,eb2    ,ec2    ,
     +                  ed2    ,ef2    ,ew2    ,em2    ,psltvr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KANORM (KAlman abcde coefficients 'NORMal' gridpoints)
c
c Module description: Subroutine KANORM computes the matrix coefficients
c                     for 'normal' gridpoints. These coefficients are
c                     A1-D1, EA1-ED1 and A2-F2, EA2-EF2.
c
c                     For normal gridpoints, coefficients for both the
c                     continuity equation and the momentum equation are
c                     determined. For structure grid points only the
c                     coefficients of the continuity equations are deter-
c                     mined. The other coefficients (of the stage
c                     discharge equation) for structure gridpoints are
c                     determined in routine KASTRU.
c                     The coefficients are described in S-FO-004 Par 3.3
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 43 a1(ngridm)        IO A1-coefficient of continuity eq. per gridpnt.
c 51 a2(ngridm)        IO A2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 20 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 25 alfab(ngrid)      I  Actual Bousinessq coefficient in grid point i.
c 44 b1(ngridm)        IO B1-coefficient of continuity eq. per gridpnt.
c 52 b2(ngridm)        IO B2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 45 c1(ngridm)        IO C1-coefficient of continuity eq. per gridpnt.
c 53 c2(ngridm)        IO C2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 23 chz               I  -
c 46 d1(ngridm)        IO D1-coefficient of continuity eq. per gridpnt.
c 54 d2(ngridm)        IO D2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 33 dalfdh(ngrid)     I  Derivative of Bousinesq constant ALFAb to
c                         waterlevel in every grid point i on time n+1/2
c                         (d(ALFAb)/dh).
c 34 dcdh(ngrid)       I  Derivative of Chezy value to waterlevel in every
c                         grid point i on time n+1/2 (dC/dh).
c 36 dcdq(ngrid)       I  Derivative of Chezy value to discharge in every
c                         grid point i on time n+1/2 (dC/dQ).
c 38 detadh(ngrid)     I  Derivative of extra resistance coefficient ETA
c                         to waterlevel in every grid point i+1/2 on time
c                         n+1/2 (d(ETA)/dh). Value at i+1/2 is stored at
c                         index i.
c 32 dqltdh(ngrid)     I  Derivative of lateral discharge to waterlevel in
c                         every grid point i+1/2 on time n+1/2
c                         (d(Qlat)/dh). Value at i+1/2 is stored at index
c                         i.
c 35 drdh(ngrid)       I  Derivative of hydraulic radius to waterlevel in
c                         every grid point i on time n+1/2 (dR/dh).
c  8 dt1               I  -
c 37 dwfdh(ngrid)      I  Derivative of flow width to waterlevel in every
c                         grid point i on time n+1/2 (dWf/dh).
c 47 ea1(ngrid)        IO EA1 right hand side coefficient of continuity
c 58 ea2(ngrid)        IO EA2 right hand side coefficient of momentum
c 48 eb1(ngrid)        IO EB1 right hand side coefficient of continuity
c 59 eb2(ngrid)        IO EB2 right hand side coefficient of momentum
c 49 ec1(ngrid)        IO EC1 right hand side coefficient of continuity
c 60 ec2(ngrid)        IO EC2 right hand side coefficient of momentum
c 50 ed1(ngrid)        IO ED1 right hand side coefficient of continuity
c 61 ed2(ngrid)        IO ED2 right hand side coefficient of momentum
c 62 ef2(ngrid)        IO EF2 right hand side coefficient of momentum
c 64 em2(ngrid)        O  EM2 right hand side coefficient of Q-h relation
c 27 eta(ngrid)        I  Extra resistance coefficient ETA in every grid
c                         point i+1/2 on time n+1/2. Value at i+1/2 is
c                         stored at index i.
c 63 ew2(ngrid)        IO EW2 right hand side coefficient of momentum
c 55 f2(ngrid)         IO F2 coefficient of momentum equation
c  7 g                 I  Acceleration of gravity.
c 17 grid(ngrid)       I  Grid cell type definition:
c                         cgrdcl (1) : Normal grid cell
c                         cstrcl (2) : Structure cell
c 13 h1(ngrid)         I  Water level in every grid point at time t(n).
c 15 h(ngrid)          I  Contains water levels in every grid point. It is
c                         stored on index 2 of the packed array hpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c  5 i1                I  Index of first grid point in actual branch.
c  6 i2                I  Index of last grid point in actual branch.
c  4 ibr               I  -
c 28 lsalt             I  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c 57 m2(ngrid)         O  M2 coefficient of Q-h relation
c  3 nbran             I  Number of branches.
c  1 ngrid             I  Number of grid points in network.
c  2 ngridm            I  Maximum number of gridpoints in a branch.
c 40 nnf               I  Number of uncertain bed friction parameters.
c 41 pfa(nnf)          I  Uncertain bed friction parameters of all
c 31 prslot(3,nbran)   I  Contains information concerning Preissmann
c                         slot (assuring positive water depths).
c                         (1,i) = Create slot indicator
c                                 csldis (0) : No slot in this branch.
c                                 cslena (1) : Create slot in branch.
c                         (2,i) = Reference area for slot generation.
c                         (3,i) = Depth of slot below lowest bottom of
c                                 branch.
c 63 psltvr            I  Preissmann slot variables for every grid point
c                         i (assuring positive water depths):
c                         (1,i) = Value for C**2*R for positive flow.
c                         (2,i) = Value for C**2*R for negative flow.
c                         (3,i) = Bottom of slot (funnel)
c                         (4,i) = Division level between trapezium and
c                                 rectangle of slot (top of rectangle
c                                 and bottom of trapezium)
c                         (5,i) = Top of slot
c                         (6,i) = Bottom width of slot (width of
c                                 rectangle)
c                         (7,i) = Top width of slot (top of trapezium)
c  9 psi               I  Space weight factor in Preissmann scheme.
c 42 pw                I  Uncertain wind stress parameter.
c 16 q(ngrid)          I  Contains discharges in every grid point. It is
c                         stored on index 2 of the packed array qpack.
c                         Flow:        current values during iteration
c                         (h*).
c                         Prediction:  last iterated value.
c                         Update:      filtered value (n+1|n+1)
c 14 q1(ngrid)         I  Discharge in every grid point at time t(n).
c 24 r(ngrid)          I  Actual hydraulic radius for total channel in
c                         every grid point.
c 29 rho(ngrid)        I  Density of diluted water per grid point.
c 30 rhow              I  Density of fresh water.
c 39 scifri(ngrid)     I  Contains the number of the uncorrelated r.n.
c                         process for bed friction (group nr. or correc-
c                         tion parameter nr.) of every normal grid cell,
c                         otherwise zero.
c 26 tauwi(ngrid)      I  Calculated wind friction for each gridpoint.
c 10 theta             I  Time weight factor in Preissmann scheme.
c 56 w2(ngrid)         IO W2 coefficient of momentum equation
c 56 w2(ngrid)         IO Total width at (n+theta2) in every grid point.
c 19 wf(ngrid)         I  Actual flow width at every grid point.
c 18 x(ngrid)          I  x(i) = X-coordinate of grid point i.
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kanorm.pf,v $
c Revision 1.11  1999/03/15  15:52:01  kuipe_j
c tabs removed
c
c Revision 1.10  1997/06/17  11:23:47  kuipe_j
c Initialize vars
c
c Revision 1.9  1997/06/04  11:18:16  kuipe_j
c Initialize arrays
c
c Revision 1.8  1997/02/17  10:22:36  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.7  1997/01/23  08:29:40  kuipe_j
c Make flow module robust
c
c Revision 1.6  1996/12/20  15:46:42  kuipe_j
c error in friction corr. par
c
c Revision 1.5  1996/12/05  10:00:04  kuipe_j
c Smoothing kgain,linearization,limit covariance,etc
c
c Revision 1.4  1996/12/02  10:04:58  kuipe_j
c avoid negative pointers
c
c Revision 1.3  1996/09/03  14:54:25  kuipe_j
c frequency time hist,etc
c
c Revision 1.2  1996/04/12  13:05:11  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:44  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
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
c
      double precision dt1
      double precision h(ngrid), h1(ngrid), q1(ngrid), q(ngrid)
      double precision a1(ngridm), ea1(ngridm),
     +                 b1(ngridm), eb1(ngridm),
     +                 c1(ngridm), ec1(ngridm),
     +                 d1(ngridm), ed1(ngridm)
      double precision a2(ngridm), ea2(ngridm),
     +                 b2(ngridm), eb2(ngridm),
     +                 c2(ngridm), ec2(ngridm),
     +                 d2(ngridm), ed2(ngridm),
     +                 f2(ngridm), ef2(ngridm),
     +                 w2(ngridm), ew2(ngridm),
     +                 m2(ngridm), em2(ngridm)
c
c     Declaration of local variables
c
      integer          i, j, k,  idx
      logical          lslot, wetlef, wetrig
      real             pf
      double precision a(7), b(7), c(7), d(7)
      double precision ea(7), eb(7), ec(7), ed(7)
      double precision ompsi, dxodt, dt2, dx, ltheta
      double precision ta, tb, tq, tnr, t1, t2, t3, t4
      double precision tm, hit, hi1t, qit, qi1t, theta1
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      dt2    = dble (dt1)
      ltheta = dble (theta)
c
c     Calculate 1 - psi
c
      ompsi  = dble (1.0 - psi)
      theta1 = 1.0D0 - ltheta
c
c     Preissmann slot active in branch ?
c
      lslot = int(prslot(1,ibr)) .eq. cslena
c
c     Loop over grid points in branch ibr
c
      k = 0
      do 100 i = i1, i2-1
         dx   = dble (x(i+1) - x(i))
         hit  = ltheta*h(i)   + theta1*h1(i)
         hi1t = ltheta*h(i+1) + theta1*h1(i+1)
         qit  = ltheta*q(i)   + theta1*q1(i)
         qi1t = ltheta*q(i+1) + theta1*q1(i+1)
c
c        Calculate dx / dt
c
         dxodt = dx / dt2
c
         k  = k + 1
c
c********************************
c        Continuity equation *
c********************************
c
c        1. First term continuity equation
c        [ Doc. S-FO-004.2PB / Eq. 3-9]
c
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
c
c        2. Second term continuity equation
c        [ Doc. S-FO-004.2PB / Eq. 3-10]
c
         a(2)  = 0.0D0
         b(2)  = -ltheta
         c(2)  = 0.0D0
         d(2)  = ltheta
         ea(2) = 0.0D0
         eb(2) = 1.0D0 - ltheta
         ec(2) = 0.0D0
         ed(2) = - (1.0D0 - ltheta)
c
c
c        3. Last term continuity equation
c        [ Doc. S-FO-004.2PB / Eq. 3-11]
c
         a(3)  = dble (-0.25 * dqltdh(i))
         b(3)  = 0.0D0
         c(3)  = a(3)
         d(3)  = 0.0D0
         ea(3) = -a(3)
         eb(3) = 0.0D0
         ec(3) = -a(3)
         ed(3) = 0.0D0
c
c        Evaluation of coefficients a1,b1,c1,d1 and ea1,eb1,ec1,ed1
c
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
c
c        Normal grid cell
c
         if (grid(i) .eq. cgrdcl) then
            idx= scifri(i)
            if (idx.eq.0) then
               pf =  1.
            else
               pf = pfa(idx)
            endif                
c
c***********************************
c           Momentum equation      *
c***********************************
c
c           Normal gridpoint
c
c           1. Acceleration term momentum equation
c           [ Doc. S-FO-004.2PB / Eq. 3-15]
c
            a(1)  = 0.0D0
            b(1)  = dble (ompsi * dxodt)
            c(1)  = 0.0D0
            d(1)  = dble (psi * dxodt)
            ea(1) = 0.0D0
            eb(1) = b(1)
            ec(1) = 0.0D0
            ed(1) = d(1)
c
c           2. Convective term momentum equation
c           [ Doc. S-FO-004.2PB / Eq. 3-16]
c
            ta    = dble ( qit**2  / af(i))
            tb    = dble ( qi1t**2 / af(i+1))
c
            t1    = dble (ta * alfab(i) / af(i) * wf(i))
            t2    = dble (ta * dalfdh(i))
            t3    = dble (tb * alfab(i+1) / af(i+1) * wf(i+1))
            t4    = dble (tb * dalfdh(i+1))
c
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
c
c           3. Hydrostatic pressure term in momentum equation
c           [ Doc. S-FO-004.2PB / Eq. 3-17]
c
            t1    = dble (0.5D0 * g * (af(i) + af(i+1)))
            t2    = dble (0.5D0 * g * (wf(i) + wf(i+1))) * (hi1t - hit)
c
            a(3)  =  ltheta * (t2 - t1)
            b(3)  =  0.0D0
            c(3)  =  ltheta * (t1 + t2)
            d(3)  =  0.0D0
            ea(3) =  theta1 * (t1 - t2)
            eb(3) =  0.0D0
            ec(3) = -theta1 * (t1 + t2)
            ed(3) =  0.0D0
c
c           4. Bottom friction term in momentum equation
c           [ Doc. S-FO-004.2PB / Eqs. 3-18,3-19,3-20,3-21,3-22]
c
            t1     = dble (g * ompsi * dx /
     +                    (chz(i  ) * chz(i  ) * r(i  ) * af(i  )))
            t2     = dble (g * psi * dx /
     +                    (chz(i+1) * chz(i+1) * r(i+1) * af(i+1)))
            t3     = abs(qit) * qit * t1
            t4     = abs(qi1t) * qi1t * t2
c
            wetlef = .true.
            wetrig = .true.

            if ( lslot ) then
               if ( hit .lt. psltvr(5,i) ) then
                  wetlef= .false.
                  tm    =  dble (pf * t3 * wf(i) / af(i))
                  a(4)  = -ltheta * tm
                  ea(4) =  theta1 * tm
                  tm    =  dble (
     +                     (1.0D0 +  (qit * qit/(qit * qit + 1.0D0)))
     +                     * pf * t1 * abs(qit))
                  b(4)  =  ltheta * tm
                  eb(4) = -theta1 * tm
               endif
               if ( hi1t.lt. psltvr(5,i+1) ) then
                  wetrig= .false.
                  tm    =  dble (pf * t4 * wf(i+1) / af(i+1))
                  c(4)  = -ltheta * tm
                  ec(4) =  theta1 * tm
                  tm    =  dble (
     +                     (1.0D0 + (qi1t * qi1t/(qi1t * qi1t + 1.0D0)))
     +                     * pf * t2 * abs(qi1t))
                  d(4)  =  ltheta * tm
                  ed(4) = -theta1 * tm
               endif
            endif
c
            if (wetlef) then
               tm    =  dble (pf * t3 *
     +                       (-wf(i  ) / af(i  )
     +                        -2.0D0 / chz(i  ) * dcdh(i  )
     +                        -drdh(i  ) / r(i  ) ))
               a(4)  =  ltheta * tm
               ea(4) = -theta1 * tm
c
               tm    =  dble (pf * t1 * abs(qit) * (1.0D0 +
     +                       (qit * qit / (qit * qit + 1.0D0))
     +                       - qit / chz(i) * dcdq(i)))
               b(4)  =  ltheta * tm
               eb(4) = -theta1 * tm
            endif
c
            if (wetrig) then
               tm    =  dble (pf * t4 *
     +                       (-wf(i+1) / af(i+1)
     +                        -2.0D0 / chz(i+1) * dcdh(i+1)
     +                        -drdh(i+1) / r(i+1) ))
               c(4)  =  ltheta * tm
               ec(4) = -theta1 * tm
c
               tm    =  dble (pf * t2 * abs(qi1t) * (1.0D0 +
     +                       (qi1t * qi1t / (qi1t * qi1t + 1.0D0))
     +                       - qi1t / chz(i+1) * dcdq(i+1)) )
               d(4)  =  ltheta * tm
               ed(4) = -theta1 * tm
            endif

            f2(k)  = 0.5D0 * ( t3 + t4 )
            ef2(k) = - f2(k)
c
c           5. Wind friction term in momentum equation
c           [ Doc. S-FO-004.2PB / Eq. 3-23]
c
            tm    = dble ( ompsi * dx * pw * dwfdh(i) *
     +                     tauwi(i) / rhow)
            a(5)  = -ltheta * tm
            ea(5) =  theta1 * tm
            b(5)  = 0.0D0
            eb(5) = 0.D0
            tm    = dble ( psi * dx * pw * dwfdh(i+1) *
     +                     tauwi(i+1) / rhow)
            c(5)  = -ltheta * tm
            ec(5) =  theta1 * tm
            d(5)  = 0.0D0
            ed(5) = 0.D0
            w2(k)  = dble (-0.5D0 * ompsi * dx * wf(i) * tauwi(i)/rhow
     +                    -0.5D0 * psi * dx * wf(i+1) * tauwi(i+1)/rhow)
            ew2(k) = -w2(k)
c
c           6. Extra resistance term in momentum equation
c           [ Doc. S-FO-004.2PB / Eq. 3-24]
c
            tq    = dble (0.25 * (q1(i) + q(i) + q1(i+1) + q(i+1)))
            tnr   = abs(tq) * tq
            t1    =  (ompsi * abs(qit) * qit +
     +                    psi * abs(qi1t) * qi1t) / tnr
            tm    = dble(g * dx * t1 *
     +                   (wf(i) * eta(i) + af(i) * detadh(i)))
            a(6)  =  ltheta * tm
            ea(6) = -theta1 * tm
            b(6)  =  0.0D0
            eb(6) =  0.0D0
            tm    = dble(g * dx * t1 *
     +                  (wf(i+1) * eta(i+1) + af(i+1) * detadh(i+1)))
            c(6)  =  ltheta * tm
            ec(6) = -theta1 * tm
            d(6)  = 0.0D0
            ed(6) = 0.0D0
c
c           7. Density term in momentum equation
c           [ Doc. S-FO-004.2PB / Eq. 3-25]
c
c           Remark: Af = dA1m/dh
c
            if (lsalt) then
               tm    =  dble ( g * ompsi * af(i) *
     +                       ( rho(i+1)-rho(i) ) / rhow )
               a(7)  =  ltheta * tm
               ea(7) = -theta1 * tm
               tm    =  dble ( g * psi * af(i+1) *
     +                       ( rho(i+1)-rho(i) ) / rhow )
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
c
c           Evaluation of coefficients a2,b2,c2,d2 and e2.
c
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
c
            ef2(k) = ef2(k) - f2(k)
            ew2(k) = ew2(k) - w2(k)
            em2(k) = 0.D0
            m2(k)  = 0.D0

            f2(k)  = 0.D0
            w2(k)  = 0.D0
            m2(k)  = 0.D0
c
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
