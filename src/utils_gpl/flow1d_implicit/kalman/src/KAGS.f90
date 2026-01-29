subroutine KAGS (g      ,il     ,ir     ,ngrid  ,istru  ,lambda ,&
&dhstru ,strclo ,strpar ,h2     ,h1     ,q2     ,&
&af2    ,wf2    ,lsalt  ,rho    ,pmu    ,a2     ,&
&b2     ,c2     ,d2     ,m2     ,ea2    ,eb2    ,&
&ec2    ,ed2    ,em2    ,sign   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAGS (KAlman General Structure)
!
! Module description: In subroutine KAGS the ABCDE coefficients are
!                     computed for a general structure.
!
!                     The coefficients are described in S-FO-004 Par 7.4
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 18 a2(ngridm)        O  A2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 13 af2(ngrid)        I  Flow area in every grid point i on time n+1.
!                         Only values at grid points around structures are
!                         defined.
! 19 b2(ngridm)        O  B2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 20 c2(ngridm)        O  C2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 21 d2(ngridm)        O  D2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
!  7 dhstru            I  Delta h used for numerical differentation.
! 23 ea2(ngrid)        O  EA2 right hand side coefficient of momentum
! 24 eb2(ngrid)        O  EB2 right hand side coefficient of momentum
! 25 ec2(ngrid)        O  EC2 right hand side coefficient of momentum
! 26 ed2(ngrid)        O  ED2 right hand side coefficient of momentum
! 27 em2(ngrid)        O  EM2 right hand side coefficient of Q-h relation
!  1 g                 I  Acceleration of gravity.
! 11 h1                P  -
! 10 h2                P  -
!  2 il                P  -
!  3 ir                P  -
!  5 istru             I  Number of structure.
!  6 lambda            I  Extra resistance in general structure.
! 15 lsalt             I  Logical indicator for salt computation
!                         = .true.  : with salt computation
!                         = .false. : no salt computation
! 22 m2(ngrid)         O  M2 coefficient of Q-h relation
!  4 ngrid             I  Number of grid points in network.
! 17 pmu               I  Uncertain energy loss parameters in case of
! 12 q2                P  -
! 16 rho(ngrid)        I  Density of diluted water per grid point.
! 28 sign              IO Flow direction (+/-).
!  8 strclo(nstru)     I  True if structure is closed.
!  9 strpar(21,nstru)  I  Each structure is characterized by a number of
!                         specific parameters. strpar (i,j) = parameter
!                         i of structure j:
!                         - Simple weir:
!                         (1,j) = Crest height Zs.
!                         (2,j) = Crest width Ws.
!                              Positive flow:
!                         (3,j) = Correction coefficient cw.
!                         (4,j) = Submergence limit Slim.
!                         (5,j) = Table pointer for drowned reduction
!                                 curve f(h2/h1).
!                              Negative flow:
!                         (6,j) = Correction coefficient cw.
!                         (7,j) = Submergence limit Slim.
!                         (8,j) = Table pointer for drowned reduction
!                                 curve f(h2/h1).
!                         - Advanced weir:
!                         (1,j) = Crest height Zs.
!                         (2,j) = Total net width Wn.
!                         (3,j) = Number of piers N.
!                              Positive flow:
!                         (4,j) = Heigth of upstream face P.
!                         (5,j) = Design head H0 of the weir.
!                         (6,j) = Pier contraction coefficient Kp.
!                         (7,j) = Abutment contraction coefficient Ka.
!                              Negative flow:
!                         (8,j) = Heigth of upstream face P.
!                         (9,j) = Design head H0 of the weir.
!                         (10,j)= Pier contraction coefficient Kp.
!                         (11,j)= Abutment contraction coefficient Ka.
!                         - Pump:
!                         (1,j) = Control direction:
!                                 cpmpup (-1) : upward control
!                                 cpmpdw (+1) : downward control
!                         (2,j) = Table pointer for pump capacitity re-
!                                 duction factor.
!                         (3,j) = Capacity.
!                         (4,j) = Water level which starts pump.
!                         (5,j) = Water level which stops pump.
!                         - General structure:
!                         (1,j) = Width left side of structure W1.
!                         (2,j) = Bed level left side of structure Zb1.
!                         (3,j) = Width structure left side Wsdl.
!                         (4,j) = Bed left side of structure Zbsl.
!                         (5,j) = Width structure centre Ws.
!                         (6,j) = Bed level centre Zs.
!                         (7,j) = Width structure right side Wsdr.
!                         (8,j) = Bed right side of structure Zbsr.
!                         (9,j) = Width right side of structure W2.
!                         (10,j)= Bed level right side of structure Zb2.
!                         (11,j)= Gate opening heigth dg.
!                              Positive flow:
!                         (12,j)= Correction coefficient for free gate
!                                 flow cgf.
!                         (13,j)= Correction coefficient for drowned
!                                 gate flow cgd.
!                         (14,j)= Correction coefficient for free weir
!                                 flow cwf.
!                         (15,j)= Correction coefficient for drowned
!                                 weir flow cwd.
!                         (16,j)= Contraction coefficient for free gate
!                                 flow MU-gf.
!                              Negative flow:
!                         (17,j)= Correction coefficient for free gate
!                                 flow cgf.
!                         (18,j)= Correction coefficient for drowned
!                                 gate flow cgd.
!                         (19,j)= Correction coefficient for free weir
!                                 flow cwf.
!                         (20,j)= Correction coefficient for drowned
!                                 weir flow cwd.
!                         (21,j)= Contraction coefficient for free gate
!                                 flow MU-gf.
! 14 wf2(ngrid)        I  Flow width in every grid point i on time n+1.
!                         Only values at grid points around structures are
!                         defined.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flgtar  FLow get General sTructure ARguments
! flupdg  FLow UP/Downstream near General structure
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kags.pf,v $
! Revision 1.6  1999/03/15  15:51:52  kuipe_j
! tabs removed
!
! Revision 1.5  1997/10/03  06:40:11  kuipe_j
! criterium for flow drection changed
!
! Revision 1.4  1996/12/05  10:00:01  kuipe_j
! Smoothing kgain,linearization,limit covariance,etc
!
! Revision 1.3  1996/05/28  13:31:50  kuipe_j
! Smoothing  added
!
! Revision 1.2  1996/04/12  13:04:58  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:35  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of function:
!
   double precision FLQHGS
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer il, ir, istru, ngrid
   logical lsalt, strclo(*)
   real    strpar(dmstrpar,*), rho(ngrid)
   real    af2(ngrid), wf2(ngrid)
   real    g, pmu, a2, b2, c2, d2, m2, ea2, eb2, ec2, ed2, em2, sign
   real    lambda ,dhstru
   double precision h2(ngrid), h1(ngrid), q2(ngrid)
!
!     Declaration of local variables:
!
   integer iup, idown, formno, idum
   logical strsta

   double  precision  tnr, tnrm, t1, t2, mugf
   double  precision  ds, dsi, dsip1, dsdhi, dsdhp1
   double  precision  hunp1 ,hdnp1 ,hun  ,hdn ,uu    ,ud  ,qstr,qdum,&
   &wstr  ,zs    ,w2   ,zb2 ,dg    ,&
   &cgf   ,cgd   ,cwf  ,cwd ,mugfcr,wsd ,ds1 ,ds2 ,&
   &rhoast,crest ,qun  ,qunp1 ,wu  ,au  ,&
   &gdub  ,lamdub,dhsdub    ,dteken
!
   gdub   = g
   lamdub = lambda
   dhsdub = dhstru
   crest  = strpar(6 ,istru)
!
   call FLUPDG (gdub   ,il     ,ir     ,ngrid  ,h2     ,h1     ,&
   &q2     ,q2     ,q2     ,af2    ,wf2    ,rho    ,&
   &crest  ,hunp1  ,hdnp1  ,hun    ,hdn    ,uu     ,&
   &ud     ,qun    ,qunp1  ,au     ,wu     ,iup    ,&
   &idown  ,dteken )
   sign = sngl(dteken)

   call FLGTAR (istru  ,strpar ,dteken ,zs     ,wstr   ,&
   &w2     ,wsd    ,zb2    ,dg     ,ds1    ,&
   &ds2    ,cgf    ,cgd    ,cwf    ,cwd    ,&
   &mugfcr )
!
   mugf = mugfcr / pmu
!
!     Set default to zero
!
   a2  = 0.
   b2  = 0.
   c2  = 0.
   d2  = 0.
   m2  = 0.
   ea2 = 0.
   eb2 = 0.
   ec2 = 0.
   ed2 = 0.
   em2 = 0.
!
   if ( strclo(istru) ) then
      b2  = 1.
   else
      if ( lsalt ) then
         rhoast = rho(idown) / rho(iup)
      else
         rhoast = 1.0D0
      endif
!
      strsta = .false.
      qstr = sign * FLQHGS (gdub   ,istru  ,strsta ,strclo ,hunp1  ,&
      &hdnp1  ,uu     ,zs     ,wstr   ,w2     ,&
      &wsd    ,zb2    ,dg     ,ds1    ,ds2    ,&
      &rhoast ,cgf    ,cgd    ,cwf    ,cwd    ,&
      &mugfcr ,ds     ,formno ,lamdub ,dhsdub )
!
      if ( formno .eq. 4 .or. formno .eq. 2 ) then
!
         strsta = .false.
         qdum = sign * FLQHGS (gdub   ,istru  ,strsta ,strclo ,&
         &hunp1-dhsdub   ,hdnp1  ,uu     ,&
         &zs     ,wstr   ,w2     ,wsd    ,&
         &zb2    ,dg     ,ds1    ,ds2    ,&
         &rhoast ,cgf    ,cgd    ,cwf    ,&
         &cwd    ,mugfcr ,dsi    ,idum   ,&
         &lamdub ,dhsdub )
         dsdhi = ( dsi - ds ) / dhsdub
         strsta = .false.
         qdum = sign * FLQHGS (gdub   ,istru  ,strsta ,strclo ,&
         &hunp1  ,hdnp1+dhsdub   ,uu     ,&
         &zs     ,wstr   ,w2     ,wsd    ,&
         &zb2    ,dg     ,ds1    ,ds2    ,&
         &rhoast ,cgf    ,cgd    ,cwf    ,&
         &cwd    ,mugfcr ,dsip1  ,idum   ,&
         &lamdub ,dhsdub )
         dsdhp1 = ( dsip1 - ds ) / dhsdub
      endif
!
      tnrm = hunp1 + qstr**2 / (2.*g*af2(iup)**2) - zs
!
      if ( formno .eq. 3 ) then
!
!           - free gate flow -

!
         tnr  = sqrt(2. * g * abs(dhstru + tnrm - mugf*dg))
         t1   = mugf * cgf * wstr * dg / tnr
!
         a2  = sngl(-pmu*t1*(g - qstr*qstr*wf2(iup)/af2(iup)**3))
!
         b2  = sngl(1.D0 - pmu*t1*qstr / (af2(iup)*af2(iup)))
!
         m2  = sngl(- mugf * cgf * wstr * dg&
         &* (tnr - sqrt(2. * g * dhstru)))
!
      else if ( formno .eq. 4 ) then
!
!           - drowned gate flow -
!

         t1  = sqrt (2. * g * abs( dhstru + tnrm - ds))
         t2  = pmu * mugf * cgd * wstr * dg * g / t1
!
         a2  = sngl(-t2 * (1.D0 - qstr*qstr*wf2(iup)/af2(iup)**3/g -&
         &dsdhi ))
!
         b2  = sngl(1.D0 - t2 * qstr / (g*af2(iup)*af2(iup)))
!
         c2  = sngl(t2 * dsdhp1)
!
         m2  = sngl(-mugf * cgd * wstr * dg&
         &* (t1 - sqrt(2. * g * dhstru)))
!
      else if ( formno .eq. 1 ) then
!
!           - free weir flow -
!

         t1  = cwf * wstr * sqrt(2./3. * g * tnrm)
!
         a2  = sngl(-t1 * (1.D0 - qstr * qstr * wf2(iup) /&
         &af2(iup)**3 / g))
!
         b2  = sngl(1.D0 -t1 * qstr / ( g * af2(iup) * af2(iup) ))
!
      else if ( formno .eq. 2 ) then
!
!           - drowned weir flow -
!
         t1  = sqrt (2. * g * abs( dhstru + tnrm - ds))
         t2  = cwd * wstr * g / t1
!
         a2  = sngl(-t2 * ds * (1.D0 - qstr*qstr*wf2(iup)/af2(iup)&
         &**3/g - dsdhi) - cwd * wstr * t1 * dsdhi)
!
         b2  = sngl(1.D0 - t2 * ds * qstr / (g*af2(iup)*af2(iup)))
!
         c2  = sngl(t2 * ds * dsdhp1 - cwd * wstr * t1 * dsdhp1)
!
      endif
   endif
!
end
