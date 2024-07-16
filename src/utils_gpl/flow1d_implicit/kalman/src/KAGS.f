      subroutine KAGS (g      ,il     ,ir     ,ngrid  ,istru  ,lambda ,
     +                 dhstru ,strclo ,strpar ,h2     ,h1     ,q2     ,
     +                 af2    ,wf2    ,lsalt  ,rho    ,pmu    ,a2     ,
     +                 b2     ,c2     ,d2     ,m2     ,ea2    ,eb2    ,
     +                 ec2    ,ed2    ,em2    ,sign   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAGS (KAlman General Structure)
c
c Module description: In subroutine KAGS the ABCDE coefficients are
c                     computed for a general structure.
c
c                     The coefficients are described in S-FO-004 Par 7.4
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 18 a2(ngridm)        O  A2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 13 af2(ngrid)        I  Flow area in every grid point i on time n+1.
c                         Only values at grid points around structures are
c                         defined.
c 19 b2(ngridm)        O  B2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 20 c2(ngridm)        O  C2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 21 d2(ngridm)        O  D2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c  7 dhstru            I  Delta h used for numerical differentation.
c 23 ea2(ngrid)        O  EA2 right hand side coefficient of momentum
c 24 eb2(ngrid)        O  EB2 right hand side coefficient of momentum
c 25 ec2(ngrid)        O  EC2 right hand side coefficient of momentum
c 26 ed2(ngrid)        O  ED2 right hand side coefficient of momentum
c 27 em2(ngrid)        O  EM2 right hand side coefficient of Q-h relation
c  1 g                 I  Acceleration of gravity.
c 11 h1                P  -
c 10 h2                P  -
c  2 il                P  -
c  3 ir                P  -
c  5 istru             I  Number of structure.
c  6 lambda            I  Extra resistance in general structure.
c 15 lsalt             I  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c 22 m2(ngrid)         O  M2 coefficient of Q-h relation
c  4 ngrid             I  Number of grid points in network.
c 17 pmu               I  Uncertain energy loss parameters in case of
c 12 q2                P  -
c 16 rho(ngrid)        I  Density of diluted water per grid point.
c 28 sign              IO Flow direction (+/-).
c  8 strclo(nstru)     I  True if structure is closed.
c  9 strpar(21,nstru)  I  Each structure is characterized by a number of
c                         specific parameters. strpar (i,j) = parameter
c                         i of structure j:
c                         - Simple weir:
c                         (1,j) = Crest height Zs.
c                         (2,j) = Crest width Ws.
c                              Positive flow:
c                         (3,j) = Correction coefficient cw.
c                         (4,j) = Submergence limit Slim.
c                         (5,j) = Table pointer for drowned reduction
c                                 curve f(h2/h1).
c                              Negative flow:
c                         (6,j) = Correction coefficient cw.
c                         (7,j) = Submergence limit Slim.
c                         (8,j) = Table pointer for drowned reduction
c                                 curve f(h2/h1).
c                         - Advanced weir:
c                         (1,j) = Crest height Zs.
c                         (2,j) = Total net width Wn.
c                         (3,j) = Number of piers N.
c                              Positive flow:
c                         (4,j) = Heigth of upstream face P.
c                         (5,j) = Design head H0 of the weir.
c                         (6,j) = Pier contraction coefficient Kp.
c                         (7,j) = Abutment contraction coefficient Ka.
c                              Negative flow:
c                         (8,j) = Heigth of upstream face P.
c                         (9,j) = Design head H0 of the weir.
c                         (10,j)= Pier contraction coefficient Kp.
c                         (11,j)= Abutment contraction coefficient Ka.
c                         - Pump:
c                         (1,j) = Control direction:
c                                 cpmpup (-1) : upward control
c                                 cpmpdw (+1) : downward control
c                         (2,j) = Table pointer for pump capacitity re-
c                                 duction factor.
c                         (3,j) = Capacity.
c                         (4,j) = Water level which starts pump.
c                         (5,j) = Water level which stops pump.
c                         - General structure:
c                         (1,j) = Width left side of structure W1.
c                         (2,j) = Bed level left side of structure Zb1.
c                         (3,j) = Width structure left side Wsdl.
c                         (4,j) = Bed left side of structure Zbsl.
c                         (5,j) = Width structure centre Ws.
c                         (6,j) = Bed level centre Zs.
c                         (7,j) = Width structure right side Wsdr.
c                         (8,j) = Bed right side of structure Zbsr.
c                         (9,j) = Width right side of structure W2.
c                         (10,j)= Bed level right side of structure Zb2.
c                         (11,j)= Gate opening heigth dg.
c                              Positive flow:
c                         (12,j)= Correction coefficient for free gate
c                                 flow cgf.
c                         (13,j)= Correction coefficient for drowned
c                                 gate flow cgd.
c                         (14,j)= Correction coefficient for free weir
c                                 flow cwf.
c                         (15,j)= Correction coefficient for drowned
c                                 weir flow cwd.
c                         (16,j)= Contraction coefficient for free gate
c                                 flow MU-gf.
c                              Negative flow:
c                         (17,j)= Correction coefficient for free gate
c                                 flow cgf.
c                         (18,j)= Correction coefficient for drowned
c                                 gate flow cgd.
c                         (19,j)= Correction coefficient for free weir
c                                 flow cwf.
c                         (20,j)= Correction coefficient for drowned
c                                 weir flow cwd.
c                         (21,j)= Contraction coefficient for free gate
c                                 flow MU-gf.
c 14 wf2(ngrid)        I  Flow width in every grid point i on time n+1.
c                         Only values at grid points around structures are
c                         defined.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flgtar  FLow get General sTructure ARguments
c flupdg  FLow UP/Downstream near General structure
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kags.pf,v $
c Revision 1.6  1999/03/15  15:51:52  kuipe_j
c tabs removed
c
c Revision 1.5  1997/10/03  06:40:11  kuipe_j
c criterium for flow drection changed
c
c Revision 1.4  1996/12/05  10:00:01  kuipe_j
c Smoothing kgain,linearization,limit covariance,etc
c
c Revision 1.3  1996/05/28  13:31:50  kuipe_j
c Smoothing  added
c
c Revision 1.2  1996/04/12  13:04:58  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:35  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of function:
c
      double precision FLQHGS
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters:
c
      integer il, ir, istru, ngrid
      logical lsalt, strclo(*)
      real    strpar(dmstrpar,*), rho(ngrid)
      real    af2(ngrid), wf2(ngrid)
      real    g, pmu, a2, b2, c2, d2, m2, ea2, eb2, ec2, ed2, em2, sign
      real    lambda ,dhstru
      double precision h2(ngrid), h1(ngrid), q2(ngrid)
c
c     Declaration of local variables:
c
      integer iup, idown, formno, idum
      logical strsta
        
      double  precision  tnr, tnrm, t1, t2, mugf
      double  precision  ds, dsi, dsip1, dsdhi, dsdhp1
      double  precision  hunp1 ,hdnp1 ,hun  ,hdn ,uu    ,ud  ,qstr,qdum,
     +                   wstr  ,zs    ,w2   ,zb2 ,dg    ,
     +                   cgf   ,cgd   ,cwf  ,cwd ,mugfcr,wsd ,ds1 ,ds2 ,
     +                   rhoast,crest ,qun  ,qunp1 ,wu  ,au  ,
     +                   gdub  ,lamdub,dhsdub    ,dteken
c
      gdub   = g
      lamdub = lambda
      dhsdub = dhstru
      crest  = strpar(6 ,istru)
c
      call FLUPDG (gdub   ,il     ,ir     ,ngrid  ,h2     ,h1     ,
     +             q2     ,q2     ,q2     ,af2    ,wf2    ,rho    ,
     +             crest  ,hunp1  ,hdnp1  ,hun    ,hdn    ,uu     ,
     +             ud     ,qun    ,qunp1  ,au     ,wu     ,iup    ,
     +             idown  ,dteken )
      sign = sngl(dteken)

      call FLGTAR (istru  ,strpar ,dteken ,zs     ,wstr   ,
     +             w2     ,wsd    ,zb2    ,dg     ,ds1    ,
     +             ds2    ,cgf    ,cgd    ,cwf    ,cwd    ,
     +             mugfcr )
c
      mugf = mugfcr / pmu
c
c     Set default to zero
c
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
c
      if ( strclo(istru) ) then
         b2  = 1.
      else
         if ( lsalt ) then
            rhoast = rho(idown) / rho(iup)
         else
            rhoast = 1.0D0
         endif
c
         strsta = .false.
         qstr = sign * FLQHGS (gdub   ,istru  ,strsta ,strclo ,hunp1  ,
     +                         hdnp1  ,uu     ,zs     ,wstr   ,w2     ,
     +                         wsd    ,zb2    ,dg     ,ds1    ,ds2    ,
     +                         rhoast ,cgf    ,cgd    ,cwf    ,cwd    ,
     +                         mugfcr ,ds     ,formno ,lamdub ,dhsdub )
c
         if ( formno .eq. 4 .or. formno .eq. 2 ) then
c
            strsta = .false.
            qdum = sign * FLQHGS (gdub   ,istru  ,strsta ,strclo ,
     +                            hunp1-dhsdub   ,hdnp1  ,uu     ,
     +                            zs     ,wstr   ,w2     ,wsd    ,
     +                            zb2    ,dg     ,ds1    ,ds2    ,
     +                            rhoast ,cgf    ,cgd    ,cwf    ,
     +                            cwd    ,mugfcr ,dsi    ,idum   ,
     +                            lamdub ,dhsdub )
            dsdhi = ( dsi - ds ) / dhsdub
            strsta = .false.
            qdum = sign * FLQHGS (gdub   ,istru  ,strsta ,strclo ,
     +                            hunp1  ,hdnp1+dhsdub   ,uu     ,
     +                            zs     ,wstr   ,w2     ,wsd    ,
     +                            zb2    ,dg     ,ds1    ,ds2    ,
     +                            rhoast ,cgf    ,cgd    ,cwf    ,
     +                            cwd    ,mugfcr ,dsip1  ,idum   ,
     +                            lamdub ,dhsdub )
            dsdhp1 = ( dsip1 - ds ) / dhsdub
         endif
c
         tnrm = hunp1 + qstr**2 / (2.*g*af2(iup)**2) - zs
c
         if ( formno .eq. 3 ) then
c
c           - free gate flow -

c
            tnr  = sqrt(2. * g * abs(dhstru + tnrm - mugf*dg))
            t1   = mugf * cgf * wstr * dg / tnr
c
            a2  = sngl(-pmu*t1*(g - qstr*qstr*wf2(iup)/af2(iup)**3))
c
            b2  = sngl(1.D0 - pmu*t1*qstr / (af2(iup)*af2(iup)))
c
            m2  = sngl(- mugf * cgf * wstr * dg 
     +            * (tnr - sqrt(2. * g * dhstru)))
c
         else if ( formno .eq. 4 ) then
c
c           - drowned gate flow -
c

            t1  = sqrt (2. * g * abs( dhstru + tnrm - ds))
            t2  = pmu * mugf * cgd * wstr * dg * g / t1
c
            a2  = sngl(-t2 * (1.D0 - qstr*qstr*wf2(iup)/af2(iup)**3/g -
     +                  dsdhi ))
c
            b2  = sngl(1.D0 - t2 * qstr / (g*af2(iup)*af2(iup)))
c
            c2  = sngl(t2 * dsdhp1)
c
            m2  = sngl(-mugf * cgd * wstr * dg
     +          * (t1 - sqrt(2. * g * dhstru)))
c
         else if ( formno .eq. 1 ) then
c
c           - free weir flow -
c

            t1  = cwf * wstr * sqrt(2./3. * g * tnrm)
c
            a2  = sngl(-t1 * (1.D0 - qstr * qstr * wf2(iup) /
     +                 af2(iup)**3 / g))
c
            b2  = sngl(1.D0 -t1 * qstr / ( g * af2(iup) * af2(iup) ))
c
         else if ( formno .eq. 2 ) then
c
c           - drowned weir flow -
c
            t1  = sqrt (2. * g * abs( dhstru + tnrm - ds))
            t2  = cwd * wstr * g / t1
c
            a2  = sngl(-t2 * ds * (1.D0 - qstr*qstr*wf2(iup)/af2(iup)
     +                 **3/g - dsdhi) - cwd * wstr * t1 * dsdhi)
c
            b2  = sngl(1.D0 - t2 * ds * qstr / (g*af2(iup)*af2(iup)))
c
            c2  = sngl(t2 * ds * dsdhp1 - cwd * wstr * t1 * dsdhp1)
c
         endif
      endif
c
      end
