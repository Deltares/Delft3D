      subroutine KAAW (g      ,il     ,ir     ,ngrid  ,istru  ,strclo ,
     +                 strpar ,h2     ,h1     ,q2     ,af2    ,wf2    ,
     +                 rho    ,a2     ,b2     ,c2     ,d2     ,m2     ,
     +                 ea2    ,eb2    ,ec2    ,ed2    ,em2    ,sign   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAAW (KAlman Advanced Weir)
c
c Module description: In subroutine KAAW the ABCDE coefficients are
c                     computed for an advanced weir.
c
c                     The coefficients are described in S-FO-004 Par 7.3
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 14 a2(ngridm)        O  A2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 11 af2(ngrid)        I  Flow area in every grid point i on time n+1.
c                         Only values at grid points around structures are
c                         defined.
c 15 b2(ngridm)        O  B2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 16 c2(ngridm)        O  C2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 17 d2(ngridm)        O  D2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 19 ea2(ngrid)        O  EA2 right hand side coefficient of momentum
c 20 eb2(ngrid)        O  EB2 right hand side coefficient of momentum
c 21 ec2(ngrid)        O  EC2 right hand side coefficient of momentum
c 22 ed2(ngrid)        O  ED2 right hand side coefficient of momentum
c 23 em2(ngrid)        O  EM2 right hand side coefficient of Q-h relation
c  1 g                 I  Acceleration of gravity.
c  9 h1                P  -
c  8 h2                P  -
c  2 il                P  -
c  3 ir                P  -
c  5 istru             I  Number of structure.
c 18 m2(ngrid)         O  M2 coefficient of Q-h relation
c  4 ngrid             I  Number of grid points in network.
c 10 q2                P  -
c 13 rho               P  -
c 24 sign              I  Flow direction (+/-).
c  6 strclo(nstru)     I  True if structure is closed.
c  7 strpar(21,nstru)  I  Each structure is characterized by a number of
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
c 12 wf2(ngrid)        I  Flow width in every grid point i on time n+1.
c                         Only values at grid points around structures are
c                         defined.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flupdo  FLow UP- and DOwnstream near structure
c kaawar  KAlman get Advanced Weir Arguments
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kaaw.pf,v $
c Revision 1.4  1999/03/15  15:51:31  kuipe_j
c tabs removed
c
c Revision 1.3  1997/10/03  06:40:10  kuipe_j
c criterium for flow drection changed
c
c Revision 1.2  1996/04/12  13:04:35  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:24:11  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of function:
c
      real    FLQHAW
c
c     Declaration of parameters:
c
      integer il, ir, istru, ngrid
      logical strclo(*)
      real    strpar(dmstrpar,*)
      real    af2(ngrid), wf2(ngrid)
      real    rho(ngrid)
      real    g, a2, b2, c2, d2, m2, ea2, eb2, ec2, ed2, em2, sign
      double precision h2(ngrid), h1(ngrid), q2(ngrid)
c
c     Declaration of local variables:
c
      integer iup, idown
      logical strsta
      real    hunp1, hdnp1, hun, hdn, uu, ud, qstr, waw, elu
      real    h0, ka, kp, n, p, wn, zs, c0, c1, ch, ct
      real    x1, x12, x13, x2
      real    ta, ta2, tb3, tb4, t1, t2, t3, t4, t5, t6, t7
      real    crest
c
      crest = strpar(1 ,istru)

      call FLUPDO(g      ,il     ,ir     ,ngrid  ,h2     ,h1     ,
     +            q2     ,q2     ,af2    ,rho    ,crest  ,hunp1  ,
     +            hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,
     +            idown  ,sign   )
c
      call KAAWAR(istru  ,strpar ,sign   ,h0     ,ka     ,kp     ,
     +            n      ,p      ,wn     ,zs     )
c
      if ( strclo(istru) ) then
         a2  = 0.
         b2  = 1.
         c2  = 0.
         d2  = 0.
         m2  = 0.
         ea2 = 0.
         eb2 = 0.
         ec2 = 0.
         ed2 = 0.
         em2 = 0.
      else
c
         strsta = .false.
         qstr = sign * FLQHAW (g      ,istru  ,strsta ,strclo ,hunp1  ,
     +                         hdnp1  ,uu     ,h0     ,ka     ,kp     ,
     +                         n      ,p      ,wn     ,zs     ,waw    ,
     +                         elu    ,c0     ,ch     ,ct     ,x1     ,
     +                         x12    ,x13    ,x2     ,ta     ,ta2    ,
     +                         tb3    ,tb4    )
c
         c1 = c0 * ch
c
         if ( x1 .lt. 1.6 ) then
            t1 = 0.4182*x12 - 0.832*x1 + 0.488
         else
            t1 = 0.0
         endif
c
         if ( x2 .lt. 0.7 ) then
            t2 = -ta / (0.49 * sqrt(1.0 - ta2/0.49)) +
     +           108. * tb3 * x2 * sqrt(x2) +
     +           40.5 * tb4 * sqrt(x2)
         else
            t2 = 0.0
         endif
c
         t3 = 1. - qstr * qstr * wf2(iup) / (g * af2(iup)**3 )
c
         t4 = qstr / (g * af2(iup) * af2(iup))
c
         t5 = (hdnp1 - zs) / ((elu-zs)*(elu-zs))
c
         t7 = sqrt(2.*g * (elu-zs))
c
         t6 = (3./2. * c1 * ct * waw * t7 + (elu-zs)) * t7 *
     +        (c0 * t1 /h0 * ct * waw + c1 * t2 * t5 * waw +
     +         c1 * ct * (-2. * n * (kp - ka)))
c
         a2  = -t6 * t3
         b2  = 1. - t6 * t4
         c2  = -c1 * t2 * waw * t7
         d2  = 0.
         m2  = 0.
         ea2 = 0.
         eb2 = 0.
         ec2 = 0.
         ed2 = 0.
         em2 = 0.
      endif
      end
