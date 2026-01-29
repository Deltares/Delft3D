subroutine KAAW (g      ,il     ,ir     ,ngrid  ,istru  ,strclo ,&
&strpar ,h2     ,h1     ,q2     ,af2    ,wf2    ,&
&rho    ,a2     ,b2     ,c2     ,d2     ,m2     ,&
&ea2    ,eb2    ,ec2    ,ed2    ,em2    ,sign   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAAW (KAlman Advanced Weir)
!
! Module description: In subroutine KAAW the ABCDE coefficients are
!                     computed for an advanced weir.
!
!                     The coefficients are described in S-FO-004 Par 7.3
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 14 a2(ngridm)        O  A2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 11 af2(ngrid)        I  Flow area in every grid point i on time n+1.
!                         Only values at grid points around structures are
!                         defined.
! 15 b2(ngridm)        O  B2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 16 c2(ngridm)        O  C2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 17 d2(ngridm)        O  D2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 19 ea2(ngrid)        O  EA2 right hand side coefficient of momentum
! 20 eb2(ngrid)        O  EB2 right hand side coefficient of momentum
! 21 ec2(ngrid)        O  EC2 right hand side coefficient of momentum
! 22 ed2(ngrid)        O  ED2 right hand side coefficient of momentum
! 23 em2(ngrid)        O  EM2 right hand side coefficient of Q-h relation
!  1 g                 I  Acceleration of gravity.
!  9 h1                P  -
!  8 h2                P  -
!  2 il                P  -
!  3 ir                P  -
!  5 istru             I  Number of structure.
! 18 m2(ngrid)         O  M2 coefficient of Q-h relation
!  4 ngrid             I  Number of grid points in network.
! 10 q2                P  -
! 13 rho               P  -
! 24 sign              I  Flow direction (+/-).
!  6 strclo(nstru)     I  True if structure is closed.
!  7 strpar(21,nstru)  I  Each structure is characterized by a number of
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
! 12 wf2(ngrid)        I  Flow width in every grid point i on time n+1.
!                         Only values at grid points around structures are
!                         defined.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flupdo  FLow UP- and DOwnstream near structure
! kaawar  KAlman get Advanced Weir Arguments
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kaaw.pf,v $
! Revision 1.4  1999/03/15  15:51:31  kuipe_j
! tabs removed
!
! Revision 1.3  1997/10/03  06:40:10  kuipe_j
! criterium for flow drection changed
!
! Revision 1.2  1996/04/12  13:04:35  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:11  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of function:
!
   real    FLQHAW
!
!     Declaration of parameters:
!
   integer il, ir, istru, ngrid
   logical strclo(*)
   real    strpar(dmstrpar,*)
   real    af2(ngrid), wf2(ngrid)
   real    rho(ngrid)
   real    g, a2, b2, c2, d2, m2, ea2, eb2, ec2, ed2, em2, sign
   double precision h2(ngrid), h1(ngrid), q2(ngrid)
!
!     Declaration of local variables:
!
   integer iup, idown
   logical strsta
   real    hunp1, hdnp1, hun, hdn, uu, ud, qstr, waw, elu
   real    h0, ka, kp, n, p, wn, zs, c0, c1, ch, ct
   real    x1, x12, x13, x2
   real    ta, ta2, tb3, tb4, t1, t2, t3, t4, t5, t6, t7
   real    crest
!
   crest = strpar(1 ,istru)

   call FLUPDO(g      ,il     ,ir     ,ngrid  ,h2     ,h1     ,&
   &q2     ,q2     ,af2    ,rho    ,crest  ,hunp1  ,&
   &hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,&
   &idown  ,sign   )
!
   call KAAWAR(istru  ,strpar ,sign   ,h0     ,ka     ,kp     ,&
   &n      ,p      ,wn     ,zs     )
!
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
!
      strsta = .false.
      qstr = sign * FLQHAW (g      ,istru  ,strsta ,strclo ,hunp1  ,&
      &hdnp1  ,uu     ,h0     ,ka     ,kp     ,&
      &n      ,p      ,wn     ,zs     ,waw    ,&
      &elu    ,c0     ,ch     ,ct     ,x1     ,&
      &x12    ,x13    ,x2     ,ta     ,ta2    ,&
      &tb3    ,tb4    )
!
      c1 = c0 * ch
!
      if ( x1 .lt. 1.6 ) then
         t1 = 0.4182*x12 - 0.832*x1 + 0.488
      else
         t1 = 0.0
      endif
!
      if ( x2 .lt. 0.7 ) then
         t2 = -ta / (0.49 * sqrt(1.0 - ta2/0.49)) +&
         &108. * tb3 * x2 * sqrt(x2) +&
         &40.5 * tb4 * sqrt(x2)
      else
         t2 = 0.0
      endif
!
      t3 = 1. - qstr * qstr * wf2(iup) / (g * af2(iup)**3 )
!
      t4 = qstr / (g * af2(iup) * af2(iup))
!
      t5 = (hdnp1 - zs) / ((elu-zs)*(elu-zs))
!
      t7 = sqrt(2.*g * (elu-zs))
!
      t6 = (3./2. * c1 * ct * waw * t7 + (elu-zs)) * t7 *&
      &(c0 * t1 /h0 * ct * waw + c1 * t2 * t5 * waw +&
      &c1 * ct * (-2. * n * (kp - ka)))
!
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
