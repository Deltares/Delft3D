subroutine KASW (g      ,il     ,ir     ,ngrid  ,istru  ,strclo ,&
&strpar ,h2     ,h1     ,q2     ,af2    ,wf2    ,&
&maxtab ,ntabm  ,ntab   ,table  ,rho    ,a2     ,&
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
! Module:             KASW (KAlman structure Simple Weir)
!
! Module description: In subroutine KASW the ABCDE coefficients for the
!                     stage-discharge equation are computed for a simple
!                     weir.
!
!                     The coefficients are described in S-FO-004 Par 7.2
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 18 a2(ngridm)        O  A2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 11 af2(ngrid)        I  Flow area in every grid point i on time n+1.
!                         Only values at grid points around structures are
!                         defined.
! 19 b2(ngridm)        O  B2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 20 c2(ngridm)        O  C2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 21 d2(ngridm)        O  D2-coefficient of momentum equation c.q. sta-
!                         ge-discharge equation per gridpoint.
! 23 ea2(ngrid)        O  EA2 right hand side coefficient of momentum
! 24 eb2(ngrid)        O  EB2 right hand side coefficient of momentum
! 25 ec2(ngrid)        O  EC2 right hand side coefficient of momentum
! 26 ed2(ngrid)        O  ED2 right hand side coefficient of momentum
! 27 em2(ngrid)        O  EM2 right hand side coefficient of Q-h relation
!  1 g                 I  Acceleration of gravity.
!  9 h1                P  -
!  8 h2                P  -
!  2 il                P  -
!  3 ir                P  -
!  5 istru             I  Number of structure.
! 22 m2(ngrid)         O  M2 coefficient of Q-h relation
! 13 maxtab            I  Maximum number of defined tables.
!  4 ngrid             I  Number of grid points in network.
! 15 ntab              P  -
! 14 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 10 q2                P  -
! 17 rho               P  -
! 28 sign              I  Flow direction (+/-).
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
! 16 table             P  -
! 12 wf2(ngrid)        I  Flow width in every grid point i on time n+1.
!                         Only values at grid points around structures are
!                         defined.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flswar  FLow get Simple Weir ARguments
! flupdo  FLow UP- and DOwnstream near structure
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kasw.pf,v $
! Revision 1.4  1999/03/15  15:52:23  kuipe_j
! tabs removed
!
! Revision 1.3  1997/10/03  06:40:12  kuipe_j
! criterium for flow drection changed
!
! Revision 1.2  1996/04/12  13:05:31  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:05  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of function:
!
   real    FLQHSW
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer il, ir, istru, ngrid, maxtab, ntabm, ntab(4,maxtab)
   logical strclo(*)
   real    strpar(dmstrpar,*), table(ntabm)
   real    af2(ngrid), wf2(ngrid)
   real    rho(ngrid)
   real    g, a2, b2, c2, d2, m2, ea2, eb2, ec2, ed2, em2, sign
   real    crest
   double precision h2(ngrid), h1(ngrid), q2(ngrid)
!
!     Declaration of local variables:
!
   integer iup, idown, itab
   logical strsta
   real    hunp1, hdnp1, hun, hdn, uu, ud, dh, qstr, qdum
   real    fred, fredi, fredp1, dfdhup, dfdhdo
   real    zs, wstr, cw, slim, t1, t2, term
!
   parameter (dh = 0.001)
!
   crest = strpar(1 ,istru)

   call FLUPDO(g      ,il     ,ir     ,ngrid  ,h2     ,h1     ,&
   &q2     ,q2     ,af2    ,rho    ,crest  ,hunp1  ,&
   &hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,&
   &idown  ,sign   )


   call FLSWAR(istru  ,strpar ,sign   ,zs     ,wstr   ,cw     ,&
   &slim   ,itab   )
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
      qstr = sign * FLQHSW (g      ,istru  ,strsta ,strclo ,hunp1  ,&
      &hdnp1  ,uu     ,ud     ,zs     ,wstr   ,&
      &cw     ,slim   ,itab   ,maxtab ,ntabm  ,&
      &ntab   ,table  ,fred   )
!
      strsta = .false.
      qdum = FLQHSW (g      ,istru  ,strsta ,strclo ,hunp1+dh,hdnp1 ,&
      &uu     ,ud     ,zs     ,wstr   ,cw     ,slim   ,&
      &itab   ,maxtab ,ntabm  ,ntab   ,table  ,fredi  )
      dfdhup = (fredi - fred) / dh
!
      strsta = .false.
      qdum = FLQHSW(g      ,istru  ,strsta ,strclo ,hunp1  ,hdnp1+dh,&
      &uu     ,ud     ,zs     ,wstr   ,cw     ,slim   ,&
      &itab   ,maxtab ,ntabm  ,ntab   ,table  ,fredp1 )
      dfdhdo = (fredp1 - fred) / dh
!
      t1  = sqrt (abs(hunp1 + qstr*qstr / ( 2.*g*af2(iup)*af2(iup) )&
      &- zs))
      t2  = cw * 2./3. * sqrt(2./3. * g) * wstr
!
      term = ( 1. - qstr * qstr * wf2(iup) / (g * af2(iup)**3) )
!
      a2  = -t1 * t2 * (t1 * t1 * dfdhup + 3./2. * fred * term)
      b2  = 1. - t1 * 3./2. * fred * t2 * qstr / (g * af2(iup)&
      &* af2(iup))
      c2  = -t2 * t1**3 * dfdhdo
      d2  = 0.
      m2  = 0.
      ea2 = 0.
      eb2 = 0.
      ec2 = 0.
      ed2 = 0.
      em2 = 0.
!
   endif
!
end
