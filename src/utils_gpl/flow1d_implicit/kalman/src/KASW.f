      subroutine KASW (g      ,il     ,ir     ,ngrid  ,istru  ,strclo ,
     +                 strpar ,h2     ,h1     ,q2     ,af2    ,wf2    ,
     +                 maxtab ,ntabm  ,ntab   ,table  ,rho    ,a2     ,
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
c Module:             KASW (KAlman structure Simple Weir)
c
c Module description: In subroutine KASW the ABCDE coefficients for the
c                     stage-discharge equation are computed for a simple
c                     weir.
c
c                     The coefficients are described in S-FO-004 Par 7.2
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 18 a2(ngridm)        O  A2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 11 af2(ngrid)        I  Flow area in every grid point i on time n+1.
c                         Only values at grid points around structures are
c                         defined.
c 19 b2(ngridm)        O  B2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 20 c2(ngridm)        O  C2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 21 d2(ngridm)        O  D2-coefficient of momentum equation c.q. sta-
c                         ge-discharge equation per gridpoint.
c 23 ea2(ngrid)        O  EA2 right hand side coefficient of momentum
c 24 eb2(ngrid)        O  EB2 right hand side coefficient of momentum
c 25 ec2(ngrid)        O  EC2 right hand side coefficient of momentum
c 26 ed2(ngrid)        O  ED2 right hand side coefficient of momentum
c 27 em2(ngrid)        O  EM2 right hand side coefficient of Q-h relation
c  1 g                 I  Acceleration of gravity.
c  9 h1                P  -
c  8 h2                P  -
c  2 il                P  -
c  3 ir                P  -
c  5 istru             I  Number of structure.
c 22 m2(ngrid)         O  M2 coefficient of Q-h relation
c 13 maxtab            I  Maximum number of defined tables.
c  4 ngrid             I  Number of grid points in network.
c 15 ntab              P  -
c 14 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 10 q2                P  -
c 17 rho               P  -
c 28 sign              I  Flow direction (+/-).
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
c 16 table             P  -
c 12 wf2(ngrid)        I  Flow width in every grid point i on time n+1.
c                         Only values at grid points around structures are
c                         defined.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flswar  FLow get Simple Weir ARguments
c flupdo  FLow UP- and DOwnstream near structure
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kasw.pf,v $
c Revision 1.4  1999/03/15  15:52:23  kuipe_j
c tabs removed
c
c Revision 1.3  1997/10/03  06:40:12  kuipe_j
c criterium for flow drection changed
c
c Revision 1.2  1996/04/12  13:05:31  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:05  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of function:
c
      real    FLQHSW
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters:
c
      integer il, ir, istru, ngrid, maxtab, ntabm, ntab(4,maxtab)
      logical strclo(*)
      real    strpar(dmstrpar,*), table(ntabm)
      real    af2(ngrid), wf2(ngrid)
      real    rho(ngrid)
      real    g, a2, b2, c2, d2, m2, ea2, eb2, ec2, ed2, em2, sign
      real    crest
      double precision h2(ngrid), h1(ngrid), q2(ngrid) 
c
c     Declaration of local variables:
c
      integer iup, idown, itab
      logical strsta
      real    hunp1, hdnp1, hun, hdn, uu, ud, dh, qstr, qdum
      real    fred, fredi, fredp1, dfdhup, dfdhdo
      real    zs, wstr, cw, slim, t1, t2, term
c
      parameter (dh = 0.001)
c
      crest = strpar(1 ,istru)

      call FLUPDO(g      ,il     ,ir     ,ngrid  ,h2     ,h1     ,
     +            q2     ,q2     ,af2    ,rho    ,crest  ,hunp1  ,
     +            hdnp1  ,hun    ,hdn    ,uu     ,ud     ,iup    ,
     +            idown  ,sign   )


      call FLSWAR(istru  ,strpar ,sign   ,zs     ,wstr   ,cw     ,
     +            slim   ,itab   )
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
         qstr = sign * FLQHSW (g      ,istru  ,strsta ,strclo ,hunp1  ,
     +                         hdnp1  ,uu     ,ud     ,zs     ,wstr   ,
     +                         cw     ,slim   ,itab   ,maxtab ,ntabm  ,
     +                         ntab   ,table  ,fred   )
c
         strsta = .false.
         qdum = FLQHSW (g      ,istru  ,strsta ,strclo ,hunp1+dh,hdnp1 ,
     +                  uu     ,ud     ,zs     ,wstr   ,cw     ,slim   ,
     +                  itab   ,maxtab ,ntabm  ,ntab   ,table  ,fredi  )
         dfdhup = (fredi - fred) / dh
c
         strsta = .false.
         qdum = FLQHSW(g      ,istru  ,strsta ,strclo ,hunp1  ,hdnp1+dh,
     +                 uu     ,ud     ,zs     ,wstr   ,cw     ,slim   ,
     +                 itab   ,maxtab ,ntabm  ,ntab   ,table  ,fredp1 )
         dfdhdo = (fredp1 - fred) / dh
c
         t1  = sqrt (abs(hunp1 + qstr*qstr / ( 2.*g*af2(iup)*af2(iup) )
     +               - zs))
         t2  = cw * 2./3. * sqrt(2./3. * g) * wstr
c
         term = ( 1. - qstr * qstr * wf2(iup) / (g * af2(iup)**3) )
c
         a2  = -t1 * t2 * (t1 * t1 * dfdhup + 3./2. * fred * term)
         b2  = 1. - t1 * 3./2. * fred * t2 * qstr / (g * af2(iup)
     +         * af2(iup))
         c2  = -t2 * t1**3 * dfdhdo
         d2  = 0.
         m2  = 0.
         ea2 = 0.
         eb2 = 0.
         ec2 = 0.
         ed2 = 0.
         em2 = 0.
c
      endif
c
      end
