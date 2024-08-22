      function FLQH09 (g     , hu    , hd    , qin   , wn    , 
     &                 wp    , le    , shapco, teken , strsta,
     &                 strclo, istru , nstru )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLow QH relation type 09 (bridge piers)
c
c Module description: Calculate discharge through structure. This 
c                     routine is taken from WENDY and altered to suit
c                     SOBEK. Functionality is unchanged. WENDY history
c                     is:
c                        Project: Construction-Module
c                        Programmer: G. van Driel
c                        Function: Discharge through structure type 9
c                        Updates: None
c                     Ref: WENDY v3.00 reference guide, appendix A
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c FLSQRT  Function: returns sqrt(a) for a > 0  and -sqrt(-a) for a < 0
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c $Id$
c
c History:
c $Log: flqh09.pf,v $
c Revision 1.2  1998/06/08  12:35:44  kuipe_j
c log added
c
c
c
c***********************************************************************
c Parameters:
c name     I/O     what
c g         P      acceleration due to gravity
c hd        I      downstream water level
c hu        I      upstream water level
c le        I      bottom level of the section Le
c qin       I      upstream discharge on previous iteration
c shapco    I      pier shape coefficient K
c teken     I      flow direction
c wn        I      total net width of section Wn
c wp        I      total pier width Wp
c
c***********************************************************************
c
c declare arguments
      real    g     , wn    , wp    , le    , shapco, qin   , hu    ,
     &        hd
      integer teken , istru , nstru
      logical strsta, strclo(nstru)
c
c declare variables
      real    wpwn  , x     , x2    , lamda1, lamda3, dc    , v     ,
     &        w     , t1    , t2    , arg   , qstr
      integer kflow
c
c declare functions
      real    FLQH09, FLSQRT

c 
c set qin = 100 for testing
c      qin = 100.
c
c check if water level is below bed level
      if (hu .lt. le)  then
        if (strsta) strclo(istru) = .true.
        qstr = 0.
        goto 10
      else
        if (strsta) strclo(istru) = .false.
      endif

      if (teken .lt. 0) then
        qstr = 0
        goto 10
      endif

      wpwn   = wn + wp
      x      = wp / wpwn
      x2     = x * x
      lamda3 = .9 * SQRT(x) + 2.7 * x2 + 1.
      dc     = (qin / wpwn)**2
      dc     = (dc / g)**(1./3.)

c
c determine flow type
      if ((hd - le) .ge. (dc * lamda3)) then
        kflow = 1
      else
        kflow = 2
      endif

      if (kflow .eq. 1) then

c
c class A (eqn. IX-6)
        v  = qin / (wpwn * (hd - le))
        w  = v * v / (2 * g * (hd - le))
        t1 = shapco + 10 * w - 0.6
        t2 = x + 15 * x2 * x2

        if (ABS(hu - hd) .lt. 1.0e-15) then
          qstr = 0.
          goto 10
        endif

        qstr = wpwn * (hd - le) * FLSQRT(hu - hd)
        qstr = qstr * SQRT(g / (shapco * t1 * t2) )

      else

c
c class B (eqn. IX-8)

        lamda1 = 1. + SQRT(x) * (1.1 + 2.4 * x)
        arg = (hu - le) / lamda1
        qstr  = wpwn * arg * SQRT(g * arg)
      endif

 10   FLQH09 = qstr

      return
      end
