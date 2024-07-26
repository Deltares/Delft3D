      function FLQH10 (g     , hu    , hd    , uu    , le    , 
     &                 appwid, gapwid, gaplen, ks    , c1    , 
     &                 c3    , applen, itype , 
     &                 qin   )
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow module
c
c Programmer:         P.R. Evans
c
c Module:             FLow QH relation type 10 (abutments)
c
c Module description: Calculate discharge through structure. This 
c                     routine is taken from WENDY and altered to suit
c                     SOBEK. Functionality is unchanged. WENDY history
c                     is:
c                        Project: Construction-Module
c                        Programmer: G. van Driel
c                        Function: Discharge through structure type 10
c                        Updates: None
c                     Ref: WENDY v3.00 reference guide, appendix A
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c FLFRST  Function: calculates the friction in the structure
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
c $Log: flqh10.pf,v $
c Revision 1.2  1998/06/08  12:35:46  kuipe_j
c log added
c
c
c
c***********************************************************************
c Parameters:
c name     I/O     what
c applen    I      length of approach section
c appwid    I      width of approach section
c c1        I      base coefficient type I
c c3        I      base coefficient type III
c(ccr       I      coefficient)
c g         P      acceleration due to gravity
c gaplen    I      gap length
c gapwid    I      gap width
c hd        I      downstream water level
c hu        I      upstream water level
c itype     I      type of abutment (I or III)
c ks        I      approach section roughness
c le        I      bed level at and near gap
c(teken     I      flow direction)
c uu        I      upstream velocity
c
c***********************************************************************
c
c declare arguments
      real    g     , hu    , hd    , uu    , le    , appwid,
     &        gapwid, gaplen, ks    , c1    , c3    , 
     &        applen, qin
      integer itype
c
c declare variables
      real    elu   , qstr  , x     , x2    , coef6 , coef7 ,
     &        a1    , r     , r1    , ai    , ri    , c     ,
     &        h1enf , hf    , hf1   , hf2   , rmax  , crit
      integer kflow

c
c declare functions
      real    FLQH10, FLSQRT, FLFRST
      logical  EQUAL
      external EQUAL

      elu = hu + uu * uu / (2. * g)
      if (elu .lt. le) then
         qstr = 0.
         goto 10
      endif

      x       = 1. - gapwid / appwid
      x2      = x * x

      if (itype .eq. 1) then
        if (EQUAL(c1 , 0.)) then
          coef6 = -.1146 * x2 * x + .5787 * x2 - .776 * x + 1.
        else
          coef6 = c1
        endif
      else
        if (EQUAL(c3 , 0.)) then
          coef7 = -.527 * x2 * x + 1.256 * x2 - 1.019 * x + 1.
        else
          coef7 = c3
        endif
      endif

c
c calculate friction terms
      a1  = appwid *  (hu - le)
      r   = appwid + 2 * ( hu - le )
      r1 = a1 / r

c
c set minimum value for r1
      rmax = MAX(0.1, 10 * ks)

      if (r1 .lt. rmax) then
        r1 = rmax
        a1 = r1 * appwid**2 / (appwid - 2 * r1)
      endif

      ai = gapwid * ( hd - le )
      ri = gapwid + 2 * ( hd - le )
      ri = ai / ri 

c
c set minimum value for ri
      rmax = MAX(0.1, 10 * ks)

      if (ri .lt. rmax) then
        ri = rmax
        ai = ri * gapwid**2 / (gapwid - 2 * ri)
      endif

      hf1 = FLFRST (qin, applen, r1, a1, ks, ri, ai, ks)
      hf2 = FLFRST (qin, gaplen, ri, ai, ks, ri, ai, ks)
      hf  = hf1 + hf2

c
c discharge calculation
      if (itype .eq. 1) then
        c = coef6
      else
        c = coef7
      endif

      h1enf = elu - hf - le
      crit = 2 * h1enf / 3.

c
c determine flow type
      if ((hd - le) .ge. crit) then
        kflow = 1
      else
        kflow = 3
      endif

      if (kflow .eq. 1) then

c
c eqn. X-22
        qstr = c * gapwid * (hd - le)
        qstr = qstr * FLSQRT(2 * g * (elu - hf - hd))
      else

c
c eqn. X-23
        qstr = c * gapwid * 2/3 * h1enf
        qstr = qstr * SQRT(2 * g * h1enf / 3 )
      endif

 10   FLQH10 = qstr

      return
      end
