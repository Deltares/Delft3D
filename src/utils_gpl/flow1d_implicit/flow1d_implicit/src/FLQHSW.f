      function FLQHSW(g      ,istru  ,strsta ,strclo ,hu     ,hd     ,
     +                uu     ,ud     ,zs     ,wstr   ,cw     ,slim   ,
     +                itab   ,maxtab ,ntabm  ,ntab   ,table  ,fred   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLQHSW (FLow QH relation for Simple Weir)
c
c Module description: Subroutine FLQHSW defines the QH-relationship for
c                     a simple weir.
c
c                     In subroutine FLQHSW for given upstream and down-
c                     stream water levels the discharge across the weir
c                     will be computed according to the specific
c                     stage-discharge equation (QH-relation) for the
c                     weir.
c
c                     Note: by definition the upstream side is defined
c                     as the side on which the highest water level
c                     exists.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 11 cw                I  Correction coefficient for weir flow.
c  0 flqhsw            O  Discharge across simple weir.
c 18 fred              IO -
c  1 g                 I  Acceleration of gravity.
c  6 hd                I  Downstream water level.
c  5 hu                I  Upstream water level.
c  2 istru             I  Number of structure.
c 13 itab              I  Table number.
c 14 maxtab            I  Maximum number of defined tables.
c 16 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                         to maxtab. For a specific table number k and
c                         function Y = f (X) the following definitions
c                         exist:
c                         (1,k) = Length of table k.
c                         (2,k) = Start address X in table.
c                         (3,k) = Start address Y in table.
c                         (4,k) = Access method and period control: xy
c                                 x = ctbnpf (0) : No period defined
c                                 x = ctbpfu (1) : Period defined
c                                 y = ctbico (0) : Continue interpltn
c                                 y = ctbidi (1) : Discrete interpltn
c 15 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c 12 slim              I  Submergence limit.
c  4 strclo(nstru)     O  True if structure is closed.
c  3 strsta            I  Logical indicator.
c 17 table             P  -
c  8 ud                I  Downstream velocity.
c  7 uu                I  Upstream velocity.
c 10 wstr              I  Width at centre of structure.
c  9 zs                I  Bed level at centre of structure.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c     Declaration of function
c
      real    FLQHSW
c
c     Declaration of parameters:
c
      integer istru, itab, maxtab, ntabm, ntab(4,maxtab)
      logical strsta, strclo(*)
      real    cw, g, hu, hd, slim, uu, ud, wstr, zs, fred
      real    table(ntabm)
c
c     Declaration of local variables:
c
      real    elu, eld, sf, elucrs

      fred = 0.
      elu = hu + uu*uu / (2.0*g)
      elucrs = elu - zs 
      if ( elucrs .le. 0.0) then
         if ( strsta ) then
            strclo(istru) = .true.
         endif
         FLQHSW = 0.
      else
         if ( strsta ) then
            strclo(istru) = .false.
         endif
c
         eld = hd + ud*ud / (2.0*g)
c
         sf = (eld-zs) / elucrs
         sf = min ( max ( sf, 0. ), 1. )
c
         if ( sf .le. slim ) then
            fred = 1.0
         else
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
     +                   dble(sf), fred )
         endif
c
c        Compute discharge Q for simple weir
c
         FLQHSW = (2./3.) * cw * fred * sqrt(2./3.*g) *
     +            wstr * elucrs *sqrt(elucrs)
      endif
c
      end
