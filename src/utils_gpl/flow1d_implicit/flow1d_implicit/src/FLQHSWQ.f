      subroutine FLQHSWQ(g      ,hu     ,hd     ,q     ,uu     ,ud    ,
     +                   zs     ,wstr   ,cw     ,wet   ,
     +                   itab   ,maxtab ,ntabm  ,ntab  ,table  ,red   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Kuipers
c
c Module:             FLQHSWQ (FLow QH relation for Simple Weir 
c                              Q incrementing)
c
c Module description: Subroutine FLQHSWQ defines the QH-relationship for
c                     a simple weir. It is the reverse of the same
c                     relation defined by FLQHSW.
c
c                     In subroutine FLQHSW for given upstream and down-
c                     stream water levels the discharge across the weir
c                     will be computed according to the specific
c                     stage-discharge equation (QH-relation) for the
c                     weir. Q=f(H1,H2)
c
c                     In subroutine FLQHSWQ the stage-discharge equation
c                     reads H2=f(Q,H1) for fred < 1 and 
c                           H1=f(Q,H2) for fred = 1.
c
c                     Note: by definition the upstream side is defined
c                     as the side on which the highest water level
c                     exists.
c
c-----------------------------------------------------------------------
c Parameters:
c  NAME              IO DESCRIPTION
c  cw                I  Correction coefficient for weir flow.
c  fred              IO -
c  g                 I  Acceleration of gravity.
c  hd                I  Downstream water level.
c  hu                I  Upstream water level.
c  itab              I  Table number.
c  maxtab            I  Maximum number of defined tables.
c  ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
c                       to maxtab. For a specific table number k and
c                       function Y = f (X) the following definitions
c                       exist:
c                       (1,k) = Length of table k.
c                       (2,k) = Start address X in table.
c                       (3,k) = Start address Y in table.
c                       (4,k) = Access method and period control: xy
c                               x = ctbnpf (0) : No period defined
c                               x = ctbpfu (1) : Period defined
c                               y = ctbico (0) : Continue interpltn
c                               y = ctbidi (1) : Discrete interpltn
c  ntabm             I  Maximum size of table (Used for dimensioning
c                       table).
c  q                 I  Discharge across simple weir.
c  red               O  False if the reduction factor = 1, else True. 
c  slim              I  Submergence limit.
c  table             P  -
c  ud                I  Downstream velocity.
c  uu                I  Upstream velocity.
c  wstr              I  Width at centre of structure.
c  zs                I  Bed level at centre of structure.
c  wet               O  True if crest is wet 
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c=======================================================================
c
c     Declaration of parameters:
c
      integer itab, maxtab, ntabm, ntab(4,maxtab)
      logical wet , red
      real    cw, g, hu, hd, uu, ud, wstr, zs ,q
      real    table(ntabm)
c
c     Declaration of local variables:
c
      real    elu, eld, sf, elucrs, fred ,fredel 

      elu = hu + uu*uu / (2.0*g)
      elucrs = elu - zs 
      if ( elucrs .le. 0.0) then
         wet = .false.
      else
         wet = .true.
c
c       Compute water level for simple weir
c
         fredel = q / ((2./3.) * cw * sqrt(2./3.*g) * wstr )
         fred   = fredel / (elucrs *sqrt(elucrs))
         if (fred.ge.1.) then
c           Reduction factor = 1, so calculate 
c           upward water level    
            red = .false.
            elu = fredel**(2./3.) + zs
            hu  = elu - uu*uu / (2.0*g)
         else
c           Reduction factor < 1, so calculate 
c           downward water level          
            red = .true.
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(3,itab)),
     +                   table(ntab(2,itab)),
     +                   dble(fred), sf )
            eld = sf * elucrs + zs
            hd  = eld - ud*ud / (2.0*g)
         endif
      endif
c
      end
