subroutine FLQHSWQ(g      ,hu     ,hd     ,q     ,uu     ,ud    ,&
&zs     ,wstr   ,cw     ,wet   ,&
&itab   ,maxtab ,ntabm  ,ntab  ,table  ,red   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Kuipers
!
! Module:             FLQHSWQ (FLow QH relation for Simple Weir
!                              Q incrementing)
!
! Module description: Subroutine FLQHSWQ defines the QH-relationship for
!                     a simple weir. It is the reverse of the same
!                     relation defined by FLQHSW.
!
!                     In subroutine FLQHSW for given upstream and down-
!                     stream water levels the discharge across the weir
!                     will be computed according to the specific
!                     stage-discharge equation (QH-relation) for the
!                     weir. Q=f(H1,H2)
!
!                     In subroutine FLQHSWQ the stage-discharge equation
!                     reads H2=f(Q,H1) for fred < 1 and
!                           H1=f(Q,H2) for fred = 1.
!
!                     Note: by definition the upstream side is defined
!                     as the side on which the highest water level
!                     exists.
!
!-----------------------------------------------------------------------
! Parameters:
!  NAME              IO DESCRIPTION
!  cw                I  Correction coefficient for weir flow.
!  fred              IO -
!  g                 I  Acceleration of gravity.
!  hd                I  Downstream water level.
!  hu                I  Upstream water level.
!  itab              I  Table number.
!  maxtab            I  Maximum number of defined tables.
!  ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                       to maxtab. For a specific table number k and
!                       function Y = f (X) the following definitions
!                       exist:
!                       (1,k) = Length of table k.
!                       (2,k) = Start address X in table.
!                       (3,k) = Start address Y in table.
!                       (4,k) = Access method and period control: xy
!                               x = ctbnpf (0) : No period defined
!                               x = ctbpfu (1) : Period defined
!                               y = ctbico (0) : Continue interpltn
!                               y = ctbidi (1) : Discrete interpltn
!  ntabm             I  Maximum size of table (Used for dimensioning
!                       table).
!  q                 I  Discharge across simple weir.
!  red               O  False if the reduction factor = 1, else True.
!  slim              I  Submergence limit.
!  table             P  -
!  ud                I  Downstream velocity.
!  uu                I  Upstream velocity.
!  wstr              I  Width at centre of structure.
!  zs                I  Bed level at centre of structure.
!  wet               O  True if crest is wet
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
!=======================================================================
!
!     Declaration of parameters:
!
   integer itab, maxtab, ntabm, ntab(4,maxtab)
   logical wet , red
   real    cw, g, hu, hd, uu, ud, wstr, zs ,q
   real    table(ntabm)
!
!     Declaration of local variables:
!
   real    elu, eld, sf, elucrs, fred ,fredel

   elu = hu + uu*uu / (2.0*g)
   elucrs = elu - zs
   if ( elucrs .le. 0.0) then
      wet = .false.
   else
      wet = .true.
!
!       Compute water level for simple weir
!
      fredel = q / ((2./3.) * cw * sqrt(2./3.*g) * wstr )
      fred   = fredel / (elucrs *sqrt(elucrs))
      if (fred.ge.1.) then
!           Reduction factor = 1, so calculate
!           upward water level
         red = .false.
         elu = fredel**(2./3.) + zs
         hu  = elu - uu*uu / (2.0*g)
      else
!           Reduction factor < 1, so calculate
!           downward water level
         red = .true.
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(3,itab)),&
         &table(ntab(2,itab)),&
         &dble(fred), sf )
         eld = sf * elucrs + zs
         hd  = eld - ud*ud / (2.0*g)
      endif
   endif
!
end
