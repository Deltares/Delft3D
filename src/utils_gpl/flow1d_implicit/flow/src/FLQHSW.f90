function FLQHSW(g      ,istru  ,strsta ,strclo ,hu     ,hd     ,&
&uu     ,ud     ,zs     ,wstr   ,cw     ,slim   ,&
&itab   ,maxtab ,ntabm  ,ntab   ,table  ,fred   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLQHSW (FLow QH relation for Simple Weir)
!
! Module description: Subroutine FLQHSW defines the QH-relationship for
!                     a simple weir.
!
!                     In subroutine FLQHSW for given upstream and down-
!                     stream water levels the discharge across the weir
!                     will be computed according to the specific
!                     stage-discharge equation (QH-relation) for the
!                     weir.
!
!                     Note: by definition the upstream side is defined
!                     as the side on which the highest water level
!                     exists.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 11 cw                I  Correction coefficient for weir flow.
!  0 flqhsw            O  Discharge across simple weir.
! 18 fred              IO -
!  1 g                 I  Acceleration of gravity.
!  6 hd                I  Downstream water level.
!  5 hu                I  Upstream water level.
!  2 istru             I  Number of structure.
! 13 itab              I  Table number.
! 14 maxtab            I  Maximum number of defined tables.
! 16 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
!                         to maxtab. For a specific table number k and
!                         function Y = f (X) the following definitions
!                         exist:
!                         (1,k) = Length of table k.
!                         (2,k) = Start address X in table.
!                         (3,k) = Start address Y in table.
!                         (4,k) = Access method and period control: xy
!                                 x = ctbnpf (0) : No period defined
!                                 x = ctbpfu (1) : Period defined
!                                 y = ctbico (0) : Continue interpltn
!                                 y = ctbidi (1) : Discrete interpltn
! 15 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 12 slim              I  Submergence limit.
!  4 strclo(nstru)     O  True if structure is closed.
!  3 strsta            I  Logical indicator.
! 17 table             P  -
!  8 ud                I  Downstream velocity.
!  7 uu                I  Upstream velocity.
! 10 wstr              I  Width at centre of structure.
!  9 zs                I  Bed level at centre of structure.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!     Declaration of function
!
   real    FLQHSW
!
!     Declaration of parameters:
!
   integer istru, itab, maxtab, ntabm, ntab(4,maxtab)
   logical strsta, strclo(*)
   real    cw, g, hu, hd, slim, uu, ud, wstr, zs, fred
   real    table(ntabm)
!
!     Declaration of local variables:
!
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
!
      eld = hd + ud*ud / (2.0*g)
!
      sf = (eld-zs) / elucrs
      sf = min ( max ( sf, 0. ), 1. )
!
      if ( sf .le. slim ) then
         fred = 1.0
      else
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &dble(sf), fred )
      endif
!
!        Compute discharge Q for simple weir
!
      FLQHSW = (2./3.) * cw * fred * sqrt(2./3.*g) *&
      &wstr * elucrs *sqrt(elucrs)
   endif
!
end
