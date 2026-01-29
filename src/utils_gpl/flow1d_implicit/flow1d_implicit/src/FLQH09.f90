function FLQH09 (g     , hu    , hd    , qin   , wn    ,&
&wp    , le    , shapco, teken , strsta,&
&strclo, istru , nstru )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow module
!
! Programmer:         P.R. Evans
!
! Module:             FLow QH relation type 09 (bridge piers)
!
! Module description: Calculate discharge through structure. This
!                     routine is taken from WENDY and altered to suit
!                     SOBEK. Functionality is unchanged. WENDY history
!                     is:
!                        Project: Construction-Module
!                        Programmer: G. van Driel
!                        Function: Discharge through structure type 9
!                        Updates: None
!                     Ref: WENDY v3.00 reference guide, appendix A
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! FLSQRT  Function: returns sqrt(a) for a > 0  and -sqrt(-a) for a < 0
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
! $Id$
!
! History:
! $Log: flqh09.pf,v $
! Revision 1.2  1998/06/08  12:35:44  kuipe_j
! log added
!
!
!
!***********************************************************************
! Parameters:
! name     I/O     what
! g         P      acceleration due to gravity
! hd        I      downstream water level
! hu        I      upstream water level
! le        I      bottom level of the section Le
! qin       I      upstream discharge on previous iteration
! shapco    I      pier shape coefficient K
! teken     I      flow direction
! wn        I      total net width of section Wn
! wp        I      total pier width Wp
!
!***********************************************************************
!
! declare arguments
   real    g     , wn    , wp    , le    , shapco, qin   , hu    ,&
   &hd
   integer teken , istru , nstru
   logical strsta, strclo(nstru)
!
! declare variables
   real    wpwn  , x     , x2    , lamda1, lamda3, dc    , v     ,&
   &w     , t1    , t2    , arg   , qstr
   integer kflow
!
! declare functions
   real    FLQH09, FLSQRT

!
! set qin = 100 for testing
!      qin = 100.
!
! check if water level is below bed level
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

!
! determine flow type
   if ((hd - le) .ge. (dc * lamda3)) then
      kflow = 1
   else
      kflow = 2
   endif

   if (kflow .eq. 1) then

!
! class A (eqn. IX-6)
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

!
! class B (eqn. IX-8)

      lamda1 = 1. + SQRT(x) * (1.1 + 2.4 * x)
      arg = (hu - le) / lamda1
      qstr  = wpwn * arg * SQRT(g * arg)
   endif

10 FLQH09 = qstr

   return
end
