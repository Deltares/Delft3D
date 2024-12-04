function FLQHPP(istru  ,strsta ,strclo ,hu     ,hd     ,&
&hact   ,itab   ,cap    ,capold ,hstart ,hstop  ,&
&maxtab ,ntabm  ,ntab   ,table  ,teken  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLQHPP (FLow QH relation for PumP)
!
! Module description: Subroutine FLQHPP defines the QH-relationship for
!                     a pump.
!
!                     In subroutine FLQHPP the given water level will be
!                     used to determine if the pump should switch on or
!                     off. The user defines the sign of the pump by
!                     giving the capacity a positive or negative value.
!
!                     The pump is controlled by the upward or downward
!                     water level. A reduction factor can be defined as
!                     a function of the head (difference in water
!                     levels).
!
!                     If the start level has been defined above the stop
!                     level the pump starts when the water level exceeds
!                     H-start. The pump stops when the water level drops
!                     below H-stop.
!
!                     If the start level has been defined below the stop
!                     level the pump starts when the water level drops
!                     below H-start. The pump stops when the water level
!                     exceeds H-stop.
!
!                     The sign of the discharge is determined using the
!                     start and stop levels. If the stop level exceeds
!                     the start level the discharge direction is oppo-
!                     site to the controlled water level.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  0 flqhpp            O  Discharge across pump.
!  7 hact              I  Actual water level at gridpoint in branch.
!  6 hd                I  Downstream water level.
!  5 hu                I  Upstream water level.
!  1 istru             I  Number of structure.
!  8 maxtab            I  Maximum number of defined tables.
! 10 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  9 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  3 strclo(nstru)     O  True if structure is closed.
!  2 strsta            I  Logical indicator.
! 11 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! inttab  INTerpolate in TABle
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flqhpp.pf,v $
! Revision 1.5  1999/03/15  15:50:36  kuipe_j
! tabs removed
!
! Revision 1.4  1995/09/22  10:02:09  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:55:21  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:22  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:02  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:47  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:24  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:54  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Function: Definition of QH-relation for a pump.
!               (stage-discharge relation)
!
!     Declaration of function
!
   real    FLQHPP
   logical EQUAL
!
!     Declaration of parameters:
!
   integer istru, maxtab, ntabm, ntab(4,maxtab)
   integer itab
   logical strsta, strclo(*)
   real    hact, teken, cap, capold, hstart, hstop
   double precision hu, hd
   real    table(ntabm)
!
!     Declaration of local variables:
!
   real    head, f, q
!
   head = hu - hd
   call INTTAB (ntab(1,itab), ntab(4,itab), table(ntab(2,itab)),&
   &table(ntab(3,itab)), dble(head), f)
!
   if ( hstart .ge. hstop ) then
!        pumping from control side
      if ( hact .ge. hstop .and. hact .le. hstart ) then
         q = capold * f
      else if ( hact .ge. hstart ) then
         q = cap * f
         capold = cap
      else if ( hact .le. hstop ) then
         q = 0.
         capold = 0.
      endif
   else
!        pumping towards control side
      if ( hact .ge. hstart .and. hact .le. hstop ) then
         q = capold * f
      else if ( hact .le. hstart ) then
         q = cap * f
         capold = cap
      else if ( hact .ge. hstop ) then
         q = 0.
         capold = 0.
      endif
   endif
!
!     Structure open or closed ?
!
   if ( strsta ) then
      strclo(istru) = EQUAL(q , 0.)
   endif
!
   FLQHPP = q * teken
!
end
