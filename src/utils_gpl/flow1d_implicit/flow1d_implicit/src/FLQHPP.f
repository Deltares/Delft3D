      function FLQHPP(istru  ,strsta ,strclo ,hu     ,hd     ,
     +                hact   ,itab   ,cap    ,capold ,hstart ,hstop  ,
     +                maxtab ,ntabm  ,ntab   ,table  ,teken  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLQHPP (FLow QH relation for PumP)
c
c Module description: Subroutine FLQHPP defines the QH-relationship for
c                     a pump.
c
c                     In subroutine FLQHPP the given water level will be
c                     used to determine if the pump should switch on or
c                     off. The user defines the sign of the pump by
c                     giving the capacity a positive or negative value.
c
c                     The pump is controlled by the upward or downward
c                     water level. A reduction factor can be defined as
c                     a function of the head (difference in water
c                     levels).
c
c                     If the start level has been defined above the stop
c                     level the pump starts when the water level exceeds
c                     H-start. The pump stops when the water level drops
c                     below H-stop.
c
c                     If the start level has been defined below the stop
c                     level the pump starts when the water level drops
c                     below H-start. The pump stops when the water level
c                     exceeds H-stop.
c
c                     The sign of the discharge is determined using the
c                     start and stop levels. If the stop level exceeds
c                     the start level the discharge direction is oppo-
c                     site to the controlled water level.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  0 flqhpp            O  Discharge across pump.
c  7 hact              I  Actual water level at gridpoint in branch.
c  6 hd                I  Downstream water level.
c  5 hu                I  Upstream water level.
c  1 istru             I  Number of structure.
c  8 maxtab            I  Maximum number of defined tables.
c 10 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c  9 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  3 strclo(nstru)     O  True if structure is closed.
c  2 strsta            I  Logical indicator.
c 11 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c inttab  INTerpolate in TABle
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flqhpp.pf,v $
c Revision 1.5  1999/03/15  15:50:36  kuipe_j
c tabs removed
c
c Revision 1.4  1995/09/22  10:02:09  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:55:21  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:22  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:02  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:47  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:24  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:54  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Function: Definition of QH-relation for a pump.
c               (stage-discharge relation)
c
c     Declaration of function
c
      real    FLQHPP
      logical EQUAL
c
c     Declaration of parameters:
c
      integer istru, maxtab, ntabm, ntab(4,maxtab)
      integer itab
      logical strsta, strclo(*)
      real    hact, teken, cap, capold, hstart, hstop
      double precision hu, hd
      real    table(ntabm)
c
c     Declaration of local variables:
c
      real    head, f, q 
c
      head = hu - hd
      call INTTAB (ntab(1,itab), ntab(4,itab), table(ntab(2,itab)),
     +             table(ntab(3,itab)), dble(head), f)
c
      if ( hstart .ge. hstop ) then
c        pumping from control side
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
c        pumping towards control side
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
c
c     Structure open or closed ?
c
      if ( strsta ) then
         strclo(istru) = EQUAL(q , 0.)
      endif
c
      FLQHPP = q * teken
c
      end
