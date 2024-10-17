subroutine FLTRIG(time   ,strtyp ,ntrigr ,triger ,maxtab ,&
&ntabm  ,ntab   ,table  ,ngrid  ,h      ,&
&q      ,strhis ,nqlat  ,qltpar ,strpar ,&
&g      ,rho    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLTRIG (FLow TRIGger)
!
! Module description: In this routine the trigger (ON/OFF) will be com-
!                     puted. If trigger=ON the controller will be trig-
!                     gered.
!
!                     In routine FLTRIG the actual trigger is computed.
!                     First, if required, the 'time trigger' will be
!                     applied. This trigger is ON if the actual simula-
!                     tion time is in a trigger period.
!
!                     After that, if required, the hydraulic trigger is
!                     evaluated. This trigger is ON in case of (under)
!                     exceeding of a user-selected parameter at a user-
!                     selected location.
!
!                     After the evaluation of both triggers it is chec-
!                     ked whether the simulation is in an AND- or an OR-
!                     period.
!
!                     For an AND-period the controller will only be
!                     triggered if both triggers are ON.
!
!                     In case of an OR-period the controller will be
!                     triggered if one of the triggers is ON.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 h                 P  -
!  5 maxtab            I  Maximum number of defined tables.
!  9 ngrid             I  Number of grid points in network.
!  7 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  6 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  3 ntrigr            I  Number of triggers.
! 11 q                 P  -
! 12 strhis            P  -
!  2 strtyp            P  -
!  8 table             P  -
!  1 time              P  -
!  4 triger(10,ntrigr) IO Array containing parameters for each trigger
!                         or trigger combination.
!                         (1,i) = Output of trigger (combination):
!                                 1 : True
!                                 0 : False
!                         (2,i) = dummy
!                         (3,i) = dummy
!                         (4,i) = dummy
!                         (5,i) = dummy
!                         (6,i) = trigger parameter
!                                 cnotrg (0) : No trigger defined
!                                 ctitrg (1) : Time trigger used
!                                 chytrg (2) : Hydraulic trigger used
!                                 candor (3) : Time & hydraulic trigger
!                                              used (and/or mode)
!                         (7,i) = table number of time trigger
!                         (8,i) = table number 1 for hydraulic trigger
!                         (9,i) = table number 2 for hydraulic trigger
!                         (10,i)= table number of and/or period
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flhtrg  FLow Hydraulic TRiGger
! flttrg  FLow Time TRiGger
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
! $Log: fltrig.pf,v $
! Revision 1.6  1999/03/15  15:50:59  kuipe_j
! tabs removed
!
! Revision 1.5  1995/09/22  10:02:25  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:36:58  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:33  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:35  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:14  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:55  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:43  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:56  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer maxtab, ntabm, ngrid, ntrigr, nqlat
   integer strtyp(10,*), ntab(4,maxtab)
   integer triger(10,*)
   real    g
   real    table(ntabm), strhis(dmstrh,*),&
   &qltpar(9,nqlat), rho(ngrid), strpar(dmstrpar,*)
   double  precision     time, h(ngrid), q(ngrid)
!
!     Declaration of local variables:
!
   logical hydtrg, timtrg, trig
   integer andor, itab, itrig
   real    value
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   do 100 itrig = 1, ntrigr
!
      if ( triger(6,itrig) .eq. candor ) then
!
!            Time and hydraulic trigger used including AND/OR
!
!            itab : table number AND/OR period
!            andor: 1 - AND ; 0 - OR
!
         itab = triger(10,itrig)
         call INTTAB(ntab(1,itab), ntab(4,itab),&
         &table(ntab(2,itab)),&
         &table(ntab(3,itab)),&
         &time, value )
!
!            Determine AND/OR period
!
         andor = int(value)
!
!            Time trigger ON/OFF ?
!
         call FLTTRG(time   ,itrig  ,triger ,maxtab ,&
         &ntabm  ,ntab   ,table  ,timtrg )
!
!            Hydraulic trigger ON/OFF ?
!
         call FLHTRG(time   ,itrig  ,strtyp ,triger ,maxtab ,&
         &ntabm  ,ntab   ,table  ,ngrid  ,h      ,&
         &q      ,strhis ,hydtrg ,nqlat  ,qltpar ,&
         &strpar ,g      ,rho    )
!
!            Trigger combination ON/OFF ?
!
         trig = .false.
         if ( andor .eq. ctrgan .and. (timtrg .and. hydtrg) ) then
            trig = .true.
         endif
!
         if ( andor .eq. ctrgor .and. (timtrg .or.  hydtrg) ) then
            trig = .true.
         endif
!
      else if ( triger(6,itrig) .eq. ctitrg ) then
!
!            Time trigger used
!
         call FLTTRG(time   ,itrig  ,triger ,maxtab ,&
         &ntabm  ,ntab   ,table  ,timtrg )
         trig = timtrg
!
      else if ( triger(6,itrig) .eq. chytrg ) then
!
!            Hydraulic trigger used
!
         call FLHTRG(time   ,itrig  ,strtyp ,triger ,maxtab ,&
         &ntabm  ,ntab   ,table  ,ngrid  ,h      ,&
         &q      ,strhis ,hydtrg ,nqlat  ,qltpar ,&
         &strpar ,g      ,rho    )
         trig = hydtrg
      endif
!
!         Store output of trigger (combination) in array -triger-
!
      if ( trig ) then
         triger(1,itrig) = 1
      else
         triger(1,itrig) = 0
      endif
!
100 continue
end
