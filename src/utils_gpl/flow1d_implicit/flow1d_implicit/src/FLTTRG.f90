subroutine FLTTRG(time   ,itrig  ,triger ,maxtab ,ntabm  ,&
&ntab   ,table  ,timtrg )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLTTRG (FLow Time TRiGger)
!
! Module description: Determine if time trigger is active.
!
!                     To determine if a time trigger is active the time
!                     table will be read. In the time table periods will
!                     be defined during which the trigger is ON or OFF.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 itrig             I  Trigger number.
!  4 maxtab            I  Maximum number of defined tables.
!  6 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  5 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  7 table             P  -
!  1 time              P  -
!  8 timtrg            O  =TRUE indicates time trigger is ON
!                         =FALSE indicates time trigger is OFF.
!  3 triger(10,ntrigr) I  Array containing parameters for each trigger
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
! $Log: flttrg.pf,v $
! Revision 1.5  1995/09/22  10:02:28  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:36:59  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:34  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:35  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:14  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:57  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:45  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:56  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer itrig, maxtab, ntabm
   integer triger(10,*), ntab(4,maxtab)
   real    table(ntabm)
   double  precision    time
   logical timtrg
!
!     Declaration of local variables:
!
   integer itab, onoff
   real    value
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
!     itab : table number time table
!     timtrg: true/false - time trigger on/off
!
   itab = triger(7,itrig)
   call INTTAB (ntab(1,itab), ntab(4,itab),&
   &table(ntab(2,itab)),&
   &table(ntab(3,itab)),&
   &time, value )
!
   onoff = int(value)
   timtrg = onoff .eq. ctrgon
!
end
