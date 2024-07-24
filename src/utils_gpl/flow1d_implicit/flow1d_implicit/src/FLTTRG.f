      subroutine FLTTRG(time   ,itrig  ,triger ,maxtab ,ntabm  ,
     +                  ntab   ,table  ,timtrg )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLTTRG (FLow Time TRiGger)
c
c Module description: Determine if time trigger is active.
c
c                     To determine if a time trigger is active the time
c                     table will be read. In the time table periods will
c                     be defined during which the trigger is ON or OFF.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 itrig             I  Trigger number.
c  4 maxtab            I  Maximum number of defined tables.
c  6 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c  5 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  7 table             P  -
c  1 time              P  -
c  8 timtrg            O  =TRUE indicates time trigger is ON
c                         =FALSE indicates time trigger is OFF.
c  3 triger(10,ntrigr) I  Array containing parameters for each trigger
c                         or trigger combination.
c                         (1,i) = Output of trigger (combination):
c                                 1 : True
c                                 0 : False
c                         (2,i) = dummy
c                         (3,i) = dummy
c                         (4,i) = dummy
c                         (5,i) = dummy
c                         (6,i) = trigger parameter
c                                 cnotrg (0) : No trigger defined
c                                 ctitrg (1) : Time trigger used
c                                 chytrg (2) : Hydraulic trigger used
c                                 candor (3) : Time & hydraulic trigger
c                                              used (and/or mode)
c                         (7,i) = table number of time trigger
c                         (8,i) = table number 1 for hydraulic trigger
c                         (9,i) = table number 2 for hydraulic trigger
c                         (10,i)= table number of and/or period
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
c $Log: flttrg.pf,v $
c Revision 1.5  1995/09/22  10:02:28  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/30  12:36:59  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:34  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:35  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:14  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:57  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:45  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:56  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer itrig, maxtab, ntabm
      integer triger(10,*), ntab(4,maxtab)
      real    table(ntabm)
      double  precision    time
      logical timtrg
c
c     Declaration of local variables:
c
      integer itab, onoff
      real    value
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
c     itab : table number time table
c     timtrg: true/false - time trigger on/off
c
      itab = triger(7,itrig)
      call INTTAB (ntab(1,itab), ntab(4,itab),
     +             table(ntab(2,itab)),
     +             table(ntab(3,itab)),
     +             time, value )
c
      onoff = int(value)
      timtrg = onoff .eq. ctrgon
c
      end
