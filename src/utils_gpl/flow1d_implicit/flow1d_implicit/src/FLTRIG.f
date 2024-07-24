      subroutine FLTRIG(time   ,strtyp ,ntrigr ,triger ,maxtab ,
     +                  ntabm  ,ntab   ,table  ,ngrid  ,h      ,
     +                  q      ,strhis ,nqlat  ,qltpar ,strpar ,
     +                  g      ,rho    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLTRIG (FLow TRIGger)
c
c Module description: In this routine the trigger (ON/OFF) will be com-
c                     puted. If trigger=ON the controller will be trig-
c                     gered.
c
c                     In routine FLTRIG the actual trigger is computed.
c                     First, if required, the 'time trigger' will be
c                     applied. This trigger is ON if the actual simula-
c                     tion time is in a trigger period.
c
c                     After that, if required, the hydraulic trigger is
c                     evaluated. This trigger is ON in case of (under)
c                     exceeding of a user-selected parameter at a user-
c                     selected location.
c
c                     After the evaluation of both triggers it is chec-
c                     ked whether the simulation is in an AND- or an OR-
c                     period.
c
c                     For an AND-period the controller will only be
c                     triggered if both triggers are ON.
c
c                     In case of an OR-period the controller will be
c                     triggered if one of the triggers is ON.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 h                 P  -
c  5 maxtab            I  Maximum number of defined tables.
c  9 ngrid             I  Number of grid points in network.
c  7 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c  6 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  3 ntrigr            I  Number of triggers.
c 11 q                 P  -
c 12 strhis            P  -
c  2 strtyp            P  -
c  8 table             P  -
c  1 time              P  -
c  4 triger(10,ntrigr) IO Array containing parameters for each trigger
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
c flhtrg  FLow Hydraulic TRiGger
c flttrg  FLow Time TRiGger
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
c $Log: fltrig.pf,v $
c Revision 1.6  1999/03/15  15:50:59  kuipe_j
c tabs removed
c
c Revision 1.5  1995/09/22  10:02:25  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/30  12:36:58  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:33  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:35  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:14  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:55  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:43  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:56  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      integer maxtab, ntabm, ngrid, ntrigr, nqlat
      integer strtyp(10,*), ntab(4,maxtab)
      integer triger(10,*)
      real    g
      real    table(ntabm), strhis(dmstrh,*),
     +        qltpar(9,nqlat), rho(ngrid), strpar(dmstrpar,*)
      double  precision     time, h(ngrid), q(ngrid)
c
c     Declaration of local variables:
c
      logical hydtrg, timtrg, trig
      integer andor, itab, itrig
      real    value
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
      do 100 itrig = 1, ntrigr
c
         if ( triger(6,itrig) .eq. candor ) then
c
c            Time and hydraulic trigger used including AND/OR
c
c            itab : table number AND/OR period
c            andor: 1 - AND ; 0 - OR
c
             itab = triger(10,itrig)
             call INTTAB(ntab(1,itab), ntab(4,itab),
     +                   table(ntab(2,itab)),
     +                   table(ntab(3,itab)),
     +                   time, value )
c
c            Determine AND/OR period
c
             andor = int(value)
c
c            Time trigger ON/OFF ?
c
             call FLTTRG(time   ,itrig  ,triger ,maxtab ,
     +                   ntabm  ,ntab   ,table  ,timtrg )
c
c            Hydraulic trigger ON/OFF ?
c
             call FLHTRG(time   ,itrig  ,strtyp ,triger ,maxtab ,
     +                   ntabm  ,ntab   ,table  ,ngrid  ,h      ,
     +                   q      ,strhis ,hydtrg ,nqlat  ,qltpar ,
     +                   strpar ,g      ,rho    )
c
c            Trigger combination ON/OFF ?
c
             trig = .false.
             if ( andor .eq. ctrgan .and. (timtrg .and. hydtrg) ) then
                trig = .true.
             endif
c
             if ( andor .eq. ctrgor .and. (timtrg .or.  hydtrg) ) then
                trig = .true.
             endif
c
          else if ( triger(6,itrig) .eq. ctitrg ) then
c
c            Time trigger used
c
             call FLTTRG(time   ,itrig  ,triger ,maxtab ,
     +                   ntabm  ,ntab   ,table  ,timtrg )
             trig = timtrg
c
          else if ( triger(6,itrig) .eq. chytrg ) then
c
c            Hydraulic trigger used
c
             call FLHTRG(time   ,itrig  ,strtyp ,triger ,maxtab ,
     +                   ntabm  ,ntab   ,table  ,ngrid  ,h      ,
     +                   q      ,strhis ,hydtrg ,nqlat  ,qltpar ,
     +                   strpar ,g      ,rho    )
             trig = hydtrg
          endif
c
c         Store output of trigger (combination) in array -triger-
c
          if ( trig ) then
             triger(1,itrig) = 1
          else
             triger(1,itrig) = 0
          endif
c
  100 continue
      end
