      subroutine FLPIDC(time   ,dt1    ,contrl ,icont  ,ngrid  ,
     +                  h      ,q      ,maxtab ,ntabm  ,ntab   ,
     +                  table  ,conhis ,istep  ,lrest  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLPIDC (FLow PID Controller)
c
c Module description: In routine FLPIDC the control parameter will be
c                     computed according to the PID controller.
c
c                     The procedure for the evaluation of the control
c                     parameter in case of a PID controller has been
c                     described extensively in algorithm 3.3 in Appendix
c                     II of the functional design of the flow module
c                     [S-FO-001.5KV].
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 12 conhis(5,ncontr)  IO For each controller some calculated values
c                         must be saved to be used in successive time
c                         steps. This array will also be part of the
c                         restart file. (i is controller number)
c                         (1,i) = Control parameter us,old.
c                           For PID controllers also:
c                         (2,i) = Sum history sigma( e(tn) ).
c                         (3,i) = Deviation e(tn).
c                           For a time controller the following variable
c                           is defined:
c                         (2,i) = Start time (relative to start of
c                                 controller. Only relevant for a
c                                 relative time controller.
c                                 Initial start time is -infinity.
c  3 contrl(17,ncontr) I  Definition of controllers. Structures can be
c                         controlled by controllers:
c                         (1,i) = Controller type:
c                                 ctimcn (1) : Time controller
c                                 chydcn (2) : Hydraulic controller
c                                 cintcn (3) : Interval controller
c                                 cpidcn (4) : PID controller
c                                 Special purpose controllers:
c                                 charcn (5) : Haringvlietsluizen
c                                              (no BOS shell)
c                         (2,i) = Denotes if controller is active.
c                                 1 : True, controller will be called
c                                 0 : False, controller will be skipped
c                         (3,i) = Control parameter:
c                                 ccpcrh (1) : Crest height
c                                 ccpcrw (2) : Crest width
c                                 ccpgat (3) : Gate opening heigth
c                         (4,i) = dummy
c                         (5,i) = Control frequency N (with N >= 1).
c                         - Time controller (type 1):
c                         (6,i) = Table number of time table.
c                         (7,i) = The times in the table can be used in
c                                 three ways:
c                                 0 : Absolute times are specified; time
c                                     zero corresponds to the start of
c                                     the computation.
c                                 1 : Relative times are specified; time
c                                     zero corresponds to the start of
c                                     an active phase of the controller.
c                                 2 : Relative times are specified; an
c                                     active phase of the controller
c                                     starts in the table at the current
c                                     value of the parameter to be
c                                     controlled.
c                         (8,i) = Minimum period of time between the
c                                 start of two active phases of the time
c                                 controller in case of relative times
c                                 (option 1 or 2).
c                         (9,i) = The change in the value of the
c                                 controlled parameter can be limited.
c                                 <>0  = maximum change in value of
c                                        parameter per second ;
c                                 0    = No limit.
c                         - Hydraulic controller (type 2):
c                         (6,i) = Controlled parameter type:
c                                 cconh  (1) : Water level
c                                 cconq  (2) : Discharge
c                                 cconhd (3) : Head difference
c                                 cconu  (4) : Velocity
c                                 cconsd (5) : Stream direction
c                                 cconpd (6) : Pressure difference
c                         (7,i) = Table number: control = f(controlled
c                                 parameter value).
c                         (8,i) = Number of locations involved (1..5).
c                         (9,i) = Controlled parameter location 1 (grid-
c                                 point or structure)
c                                 Grid point for 1, 2, 4, 5;
c                         (10,i)= Controlled parameter location 2 (grid-
c                                 point) etc.
c                         - Interval controller (type 3):
c                         (6,i) = Controlled parameter location 1 (grid-
c                                 point).
c                         (7,i) = Dummy
c                         (8,i) = Controlled parameter type:
c                                 cconh  (1) : Water level
c                                 cconq  (2) : Discharge
c                         (9,i) = Table number: setpoints as function of
c                                 time.
c                         (10,i)= Control interval:
c                                 cintfx (1) : Fixed interval
c                                 cintvr (2) : Variable interval
c                         (11,i)= Parameter DELTA-us (Fixed interval).
c                         (11,i)= Control velocity v (Variable inter-
c                                 val).
c                         (12,i)= Dead band type:
c                                 cdbdfx (1) : Fixed dead band
c                                 cdbdpq (2) : Dead band as percentage
c                                              of discharge
c                         (13,i)= Fixed dead band D (D fixed).
c                         (13,i)= Percentage of actual discharge Q(tn)
c                                 (D variable).
c                         (14,i)= Minimum value Dmin for dead band
c                                 (D variable).
c                         (15,i)= Maximum value Dmax for dead band
c                                 (D variable).
c                         (16,i)= Constant parameter us,min.
c                         (17,i)= Constant parameter us,max.
c                         - PID controller (type 4):
c                         (6,i) = Controlled parameter location 1 (grid-
c                                 point).
c                         (7,i) = Dummy
c                         (8,i) = Controlled parameter type:
c                                 cconh  (1) : Water level
c                                 cconq  (2) : Discharge
c                         (9,i) = Table number: setpoints as function of
c                                 time.
c                         (10,i)= Dummy
c                         (11,i)= Proportional amplification factor Kp.
c                         (12,i)= Integrating amplification factor Ki.
c                         (13,i)= Differential amplification factor Kd.
c                         (14,i)= Parameter u0.
c                         (15,i)= Parameter vmax.
c                         (16,i)= Constant parameter us,min.
c                         (17,i)= Constant parameter us,max.
c  2 dt1               I  Time step.
c  6 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  4 icont             I  Index in array contrl(*,ncontr).
c  8 maxtab            I  Maximum number of defined tables.
c  5 ngrid             I  Number of grid points in network.
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
c  7 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c 11 table             P  -
c  1 time              P  -
c    istep             I  Timestepnumber
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
c $Log: flpidc.pf,v $
c Revision 1.7  1999/03/15  15:50:24  kuipe_j
c tabs removed
c
c Revision 1.6  1996/01/17  14:38:42  kuipe_j
c header update
c
c Revision 1.5  1995/09/22  10:02:04  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/30  12:36:46  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:18  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:18  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:59  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:43  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:21  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:53  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer icont, istep, ngrid, maxtab, ntabm
      integer ntab(4,maxtab)
      real    table(ntabm), contrl(17,*)
      real    conhis(5,*)
      double  precision  h(ngrid), q(ngrid), time ,dt1
      logical lrest
c
c     Declaration of local variables:
c
      integer itab, igr, hydtyp
      real    actval, setval, e , conint
      real    us, usold, usmin, usmax, dus, dusmax, vmax
      real    esum, kp, ki, kd, eold

c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
c     PID Controller
c
c     For description of controller, see
c     Doc. S-FO-001.5KV, Appendix II , section 3
c
c     Local variables extracted from contrl
c
c     conint : control interval in seconds
c     igr    : gridpoint location controlled parameter
c     hydtyp : controlled parameter type
c     itab   : table number setpoint table
c     vmax   : control velocity
c     dus    : parameter delta us
c     usmin  : constant parameter
c     usmax  : constant parameter
c
      igr    = int(contrl(6,icont))
      hydtyp = int(contrl(8,icont))
c
c     Read actual value (actval) of controlled parameter (h or Q)
c
      if      ( hydtyp .eq. cconh ) then
         actval = h(igr)
      else if ( hydtyp .eq. cconq ) then
         actval = q(igr)
      endif
c
c     Compute deviation (e = wanted value - actual value)
c     The wanted value (setval) is read from a table with setpoints for
c     h and Q as function of time by interpolation
c
      itab = int(contrl(9,icont))
      call INTTAB (ntab(1,itab), ntab(4,itab),
     +             table(ntab(2,itab)),
     +             table(ntab(3,itab)),
     +             time, setval )

      e = setval - actval
c
c     Sum over deviations
c
      esum = conhis(2,icont) + e
c
c     Compute control signal us
c
      kp    = contrl(11,icont)
      ki    = contrl(12,icont)
      kd    = contrl(13,icont)

      usold = conhis(1,icont)
      eold  = conhis(3,icont)

c     In case of first step, intialize correctly
      if (istep == 1 .and. .NOT. lrest ) then
         usold = contrl(14, icont)
         esum  = 0.0
         eold  = e
      endif
    
c     In case of getting active after non-active period, reset correctly
      if (conhis(4, icont) == 0) then
         esum = 0.0
         eold = e 
      endif
         
      us = usold + kp*e + ki*esum + kd*(e-eold)

      dus = us - usold

      conint = contrl( 5,icont) * sngl(dt1)
      vmax   = contrl(15,icont)
      dusmax = vmax * conint
c
c     Check maximum allowable diffence between us at present time level
c     and at the previous one.
c
      if      ( dus .lt. -dusmax ) then
         us = usold - dusmax
      else if ( dus .gt.  dusmax ) then
         us = usold + dusmax
      endif
c
c     Check control signal us on maximum and minimum
c
      usmin = contrl(16,icont)
      usmax = contrl(17,icont)
c
      if ( us .lt. usmin ) then
         us = usmin
         esum = esum - e
      endif
      if ( us .gt. usmax ) then
         us   = usmax
         esum = esum - e
      endif
c
c     Store controlled parameter in conhis
c
      conhis(1,icont)   = us
      conhis(2,icont)   = esum
      conhis(3,icont)   = e
c
      end
