subroutine FLPIDC(time   ,dt1    ,contrl ,icont  ,ngrid  ,&
&h      ,q      ,maxtab ,ntabm  ,ntab   ,&
&table  ,conhis ,istep  ,lrest  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLPIDC (FLow PID Controller)
!
! Module description: In routine FLPIDC the control parameter will be
!                     computed according to the PID controller.
!
!                     The procedure for the evaluation of the control
!                     parameter in case of a PID controller has been
!                     described extensively in algorithm 3.3 in Appendix
!                     II of the functional design of the flow module
!                     [S-FO-001.5KV].
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 12 conhis(5,ncontr)  IO For each controller some calculated values
!                         must be saved to be used in successive time
!                         steps. This array will also be part of the
!                         restart file. (i is controller number)
!                         (1,i) = Control parameter us,old.
!                           For PID controllers also:
!                         (2,i) = Sum history sigma( e(tn) ).
!                         (3,i) = Deviation e(tn).
!                           For a time controller the following variable
!                           is defined:
!                         (2,i) = Start time (relative to start of
!                                 controller. Only relevant for a
!                                 relative time controller.
!                                 Initial start time is -infinity.
!  3 contrl(17,ncontr) I  Definition of controllers. Structures can be
!                         controlled by controllers:
!                         (1,i) = Controller type:
!                                 ctimcn (1) : Time controller
!                                 chydcn (2) : Hydraulic controller
!                                 cintcn (3) : Interval controller
!                                 cpidcn (4) : PID controller
!                                 Special purpose controllers:
!                                 charcn (5) : Haringvlietsluizen
!                                              (no BOS shell)
!                         (2,i) = Denotes if controller is active.
!                                 1 : True, controller will be called
!                                 0 : False, controller will be skipped
!                         (3,i) = Control parameter:
!                                 ccpcrh (1) : Crest height
!                                 ccpcrw (2) : Crest width
!                                 ccpgat (3) : Gate opening heigth
!                         (4,i) = dummy
!                         (5,i) = Control frequency N (with N >= 1).
!                         - Time controller (type 1):
!                         (6,i) = Table number of time table.
!                         (7,i) = The times in the table can be used in
!                                 three ways:
!                                 0 : Absolute times are specified; time
!                                     zero corresponds to the start of
!                                     the computation.
!                                 1 : Relative times are specified; time
!                                     zero corresponds to the start of
!                                     an active phase of the controller.
!                                 2 : Relative times are specified; an
!                                     active phase of the controller
!                                     starts in the table at the current
!                                     value of the parameter to be
!                                     controlled.
!                         (8,i) = Minimum period of time between the
!                                 start of two active phases of the time
!                                 controller in case of relative times
!                                 (option 1 or 2).
!                         (9,i) = The change in the value of the
!                                 controlled parameter can be limited.
!                                 <>0  = maximum change in value of
!                                        parameter per second ;
!                                 0    = No limit.
!                         - Hydraulic controller (type 2):
!                         (6,i) = Controlled parameter type:
!                                 cconh  (1) : Water level
!                                 cconq  (2) : Discharge
!                                 cconhd (3) : Head difference
!                                 cconu  (4) : Velocity
!                                 cconsd (5) : Stream direction
!                                 cconpd (6) : Pressure difference
!                         (7,i) = Table number: control = f(controlled
!                                 parameter value).
!                         (8,i) = Number of locations involved (1..5).
!                         (9,i) = Controlled parameter location 1 (grid-
!                                 point or structure)
!                                 Grid point for 1, 2, 4, 5;
!                         (10,i)= Controlled parameter location 2 (grid-
!                                 point) etc.
!                         - Interval controller (type 3):
!                         (6,i) = Controlled parameter location 1 (grid-
!                                 point).
!                         (7,i) = Dummy
!                         (8,i) = Controlled parameter type:
!                                 cconh  (1) : Water level
!                                 cconq  (2) : Discharge
!                         (9,i) = Table number: setpoints as function of
!                                 time.
!                         (10,i)= Control interval:
!                                 cintfx (1) : Fixed interval
!                                 cintvr (2) : Variable interval
!                         (11,i)= Parameter DELTA-us (Fixed interval).
!                         (11,i)= Control velocity v (Variable inter-
!                                 val).
!                         (12,i)= Dead band type:
!                                 cdbdfx (1) : Fixed dead band
!                                 cdbdpq (2) : Dead band as percentage
!                                              of discharge
!                         (13,i)= Fixed dead band D (D fixed).
!                         (13,i)= Percentage of actual discharge Q(tn)
!                                 (D variable).
!                         (14,i)= Minimum value Dmin for dead band
!                                 (D variable).
!                         (15,i)= Maximum value Dmax for dead band
!                                 (D variable).
!                         (16,i)= Constant parameter us,min.
!                         (17,i)= Constant parameter us,max.
!                         - PID controller (type 4):
!                         (6,i) = Controlled parameter location 1 (grid-
!                                 point).
!                         (7,i) = Dummy
!                         (8,i) = Controlled parameter type:
!                                 cconh  (1) : Water level
!                                 cconq  (2) : Discharge
!                         (9,i) = Table number: setpoints as function of
!                                 time.
!                         (10,i)= Dummy
!                         (11,i)= Proportional amplification factor Kp.
!                         (12,i)= Integrating amplification factor Ki.
!                         (13,i)= Differential amplification factor Kd.
!                         (14,i)= Parameter u0.
!                         (15,i)= Parameter vmax.
!                         (16,i)= Constant parameter us,min.
!                         (17,i)= Constant parameter us,max.
!  2 dt1               I  Time step.
!  6 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  4 icont             I  Index in array contrl(*,ncontr).
!  8 maxtab            I  Maximum number of defined tables.
!  5 ngrid             I  Number of grid points in network.
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
!  7 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
! 11 table             P  -
!  1 time              P  -
!    istep             I  Timestepnumber
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
! $Log: flpidc.pf,v $
! Revision 1.7  1999/03/15  15:50:24  kuipe_j
! tabs removed
!
! Revision 1.6  1996/01/17  14:38:42  kuipe_j
! header update
!
! Revision 1.5  1995/09/22  10:02:04  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:36:46  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:18  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:18  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:59  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:43  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:21  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:53  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer icont, istep, ngrid, maxtab, ntabm
   integer ntab(4,maxtab)
   real    table(ntabm), contrl(17,*)
   real    conhis(5,*)
   double  precision  h(ngrid), q(ngrid), time ,dt1
   logical lrest
!
!     Declaration of local variables:
!
   integer itab, igr, hydtyp
   real    actval, setval, e , conint
   real    us, usold, usmin, usmax, dus, dusmax, vmax
   real    esum, kp, ki, kd, eold

!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
!     PID Controller
!
!     For description of controller, see
!     Doc. S-FO-001.5KV, Appendix II , section 3
!
!     Local variables extracted from contrl
!
!     conint : control interval in seconds
!     igr    : gridpoint location controlled parameter
!     hydtyp : controlled parameter type
!     itab   : table number setpoint table
!     vmax   : control velocity
!     dus    : parameter delta us
!     usmin  : constant parameter
!     usmax  : constant parameter
!
   igr    = int(contrl(6,icont))
   hydtyp = int(contrl(8,icont))
!
!     Read actual value (actval) of controlled parameter (h or Q)
!
   if      ( hydtyp .eq. cconh ) then
      actval = h(igr)
   else if ( hydtyp .eq. cconq ) then
      actval = q(igr)
   endif
!
!     Compute deviation (e = wanted value - actual value)
!     The wanted value (setval) is read from a table with setpoints for
!     h and Q as function of time by interpolation
!
   itab = int(contrl(9,icont))
   call INTTAB (ntab(1,itab), ntab(4,itab),&
   &table(ntab(2,itab)),&
   &table(ntab(3,itab)),&
   &time, setval )

   e = setval - actval
!
!     Sum over deviations
!
   esum = conhis(2,icont) + e
!
!     Compute control signal us
!
   kp    = contrl(11,icont)
   ki    = contrl(12,icont)
   kd    = contrl(13,icont)

   usold = conhis(1,icont)
   eold  = conhis(3,icont)

!     In case of first step, intialize correctly
   if (istep == 1 .and. .NOT. lrest ) then
      usold = contrl(14, icont)
      esum  = 0.0
      eold  = e
   endif

!     In case of getting active after non-active period, reset correctly
   if (conhis(4, icont) == 0) then
      esum = 0.0
      eold = e
   endif

   us = usold + kp*e + ki*esum + kd*(e-eold)

   dus = us - usold

   conint = contrl( 5,icont) * real(dt1, kind=kind(conint))
   vmax   = contrl(15,icont)
   dusmax = vmax * conint
!
!     Check maximum allowable diffence between us at present time level
!     and at the previous one.
!
   if      ( dus .lt. -dusmax ) then
      us = usold - dusmax
   else if ( dus .gt.  dusmax ) then
      us = usold + dusmax
   endif
!
!     Check control signal us on maximum and minimum
!
   usmin = contrl(16,icont)
   usmax = contrl(17,icont)
!
   if ( us .lt. usmin ) then
      us = usmin
      esum = esum - e
   endif
   if ( us .gt. usmax ) then
      us   = usmax
      esum = esum - e
   endif
!
!     Store controlled parameter in conhis
!
   conhis(1,icont)   = us
   conhis(2,icont)   = esum
   conhis(3,icont)   = e
!
end
