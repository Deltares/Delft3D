subroutine FLINTC(time   ,dt1    ,contrl ,icont  ,ngrid  ,&
&h      ,q      ,maxtab ,ntabm  ,ntab   ,&
&table  ,conhis )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLINTC (FLow INTerval Controller)
!
! Module description: In routine FLINTC the control parameter will be
!                     computed according to the INTERVAL controller.
!
!                     The procedure for the evaluation of the control
!                     parameter in case of an INTERVAL controller has
!                     been described extensively in algorithm 2.3 in
!                     Appendix II of the functional design of the flow
!                     module [S-FO-001.5KV].
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
! $Log: flintc.pf,v $
! Revision 1.7  1999/03/15  15:50:10  kuipe_j
! tabs removed
!
! Revision 1.6  1996/01/17  14:38:35  kuipe_j
! header update
!
! Revision 1.5  1995/09/22  10:01:50  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:36:41  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:13  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:12  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:54  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:37  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:12  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:52  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer icont, ngrid, maxtab, ntabm
   integer ntab(4,maxtab)
   real    table(ntabm), contrl(17,*)
   real    conhis(5,*)
   double  precision  time ,dt1, h(ngrid), q(ngrid)
!
!     Declaration of local variables:
!
   integer itab, igr, hydtyp, contyp, dbtype
   real    actval, setval, e, d, perc, dmin, dmax ,conint
   real    us, usold, usmin, usmax, dus, v
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
!     Interval Controller
!
!     For description of controller, see
!     Doc. S-FO-001.5KV, Appendix II , section 2
!
!     Local variables extracted from contrl
!
!     igr    : gridpoint location controlled parameter
!     hydtyp : type of controlled hydraulic parameter (Q or h)
!     contyp : control interval type
!     conint : control interval in seconds
!     itab   : table number setpoint table
!     dbtype : dead band type
!     dmin   : minimum value for dead band
!     dmax   : maximum value for dead band
!     perc   : percentage of actual discharge
!     v      : control velocity
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
   else if ( hydtyp .eq. cconq) then
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
!     Determine dead band D (depending on dbtype)
!
   dbtype = int(contrl(12,icont))
!
!        Dead band type:
!        dbtype = 1 : fixed dead band
!               = 2 : dead band as percentage of discharge
!
   if      ( dbtype .eq. cdbdfx ) then
      d = contrl(13,icont)
   else if ( dbtype .eq. cdbdpq ) then
      perc = contrl(13,icont)
      d    = perc * q(igr) /100.
      dmin = contrl(14,icont)
      dmax = contrl(15,icont)
      d    = min( d , dmax )
      d    = max( d , dmin )
   endif
!
   usold = conhis(1,icont)
   usmin = contrl(16,icont)
   usmax = contrl(17,icont)
!
!     Determine interval dus
!
!     contyp = 1 : fixed interval
!            = 2 : variable interval
!
   conint = contrl( 5,icont) * sngl(dt1)
   contyp = int(contrl(10,icont))
!
   if      ( contyp .eq. cintfx ) then
      dus = contrl(11,icont)
   else if ( contyp .eq. cintvr ) then
      v   = contrl(11,icont)
      dus = v * conint
   endif
!
!     Compute contoller us
!
   if      ( e .ge. -0.5*d  .and. e .le. 0.5*d ) then
      us = usold
   else if ( e .lt. -0.5*d ) then
      us = usold + dus
   else if ( e .gt.  0.5*d ) then
      us = usold - dus
   endif
   us = min(us,usmax)
   us = max(us,usmin)
!
!     Store controlled parameter in conhis
!
   conhis(1,icont)   = us
end
