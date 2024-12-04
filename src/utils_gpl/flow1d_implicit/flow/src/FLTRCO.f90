subroutine FLTRCO(ntcrel, trcnrl, triger, contrl)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLTRCO (FLow TRigger COntroller relations)
!
! Module description: This routine determines for each controller if the
!                     status is active or not.
!
!                     At the start of a new time step all triggers are
!                     evaluated. Using the trigger controller relation
!                     table and the trigger values, this routine
!                     determines the status of the controllers.
!                     A controller which is not triggered is always
!                     active. This status (active/true) will be set
!                     initially.
!                     AND has a higher priority then OR
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 contrl(17,ncontr) IO Definition of controllers. Structures can be
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
!  1 ntcrel            I  Is the number of trigger controller relations.
!  2 trcnrl(5,ntcrel)  I  Trigger controller relations
!                         (1,i) = Trigger number of trigger related to
!                                 controller at (2,i)
!                         (2,i) = Controller number of controller
!                                 related to trigger at (1,i)
!                         (3,i) = Denotes when controller will be active
!                                 0 : Trigger = false --> controller is active
!                                     (NOT option)
!                                 1 : Trigger = true --> controller is active
!                         (4,i) = AND / OR switch:
!                                 AND (1): The controller status is
!                                          active if this trigger and
!                                          all related triggers (at
!                                          previous entries in the
!                                          relation table) are true
!                                          (after performing the NOT
!                                           switch).
!                                 OR (0): The controller status is
!                                         active if this trigger or one
!                                         of the related triggers (at
!                                         previous entries in the
!                                         relation table) are true
!                                         (after performing the NOT
!                                          switch).
!                                 Remark: Evaluation of AND-s and OR-s
!                                         is from the beginning to the
!                                         end of the relation table.
!                                         They have an equal priority.
!                         (5,i) = Future use (OFF switch ?)
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
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: fltrco.pf,v $
! Revision 1.6  1999/03/15  15:50:58  kuipe_j
! tabs removed
!
! Revision 1.5  1997/05/26  07:44:45  kuipe_j
! Small changes
!
! Revision 1.4  1997/05/07  14:40:23  kuipe_j
! AND higher then OR
!
! Revision 1.3  1996/04/12  13:04:23  kuipe_j
! headers, minor changes
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer ntcrel
   integer trcnrl(5,*), triger(10,*)
   real    contrl(17,*)
!
!     Declaration of local variables
!
   integer i, j, k, iop, temp
!
!     - Controller status of all triggered controllers to undefined.
!     - (undefined = -1)
!     The status field contains temporarily o stautus number of 2
!     digits. Each digit is 0 (false) or 1 (true).
!     Left digit contains current value of expression left of an OR.
!     right digit contains current value of expression right of an OR.
!     An OR initiates 2 actions:
!     - OR  right and left and put in left
!     - fill current value in right
!
   do 10 i = 1, ntcrel
      k = trcnrl(2,i)
      contrl(2,k) = -1.0
10 continue
!
!     - Determine controller status
!
   do 20 i = 1, ntcrel
      j   = trcnrl(1,i)
      k   = trcnrl(2,i)
      iop = trcnrl(4,i)
!
!        - contribution of trigger i --> temp
!
      if ( ( triger(1,j) .eq. 1 .AND. trcnrl(3,i) .eq. 1 ) .OR.&
      &( triger(1,j) .eq. 0 .AND. trcnrl(3,i) .eq. 0 )) then
         temp = 1
      else
         temp = 0
      endif
      if ( int(contrl(2,k)) .eq. -1 ) then
!           - first occurrence -
         contrl(2,k) = temp
      else
         if ( iop .eq. 1 ) then
!              - AND operator -
            if ( mod(int(contrl(2,k)),2) .eq. 1 .and.&
            &temp .eq. 0 ) then
               contrl(2,k) = int(contrl(2,k)) -  1
            endif
         else
!              - OR operator -
            if ( int(contrl(2,k)) .eq. 0 ) then
               contrl(2,k) = temp
            else
               contrl(2,k) = 10 + temp
            endif
         endif
      endif
20 continue
!
!     Perform last OR
!
   do 30 i = 1, ntcrel
      k = trcnrl(2,i)
      contrl(2,k) = min(int(contrl(2,k)),1)
30 continue
end
