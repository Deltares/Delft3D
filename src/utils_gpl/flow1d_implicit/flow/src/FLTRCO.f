      subroutine FLTRCO(ntcrel, trcnrl, triger, contrl)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLTRCO (FLow TRigger COntroller relations)
c
c Module description: This routine determines for each controller if the
c                     status is active or not.
c
c                     At the start of a new time step all triggers are
c                     evaluated. Using the trigger controller relation
c                     table and the trigger values, this routine
c                     determines the status of the controllers.
c                     A controller which is not triggered is always
c                     active. This status (active/true) will be set
c                     initially.
c                     AND has a higher priority then OR     
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 contrl(17,ncontr) IO Definition of controllers. Structures can be
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
c  1 ntcrel            I  Is the number of trigger controller relations.
c  2 trcnrl(5,ntcrel)  I  Trigger controller relations
c                         (1,i) = Trigger number of trigger related to
c                                 controller at (2,i)
c                         (2,i) = Controller number of controller
c                                 related to trigger at (1,i)
c                         (3,i) = Denotes when controller will be active
c                                 0 : Trigger = false --> controller is active
c                                     (NOT option)
c                                 1 : Trigger = true --> controller is active
c                         (4,i) = AND / OR switch:
c                                 AND (1): The controller status is
c                                          active if this trigger and
c                                          all related triggers (at
c                                          previous entries in the
c                                          relation table) are true
c                                          (after performing the NOT
c                                           switch).
c                                 OR (0): The controller status is
c                                         active if this trigger or one
c                                         of the related triggers (at
c                                         previous entries in the
c                                         relation table) are true
c                                         (after performing the NOT
c                                          switch).
c                                 Remark: Evaluation of AND-s and OR-s
c                                         is from the beginning to the
c                                         end of the relation table.
c                                         They have an equal priority.
c                         (5,i) = Future use (OFF switch ?)
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
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: fltrco.pf,v $
c Revision 1.6  1999/03/15  15:50:58  kuipe_j
c tabs removed
c
c Revision 1.5  1997/05/26  07:44:45  kuipe_j
c Small changes
c
c Revision 1.4  1997/05/07  14:40:23  kuipe_j
c AND higher then OR
c
c Revision 1.3  1996/04/12  13:04:23  kuipe_j
c headers, minor changes
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer ntcrel
      integer trcnrl(5,*), triger(10,*)
      real    contrl(17,*)
c
c     Declaration of local variables
c
      integer i, j, k, iop, temp
c
c     - Controller status of all triggered controllers to undefined.
c     - (undefined = -1)
c     The status field contains temporarily o stautus number of 2 
c     digits. Each digit is 0 (false) or 1 (true).
c     Left digit contains current value of expression left of an OR.
c     right digit contains current value of expression right of an OR.
c     An OR initiates 2 actions:
c     - OR  right and left and put in left
c     - fill current value in right
c
      do 10 i = 1, ntcrel
         k = trcnrl(2,i)
         contrl(2,k) = -1.0
   10 continue
c
c     - Determine controller status
c
      do 20 i = 1, ntcrel
         j   = trcnrl(1,i)
         k   = trcnrl(2,i)
         iop = trcnrl(4,i)
c
c        - contribution of trigger i --> temp
c
         if ( ( triger(1,j) .eq. 1 .AND. trcnrl(3,i) .eq. 1 ) .OR.
     &        ( triger(1,j) .eq. 0 .AND. trcnrl(3,i) .eq. 0 )) then
            temp = 1
         else
            temp = 0
         endif
         if ( int(contrl(2,k)) .eq. -1 ) then
c           - first occurrence -
            contrl(2,k) = temp
         else
            if ( iop .eq. 1 ) then
c              - AND operator -
               if ( mod(int(contrl(2,k)),2) .eq. 1 .and.
     &            temp .eq. 0 ) then
                  contrl(2,k) = int(contrl(2,k)) -  1 
               endif
            else
c              - OR operator -
               if ( int(contrl(2,k)) .eq. 0 ) then
                  contrl(2,k) = temp
               else
                  contrl(2,k) = 10 + temp
               endif
            endif
         endif
   20 continue   
c
c     Perform last OR
c
      do 30 i = 1, ntcrel
         k = trcnrl(2,i)
         contrl(2,k) = min(int(contrl(2,k)),1)
   30 continue
      end
