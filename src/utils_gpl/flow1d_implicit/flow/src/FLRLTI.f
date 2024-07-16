      subroutine flrlti (ncontr ,add , contrl, conhis)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLRLTI (FLow adapt time ReL. TIme contr.      )
c
c Module description: Add a time to the start time of a relative
c                     time controller. This will be done to express
c                     the start time in seconds with respect to the
c                     start ot the current run.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 add               I  -
c  4 conhis(5,ncontr)  IO For each controller some calculated values
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
c  1 ncontr            I  Number of controlled structures.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flrlti.pf,v $
c Revision 1.4  1999/03/15  15:50:42  kuipe_j
c tabs removed
c
c Revision 1.3  1996/04/12  13:04:20  kuipe_j
c headers, minor changes
c
c Revision 1.2  1996/01/17  14:38:46  kuipe_j
c header update
c
c Revision 1.1  1996/01/16  15:01:23  kuipe_j
c Restart improvements
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer ncontr
      real    add      ,contrl(17,*), conhis(5,*)
c
c     Declaration of local variables:
c
      integer icont, contyp
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      do 10 icont=1,ncontr
         contyp = int(contrl(1,icont))
         if (contyp .eq. ctimcn ) then
c
c           - Time controller -
c
            conhis (2,icont) = conhis(2,icont) + add
         endif
   10 continue

      end
