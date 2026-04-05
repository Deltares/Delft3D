subroutine flrlti (ncontr ,add , contrl, conhis)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLRLTI (FLow adapt time ReL. TIme contr.      )
!
! Module description: Add a time to the start time of a relative
!                     time controller. This will be done to express
!                     the start time in seconds with respect to the
!                     start ot the current run.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 add               I  -
!  4 conhis(5,ncontr)  IO For each controller some calculated values
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
!  1 ncontr            I  Number of controlled structures.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flrlti.pf,v $
! Revision 1.4  1999/03/15  15:50:42  kuipe_j
! tabs removed
!
! Revision 1.3  1996/04/12  13:04:20  kuipe_j
! headers, minor changes
!
! Revision 1.2  1996/01/17  14:38:46  kuipe_j
! header update
!
! Revision 1.1  1996/01/16  15:01:23  kuipe_j
! Restart improvements
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer ncontr
   real    add      ,contrl(17,*), conhis(5,*)
!
!     Declaration of local variables:
!
   integer icont, contyp
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
   do 10 icont=1,ncontr
      contyp = int(contrl(1,icont))
      if (contyp .eq. ctimcn ) then
!
!           - Time controller -
!
         conhis (2,icont) = conhis(2,icont) + add
      endif
10 continue

end
