subroutine FLTIMC(curtim ,dt1    ,contrl ,icont  ,maxtab ,ntabm  ,&
&ntab   ,table  ,ncsrel ,cnstrl ,strhis ,conhis )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLTIMC (FLow TIMe Control)
!
! Module description: In subroutine FLTIMC the actual control parameter
!                     for a 'time controlled structure' is performed.
!
!                     For a time controlled structure the control
!                     parameter is defined as a time series. In this
!                     routine a value will be interpolated from a time
!                     table. There are 3 options:
!                     - Interpolation using the times of the table as
!                       they are defined (absolute times);
!                     - Interpolation using relative times; time zero
!                       corresponds to the start of an active phase of
!                       the controller.
!                     - Interpolation from actual value (using relative
!                       times); An active phase of the controller starts
!                       in the table at the actual value of the
!                       parameter to be controlled.
!
!                     The change between 2 steps in the value of the
!                     controlled parameter can be limited. In case of
!                     relative times a new active phase can start after
!                     a user defined minimum period of time.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 cnstrl            P  -
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
!  1 curtim            I  Current time.
!  2 dt1               I  Time step.
!  4 icont             I  Index in array contrl(*,ncontr).
!  5 maxtab            I  Maximum number of defined tables.
!  9 ncsrel            P  -
!  7 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
!  6 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
! 11 strhis(10,nstru)  I  For each structure the discharge and the
!                         parameters to be controlled must be saved to
!                         be able to write to the output file. This will
!                         be done in array strhis(8,nstru). This array
!                         will also be used to check the values of the
!                         controlled parameters or to determine if
!                         increase(open) or decrease(close) of these
!                         parameters occurs. This array will also be
!                         part of the restart file.
!                         (1,i) = Gate height
!                         (2,i) = Crest height
!                         (3,i) = Crest width
!                         (4,i) = Discharge through structure
!                         (5,i) = Gate height at previous time step
!                         (6,i) = Crest height at previous time step
!                         (7,i) = Crest width at previous time step
!                         (8,i) = Flow condition of general structure:
!                                 formno = 0, closed or other structure
!                                 formno = 1, free weir
!                                 formno = 2, drowned weir
!                                 formno = 3, free gate
!                                 formno = 4, drowned gate
!                         (9,i) = coefficient Q-H-realtion asde
!                         (10,i)= coefficient Q-H-realtion bsde
!                         (11,i)= coefficient Q-H-realtion csde
!                         (12,i)= coefficient Q-H-realtion dsde
!  8 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! equal   EQUAL test of two real variables
! flihis  FLow Index renumbering structure HIStory
! fllwst  FLow LoWest Structure
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
! $Log: fltimc.pf,v $
! Revision 1.10  1999/03/15  15:50:54  kuipe_j
! tabs removed
!
! Revision 1.9  1996/05/30  09:56:42  kuipe_j
! general structure dlim, controllers
!
! Revision 1.8  1996/01/17  14:38:55  kuipe_j
! header update
!
! Revision 1.7  1996/01/16  15:01:26  kuipe_j
! Restart improvements
!
! Revision 1.6  1995/11/21  11:08:04  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.5  1995/09/22  10:02:21  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:36:57  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:32  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:33  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:08:12  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:53  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:42  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:56  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters:
!
   integer icont, maxtab, ntabm, ncsrel
   integer ntab(4,maxtab), cnstrl(2,*)
   real    table(ntabm), contrl(17,*)
   real    conhis(5,*), strhis(dmstrh,*)
   double  precision     curtim, dt1
!
!     Declaration of local variables:
!
   double  precision     time
   integer               itab  , istru , icp
   real                  conpar, statim, timper, srchtm, actval
   real                  dc    , dcdtmx
!
!     Declaration of external functions
!
   integer  FLLWST, FLIHIS
   logical  EQUAL
   external FLLWST, FLIHIS, EQUAL
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   statim = conhis(2,icont)
   timper = contrl(8,icont)
   itab   = int(contrl(6,icont))
!
!     - relative time [opt 2]
!
   if ( int(contrl(7,icont)) .eq. ctmrl0 ) then
!        - start time not set or expired ?
      if (curtim .gt. (statim + timper) ) then
!           - store start time (=current time)
         statim = sngl(curtim - dt1)
         conhis(2,icont) = statim
      endif
      time = curtim - dble(statim)
!
!     - interpolation from actual value [opt 3]
!
   else if ( int(contrl(7,icont)) .eq. ctmrlv ) then
!        - start time not set or expired ?
      if ( curtim .gt. (statim + timper) ) then
!
!           - search for first related structure (lowest) and get the
!           - actual value of controlled parameter of that structure
!
         istru  = FLLWST(ncsrel, cnstrl, icont)
         icp    = int(contrl(3,icont))
         actval = strhis(FLIHIS(icp),istru)
!
!           - search time in table that corresponds to actual value
!
         call INTTAB (ntab(1,itab), ntab(4,itab),&
         &table(ntab(3,itab)),&
         &table(ntab(2,itab)),&
         &dble(actval), srchtm )
!
!           - store start time (=searched time)
!
         statim = sngl(curtim - dt1) - srchtm
         conhis(2,icont) = statim
      endif
      time = curtim - dble(statim)
   else
      time = curtim
   endif
!
!     Determine the control parameter for a time controlled structure
!
!     conpar : control parameter value
!     itab   : table number time table
!     istru  : structure number of controlled structure
!
   call INTTAB (ntab(1,itab), ntab(4,itab),&
   &table(ntab(2,itab)),&
   &table(ntab(3,itab)),&
   &time, conpar )
!
!     Change in controlled parameter is limited ?
!
   dcdtmx = contrl(9,icont)
   if ( .not. EQUAL(dcdtmx , 0. )) then
!
!        - search for first related structure (lowest) and get the
!        - previous value of controlled parameter of that structure
!
      istru  = FLLWST(ncsrel, cnstrl, icont)
      icp    = int(contrl(3,icont))
      actval = strhis(FLIHIS(icp),istru)
!
      dc = conpar - actval
      conpar = actval + sign( min(abs(dc), dcdtmx * sngl(dt1) ), dc)
   endif
!
!     Store controlled parameter in -conhis-
!
   conhis(1,icont)   = conpar
!
end
