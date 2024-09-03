      subroutine FLTIMC(curtim ,dt1    ,contrl ,icont  ,maxtab ,ntabm  ,
     +                  ntab   ,table  ,ncsrel ,cnstrl ,strhis ,conhis )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLTIMC (FLow TIMe Control)
c
c Module description: In subroutine FLTIMC the actual control parameter
c                     for a 'time controlled structure' is performed.
c
c                     For a time controlled structure the control
c                     parameter is defined as a time series. In this
c                     routine a value will be interpolated from a time
c                     table. There are 3 options:
c                     - Interpolation using the times of the table as
c                       they are defined (absolute times);
c                     - Interpolation using relative times; time zero
c                       corresponds to the start of an active phase of
c                       the controller.
c                     - Interpolation from actual value (using relative
c                       times); An active phase of the controller starts
c                       in the table at the actual value of the
c                       parameter to be controlled.
c
c                     The change between 2 steps in the value of the
c                     controlled parameter can be limited. In case of
c                     relative times a new active phase can start after
c                     a user defined minimum period of time.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 cnstrl            P  -
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
c  1 curtim            I  Current time.
c  2 dt1               I  Time step.
c  4 icont             I  Index in array contrl(*,ncontr).
c  5 maxtab            I  Maximum number of defined tables.
c  9 ncsrel            P  -
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
c 11 strhis(10,nstru)  I  For each structure the discharge and the
c                         parameters to be controlled must be saved to
c                         be able to write to the output file. This will
c                         be done in array strhis(8,nstru). This array
c                         will also be used to check the values of the
c                         controlled parameters or to determine if
c                         increase(open) or decrease(close) of these
c                         parameters occurs. This array will also be
c                         part of the restart file.
c                         (1,i) = Gate height
c                         (2,i) = Crest height
c                         (3,i) = Crest width
c                         (4,i) = Discharge through structure
c                         (5,i) = Gate height at previous time step
c                         (6,i) = Crest height at previous time step
c                         (7,i) = Crest width at previous time step
c                         (8,i) = Flow condition of general structure:
c                                 formno = 0, closed or other structure
c                                 formno = 1, free weir
c                                 formno = 2, drowned weir
c                                 formno = 3, free gate
c                                 formno = 4, drowned gate
c                         (9,i) = coefficient Q-H-realtion asde
c                         (10,i)= coefficient Q-H-realtion bsde
c                         (11,i)= coefficient Q-H-realtion csde
c                         (12,i)= coefficient Q-H-realtion dsde
c  8 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c equal   EQUAL test of two real variables
c flihis  FLow Index renumbering structure HIStory
c fllwst  FLow LoWest Structure
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
c $Log: fltimc.pf,v $
c Revision 1.10  1999/03/15  15:50:54  kuipe_j
c tabs removed
c
c Revision 1.9  1996/05/30  09:56:42  kuipe_j
c general structure dlim, controllers
c
c Revision 1.8  1996/01/17  14:38:55  kuipe_j
c header update
c
c Revision 1.7  1996/01/16  15:01:26  kuipe_j
c Restart improvements
c
c Revision 1.6  1995/11/21  11:08:04  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.5  1995/09/22  10:02:21  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/30  12:36:57  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:32  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:33  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:08:12  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:53  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:42  kuipe_j
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
      integer icont, maxtab, ntabm, ncsrel
      integer ntab(4,maxtab), cnstrl(2,*)
      real    table(ntabm), contrl(17,*)
      real    conhis(5,*), strhis(dmstrh,*)
      double  precision     curtim, dt1
c
c     Declaration of local variables:
c
      double  precision     time
      integer               itab  , istru , icp
      real                  conpar, statim, timper, srchtm, actval
      real                  dc    , dcdtmx
c
c     Declaration of external functions
c
      integer  FLLWST, FLIHIS
      logical  EQUAL
      external FLLWST, FLIHIS, EQUAL
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
      statim = conhis(2,icont)
      timper = contrl(8,icont)
      itab   = int(contrl(6,icont))
c
c     - relative time [opt 2]
c
      if ( int(contrl(7,icont)) .eq. ctmrl0 ) then
c        - start time not set or expired ?
         if (curtim .gt. (statim + timper) ) then
c           - store start time (=current time)
            statim = real(curtim - dt1, kind=kind(statim))
            conhis(2,icont) = statim
         endif
         time = curtim - dble(statim)
c
c     - interpolation from actual value [opt 3]
c
      else if ( int(contrl(7,icont)) .eq. ctmrlv ) then
c        - start time not set or expired ?
         if ( curtim .gt. (statim + timper) ) then
c
c           - search for first related structure (lowest) and get the
c           - actual value of controlled parameter of that structure
c
            istru  = FLLWST(ncsrel, cnstrl, icont)
            icp    = int(contrl(3,icont))
            actval = strhis(FLIHIS(icp),istru)
c
c           - search time in table that corresponds to actual value
c
            call INTTAB (ntab(1,itab), ntab(4,itab),
     +                   table(ntab(3,itab)),
     +                   table(ntab(2,itab)),
     +                   dble(actval), srchtm )
c
c           - store start time (=searched time)
c
            statim = real(curtim - dt1, kind=kind(statim)) - srchtm
            conhis(2,icont) = statim
         endif
         time = curtim - dble(statim)
      else
         time = curtim
      endif
c
c     Determine the control parameter for a time controlled structure
c
c     conpar : control parameter value
c     itab   : table number time table
c     istru  : structure number of controlled structure
c
      call INTTAB (ntab(1,itab), ntab(4,itab),
     +             table(ntab(2,itab)),
     +             table(ntab(3,itab)),
     +             time, conpar )
c
c     Change in controlled parameter is limited ?
c
      dcdtmx = contrl(9,icont)
      if ( .not. EQUAL(dcdtmx , 0. )) then
c
c        - search for first related structure (lowest) and get the
c        - previous value of controlled parameter of that structure
c
         istru  = FLLWST(ncsrel, cnstrl, icont)
         icp    = int(contrl(3,icont))
         actval = strhis(FLIHIS(icp),istru)
c
         dc = conpar - actval
         conpar = actval + sign( min(abs(dc), dcdtmx * real(dt1) ), dc)
      endif
c
c     Store controlled parameter in -conhis-
c
      conhis(1,icont)   = conpar
c
      end
