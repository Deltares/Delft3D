subroutine FLISTR(inires, nstru , ncsrel, ncontr, strtyp, strpar,&
&strhis, cnstrl, conhis, contrl, juer  , ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLISTR (FLow Initialize STRucture related data)
!
! Module description: In subroutine FLISTR the controlled parameters of
!                     structures (gate heigth, crest width etc.)  will
!                     be initialized. There are two options:
!
!                     Option 1:    Initial water flow will be read from
!                                  the restart file created in a
!                                  previous SOBEK run;
!                     Option 2:    Initial conditions are available from
!                                  the user (or auto start);
!
!                     In case of an initial run the values of the
!                     control parameters (gate heigth, crest width etc.)
!                     which are stored in array strpar must be copied to
!                     data structures used by triggers, controllers and
!                     output routines.
!
!                     In case a restart file has been found the last
!                     values of the controlled parameters of structures
!                     (gate heigth, crest width etc.) which are stored
!                     in the data structure -strhis-, will be assigned
!                     to the structure parameters in array -strpar-.
!
!                     The next step is activation of untriggered
!                     controllers.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 cnstrl            P  -
!  9 conhis(5,ncontr)  O  For each controller some calculated values
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
! 10 contrl(17,ncontr) IO Definition of controllers. Structures can be
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
!  1 inires            I  True when no restart info of this module has
!                         been written before.
!  4 ncontr            I  Number of controlled structures.
!  3 ncsrel            P  -
!  2 nstru             I  Number of structures.
!  7 strhis(10,nstru)  IO For each structure the discharge and the
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
!  6 strpar            P  -
!  5 strtyp            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flcnpa  FLow put or get CoNtrolled PArameter
! flihis  FLow Index renumbering structure HIStory
! fllwst  FLow LoWest Structure
! flrlti  FLow adapt time ReL. TIme contr.
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flistr.pf,v $
! Revision 1.7  1999/03/15  15:50:12  kuipe_j
! tabs removed
!
! Revision 1.6  1996/09/03  14:52:04  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.5  1996/04/12  13:03:53  kuipe_j
! headers, minor changes
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters
!
   integer nstru, ncsrel, ncontr, juer, ker
   integer strtyp(10,*), cnstrl(2,*)
   real    strhis(dmstrh,*), strpar(dmstrpar,*), conhis(5,*),&
   &contrl(17,*)
   logical inires
!
!     Declaration of local variables
!
   integer put, get, i, j, k, parnr
   parameter (put=1, get=2)
!
!     Declaration of external functions
!
   integer  FLLWST, FLIHIS
   external FLLWST, FLIHIS
!
!     If no restart file found :
!
   if (inires) then
      do 20 i = 1, nstru
         do 10 j = 1, 3
            k = FLIHIS(j)
!
!              - Copy control parameters from -strpar- to -strhis-
!
            call FLCNPA (get, strtyp(1,i), i, j, strhis(k,i),&
            &strpar, juer, ker)
!
!              - Set previous values (indexes 5-7) in -strhis- to
!                current values (indexes 1-3)
!
            strhis(k+4,i) = strhis(k,i)
10       continue
!
!           Discharge through structure initial value zero
!
         strhis(4,i) = 0.
20    continue

!
      do 30 i = 1, ncontr
         do 30 j = 1, 5
            conhis(j,i) = 0.
30    continue
!
!     - copy control signal to conhis
!
      do 50 i = 1, ncontr
         j = int(contrl(3,i))
         k = FLLWST(ncsrel ,cnstrl ,i)
         conhis(1,i) = strhis(FLIHIS(j),k)
50    continue
!
!        Set start times of active controller phases initially
!        to minus infinity.
!
      call flrlti (ncontr ,-1E30 , contrl, conhis)
   else
!
!        - copy control parameters from -strhis- to -strpar-
!
      do 70 i = 1, nstru
         do 60 j = 1, 3
!
!              A negative sign will be added to initiate
!              check on change of control value at restart.
!              This check will only be carried out at an
!              uncontrolled structure
!
            parnr = -j
            do k=1,ncsrel
               if (cnstrl(2,k) .eq. i) then
                  if (int(contrl(3,cnstrl(1,k))) .eq. j) then
                     parnr = j
                     goto 55
                  endif
               endif
            enddo
55          continue
            call FLCNPA (put, strtyp(1,i), i, parnr ,&
            &strhis(FLIHIS(j),i), strpar, juer, ker)
60       continue
70    continue
!
!        - set all controllers active
!
      do 80 i = 1, ncontr
         contrl(2,i) = 1.
80    continue
   endif
!
end
