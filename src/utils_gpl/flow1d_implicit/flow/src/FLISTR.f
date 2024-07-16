      subroutine FLISTR(inires, nstru , ncsrel, ncontr, strtyp, strpar,
     &                  strhis, cnstrl, conhis, contrl, juer  , ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLISTR (FLow Initialize STRucture related data)
c
c Module description: In subroutine FLISTR the controlled parameters of
c                     structures (gate heigth, crest width etc.)  will
c                     be initialized. There are two options:
c
c                     Option 1:    Initial water flow will be read from
c                                  the restart file created in a
c                                  previous SOBEK run;
c                     Option 2:    Initial conditions are available from
c                                  the user (or auto start);
c
c                     In case of an initial run the values of the
c                     control parameters (gate heigth, crest width etc.)
c                     which are stored in array strpar must be copied to
c                     data structures used by triggers, controllers and
c                     output routines.
c
c                     In case a restart file has been found the last
c                     values of the controlled parameters of structures
c                     (gate heigth, crest width etc.) which are stored
c                     in the data structure -strhis-, will be assigned
c                     to the structure parameters in array -strpar-.
c
c                     The next step is activation of untriggered
c                     controllers.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  8 cnstrl            P  -
c  9 conhis(5,ncontr)  O  For each controller some calculated values
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
c 10 contrl(17,ncontr) IO Definition of controllers. Structures can be
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
c  1 inires            I  True when no restart info of this module has
c                         been written before.
c  4 ncontr            I  Number of controlled structures.
c  3 ncsrel            P  -
c  2 nstru             I  Number of structures.
c  7 strhis(10,nstru)  IO For each structure the discharge and the
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
c  6 strpar            P  -
c  5 strtyp            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flcnpa  FLow put or get CoNtrolled PArameter
c flihis  FLow Index renumbering structure HIStory
c fllwst  FLow LoWest Structure
c flrlti  FLow adapt time ReL. TIme contr.
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flistr.pf,v $
c Revision 1.7  1999/03/15  15:50:12  kuipe_j
c tabs removed
c
c Revision 1.6  1996/09/03  14:52:04  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.5  1996/04/12  13:03:53  kuipe_j
c headers, minor changes
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '..\include\sobdim.i'
c
c     Declaration of parameters
c
      integer nstru, ncsrel, ncontr, juer, ker
      integer strtyp(10,*), cnstrl(2,*)
      real    strhis(dmstrh,*), strpar(dmstrpar,*), conhis(5,*),
     &        contrl(17,*)
      logical inires
c
c     Declaration of local variables
c
      integer put, get, i, j, k, parnr
      parameter (put=1, get=2)
c
c     Declaration of external functions
c
      integer  FLLWST, FLIHIS
      external FLLWST, FLIHIS
c
c     If no restart file found :
c
      if (inires) then
         do 20 i = 1, nstru
            do 10 j = 1, 3
               k = FLIHIS(j)
c
c              - Copy control parameters from -strpar- to -strhis-
c
               call FLCNPA (get, strtyp(1,i), i, j, strhis(k,i),
     &                      strpar, juer, ker)
c
c              - Set previous values (indexes 5-7) in -strhis- to
c                current values (indexes 1-3)
c
               strhis(k+4,i) = strhis(k,i)
   10       continue
c
c           Discharge through structure initial value zero
c
            strhis(4,i) = 0.
   20    continue

c
         do 30 i = 1, ncontr
            do 30 j = 1, 5
               conhis(j,i) = 0.
   30    continue
c
c     - copy control signal to conhis
c
         do 50 i = 1, ncontr
            j = int(contrl(3,i))
            k = FLLWST(ncsrel ,cnstrl ,i)
            conhis(1,i) = strhis(FLIHIS(j),k)
   50    continue
c
c        Set start times of active controller phases initially
c        to minus infinity.
c
         call flrlti (ncontr ,-1E30 , contrl, conhis)
      else
c
c        - copy control parameters from -strhis- to -strpar-
c
         do 70 i = 1, nstru
            do 60 j = 1, 3
c
c              A negative sign will be added to initiate
c              check on change of control value at restart.
c              This check will only be carried out at an
c              uncontrolled structure 
c
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
               call FLCNPA (put, strtyp(1,i), i, parnr , 
     &                      strhis(FLIHIS(j),i), strpar, juer, ker)
   60       continue
   70    continue
c
c        - set all controllers active
c
         do 80 i = 1, ncontr
            contrl(2,i) = 1.
   80    continue
      endif
c
      end
