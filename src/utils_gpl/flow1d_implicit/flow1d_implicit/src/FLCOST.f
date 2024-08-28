      subroutine FLCOST(ncsrel, nstru , strtyp, strpar, strhis,
     &                  cnstrl, conhis, contrl, cnpflg, juer  , ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLCOST (FLow COntroller STructure relations)
c
c Module description: This routine determines the values of the
c                     controlled parameters of all structures.
c
c                     At entry of this routine the status of all
c                     controllers are known (active / not active).
c                     Using the controller-structure relation table and
c                     the status, this routine sets the controlled
c                     parameters of all related structures. Parameters
c                     of uncontrolled structures will hold their
c                     initial values.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  5 cnstrl(2,ncsrel)  I  table for controller structure relations.
c                         (1,i) = Controller number of controller
c                                 related to structure at (2,i)
c                         (2,i) = Structure number of structure related
c                                 to controller at (1,i)
c  6 conhis(5,ncontr)  I  For each controller some calculated values
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
c  7 contrl(17,ncontr) I  Definition of controllers. Structures can be
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
c  1 ncsrel            I  number of controller structure relations.
c  4 strhis(10,nstru)  O  For each structure the discharge and the
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
c  3 strpar            P  -
c  2 strtyp            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flcnpa  FLow put or get CoNtrolled PArameter
c flihis  FLow Index renumbering structure HIStory
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flcost.pf,v $
c Revision 1.8  1999/06/01  13:42:15  kuipe_j
c names in messages substituted + message template
c
c Revision 1.7  1999/03/15  15:49:43  kuipe_j
c tabs removed
c
c Revision 1.6  1996/11/01  15:04:10  kuipe_j
c Improve contoller messages
c
c Revision 1.5  1996/09/03  14:51:52  kuipe_j
c frequency time hist,Messages controllers added
c
c Revision 1.4  1996/04/12  13:03:43  kuipe_j
c headers, minor changes
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters
c
      integer ncsrel          , nstru       , juer , ker 
      integer strtyp(10,*)    , cnstrl(2,*) , cnpflg(dmcopr,*)
      real    strhis(dmstrh,*), strpar(dmstrpar,*), conhis(5,*),
     &        contrl(17,*)
c
c     Declaration of local variables
c
      integer put, i, j, k, icn,lstnam,lcntrnm1,lcntrnm2, nrcon0
      logical lwrn
      parameter (put=1)
      character(len=40) strnam, cntrnm1, cntrnm2
c
c     Declaration of external function
c
      integer  FLIHIS
      external FLIHIS
c
      include '../include/errcod.i'
c
c     Initialize flags for controlled parameter
c
      do 20 i = 1, dmcopr
         do 10 j = 1, nstru
            cnpflg(i,j) = 0
   10    continue
   20 continue

      lwrn = .false.
c
      do 30 icn = 1, ncsrel
         i = cnstrl(1,icn)
         j = cnstrl(2,icn)
         k = int(contrl(3,i))
c
c        Controller active ?
c
         if ( int(contrl(2,i)) .eq. 1 ) then
c
c           Test controller if controlled twice?
c
            nrcon0 = cnpflg(k,j)
            if ( nrcon0 .eq. 0 ) then
               cnpflg(k,j) = i
c
c              Store controlled parameter in -strhis- and -strpar-
c
               
               strhis(FLIHIS(k),j) = conhis(1,i)
               call FLCNPA (put, strtyp(1,j), j, k, conhis(1,i), strpar,
     &                      juer, ker )
            else
               call getstr(j,strnam,lstnam)
               call getcontr(nrcon0 ,cntrnm1,lcntrnm1)
               call getcontr(i      ,cntrnm2,lcntrnm2)
               ker = fatal
          call sre_error (juer,'FLCOST Controlled parameter controlled'
     &         //'twice at the same time in structure @'//
     &         strnam(:lstnam)//'@ (@'
     &         //cntrnm1(:lcntrnm1)//'@,@' 
     &         //cntrnm2(:lcntrnm2)//'@)' , eflcpt, ker)
               lwrn = .true.
            endif
         endif
   30 continue
c
      if ( lwrn ) then
         ker = fatal
      endif
c
      end
