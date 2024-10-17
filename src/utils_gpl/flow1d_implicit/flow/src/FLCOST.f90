subroutine FLCOST(ncsrel, nstru , strtyp, strpar, strhis,&
&cnstrl, conhis, contrl, cnpflg, juer  , ker   )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLCOST (FLow COntroller STructure relations)
!
! Module description: This routine determines the values of the
!                     controlled parameters of all structures.
!
!                     At entry of this routine the status of all
!                     controllers are known (active / not active).
!                     Using the controller-structure relation table and
!                     the status, this routine sets the controlled
!                     parameters of all related structures. Parameters
!                     of uncontrolled structures will hold their
!                     initial values.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  5 cnstrl(2,ncsrel)  I  table for controller structure relations.
!                         (1,i) = Controller number of controller
!                                 related to structure at (2,i)
!                         (2,i) = Structure number of structure related
!                                 to controller at (1,i)
!  6 conhis(5,ncontr)  I  For each controller some calculated values
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
!  7 contrl(17,ncontr) I  Definition of controllers. Structures can be
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
!  1 ncsrel            I  number of controller structure relations.
!  4 strhis(10,nstru)  O  For each structure the discharge and the
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
!  3 strpar            P  -
!  2 strtyp            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flcnpa  FLow put or get CoNtrolled PArameter
! flihis  FLow Index renumbering structure HIStory
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flcost.pf,v $
! Revision 1.8  1999/06/01  13:42:15  kuipe_j
! names in messages substituted + message template
!
! Revision 1.7  1999/03/15  15:49:43  kuipe_j
! tabs removed
!
! Revision 1.6  1996/11/01  15:04:10  kuipe_j
! Improve contoller messages
!
! Revision 1.5  1996/09/03  14:51:52  kuipe_j
! frequency time hist,Messages controllers added
!
! Revision 1.4  1996/04/12  13:03:43  kuipe_j
! headers, minor changes
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '..\include\sobdim.i'
!
!     Declaration of parameters
!
   integer ncsrel          , nstru       , juer , ker
   integer strtyp(10,*)    , cnstrl(2,*) , cnpflg(dmcopr,*)
   real    strhis(dmstrh,*), strpar(dmstrpar,*), conhis(5,*),&
   &contrl(17,*)
!
!     Declaration of local variables
!
   integer put, i, j, k, icn,lstnam,lcntrnm1,lcntrnm2, nrcon0
   logical lwrn
   parameter (put=1)
   character*40  strnam, cntrnm1, cntrnm2
!
!     Declaration of external function
!
   integer  FLIHIS
   external FLIHIS
!
   include '..\include\errcod.i'
!
!     Initialize flags for controlled parameter
!
   do 20 i = 1, dmcopr
      do 10 j = 1, nstru
         cnpflg(i,j) = 0
10    continue
20 continue

   lwrn = .false.
!
   do 30 icn = 1, ncsrel
      i = cnstrl(1,icn)
      j = cnstrl(2,icn)
      k = int(contrl(3,i))
!
!        Controller active ?
!
      if ( int(contrl(2,i)) .eq. 1 ) then
!
!           Test controller if controlled twice?
!
         nrcon0 = cnpflg(k,j)
         if ( nrcon0 .eq. 0 ) then
            cnpflg(k,j) = i
!
!              Store controlled parameter in -strhis- and -strpar-
!

            strhis(FLIHIS(k),j) = conhis(1,i)
            call FLCNPA (put, strtyp(1,j), j, k, conhis(1,i), strpar,&
            &juer, ker )
         else
            call getstr(j,strnam,lstnam)
            call getcontr(nrcon0 ,cntrnm1,lcntrnm1)
            call getcontr(i      ,cntrnm2,lcntrnm2)
            ker = fatal
            call error (juer,'FLCOST Controlled parameter controlled'&
            &//'twice at the same time in structure @'//&
            &strnam(:lstnam)//'@ (@'&
            &//cntrnm1(:lcntrnm1)//'@,@'&
            &//cntrnm2(:lcntrnm2)//'@)' , eflcpt, ker)
            lwrn = .true.
         endif
      endif
30 continue
!
   if ( lwrn ) then
      ker = fatal
   endif
!
end
