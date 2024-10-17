subroutine FLHYDC(g      ,rho    ,contrl ,icont  ,strtyp ,&
&ngrid  ,h      ,q      ,af     ,maxtab ,&
&ntabm  ,ntab   ,table  ,conhis ,dt1    ,&
&strpar ,nstru  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLHYDC (FLow HYDraulic Controller)
!
! Module description: In subroutine FLHYDC the control parameter is
!                     calculated as a function of some hydraulic
!                     parameter, e.g. h or Q.
!
!                     For a hydraulic controlled structure the control
!                     parameter is a function of a hydraulic parameter.
!                     The following hydraulic parameters are possible:
!
!                     -      water level at a certain location;
!                     -      discharge at a certain location;
!                     -      head difference over a structure;
!                     -      velocity at a certain location;
!                     -      stream direction at a certain location;
!                     -      pressure difference over a structure.
!
!                     The control parameter as function of the hydraulic
!                     parameter is stored in the hydraulic controller
!                     table. After computation of the argument the
!                     controller value is determined by interpolation.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  9 af(ngrid)         I  Flow area at every grid point at time t(n+1)
! 14 conhis(5,ncontr)  O  For each controller some calculated values
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
!  1 g                 I  Acceleration of gravity.
!  7 h(ngrid)          I  Water level in every grid point at the latest
!                         iteration.
!  4 icont             I  Index in array contrl(*,ncontr).
! 10 maxtab            I  Maximum number of defined tables.
!  6 ngrid             I  Number of grid points in network.
! 12 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
! 11 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  8 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
!  2 rho(ngrid)        I  Density of water.
!  5 strtyp(10,nstru)  I  Structure definitions:
!                         (1,i) = Type of structure:
!                                 csweir (1) : Simple weir
!                                 caweir (2) : Advanced weir
!                                 csgate (3) : - not used, reserved for
!                                                simple gate -
!                                 cpump  (4) : Pump
!                                 cgenst (5) : General Structure
!                         (2,i) = Position of structure:
!                                 cstbra (1) : In branch
!                                 cstlat (2) : Lateral structure
!                         (3,i) = Left gridpoint.
!                         (4,i) = Right gridpoint.
!                         (5,i) = dummy
!                         (6,i) = dummy
!                         (7,i) = dummy
!                         (8,i) = dummy
!                         (9,i) = dummy
!                         (10,i)= dummy
! 13 table             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
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
! $Log: flhydc.pf,v $
! Revision 1.8  1999/03/15  15:50:03  kuipe_j
! tabs removed
!
! Revision 1.7  1998/06/08  12:29:51  kuipe_j
! time lag hydr controller
!
! Revision 1.6  1996/01/17  14:38:30  kuipe_j
! header update
!
! Revision 1.5  1995/09/22  10:01:40  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:36:38  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:55:05  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:59:02  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:48  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:28  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:31:00  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:50  kuipe_j
! Initial version
!
!
!***********************************************************************
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters:
!
   integer icont, ngrid, maxtab, ntabm, nstru
   integer ntab(4,maxtab), strtyp(10,*)
   real    g
   real    table(ntabm), contrl(17,*)
   real    af(ngrid), conhis(5,*)
   real    strpar(dmstrpar,nstru), rho(ngrid)
   double precision  dt1, h(ngrid), q(ngrid)
!
!     Declaration of local variables:
!
   integer    get   ,icpnum     ,juerd
   parameter (get=2 ,icpnum = 1 ,juerd = -1)
   integer    i, il, ir, type, itab, ngrp, igr, strno, isttyp, kerd
   real       hydvar, conpar, zs
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
!     Assign values to local variables:
!
   type  = int(contrl(6,icont))
   itab  = int(contrl(7,icont))
   ngrp  = int(contrl(8,icont))
!
!     Location can be a gridpoint or structure number
!
   igr   = int(contrl(9,icont))
   strno = int(contrl(9,icont))
!
!     Determine actual value of hydraulic argument
!
   if      ( type .eq. cconh ) then
!
!        - Waterlevel from gridpoint location -
!
      hydvar = h(igr)
!
   else if ( type .eq. cconq ) then
!
!        - Discharge from gridpoint location or summation of the max.
!          5 gridpoints -
!
      if (contrl(14,icont) .gt. real(dt1)) then
         hydvar = contrl(15,icont)
      else
         hydvar = 0.
         do 10 i = 1, ngrp
            igr = int(contrl(8+i,icont))
            hydvar = hydvar + q(igr)
10       continue
      endif

!
   else if ( type .eq. cconhd ) then
!
!        - Head difference at the structure number -
!
      il = strtyp(3,strno)
      ir = strtyp(4,strno)
      hydvar = h(il) - h(ir)
!
   else if ( type .eq. cconu ) then
!
!        - Velocity from gridpoint location -
!
      hydvar = q(igr) / af(igr)
!
   else if ( type .eq. cconsd ) then
!
!        - Stream direction from gridpoint location -
!
      if ( q(igr) .lt. 0. ) then
         hydvar = -1.0
      else
         hydvar = 1.0
      endif
!
   else if ( type .eq. cconpd ) then
!
!        - Pressure difference from structure name -
!
      il     = strtyp(3,strno)
      ir     = strtyp(4,strno)
! new JK ARS 3263
      isttyp = strtyp(1,strno)
      call FLCNPA(get    ,isttyp ,strno ,icpnum, zs ,&
      &strpar ,juerd  ,kerd  )

      hydvar = max((h(il) - zs)**2 * rho(il)*g/2.0 ,0.0d0) -&
      &max((h(ir) - zs)**2 * rho(ir)*g/2.0 ,0.0d0)
! old
!        hydvar = abs( h(il) - h(ir) ) * rhow*g/2.0
   endif
!
   call INTTAB (ntab(1,itab), ntab(4,itab),&
   &table(ntab(2,itab)),&
   &table(ntab(3,itab)),&
   &dble(hydvar) ,conpar )
!
!     Store controlled parameter in -conhis-
!
   conhis(1,icont)   = conpar
!
end
