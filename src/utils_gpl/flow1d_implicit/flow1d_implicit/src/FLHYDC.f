      subroutine FLHYDC(g      ,rho    ,contrl ,icont  ,strtyp ,
     +                  ngrid  ,h      ,q      ,af     ,maxtab ,
     +                  ntabm  ,ntab   ,table  ,conhis ,dt1    ,
     +                  strpar ,nstru  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLHYDC (FLow HYDraulic Controller)
c
c Module description: In subroutine FLHYDC the control parameter is
c                     calculated as a function of some hydraulic
c                     parameter, e.g. h or Q.
c
c                     For a hydraulic controlled structure the control
c                     parameter is a function of a hydraulic parameter.
c                     The following hydraulic parameters are possible:
c
c                     -      water level at a certain location;
c                     -      discharge at a certain location;
c                     -      head difference over a structure;
c                     -      velocity at a certain location;
c                     -      stream direction at a certain location;
c                     -      pressure difference over a structure.
c
c                     The control parameter as function of the hydraulic
c                     parameter is stored in the hydraulic controller
c                     table. After computation of the argument the
c                     controller value is determined by interpolation.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  9 af(ngrid)         I  Flow area at every grid point at time t(n+1)
c 14 conhis(5,ncontr)  O  For each controller some calculated values
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
c  1 g                 I  Acceleration of gravity.
c  7 h(ngrid)          I  Water level in every grid point at the latest
c                         iteration.
c  4 icont             I  Index in array contrl(*,ncontr).
c 10 maxtab            I  Maximum number of defined tables.
c  6 ngrid             I  Number of grid points in network.
c 12 ntab(4,maxtab)    I  Table descriptor. Tables are numbererd from 1
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
c 11 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  8 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c  2 rho(ngrid)        I  Density of water.
c  5 strtyp(10,nstru)  I  Structure definitions:
c                         (1,i) = Type of structure:
c                                 csweir (1) : Simple weir
c                                 caweir (2) : Advanced weir
c                                 csgate (3) : - not used, reserved for
c                                                simple gate -
c                                 cpump  (4) : Pump
c                                 cgenst (5) : General Structure
c                         (2,i) = Position of structure:
c                                 cstbra (1) : In branch
c                                 cstlat (2) : Lateral structure
c                         (3,i) = Left gridpoint.
c                         (4,i) = Right gridpoint.
c                         (5,i) = dummy
c                         (6,i) = dummy
c                         (7,i) = dummy
c                         (8,i) = dummy
c                         (9,i) = dummy
c                         (10,i)= dummy
c 13 table             P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
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
c $Log: flhydc.pf,v $
c Revision 1.8  1999/03/15  15:50:03  kuipe_j
c tabs removed
c
c Revision 1.7  1998/06/08  12:29:51  kuipe_j
c time lag hydr controller
c
c Revision 1.6  1996/01/17  14:38:30  kuipe_j
c header update
c
c Revision 1.5  1995/09/22  10:01:40  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/30  12:36:38  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:55:05  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:59:02  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:48  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:28  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:31:00  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:50  kuipe_j
c Initial version
c
c
c***********************************************************************
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      integer icont, ngrid, maxtab, ntabm, nstru
      integer ntab(4,maxtab), strtyp(10,*)
      real    g
      real    table(ntabm), contrl(17,*)
      real    af(ngrid), conhis(5,*)
      real    strpar(dmstrpar,nstru), rho(ngrid)
      double precision  dt1, h(ngrid), q(ngrid)
c
c     Declaration of local variables:
c
      integer    get   ,icpnum     ,juerd
      parameter (get=2 ,icpnum = 1 ,juerd = -1)
      integer    i, il, ir, type, itab, ngrp, igr, strno, isttyp, kerd
      real       hydvar, conpar, zs
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
c     Assign values to local variables:
c
      type  = int(contrl(6,icont))
      itab  = int(contrl(7,icont))
      ngrp  = int(contrl(8,icont))
c
c     Location can be a gridpoint or structure number
c
      igr   = int(contrl(9,icont))
      strno = int(contrl(9,icont))
c
c     Determine actual value of hydraulic argument
c
      if      ( type .eq. cconh ) then
c
c        - Waterlevel from gridpoint location -
c
         hydvar = h(igr)
c
      else if ( type .eq. cconq ) then
c
c        - Discharge from gridpoint location or summation of the max.
c          5 gridpoints -
c
        if (contrl(14,icont) .gt. real(dt1)) then 
         hydvar = contrl(15,icont)
        else
         hydvar = 0.
         do 10 i = 1, ngrp
            igr = int(contrl(8+i,icont))
            hydvar = hydvar + q(igr)
   10    continue
        endif
         
c
      else if ( type .eq. cconhd ) then
c
c        - Head difference at the structure number -
c
         il = strtyp(3,strno)
         ir = strtyp(4,strno)
         hydvar = h(il) - h(ir)
c
      else if ( type .eq. cconu ) then
c
c        - Velocity from gridpoint location -
c
         hydvar = q(igr) / af(igr)
c
      else if ( type .eq. cconsd ) then
c
c        - Stream direction from gridpoint location -
c
         if ( q(igr) .lt. 0. ) then
            hydvar = -1.0
         else
            hydvar = 1.0
         endif
c
      else if ( type .eq. cconpd ) then
c
c        - Pressure difference from structure name -
c
         il     = strtyp(3,strno)
         ir     = strtyp(4,strno)
c new JK ARS 3263
         isttyp = strtyp(1,strno)
         call FLCNPA(get    ,isttyp ,strno ,icpnum, zs ,
     &               strpar ,juerd  ,kerd  )
     
         hydvar = max((h(il) - zs)**2 * rho(il)*g/2.0 ,0.0d0) - 
     &            max((h(ir) - zs)**2 * rho(ir)*g/2.0 ,0.0d0)
c old
c        hydvar = abs( h(il) - h(ir) ) * rhow*g/2.0
      endif
c
      call INTTAB (ntab(1,itab), ntab(4,itab),
     +             table(ntab(2,itab)),
     +             table(ntab(3,itab)),
     +             dble(hydvar) ,conpar )
c
c     Store controlled parameter in -conhis-
c
      conhis(1,icont)   = conpar
c
      end
