subroutine FLTLAG(kode   ,ncontr ,contrl ,lagstm ,nlags  ,buflag ,&
&dt1    ,ngrid  ,q      )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Kuipers
!
! Module:             FLTLAG (FLow Time LAG)
!
! Module description: In subroutine FLHYDC the control parameter is
!                     calculated as a function of some hydraulic
!                     parameter, e.g. h or Q.
!
!                     In case of F(Q) a time lag can be taken in
!                     account. The discharge argument will be
!                     calculated and stored to use in FLHYDC.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
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
!  6 ngrid             I  Number of grid points in network.
!  8 q(ngrid)          I  Discharge in every grid point at the latest
!                         iteration.
!  6 buflag(lagstm,nlags) Buffer with times and discharges for hydr.
!                         F(Q)-controllers with time-lag
!                         (1,lagstm)..(1,3) = times (-dt,-2*dt,...)
!                         (i,lagstm)..(i,j) = discharges at -dt,-2*dt,..
!                         relative to current time (t=n+1) for
!                         controller (i,1)
!                         For i=2,nlags:
!                         (i,1) = controller number
!                         (i,2) = number of discharges
!                         (i,3) = index j of most early discharge in
!                                 buffer
!                         (1,1) = lagstm
!                         (1,2) = nlags
!  4 lagstm               Length of buffer with times and discharges.
!  5 nlags                Number of F(Q)-controllers with time-lag + 1.
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
! $Log: fltlag.pf,v $
! Revision 1.2  1999/03/15  15:50:56  kuipe_j
! tabs removed
!
! Revision 1.1  1998/06/08  12:30:34  kuipe_j
! time lag hydr controller
!
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer kode ,ncontr, ngrid, lagstm, nlags
   real    contrl(17,ncontr), buflag(lagstm,nlags)

   double precision  dt1, q(ngrid)
!
!     Declaration of local variables:
!
   integer i, type, ngrp, igr, ilag ,icont ,first, lagst ,ilags
   real    hydvar, dt
!
!     Include sobek constants
!
   include '..\include\sobcon.i'
!
   dt = real(dt1)
!
   if (kode .eq. 2) then
!
!        Insert administration in array buflag
!        Fill buflag with initial discharge on all times
!        and with times.
!
      ilag  = 1
      do 40 icont=1,ncontr
         if (int(contrl(1,icont)) .eq. chydcn) then
            if (contrl(14,icont) .gt. dt ) then
               lagst = int(contrl(14,icont)/dt) + 1
               ilag  = ilag + 1
               buflag(1,ilag) = icont
               buflag(2,ilag) = lagst
               buflag(3,ilag) = lagstm - lagst + 1
               ngrp   = int(contrl(8,icont))
               type   = int(contrl(6,icont))
               hydvar = 0.
               if ( type .eq. cconq ) then
!
!                    Discharge from gridpoint location or
!                    summation of max. 5 gridpoints
!
                  do 20 i = 1, ngrp
                     igr = int(contrl(8+i,icont))
                     hydvar = hydvar + q(igr)
20                continue
               endif
               do 30 i=4,lagstm
                  buflag(i,ilag) = hydvar
30             continue
            endif
         endif
40    continue
!
!        Fill with times
!
      do 50 i =1,lagstm
         buflag(i,1) =  -real(lagstm-i+1)*dt
50    continue
!
!        The size is also stored to control the dimensions at restart
!
      buflag(1,1) =  lagstm
      buflag(2,1) =  nlags

   else if (kode .eq. 3) then
!
!        Shift discharges.
!
      do 80 ilags=2,nlags
         icont = int(buflag(1,ilags))
         first = int(buflag(3,ilags))
         do 60 i=first,lagstm-1
            buflag(i,ilags) = buflag(i+1,ilags)
60       continue
!
!           Discharge from gridpoint location or
!           summation of max. 5 gridpoints
!           put in buflag
!
         ngrp   = int(contrl(8,icont))
         type   = int(contrl(6,icont))
         hydvar = 0.
         if ( type .eq. cconq ) then
            do 70 i = 1, ngrp
               igr = int(contrl(8+i,icont))
               hydvar = hydvar + q(igr)
70          continue
         endif
         buflag(lagstm,ilags) = hydvar
!
!           store discharge at time lag in array contrl
!
         call INTTAB (int(buflag(2,ilags)), 0,&
         &buflag(first,1), buflag(first,ilags),&
         &-dble(contrl(14,icont)) ,contrl(15,icont))
!
80    continue
   endif
!
end
