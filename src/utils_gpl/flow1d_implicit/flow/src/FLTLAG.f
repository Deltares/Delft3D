      subroutine FLTLAG(kode   ,ncontr ,contrl ,lagstm ,nlags  ,buflag ,
     +                  dt1    ,ngrid  ,q      )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Kuipers  
c
c Module:             FLTLAG (FLow Time LAG)
c
c Module description: In subroutine FLHYDC the control parameter is
c                     calculated as a function of some hydraulic
c                     parameter, e.g. h or Q. 
c
c                     In case of F(Q) a time lag can be taken in 
c                     account. The discharge argument will be
c                     calculated and stored to use in FLHYDC.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
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
c  6 ngrid             I  Number of grid points in network.
c  8 q(ngrid)          I  Discharge in every grid point at the latest
c                         iteration.
c  6 buflag(lagstm,nlags) Buffer with times and discharges for hydr.
c                         F(Q)-controllers with time-lag
c                         (1,lagstm)..(1,3) = times (-dt,-2*dt,...)
c                         (i,lagstm)..(i,j) = discharges at -dt,-2*dt,..
c                         relative to current time (t=n+1) for 
c                         controller (i,1)
c                         For i=2,nlags:
c                         (i,1) = controller number
c                         (i,2) = number of discharges
c                         (i,3) = index j of most early discharge in
c                                 buffer
c                         (1,1) = lagstm
c                         (1,2) = nlags
c  4 lagstm               Length of buffer with times and discharges.
c  5 nlags                Number of F(Q)-controllers with time-lag + 1.
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
c $Log: fltlag.pf,v $
c Revision 1.2  1999/03/15  15:50:56  kuipe_j
c tabs removed
c
c Revision 1.1  1998/06/08  12:30:34  kuipe_j
c time lag hydr controller
c
c
c
c***********************************************************************
c
c     Declaration of parameters:
c
      integer kode ,ncontr, ngrid, lagstm, nlags
      real    contrl(17,ncontr), buflag(lagstm,nlags)

      double precision  dt1, q(ngrid)
c
c     Declaration of local variables:
c
      integer i, type, ngrp, igr, ilag ,icont ,first, lagst ,ilags
      real    hydvar, dt
c
c     Include sobek constants
c
      include '..\include\sobcon.i'
c
      dt = real(dt1)
c
      if (kode .eq. 2) then
c       
c        Insert administration in array buflag
c        Fill buflag with initial discharge on all times
c        and with times.
c
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
c
c                    Discharge from gridpoint location or
c                    summation of max. 5 gridpoints
c
                     do 20 i = 1, ngrp
                        igr = int(contrl(8+i,icont))
                        hydvar = hydvar + q(igr)
  20                 continue
                  endif 
                  do 30 i=4,lagstm
                     buflag(i,ilag) = hydvar   
  30              continue
               endif
            endif
  40     continue
c
c        Fill with times
c
         do 50 i =1,lagstm
            buflag(i,1) =  -real(lagstm-i+1)*dt
  50     continue
c
c        The size is also stored to control the dimensions at restart
c
         buflag(1,1) =  lagstm
         buflag(2,1) =  nlags

      else if (kode .eq. 3) then
c
c        Shift discharges.     
c
         do 80 ilags=2,nlags
            icont = int(buflag(1,ilags))  
            first = int(buflag(3,ilags))
            do 60 i=first,lagstm-1
               buflag(i,ilags) = buflag(i+1,ilags)
  60        continue
c
c           Discharge from gridpoint location or
c           summation of max. 5 gridpoints
c           put in buflag 
c
            ngrp   = int(contrl(8,icont))
            type   = int(contrl(6,icont))
            hydvar = 0.
            if ( type .eq. cconq ) then
               do 70 i = 1, ngrp
                  igr = int(contrl(8+i,icont))
                  hydvar = hydvar + q(igr)
  70           continue
            endif 
            buflag(lagstm,ilags) = hydvar   
c     
c           store discharge at time lag in array contrl
c
            call INTTAB (int(buflag(2,ilags)), 0,                 
     +                   buflag(first,1), buflag(first,ilags),
     +                  -dble(contrl(14,icont)) ,contrl(15,icont))
c
  80     continue
      endif
c
      end
