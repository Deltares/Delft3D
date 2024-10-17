subroutine SOTLAG(ncontr ,contrl ,dt1   ,juer  ,ker  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Kuipers
!
! Module:             SOTLAG (FLow Time LAG)
!
! Module description: In  a hydraulic controler the control parameter is
!                     calculated as a function of some hydraulic
!                     parameter, e.g. h or Q. In case of a discharge
!                     a time lag can be taken into account.
!
!                     SOTLAG calculates the dimensions of the buffer
!                     with discharges. Space on memory pool is also
!                     created.
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
!    buflag(lagstm,nlags) Buffer with times and discharges for hydr.
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
!    lagstm               Length of buffer with times and discharges.
!    nlags                Number of F(Q)-controllers with time-lag + 1.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sotlag.pf,v $
! Revision 1.2  1999/03/15  15:19:54  kuipe_j
! tabs removed
!
! Revision 1.1  1998/06/08  14:35:00  kuipe_j
! initial
!
!
!
!***********************************************************************
!
!     Declaration of parameters:
!
   integer           ncontr  ,juer ,ker
   double precision  dt1
   real              contrl(17,ncontr)
!
!     Declaration of local variables:
!
   integer       errcod ,errno  ,size
   integer       lagstm ,nlags  ,lagst, icont
   real          dt
   character*16  name
   character*80  txt
!
!     External functions
!
   integer       mkipnt, mkrpnt, gtipnt
   external      mkipnt, mkrpnt, gtipnt
!
!     Include sobek constants
!
   include '..\include\mempool.i'
   include '..\include\errcod.i'
   include '..\include\sobcon.i'
!
   dt = real(dt1)
!
!     Determine array dimensions of the buffer with discharges
!
   nlags  = 1
   lagstm = 0
   do 10 icont=1,ncontr
      if (int(contrl(1,icont)) .eq. chydcn) then
         if (contrl(14,icont) .gt. dt ) then
            lagst  = int(contrl(14,icont)/dt)
            lagstm = max(lagstm,lagst)
            nlags  = nlags+1
         endif
      endif
10 continue
   lagstm = lagstm + 4
!
!     Declare memory
!
   size = 1
   name = 'LAGSTM'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0 ) goto 900
   ip(gtipnt(name)) = lagstm
!
   name = 'NLAGS'
   errcod = mkipnt ( name , size )
   if (errcod .lt. 0 ) goto 900
   ip(gtipnt(name)) = nlags
!
   size = lagstm*nlags
   name = 'BUFLAG'
   errcod = mkrpnt ( name , size )
   if (errcod .lt. 0 ) goto 900

   return
!
900 continue

   ker = fatal

   if (errcod .eq. -1) errno = evrdec
   if (errcod .eq. -2) errno = eoutds
   if (errcod .eq. -3) errno = eoutns
   txt = 'SOTLAG Memory error for @' // name // '@'
   call error ( juer, txt, errno, ker )

end
