subroutine FLCONT(lauto  ,time   ,dt1    ,istep  ,g      ,rho    ,&
&strtyp ,ngrid  ,h      ,q      ,af     ,maxtab ,&
&ntabm  ,ntab   ,table  ,ncontr ,contrl ,ncsrel ,&
&cnstrl ,strhis ,conhis ,strpar ,nstru  ,lrest  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:         J.Brouwer
!
! Module:             FLCONT (FLow CONTrollers)
!
! Module description: In subroutine FLCONT the computation of the actual
!                     control parameter for a structure is performed.
!                     Each controller calculates the value of one
!                     control parameter.
!
!                     For a 'time controlled' structure, e.g. control
!                     parameter is an explicit function of time, the
!                     control parameter is computed in routine FLTIMC.
!
!                     For an 'hydraulic controlled' structure, e.g.
!                     control parameter is a function of a hydraulic
!                     parameter, the actual control parameter is compu-
!                     ted in routine FLHYDC
!
!                     The actual control parameter for an Interval con-
!                     troller is evaluated in routine FLINTC.
!
!                     The actual control parameter for a PID controller
!                     is evaluated in routine FLPIDC.
!
!                     For every controller it is possible to define the
!                     control frequency. This frequency is defined in
!                     number of flow steps.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 10 af                P  -
! 18 cnstrl            P  -
! 20 conhis            P  -
! 16 contrl(17,ncontr) I  Definition of controllers. Structures can be
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
!  2 dt1               P  -
!  4 g                 P  -
!  8 h                 P  -
!  3 istep             I  Current time step number (t(n+1)).
! 11 maxtab            I  Maximum number of defined tables.
! 15 ncontr            I  Number of controlled structures.
! 17 ncsrel            P  -
!  7 ngrid             I  Number of grid points in network.
! 13 ntab              P  -
! 12 ntabm             I  Maximum size of table (Used for dimensioning
!                         table).
!  9 q                 P  -
!  5 rhow              P  -
! 19 strhis            P  -
!  6 strtyp            P  -
! 14 table             P  -
!  1 time              P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flhydc  FLow HYDraulic Controller
! flintc  FLow INTerval Controller
! flpidc  FLow PID Controller
! fltimc  FLow TIMe Control
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: flcont.pf,v $
! Revision 1.7  1998/12/11  13:05:03  kuipe_j
! avoid test dupl contr in autostart
!
! Revision 1.6  1998/06/08  12:29:44  kuipe_j
! time lag hydr controller
!
! Revision 1.5  1995/09/22  10:01:08  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/08/30  12:36:27  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.3  1995/05/30  09:54:54  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  06:58:51  hoeks_a
! file converted from dos to ux
!
! Revision 1.1  1995/04/13  07:07:37  hoeks_a
! Initial check-in
!
! Revision 1.3  1994/11/28  08:37:22  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:30:44  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Include constants for array dimensions
!
   include '../include/sobdim.i'
!
!     Declaration of parameters:
!
   integer istep, ngrid, maxtab, ntabm, ncsrel, ncontr, nstru
   integer ntab(4,maxtab), strtyp(10,*), cnstrl(2,*)
   real    g, table(ntabm)
   real    contrl(17,*), conhis(5,*), strhis(dmstrh,*)
   real    af(ngrid)
   real    strpar(dmstrpar,nstru), rho(ngrid)
   double  precision  time   , dt1, h(ngrid), q(ngrid)
   logical lauto , lrest
!
!     Declaration of local variables:
!
   integer contyp, freq, icont, icontlist, istru, iconpar, kerdum,&
   &icsind
   logical docontr
   integer ,parameter :: get=2
!
!     Include sobek constants
!
   include '../include/sobcon.i'
!
   do 10 icont = 1, ncontr
!
      if ( int(contrl(2,icont)) .eq. 1 ) then
!
         contyp = int(contrl(1,icont))
         freq   = int(contrl(5,icont))
!
         docontr = .false.
         if (freq.gt.0) then
            if (mod(istep,freq) .eq. 0 ) docontr = .true.
         else if(freq.eq.0) then
            if (conhis(4,icont).eq.0) docontr = .true.
         endif
         if (docontr) then
            if ( contyp .eq. ctimcn ) then
               if (int(contrl(7,icont)).eq. ctmabs .or.&
               &.not. lauto) then
!
!                 - Time controller -
!
                  call FLTIMC(time   ,dt1    ,contrl ,icont  ,maxtab ,&
                  &ntabm  ,ntab   ,table  ,ncsrel ,cnstrl ,&
                  &strhis ,conhis )
               else
!                 skip controllers and set flag on inactive
                  contrl(2,icont) = 0
               endif
!
            else if (lauto) then
!                 skip controllers and set flag on inactive
               contrl(2,icont) = 0
            else

               if ( contyp .eq. chydcn ) then
!
!                 - Hydraulic controller -
!
                  call FLHYDC(g      ,rho    ,contrl ,icont  ,strtyp ,&
                  &ngrid  ,h      ,q      ,af     ,maxtab ,&
                  &ntabm  ,ntab   ,table  ,conhis ,dt1    ,&
                  &strpar ,nstru  )
!
               else if ( contyp .eq. cintcn ) then
!
!                 - Interval controller -
!
!                 if Interval controller becomes active set current control
!                 value in Conhis.
!
                  if (nint(conhis(4,icont)).eq.0) then
                     do icsind = 1, ncsrel
                        icontlist = cnstrl(1,icsind)
!
                        if ( icontlist .eq. icont ) then
                           istru   = cnstrl(2,icsind)
                           iconpar = nint(contrl(3,icont))
!
!                          Corresponding structure found so
!                          store controlled parameter in -conhis- from
!                          -strpar-
!
                           call FLCNPA(get,     strtyp(1,istru), istru ,&
                           &iconpar, conhis(1,icont), strpar,&
                           &0      , kerdum  )
!
                        endif
                     enddo
                  endif
!
                  call FLINTC(time   ,dt1    ,contrl ,icont  ,ngrid  ,&
                  &h      ,q      ,maxtab ,ntabm  ,ntab   ,&
                  &table  ,conhis )
!
               else if ( contyp .eq. cpidcn ) then
!
!                 - PID controller -
!
!                 ARS 7656
!                 Avoid at first step that u0 will be set to the
!                 current value.
!
!                  if (istep.le.1) conhis(4,icont) = 1
!
!                 if PID controller becomes active set current control
!                 value in Conhis.
!
                  if (nint(conhis(4,icont)).eq.0) then
                     do icsind = 1, ncsrel
                        icontlist = cnstrl(1,icsind)
!
                        if ( icontlist .eq. icont ) then
                           istru   = cnstrl(2,icsind)
                           iconpar = nint(contrl(3,icont))
!
!                          Corresponding structure found so
!                          store controlled parameter in -conhis- from
!                          -strpar-
!
                           call FLCNPA(get,     strtyp(1,istru), istru ,&
                           &iconpar, conhis(1,icont), strpar,&
                           &0      , kerdum  )
!
                        endif
                     enddo
                  endif

                  call FLPIDC(time   ,dt1    ,contrl ,icont  ,ngrid  ,&
                  &h      ,q      ,maxtab ,ntabm  ,ntab   ,&
                  &table  ,conhis ,istep  ,lrest  )
               endif
            endif
         endif
!           controller is active in this step
         conhis (4,icont) = 1
      else
!           controller is not active in this step
         conhis (4,icont) = 0
      endif
10 continue
!
end
