      subroutine FLCONT(lauto  ,time   ,dt1    ,istep  ,g      ,rho    ,
     +                  strtyp ,ngrid  ,h      ,q      ,af     ,maxtab ,
     +                  ntabm  ,ntab   ,table  ,ncontr ,contrl ,ncsrel ,
     +                  cnstrl ,strhis ,conhis ,strpar ,nstru  ,lrest  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:         J.Brouwer
c
c Module:             FLCONT (FLow CONTrollers)
c
c Module description: In subroutine FLCONT the computation of the actual
c                     control parameter for a structure is performed.
c                     Each controller calculates the value of one
c                     control parameter.
c
c                     For a 'time controlled' structure, e.g. control
c                     parameter is an explicit function of time, the
c                     control parameter is computed in routine FLTIMC.
c
c                     For an 'hydraulic controlled' structure, e.g.
c                     control parameter is a function of a hydraulic
c                     parameter, the actual control parameter is compu-
c                     ted in routine FLHYDC
c
c                     The actual control parameter for an Interval con-
c                     troller is evaluated in routine FLINTC.
c
c                     The actual control parameter for a PID controller
c                     is evaluated in routine FLPIDC.
c
c                     For every controller it is possible to define the
c                     control frequency. This frequency is defined in
c                     number of flow steps.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 10 af                P  -
c 18 cnstrl            P  -
c 20 conhis            P  -
c 16 contrl(17,ncontr) I  Definition of controllers. Structures can be
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
c  2 dt1               P  -
c  4 g                 P  -
c  8 h                 P  -
c  3 istep             I  Current time step number (t(n+1)).
c 11 maxtab            I  Maximum number of defined tables.
c 15 ncontr            I  Number of controlled structures.
c 17 ncsrel            P  -
c  7 ngrid             I  Number of grid points in network.
c 13 ntab              P  -
c 12 ntabm             I  Maximum size of table (Used for dimensioning
c                         table).
c  9 q                 P  -
c  5 rhow              P  -
c 19 strhis            P  -
c  6 strtyp            P  -
c 14 table             P  -
c  1 time              P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flhydc  FLow HYDraulic Controller
c flintc  FLow INTerval Controller
c flpidc  FLow PID Controller
c fltimc  FLow TIMe Control
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: flcont.pf,v $
c Revision 1.7  1998/12/11  13:05:03  kuipe_j
c avoid test dupl contr in autostart
c
c Revision 1.6  1998/06/08  12:29:44  kuipe_j
c time lag hydr controller
c
c Revision 1.5  1995/09/22  10:01:08  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/08/30  12:36:27  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:54:54  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  06:58:51  hoeks_a
c file converted from dos to ux
c
c Revision 1.1  1995/04/13  07:07:37  hoeks_a
c Initial check-in
c
c Revision 1.3  1994/11/28  08:37:22  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:30:44  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:43:48  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Include constants for array dimensions
c
      include '../include/sobdim.i'
c
c     Declaration of parameters:
c
      integer istep, ngrid, maxtab, ntabm, ncsrel, ncontr, nstru
      integer ntab(4,maxtab), strtyp(10,*), cnstrl(2,*)
      real    g, table(ntabm)
      real    contrl(17,*), conhis(5,*), strhis(dmstrh,*)
      real    af(ngrid)
      real    strpar(dmstrpar,nstru), rho(ngrid)
      double  precision  time   , dt1, h(ngrid), q(ngrid)
      logical lauto , lrest
c
c     Declaration of local variables:
c
      integer contyp, freq, icont, icontlist, istru, iconpar, kerdum,
     &        icsind 
      logical docontr
      integer ,parameter :: get=2
c
c     Include sobek constants
c
      include '../include/sobcon.i'
c
      do 10 icont = 1, ncontr
c
         if ( int(contrl(2,icont)) .eq. 1 ) then
c
            contyp = int(contrl(1,icont))
            freq   = int(contrl(5,icont))
c
            docontr = .false.
            if (freq.gt.0) then          
               if (mod(istep,freq) .eq. 0 ) docontr = .true.
            else if(freq.eq.0) then         
               if (conhis(4,icont).eq.0) docontr = .true.
            endif
            if (docontr) then
               if ( contyp .eq. ctimcn ) then
               if (int(contrl(7,icont)).eq. ctmabs .or.
     +                .not. lauto) then
c
c                 - Time controller -
c
                    call FLTIMC(time   ,dt1    ,contrl ,icont  ,maxtab ,
     +                          ntabm  ,ntab   ,table  ,ncsrel ,cnstrl ,
     +                          strhis ,conhis )
               else
c                 skip controllers and set flag on inactive
                  contrl(2,icont) = 0
               endif
c
               else if (lauto) then
c                 skip controllers and set flag on inactive
                  contrl(2,icont) = 0
               else

               if ( contyp .eq. chydcn ) then
c
c                 - Hydraulic controller -
c
                  call FLHYDC(g      ,rho    ,contrl ,icont  ,strtyp ,
     +                        ngrid  ,h      ,q      ,af     ,maxtab ,
     +                        ntabm  ,ntab   ,table  ,conhis ,dt1    ,
     +                        strpar ,nstru  )
c
               else if ( contyp .eq. cintcn ) then
c
c                 - Interval controller -
c
c                 if Interval controller becomes active set current control
c                 value in Conhis.
c
                  if (nint(conhis(4,icont)).eq.0) then 
                     do icsind = 1, ncsrel
                        icontlist = cnstrl(1,icsind)
c
                        if ( icontlist .eq. icont ) then
                           istru   = cnstrl(2,icsind)
                           iconpar = nint(contrl(3,icont))
c
c                          Corresponding structure found so
c                          store controlled parameter in -conhis- from
c                          -strpar-
c
                           call FLCNPA(get,     strtyp(1,istru), istru ,
     &                                 iconpar, conhis(1,icont), strpar,
     &                                 0      , kerdum  )
c
                        endif
                     enddo 
                  endif
c
                  call FLINTC(time   ,dt1    ,contrl ,icont  ,ngrid  ,
     +                        h      ,q      ,maxtab ,ntabm  ,ntab   ,
     +                        table  ,conhis )
c
               else if ( contyp .eq. cpidcn ) then
c
c                 - PID controller -
c
c                 ARS 7656
c                 Avoid at first step that u0 will be set to the 
c                 current value.
c
c                  if (istep.le.1) conhis(4,icont) = 1
c   
c                 if PID controller becomes active set current control
c                 value in Conhis.
c
                  if (nint(conhis(4,icont)).eq.0) then 
                     do icsind = 1, ncsrel
                        icontlist = cnstrl(1,icsind)
c
                        if ( icontlist .eq. icont ) then
                           istru   = cnstrl(2,icsind)
                           iconpar = nint(contrl(3,icont))
c
c                          Corresponding structure found so
c                          store controlled parameter in -conhis- from
c                          -strpar-
c
                           call FLCNPA(get,     strtyp(1,istru), istru ,
     &                                 iconpar, conhis(1,icont), strpar,
     &                                 0      , kerdum  )
c
                        endif
                     enddo 
                  endif

                  call FLPIDC(time   ,dt1    ,contrl ,icont  ,ngrid  ,
     +                        h      ,q      ,maxtab ,ntabm  ,ntab   ,
     +                        table  ,conhis ,istep  ,lrest  )
               endif
               endif
            endif
c           controller is active in this step
            conhis (4,icont) = 1
         else 
c           controller is not active in this step
            conhis (4,icont) = 0
         endif
   10 continue
c
      end
