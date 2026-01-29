subroutine SOSIMi (lauto  ,lflow  ,lkalm  ,lsalt  ,lsedt  ,&
&lmorp  ,lgrad  ,lwqin  ,lrivr  ,lestu  ,&
!                        mozart parameter plus switch groundwater
&lgrwt  ,lrest  ,wqagst ,restrt ,&
&newres ,itim   ,nstep  ,dtf    ,steady ,&
&itp    ,ifp    ,ipc    ,idtm   ,juer   ,&
&juresi ,jufrou ,juresd ,justrd ,ker    ,&
&inocon ,jusold ,lfrou  ,itstat ,frobuf ,&
&jugraut,jugralg)

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOSIM (SObek SIMulation)
!
! Module description: Main task of subroutine SOSIM is to perform the
!                     time integration for the user selected applicati-
!                     on.
!
!                     In the time integration for the user selected
!                     simulation period routine SOSIM controls which
!                     subroutines should be activated for the user se-
!                     lected application. Currently four different ap-
!                     plications have been defined. These applications
!                     are:
!
!                     o   Rivers and estuaries without morphology
!                         (SONOMO)
!
!                     Modules involved
!
!                         - Flow
!                         - (optionally salt)
!                         - (optionally sediment)
!
!                     The flow is calculated with time step dtf. The
!                     salt and sediment module are called every
!                     flow time step. For the flow module the steady and
!                     unsteady calculation mode can be used.
!
!                     Initialisation
!
!                     There are no specific initialisation runs.
!
!                     o   River morphology (SORIMO);
!
!                     Modules involved
!
!                         - Flow
!                         - Sediment
!                         - Morphology
!
!                     The flow is calculated with time step dtf. The
!                     sediment module is called every time step.  Depen-
!                     ding on the maximum courant number in the model
!                     the time step can be reduced.
!
!                     Initialisation
!
!                     The application should be initialised. If a res-
!                     tart file has been read the sediment transports
!                     must be calculated. If no restart file has been
!                     read a steady flow step must be calculated
!                     followed by calculation of the sediment trans-
!                     ports.
!
!                     o   Estuary morphology (SOESMO);
!
!                     Modules involved
!
!                         - Flow
!                         - Salt
!                         - Sediment
!                         - Morphology
!
!                     Two different loops are implemented. In the first
!                     loop one or more tides (flow period times) are
!                     calculated and sediment transports and celerities
!                     are avaraged. After this the morphology module is
!                     called and the maximum time step is determined. If
!                     the morphology time step is too large the time
!                     step is adapted.
!
!                     Initialisation
!
!                     The application should be initialised in case a
!                     restart file was not available. A number of tides
!                     must be calculated to reach a played in flow cal-
!                     culation.
!
!                     o   Water quality (SOWQ).
!
!                     Modules involved
!
!                         - Water Quality Interface
!                         - Delwaq
!
!                     Initialisation
!
!                     The application should be initialised. This is the
!                     case if a water quality interface file has been
!                     written by the flow module.
!
!                     All combinations of simulation modules possible
!                     are processed by one or more of these defined
!                     applications.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 19 dtf               I  Time step flow module
! 27 idtm              I  Time step morphology module in whole numbers
!                         of flow step
! 24 ifp               I  Flow period in whole numbers of flow step
! 35 inocon            P  -
! 25 ipc               I  Play in period in whole numbers of flow period
! 26 ipcs              I  -
! 17 itim(2)           I  Actual time level tn+1 expressed in date and
!                         time. Format (integer):
!                         itim(1) = YYYYMMDD (year,month,day)
!                         itim(2) = HHMMSSHH (hour,minute,second,
!                                   hundredth of a second)
! 23 itp               I  Tidal period in whole numbers of flow step
! 38 itstat            P  -
! 28 juer              P  -
! 30 jufrou            P  -
! 31 juresd            P  -
! 29 juresi            P  -
! 36 jusold            P  -
! 32 justrd            P  -
! 33 ker               I  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  2 lauto             I  Switch to execute autostart procedure
! 12 laux              P  -
! 34 ldebug            I  Switch to enable debug mode
!  9 ldlwq             I  Switch to convert water quality files to del-
!                         waq input files
! 11 lestu             I  Switch to indicate estuary case
!  3 lflow             I  Switch to enable flow module
! 37 lfrou             P  -
!  4 lkalm             P  -
!  7 lmorp             I  Logical indicator for morphology computation
!                         = .true.  : with morphology computation
!                         = .false. : without morphology computation
! 13 lrest             I  Switch to indicate that restart file has been
!                         read
! 10 lrivr             I  Switch to indicate river case
!  5 lsalt             O  Logical indicator for salt computation
!                         = .true.  : with salt computation
!                         = .false. : no salt computation
!  6 lsedt             O  Switch to enable sediment transport module
! 22 lustat            P  -
!  8 lwqin             P  -
! 21 lwstat            P  -
! 16 newres            P  -
! 18 nstep             I  Last time step number in simulation.
! 15 restrt            P  -
! 20 steady            I  Switch to enable a steady flow calculation
! 14 wqagst            P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! debmem  DEBug MEMory
! socfil  SObek Close FILes
! soesmo  SObek EStuary MOrphology
! soesti  SObek EStuary TIdal calculation
! soinrm  SObek INitialise River Morphology
! sonomo  SObek NO MOrphology
! sorimo  SObek RIver MOrphology
! sostat  SObek write STATus file
! sotime  SObek TIME
! sowq    SObek Water Quality
! sowres  SObek Write RESults
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sosim.pf,v $
! Revision 1.22  1999/03/15  15:05:22  kuipe_j
! Improve Froude file
!
! Revision 1.21  1998/06/11  11:47:52  kuipe_j
! Estuary special integrated
!
! Revision 1.20  1998/06/08  13:15:41  kuipe_j
! time lag hydr controller
!
! Revision 1.19  1998/02/13  13:23:47  kuipe_j
! Adapt to CMT
!
! Revision 1.18  1997/10/28  09:13:31  kuipe_j
! Improve reductions handling estuary morph.
!
! Revision 1.17  1997/05/26  07:37:04  kuipe_j
! statistic of iteration improved
!
! Revision 1.16  1997/01/23  08:30:17  kuipe_j
! Make flow module robust
!
! Revision 1.15  1996/11/12  15:12:30  kuipe_j
! Declare auxill. arrays later
!
! Revision 1.14  1996/10/31  13:03:50  kuipe_j
! Extra resistance finished, Exchanges are calculated
!
! Revision 1.13  1996/09/03  14:33:45  kuipe_j
! frequency time hist, run in time est. morp
!
! Revision 1.12  1996/05/30  09:55:15  kuipe_j
! no output on t=0 for wat qual
!
! Revision 1.11  1996/04/12  13:06:12  kuipe_j
! headers, minor changes
!
! Revision 1.10  1996/04/11  08:16:40  kuipe_j
! Kalman module added
!
! Revision 1.9  1996/02/09  15:13:40  kuipe_j
! a.o. Restart improvements
!
! Revision 1.8  1996/01/17  14:47:41  kuipe_j
! header update
!
! Revision 1.7  1996/01/16  15:01:56  kuipe_j
! Restart improvements
!
! Revision 1.6  1995/10/18  09:01:08  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.5  1995/09/22  10:04:34  kuipe_j
! variable dimensions, new headers
!
! Revision 1.4  1995/09/12  08:11:35  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.3  1995/05/30  09:57:07  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:10:01  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:27  hoeks_a
! Initial check-in
!
! Revision 1.5  1995/03/08  09:22:35  kuipe_j
! improvement message
!
! Revision 1.4  1994/12/02  13:33:34  kuipe_j
! Improvement of message handling.
!
! Revision 1.3  1994/11/28  08:28:45  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:10:09  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:45  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   use flow_in_datools

   implicit none

!
!     Parameters
!
   logical       lauto ,lflow ,lsalt ,lsedt ,lmorp ,&
   &lgrad ,lkalm ,lrivr ,lestu ,lrest ,&
   &lwqin ,lfrou
!                   mozart declaration
   logical       lgrwt
   integer       itim(2),nstep  ,juresi ,jufrou ,juresd ,&
   &justrd ,jusold ,jugraut,jugralg
   double precision      dtf
   logical       steady ,newres
   integer       itp    ,ifp   ,ipc   ,idtm
   integer       wqagst ,restrt
   integer       juer   ,ker   ,inocon,itstat(4)
   real          frobuf (8)
!
!     Variables
!
   include '..\include\ccomp.i'
!
!     External functions
!
   integer       gtipnt, gtrpnt, gtdpnt, gtcpnt
   external      gtipnt, gtrpnt, gtdpnt, gtcpnt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\mempool.i'

!     Set pointers (Estmorf)
   branch =    gtipnt( 'BRANCH')
   nbran  = ip(gtipnt( 'NBRAN' ))
   ngrid  = ip(gtipnt( 'NGRID' ))
   qpack  =    gtdpnt( 'QPACK' )
   hpack  =    gtdpnt( 'HPACK' )
!
!     Set pointers for SRW
!
   h2 = hpack + ngrid * 2
   q2 = qpack + ngrid * 2
   nhstat = ip (gtipnt ( 'NHSTAT'))
   hstat  =     gtrpnt ( 'HSTAT' )
   nqstat = ip (gtipnt ( 'NQSTAT'))
   qstat  =     gtrpnt ( 'QSTAT' )
   nqlat  = ip (gtipnt ( 'NQLAT' ))
   qlat   =     gtrpnt ( 'QLAT'  )
   storWidth = gtrpnt ( 'WAOFT' ) + ngrid
!
!     First<module> indicates first call to sowres for each module
!
   fflow = .true.
   fsalt = .true.
   fsedt = .true.
   fmorp = .true.
   fkalm = .true.
   fgrad = .true.
!
!     Set local flag Iqwin: 0 = no aggregation for waterquality
!                           1 = standard aggregation for waterquality
!
   if (lwqin) then
      Iwqin = 1
   else
      Iwqin = 0
   endif
!
!     Select application
!
   if ((lflow) .and. .not. (lmorp .or. lgrad)) then
!
!        APPLICATION: Rivers and estuaries without morphology
!
!ESTMORF =====Check=for=special=input========================================
!
      call estmorfini (dtf, nstep, lestmorf, nstart ,juer)
!estmorf --------------------------------------------------------------------
      cpredn = 0
      ostep  = 0

      if (steady) then
         time  = 0D0
      else
         time  = 0D0
         if ( da_running_in_da_tools() ) then
            time = da_get_seconds_from_org_start()
         endif
         call sowres ( dtf    ,itim   ,time   ,ostep  ,nstep  ,&
         &wqagst ,restrt ,newres ,cpredn ,fflow  ,&
         &fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,&
         &lflow  ,lkalm  ,lsalt  ,lsedt  ,lmorp  ,&
         &lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )

         time = time + dtf
         call sotime ( itim, dtf )
      endif
!
      istep  = 1
      filstp = 0


!ESTMORF ===============================================================

!        Schrijven ---------------
      IF ( lestmorf) THEN
         CALL EstMorf( 1, ip( branch),    nbran,  ngrid,&
         &dp( hpack), dp( qpack), dtf)
      ENDIF

!ESTMORF ---------------------------------------------------------------
!
!
   elseif ((lmorp) .and. (lrivr)) then
!
!        APPLICATION: River morphology
!
      time  = 0D0
      istep = 1
      ostep = 0
!
!        River morphology should be initialised. If this is not the
!        case a steady state step must be calculated. If a restart
!        file is available the sediment transports must be calculated
!        because these are not saved on the restart file.
!
      tmpkal = .false.
      if ((.not. lrest) .and. (.not. lauto)) then
!
         tmpstd = .true.
         lsalt  = .false.
         lsedt  = .true.
         filstp = 0
         cpredn = 0
!          mozart  parameters
         lmozad = .false.
         nstepd = 0
!
         call  SONOMO (istep  ,time   ,itim   ,dtf    ,filstp ,cpredn,&
         &tmpstd ,itp    ,tmpkal ,lsalt  ,lsedt  ,lmorp ,&
!                        mozart parameters
         &lmozad ,lgrwt  ,lrest  ,nstepd ,&
         &lrivr  ,juer   ,juresi ,jufrou ,juresd ,justrd,&
         &ker    ,inocon ,jusold ,lfrou  ,itstat ,frobuf)
!
         if (ker .eq. fatal) then
            goto 900
         endif

!
!          Output on time t = 0
!
         call sowres ( dtf    ,itim   ,time   ,ostep  ,nstep  ,&
         &wqagst ,restrt ,newres ,cpredn ,fflow  ,&
         &fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,&
         &lflow  ,tmpkal ,lsalt  ,lsedt  ,lmorp  ,&
         &lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )
      else
!
!           Calculate sediment using h and Q from restart file or
!           auto-start procedure
!
         call soinrm ( time   ,itim   ,dtf    ,tmpkal,&
         &lmorp  ,juer   ,ker    )
!
         if (ker .eq. fatal) then
            goto 900
         endif
!
      endif
!
      time   = 0D0
      istep  = 1
      cpredn = 0

   elseif ((lmorp) .and. (lestu)) then
!
!       APPLICATION: Estuary morphology
!
      time  = 0D0
      istep = 1
!
!       Set temp switches
!
      tmpsed = .false.
      tmpstd = .false.
      tmpmor = .false.
      tmpriv = .false.
      tmpkal = .false.
      filstp = 0
      cpredn = 0
!       mozart  parameters
      lmozad = .false.
      nstepd = 0
!
!       Estuary morphology should be initialised. If this is not the
!       case a user selected number of times tides should be calculated
!
      if (.not. lrest .and. ipc .gt. 1) then
!
!          Initialise time and step number
!
         istep   = 0 - (itp * ifp * ipc)
         time    = 0 - (itp * ifp * ipc * dtf )
         ostep   = istep
!
!          Copy start time into play-in time
!
         ptim(1) = itim(1)
         ptim(2) = itim(2)
!
!          Calculate start date and time for play-in period
!          During play period no sediment calculation
!
         call sotime ( ptim, time )
!
         do 300 it = 1, (itp * ifp * ipc) + 1
!
            call SONOMO(istep ,time  ,ptim   ,dtf   ,filstp ,cpredn,&
            &tmpstd,itp   ,tmpkal ,lsalt ,tmpsed ,tmpmor,&
!                         mozart parameters
            &lmozad,lgrwt ,lrest ,nstepd,&
            &tmpriv,juer  ,juresi ,jufrou,juresd ,justrd,&
            &ker   ,inocon,jusold ,lfrou ,itstat ,frobuf)
!
            if (ker .eq. fatal) then
               goto 900
            endif
!
!             No output of sediment and morphology module
!
            call sowres ( dtf    ,ptim   ,time   ,ostep  ,nstep  ,&
            &wqagst ,restrt ,newres ,cpredn ,fflow  ,&
            &fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,&
            &lflow  ,tmpkal ,lsalt  ,tmpsed ,tmpmor ,&
            &lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )
!
!             Increment step nr and time
!
            istep = istep + 1
            ostep = ostep + 1
            time  = time + dtf
            call sotime ( ptim, dtf )
!
300      continue

      endif
!
!       Now start the estuary morphology case
!
!       Determine flow period, dt morphology
!
      fp  = ifp * itp * dtf
      dtm = idtm * fp
!
      time   = 0.
!
!       Initialize counters
!       istep = counter of flow steps (accumulates in next morfo step)
!       ostep = counter of flow output steps (accumulates in next
!               morfo step)
!       mstep = counter of morfo steps


      istep  = 1
      ostep  = 1
      mstep  = 1
      ffp    = .true.
!
      flwpar = gtrpnt ( 'FLWPAR' )
      thetau = rp(flwpar+2)

!       Set number of time step reductions to zero
!
      nreduc = 0
!
!
   elseif ((lgrad) .and. (lrivr)) then
!
!        APPLICATION: River morphology with graded sediment
!
      time   = 0D0
      istep  = 1
      ostep  = 0
      cpredn = 0
!
!        River morphology should be initialised. If this is not the
!        case a steady state step must be calculated. If a restart
!        file is available the sediment transports must be calculated
!        because these are not saved on the restart file.
!
      if ((.not. lrest) ) then
!
         tmpstd = .true.
!

         call gsnomo ( istep  ,time   ,itim   ,dtf    ,tmpstd ,&
         &juer   ,juresi ,jufrou ,juresd ,justrd ,&
         &ker    ,inocon ,jusold ,lfrou  ,itstat ,&
         &frobuf ,jugraut,jugralg,lrest)
!
         if (ker .eq. fatal) then
            goto 900
         endif
!
!          Output on time t = 0
!
         call sowres ( dtf    ,itim   ,time   ,ostep  ,nstep ,&
         &wqagst ,restrt ,newres ,cpredn ,fflow ,&
         &fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad ,&
         &lflow  ,tmpkal ,lsalt  ,lsedt  ,lmorp ,&
         &lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )

      else
!
!           Calculate sediment using h and Q from restart file
         call GSINRES(istep   ,time   ,itim   ,dtf   ,jugraut ,&
         &jugralg ,juer   , ker   )
!
         if (ker .eq. fatal) then
            goto 900
         endif
!
      endif
!
      time  = 0D0
      istep = 1
   endif
900 continue
   return
end
