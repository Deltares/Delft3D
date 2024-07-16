      subroutine SOSIMi (lauto  ,lflow  ,lkalm  ,lsalt  ,lsedt  ,
     +                   lmorp  ,lgrad  ,lwqin  ,lrivr  ,lestu  ,
c                        mozart parameter plus switch groundwater
     +                   lgrwt  ,lrest  ,wqagst ,restrt ,
     +                   newres ,itim   ,nstep  ,dtf    ,steady ,
     +                   itp    ,ifp    ,ipc    ,idtm   ,juer   ,
     +                   juresi ,jufrou ,juresd ,justrd ,ker    ,
     +                   inocon ,jusold ,lfrou  ,itstat ,frobuf ,
     +                   jugraut,jugralg)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOSIM (SObek SIMulation)
c
c Module description: Main task of subroutine SOSIM is to perform the
c                     time integration for the user selected applicati-
c                     on.
c
c                     In the time integration for the user selected
c                     simulation period routine SOSIM controls which
c                     subroutines should be activated for the user se-
c                     lected application. Currently four different ap-
c                     plications have been defined. These applications
c                     are:
c
c                     o   Rivers and estuaries without morphology
c                         (SONOMO)
c
c                     Modules involved
c
c                         - Flow
c                         - (optionally salt)
c                         - (optionally sediment)
c
c                     The flow is calculated with time step dtf. The
c                     salt and sediment module are called every
c                     flow time step. For the flow module the steady and
c                     unsteady calculation mode can be used.
c
c                     Initialisation
c
c                     There are no specific initialisation runs.
c
c                     o   River morphology (SORIMO);
c
c                     Modules involved
c
c                         - Flow
c                         - Sediment
c                         - Morphology
c
c                     The flow is calculated with time step dtf. The
c                     sediment module is called every time step.  Depen-
c                     ding on the maximum courant number in the model
c                     the time step can be reduced.
c
c                     Initialisation
c
c                     The application should be initialised. If a res-
c                     tart file has been read the sediment transports
c                     must be calculated. If no restart file has been
c                     read a steady flow step must be calculated
c                     followed by calculation of the sediment trans-
c                     ports.
c
c                     o   Estuary morphology (SOESMO);
c
c                     Modules involved
c
c                         - Flow
c                         - Salt
c                         - Sediment
c                         - Morphology
c
c                     Two different loops are implemented. In the first
c                     loop one or more tides (flow period times) are
c                     calculated and sediment transports and celerities
c                     are avaraged. After this the morphology module is
c                     called and the maximum time step is determined. If
c                     the morphology time step is too large the time
c                     step is adapted.
c
c                     Initialisation
c
c                     The application should be initialised in case a
c                     restart file was not available. A number of tides
c                     must be calculated to reach a played in flow cal-
c                     culation.
c
c                     o   Water quality (SOWQ).
c
c                     Modules involved
c
c                         - Water Quality Interface
c                         - Delwaq
c
c                     Initialisation
c
c                     The application should be initialised. This is the
c                     case if a water quality interface file has been
c                     written by the flow module.
c
c                     All combinations of simulation modules possible
c                     are processed by one or more of these defined
c                     applications.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 19 dtf               I  Time step flow module
c 27 idtm              I  Time step morphology module in whole numbers
c                         of flow step
c 24 ifp               I  Flow period in whole numbers of flow step
c 35 inocon            P  -
c 25 ipc               I  Play in period in whole numbers of flow period
c 26 ipcs              I  -
c 17 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c 23 itp               I  Tidal period in whole numbers of flow step
c 38 itstat            P  -
c 28 juer              P  -
c 30 jufrou            P  -
c 31 juresd            P  -
c 29 juresi            P  -
c 36 jusold            P  -
c 32 justrd            P  -
c 33 ker               I  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  2 lauto             I  Switch to execute autostart procedure
c 12 laux              P  -
c 34 ldebug            I  Switch to enable debug mode
c  9 ldlwq             I  Switch to convert water quality files to del-
c                         waq input files
c 11 lestu             I  Switch to indicate estuary case
c  3 lflow             I  Switch to enable flow module
c 37 lfrou             P  -
c  4 lkalm             P  -
c  7 lmorp             I  Logical indicator for morphology computation
c                         = .true.  : with morphology computation
c                         = .false. : without morphology computation
c 13 lrest             I  Switch to indicate that restart file has been
c                         read
c 10 lrivr             I  Switch to indicate river case
c  5 lsalt             O  Logical indicator for salt computation
c                         = .true.  : with salt computation
c                         = .false. : no salt computation
c  6 lsedt             O  Switch to enable sediment transport module
c 22 lustat            P  -
c  8 lwqin             P  -
c 21 lwstat            P  -
c 16 newres            P  -
c 18 nstep             I  Last time step number in simulation.
c 15 restrt            P  -
c 20 steady            I  Switch to enable a steady flow calculation
c 14 wqagst            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c debmem  DEBug MEMory
c socfil  SObek Close FILes
c soesmo  SObek EStuary MOrphology
c soesti  SObek EStuary TIdal calculation
c soinrm  SObek INitialise River Morphology
c sonomo  SObek NO MOrphology
c sorimo  SObek RIver MOrphology
c sostat  SObek write STATus file
c sotime  SObek TIME
c sowq    SObek Water Quality
c sowres  SObek Write RESults
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sosim.pf,v $
c Revision 1.22  1999/03/15  15:05:22  kuipe_j
c Improve Froude file
c
c Revision 1.21  1998/06/11  11:47:52  kuipe_j
c Estuary special integrated
c
c Revision 1.20  1998/06/08  13:15:41  kuipe_j
c time lag hydr controller
c
c Revision 1.19  1998/02/13  13:23:47  kuipe_j
c Adapt to CMT
c
c Revision 1.18  1997/10/28  09:13:31  kuipe_j
c Improve reductions handling estuary morph.
c
c Revision 1.17  1997/05/26  07:37:04  kuipe_j
c statistic of iteration improved
c
c Revision 1.16  1997/01/23  08:30:17  kuipe_j
c Make flow module robust
c
c Revision 1.15  1996/11/12  15:12:30  kuipe_j
c Declare auxill. arrays later
c
c Revision 1.14  1996/10/31  13:03:50  kuipe_j
c Extra resistance finished, Exchanges are calculated
c
c Revision 1.13  1996/09/03  14:33:45  kuipe_j
c frequency time hist, run in time est. morp
c
c Revision 1.12  1996/05/30  09:55:15  kuipe_j
c no output on t=0 for wat qual
c
c Revision 1.11  1996/04/12  13:06:12  kuipe_j
c headers, minor changes
c
c Revision 1.10  1996/04/11  08:16:40  kuipe_j
c Kalman module added
c
c Revision 1.9  1996/02/09  15:13:40  kuipe_j
c a.o. Restart improvements
c
c Revision 1.8  1996/01/17  14:47:41  kuipe_j
c header update
c
c Revision 1.7  1996/01/16  15:01:56  kuipe_j
c Restart improvements
c
c Revision 1.6  1995/10/18  09:01:08  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.5  1995/09/22  10:04:34  kuipe_j
c variable dimensions, new headers
c
c Revision 1.4  1995/09/12  08:11:35  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.3  1995/05/30  09:57:07  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:10:01  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:27  hoeks_a
c Initial check-in
c
c Revision 1.5  1995/03/08  09:22:35  kuipe_j
c improvement message
c
c Revision 1.4  1994/12/02  13:33:34  kuipe_j
c Improvement of message handling.
c
c Revision 1.3  1994/11/28  08:28:45  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:10:09  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:45  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      use flow_in_datools

      implicit none

c
c     Parameters
c
      logical       lauto ,lflow ,lsalt ,lsedt ,lmorp ,
     +              lgrad ,lkalm ,lrivr ,lestu ,lrest ,
     +              lwqin ,lfrou
c                   mozart declaration
      logical       lgrwt
      integer       itim(2),nstep  ,juresi ,jufrou ,juresd ,
     +              justrd ,jusold ,jugraut,jugralg
      double precision      dtf
      logical       steady ,newres
      integer       itp    ,ifp   ,ipc   ,idtm
      integer       wqagst ,restrt
      integer       juer   ,ker   ,inocon,itstat(4)
      real          frobuf (8)
c
c     Variables
c
      include '..\include\ccomp.i'
c
c     External functions
c
      integer       gtipnt, gtrpnt, gtdpnt, gtcpnt
      external      gtipnt, gtrpnt, gtdpnt, gtcpnt
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\mempool.i'

c     Set pointers (Estmorf)
      branch =    gtipnt( 'BRANCH') 
      nbran  = ip(gtipnt( 'NBRAN' ))
      ngrid  = ip(gtipnt( 'NGRID' ))
      qpack  =    gtdpnt( 'QPACK' )
      hpack  =    gtdpnt( 'HPACK' )
c
c     Set pointers for SRW
c
      h2 = hpack + ngrid * 2
      q2 = qpack + ngrid * 2
      nhstat = ip (gtipnt ( 'NHSTAT'))
      hstat  =     gtrpnt ( 'HSTAT' )
      nqstat = ip (gtipnt ( 'NQSTAT'))
      qstat  =     gtrpnt ( 'QSTAT' )
      nqlat  = ip (gtipnt ( 'NQLAT' ))
      qlat   =     gtrpnt ( 'QLAT'  )
      storWidth = gtrpnt ( 'WAOFT' ) + ngrid
c
c     First<module> indicates first call to sowres for each module
c
      fflow = .true.
      fsalt = .true.
      fsedt = .true.
      fmorp = .true.
      fkalm = .true.
      fgrad = .true.
c
c     Set local flag Iqwin: 0 = no aggregation for waterquality
c                           1 = standard aggregation for waterquality 
c
      if (lwqin) then
         Iwqin = 1
      else
         Iwqin = 0
      endif 
c
c     Select application
c
      if ((lflow) .and. .not. (lmorp .or. lgrad)) then
c
c        APPLICATION: Rivers and estuaries without morphology
c
CESTMORF =====Check=for=special=input========================================
c
         call estmorfini (dtf, nstep, lestmorf, nstart ,juer)
cestmorf --------------------------------------------------------------------
         cpredn = 0
         ostep  = 0

         if (steady) then
            time  = 0D0
         else
            time  = 0D0
            if ( da_running_in_da_tools() ) then
               time = da_get_seconds_from_org_start()
            endif
            call sowres ( dtf    ,itim   ,time   ,ostep  ,nstep  ,
     +                    wqagst ,restrt ,newres ,cpredn ,fflow  ,
     +                    fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,
     +                    lflow  ,lkalm  ,lsalt  ,lsedt  ,lmorp  ,
     +                    lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )

            time = time + dtf
            call sotime ( itim, dtf )
         endif
c
      istep  = 1
         filstp = 0


CESTMORF ===============================================================

C        Schrijven ---------------
         IF ( lestmorf) THEN 
           CALL EstMorf( 1, ip( branch),    nbran,  ngrid,
     &                      dp( hpack), dp( qpack), dtf) 
         ENDIF

CESTMORF ---------------------------------------------------------------
c
c
      elseif ((lmorp) .and. (lrivr)) then
c
c        APPLICATION: River morphology
c
         time  = 0D0
         istep = 1
         ostep = 0
c
c        River morphology should be initialised. If this is not the
c        case a steady state step must be calculated. If a restart
c        file is available the sediment transports must be calculated
c        because these are not saved on the restart file.
c
         tmpkal = .false.
         if ((.not. lrest) .and. (.not. lauto)) then
c
           tmpstd = .true.
           lsalt  = .false.
           lsedt  = .true.
           filstp = 0
           cpredn = 0
c          mozart  parameters
           lmozad = .false.
           nstepd = 0
c
           call  SONOMO (istep  ,time   ,itim   ,dtf    ,filstp ,cpredn,
     +                   tmpstd ,itp    ,tmpkal ,lsalt  ,lsedt  ,lmorp ,
c                        mozart parameters
     +                   lmozad ,lgrwt  ,lrest  ,nstepd ,
     +                   lrivr  ,juer   ,juresi ,jufrou ,juresd ,justrd,
     +                   ker    ,inocon ,jusold ,lfrou  ,itstat ,frobuf)
c
           if (ker .eq. fatal) then
              goto 900
           endif
 
c
c          Output on time t = 0
c
           call sowres ( dtf    ,itim   ,time   ,ostep  ,nstep  ,
     +                   wqagst ,restrt ,newres ,cpredn ,fflow  ,
     +                   fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,
     +                   lflow  ,tmpkal ,lsalt  ,lsedt  ,lmorp  ,
     +                   lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )
         else
c
c           Calculate sediment using h and Q from restart file or
c           auto-start procedure
c
            call soinrm ( time   ,itim   ,dtf    ,tmpkal,
     +                    lmorp  ,juer   ,ker    )
c
            if (ker .eq. fatal) then
              goto 900
            endif
c
         endif
c
         time   = 0D0
         istep  = 1
         cpredn = 0

      elseif ((lmorp) .and. (lestu)) then
c
c       APPLICATION: Estuary morphology
c
        time  = 0D0
        istep = 1
c
c       Set temp switches
c
        tmpsed = .false.
        tmpstd = .false.
        tmpmor = .false.
        tmpriv = .false.
        tmpkal = .false.
        filstp = 0
        cpredn = 0
c       mozart  parameters
        lmozad = .false.
        nstepd = 0
c
c       Estuary morphology should be initialised. If this is not the
c       case a user selected number of times tides should be calculated
c
        if (.not. lrest .and. ipc .gt. 1) then
c
c          Initialise time and step number
c
           istep   = 0 - (itp * ifp * ipc)
           time    = 0 - (itp * ifp * ipc * dtf )
           ostep   = istep
c
c          Copy start time into play-in time
c
           ptim(1) = itim(1)
           ptim(2) = itim(2)
c
c          Calculate start date and time for play-in period
c          During play period no sediment calculation
c
           call sotime ( ptim, time )
c
           do 300 it = 1, (itp * ifp * ipc) + 1
c
              call SONOMO(istep ,time  ,ptim   ,dtf   ,filstp ,cpredn,
     +                    tmpstd,itp   ,tmpkal ,lsalt ,tmpsed ,tmpmor,
c                         mozart parameters
     +                    lmozad,lgrwt ,lrest ,nstepd,
     +                    tmpriv,juer  ,juresi ,jufrou,juresd ,justrd,
     +                    ker   ,inocon,jusold ,lfrou ,itstat ,frobuf)
c
              if (ker .eq. fatal) then
                 goto 900
              endif
c
c             No output of sediment and morphology module
c
              call sowres ( dtf    ,ptim   ,time   ,ostep  ,nstep  ,
     +                      wqagst ,restrt ,newres ,cpredn ,fflow  ,
     +                      fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,
     +                      lflow  ,tmpkal ,lsalt  ,tmpsed ,tmpmor ,
     +                      lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )
c
c             Increment step nr and time
c
              istep = istep + 1
              ostep = ostep + 1
              time  = time + dtf
              call sotime ( ptim, dtf )
c
 300        continue

        endif
c
c       Now start the estuary morphology case
c
c       Determine flow period, dt morphology
c
        fp  = ifp * itp * dtf
        dtm = idtm * fp
c
        time   = 0.
c
c       Initialize counters
c       istep = counter of flow steps (accumulates in next morfo step)
c       ostep = counter of flow output steps (accumulates in next
c               morfo step)     
c       mstep = counter of morfo steps


        istep  = 1
        ostep  = 1
        mstep  = 1
        ffp    = .true.
c
        flwpar = gtrpnt ( 'FLWPAR' )
        thetau = rp(flwpar+2)

c       Set number of time step reductions to zero
c
        nreduc = 0
c
c
      elseif ((lgrad) .and. (lrivr)) then
c
c        APPLICATION: River morphology with graded sediment
c
         time   = 0D0
         istep  = 1
         ostep  = 0
         cpredn = 0
c
c        River morphology should be initialised. If this is not the
c        case a steady state step must be calculated. If a restart
c        file is available the sediment transports must be calculated
c        because these are not saved on the restart file.
c
         if ((.not. lrest) ) then
c
           tmpstd = .true.
c

           call gsnomo ( istep  ,time   ,itim   ,dtf    ,tmpstd ,
     +                   juer   ,juresi ,jufrou ,juresd ,justrd ,
     +                   ker    ,inocon ,jusold ,lfrou  ,itstat ,
     +                   frobuf ,jugraut,jugralg,lrest)
c
           if (ker .eq. fatal) then
              goto 900
           endif
c
c          Output on time t = 0
c
           call sowres ( dtf    ,itim   ,time   ,ostep  ,nstep ,
     +                   wqagst ,restrt ,newres ,cpredn ,fflow ,
     +                   fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad ,
     +                   lflow  ,tmpkal ,lsalt  ,lsedt  ,lmorp ,
     +                   lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )
     
         else
c
c           Calculate sediment using h and Q from restart file 
            call GSINRES(istep   ,time   ,itim   ,dtf   ,jugraut ,
     &                   jugralg ,juer   , ker   )
c
            if (ker .eq. fatal) then
               goto 900
            endif
c
         endif
c
         time  = 0D0
         istep = 1
      endif
900   continue
      return
      end
