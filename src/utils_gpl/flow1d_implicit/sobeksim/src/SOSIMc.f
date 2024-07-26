      subroutine SOSIMC( lflow  ,lkalm  ,
     +                   lsalt  ,lsedt  ,lmorp  ,lgrad  ,
     +                   lwqin  ,lrivr  ,lestu  ,
c                        mozart parameter plus switch groundwater
     +                   lmoza  ,lgrwt  ,lrest  ,
     +                   wqagst ,restrt ,
     +                   newres ,itim   ,nstep  ,dtf    ,
     +                   steady ,lwstat ,lustat ,itp    ,
     +                   ifp    ,ipcs   ,idtm   ,
     +                   juer   ,juresi ,jufrou ,juresd ,
     +                   justrd ,ker    ,ldebug ,inocon ,
     +                   jusold ,lfrou  ,itstat ,juscr  ,
     +                   frobuf ,jugraut,jugralg,step)

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
c     Parameters
c
      logical       lflow ,lsalt ,lsedt ,lmorp ,
     +              lgrad ,lkalm ,lrivr ,lestu ,
     +              lwqin ,ldebug,lwstat,
     +              lfrou
c                   mozart declaration
      logical       lmoza ,lgrwt
      integer       itim(2),nstep  ,juresi ,jufrou ,juresd ,
     +              justrd ,jusold ,juscr  ,jugraut,jugralg, step
      double precision      dtf
      logical       steady ,newres ,lrest
      integer       itp    ,ifp   ,ipcs,   idtm
      integer       wqagst ,restrt
      integer       juer   ,ker   ,lustat,inocon,itstat(4)
      real          frobuf (8)
c
c     Variables
c
      include '..\include\ccomp.i'
c
c     External functions
c
      integer       gtdpnt, gtipnt, gtrpnt, gtcpnt
      external      gtdpnt, gtipnt, gtrpnt, gtcpnt
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\mempool.i'
c
c     Set pointers (Estmorf)
c
      branch =    gtipnt( 'BRANCH') 
      nbran  = ip(gtipnt( 'NBRAN' ))
      ngrid  = ip(gtipnt( 'NGRID' ))
      qpack  =    gtdpnt( 'QPACK')
      hpack  =    gtdpnt( 'HPACK')
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
      if ( istep == 0 ) then
           fflow = .true.
           fsalt = .true.
           fsedt = .true.
           fmorp = .true.
           fkalm = .true.
           fgrad = .true.
      endif
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
         call SONOMO (istep  ,time   ,itim   ,dtf    ,filstp ,cpredn,
     +                steady ,itp    ,lkalm  ,lsalt  ,lsedt  ,lmorp ,
c                     mozart parameters
     +                lmoza  ,lgrwt  ,lrest  ,nstep  ,
     +                lrivr  ,juer   ,juresi ,jufrou ,juresd ,justrd,
     +                ker    ,inocon ,jusold ,lfrou  ,itstat ,frobuf)
c
         if (ldebug) then
            call debmem(2,juer,istep,'SOSIM: na sonomo')
         endif
c
         if (ker .eq. fatal) then
           goto 900
         endif
c
CESTMORF ============================================================

C        Extreme waterstanden bijhouden en Debieten accumuleren ----
         
         IF ( lestmorf) THEN
           IF ( istep .GE. nstart) THEN
             CALL EstMorf( 0, ip( branch),    nbran,  ngrid ,
     &                        dp( hpack), dp( qpack), dtf) 
           ENDIF
         ENDIF

CESTMORF ---------------------------------------------------------------
c 
c
c        In this case output step equals istep
c
         ostep = istep
c
         call sowres ( dtf    ,itim   ,time   ,ostep  ,nstep  ,
     +                 wqagst ,restrt ,newres ,cpredn ,fflow  ,
     +                 fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,
     +                 lflow  ,lkalm  ,lsalt  ,lsedt  ,lmorp  ,
     +                 lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )
c
c        Write status information
c
         call WRLOGO ( ostep,nstep  , 1 , 1, juscr, 1)
         call sostat ( ostep, nstep, lustat, lwstat )
c
c        Increase time
c
         time  = time + dtf
         call sotime ( itim, dtf )
         istep = istep+1
         step  = istep
      elseif ((lmorp) .and. (lrivr)) then
c
         call sorimo ( istep,time, itim, dtf, steady,
     +                 juer ,juresi ,jufrou ,juresd ,justrd ,ker,
     +                 inocon,jusold ,lfrou ,itstat ,frobuf ,lrest)
c
         if (ldebug) then
            call debmem(2,juer,istep,'SOSIM: na sorimo')
         endif
c
         if (ker .eq. fatal) then
            goto 900
         endif
c
c        Increment ostep
c        istep has been incremented in Sorimo
c
         ostep = ostep + 1
c
         call sowres ( dtf    ,itim   ,time   ,ostep  ,nstep  ,
     +                 wqagst ,restrt ,newres ,cpredn ,fflow  ,
     +                 fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,
     +                 lflow  ,tmpkal ,lsalt  ,lsedt  ,lmorp  ,
     +                 lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )
c
c        Write status information
c
         call WRLOGO ( ostep, nstep  , 1 , 1, juscr, 1)
         call sostat ( ostep, nstep, lustat, lwstat )
c
         step = ostep

      elseif ((lmorp) .and. (lestu)) then
c
c        Set number of aggregation steps to zero
c
         steps = 0
c
c        Every new flow period will start with a play-in period
c        if specified (ipcs>0)
c
         if (ipcs .gt. 0 .and. istep .gt. 1) then
c
c           Copy start time of new flow period into play-in time
c
            ptim(1) = itim(1)
            ptim(2) = itim(2)
c
c           Times are set back over play-in period
c
            timeps = itp * ifp * ipcs * dtf
            time   = time - timeps
            time0  = time
            call sotime (ptim,-timeps)
c
c           Set THETA to 1 during the first hour in the play in period
c
            rp(flwpar+2) = 1.

            do 450 it = 1, (itp * ifp * ipcs)
c
               call sotime (ptim , dtf)
               time = time + dtf

               if (time-time0 .gt. 3600.D0 .or. it .gt. 10 .or.
     +             it .ge. itp) then
                  rp(flwpar+2) = thetau
               endif

               call SONOMO(istep ,time  ,ptim  ,dtf   ,filstp ,cpredn,
     +                     tmpstd,itp   ,tmpkal,lsalt ,tmpsed ,tmpmor,
c                          mozart parameters
     +                     lmozad,lgrwt,lrest ,nstepd,      
     +                     tmpriv,juer  ,juresi,jufrou,juresd, justrd,
     +                     ker   ,inocon,jusold,lfrou ,itstat ,frobuf)
c
               if (ker .eq. fatal) then
                  goto 900
               else
                  istep = istep + 1
               endif

 450        continue
            rp(flwpar+2) = thetau
         endif
c
c        Calculate a flow period
c
         do 500 it = 1, itp * ifp
c
            call sotime ( itim, dtf )
            time = time + dtf
c
            call soesti ( istep  ,time  ,itim  ,dtf   ,
     +                    tmpstd ,itp   ,steps ,lsalt ,
     +                    juer   ,juresi,jufrou,juresd,justrd,ker,
     +                    inocon ,jusold,lfrou ,itstat,frobuf,
     +                    lrest  )

            if (ldebug) then
               call debmem(2,juer,istep,'SOSIM: na soesti')
            endif
c
            if (ker .eq. fatal) then
               goto 900
            endif
c
c           Only output during first flow period in morphological
c           step. Nore flow calls are possible due to time step
c           reduction.
c           No Output of morphology module
c
            if (ffp) then
c
c              nostep will be used to detect if it is the last
c              step to be written. This feature will be set off
c              as the last step flow step will be written when
c              a morphological step is written.
c
               nostep = ostep + 1
               call sowres ( dtf    ,itim   ,time   ,ostep  ,nostep ,
     +                       wqagst ,restrt ,newres ,cpredn ,fflow  ,
     +                       fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,
     +                       lflow  ,tmpkal ,lsalt  ,lsedt  ,tmpmor ,
     +                       lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )
c
c               Increment output step
c
                ostep = ostep + 1
            endif
c
c           Increment flow step
c
            istep = istep + 1
c
 500      continue
c
c         Adapt bottom
c
c
          call soesmo ( time   ,itim   ,steps  ,fp     ,
     +                  dtm    ,lastts ,lsalt  ,tmpkal      ,
     +                  nreduc ,juer   ,ker
     +                )
c
          nreduc = nreduc + 1

          if (ldebug) then
             call debmem(2,juer,istep,'SOSIM: na soesmo')
          endif

          if (ker .eq. fatal) then
             goto 900
          endif
c
c         Only write results of morphology module if step
c         reaches original dt morphology ( = dtm)
c
          if (lastts) then
c
c            Reset dtm to original value
c
             dtm = idtm * fp
c
c            Negative sign for dtm to be able to skip the flow, salt,
c            kalman, and sediment his-output.
c
             call sowres (-dtm    ,itim   ,time   ,mstep  ,nstep  ,
     +                     wqagst ,restrt ,newres ,cpredn ,fflow  ,
     +                     fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad  ,
     +                     lflow  ,tmpkal ,lsalt  ,lsedt  ,lmorp  ,
     +                     lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )
c
c            Write status information
c
             call WRLOGO ( mstep, nstep  , 1 , 1, juscr, 1)
             call sostat ( mstep, nstep, lustat, lwstat )
c
c            Update output step morphology module
c
             mstep = mstep + 1
c
c            First flow period becomes true
c
             ffp    = .true.
c
c            Set number of time step reductions to zero
c
             nreduc = 0
          else
             ffp = .false.
          endif
         step = mstep
c
      elseif ((lgrad) .and. (lrivr)) then

         call gsrimo ( istep  ,time   ,itim   ,dtf    ,steady ,
     +                 juer   ,juresi ,jufrou ,juresd ,justrd ,
     +                 ker    ,inocon ,jusold ,lfrou  ,itstat ,
     +                 frobuf ,jugraut,jugralg,lrest)
c
         if (ldebug) then
            call debmem(2,juer,istep,'SOSIM: na gsrimo')
         endif
c
         if (ker .eq. fatal) then
            goto 900
         endif
c
c        Increment ostep
c
         ostep = ostep + 1
         istep = istep + 1
c
         call sowres ( dtf    ,itim   ,time   ,ostep  ,nstep ,
     +                 wqagst ,restrt ,newres ,cpredn ,fflow ,
     +                 fkalm  ,fsalt  ,fsedt  ,fmorp  ,fgrad ,
     +                 lflow  ,tmpkal ,lsalt  ,lsedt  ,lmorp ,
     +                 lgrad  ,Iwqin  ,lgrwt  ,juer   ,ker    )

c 
c        Write status information
c
         call WRLOGO ( ostep, nstep  , 1 , 1, juscr, 1)
         call sostat ( ostep, nstep, lustat, lwstat )
         step = ostep
      endif
900   continue
      
      end
