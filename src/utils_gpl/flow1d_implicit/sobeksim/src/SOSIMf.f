      subroutine SOSIMf (lflow  ,lsalt  ,lmorp  ,lgrad  ,lwqin  ,
     +                   ldlwq  ,laux   ,newres ,itim   ,
     +                   nstep  ,dtf    ,juer   ,ker    ,ldebug )

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
      include '..\include\ccomp.i'

c
c     Parameters
c
      logical       lflow ,lsalt ,lmorp ,lgrad ,
     +              lwqin ,ldlwq ,laux  ,ldebug
c                   mozart declaration
      integer       itim(2),nstep
      double precision      dtf
      logical       newres
      integer       juer   ,ker
c
c     Variables
c
      logical       open
c
c     Pointers to arrays and single integer values
c
      integer       flwrun, hyrtim, saltim, nhytim, nsatim,
     +              hmax  , hmin ,  tmaxh,  tminh,  gridnm,
     +              qmax,   qmin,   tmaxq,  tminq,  hpack3, qpack3 
c
c     External functions
c
      integer, external   ::    gtipnt, gtrpnt, gtcpnt, gtdpnt
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
      if ((lflow) .and. .not. (lmorp .or. lgrad)) then

CESTMORF ===============================================================

C        Schrijven ---------------
         IF ( lestmorf) THEN 
           CALL EstMorf( 1, ip( branch),    nbran,  ngrid,
     &                      dp( hpack), dp( qpack), dtf) 
         ENDIF

CESTMORF ---------------------------------------------------------------
c
c
      endif
c
c     Close the nefis files, water quality file will be opened
c     by subroutine wqint

 900  continue
c
c
c     Output of maxima and minima
c
c     Set pointers
c
      gridnm  = gtcpnt('GRIDNM')
      hmax    = gtrpnt('HMAX'  )
      qpack   = gtdpnt('QPACK' )

      hmin    = hmax  + ngrid
      tmaxh   = hmin  + ngrid
      tminh   = tmaxh + ngrid
      qmax    = tminh + ngrid
      qmin    = qmax  + ngrid
      tmaxq   = qmin  + ngrid
      tminq   = tmaxq + ngrid

cARS07786 Extra pointers ingevoerd en extra parameters in
c         call naar FLMINMAX
      
      hpack3  = hpack + ngrid * 2
      qpack3  = qpack + ngrid * 2
c
      call FLMINMAX (3  ,ngrid  ,istep ,nstep  ,dtf   ,itim   ,
     &               dp(hpack3) ,rp(hmin)      ,rp(hmax)      ,
     &               rp(tminh)  ,rp(tmaxh)     ,
     &               dp(qpack3) ,rp(qmin)      ,rp(qmax)      ,
     &               rp(tminq)  ,rp(tmaxq)     ,cp(gridnm)    )
c                          
      call socfil ( lflow, lwqin ,newres)
c
c     Test if water quality has been included in application
c
      if (ldlwq .and. ker .ne. fatal) then
c
c        APPLICATION: water quality
c
        call sowq ( laux, juer, ker )
        if (ldebug) then
           call debmem(2,juer,istep,'SOSIM: na sowq')
        endif
      endif
c
c     Tidal analyses
c
      if (ker .ne. fatal) then
         flwrun  =    gtrpnt('FLWRUN')
         hyrtim  =    gtipnt('HYRTIM') 
         nhytim  = ip(gtipnt('NHYTIM'))
         if (lsalt) then
            saltim  =    gtipnt('SALTIM') 
            nsatim  = ip(gtipnt('NSATIM'))
         else
c           define dummy values
            saltim = 1
            nsatim = 0
         endif   
c
c        close files to be processed
c
         inquire (51,opened=open)
         if (open) close (51)
         inquire (55,opened=open)
         if (open) close (55)
c      
         call GAGetParAnalys (rp(flwrun),ip(hyrtim),nhytim ,ip(saltim),
     +                           nsatim ,   juer   ,ker    )
      endif 
c
      return
      end
