program sobeksim

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOBEK (SOBEK)
!
! Module description: Program SOBEK is the main program.
!
!                     Sobek has the possibility to process one model
!                     file or more model files in batch mode. Sobek will
!                     read the model database by calling module SOGETM.
!                     This routine reads the element names of the nefis
!                     input file [6] and allocates memory in the memory
!                     pools. After allocating the contents of the arrays
!                     on the input file are copied to the memory pool.
!                     The next step is calling routine SODECL. This
!                     routine allocates variables which are needed for
!                     the simulation modules in the application but
!                     which are not received from the user interface.
!                     Examples of these variables are arrays for storage
!                     of temporary data. When all variables are alloca-
!                     ted in memory the memory management routine SRTMEM
!                     is called to sort the variable names in alphabeti-
!                     cal order.
!
!                     The runtime parameters (number of time steps, time
!                     step etc.) are extracted from the input arrays by
!                     routine SOSRUN. After this several initialisations
!                     will be made in routine SOINIT. After initialisa-
!                     tion routine SOSIM will be activated. This routine
!                     will perform the model simulation for the user
!                     selected application. For a selected application
!                     in subroutine SOSIM the simulation will be perfor-
!                     med starting from the user specified begin time
!                     until the end time, apart from user interrupts.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!    simula               -
!    subrou               -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! debmem  DEBug MEMory
! dmpmem  DuMP MEMory
! error   write an ERROR to the error file.
! inimem  INItialise MEMory pools
! motrin  MOrphology TRaject definition INput file
! soaux   SObek AUXilliary output
! socnam  SObek Create NAMe
! sodecl  SObek DECLare variables
! sogarg  SObek Get ARGument
! sogetm  SObek GET Model info
! soinit  SObek INITialise
! sosim   SObek SIMulation
! sosrun  SObek Simulation RUN parameters
! srtmem  SoRT MEMory
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
! $Log: sobek.pf,v $
! Revision 1.49  1999/07/23  15:10:35  kuipe_j
! improve restart
!
! Revision 1.48  1999/06/01  13:42:44  kuipe_j
! names in messages substituted + message template
!
! Revision 1.47  1999/04/22  08:45:18  kuipe_j
! remove initial ODA/ODF and WDA/WDF files
!
! Revision 1.46  1999/03/15  15:19:34  kuipe_j
! tabs removed
!
! Revision 1.45  1998/12/11  13:08:17  kuipe_j
! Improve return code
!
! Revision 1.42  1998/07/06  09:05:24  kuipe_j
! Improve logo
!
! Revision 1.41  1998/06/24  11:10:32  kuipe_j
! Try direct solver if BICGST fails
!
! Revision 1.40  1998/06/11  11:47:35  kuipe_j
! Estuary special integrated
!
! Revision 1.39  1998/06/08  13:15:34  kuipe_j
! time lag hydr controller
!
! Revision 1.38  1998/05/25  19:16:50  kuipe_j
! Wendy structures
!
! Revision 1.37  1998/02/23  13:41:27  kuipe_j
! small CMT  change
!
! Revision 1.36  1998/02/13  13:23:45  kuipe_j
! Adapt to CMT
!
! Revision 1.35  1997/11/04  14:21:56  kuipe_j
! Retention basin
!
! Revision 1.34  1997/11/04  14:19:56  kuipe_j
! Retention basin
!
! Revision 1.33  1997/07/11  10:36:24  kuipe_j
! Structure length set to zero
!
! Revision 1.32  1997/07/10  14:21:05  kuipe_j
! C=f(h), E=0 structure
!
! Revision 1.31  1997/06/18  09:12:52  kuipe_j
! rel nr
!
! Revision 1.30  1997/06/17  11:29:16  kuipe_j
! output in history format
!
! Revision 1.29  1997/06/04  11:20:16  kuipe_j
! Initialize arrays
!
! Revision 1.28  1997/05/26  07:36:57  kuipe_j
! statistic of iteration improved
!
! Revision 1.27  1997/03/13  12:30:29  kuipe_j
! Error in Status file properly set
!
! Revision 1.26  1997/02/17  10:09:37  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.25  1997/01/23  08:30:05  kuipe_j
! Make flow module robust
!
! Revision 1.24  1997/01/07  10:33:47  kuipe_j
! rel nr
!
! Revision 1.23  1996/11/12  15:12:23  kuipe_j
! Declare auxill. arrays later
!
! Revision 1.22  1996/11/01  11:25:43  kuipe_j
! Update of Delwaq input file added
!
! Revision 1.21  1996/10/31  13:03:48  kuipe_j
! Extra resistance finished, Exchanges are calculated
!
! Revision 1.20  1996/09/03  14:33:37  kuipe_j
! frequency time hist, run in time est. morp
!
! Revision 1.19  1996/05/31  12:55:46  kuipe_j
! keep mimimum level for proportional distr.
!
! Revision 1.18  1996/05/28  13:29:24  kuipe_j
! Error message courant nr added
!
! Revision 1.17  1996/04/12  13:05:55  kuipe_j
! headers, minor changes
!
! Revision 1.16  1996/04/11  08:16:20  kuipe_j
! Kalman module added
!
! Revision 1.15  1996/03/07  10:44:27  kuipe_j
! Bottom scema acc. to Lax Wendroff with flux limitter
!
! Revision 1.14  1996/02/09  15:13:36  kuipe_j
! a.o. Restart improvements
!
! Revision 1.13  1996/01/16  15:01:50  kuipe_j
! Restart improvements
!
! Revision 1.12  1995/12/06  08:55:52  hoeks_a
! Aux. output removed
!
! Revision 1.11  1995/11/21  11:09:06  kuipe_j
! Added features are: Special morphology output for IVR; Improvement of
!     auxilliary output; Automatic speudo time stepping; general structure
!     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
!     relation); removal of grid points in messages; etc.
!
! Revision 1.10  1995/10/18  09:00:54  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.9  1995/10/11  12:24:11  kuipe_j
! Remove aux output temp
!
! Revision 1.8  1995/09/29  10:36:34  kuipe_j
! Improvement of autostart and simple weir
!
! Revision 1.7  1995/09/22  10:03:24  kuipe_j
! variable dimensions, new headers
!
! Revision 1.6  1995/09/12  08:11:26  overmar
! - Option "zomerkaden" added
! - Better linearization
! - Pseudo time
! - Iterative matrix solution
!
! Revision 1.5  1995/08/30  12:37:28  kuipe_j
! Triggers + controllers for BOS
!
! Revision 1.4  1995/08/23  14:29:49  overmar
! Lelystad juli 95 ingebracht
!
! Revision 1.3  1995/05/30  09:56:40  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:09:33  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:11:58  hoeks_a
! Initial check-in
!
! Revision 1.4  1994/12/02  13:33:25  kuipe_j
! Improvement of message handling.
!
! Revision 1.3  1994/11/28  08:28:31  kuipe_j
! Time and timestep in double precision.
!
! Revision 1.2  1993/11/26  15:09:28  kuipe_j
! Update after  finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:40  kuipe_j
! Initial version
!
!
!***********************************************************************
!
   use sobeksim_version_module

   implicit none
   include '..\include\filsim.i'
!
   integer       lubtc,  lustat, ker
   integer       itim(2),nstep,  wqagst, restrt
   integer       itp,    ifp,    ipc,    ipcs,   idtm
   integer       sbkrel(3), step, juer
   integer       juresi ,jufrou ,juresd,  justrd, jusold
   integer       inocon ,itstat(4) ,juscr,lurtn,  jugraut,jugralg

   logical       lauto,  lflow,  lkalm,  lsalt,  lsedt
   logical       lgrad,  lmorp,  lwqin,  ldlwq,  laux
   logical       lrivr,  lestu,  lrest,  ldebug
   logical       lbatch, steady, lwstat, newres
   logical       lfrou
!     mozart declaration
   logical       lmoza, lgrwt
!
   double precision      dtf
   real                  frobuf(8)
   real                  strcpu

   character*256 filnam
!
!     External functions
!
   integer  gtipnt, gtrpnt
   external gtipnt, gtrpnt
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\memplf90.i'

!
!     Logical unit numbers
!
   juer    =  11
   lubtc   =  12
   lustat  =  13
   juscr   =   6
   lurtn   =  14
   jugraut = 112
   jugralg = 113
!     Start initializing

   call soini1( sbkrel, itstat, lbatch, lfrou, ldebug, lurtn,&
   &juscr, filnam, ker )


!     If batch file read names from batch input file


   if (lbatch) then
      open ( unit = lubtc, file = filnam )
   endif

!     Loop for batches
100 continue

   if (lbatch) then
      read ( unit = lubtc, fmt = '(A64)', end = 900 ) filnam
   endif

!     Check whether still ok:

   if ( ker .ne. fatal ) then

      ker = ok

      call soini2( sbkrel, itstat, lfrou , ldebug, juscr , juer,&
      &juresi, jufrou, juresd, justrd, jusold, itim,&
      &nstep , itp,    ifp,    ipc,    ipcs,   wqagst,&
      &restrt, idtm,   lustat, jugraut,jugralg,inocon,&
      &frobuf, strcpu, dtf,    lauto,  lflow,  lkalm,&
      &lsalt,  lsedt,  lmorp,  lgrad,  lwqin,  ldlwq,&
      &laux,   lmoza,  lgrwt,  lrivr,  lestu,&
      &steady, lwstat, newres, lrest,  filnam, ker)

   endif

!     Check whether still ok:


   if ( ker .ne. fatal ) then

      ker = ok

      call SOSIMi (lauto  ,lflow  ,lkalm  ,lsalt  ,lsedt  ,&
      &lmorp  ,lgrad  ,lwqin  ,lrivr  ,lestu  ,&
!                     mozart parameter plus switch groundwater
      &lgrwt  ,lrest  ,wqagst ,restrt ,&
      &newres ,itim   ,nstep  ,dtf    ,steady ,&
      &itp    ,ifp    ,ipc    ,idtm   ,juer   ,&
      &juresi ,jufrou ,juresd ,justrd ,ker    ,&
      &inocon ,jusold ,lfrou  ,itstat ,frobuf ,&
      &jugraut,jugralg)

      if ( ker .ne. fatal ) ker = ok

   endif

   step = 1

   do while ( (step < nstep + 1 ) .and. (ker < fatal ) )

      call SOSIMc( lflow  ,lkalm  ,&
      &lsalt  ,lsedt  ,lmorp  ,lgrad  ,&
      &lwqin  ,lrivr  ,lestu  ,&
!                     mozart parameter plus switch groundwater
      &lmoza  ,lgrwt  ,lrest  ,&
      &wqagst ,restrt ,&
      &newres ,itim   ,nstep  ,dtf    ,&
      &steady ,lwstat ,lustat ,itp    ,&
      &ifp    ,ipcs   ,idtm   ,&
      &juer   ,juresi ,jufrou ,juresd ,&
      &justrd ,ker    ,ldebug ,inocon ,&
      &jusold ,lfrou  ,itstat ,juscr  ,&
      &frobuf ,jugraut,jugralg,step)

      if ( ker .ne. fatal ) ker = ok

   enddo

!     Check whether still ok:

   if ( ker == ok ) then

      call SOSIMf ( lflow  ,lsalt  ,lmorp  ,lgrad  ,lwqin  ,&
      &ldlwq  ,laux   ,newres ,itim   ,&
      &nstep  ,dtf    ,juer   ,ker    ,ldebug )

      if ( ker .ne. fatal ) ker = ok

   endif

!     Check whether still ok:

   if ( ker == ok ) then

      call sofin( frobuf, strcpu, lfrou , lwstat, juer, inocon,&
      &lustat, lurtn , itstat, ker   )

   endif
!
!     until last name processed
!
   if (lbatch) then
      goto 100
   endif

900 continue

   if (lbatch) then
      close ( unit = lubtc )
   endif

end
