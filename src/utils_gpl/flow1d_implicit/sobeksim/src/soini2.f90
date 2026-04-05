subroutine soini2(sbkrel, itstat, lfrou , ldebug, juscr , juer,&
&juresi, jufrou, juresd, justrd, jusold, itim,&
&nstep , itp,    ifp,    ipc,    ipcs,   wqagst,&
&restrt, idtm,   lustat, jugraut,jugralg,inocon,&
&frobuf, strcpu, dtf,    lauto,  lflow,  lkalm,&
&lsalt,  lsedt,  lmorp,  lgrad,  lwqin,  ldlwq,&
&laux,   lmoza,  lgrwt,  lrivr,  lestu,&
&steady, lwstat, newres, lrest,  filnam, ker)
   implicit none
   include '..\include\filsim.i'
   integer ker, juer, juresi, jufrou,&
   &juresd, justrd, jusold, itim(2),&
   &nstep, itp, ifp, ipc, ipcs,&
   &wqagst, restrt, idtm, lustat, juscr, sbkrel(2),&
   &itstat(4), jugraut, jugralg, inocon
   real    frobuf(8)
   real    strcpu

   double precision dtf
   logical ldebug, lhisgp, lauto, lflow, lkalm,&
   &lsalt, lsedt, lmorp, lgrad, lwqin, ldlwq, laux, lmoza,&
   &lgrwt, lrivr, lestu, steady, lwstat, newres, lrest,&
   &lfrou

   character*256 filnam
   character*1   coding
   character*120 appl
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
   include '..\include\mempool.i'
!
!     Initialise memory pools
!
   call inimem
!
!     Open error log file
!
   if (ker .eq. ok) then
      open ( unit = juer, file = logfil )
!
!        Initialize error routine
!
      call error (juer,' ',0,0)
!
!        Read model info into memory (pools) and determine
!        value for coding.
!
      call sogetm ( coding, juer, ker )
   endif
!
!     Declare working arrays into memory pools
!
   if (ker .eq. ok) then
      call sodecl ( appl, lhisgp , juer, ker )
   endif
!
   if (ker .eq. ok) then
!
!        Sort names in memory pools
!
      if (.not. ldebug) then
         call srtmem
      endif
!
!        Auxilliary output
!
      call soaux (juresi, jufrou, juresd, justrd, jusold )
!
!        Read time information for simulation modules
!
      call sosrun ( appl   ,lauto  ,&
      &lflow  ,lkalm  ,lsalt  ,lsedt  ,lmorp ,&
      &lgrad  ,lwqin  ,ldlwq  ,laux   ,&
!                      mozart parameter
      &lmoza  ,lgrwt  ,&
      &lrivr  ,lestu  ,&
      &itim   ,nstep  ,&
      &dtf    ,steady ,lwstat ,&
      &itp    ,ifp    ,ipc    ,ipcs   ,&
      &wqagst ,restrt ,newres ,&
      &idtm&
      &)

!
!        If write status info = true: open file
!
      if (lwstat) then
         open ( unit = lustat, file = statfl )
      endif
!
!        If morphology module active:
!        Read info from traject definition file
!
      if (lmorp) call motrin ( filnam )
!
      if (ldebug) then
         call debmem(1,juer,0,' ')
         call debmem(2,juer,0,'SOBEK: Before soinit')
         call dmpmem(juer)
      endif
!
      call WRLOGO (0 ,nstep  , 1 , 1, juscr, 1)
!
!        Initialise the different modules in the application
!
      call soinit ( filnam ,itim   ,itp   ,dtf   ,lauto ,lflow ,&
      &lgrwt  ,&
      &lkalm  ,lsalt  ,lsedt ,lmorp ,lgrad ,lwqin ,&
      &lrest  ,newres ,sbkrel,coding,juer  ,juresi,&
      &jufrou ,juresd ,justrd,ker   ,inocon,jusold,&
      &lfrou  ,itstat ,lhisgp,frobuf,jugraut,jugralg)
!
      if (ldebug) then
         call debmem(1,juer,0,' ')
         call debmem(2,juer,0,'SOBEK: Na soinit')
      endif
!
      if (ker .ne. fatal) then
         ker = ok
!
!           Start the simulation run
!
#if defined (USE_MSWINDOWS)

         call CPU_TIME ( strcpu )
#endif
      endif
   endif
end
