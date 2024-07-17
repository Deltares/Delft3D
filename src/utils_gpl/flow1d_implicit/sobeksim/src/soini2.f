      subroutine soini2(sbkrel, itstat, lfrou , ldebug, juscr , juer, 
     +                  juresi, jufrou, juresd, justrd, jusold, itim,
     +                  nstep , itp,    ifp,    ipc,    ipcs,   wqagst,
     +                  restrt, idtm,   lustat, jugraut,jugralg,inocon,
     +                  frobuf, strcpu, dtf,    lauto,  lflow,  lkalm,
     +                  lsalt,  lsedt,  lmorp,  lgrad,  lwqin,  ldlwq, 
     +                  laux,   lmoza,  lgrwt,  lrivr,  lestu, 
     +                  steady, lwstat, newres, lrest,  filnam, ker)
      implicit none
      include '..\include\filsim.i'
      integer ker, juer, juresi, jufrou, 
     +        juresd, justrd, jusold, itim(2),
     +        nstep, itp, ifp, ipc, ipcs,
     +        wqagst, restrt, idtm, lustat, juscr, sbkrel(2), 
     +        itstat(4), jugraut, jugralg, inocon
      real    frobuf(8)
      real    strcpu

      double precision dtf
      logical ldebug, lhisgp, lauto, lflow, lkalm, 
     +        lsalt, lsedt, lmorp, lgrad, lwqin, ldlwq, laux, lmoza,  
     +        lgrwt, lrivr, lestu, steady, lwstat, newres, lrest,
     +        lfrou

      character*256 filnam
      character*1   coding
      character*120 appl
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
      include '..\include\mempool.i'
c
c     Initialise memory pools
c
      call inimem
c
c     Open error log file
c
      if (ker .eq. ok) then
         open ( unit = juer, file = logfil )
c
c        Initialize error routine
c
         call error (juer,' ',0,0)
c
c        Read model info into memory (pools) and determine
c        value for coding.
c
         call sogetm ( coding, juer, ker )
      endif
c
c     Declare working arrays into memory pools
c
      if (ker .eq. ok) then
         call sodecl ( appl, lhisgp , juer, ker )
      endif
c
      if (ker .eq. ok) then
c
c        Sort names in memory pools
c
         if (.not. ldebug) then
            call srtmem
         endif
c
c        Auxilliary output
c
         call soaux (juresi, jufrou, juresd, justrd, jusold )
c
c        Read time information for simulation modules
c
         call sosrun ( appl   ,lauto  ,
     +                 lflow  ,lkalm  ,lsalt  ,lsedt  ,lmorp ,
     +                 lgrad  ,lwqin  ,ldlwq  ,laux   ,
c                      mozart parameter
     +                 lmoza  ,lgrwt  ,
     +                 lrivr  ,lestu  ,
     +                 itim   ,nstep  ,
     +                 dtf    ,steady ,lwstat ,
     +                 itp    ,ifp    ,ipc    ,ipcs   ,
     +                 wqagst ,restrt ,newres ,
     +                 idtm
     +                )

c
c        If write status info = true: open file
c
         if (lwstat) then
            open ( unit = lustat, file = statfl )
         endif
c
c        If morphology module active:
c        Read info from traject definition file
c
         if (lmorp) call motrin ( filnam )
c
         if (ldebug) then
            call debmem(1,juer,0,' ')
            call debmem(2,juer,0,'SOBEK: Before soinit')
            call dmpmem(juer)
         endif
c 
         call WRLOGO (0 ,nstep  , 1 , 1, juscr, 1)
c
c        Initialise the different modules in the application
c
         call soinit ( filnam ,itim   ,itp   ,dtf   ,lauto ,lflow ,
     +                 lgrwt  ,
     +                 lkalm  ,lsalt  ,lsedt ,lmorp ,lgrad ,lwqin ,
     +                 lrest  ,newres ,sbkrel,coding,juer  ,juresi,
     +                 jufrou ,juresd ,justrd,ker   ,inocon,jusold,
     +                 lfrou  ,itstat ,lhisgp,frobuf,jugraut,jugralg)
c
         if (ldebug) then
            call debmem(1,juer,0,' ')
            call debmem(2,juer,0,'SOBEK: Na soinit')
         endif
c
         if (ker .ne. fatal) then
            ker = ok
c
c           Start the simulation run
c
#if defined (USE_MSWINDOWS)

            call CPU_TIME ( strcpu )
#endif
         endif
      endif
      end
