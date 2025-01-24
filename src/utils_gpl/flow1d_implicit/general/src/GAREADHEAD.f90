subroutine GAreadheader (lunhis  ,filnamhis ,nstepsim ,dtsim   ,&
&outframe,dattimsim ,nreeks   ,nrinbuf ,&
&tstart  ,tstop     ,tstep    ,hisstart,&
&idattim ,nvar      ,nloc     ,juer    ,&
&ker     )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Getij Analyse Module
!
! Programmer:         J.Kuipers
!
! Module:             GAReadHeader (READ HEADER of his-file)
!
! Module description: The header of the his-file will read.
!                     The analysis time frame will be termined.
!                     Allocation of scratch arrays.
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  lunhis            I  unit number of his-file with time series
! 2  filnamhis         I  file name of his-file with time series
! 3  nstepsim          I  number of simulation time steps
! 4  dtsim             IO simulation time step in seconds (flow module)
! 5  outframe          I  time frime for output to his-file
!                         1: begin step number
!                         2: end step number
!                         3: step interval
! 6  dattimsim(2)      IO start date and time of simulation
!                         (Sobeksim format)
! 7  nreeks            O  number of steps of one time serie
! 8  nrinbuf           O  number of time serie in buffer to be processed
! 9  tstart            O  start time analysis in seconds w.r.t
!                         start of simulation
!10  tstop             O  stop time analysis in seconds w.r.t
!                         start of simulation
!11  tstep             O  time step of series in seconds
!12  hisstart          O  time of first step on his-file w.r.t.
!                         start of simulation
!13  idattim(6)        IO start date and time of simulation (his-format)
!14  nvar              O  number of variables on file
!15  nloc              O  number of locations on file
!16  juer              I  Unit number of error file.
!17  ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! parsdt           Parse Date and split up
! dattimdif        calculate difference in date
! error           Error messages
!=======================================================================
!
!     Declaration of Parameters:
!
   use       gadata
   include '..\include\errcod.i'
!
   integer   lunhis  ,nstepsim  ,nreeks   ,nrinbuf ,juer  ,ker
   integer   outframe(3)        ,dattimsim(2)      ,&
   &idattim(6)
   real      dtsim   ,tstart    ,tstop    ,tstep   ,hisstart
   character*(*)      filnamhis
!
!     Declaration of local variables:
!
   integer   nvar   ,nloc  ,iscu  ,i     ,istart ,istop ,iostat,&
   &timind ,time  ,timep ,d
   integer   idattimfil(6)
   integer   ierr   ,newsize
   character*10      artype,siztxt
   character*100     txt
   double precision         diff
   character dattim*40
!
   double precision         dattimdiff
   external                 dattimdiff
!
   integer, parameter :: hbegin=1, hend=2, hfreq=3
   integer            :: lrenbuf = 1500
!     integer            :: lrenbuf = 20000
!
!     Open his file
!
   open(lunhis , file = filnamhis , form = 'binary')
!
   read   (lunhis)  (dattim,i=1,4)
   read   (lunhis)  nvar,nloc
!
!     The number of variables and locations are known,
!     so allocate memory
!
   artype  = 'parnam'
   newsize = nvar
   if (allocated (parnam)) then
      if (size(parnam).lt.nvar) then
         deallocate(parnam, stat=ierr)
         if ( ierr .gt. 0 ) goto 9010
         allocate(parnam(nvar), stat=ierr)
         if ( ierr .gt. 0 ) goto 9000
      endif
   else
      allocate(parnam(nvar), stat=ierr)
      if ( ierr .gt. 0 ) goto 9000
   endif
   artype  = 'locnam'
   newsize = nloc
   if (allocated (locnam)) then
      if (size(locnam).lt.nloc) then
         deallocate(locnam, stat=ierr)
         if ( ierr .gt. 0 ) goto 9010
         allocate(locnam(nloc), stat=ierr)
         if ( ierr .gt. 0 ) goto 9000
      endif
   else
      allocate(locnam(nloc), stat=ierr)
      if ( ierr .gt. 0 ) goto 9000
   endif
   artype  = 'buffer'
   newsize = nvar*nloc
   if (allocated (buffer)) then
      if (size(buffer).lt.nvar*nloc) then
         deallocate(buffer, stat=ierr)
         if ( ierr .gt. 0 ) goto 9010
         allocate(buffer(nvar*nloc), stat=ierr)
         if ( ierr .gt. 0 ) goto 9000
      endif
   else
      allocate(buffer(nvar*nloc), stat=ierr)
      if ( ierr .gt. 0 ) goto 9000
   endif
!
   read   (lunhis) (parnam(i),i=1,nvar)
   read   (lunhis)  (d,locnam(i),i=1,nloc)
!
   read ( dattim,1000 ) idattimfil,iscu
!
   tstep = 0
   if (nstepsim .lt. 0) then
!
!        All information must be read from file
!
      timind = 0
      do
         read (lunhis,iostat=iostat) time,&
         &(buffer(i),i=1,nvar*nloc)
!           if end of file stop reading
         if (iostat.ne.0) exit
         timind  = timind + 1
         if (timind .eq. 2) then
            tstep  = (time-timep)*iscu
         endif
         timep = time
      enddo
!
      tstart   = 0.
      tstop    = time*iscu
      nreeks   = timind
      hisstart = 0.
      do i=1,6
         idattim(i) = idattimfil(i)
      enddo
      dattimsim(1) = idattim(1)*10000  +idattim(2)*100  +idattim(3)
      dattimsim(2) = idattim(4)*1000000+idattim(5)*10000+&
      &idattim(6)*100
!
   else
!
!        To be analysed interval, timestep etc. are provided
!        by Sobeksim.
!
!        Calculate difference in time between start of the his file
!        and start of simulation
!
      call parsdt(dattimsim  ,idattim(1) ,idattim(2) ,idattim(3) ,&
      &idattim(4) ,idattim(5) ,idattim(6) )
      diff     = dattimdiff (idattimfil,idattim)
      hisstart = real(diff)
!
      istart   = max(outframe(hbegin),1)
      istop    = min(outframe(hend),nstepsim)
      tstart   = istart * dtsim
      tstop    = istop * dtsim
      tstep    = outframe(hfreq) * dtsim
      nreeks   = (istop-istart)/outframe(hfreq) + 1
   endif
!
   nrinbuf = lrenbuf / nreeks
   if (nrinbuf .lt. 1) then
      nrinbuf = 1
      lrenbuf = nreeks
   endif
!
!     Declare space for time series as they are
!     read from file.
!
   artype  = 'reeksen'
   newsize = lrenbuf
   if (allocated (reeksen)) then
      if (size(reeksen).lt.lrenbuf) then
         deallocate(reeksen, stat=ierr)
         if ( ierr .gt. 0 ) goto 9010
         allocate(reeksen(lrenbuf), stat=ierr)
         if ( ierr .gt. 0 ) goto 9000
      endif
   else
      allocate(reeksen(lrenbuf), stat=ierr)
      if ( ierr .gt. 0 ) goto 9000
   endif
   artype  = 'tijdenf'
   newsize = nreeks
   if (allocated (tijdenf)) then
      if (size(tijdenf).lt.nreeks) then
         deallocate(tijdenf, stat=ierr)
         if ( ierr .gt. 0 ) goto 9010
         allocate(tijdenf(nreeks), stat=ierr)
         if ( ierr .gt. 0 ) goto 9000
      endif
   else
      allocate(tijdenf(nreeks), stat=ierr)
      if ( ierr .gt. 0 ) goto 9000
   endif
   artype  = 'reeksf'
   if (allocated (reeksf)) then
      if (size(reeksf).lt.nreeks) then
         deallocate(reeksf, stat=ierr)
         if ( ierr .gt. 0 ) goto 9010
         allocate(reeksf(nreeks), stat=ierr)
         if ( ierr .gt. 0 ) goto 9000
      endif
   else
      allocate(reeksf(nreeks), stat=ierr)
      if ( ierr .gt. 0 ) goto 9000
   endif
   return
!
9000 continue
!     Error allocating array space
   write(siztxt,'(i10)') newsize
   txt   = 'GA1FILE @' // siztxt // '@ @' // artype // '@'
   ker   = fatal
   call error (juer, txt, ealloc, ker )
   return
!
9010 continue
!     Error deallocating array space'
   txt   = 'GA1FILE @' // artype // '@'
   ker   = fatal
   call error (juer, txt, edeall, ker )
!
1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,&
   &'  (scu=',i8,'s)')
end
