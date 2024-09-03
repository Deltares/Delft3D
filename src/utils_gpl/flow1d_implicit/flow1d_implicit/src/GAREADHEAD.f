      subroutine GAreadheader (lunhis  ,filnamhis ,nstepsim ,dtsim   ,
     +                         outframe,dattimsim ,nreeks   ,nrinbuf ,
     +                         tstart  ,tstop     ,tstep    ,hisstart,
     +                         idattim ,nvar      ,nloc     ,juer    ,
     +                         ker     )
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Getij Analyse Module
c
c Programmer:         J.Kuipers
c
c Module:             GAReadHeader (READ HEADER of his-file)
c
c Module description: The header of the his-file will read.
c                     The analysis time frame will be termined.
c                     Allocation of scratch arrays.
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  lunhis            I  unit number of his-file with time series 
c 2  filnamhis         I  file name of his-file with time series  
c 3  nstepsim          I  number of simulation time steps 
c 4  dtsim             IO simulation time step in seconds (flow module)
c 5  outframe          I  time frime for output to his-file
c                         1: begin step number
c                         2: end step number
c                         3: step interval
c 6  dattimsim(2)      IO start date and time of simulation 
c                         (Sobeksim format) 
c 7  nreeks            O  number of steps of one time serie
c 8  nrinbuf           O  number of time serie in buffer to be processed
c 9  tstart            O  start time analysis in seconds w.r.t 
c                         start of simulation 
c10  tstop             O  stop time analysis in seconds w.r.t 
c                         start of simulation 
c11  tstep             O  time step of series in seconds
c12  hisstart          O  time of first step on his-file w.r.t.
c                         start of simulation 
c13  idattim(6)        IO start date and time of simulation (his-format)
c14  nvar              O  number of variables on file
c15  nloc              O  number of locations on file
c16  juer              I  Unit number of error file.
c17  ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c parsdt           Parse Date and split up   
c dattimdif        calculate difference in date
c error           Error messages
c=======================================================================
c
c     Declaration of Parameters:
c
      use       gadata
      include '../include/errcod.i'
c
      integer   lunhis  ,nstepsim  ,nreeks   ,nrinbuf ,juer  ,ker
      integer   outframe(3)        ,dattimsim(2)      ,
     +          idattim(6)
      real      dtsim   ,tstart    ,tstop    ,tstep   ,hisstart
      character(len=*)      filnamhis
c
c     Declaration of local variables:
c 
      integer   nvar   ,nloc  ,iscu  ,i     ,istart ,istop ,iostat,
     +          timind ,time  ,timep ,d
      integer   idattimfil(6)
      integer   ierr   ,newsize
      character(len=10)  artype,siztxt
      character(len=100) txt
      double precision         diff
      character dattim*40
c      
      double precision         dattimdiff
      external                 dattimdiff
c
      integer, parameter :: hbegin=1, hend=2, hfreq=3
      integer            :: lrenbuf = 1500
c     integer            :: lrenbuf = 20000
c
c     Open his file
c
      open(lunhis , file = filnamhis , form = 'binary')      
c
      read   (lunhis)  (dattim,i=1,4)
      read   (lunhis)  nvar,nloc
c
c     The number of variables and locations are known,
c     so allocate memory             
c
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
c
      read   (lunhis) (parnam(i),i=1,nvar)
      read   (lunhis)  (d,locnam(i),i=1,nloc)
c      
      read ( dattim,1000 ) idattimfil,iscu
c
      tstep = 0
      if (nstepsim .lt. 0) then
c
c        All information must be read from file
c
         timind = 0
         do
            read (lunhis,iostat=iostat) time,
     +                  (buffer(i),i=1,nvar*nloc)
c           if end of file stop reading
            if (iostat.ne.0) exit
            timind  = timind + 1 
            if (timind .eq. 2) then
               tstep  = (time-timep)*iscu
            endif 
            timep = time
         enddo
c        
         tstart   = 0.
         tstop    = time*iscu
         nreeks   = timind
         hisstart = 0.
         do i=1,6
            idattim(i) = idattimfil(i)
         enddo
         dattimsim(1) = idattim(1)*10000  +idattim(2)*100  +idattim(3)
         dattimsim(2) = idattim(4)*1000000+idattim(5)*10000+
     +                  idattim(6)*100
c         
      else   
c
c        To be analysed interval, timestep etc. are provided
c        by Sobeksim.
c
c        Calculate difference in time between start of the his file
c        and start of simulation
c
         call parsdt(dattimsim  ,idattim(1) ,idattim(2) ,idattim(3) ,
     +               idattim(4) ,idattim(5) ,idattim(6) )
         diff     = dattimdiff (idattimfil,idattim)
         hisstart = real(diff)
c
         istart   = max(outframe(hbegin),1)
         istop    = min(outframe(hend),nstepsim)
         tstart   = istart * dtsim
         tstop    = istop * dtsim
         tstep    = outframe(hfreq) * dtsim
         nreeks   = (istop-istart)/outframe(hfreq) + 1
      endif   
c
      nrinbuf = lrenbuf / nreeks
      if (nrinbuf .lt. 1) then
         nrinbuf = 1
         lrenbuf = nreeks
      endif
c
c     Declare space for time series as they are
c     read from file.
c
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
c
 9000 continue
c     Error allocating array space
      write(siztxt,'(i10)') newsize
      txt   = 'GA1FILE @' // siztxt // '@ @' // artype // '@'
      ker   = fatal
      call sre_error (juer, txt, ealloc, ker )
      return
c      
 9010 continue
c     Error deallocating array space'
      txt   = 'GA1FILE @' // artype // '@'
      ker   = fatal
      call sre_error (juer, txt, edeall, ker )
c
 1000 format ('T0: ',I4.4,'.',I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,
     +        '  (scu=',i8,'s)')      
      end
