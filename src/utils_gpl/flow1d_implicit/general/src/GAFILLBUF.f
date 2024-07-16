      subroutine GAfillbuffer (lunhis  ,iloclast ,ivaranlast,varlist ,
     +                         nvaran  ,nreeks   ,nvar      ,nrinbuf , 
     +                         buffer  ,reeksen  ,skip      ,juer    ,
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
c Module:             GAfillbuffer (FILL BUFFER which series)
c
c Module description: The buffer 'reeksen' will be filled with as
c                     many time series to be analyxed as possible. 
c                     So the number of times the file will be read
c                     will be minimal.
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 lunhis            I  unit number of his-file with time series 
c  2 iloclast          I  location number of time serie last read and
c                         analyzed in previous buffer
c  3 ivaranlast        I  index of to be analyzed variable number of 
c                         which some time series are read last and
c                         analyzed in previous buffer
c  4 varlist           I  contains the variable numbers of variables
c                         with time series that must be analyzed
c  5 nvaran            I  number of variables to be analyzed
c  6 nreeks            I  length of one time serie
c  7 nvar              I  Number of variables on his file 
c  8 nrinbuf           I  number of time serie in buffer to be processed
c  9 buffer            -  contains data of one report time step on
c                         his file
c 10 reeksen           I  contains as many time series as possible 
c 11 juer              I  Unit number of error file.
c 12 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c=======================================================================
c
      include '..\include\errcod.i'
c
c     Declaration of Parameters:
c
      integer   lunhis  ,iloclast ,ivaranlast ,nvaran ,skip  ,  
     +          nrinbuf ,nreeks   ,nvar       ,juer   ,ker   ,time
      integer   varlist (nvaran)
      real      buffer(nvar,*)    ,reeksen(nreeks,*)
c
c     Declaration of local variables:
c 
      integer   ivar   ,nvarf  ,ivaran  ,iloc   ,nloc  ,irinbuf ,i    ,
     +          idum   ,iostat ,timind ,filind 
      logical   moretoanlz 
      character c20dum*20      ,c40dum*40
c
c     Rewinf file and skip header
c
      rewind (lunhis)
      read   (lunhis) (c40dum,i=1,4)
      read   (lunhis) nvarf,nloc
      read   (lunhis) (c20dum,i=1,nvar)
      read   (lunhis) (idum,c20dum,i=1,nloc)
c
c     Read file and fill buffer 'reeksen'
c
      timind = 1
      filind = 0
      
      do
         read (lunhis,iostat=iostat) time,
     +               ((buffer(ivar,iloc),ivar=1,nvar),iloc=1,nloc)
c        if end of file stop reading
         if (iostat.ne.0) exit
c         
         if (filind.gt.skip) timind  = timind + 1
         filind     = filind + 1
c         
         iloc       = iloclast
         ivaran     = ivaranlast
         ivar       = varlist(ivaran)
         irinbuf    = 1
         moretoanlz = .true.
c         
         do while (irinbuf .le. nrinbuf .and. moretoanlz) 
            iloc = iloc + 1
            if (iloc .gt. nloc) then
               iloc = 1
               ivaran = ivaran + 1 
               if (ivaran .gt. nvaran) then
                  moretoanlz = .false.
               else
                  ivar = varlist(ivaran)
               endif
            endif 
            if (moretoanlz) then
               reeksen(timind,irinbuf) = buffer(ivar,iloc)
               irinbuf                 = irinbuf  +  1
            endif  
         enddo
      enddo
c      
      if (timind.lt.5) then
         ker = fatal
         call error (juer,'GAFILLBUF to less dat', egales, ker )         
      endif   
c
      iloclast   = iloc
      ivaranlast = ivaran
      nreeks     = timind
c
      end
