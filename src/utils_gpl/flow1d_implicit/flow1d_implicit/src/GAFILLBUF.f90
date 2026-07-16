subroutine GAfillbuffer (lunhis  ,iloclast ,ivaranlast,varlist ,&
&nvaran  ,nreeks   ,nvar      ,nrinbuf ,&
&buffer  ,reeksen  ,skip      ,juer    ,&
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
! Module:             GAfillbuffer (FILL BUFFER which series)
!
! Module description: The buffer 'reeksen' will be filled with as
!                     many time series to be analyxed as possible.
!                     So the number of times the file will be read
!                     will be minimal.
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 lunhis            I  unit number of his-file with time series
!  2 iloclast          I  location number of time serie last read and
!                         analyzed in previous buffer
!  3 ivaranlast        I  index of to be analyzed variable number of
!                         which some time series are read last and
!                         analyzed in previous buffer
!  4 varlist           I  contains the variable numbers of variables
!                         with time series that must be analyzed
!  5 nvaran            I  number of variables to be analyzed
!  6 nreeks            I  length of one time serie
!  7 nvar              I  Number of variables on his file
!  8 nrinbuf           I  number of time serie in buffer to be processed
!  9 buffer            -  contains data of one report time step on
!                         his file
! 10 reeksen           I  contains as many time series as possible
! 11 juer              I  Unit number of error file.
! 12 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!=======================================================================
!
   include '../include/errcod.i'
!
!     Declaration of Parameters:
!
   integer   lunhis  ,iloclast ,ivaranlast ,nvaran ,skip  ,&
   &nrinbuf ,nreeks   ,nvar       ,juer   ,ker   ,time
   integer   varlist (nvaran)
   real      buffer(nvar,*)    ,reeksen(nreeks,*)
!
!     Declaration of local variables:
!
   integer   ivar   ,nvarf  ,ivaran  ,iloc   ,nloc  ,irinbuf ,i    ,&
   &idum   ,iostat ,timind ,filind
   logical   moretoanlz
   character c20dum*20      ,c40dum*40
!
!     Rewinf file and skip header
!
   rewind (lunhis)
   read   (lunhis) (c40dum,i=1,4)
   read   (lunhis) nvarf,nloc
   read   (lunhis) (c20dum,i=1,nvar)
   read   (lunhis) (idum,c20dum,i=1,nloc)
!
!     Read file and fill buffer 'reeksen'
!
   timind = 1
   filind = 0

   do
      read (lunhis,iostat=iostat) time,&
      &((buffer(ivar,iloc),ivar=1,nvar),iloc=1,nloc)
!        if end of file stop reading
      if (iostat.ne.0) exit
!
      if (filind.gt.skip) timind  = timind + 1
      filind     = filind + 1
!
      iloc       = iloclast
      ivaran     = ivaranlast
      ivar       = varlist(ivaran)
      irinbuf    = 1
      moretoanlz = .true.
!
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
!
   if (timind.lt.5) then
      ker = fatal
      call sre_error (juer,'GAFILLBUF to less dat', egales, ker )
   endif
!
   iloclast   = iloc
   ivaranlast = ivaran
   nreeks     = timind
!
end
