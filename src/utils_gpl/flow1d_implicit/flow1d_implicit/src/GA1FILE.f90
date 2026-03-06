subroutine GetijAnalyse1File(lunhis    ,filnamhis ,outframe ,&
&dtsim     ,dattimsim ,nstepsim ,&
&EstimPer  ,ind       ,juer     ,&
&ker       )
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
! Module:             GetijAnalyse1File (GETIJANALYSE van 1 File)
!
! Module description: Tidal analyses of 1 file. The results are written
!                     to array GARESULT.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  lunhis            I  unit number of his-file with time series
! 2  filnamhis         I  file name of his-file with time series
! 3  outframe          I  time frime for output to his-file
!                         1: begin step number
!                         2: end step number
!                         3: step interval
! 4  dtsim             I  simulation time step in seconds (flow module)
! 5  dattimsim(2)      I  start date and time of simulation
!                         (Sobeksim format)
! 6  nstepsim          I  number of simulation time steps
! 7  EstimPer          I  estimated tidal period in seconds
! 8  ind               I  Index in array garesult
! 9  juer              I  Unit number of error file.
!10  ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! GAreadheader     Getij Analyse, array GARESULT: request for memory
! GAfillbuffer     Getij Analyse, bepaal INDEX in tijdreeks
! GAinterpoleer    Getij Analyse, INTERPOLEER naar reeks voor analyse
! GAReeksHenC      Getij Analyse, analyseer REEKS H EN C)
! GAReeksQenV      Getij Analyse, analyseer REEKS Q EN V)
!=======================================================================
!
!     Declaration of Parameters:
!
   use       gadata
   include '../include/errcod.i'
!
   integer   lunhis   ,nstepsim ,ind         ,juer        ,ker
   integer   dattimsim(2)       ,idattim(6)  ,outframe(3)
   real      EstimPer ,dtsim    ,hisstart
   character(len=*)       filnamhis
!
!     Declaration of local variables:
!
   integer   ivar   ,nvar  ,nvaran  ,maxtype ,irinbuf    ,nrinbuf,&
   &iloc   ,nloc  ,iloclast,vartype ,ivaranlast ,maxvar ,&
   &nreeks ,itype ,request ,i       ,skip
   integer   varlist(9)
   real      tstart ,tstop ,tstep  ,tstepc
   logical   found
!
   integer, parameter :: maxtypes=6
   integer               lenvartypes(maxtypes)
   real,    parameter :: tstepi=60.
   character(len=20)     vartypes(maxtypes)
!
   vartypes(hreeks) = 'Water level'
   vartypes(qreeks) = 'Disch'
   vartypes(vreeks) = 'Velocity'
   vartypes(creeks) = 'Salt concentration'
   vartypes(sreeks) = 'Salinity'
   vartypes(greeks) = 'Chloride'
!
   do i=1,maxtypes
      lenvartypes(i) = index(vartypes(i),'  ')-1
   enddo
!
   call GAreadheader (lunhis  ,filnamhis ,nstepsim ,dtsim   ,&
   &outframe,dattimsim ,nreeks   ,nrinbuf ,&
   &tstart  ,tstop     ,tstep    ,hisstart,&
   &idattim ,nvar      ,nloc     ,juer    ,&
   &ker     )
   if (ker.eq.fatal) goto 9000
   skip    = nint((tstart-hisstart)/dtsim)
!
!     Make a mask to get the needed time series from file
!
   nvaran  = 0
   maxtype = 0
   maxvar  = 0
   do ivar = 1,nvar
      do vartype = 1,maxtypes
         if (index(parnam(ivar),vartypes(vartype)&
         &(1:lenvartypes(vartype))).gt.0) then
            nvaran          = nvaran + 1
            varlist(nvaran) = ivar
            if (vartype.gt.maxtype) then
               maxtype = vartype
               maxvar  = ivar
            endif
            exit
         endif
      enddo
   enddo
!
!     Analyse chloride concentration if available,
!     if not analyse salinity,
!     if not analyse salt concentration
!     else analyse from flow file
!
   if (maxtype.ge.creeks) then
      nvaran     = 1
      varlist(1) = maxvar
   endif
   do vartype=creeks,greeks
      if (vartype.ne.maxtype) then
         vartypes(vartype) = ' '
      endif
   enddo
!
   irinbuf    = nrinbuf
   iloclast   = 0
   ivaranlast = 1
   ind        = 0
!
!     Proces all requested functions
!
   do ivar = 1,nvar
      found = .false.
      do itype = 1,maxtypes
         if (index(parnam(ivar),vartypes(itype)&
         &(1:lenvartypes(itype))).gt.0) then
            found  = .true.
            vartype = itype
            exit
         endif
      enddo
!
      if (found) then
         do iloc = 1,nloc
            irinbuf = irinbuf + 1
            if (irinbuf .gt. nrinbuf) then
!
!                 Read as many to be processed functions as fit
!                 in buffer.
!
               call GAfillbuffer(lunhis  ,iloclast ,ivaranlast ,&
               &varlist ,nvaran   ,nreeks     ,&
               &nvar    ,nrinbuf  ,buffer     ,&
               &reeksen ,skip     ,juer       ,&
               &ker     )
               irinbuf = 1
               if (ker.eq.fatal) exit
            endif
!
!              TSTEPC is initialy the report timestep on file and
!              after interpolation the timestep of the interpolated
!              time series.
!
            tstepc = tstep
            call GAinterpoleer  (tstart  ,tstepc ,tstepi ,&
            &hisstart,nreeks ,&
            &reeksen(nreeks*(irinbuf-1)+1) ,&
            &juer   ,ker     )
            if (ker.eq.fatal) exit
!
            if (vartype .eq. hreeks .or. vartype .ge. creeks) then
               call GAReeksHenC (tstart  ,tstop  ,tstepc ,EstimPer ,&
               &vartype ,ivar   ,iloc   ,ind      ,&
               &juer    ,ker     )
            else&
            &if (vartype .eq. qreeks .or. vartype .eq. vreeks) then
               call GAReeksQenV (tstart  ,tstop  ,tstepc ,EstimPer ,&
               &vartype ,ivar   ,iloc   ,ind      ,&
               &juer    ,ker     )
            endif
            if (ker.eq.fatal) exit
         enddo
      endif
   enddo
   request = ind + 1
   call garesultrq (request  ,juer    , ker     )
   garesult(request) = -1
!
9000 continue
!
end
