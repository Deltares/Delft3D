      subroutine GetijAnalyse1File(lunhis    ,filnamhis ,outframe ,
     +                             dtsim     ,dattimsim ,nstepsim ,
     +                             EstimPer  ,ind       ,juer     ,
     +                             ker       )
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
c Module:             GetijAnalyse1File (GETIJANALYSE van 1 File)
c
c Module description: Tidal analyses of 1 file. The results are written
c                     to array GARESULT.
c                     
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  lunhis            I  unit number of his-file with time series 
c 2  filnamhis         I  file name of his-file with time series  
c 3  outframe          I  time frime for output to his-file
c                         1: begin step number
c                         2: end step number
c                         3: step interval
c 4  dtsim             I  simulation time step in seconds (flow module)
c 5  dattimsim(2)      I  start date and time of simulation 
c                         (Sobeksim format) 
c 6  nstepsim          I  number of simulation time steps 
c 7  EstimPer          I  estimated tidal period in seconds
c 8  ind               I  Index in array garesult
c 9  juer              I  Unit number of error file.
c10  ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c GAreadheader     Getij Analyse, array GARESULT: request for memory
c GAfillbuffer     Getij Analyse, bepaal INDEX in tijdreeks
c GAinterpoleer    Getij Analyse, INTERPOLEER naar reeks voor analyse
c GAReeksHenC      Getij Analyse, analyseer REEKS H EN C) 
c GAReeksQenV      Getij Analyse, analyseer REEKS Q EN V) 
c=======================================================================
c
c     Declaration of Parameters:
c
      use       gadata
      include '../include/errcod.i'
c      
      integer   lunhis   ,nstepsim ,ind         ,juer        ,ker 
      integer   dattimsim(2)       ,idattim(6)  ,outframe(3)
      real      EstimPer ,dtsim    ,hisstart 
      character(len=*)       filnamhis 
c
c     Declaration of local variables:
c
      integer   ivar   ,nvar  ,nvaran  ,maxtype ,irinbuf    ,nrinbuf,
     +          iloc   ,nloc  ,iloclast,vartype ,ivaranlast ,maxvar ,
     +          nreeks ,itype ,request ,i       ,skip
      integer   varlist(9)
      real      tstart ,tstop ,tstep  ,tstepc
      logical   found
c
      integer, parameter :: maxtypes=6
      integer               lenvartypes(maxtypes)
      real,    parameter :: tstepi=60.     
      character(len=20)     vartypes(maxtypes)
c      
      vartypes(hreeks) = 'Water level'
      vartypes(qreeks) = 'Disch'
      vartypes(vreeks) = 'Velocity'      
      vartypes(creeks) = 'Salt concentration'
      vartypes(sreeks) = 'Salinity'
      vartypes(greeks) = 'Chloride'      
c
      do i=1,maxtypes
         lenvartypes(i) = index(vartypes(i),'  ')-1
      enddo   
c
      call GAreadheader (lunhis  ,filnamhis ,nstepsim ,dtsim   ,
     +                   outframe,dattimsim ,nreeks   ,nrinbuf ,
     +                   tstart  ,tstop     ,tstep    ,hisstart,
     +                   idattim ,nvar      ,nloc     ,juer    ,
     +                   ker     )
      if (ker.eq.fatal) goto 9000
      skip    = nint((tstart-hisstart)/dtsim)
c
c     Make a mask to get the needed time series from file
c
      nvaran  = 0
      maxtype = 0
      maxvar  = 0
      do ivar = 1,nvar
         do vartype = 1,maxtypes
            if (index(parnam(ivar),vartypes(vartype)
     +                            (1:lenvartypes(vartype))).gt.0) then
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
c
c     Analyse chloride concentration if available,
c     if not analyse salinity,
c     if not analyse salt concentration 
c     else analyse from flow file 
c
      if (maxtype.ge.creeks) then
         nvaran     = 1
         varlist(1) = maxvar
      endif   
      do vartype=creeks,greeks
          if (vartype.ne.maxtype) then
            vartypes(vartype) = ' '
         endif   
      enddo
c
      irinbuf    = nrinbuf
      iloclast   = 0
      ivaranlast = 1
      ind        = 0 
c
c     Proces all requested functions
c
      do ivar = 1,nvar
         found = .false.
         do itype = 1,maxtypes
            if (index(parnam(ivar),vartypes(itype)
     +                            (1:lenvartypes(itype))).gt.0) then
               found  = .true.
               vartype = itype
               exit
            endif   
         enddo
c
         if (found) then
            do iloc = 1,nloc
               irinbuf = irinbuf + 1
               if (irinbuf .gt. nrinbuf) then
c
c                 Read as many to be processed functions as fit
c                 in buffer.
c
                  call GAfillbuffer(lunhis  ,iloclast ,ivaranlast ,
     +                              varlist ,nvaran   ,nreeks     ,
     +                              nvar    ,nrinbuf  ,buffer     ,
     +                              reeksen ,skip     ,juer       ,
     +                              ker     )
                  irinbuf = 1
                  if (ker.eq.fatal) exit 
               endif
c
c              TSTEPC is initialy the report timestep on file and
c              after interpolation the timestep of the interpolated 
c              time series.
c
               tstepc = tstep
               call GAinterpoleer  (tstart  ,tstepc ,tstepi , 
     +                              hisstart,nreeks ,
     +                              reeksen(nreeks*(irinbuf-1)+1) ,
     +                              juer   ,ker     )
               if (ker.eq.fatal) exit      
c               
               if (vartype .eq. hreeks .or. vartype .ge. creeks) then
                  call GAReeksHenC (tstart  ,tstop  ,tstepc ,EstimPer ,
     +                              vartype ,ivar   ,iloc   ,ind      ,
     +                              juer    ,ker     )
               else
     +         if (vartype .eq. qreeks .or. vartype .eq. vreeks) then
                  call GAReeksQenV (tstart  ,tstop  ,tstepc ,EstimPer ,
     +                              vartype ,ivar   ,iloc   ,ind      ,
     +                              juer    ,ker     )
               endif
               if (ker.eq.fatal) exit 
            enddo   
         endif
      enddo
      request = ind + 1
      call garesultrq (request  ,juer    , ker     )
      garesult(request) = -1  
c
 9000 continue
c      
      end
