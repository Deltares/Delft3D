      subroutine GetijAnalyse(flownamhis ,saltnamhis ,gaprinam  ,
     +                        gaviewnam  ,flowframe  ,saltframe ,
     +                        dtsim      ,dattimsim  ,nstepsim  ,
     +                        EstimPer   ,juer       ,ker       )
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
c Module:             GetijAnalyse
c
c Module description: Tidal analyses of flow and salt file. The results are 
c                     written to Ascii and to a file readable by ODSVIEW.
c                     
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  flownamhis        I  file name of his-file with flow time series  
c 2  saltnamhis        I  file name of his-file with salt time series
c 3  gapriname         I  file name containing print report tidal 
c                         analyses results.
c 4  gaviewname        I  file names of files containing tidal analyses 
c                         results readable for ODSVIEW 
c 5  flowframe         I  time frame for flow output to his-file
c                         1: begin step number
c                         2: end step number
c                         3: step interval
c 6  saltframe         I  time frame for salt output to his-file
c                         1: begin step number
c                         2: end step number
c                         3: step interval
c 7  dtsim             I  simulation time step in seconds (flow module)
c 8  dattimsim(2)      I  start date and time of simulation 
c                         (Sobeksim format) 
c 9  nstepsim          I  number of simulation time steps 
c10  EstimPer          I  estimated tidal period in seconds
c11  juer              I  Unit number of error file.
c12  ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c GetijAnalyse1File      Getij Analyse van 1 file
c GAPrintResults         Getij Analyse, Print Results
c GAODSVIEWResults       Getij Analyse, ODSVIEW Results
c=======================================================================
c
c     Declaration of Parameters:
c
      use       gadata
      include '..\include\errcod.i'
c      
      integer   nstepsim ,juer        ,ker 
      integer   dattimsim(2)          ,flowframe(3) ,saltframe(3)
      real      EstimPer ,dtsim    
      character*(256)     flownamhis  ,saltnamhis   ,gaprinam    ,
     +                    gaviewnam(4) 
c
c     Declaration of local variables:
c
      integer   index  ,ir
c
      integer, parameter :: lunhis = 150 ,luanpri = 151 
c
      open (luanpri, file = gaprinam)
      write (luanpri,100)
c
      index = 0
      if (flownamhis.ne.' ') then
         call GetijAnalyse1File(lunhis    ,flownamhis ,flowframe ,
     +                          dtsim     ,dattimsim  ,nstepsim  ,
     +                          EstimPer  ,index      ,juer      ,
     +                          ker       )
         if (ker.eq.fatal) goto 1000
         do ir = 1,vreeks
            call GAPrintResults   (luanpri ,dattimsim ,ir)
            call GAODSVIEWResults (gaviewnam ,dattimsim ,ir)   
         enddo
      endif
c     
      if (saltnamhis.ne.' ') then
         call GetijAnalyse1File(lunhis    ,saltnamhis ,saltframe ,
     +                          dtsim     ,dattimsim  ,nstepsim  ,
     +                          EstimPer  ,index      ,juer      ,
     +                          ker       )           
         if (ker.eq.fatal) goto 1000
c
         do ir = creeks,greeks
            call GAPrintResults (luanpri ,dattimsim ,ir)
            call GAODSVIEWResults (gaviewnam ,dattimsim ,ir)   
         enddo
      endif 
c
  100 format (' SOBEK'/' Tidal Analyses'/)

 1000 continue  
      end
