subroutine GetijAnalyse(flownamhis ,saltnamhis ,gaprinam  ,&
&gaviewnam  ,flowframe  ,saltframe ,&
&dtsim      ,dattimsim  ,nstepsim  ,&
&EstimPer   ,juer       ,ker       )
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
! Module:             GetijAnalyse
!
! Module description: Tidal analyses of flow and salt file. The results are
!                     written to Ascii and to a file readable by ODSVIEW.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  flownamhis        I  file name of his-file with flow time series
! 2  saltnamhis        I  file name of his-file with salt time series
! 3  gapriname         I  file name containing print report tidal
!                         analyses results.
! 4  gaviewname        I  file names of files containing tidal analyses
!                         results readable for ODSVIEW
! 5  flowframe         I  time frame for flow output to his-file
!                         1: begin step number
!                         2: end step number
!                         3: step interval
! 6  saltframe         I  time frame for salt output to his-file
!                         1: begin step number
!                         2: end step number
!                         3: step interval
! 7  dtsim             I  simulation time step in seconds (flow module)
! 8  dattimsim(2)      I  start date and time of simulation
!                         (Sobeksim format)
! 9  nstepsim          I  number of simulation time steps
!10  EstimPer          I  estimated tidal period in seconds
!11  juer              I  Unit number of error file.
!12  ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! GetijAnalyse1File      Getij Analyse van 1 file
! GAPrintResults         Getij Analyse, Print Results
! GAODSVIEWResults       Getij Analyse, ODSVIEW Results
!=======================================================================
!
!     Declaration of Parameters:
!
   use       gadata
   include '../include/errcod.i'
!
   integer   nstepsim ,juer        ,ker
   integer   dattimsim(2)          ,flowframe(3) ,saltframe(3)
   real      EstimPer ,dtsim
   character(len=256) flownamhis, saltnamhis, gaprinam,&
   &gaviewnam(4)
!
!     Declaration of local variables:
!
   integer   index  ,ir
!
   integer, parameter :: lunhis = 150 ,luanpri = 151
!
   open (luanpri, file = gaprinam)
   write (luanpri,100)
!
   index = 0
   if (flownamhis.ne.' ') then
      call GetijAnalyse1File(lunhis    ,flownamhis ,flowframe ,&
      &dtsim     ,dattimsim  ,nstepsim  ,&
      &EstimPer  ,index      ,juer      ,&
      &ker       )
      if (ker.eq.fatal) goto 1000
      do ir = 1,vreeks
         call GAPrintResults   (luanpri ,dattimsim ,ir)
         call GAODSVIEWResults (gaviewnam ,dattimsim ,ir)
      enddo
   endif
!
   if (saltnamhis.ne.' ') then
      call GetijAnalyse1File(lunhis    ,saltnamhis ,saltframe ,&
      &dtsim     ,dattimsim  ,nstepsim  ,&
      &EstimPer  ,index      ,juer      ,&
      &ker       )
      if (ker.eq.fatal) goto 1000
!
      do ir = creeks,greeks
         call GAPrintResults (luanpri ,dattimsim ,ir)
         call GAODSVIEWResults (gaviewnam ,dattimsim ,ir)
      enddo
   endif
!
100 format (' SOBEK'/' Tidal Analyses'/)

1000 continue
end
