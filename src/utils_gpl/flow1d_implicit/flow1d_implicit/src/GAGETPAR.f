      subroutine GAGetParAnalys (flwrun ,hyrtim ,nhytim ,saltim ,
     +                           nsatim ,juer   ,ker    )
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
c Module:             GAGetParAnalys (GET PARAMETERS and ANALYSe)
c
c Module description: Get the parameters from Sobek arrays for the
c                     tidal analyses of the flow and salt file. Then
c                     call the analyse module
c                     
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 flwrun            I  Times 
c  2 hyrtim(nhytim)    I  Parameter list for HIST block with hydrodyna-
c                         mic results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report begin time
c                         (i+2)    = Report end time
c                         (i+3)    = Report time step
c                         (i+5)    = Report parameter code 1
c                         (i+6)    = Report parameter sub code 1
c                         (nhytim) = Report parameter n sub code
c  3 nhytim            I  Number of entries in hyrtim.
c  4 saltim(nsatim)    I  Parameter list for salt results:
c                         See hyrtim.
c  5 nsatim            I  Number of entries in saltim.
c  6 juer              I  Unit number of error file.
c  7 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c GetijAnalyse     Getij Analyse
c=======================================================================
c
c     Declaration of Parameters:
c
      include '../include/filsim.i'
c      
      integer   nhytim         ,nsatim        ,juer   ,ker 
      integer   hyrtim(nhytim) ,saltim(*)
      real      flwrun(20) 
c
c     Declaration of local variables:

      integer        nstepsim ,nlcflw   ,nlcsal ,nsk ,yyyy ,mmdd ,hhmm ,
     +               sshh     ,stastep  ,i      ,dattimsim(2)
      real           dtsim    ,estimper ,perstep
      logical        ana
      character(len=256) flownamhis ,saltnamhis ,gaviewnam(4)
      
c  
c     Get start timestep for tidal analysis
      stastep = max(nint(flwrun(17)),1)
c      
      ana = .false.
c
c     Flow data available for tidal analysis? 
c
      nlcflw = hyrtim(1)
      nsk    = 3
c      
      flownamhis = ' '
      if (nhytim .gt. 1+nsk) then
         do i = nlcflw+2+nsk,nhytim,2
            if (hyrtim(i).eq.13    .and.
     +          hyrtim(i+1) .eq. 0) then
               flownamhis = flwhis
               ana = .true.
            endif
         enddo
      endif
c
c     Set start in time frame
      nlcflw         = min(nlcflw+2,nhytim)
      hyrtim(nlcflw) = stastep
c
c     Salt data available for tidal analysis? 
c      
      nsk       = 3
c     nsatim=0 and so nlcsal=0 means no salt module     
      nlcsal    = 0
c
      saltnamhis = ' '
      if (nsatim .gt. 1+nsk) then
         nlcsal = saltim(1)
         do i = nlcsal+2+nsk,nsatim,2
            if (saltim(i).eq.6     .and.
     +          saltim(i+1) .eq. 0) then
               saltnamhis = slthis
               ana = .true.
            endif
         enddo
      endif
c
c     Set start in time frame      
      nlcsal         = min(nlcsal+2,nsatim)
      if (nlcsal.gt.0) then 
         saltim(nlcsal) = stastep
      else
c        pointer set to 1 to prevent an array bouns error      
         nlcsal = 1
      endif   
c
      if (ana) then
c
c        simulation time step
         dtsim   = flwrun(6)
c  
c        Detemine estimated tidal period
c
         perstep = flwrun(10)
         if (perstep .gt. 1.1) then
            Estimper = perstep*dtsim
         else
            Estimper = (12.*60.+25.)*60.
         endif
c
c        Get number of time steps
         nstepsim = flwrun(5)
c
c        Set date and time in internal sobek format
         yyyy    = flwrun( 1 )
         mmdd    = flwrun( 2 )
         hhmm    = flwrun( 3 )
         sshh    = flwrun( 4 )
c                  
         dattimsim(1) = yyyy * 10000 + mmdd
         dattimsim(2) = hhmm * 10000 + sshh
c         
         gaviewnam(1) = gawlev
         gaviewnam(2) = gadisch 
         gaviewnam(3) = gaveloc    
         gaviewnam(4) = gaconcen
c            
         call GetijAnalyse (flownamhis     ,saltnamhis  ,gaprinam  ,
     +                      gaviewnam      ,hyrtim(nlcflw)         ,
     +                      saltim(nlcsal) ,dtsim       ,dattimsim ,
     +                      nstepsim       ,EstimPer    ,juer      ,
     +                      ker            )
      endif
c
      end
