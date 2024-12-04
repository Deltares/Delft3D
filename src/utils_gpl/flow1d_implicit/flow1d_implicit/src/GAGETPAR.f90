subroutine GAGetParAnalys (flwrun ,hyrtim ,nhytim ,saltim ,&
&nsatim ,juer   ,ker    )
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
! Module:             GAGetParAnalys (GET PARAMETERS and ANALYSe)
!
! Module description: Get the parameters from Sobek arrays for the
!                     tidal analyses of the flow and salt file. Then
!                     call the analyse module
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 flwrun            I  Times
!  2 hyrtim(nhytim)    I  Parameter list for HIST block with hydrodyna-
!                         mic results:
!                         (1)      = Number of places
!                         (2)      = Report grid point 1
!                         (3)      = Report grid point 2
!                         (i)      = Report grid point n
!                         (i+1)    = Report begin time
!                         (i+2)    = Report end time
!                         (i+3)    = Report time step
!                         (i+5)    = Report parameter code 1
!                         (i+6)    = Report parameter sub code 1
!                         (nhytim) = Report parameter n sub code
!  3 nhytim            I  Number of entries in hyrtim.
!  4 saltim(nsatim)    I  Parameter list for salt results:
!                         See hyrtim.
!  5 nsatim            I  Number of entries in saltim.
!  6 juer              I  Unit number of error file.
!  7 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! GetijAnalyse     Getij Analyse
!=======================================================================
!
!     Declaration of Parameters:
!
   include '../include/filsim.i'
!
   integer   nhytim         ,nsatim        ,juer   ,ker
   integer   hyrtim(nhytim) ,saltim(*)
   real      flwrun(20)
!
!     Declaration of local variables:

   integer        nstepsim ,nlcflw   ,nlcsal ,nsk ,yyyy ,mmdd ,hhmm ,&
   &sshh     ,stastep  ,i      ,dattimsim(2)
   real           dtsim    ,estimper ,perstep
   logical        ana
   character(len=256) flownamhis ,saltnamhis ,gaviewnam(4)

!
!     Get start timestep for tidal analysis
   stastep = max(nint(flwrun(17)),1)
!
   ana = .false.
!
!     Flow data available for tidal analysis?
!
   nlcflw = hyrtim(1)
   nsk    = 3
!
   flownamhis = ' '
   if (nhytim .gt. 1+nsk) then
      do i = nlcflw+2+nsk,nhytim,2
         if (hyrtim(i).eq.13    .and.&
         &hyrtim(i+1) .eq. 0) then
            flownamhis = flwhis
            ana = .true.
         endif
      enddo
   endif
!
!     Set start in time frame
   nlcflw         = min(nlcflw+2,nhytim)
   hyrtim(nlcflw) = stastep
!
!     Salt data available for tidal analysis?
!
   nsk       = 3
!     nsatim=0 and so nlcsal=0 means no salt module
   nlcsal    = 0
!
   saltnamhis = ' '
   if (nsatim .gt. 1+nsk) then
      nlcsal = saltim(1)
      do i = nlcsal+2+nsk,nsatim,2
         if (saltim(i).eq.6     .and.&
         &saltim(i+1) .eq. 0) then
            saltnamhis = slthis
            ana = .true.
         endif
      enddo
   endif
!
!     Set start in time frame
   nlcsal         = min(nlcsal+2,nsatim)
   if (nlcsal.gt.0) then
      saltim(nlcsal) = stastep
   else
!        pointer set to 1 to prevent an array bouns error
      nlcsal = 1
   endif
!
   if (ana) then
!
!        simulation time step
      dtsim   = flwrun(6)
!
!        Detemine estimated tidal period
!
      perstep = flwrun(10)
      if (perstep .gt. 1.1) then
         Estimper = perstep*dtsim
      else
         Estimper = (12.*60.+25.)*60.
      endif
!
!        Get number of time steps
      nstepsim = flwrun(5)
!
!        Set date and time in internal sobek format
      yyyy    = flwrun( 1 )
      mmdd    = flwrun( 2 )
      hhmm    = flwrun( 3 )
      sshh    = flwrun( 4 )
!
      dattimsim(1) = yyyy * 10000 + mmdd
      dattimsim(2) = hhmm * 10000 + sshh
!
      gaviewnam(1) = gawlev
      gaviewnam(2) = gadisch
      gaviewnam(3) = gaveloc
      gaviewnam(4) = gaconcen
!
      call GetijAnalyse (flownamhis     ,saltnamhis  ,gaprinam  ,&
      &gaviewnam      ,hyrtim(nlcflw)         ,&
      &saltim(nlcsal) ,dtsim       ,dattimsim ,&
      &nstepsim       ,EstimPer    ,juer      ,&
      &ker            )
   endif
!
end
