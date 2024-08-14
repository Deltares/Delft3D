      subroutine GAinterpoleer (tstart ,tstep ,tstepi ,hisstart,
     +                          nreeks ,reeks1,juer   ,ker     )
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Getij Analyse Module
c
c Programmer:         J.Kuipers
c                     ontleent aan S. de Goederen 
c
c Module:             GAinterpoleer (INTERPOLEER naar reeks voor analyse) 
c
c Module description: Interpoleer de tijdreeks naar de gegeven 
c                     tijdstap (in seconden). Indien de tijdstap van de 
c                     originele reeks klein genoeg is wordt er 
c                     gecopieerd.
c                     
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  tstart            I  start tijd analyse in seconden tov 
c                         start berekening
c 2  tstep             I  tijdstap van de gegeven reeks in seconden
c                      O  tijdstap van de geinterpoleerde reeks
c 3  tstepi            I  tijdstap van de geinterpoleerde reeks in 
c                         seconden
c 4  hisstart          O  time of first step on his-file w.r.t.
c                         start of simulation 
c 5  nreeks            I  aantal punten van de gegeven reeks.
c 6  reeks1            I  Gedeelte uit de reeksenbuffer dat de 
c                         onderhavige reeks bevat
c 7  juer              I  Unit number of error file.
c 8  ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c spline          SPLINE functie
c error           Error messages
c=======================================================================
c
c     Declaration of Parameters:
c
      use gadata
      include '../include/errcod.i'
c
      integer   nreeks ,juer  ,ker
      real      tstart ,tstep ,tstepi, hisstart  
      real      reeks1(*)
c
c     Declaration of local variables:
c 
      integer   i      ,nreeksi, nreeksf , 
     +          ierr   ,newsize
      character*10      artype,siztxt
      character*100     txt
c   
c     Maak reeksen

c     Vul de invoerreeks

      nreeksf = nreeks
      do i = 1,nreeksf
         tijdenf(i) = tstart + (i-1) * tstep + hisstart
         reeksf(i)  = reeks1(i)
      enddo
c
c     Determine number of points of interpolated series
c
      if (tstep .gt. tstepi) then
         nreeksi = ((nreeksf-1) * tstep) / tstepi + 2
      else
         nreeksi = nreeksf
      endif   
c
c     Allocate arrays for series to be analyzed.
c
      artype  = 'tijden'
      newsize = nreeksi
      if (allocated (tijden)) then 
         if (size(tijden).lt.nreeksi) then
            deallocate(tijden, stat=ierr)
            if ( ierr .gt. 0 ) goto 9010
            allocate(tijden(nreeksi), stat=ierr)
            if ( ierr .gt. 0 ) goto 9000
            artype  = 'reeks'
            deallocate(reeks, stat=ierr)
            if ( ierr .gt. 0 ) goto 9010
            allocate(reeks(nreeksi), stat=ierr)
            if ( ierr .gt. 0 ) goto 9000
         endif   
      else
         allocate(tijden(nreeksi), stat=ierr)
         if ( ierr .gt. 0 ) goto 9000
         artype  = 'reeks'
         allocate(reeks(nreeksi), stat=ierr)
         if ( ierr .gt. 0 ) goto 9000
      endif      
      
      if (tstep .gt. tstepi) then
c
c        Allocate scratch arrays.
c
         artype  = 'u'
         newsize = nreeksi
         if (allocated (u)) then 
            if (size(u).lt.nreeksi) then
               deallocate(u, stat=ierr)
               if ( ierr .gt. 0 ) goto 9010
               allocate(u(nreeksi), stat=ierr)
               if ( ierr .gt. 0 ) goto 9000
               artype  = 'y2'
               deallocate(y2, stat=ierr)
               if ( ierr .gt. 0 ) goto 9010
               allocate(y2(nreeksi), stat=ierr) 
               if ( ierr .gt. 0 ) goto 9000
            endif   
         else
            allocate(u(nreeksi), stat=ierr)
            if ( ierr .gt. 0 ) goto 9000
            artype  = 'y2'
            allocate(y2(nreeksi), stat=ierr)
            if ( ierr .gt. 0 ) goto 9000
         endif
c         
c        Initialiseer de geinterpoleerde reeks
c
         do i = 1,nreeksi
            tijden(i) = tstart + (i-1) * tstepi + hisstart
            reeks(i)  = 0.
         enddo
c
c        Interpoleren met spline
c
         call spline(tijdenf ,reeksf ,nreeksf ,tijden ,reeks ,nreeksi ,
     +               y2      ,u      )
c   
         tstep  = tstepi
      else   
c
c        Make copy of original
c
         do i = 1,nreeksi
            tijden(i) = tijdenf(i) 
            reeks(i)  = reeksf(i)
         enddo
      endif
      return
c
 9000 continue
c     Error allocating array space
      write(siztxt,'(i10)') newsize
      txt   = 'GAINTERPOL @' // siztxt // '@ @' // artype // '@'
      ker   = fatal
      call sre_error (juer, txt, ealloc, ker )
      return
c      
 9010 continue
c     Error deallocating array space'
      txt   = 'GAINTERPOL @' // artype // '@'
      ker   = fatal
      call sre_error (juer, txt, edeall, ker )
c
      end
