subroutine GAinterpoleer (tstart ,tstep ,tstepi ,hisstart,&
&nreeks ,reeks1,juer   ,ker     )
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Getij Analyse Module
!
! Programmer:         J.Kuipers
!                     ontleent aan S. de Goederen
!
! Module:             GAinterpoleer (INTERPOLEER naar reeks voor analyse)
!
! Module description: Interpoleer de tijdreeks naar de gegeven
!                     tijdstap (in seconden). Indien de tijdstap van de
!                     originele reeks klein genoeg is wordt er
!                     gecopieerd.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  tstart            I  start tijd analyse in seconden tov
!                         start berekening
! 2  tstep             I  tijdstap van de gegeven reeks in seconden
!                      O  tijdstap van de geinterpoleerde reeks
! 3  tstepi            I  tijdstap van de geinterpoleerde reeks in
!                         seconden
! 4  hisstart          O  time of first step on his-file w.r.t.
!                         start of simulation
! 5  nreeks            I  aantal punten van de gegeven reeks.
! 6  reeks1            I  Gedeelte uit de reeksenbuffer dat de
!                         onderhavige reeks bevat
! 7  juer              I  Unit number of error file.
! 8  ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! spline          SPLINE functie
! error           Error messages
!=======================================================================
!
!     Declaration of Parameters:
!
   use gadata
   include '../include/errcod.i'
!
   integer   nreeks ,juer  ,ker
   real      tstart ,tstep ,tstepi, hisstart
   real      reeks1(*)
!
!     Declaration of local variables:
!
   integer   i      ,nreeksi, nreeksf ,&
   &ierr   ,newsize
   character(len=10)  artype,siztxt
   character(len=100) txt
!
!     Maak reeksen

!     Vul de invoerreeks

   nreeksf = nreeks
   do i = 1,nreeksf
      tijdenf(i) = tstart + (i-1) * tstep + hisstart
      reeksf(i)  = reeks1(i)
   enddo
!
!     Determine number of points of interpolated series
!
   if (tstep .gt. tstepi) then
      nreeksi = ((nreeksf-1) * tstep) / tstepi + 2
   else
      nreeksi = nreeksf
   endif
!
!     Allocate arrays for series to be analyzed.
!
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
!
!        Allocate scratch arrays.
!
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
!
!        Initialiseer de geinterpoleerde reeks
!
      do i = 1,nreeksi
         tijden(i) = tstart + (i-1) * tstepi + hisstart
         reeks(i)  = 0.
      enddo
!
!        Interpoleren met spline
!
      call spline(tijdenf ,reeksf ,nreeksf ,tijden ,reeks ,nreeksi ,&
      &y2      ,u      )
!
      tstep  = tstepi
   else
!
!        Make copy of original
!
      do i = 1,nreeksi
         tijden(i) = tijdenf(i)
         reeks(i)  = reeksf(i)
      enddo
   endif
   return
!
9000 continue
!     Error allocating array space
   write(siztxt,'(i10)') newsize
   txt   = 'GAINTERPOL @' // siztxt // '@ @' // artype // '@'
   ker   = fatal
   call sre_error (juer, txt, ealloc, ker )
   return
!
9010 continue
!     Error deallocating array space'
   txt   = 'GAINTERPOL @' // artype // '@'
   ker   = fatal
   call sre_error (juer, txt, edeall, ker )
!
end
