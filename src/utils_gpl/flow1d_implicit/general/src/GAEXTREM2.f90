subroutine GAextrem2 (ptb    ,pte    ,tstep  ,Wmax  ,Tmax  ,&
&Wmin   ,Tmin   ,juer   ,ker   )
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
! Module:             GAextrem2 (bepaal EXTREMen)
!
! Module description: Bepaal 'extremen' in monotoon stijgende/
!                     dalende lijn
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  ptb               I  begin tijd van een getij in seconden tov
!                         start berekening
! 2  pte               I  eind tijd van een getij in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
! 4  Wmax              O  maximum waarde in getijperiode
! 5  Tmax              O  Tijd van maximum in seconden tov
!                         start berekening
! 6  Wmin              O  minimum waarde in getijperiode
! 7  Tmin              O  Tijd van minimum in seconden tov
!                         start berekening
! 8  juer              I  Unit number of error file.
! 9  ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
! GALineFit        Getij Analyse, bepaal LINE FIT
! error            Error messages
!=======================================================================
!
!     Declaration of Parameters:
!
   use gadata
   include '..\include\errcod.i'
!
   integer  juer   ,ker
   real     ptb    ,pte    ,tstep  ,Wmax  ,Tmax  ,&
   &Wmin   ,Tmin
!
!     Declaration of local variables:
!
   integer  i       ,ib      ,ie      ,i0    ,n     ,ierr ,&
   &newsize
   real     aWaarde ,pWaarde ,cWaarde ,cmax  ,cmin  ,a0   ,a1
!
   character*10      artype,siztxt
   character*100     txt
!
   integer  GAIndex
   external GAIndex
!
!     Als geen extremen kunnen worden gevonden dan er sprake van een monotoon
!     dalende of stijgende lijn.
!     In deze gevallen wordt een correctielijn door de reeks gefit.
!     De nulpunten van h - correctielijn worden bepaald. Tussen elke twee
!     nulpunten wordt het moment van maximale absolute afwijking van h-cor
!     bepaald. Als gevonden punt een absoluut max of min is, dan wordt deze
!     als maximum of minimum aangewezen.
!     Na het laatste nulpunt wordt niet meer gezocht.
!     Dat bepalen van de nulpunten is natuurlijk onzinnig. Je kunt net zo
!     goed gelijk de momenten van maximale en minimale afwijking bepalen
!     vanaf begin reeks tot laatste nulpunt.
!
!     Bepaal de gecorrigeerde lijn
!
!     Bereken eerst start index Ib en eind index Ie
!
   ib = GAIndex(ptb,tijden(1),tstep)
   ie = GAIndex(pte,tijden(1),tstep)
!
!     Fit een lijn a1*x+a0 d.m.v. lineaire regressie
!
   call GALineFit (ptb ,pte ,tstep ,a0 ,a1)
!
!     Bepaal de gecorrigeerde lijn

!     Allocate scratch arrays.
!
   artype  = 'coreeks'
   newsize = ie
   if (allocated (coreeks)) then
      if (size(coreeks).lt.ie) then
         deallocate(coreeks, stat=ierr)
         if ( ierr .gt. 0 ) goto 9010
         allocate(coreeks(ie), stat=ierr)
         if ( ierr .gt. 0 ) goto 9000
      endif
   else
      allocate(coreeks(ie), stat=ierr)
      if ( ierr .gt. 0 ) goto 9000
   endif
!
   n = 0
   do i = ib , ie
      coreeks(i) = reeks(i) - (a1 * n + a0)
      n = n + 1
   enddo
!
!     Bepaal het laatste nulpunt van de gecorrigeerde lijn
!
   i0 = ib
   do i = ib + 1 , ie
      aWaarde = coReeks(i)
      pWaarde = coReeks(i-1)
!
      if (aWaarde * pWaarde .le. 0.0) then
!            Nulpunt gevonden
         if (abs(aWaarde) .lt. abs(pWaarde)) then
            i0 = i
         else
            i0 = i-1
         endif
      endif
   enddo
!
!     Bepaal de extremen in de gecorrigeerde lijn tot het laatste nulpunt
!     Initialiseer maximum en minium
!
   cmax = -1.0e+30
   cmin =  1.0e+30

   do i = ib , i0
      cWaarde = coReeks(i)
      if (cWaarde .gt. cmax) then
         cmax = cWaarde
         Tmax = tijden(i)
         Wmax = reeks(i)
      endif
      if (cWaarde .lt. cmin) then
         cmin = cWaarde
         Tmin = tijden(i)
         Wmin = reeks(i)
      endif
   enddo
   return
!
9000 continue
!     Error allocating array space
   write(siztxt,'(i10)') newsize
   txt   = 'GAINTERPOL @' // siztxt // '@ @' // artype // '@'
   ker   = fatal
   call error (juer, txt, ealloc, ker )
   return
!
9010 continue
!     Error deallocating array space'
   txt   = 'GAINTERPOL @' // artype // '@'
   ker   = fatal
   call error (juer, txt, edeall, ker )
!
end
