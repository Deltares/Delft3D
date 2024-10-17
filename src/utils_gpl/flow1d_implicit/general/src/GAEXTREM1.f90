subroutine GAextrem (ptb    ,pte    ,tstep  ,Wmax  ,Tmax ,Wmin  ,&
&Tmin   )
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
! Module:             GAextrem (bepaal EXTREMen)
!
! Module description: Bepaal maximum en minimum binnen gegeven zoek
!                     periode in de reeks. De zoekperiode stopt altijd
!                     minstens 1 tijdstap voor het einde van de reeks.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  ptb               I  begin tijd zoekperiode in seconden tov
!                         start berekening
! 2  pte               I  eind tijd zoekperiode in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
! 4  Wmax              O  maximum waarde in getijperiode
! 5  Tmax              O  tijd van maximum in seconden tov
!                         start berekening
! 6  Wmin              O  minimum waarde in getijperiode
! 7  Tmin              O  tijd van minimum in seconden tov
!                         start berekening
!-----------------------------------------------------------------------
! Subprogram calls:
! GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
!=======================================================================
!
!     Declaration of Parameters:
!
   use gadata
!
   real          ptb     ,pte    ,tstep  ,Wmax  ,Tmax  ,&
   &Wmin    ,Tmin
!
!     Declaration of local variables:
!
   integer       trend   ,i    ,ib  ,ie
   integer, parameter :: DALEND=-1  ,STIJGEND=1 ,GEENTREND=0
   real          aWaarde ,nWaarde
!
   integer       GAIndex
   external      GAIndex
!
!     Initialiseer maximum en minium
!
   Wmax = -1.0e+30
   Wmin =  1.0e+30
!
!     Initialiseer maxtijd en mintijd op begin periode - 1 seconde
!
   Tmax = ptb - 1.
   Tmin = ptb - 1.
!
!     Zoek het globale maximum en minimum in de gegeven periode.
!     Een locaal maximum is een moment dat de trend omslaat van STIJGEND
!     naar DALEND. Het globale maximum is het grootste locale maximum.
!     Een locaal minimum is een moment dat de trend omslaat van DALEND
!     naar STIJGEND. Het globale minimum is het kleinste locale minimum.
!
   trend = GEENTREND
!
!     Bereken start index Ib en eind index Ie
!
   ib = GAIndex(ptb,tijden(1),tstep)
   ie = GAIndex(pte,tijden(1),tstep)
!
   do i = ib,ie
!         Je mag 1 tijdstap voorbij einde periode kijken
      aWaarde = reeks(i)
      nWaarde = reeks(i+1)

      if (nWaarde .lt. aWaarde) then
!             nieuwe trend is DALEND
         if (trend .eq. STIJGEND) then
!                 locaal maximum gevonden, controleer of dit in
!                 aanmerking komt als globaal maximum
            if (aWaarde .gt. Wmax) then
               Wmax = aWaarde
               Tmax = tijden(i)
            endif
         endif
         trend = DALEND
      else if (nWaarde .gt. aWaarde) then
!            nieuwe trend is STIJGEND
         if (trend .eq. DALEND) then
!                 locaal minimum gevonden, controleer of dit in
!                 aanmerking komt als globaal minimum
            if (aWaarde .lt. Wmin) then
               Wmin = aWaarde
               Tmin = tijden(i)
            endif
         endif
         trend = STIJGEND
      endif
!         als t[tn] = t[ta] handhaaf de huidige trend
   enddo
!
end
