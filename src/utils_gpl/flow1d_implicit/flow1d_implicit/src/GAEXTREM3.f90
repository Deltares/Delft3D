subroutine GAextrem3 (ptb    ,pte    ,tstep  ,Wmax  ,Tmax ,&
&Wmin   ,Tmin   )
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
! 1  ptb               I  begin tijd (getij)periode in seconden tov
!                         start berekening
! 2  pte               I  eind tijd (getij)periode in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
! 4  Wmax              O  maximum waarde in getijperiode
! 5  Tmax              O  Tijd van maximum in seconden tov
!                         start berekening
! 6  Wmin              O  minimum waarde in getijperiode
! 7  Tmin              O  Tijd van minimum in seconden tov
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
   real           ptb    ,pte    ,tstep  ,Wmax  ,Tmax  ,&
   &Wmin   ,Tmin
!
!     Declaration of local variables:
!
   integer        trend   ,i       ,ib     ,ie
   integer, parameter :: DALEND=-1 ,STIJGEND=1 ,GEENTREND=0
   real           aWaarde ,nWaarde ,Tmax1  ,Tmin1  ,Tmax2 ,&
   &Tmin2   ,Wmax1   ,Wmin1  ,Wmax2  ,Wmin2
!
   integer        GAIndex
   external       GAIndex
!
!     Initialiseer maximum en minium
!
   Wmax = -1.0e+30
   Wmin =  1.0e+30
!
!     Initialiseer maxtijd en mintijd op begin periode - 1 seconde

   Tmax = ptb - 1.
   Tmin = ptb - 1.
!
!     Zoek het globale maximum en minimum in de gegeven periode.
!     Een locaal maximum is een moment dat de trend omslaat van STIJGEND
!     naar DALEND. Het globale maximum is het grootste locale maximum.
!     Een locaal minimum is een moment dat de trend omslaat van DALEND
!     naar STIJGEND. Het globale minimum is het kleinste locale minimum.
!     Indien er geen maximum en minimum wordt gevonden, dan wordt de
!     laagste waarde uit de reeks als minimum gebruikt en de hoogste
!     waarde uit de reeks als maximum.

   Tmax1 = Tmax
   Tmin1 = Tmin
   Tmax2 = Tmax
   Tmin2 = Tmin
   Wmax1 = Wmax
   Wmin1 = Wmin
   Wmax2 = Wmax
   Wmin2 = Wmin

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
!
      if (nWaarde .lt. aWaarde) then
!             nieuwe trend is DALEND
         if (trend .eq. STIJGEND) then
!                 locaal maximum gevonden, controleer of dit in
!                 aanmerking komt als globaal maximum
            if (aWaarde .gt. Wmax1) then
               Wmax1 = aWaarde
               Tmax1 = tijden(i)
            endif
         endif
         trend = DALEND
      else if (nWaarde .gt. aWaarde) then
!            nieuwe trend is STIJGEND
         if (trend .eq. DALEND) then
!                 locaal minimum gevonden, controleer of dit in
!                 aanmerking komt als globaal minimum
            if (aWaarde .lt. Wmin1) then
               Wmin1 = aWaarde
               Tmin1 = tijden(i)
            endif
         endif
         trend = STIJGEND
      endif
!
!         als t[tn] = t[ta] handhaaf de huidige trend
!
      if (aWaarde .gt. Wmax2) then
         Wmax2 = aWaarde
         Tmax2 = tijden(i)
      endif
      if (aWaarde .lt. Wmin2) then
         Wmin2 = aWaarde
         Tmin2 = tijden(i)
      endif
   enddo
!
!     Bepaal de te gebruiken extremen
!
   if (Tmax1 .ge. ptb) then
      Wmax = Wmax1
      Tmax = Tmax1
   else
      Wmax = Wmax2
      Tmax = Tmax2
   endif
   if (Tmin1 .ge. ptb) then
      Wmin = Wmin1
      Tmin = Tmin1
   else
      Wmin = Wmin2
      Tmin = Tmin2
   endif
!
end
