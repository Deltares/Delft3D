
real function GAinitHenC (tstart ,tstop, tstep ,EstimPer)
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
! Module:             GAinitHenC (INITialiseer voor H EN C reeksen)
!
! Module description: Bepaal het tijdstip van de eerste maximum
!                     in eerste getijperiode + 2:30
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 0  GAinitHenC        O  Tijd 1e maximum in seconden tov
!                         start berekening
! 1  tstart            I  start tijd analyse in seconden tov
!                         start berekening
! 2  tstop             I  stop tijd analyse in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
! 4  EstimPer          I  geschatte getijperiode in seconden
!-----------------------------------------------------------------------
! Subprogram calls:
! GAExtrem         Getij Analyse, bepaal EXTREMen
!=======================================================================
!
!     Declaration of Parameters:
!
   real     tstart ,tstop  ,tstep ,EstimPer
!
!     Declaration of local variables:
!
   real     ptb    ,pte    ,Wmax  ,Tmax  ,Wmin   ,Tmin
!
!     Bepaal de periode waarin gezocht gaat worden. Dit is gedurende
!     de eerste getijperiode + 2:30, tenzij de lengte van de tijdreeks
!     dit niet toelaat.
!
   ptb = tstart
   pte = ptb + EstimPer + 9000.
   if (pte + tstep .gt. tstop) pte = tstop - tstep
!
!     Zoek het globaal maximum in de bepaalde periode.
!
   call GAExtrem(ptb ,pte ,tstep ,Wmax ,Tmax ,Wmin ,Tmin)
!
!     Als geen maximum kan worden gevonden, neem dan het begin
!     van de periode
!
   if (Tmax .lt. ptb) Tmax = ptb
!
   GAinitHenC = Tmax
!
end

real function GAinitQenV (tstart ,tstop, tstep ,EstimPer)
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
! Module:             GAinitQenV (INITialiseer voor Q EN V reeksen)
!
! Module description: Bepaal het tijdstip van de eerste ebkentering,
!                     of als deze niet gevonden kan worden, het minimum.
!                     Bij een ebkentering wordt de waarde 0 in positieve
!                     richting doorschreden.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 0  GAinitQenV        O  Tijd 1e ebkentering in seconden tov
!                         start berekening
! 1  tstart            I  start tijd analyse in seconden tov
!                         start berekening
! 2  tstop             I  stop tijd analyse in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
! 4  EstimPer          I  geschatte getijperiode in seconden
!-----------------------------------------------------------------------
! Subprogram calls:
! GAExtrem         Getij Analyse, bepaal EXTREMen
! GANulpunten      Getij Analyse, zoek NULPUNTEN
!=======================================================================
!
!     Declaration of Parameters:
!
   real     tstart  ,tstop  ,tstep  ,EstimPer
!
!     Declaration of local variables:
!
   real     ptb     ,pte    ,Wmax   ,Tmax  ,Wmin   ,&
   &veTijd  ,evTijd
!
!     Bepaal tijdstip eerste ebkentering in eerste getijperiode + 2:00
!     Bepaal eerst de periode waarin gezocht gaat worden. Dit is
!     gedurende de eerste getijperiode + 2:00, tenzij de lengte van de
!     tijdreeks dit niet toelaat.
!
   ptb = tstart
   pte = ptb + EstimPer + 7200.
   if (pte + tstep .gt. tstop) pte = tstop - tstep
!
!     Zoek de eerste vloed-ebkentering in de bepaalde periode.
!                                        pos     neg
   call GANulpunten (ptb ,pte ,tstep ,veTijd ,evTijd)
!
!     Als geen ebkentering kan worden gevonden, neem dan het tijdstip
!     van het minimum

   if (veTijd .lt. ptb) then
!                                                          Tmin
      call GAExtrem (ptb ,pte ,tstep ,Wmax ,Tmax ,Wmin ,veTijd)
!
!        Als geen minium kan worden gevonden, neem dan het begin
!        van de periode
!
      if (veTijd .lt. ptb) veTijd = ptb
   endif
!
   GAinitQenV = veTijd

end
