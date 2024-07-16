
      real function GAinitHenC (tstart ,tstop, tstep ,EstimPer)
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
c Module:             GAinitHenC (INITialiseer voor H EN C reeksen) 
c
c Module description: Bepaal het tijdstip van de eerste maximum 
c                     in eerste getijperiode + 2:30
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 0  GAinitHenC        O  Tijd 1e maximum in seconden tov 
c                         start berekening
c 1  tstart            I  start tijd analyse in seconden tov 
c                         start berekening
c 2  tstop             I  stop tijd analyse in seconden tov 
c                         start berekening
c 3  tstep             I  tijdstap van de reeks in seconden
c 4  EstimPer          I  geschatte getijperiode in seconden
c-----------------------------------------------------------------------
c Subprogram calls:
c GAExtrem         Getij Analyse, bepaal EXTREMen
c=======================================================================
c
c     Declaration of Parameters:
c
      real     tstart ,tstop  ,tstep ,EstimPer 
c
c     Declaration of local variables:
c 
      real     ptb    ,pte    ,Wmax  ,Tmax  ,Wmin   ,Tmin  
c      
c     Bepaal de periode waarin gezocht gaat worden. Dit is gedurende
c     de eerste getijperiode + 2:30, tenzij de lengte van de tijdreeks
c     dit niet toelaat.
c
      ptb = tstart
      pte = ptb + EstimPer + 9000.
      if (pte + tstep .gt. tstop) pte = tstop - tstep
c
c     Zoek het globaal maximum in de bepaalde periode.
c
      call GAExtrem(ptb ,pte ,tstep ,Wmax ,Tmax ,Wmin ,Tmin)
c
c     Als geen maximum kan worden gevonden, neem dan het begin
c     van de periode
c
      if (Tmax .lt. ptb) Tmax = ptb
c
      GAinitHenC = Tmax
c
      end

      real function GAinitQenV (tstart ,tstop, tstep ,EstimPer)
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
c Module:             GAinitQenV (INITialiseer voor Q EN V reeksen) 
c
c Module description: Bepaal het tijdstip van de eerste ebkentering,
c                     of als deze niet gevonden kan worden, het minimum.
c                     Bij een ebkentering wordt de waarde 0 in positieve
c                     richting doorschreden.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 0  GAinitQenV        O  Tijd 1e ebkentering in seconden tov 
c                         start berekening
c 1  tstart            I  start tijd analyse in seconden tov 
c                         start berekening
c 2  tstop             I  stop tijd analyse in seconden tov 
c                         start berekening
c 3  tstep             I  tijdstap van de reeks in seconden
c 4  EstimPer          I  geschatte getijperiode in seconden
c-----------------------------------------------------------------------
c Subprogram calls:
c GAExtrem         Getij Analyse, bepaal EXTREMen
c GANulpunten      Getij Analyse, zoek NULPUNTEN
c=======================================================================
c
c     Declaration of Parameters:
c
      real     tstart  ,tstop  ,tstep  ,EstimPer
c
c     Declaration of local variables:
c
      real     ptb     ,pte    ,Wmax   ,Tmax  ,Wmin   ,
     +         veTijd  ,evTijd  
c
c     Bepaal tijdstip eerste ebkentering in eerste getijperiode + 2:00
c     Bepaal eerst de periode waarin gezocht gaat worden. Dit is 
c     gedurende de eerste getijperiode + 2:00, tenzij de lengte van de
c     tijdreeks dit niet toelaat.
c
      ptb = tstart
      pte = ptb + EstimPer + 7200.
      if (pte + tstep .gt. tstop) pte = tstop - tstep
c
c     Zoek de eerste vloed-ebkentering in de bepaalde periode.
c                                        pos     neg
      call GANulpunten (ptb ,pte ,tstep ,veTijd ,evTijd)
c
c     Als geen ebkentering kan worden gevonden, neem dan het tijdstip
c     van het minimum

      if (veTijd .lt. ptb) then
c                                                          Tmin
         call GAExtrem (ptb ,pte ,tstep ,Wmax ,Tmax ,Wmin ,veTijd)
c
c        Als geen minium kan worden gevonden, neem dan het begin
c        van de periode
c
         if (veTijd .lt. ptb) veTijd = ptb
      endif
c
      GAinitQenV = veTijd

      end
