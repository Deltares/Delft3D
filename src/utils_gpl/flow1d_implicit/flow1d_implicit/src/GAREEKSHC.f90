subroutine GAReeksHenC (tstart ,tstop ,tstep ,EstimPer ,vartype,&
&ivar   ,iloc  ,index ,juer     ,ker    )
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
! Module:             GAReeksHenC (analyseer REEKS H EN C)
!
! Module description: Bepaal de getijgrootheden van de soort H
!                     (waterstand) of C (concentratie)
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  tstart            I  start tijd analyse in seconden tov
!                         start berekening
! 2  tstop             I  stop tijd analyse in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
! 4  EstimPer          I  geschatte getijperiode in seconden
! 5  vartype           I  type van de variabele
!                         1 = waterstand    2 = debiet
!                         3 = snelheid      4 = zout conc.
!                         5 = saliniteit    6 = chloride conc.
! 6  ivar              I  variabele nummer op de His-file
! 7  iloc              I  locatie nummer op de His-file
! 8  index             I  Index in array garesult
! 9  juer              I  unit number of error file.
!10  ker               O  error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! GAresultrq       Getij Analyse, array GARESULT: request for memory
! GAindex          Getij Analyse, bepaal INDEX in tijdreeks
! GAinitHenC       Getij Analyse, INITialiseer voor H EN C reeksen
! GAextrem         Getij Analyse, bepaal EXTREMen
! GAextrem2        Getij Analyse, bepaal EXTREMen 2
! GAGemiddelde     Getij Analyse, bepaal GEMIDDELDE
!=======================================================================
!
!     Declaration of Parameters:
!
   use       gadata
   include '../include/errcod.i'
!
   integer   vartype ,ivar   ,iloc  ,index  ,indexp, juer    ,ker
   real      tstart  ,tstop  ,tstep ,EstimPer
!
!     Declaration of local variables:
!
   integer   getijnum   ,request   ,i
   real      tba        ,ptb       ,pte    ,Wmax  ,Tmax  ,Wmin   ,&
   &Tmin       ,teVerwacht,Tmax1
!
   integer   GAIndex
   real      GAinitHenC ,GAGemiddelde
   external  GAIndex    ,GAinitHenC   ,GAGemiddelde
!
!     Bepaal het begin van de eerste getijperiode
!
   tba = GAinitHenC (tstart ,tstop, tstep ,EstimPer)
!
!     Bepaal getijgrootheden uit alle getijperioden
!
   indexp  = index
   request = index + 4
   call garesultrq (request  ,juer    , ker     )
!
   garesult(index+type)   = vartype
   garesult(index+grooth) = ivar
   garesult(index+locat)  = iloc
!
   index    = index + 4
   getijnum = 0
!
   do
!
!        Bepaal of er nog een volledig getij beschikbaar is, d.w.z. dat de
!        tijdreeks na de begintijd nog een getijperiode verderloopt.
!        Is dit niet het geval, dan stoppen.
!
      if (tba + EstimPer .gt. tstop) exit
!
!           Zet het getijnummer
!
      getijnum = getijnum + 1
      request  = index + 12
!
      call garesultrq (request  ,juer    , ker     )
      if (ker.eq.fatal) goto 9000
!
      garesult(index+igetij) = getijnum
!
!           Initialiseer de begintijd van de getijperiode
!
      garesult(index+begin) = tba
!
!           Zoek vanaf 2 uur na de begintijd van het huidige getij gedurende
!           een getijperiode naar het maximum en het minimum.
!
      ptb = tba + 7200.
      pte = ptb + EstimPer
      if (pte + tstep .gt. tstop) pte = tstop - tstep
!
      call GAextrem (ptb    ,pte    ,tstep  ,Wmax  ,Tmax ,Wmin  ,&
      &Tmin   )
!
!           Als geen minimum is gevonden, dan is er sprake van een monotoon
!           dalende of stijgende lijn. Probeer extremen te bepalen via de
!           alternatieve methode.
!
      if (Tmin .lt. ptb) then
         call GAextrem2 (ptb    ,pte    ,tstep  ,Wmax  ,Tmax  ,&
         &Wmin   ,Tmin   ,juer    , ker )
         if (ker.eq.fatal) goto 9000
      endif
!
!           Als einde van de getijperiode wordt het tijdstip van het
!           gevonden maximum gekozen, mits dit tijdstip ten hoogste 2
!           uur voor of 2 uur na het verwachte einde (tb + getijperiode)
!           ligt. Is dit niet het geval dan wordt het verwachte einde
!           van de getijperiode aangehouden.
!           Indien helemaal geen maximum is gevonden, dan wordt als
!           maximum ok het einde van de verwachte getijperiode gekozen.
!
      Tmax1 = Tmax
      teVerwacht = tba + EstimPer
      if (Tmax .le. teVerwacht - 7200. .or.&
      &Tmax .ge. teVerwacht + 7200.) then
         if (Tmax .lt. ptb) then
            Tmax = teVerwacht
            i    = GAIndex(Tmax,tijden(1),tstep)
            Wmax = reeks(i)
         else
            Tmax = teVerwacht
         endif
      endif
!
      garesult(index+mintijd) = Tmin
      garesult(index+maxtijd) = Tmax1
      garesult(index+minw)    = Wmin
      garesult(index+maxw)    = Wmax
!
      pte = Tmax
      garesult(index+eind) = pte
!
!           Bereken het gemiddelde
!
      garesult(index+gemidd) = GAGemiddelde (tba ,pte ,tstep)
!
      garesult(index+duur)   = pte - tba
      garesult(index+slag)   = Wmax - Wmin
      garesult(index+vloed)  = 0.
      garesult(index+eb)     = 0.
!
!           De volgende getijperiode begint bij het einde van de huidige
!
      tba = pte
!
      index = request
   enddo
!
   garesult(indexp+ngetij) = getijnum
!
9000 continue
!
end
