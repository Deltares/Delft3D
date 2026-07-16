subroutine GAReeksQenV (tstart  ,tstop  ,tstep ,EstimPer ,vartype,&
&ivar    ,iloc   ,index ,juer     ,ker    )
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
! Module:             GAReeksQenV (analyseer REEKS Q EN V)
!
! Module description: Bepaal de getijgrootheden van de soort Q
!                     (debiet) of V (snelheid)
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
!                         3 = snelheid     >3 = zout
! 6  ivar              I  variabele nummer op de His-file
! 7  iloc              I  locatie nummer op de His-file
! 8  index             I  Index in array garesult
! 9  juer              I  Unit number of error file.
!10  ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!-----------------------------------------------------------------------
! Subprogram calls:
! GAresultrq       Getij Analyse, array GARESULT: request for memory
! GANulpunten      Getij Analyse, zoek NULPUNTEN
! GAinitQenV       Getij Analyse, INITialiseer voor Q EN V reeksen
! GAextrem3        Getij Analyse, bepaal EXTREMen 3
! GAVloedVolume    Getij Analyse, bepaal VLOEDVOLUME
! GAEbVolume       Getij Analyse, bepaal EBVOLUME
!=======================================================================
!
!     Declaration of Parameters:
!
   use       gadata
   include '../include/errcod.i'
!
   integer   vartype ,ivar   ,iloc  ,index   ,juer    ,ker
   real      tstart  ,tstop  ,tstep ,EstimPer
!
!     Declaration of local variables:
!
   integer   getijnum      ,request,indexp
   real      tba   ,tea    ,ptb    ,pte    ,Wmax   ,Tmax  ,Wmin   ,&
   &Tmin  ,teVerwacht     ,veTijd ,evTijd ,tduur
!
   real      GAinitQenV ,GAVloedVolume ,GAEbVolume
   external  GAinitQenV ,GAVloedVolume ,GAEbVolume
!
!     Bepaal het begin van de eerste getijperiode
!
   tba = GAinitQenV (tstart ,tstop, tstep ,EstimPer)
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
!        Bepaal of er nog een volledig getij beschikbaar is, d.w.z. dat
!        de tijdreeks na de begintijd nog een getijperiode verderloopt.
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
!           Zoek vanaf 3 uur voor tot 2 uur na de verwachte eindtijd
!           van de huidige getijperiode naar de eerste vloed-eb
!           kentering
!
      teVerwacht = tba + EstimPer
      ptb = teVerwacht - 10800.
      if (ptb .lt. tba) ptb = tba
      pte = teVerwacht + 7200.
      if (pte + tstep .gt. tstop) pte = tstop - tstep
!
      call GANulpunten (ptb ,pte ,tstep ,veTijd ,evTijd)
!
!           Als einde van de getijperiode wordt het tijdstip van de
!           gevonden vloed-eb kentering gekozen, mits dit tijdstip ten
!           hoogste 3 uur voor of 2 uur na het verwachte einde (tb +
!           getijperiode) ligt.
!           Is dit niet het geval, dan wordt als verwachte einde het t
!           tijdstip van het minimum aangehouden, mits dit tijdstip ten
!           hoogste 2 uur voor of 2 uur na het verwachte einde ligt.
!           Is ook dit niet het geval dan wordt het einde van de
!           verwachte getijperiode gekozen.
!
      ptb = tba + 1800.

      if (veTijd .gt. teVerwacht - 10800. .and.&
      &veTijd .lt. teVerwacht + 7200.) then
         tea = veTijd
      else
!
!              Bepaal extremen
!
         call GAextrem3 (ptb    ,pte    ,tstep  ,Wmax  ,Tmax ,&
         &Wmin   ,Tmin   )
!
         if (Tmin .gt. teVerwacht - 7200. .and.&
         &Tmin .lt. teVerwacht + 7200.) then
            tea = Tmin
         else
            tea = teVerwacht
         endif
      endif
!
!           Zoek vanaf 0.5 uur na de begintijd van het huidige getij
!           tot het einde van de getijperiode naar het maximum en het
!           minimum.
!
      pte = tea
      call GAextrem3 (ptb    ,pte    ,tstep  ,Wmax  ,Tmax ,&
      &Wmin   ,Tmin   )
!
      garesult(index+mintijd) = Tmin
      garesult(index+maxtijd) = Tmax
      garesult(index+minw)    = Wmin
      garesult(index+maxw)    = Wmax
!
      garesult(index+eind)    = pte
!
!           Bereken de duur van het getij
!
      tduur                = pte - tba
      garesult(index+duur) = tduur
!
!           Bereken vloed- en ebvolume en het gemiddelde
!
      garesult(index+slag)   = 0.
      garesult(index+vloed)  = GAVloedVolume (tba , pte ,tstep)
      garesult(index+eb)     = GAEbVolume (tba , pte ,tstep)
      garesult(index+gemidd) = (garesult(index+vloed) +&
      &garesult(index+eb)) / tduur
!
!           Bereken gemiddelde positieve en negatieve snelheid
!
      if (vartype .eq. vreeks) then
         garesult(index+vloed) = garesult(index+vloed) / tduur
         garesult(index+eb)    = garesult(index+eb) / tduur
      endif
!
!           De volgende getijperiode begint bij het einde van de huidige
!
      tba = pte
!
      index = request
   enddo
!
   garesult(indexp + ngetij) = getijnum
!
9000 continue
!
end
