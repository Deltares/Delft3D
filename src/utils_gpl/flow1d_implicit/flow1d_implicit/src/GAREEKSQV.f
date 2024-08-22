      subroutine GAReeksQenV (tstart  ,tstop  ,tstep ,EstimPer ,vartype,
     +                        ivar    ,iloc   ,index ,juer     ,ker    )
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
c Module:             GAReeksQenV (analyseer REEKS Q EN V) 
c
c Module description: Bepaal de getijgrootheden van de soort Q 
c                     (debiet) of V (snelheid)
c                     
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  tstart            I  start tijd analyse in seconden tov 
c                         start berekening
c 2  tstop             I  stop tijd analyse in seconden tov 
c                         start berekening
c 3  tstep             I  tijdstap van de reeks in seconden
c 4  EstimPer          I  geschatte getijperiode in seconden
c 5  vartype           I  type van de variabele
c                         1 = waterstand    2 = debiet
c                         3 = snelheid     >3 = zout 
c 6  ivar              I  variabele nummer op de His-file       
c 7  iloc              I  locatie nummer op de His-file
c 8  index             I  Index in array garesult
c 9  juer              I  Unit number of error file.
c10  ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c GAresultrq       Getij Analyse, array GARESULT: request for memory
c GANulpunten      Getij Analyse, zoek NULPUNTEN
c GAinitQenV       Getij Analyse, INITialiseer voor Q EN V reeksen
c GAextrem3        Getij Analyse, bepaal EXTREMen 3
c GAVloedVolume    Getij Analyse, bepaal VLOEDVOLUME 
c GAEbVolume       Getij Analyse, bepaal EBVOLUME
c=======================================================================
c
c     Declaration of Parameters:
c
      use       gadata
      include '../include/errcod.i'
c
      integer   vartype ,ivar   ,iloc  ,index   ,juer    ,ker 
      real      tstart  ,tstop  ,tstep ,EstimPer 
c
c     Declaration of local variables:
c
      integer   getijnum      ,request,indexp
      real      tba   ,tea    ,ptb    ,pte    ,Wmax   ,Tmax  ,Wmin   ,
     +          Tmin  ,teVerwacht     ,veTijd ,evTijd ,tduur
c
      real      GAinitQenV ,GAVloedVolume ,GAEbVolume
      external  GAinitQenV ,GAVloedVolume ,GAEbVolume
c
c     Bepaal het begin van de eerste getijperiode
c
      tba = GAinitQenV (tstart ,tstop, tstep ,EstimPer)
c
c     Bepaal getijgrootheden uit alle getijperioden
c
      indexp  = index
      request = index + 4 
      call garesultrq (request  ,juer    , ker     )
c
      garesult(index+type)   = vartype
      garesult(index+grooth) = ivar 
      garesult(index+locat)  = iloc
c      
      index    = index + 4 
      getijnum = 0
c
      do 
c
c        Bepaal of er nog een volledig getij beschikbaar is, d.w.z. dat
c        de tijdreeks na de begintijd nog een getijperiode verderloopt.
c        Is dit niet het geval, dan stoppen.
c
         if (tba + EstimPer .gt. tstop) exit
c
c           Zet het getijnummer
c
            getijnum = getijnum + 1
            request  = index + 12
c            
            call garesultrq (request  ,juer    , ker     )
            if (ker.eq.fatal) goto 9000 
c
            garesult(index+igetij) = getijnum
c
c           Initialiseer de begintijd van de getijperiode
c
            garesult(index+begin) = tba
c
c           Zoek vanaf 3 uur voor tot 2 uur na de verwachte eindtijd
c           van de huidige getijperiode naar de eerste vloed-eb
c           kentering
c
            teVerwacht = tba + EstimPer
            ptb = teVerwacht - 10800.
            if (ptb .lt. tba) ptb = tba
            pte = teVerwacht + 7200.
            if (pte + tstep .gt. tstop) pte = tstop - tstep
c            
            call GANulpunten (ptb ,pte ,tstep ,veTijd ,evTijd)
c
c           Als einde van de getijperiode wordt het tijdstip van de
c           gevonden vloed-eb kentering gekozen, mits dit tijdstip ten
c           hoogste 3 uur voor of 2 uur na het verwachte einde (tb + 
c           getijperiode) ligt.
c           Is dit niet het geval, dan wordt als verwachte einde het t
c           tijdstip van het minimum aangehouden, mits dit tijdstip ten
c           hoogste 2 uur voor of 2 uur na het verwachte einde ligt.
c           Is ook dit niet het geval dan wordt het einde van de
c           verwachte getijperiode gekozen.
c
            ptb = tba + 1800.

            if (veTijd .gt. teVerwacht - 10800. .and.
     +          veTijd .lt. teVerwacht + 7200.) then
               tea = veTijd
            else
c                
c              Bepaal extremen
c
               call GAextrem3 (ptb    ,pte    ,tstep  ,Wmax  ,Tmax ,
     +                        Wmin   ,Tmin   )
c            
               if (Tmin .gt. teVerwacht - 7200. .and.
     +             Tmin .lt. teVerwacht + 7200.) then
                  tea = Tmin
               else
                  tea = teVerwacht
               endif
            endif   
c
c           Zoek vanaf 0.5 uur na de begintijd van het huidige getij
c           tot het einde van de getijperiode naar het maximum en het
c           minimum.
c
            pte = tea
            call GAextrem3 (ptb    ,pte    ,tstep  ,Wmax  ,Tmax ,
     +                      Wmin   ,Tmin   )
c
            garesult(index+mintijd) = Tmin
            garesult(index+maxtijd) = Tmax
            garesult(index+minw)    = Wmin
            garesult(index+maxw)    = Wmax
c
            garesult(index+eind)    = pte
c
c           Bereken de duur van het getij
c
            tduur                = pte - tba 
            garesult(index+duur) = tduur
c
c           Bereken vloed- en ebvolume en het gemiddelde
c
            garesult(index+slag)   = 0.
            garesult(index+vloed)  = GAVloedVolume (tba , pte ,tstep)
            garesult(index+eb)     = GAEbVolume (tba , pte ,tstep)
            garesult(index+gemidd) = (garesult(index+vloed) +
     +                                garesult(index+eb)) / tduur 
c             
c           Bereken gemiddelde positieve en negatieve snelheid
c
            if (vartype .eq. vreeks) then
               garesult(index+vloed) = garesult(index+vloed) / tduur
               garesult(index+eb)    = garesult(index+eb) / tduur
            endif
c
c           De volgende getijperiode begint bij het einde van de huidige
c
            tba = pte
c            
            index = request                  
      enddo
c
      garesult(indexp + ngetij) = getijnum
c
 9000 continue  
c
      end
