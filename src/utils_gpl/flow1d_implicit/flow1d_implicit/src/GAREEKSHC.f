      subroutine GAReeksHenC (tstart ,tstop ,tstep ,EstimPer ,vartype,
     +                        ivar   ,iloc  ,index ,juer     ,ker    )
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
c Module:             GAReeksHenC (analyseer REEKS H EN C) 
c
c Module description: Bepaal de getijgrootheden van de soort H 
c                     (waterstand) of C (concentratie)
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
c                         3 = snelheid      4 = zout conc.
c                         5 = saliniteit    6 = chloride conc.
c 6  ivar              I  variabele nummer op de His-file       
c 7  iloc              I  locatie nummer op de His-file 
c 8  index             I  Index in array garesult
c 9  juer              I  unit number of error file.
c10  ker               O  error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c GAresultrq       Getij Analyse, array GARESULT: request for memory
c GAindex          Getij Analyse, bepaal INDEX in tijdreeks
c GAinitHenC       Getij Analyse, INITialiseer voor H EN C reeksen
c GAextrem         Getij Analyse, bepaal EXTREMen
c GAextrem2        Getij Analyse, bepaal EXTREMen 2
c GAGemiddelde     Getij Analyse, bepaal GEMIDDELDE  
c=======================================================================
c
c     Declaration of Parameters:
c
      use       gadata
      include '../include/errcod.i'
c
      integer   vartype ,ivar   ,iloc  ,index  ,indexp, juer    ,ker 
      real      tstart  ,tstop  ,tstep ,EstimPer 
c
c     Declaration of local variables:
c
      integer   getijnum   ,request   ,i
      real      tba        ,ptb       ,pte    ,Wmax  ,Tmax  ,Wmin   ,
     +          Tmin       ,teVerwacht,Tmax1
c
      integer   GAIndex
      real      GAinitHenC ,GAGemiddelde
      external  GAIndex    ,GAinitHenC   ,GAGemiddelde
c
c     Bepaal het begin van de eerste getijperiode
c
      tba = GAinitHenC (tstart ,tstop, tstep ,EstimPer)
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
c        Bepaal of er nog een volledig getij beschikbaar is, d.w.z. dat de
c        tijdreeks na de begintijd nog een getijperiode verderloopt.
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
c           Zoek vanaf 2 uur na de begintijd van het huidige getij gedurende
c           een getijperiode naar het maximum en het minimum.
c
            ptb = tba + 7200.
            pte = ptb + EstimPer
            if (pte + tstep .gt. tstop) pte = tstop - tstep
c            
            call GAextrem (ptb    ,pte    ,tstep  ,Wmax  ,Tmax ,Wmin  ,
     +                     Tmin   )
c
c           Als geen minimum is gevonden, dan is er sprake van een monotoon
c           dalende of stijgende lijn. Probeer extremen te bepalen via de
c           alternatieve methode.
c
            if (Tmin .lt. ptb) then
               call GAextrem2 (ptb    ,pte    ,tstep  ,Wmax  ,Tmax  ,
     +                         Wmin   ,Tmin   ,juer    , ker )
               if (ker.eq.fatal) goto 9000    
            endif
c
c           Als einde van de getijperiode wordt het tijdstip van het 
c           gevonden maximum gekozen, mits dit tijdstip ten hoogste 2
c           uur voor of 2 uur na het verwachte einde (tb + getijperiode)
c           ligt. Is dit niet het geval dan wordt het verwachte einde
c           van de getijperiode aangehouden. 
c           Indien helemaal geen maximum is gevonden, dan wordt als
c           maximum ok het einde van de verwachte getijperiode gekozen.
c
            Tmax1 = Tmax
            teVerwacht = tba + EstimPer
            if (Tmax .le. teVerwacht - 7200. .or. 
     +          Tmax .ge. teVerwacht + 7200.) then
               if (Tmax .lt. ptb) then
                  Tmax = teVerwacht
                  i    = GAIndex(Tmax,tijden(1),tstep)
                  Wmax = reeks(i)
               else
                  Tmax = teVerwacht
               endif          
            endif
c
            garesult(index+mintijd) = Tmin
            garesult(index+maxtijd) = Tmax1
            garesult(index+minw)    = Wmin
            garesult(index+maxw)    = Wmax
c
            pte = Tmax 
            garesult(index+eind) = pte
c
c           Bereken het gemiddelde
c
            garesult(index+gemidd) = GAGemiddelde (tba ,pte ,tstep)
c            
            garesult(index+duur)   = pte - tba
            garesult(index+slag)   = Wmax - Wmin
            garesult(index+vloed)  = 0.
            garesult(index+eb)     = 0.
c
c           De volgende getijperiode begint bij het einde van de huidige
c            
            tba = pte
c            
            index = request            
      enddo
c
      garesult(indexp+ngetij) = getijnum
c
 9000 continue    
c
      end
