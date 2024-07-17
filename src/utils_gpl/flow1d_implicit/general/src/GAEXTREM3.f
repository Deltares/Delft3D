      subroutine GAextrem3 (ptb    ,pte    ,tstep  ,Wmax  ,Tmax ,
     +                      Wmin   ,Tmin   )
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
c Module:             GAextrem (bepaal EXTREMen) 
c
c Module description: Bepaal maximum en minimum binnen gegeven zoek 
c                     periode in de reeks. De zoekperiode stopt altijd
c                     minstens 1 tijdstap voor het einde van de reeks.
c                     
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  ptb               I  begin tijd (getij)periode in seconden tov 
c                         start berekening
c 2  pte               I  eind tijd (getij)periode in seconden tov 
c                         start berekening
c 3  tstep             I  tijdstap van de reeks in seconden
c 4  Wmax              O  maximum waarde in getijperiode 
c 5  Tmax              O  Tijd van maximum in seconden tov 
c                         start berekening
c 6  Wmin              O  minimum waarde in getijperiode
c 7  Tmin              O  Tijd van minimum in seconden tov 
c                         start berekening
c-----------------------------------------------------------------------
c Subprogram calls:
c GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
c=======================================================================
c
c     Declaration of Parameters:
c
      use gadata
c
      real           ptb    ,pte    ,tstep  ,Wmax  ,Tmax  ,
     +               Wmin   ,Tmin  
c
c     Declaration of local variables:
c
      integer        trend   ,i       ,ib     ,ie
      integer, parameter :: DALEND=-1 ,STIJGEND=1 ,GEENTREND=0
      real           aWaarde ,nWaarde ,Tmax1  ,Tmin1  ,Tmax2 ,
     +               Tmin2   ,Wmax1   ,Wmin1  ,Wmax2  ,Wmin2  
c
      integer        GAIndex
      external       GAIndex
c
c     Initialiseer maximum en minium
c
      Wmax = -1.0e+30
      Wmin =  1.0e+30
c  
c     Initialiseer maxtijd en mintijd op begin periode - 1 seconde

      Tmax = ptb - 1.
      Tmin = ptb - 1.
c      
c     Zoek het globale maximum en minimum in de gegeven periode.
c     Een locaal maximum is een moment dat de trend omslaat van STIJGEND 
c     naar DALEND. Het globale maximum is het grootste locale maximum.
c     Een locaal minimum is een moment dat de trend omslaat van DALEND 
c     naar STIJGEND. Het globale minimum is het kleinste locale minimum.
c     Indien er geen maximum en minimum wordt gevonden, dan wordt de
c     laagste waarde uit de reeks als minimum gebruikt en de hoogste
c     waarde uit de reeks als maximum.
 
      Tmax1 = Tmax
      Tmin1 = Tmin 
      Tmax2 = Tmax
      Tmin2 = Tmin
      Wmax1 = Wmax
      Wmin1 = Wmin
      Wmax2 = Wmax
      Wmin2 = Wmin

      trend = GEENTREND
c      
c     Bereken start index Ib en eind index Ie 
c
      ib = GAIndex(ptb,tijden(1),tstep)
      ie = GAIndex(pte,tijden(1),tstep)
c
      do i = ib,ie
c         Je mag 1 tijdstap voorbij einde periode kijken
          aWaarde = reeks(i)
          nWaarde = reeks(i+1) 
c
          if (nWaarde .lt. aWaarde) then
c             nieuwe trend is DALEND
              if (trend .eq. STIJGEND) then
c                 locaal maximum gevonden, controleer of dit in
c                 aanmerking komt als globaal maximum
                  if (aWaarde .gt. Wmax1) then
                      Wmax1 = aWaarde
                      Tmax1 = tijden(i)
                  endif
              endif
              trend = DALEND
          else if (nWaarde .gt. aWaarde) then
c            nieuwe trend is STIJGEND
             if (trend .eq. DALEND) then
c                 locaal minimum gevonden, controleer of dit in
c                 aanmerking komt als globaal minimum
                  if (aWaarde .lt. Wmin1) then
                        Wmin1 = aWaarde
                        Tmin1 = tijden(i)
                  endif
              endif
              trend = STIJGEND
          endif
c          
c         als t[tn] = t[ta] handhaaf de huidige trend
c
          if (aWaarde .gt. Wmax2) then
             Wmax2 = aWaarde
             Tmax2 = tijden(i)
          endif
          if (aWaarde .lt. Wmin2) then
             Wmin2 = aWaarde
             Tmin2 = tijden(i)
          endif
      enddo
c
c     Bepaal de te gebruiken extremen
c
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
c
      end
