      subroutine GANulpunten (ptb ,pte ,tstep ,tpos  ,tneg)
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
c Module:             GANulpunten (zoek NULPUNTEN) 
c
c Module description: Bepaal de eerste nulpunten van positieve en 
c                     negatieve doorschrijding in de gegeven periode.
c                     Het tijdstip van doorschrijding wordt afgerond
c                     naar de dichtstbijzijnde minuutwaarde.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  ptb               I  begin tijd zoekperiode in seconden tov 
c                         start berekening
c 2  pte               I  eind tijd zoekperiode in seconden tov 
c                         start berekening
c 3  tstep             I  tijdstap van de reeks in seconden
c 4  tpos              O  tijd van nulpunt waarna positieve waarden
c                         beginnen in seconden tov start berekening
c 5  tneg              O  tijd van nulpunt waarna negatieve waarden
c                         beginnen in seconden tov start berekening
c-----------------------------------------------------------------------
c Subprogram calls:
c GAIndex          Getij Analyse, bepaal INDEX in tijdreeks 
c=======================================================================
c
c     Declaration of Parameters:
c
      use gadata
c
      real          ptb   ,pte  ,tstep  ,tpos  ,tneg
c
c     Declaration of local variables:
c
      integer       i     ,ib   ,ie    
      real          aWaarde     ,nWaarde
      logical       pos   ,neg
c      
      integer       GAIndex
      external      GAIndex
c
c     Initialiseer maxtijd en mintijd op begin periode - 1 seconde
c
      tpos = ptb - 1.
      tneg = ptb - 1.      
c
      pos  = .false.
      neg  = .false.
c      
c     Bereken start index Ib en eind index Ie 
c
      ib = GAIndex(ptb,tijden(1),tstep)
      ie = GAIndex(pte,tijden(1),tstep)
c
      do i = ib,ie
c        Je mag 1 tijdstap voorbij einde periode kijken
         aWaarde = reeks(i)
         nWaarde = reeks(i+1)

         if (.not.pos) then
            if (aWaarde .lt. 0. .and. nWaarde .ge. 0.) then
               if (abs(aWaarde) .lt. abs(nWaarde)) then
                  tpos = tijden(i)
               else
                  tpos = tijden(i+1)
               endif
               pos  = .true.
            endif 
         endif
         if (.not.neg) then
            if (aWaarde .ge. 0. .and. nWaarde .lt. 0.) then
               if (abs(aWaarde) .lt. abs(nWaarde)) then
                  tneg = tijden(i)
               else
                  tneg = tijden(i+1)
               endif
               neg  = .true.
            endif
         endif
         if (pos .and. neg) exit
      enddo
c
      end
c
      subroutine GALineFit (ptb ,pte ,tstep ,a0 ,a1)
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
c Module:             GALineFit (bepaal LINE FIT) 
c
c Module description: Bepaal een lijn a1*x+a0 door de tijdreeks over de 
c                     gegeven periode d.m.v. lineare regressie.
c                     Referentie : polytechnisch zakboekje,
c                     40e druk, blz 120
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  ptb               I  begin tijd de periode in seconden tov 
c                         start berekening
c 2  pte               I  eind tijd van de periode in seconden tov 
c                         start berekening
c 3  tstep             I  tijdstap van de reeks in seconden
c 4  a0                O  coefficient a0 in regressielijn
c 5  a1                O  coefficient a1 in regressielijn
c-----------------------------------------------------------------------
c Subprogram calls:
c GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
c=======================================================================
cc
c     Declaration of Parameters:
c
      use gadata
c
      real             ptb   ,pte  ,tstep ,a0   ,a1
c
c     Declaration of local variables:
c
      integer          i     ,ib   ,ie          
      double precision sx    ,sx2  ,sy    ,sxy  ,x   ,y   ,n 
c
      integer       GAIndex
      external      GAIndex
c
c  sx     De som van alle x-waarden
c  sx2    De som van alle y-waarden
c  sy     De som van alle x-waarden in het kwadraat
c  sxy    De som van alle x-waarden maal de y-waarden
c  n      Volgnummer van stap
c      
c     Bereken start index Ib en eind index Ie 
c
      ib = GAIndex(ptb,tijden(1),tstep)
      ie = GAIndex(pte,tijden(1),tstep)
c
      sx  = 0. 
      sx2 = 0.
      sy  = 0.
      sxy = 0.
      n   = 0.    
c
      do i = ib,ie 
         x   = n
         sx  = sx + x
         sx2 = sx2 + x * x
         y   = reeks(i)
         sy  = sy + y
         sxy = sxy + x * y
         n   = n + 1
      enddo

      a0 = (sy * sx2 - sx * sxy) / (n * sx2 - sx * sx)
      a1 = ( n * sxy - sx *  sy) / (n * sx2 - sx * sx)
 
      end
c
      real function GAGemiddelde (ptb ,pte ,tstep)
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
c Module:             GAGemiddelde (bepaal GEMIDDELDE) 
c
c Module description: Bereken het gemiddelde gedurende de gegeven periode. 
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 0  GAGemiddlede      O  gemiddelde
c 1  ptb               I  begin tijd van een getij in seconden tov 
c                         start berekening
c 2  pte               I  eind tijd van een getij in seconden tov 
c                         start berekening
c 3  tstep             I  tijdstap van de reeks in seconden
c-----------------------------------------------------------------------
c Subprogram calls:
c GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
c=======================================================================
c
c     Declaration of Parameters:
c
      use gadata
c
      real          ptb   ,pte  ,tstep
c
c     Declaration of local variables:
c 
      integer       i     ,ib   ,ie   ,n
      real          som
c
      integer       GAIndex
      external      GAIndex
c
c     Bereken start index Ib en eind index Ie 
c
      ib = GAIndex(ptb,tijden(1),tstep)
      ie = GAIndex(pte,tijden(1),tstep)
c
c     Sommeer over periode [tb;te], dus het eerste tijdstip van deze berekening
c     is gelijk aan het laatste tijdstip van de vorige berekening
c      
      n   = 0
      som = 0.
      do i = ib ,ie
         som = som + reeks(i)
         n   = n + 1
      enddo
c
      GAGemiddelde = som / n
c
      end
c
      real function GAVloedVolume (ptb , pte ,tstep)
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
c Module:             VloedVolume (bepaal VLOEDVOLUME) 
c
c Module description: Bereken het vloedbvolume gedurende de gegeven
c                     periode. 
c
c Preconditie:        Bij vloed heb je negatieve debieten.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 0  GAVloedVolume     O  vloedvolume (negatief)
c 1  ptb               I  begin tijd van een getij in seconden tov 
c                         start berekening
c 2  pte               I  eind tijd van een getij in seconden tov 
c                         start berekening
c 3  tstep             I  tijdstap van de reeks in seconden
c-----------------------------------------------------------------------
c Subprogram calls:
c GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
c=======================================================================
c
c     Declaration of Parameters:
c
      use gadata
c
      real          ptb   ,pte  ,tstep
c
c     Declaration of local variables:
c
      integer       i     ,ib   ,ie
      real          vol   ,q
c
      integer       GAIndex
      external      GAIndex
c      
c     Bereken start index Ib en eind index Ie 
c
      ib = GAIndex(ptb,tijden(1),tstep)
      ie = GAIndex(pte,tijden(1),tstep)
c
c     Sommeer over periode <tb;te], dus eerste tijdstip overslaan
c
      vol = 0.
      do i = ib + 1 , ie
         q = reeks(i)
         if (q .lt. 0.0 ) vol = vol + q
      enddo
c
      GAVloedVolume =  vol * tstep
c
      end
c
      real function GAEbVolume (ptb , pte ,tstep)
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
c Module:             GAEbVolume (bepaal EBVOLUME) 
c
c Module description: Bereken het ebvolume gedurende de gegeven periode. 
c
c Preconditie:        Bij eb heb je positieve debieten.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 0  GAEbVolume        O  ebvolume (positief)
c 1  ptb               I  begin tijd van een getij in seconden tov 
c                         start berekening
c 2  pte               I  eind tijd van een getij in seconden tov 
c                         start berekening
c 3  tstep             I  tijdstap van de reeks in seconden
c-----------------------------------------------------------------------
c Subprogram calls:
c GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
c=======================================================================
c
c     Declaration of Parameters:
c
      use gadata
c
      real          ptb   ,pte  ,tstep
c
c     Declaration of local variables:
c
      integer       i     ,ib   ,ie
      real          vol   ,q
c
      integer       GAIndex
      external      GAIndex
c       
c     Bereken start index Ib en eind index Ie 
c
      ib = GAIndex(ptb,tijden(1),tstep)
      ie = GAIndex(pte,tijden(1),tstep)
c
c     Sommeer over periode <tb;te], dus eerste tijdstip overslaan
c
      vol = 0.
      do i = ib + 1 , ie
         q = reeks(i)
         if (q .gt. 0.0 ) vol = vol + q
      enddo
c
      GAEbVolume =  vol * tstep
c
      end
c      
      integer function GAIndex (t ,t0 ,tstep)
c
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Getij Analyse Module
c
c Programmer:         J.Kuipers
c
c Module:             GAIndex (bepaal INDEX in tijdreeks) 
c
c Module description: Bepaal de dichtsbijliggende index in de tijdreeks
c                     bij opgegeven
c                     tijd. 
c
c Preconditie:        Equidistante tijdreeks
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 0  GAIndex           O  index in tijdreeks van gevraagde tijd (1e punt
c                         heeft index 1.
c 1  t                 I  tijd in seconden tov start berekening waarvoor
c                         de index gevraagd wordt.
c 2  t0                I  tijd in seconden tov start berekening van 
c                         eerste punt in de reeks. 
c 3  tstep             I  tijdstap van de reeks in seconden
c=======================================================================
c
c     Declaration of Parameters:
c
      real   t  ,t0  ,tstep
c
      GAIndex = (t-t0)/tstep + 1.5
c      
      end
