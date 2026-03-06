subroutine GANulpunten (ptb ,pte ,tstep ,tpos  ,tneg)
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
! Module:             GANulpunten (zoek NULPUNTEN)
!
! Module description: Bepaal de eerste nulpunten van positieve en
!                     negatieve doorschrijding in de gegeven periode.
!                     Het tijdstip van doorschrijding wordt afgerond
!                     naar de dichtstbijzijnde minuutwaarde.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  ptb               I  begin tijd zoekperiode in seconden tov
!                         start berekening
! 2  pte               I  eind tijd zoekperiode in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
! 4  tpos              O  tijd van nulpunt waarna positieve waarden
!                         beginnen in seconden tov start berekening
! 5  tneg              O  tijd van nulpunt waarna negatieve waarden
!                         beginnen in seconden tov start berekening
!-----------------------------------------------------------------------
! Subprogram calls:
! GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
!=======================================================================
!
!     Declaration of Parameters:
!
   use gadata
!
   real          ptb   ,pte  ,tstep  ,tpos  ,tneg
!
!     Declaration of local variables:
!
   integer       i     ,ib   ,ie
   real          aWaarde     ,nWaarde
   logical       pos   ,neg
!
   integer       GAIndex
   external      GAIndex
!
!     Initialiseer maxtijd en mintijd op begin periode - 1 seconde
!
   tpos = ptb - 1.
   tneg = ptb - 1.
!
   pos  = .false.
   neg  = .false.
!
!     Bereken start index Ib en eind index Ie
!
   ib = GAIndex(ptb,tijden(1),tstep)
   ie = GAIndex(pte,tijden(1),tstep)
!
   do i = ib,ie
!        Je mag 1 tijdstap voorbij einde periode kijken
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
!
end
!
subroutine GALineFit (ptb ,pte ,tstep ,a0 ,a1)
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
! Module:             GALineFit (bepaal LINE FIT)
!
! Module description: Bepaal een lijn a1*x+a0 door de tijdreeks over de
!                     gegeven periode d.m.v. lineare regressie.
!                     Referentie : polytechnisch zakboekje,
!                     40e druk, blz 120
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 1  ptb               I  begin tijd de periode in seconden tov
!                         start berekening
! 2  pte               I  eind tijd van de periode in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
! 4  a0                O  coefficient a0 in regressielijn
! 5  a1                O  coefficient a1 in regressielijn
!-----------------------------------------------------------------------
! Subprogram calls:
! GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
!=======================================================================
!c
!     Declaration of Parameters:
!
   use gadata
!
   real             ptb   ,pte  ,tstep ,a0   ,a1
!
!     Declaration of local variables:
!
   integer          i     ,ib   ,ie
   double precision sx    ,sx2  ,sy    ,sxy  ,x   ,y   ,n
!
   integer       GAIndex
   external      GAIndex
!
!  sx     De som van alle x-waarden
!  sx2    De som van alle y-waarden
!  sy     De som van alle x-waarden in het kwadraat
!  sxy    De som van alle x-waarden maal de y-waarden
!  n      Volgnummer van stap
!
!     Bereken start index Ib en eind index Ie
!
   ib = GAIndex(ptb,tijden(1),tstep)
   ie = GAIndex(pte,tijden(1),tstep)
!
   sx  = 0.
   sx2 = 0.
   sy  = 0.
   sxy = 0.
   n   = 0.
!
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
!
real function GAGemiddelde (ptb ,pte ,tstep)
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
! Module:             GAGemiddelde (bepaal GEMIDDELDE)
!
! Module description: Bereken het gemiddelde gedurende de gegeven periode.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 0  GAGemiddlede      O  gemiddelde
! 1  ptb               I  begin tijd van een getij in seconden tov
!                         start berekening
! 2  pte               I  eind tijd van een getij in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
!-----------------------------------------------------------------------
! Subprogram calls:
! GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
!=======================================================================
!
!     Declaration of Parameters:
!
   use gadata
!
   real          ptb   ,pte  ,tstep
!
!     Declaration of local variables:
!
   integer       i     ,ib   ,ie   ,n
   real          som
!
   integer       GAIndex
   external      GAIndex
!
!     Bereken start index Ib en eind index Ie
!
   ib = GAIndex(ptb,tijden(1),tstep)
   ie = GAIndex(pte,tijden(1),tstep)
!
!     Sommeer over periode [tb;te], dus het eerste tijdstip van deze berekening
!     is gelijk aan het laatste tijdstip van de vorige berekening
!
   n   = 0
   som = 0.
   do i = ib ,ie
      som = som + reeks(i)
      n   = n + 1
   enddo
!
   GAGemiddelde = som / n
!
end
!
real function GAVloedVolume (ptb , pte ,tstep)
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
! Module:             VloedVolume (bepaal VLOEDVOLUME)
!
! Module description: Bereken het vloedbvolume gedurende de gegeven
!                     periode.
!
! Preconditie:        Bij vloed heb je negatieve debieten.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 0  GAVloedVolume     O  vloedvolume (negatief)
! 1  ptb               I  begin tijd van een getij in seconden tov
!                         start berekening
! 2  pte               I  eind tijd van een getij in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
!-----------------------------------------------------------------------
! Subprogram calls:
! GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
!=======================================================================
!
!     Declaration of Parameters:
!
   use gadata
!
   real          ptb   ,pte  ,tstep
!
!     Declaration of local variables:
!
   integer       i     ,ib   ,ie
   real          vol   ,q
!
   integer       GAIndex
   external      GAIndex
!
!     Bereken start index Ib en eind index Ie
!
   ib = GAIndex(ptb,tijden(1),tstep)
   ie = GAIndex(pte,tijden(1),tstep)
!
!     Sommeer over periode <tb;te], dus eerste tijdstip overslaan
!
   vol = 0.
   do i = ib + 1 , ie
      q = reeks(i)
      if (q .lt. 0.0 ) vol = vol + q
   enddo
!
   GAVloedVolume =  vol * tstep
!
end
!
real function GAEbVolume (ptb , pte ,tstep)
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
! Module:             GAEbVolume (bepaal EBVOLUME)
!
! Module description: Bereken het ebvolume gedurende de gegeven periode.
!
! Preconditie:        Bij eb heb je positieve debieten.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 0  GAEbVolume        O  ebvolume (positief)
! 1  ptb               I  begin tijd van een getij in seconden tov
!                         start berekening
! 2  pte               I  eind tijd van een getij in seconden tov
!                         start berekening
! 3  tstep             I  tijdstap van de reeks in seconden
!-----------------------------------------------------------------------
! Subprogram calls:
! GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
!=======================================================================
!
!     Declaration of Parameters:
!
   use gadata
!
   real          ptb   ,pte  ,tstep
!
!     Declaration of local variables:
!
   integer       i     ,ib   ,ie
   real          vol   ,q
!
   integer       GAIndex
   external      GAIndex
!
!     Bereken start index Ib en eind index Ie
!
   ib = GAIndex(ptb,tijden(1),tstep)
   ie = GAIndex(pte,tijden(1),tstep)
!
!     Sommeer over periode <tb;te], dus eerste tijdstip overslaan
!
   vol = 0.
   do i = ib + 1 , ie
      q = reeks(i)
      if (q .gt. 0.0 ) vol = vol + q
   enddo
!
   GAEbVolume =  vol * tstep
!
end
!
integer function GAIndex (t ,t0 ,tstep)
!
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Getij Analyse Module
!
! Programmer:         J.Kuipers
!
! Module:             GAIndex (bepaal INDEX in tijdreeks)
!
! Module description: Bepaal de dichtsbijliggende index in de tijdreeks
!                     bij opgegeven
!                     tijd.
!
! Preconditie:        Equidistante tijdreeks
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
! 0  GAIndex           O  index in tijdreeks van gevraagde tijd (1e punt
!                         heeft index 1.
! 1  t                 I  tijd in seconden tov start berekening waarvoor
!                         de index gevraagd wordt.
! 2  t0                I  tijd in seconden tov start berekening van
!                         eerste punt in de reeks.
! 3  tstep             I  tijdstap van de reeks in seconden
!=======================================================================
!
!     Declaration of Parameters:
!
   real   t  ,t0  ,tstep
!
   GAIndex = (t-t0)/tstep + 1.5
!
end
