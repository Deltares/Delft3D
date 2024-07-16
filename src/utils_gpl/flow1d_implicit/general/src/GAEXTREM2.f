      subroutine GAextrem2 (ptb    ,pte    ,tstep  ,Wmax  ,Tmax  ,
     +                      Wmin   ,Tmin   ,juer   ,ker   )
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
c Module:             GAextrem2 (bepaal EXTREMen) 
c
c Module description: Bepaal 'extremen' in monotoon stijgende/
c                     dalende lijn
c                     
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 1  ptb               I  begin tijd van een getij in seconden tov 
c                         start berekening
c 2  pte               I  eind tijd van een getij in seconden tov 
c                         start berekening
c 3  tstep             I  tijdstap van de reeks in seconden
c 4  Wmax              O  maximum waarde in getijperiode 
c 5  Tmax              O  Tijd van maximum in seconden tov 
c                         start berekening
c 6  Wmin              O  minimum waarde in getijperiode
c 7  Tmin              O  Tijd van minimum in seconden tov 
c                         start berekening
c 8  juer              I  Unit number of error file.
c 9  ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c-----------------------------------------------------------------------
c Subprogram calls:
c GAIndex          Getij Analyse, bepaal INDEX in tijdreeks
c GALineFit        Getij Analyse, bepaal LINE FIT
c error            Error messages
c=======================================================================
c
c     Declaration of Parameters:
c
      use gadata
      include '..\include\errcod.i'      
c
      integer  juer   ,ker 
      real     ptb    ,pte    ,tstep  ,Wmax  ,Tmax  ,
     +         Wmin   ,Tmin  
c
c     Declaration of local variables:
c
      integer  i       ,ib      ,ie      ,i0    ,n     ,ierr ,
     +         newsize
      real     aWaarde ,pWaarde ,cWaarde ,cmax  ,cmin  ,a0   ,a1
c 
      character*10      artype,siztxt
      character*100     txt
c
      integer  GAIndex
      external GAIndex
c
c     Als geen extremen kunnen worden gevonden dan er sprake van een monotoon
c     dalende of stijgende lijn.
c     In deze gevallen wordt een correctielijn door de reeks gefit.
c     De nulpunten van h - correctielijn worden bepaald. Tussen elke twee
c     nulpunten wordt het moment van maximale absolute afwijking van h-cor
c     bepaald. Als gevonden punt een absoluut max of min is, dan wordt deze 
c     als maximum of minimum aangewezen.
c     Na het laatste nulpunt wordt niet meer gezocht.
c     Dat bepalen van de nulpunten is natuurlijk onzinnig. Je kunt net zo
c     goed gelijk de momenten van maximale en minimale afwijking bepalen
c     vanaf begin reeks tot laatste nulpunt.
c
c     Bepaal de gecorrigeerde lijn
c      
c     Bereken eerst start index Ib en eind index Ie 
c
      ib = GAIndex(ptb,tijden(1),tstep)
      ie = GAIndex(pte,tijden(1),tstep)
c
c     Fit een lijn a1*x+a0 d.m.v. lineaire regressie
c
      call GALineFit (ptb ,pte ,tstep ,a0 ,a1)
c
c     Bepaal de gecorrigeerde lijn

c     Allocate scratch arrays.
c
      artype  = 'coreeks'
      newsize = ie
      if (allocated (coreeks)) then 
         if (size(coreeks).lt.ie) then
            deallocate(coreeks, stat=ierr)
            if ( ierr .gt. 0 ) goto 9010
            allocate(coreeks(ie), stat=ierr)
            if ( ierr .gt. 0 ) goto 9000
         endif   
      else
         allocate(coreeks(ie), stat=ierr)
         if ( ierr .gt. 0 ) goto 9000
      endif
c
      n = 0
      do i = ib , ie 
         coreeks(i) = reeks(i) - (a1 * n + a0)
         n = n + 1
      enddo
c
c     Bepaal het laatste nulpunt van de gecorrigeerde lijn
c
      i0 = ib
      do i = ib + 1 , ie 
          aWaarde = coReeks(i)
          pWaarde = coReeks(i-1)
c
          if (aWaarde * pWaarde .le. 0.0) then
c            Nulpunt gevonden
             if (abs(aWaarde) .lt. abs(pWaarde)) then
                i0 = i
             else
                i0 = i-1
             endif
          endif
      enddo
c      
c     Bepaal de extremen in de gecorrigeerde lijn tot het laatste nulpunt
c     Initialiseer maximum en minium
c
      cmax = -1.0e+30
      cmin =  1.0e+30

      do i = ib , i0 
         cWaarde = coReeks(i)
         if (cWaarde .gt. cmax) then
            cmax = cWaarde
            Tmax = tijden(i)
            Wmax = reeks(i)
         endif
         if (cWaarde .lt. cmin) then
            cmin = cWaarde
            Tmin = tijden(i)
            Wmin = reeks(i)
         endif
      enddo
      return
c
 9000 continue
c     Error allocating array space
      write(siztxt,'(i10)') newsize
      txt   = 'GAINTERPOL @' // siztxt // '@ @' // artype // '@'
      ker   = fatal
      call error (juer, txt, ealloc, ker )
      return
c      
 9010 continue
c     Error deallocating array space'
      txt   = 'GAINTERPOL @' // artype // '@'
      ker   = fatal
      call error (juer, txt, edeall, ker )
c
      end
