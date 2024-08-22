      subroutine IVBCLS(time   ,timep  ,ngrid  ,nbran  ,branch ,
     +                  x      ,gridnm ,q      ,h      ,q1     )
         use precision_basics, only: dp
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Flow Module
c
c Programmer:
c
c Module:             IVBCLS (                                )
c
c Module description:
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  1 time              I  Actual time level (at t=n+1) in sec.
c  2 timep             I  Previous time level (at t=n) in sec.
c  3 ngrid             I  Number of grid points in network.
c  4 nbran             I  Number of branches.
c  5 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c  6 x(ngrid)          I  Coordinates in every grid point.
c  7 q(ngrid)          I  Discharge in every grid point at t=n+1
c                         (TIME).
c  8 h(ngrid)          I  Water level in every grid point at t=n+1
c                         (TIME).
c  9 q1(ngrid)         I  Discharge in every grid point at t=n
c                         (TIMEP).
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: ivbcls.pf,v $
c Revision 1.2  1999/03/17  14:22:46  kuipe_j
c correctie
c
c Revision 1.1  1999/03/15  14:30:13  kuipe_j
c IVBDOS
c
c
c***********************************************************************
c
c     Declaration of Parameters:
c
      integer          ngrid           ,nbran
      integer          branch(4,nbran)
      real             x     (ngrid)
	double precision q (ngrid)  ,h(ngrid)   ,q1(ngrid)
      double precision time            ,timep
      character(len=40) gridnm(ngrid)
c
c     Common block for communication with Sobek
c
c     NAME              IO DESCRIPTION
c     ivbact            O  Flag to indicate MHW processor run
c     makhmx            O  Flag to make Sobek write max levels
c     wrires            O  Flag to make Sobek write restart moment
c     they are initialised .false. by Sobek
      logical         ivbact, makhmx, wrires
      common /ivbdos/ ivbact, makhmx, wrires
c
c     Declaration of local variables:
c
      integer      io_ini, io_sob, ihulp , io_mes, nosdat,
     j             nosdam, io_osk, i
      parameter   (nosdam=99)
      real         qtab_os(nosdam), htab_os(nosdam)
      integer      gpsvkw, gpsvkh, gprott, gpdord, gpmamo,
     j             rtncod, gpsvko, gproom
      real         qlobit, rotter, dordre, hmax_hvh, hcrit ,
     j             hin   , qin   , peilos, alaros
      character(len=80) inifil, sobfil, mesfil, oskfil
      character(len=20) c20   , tstart
      logical      active, first , ontere, kenter, flood , svkoflag
      double precision
     j             tsvkwt, tsvkwo, tsvkht, tsvkho, tsvkot, tsvkoo,
     j             tclosw, tclosh, tcloso, tendst, timtop, tstartdd,
     j             tendst_os
      real(dp)       timediff
      save         active, first , ontere, kenter, flood , svkoflag
      save         rotter, dordre, hmax_hvh
      save         gpsvkw, gpsvkh, gpsvko, gprott, gpdord, gpmamo, 
     j             gproom, peilos, alaros
      save         tsvkwt, tsvkwo, tsvkht, tsvkho, tsvkot, tsvkoo
      save         timtop, qtab_os, htab_os
      data io_ini,io_sob,io_mes,io_osk /131,132,133,134/
      data inifil /'mhwproc.ini'/
      data sobfil /'ivbdos.ini'/
      data mesfil /'ivbcls.log'/
      data oskfil /'osk.dat'/
      data active /.false./
      data flood  /.false./
      data svkoflag /.false./
      data first  /.true./
      data tsvkwt, tsvkwo, tsvkht, tsvkho, tsvkot, tsvkoo /6*-999D0/
      data tendst, tendst_os /2*-999D0/
      data hmax_hvh /-9999./

c----------------------------------------------------------------------c
c     Actions in first call:
c----------------------------------------------------------------------c

      if ( first ) then
          open ( io_mes , file = mesfil )
          open ( io_sob , file = sobfil , status = 'old' , err = 10 )
          goto 20

c         Not an IVBDOS run
   10     continue
          write (io_mes,*) ' Not an IVBDOS run '
          ivbact = .false.
          makhmx = .false.
          active = .false.
          first = .false.
          close (io_mes)
          return

   20     continue
          ivbact = .true.

          makhmx = .true.
          write (io_mes,*) ' IVBDOS run: MAKHMX = ', makhmx

c         Check whether closure should be evaluated
c         If not RETURN and make sure that all subsequent calls are void

          call gkwini ( io_sob , 'Sobek' , 'ZoekSluitMoment', c20 )
          read ( c20 , '(i20)' , err=902 ) ihulp
          if ( ihulp .eq. 0 ) then
              close (io_sob)
              first = .false.
              write (io_mes,*) ' IVBDOS run: ACTIVE = ', active
              close (io_mes)
              return
          else
              active = .true.
              write (io_mes,*) ' IVBDOS run: ACTIVE = ', active
          endif

c----------------------------------------------------------------------c
c         READ SPECIFIC INPUT FROM MHWp
c----------------------------------------------------------------------c

          call gkwini ( io_sob , 'Sobek' , 'OnterechtSluiten' , c20 )
          read ( c20 , '(i20)' , err=903 ) ihulp
          if ( ihulp .eq. 0 ) then
              ontere = .false.
          else
              ontere = .true.
          endif
          write (io_mes,*) ' IVBDOS run: ONTERE = ', ontere

          call gkwini ( io_sob , 'Sobek' , 'QLobith' , c20 )
          read ( c20 , '(f20.0)' , err=904 ) qlobit
          if ( qlobit .gt. 5999.5 ) then
              kenter = .true.
          else
              kenter = .false.
          endif
          write (io_mes,*) ' IVBDOS run: KENTER = ', kenter

          call gkwini ( io_sob , 'Sobek' , 'SluitpeilRotterdam', c20 )
          read ( c20 , '(f20.0)' , err=905 ) rotter
          call gkwini ( io_sob , 'Sobek' , 'SluitpeilDordrecht', c20 )
          read ( c20 , '(f20.0)' , err=906 ) dordre
          write (io_mes,*) ' Sluitpeil Rotterdam ', rotter
          write (io_mes,*) ' Sluitpeil Dordrecht ', dordre

          call gkwini ( io_sob , 'Sobek' , 'TStart', tstart )
          write (io_mes,*) ' TStart ', tstart

c         Read last time for onterecht sluiten
          call gkwini ( io_sob , 'Sobek' , 'TOnterechtSluiten', c20)
          write (io_mes,*) ' TOnterechtsluiten ', c20
          timtop = timediff(tstart,c20)

          close ( io_sob )
          
c----------------------------------------------------------------------c
c         READ SPECIFIC INPUT FROM ini file
c----------------------------------------------------------------------c

          open ( io_ini , file = inifil , status = 'old' , err = 900 )

          call locate (io_ini, 'Sobek' , 
     j                 'SVKWBranch', 'SVKWDistance', gpsvkw, 
     j                 gridnm, ngrid, branch, nbran, x     , rtncod)
          if ( rtncod .ne. 0 ) goto 915
          write (io_mes,*) ' GP SVKW: ', gpsvkw
         
          call locate (io_ini, 'Sobek' , 
     j                 'SVKHBranch', 'SVKHDistance', gpsvkh,
     j                 gridnm, ngrid, branch, nbran, x     , rtncod)
          if ( rtncod .ne. 0 ) goto 916
          write (io_mes,*) ' GP SVKH: ', gpsvkh

          call locate (io_ini, 'Sobek' , 
     j                 'SVKOBranch', 'SVKODistance', gpsvko,
     j                 gridnm, ngrid, branch, nbran, x     , rtncod)
          if ( rtncod .ne. 0 ) gpsvko = -999
          write (io_mes,*) ' GP SVKO: ', gpsvko

          call locate (io_ini, 'Sobek' , 
     j                 'RotterdamBranch', 'RotterdamDistance', gprott,
     j                 gridnm, ngrid, branch, nbran, x     , rtncod)
          if ( rtncod .ne. 0 ) goto 917
          write (io_mes,*) ' GP Rott: ', gprott

          call locate (io_ini, 'Sobek' , 
     j                 'DordrechtBranch', 'DordrechtDistance', gpdord,
     j                 gridnm, ngrid, branch, nbran, x     , rtncod)
          if ( rtncod .ne. 0 ) goto 918
          write (io_mes,*) ' GP Dord: ', gpdord

          call locate (io_ini, 'Sobek' , 
     j                 'MaasmondBranch', 'MaasmondDistance', gpmamo,
     j                 gridnm, ngrid, branch, nbran, x     , rtncod)
          if ( rtncod .ne. 0 ) goto 919
          write (io_mes,*) ' GP Mamo: ', gpmamo
          
          call locate (io_ini, 'Sobek' , 
     j                 'RoompotBranch', 'RoompotDistance', gproom,
     j                 gridnm, ngrid, branch, nbran, x     , rtncod)
          if ( rtncod .ne. 0 ) gproom = -999
          write (io_mes,*) ' GP Room: ', gproom
          
          call gkwini ( io_ini , 'Sobek' , 'SluitPeilOS', c20 )
          if ( c20 .ne. ' ' ) then
              read ( c20 , '(f20.0)' , err=910 ) peilos
          else
              peilos = 0.00
          endif
          write (io_mes,*) ' OS sluit op: ', peilos
          call gkwini ( io_ini , 'Sobek' , 'AlarmPeilOS', c20 )
          if ( c20 .ne. ' ' ) then
              read ( c20 , '(f20.0)' , err=911 ) alaros
          else
              alaros = 0.00
          endif
          write (io_mes,*) ' OS sluit bij verwacht peil Roompot: ',
     j                     alaros

          close ( io_ini )

c          if ( gpsvko .gt. 0 ) then

c             Read doorvoer data
              open ( io_osk, file = oskfil )
              nosdat = 0
              read ( io_osk, * , end = 30 )
   25             read ( io_osk, * , end = 30 ) qin, hin
                  nosdat = nosdat + 1
                  if ( nosdat .gt. nosdam ) goto 920
                  qtab_os(nosdat) = qin
                  htab_os(nosdat) = hin
                  goto 25
   30         continue    
              close ( io_osk )
              if ( qlobit .le. qtab_os(1) ) then
                  hcrit = htab_os(1) 
              elseif ( qlobit .ge. qtab_os(nosdat) ) then
                  hcrit = htab_os(nosdat) 
              else
                  do i=2,nosdat
                      if ( qlobit .le. qtab_os(i) ) then
                          hcrit = htab_os(i-1) 
     j                          +  (qlobit    -qtab_os(i-1))
     j                            /(qtab_os(i)-qtab_os(i-1))
     j                            *(htab_os(i)-htab_os(i-1))
                          goto 123
                      endif
                  enddo
  123             continue
              endif       
              write (io_mes,*) ' Critical level for doorvoer: ', hcrit
c          endif
          
c          close ( io_mes )

c         Write default file
          open ( io_sob , file = sobfil )
          close ( io_sob , status = 'delete' )
          open ( io_sob , file = sobfil )
          write ( io_sob , '(''[Sobek]'')' )
          c20 = 'undefined'
          write ( io_sob , '(''SluitMomentSVKH = '',a20)' ) c20
          write ( io_sob , '(''SluitMomentSVKW = '',a20)' ) c20
          write ( io_sob , '(''SluitMomentSVKO = '',a20)' ) c20
          write ( io_sob , '(''D_SluitMomentSVKO = '',a20)' ) c20
          write ( io_sob , '(''EindeStormvloed = '',a20)' ) c20
          write ( io_sob , '(''OSEindeStormvloed = '',a20)' ) c20
          write ( io_sob , '(''TDDStart = '',a20)' ) c20
          c20 = 'no'
          write ( io_sob , '(''Doorvoer = '',a20)' ) c20
          close ( io_sob )

c----------------------------------------------------------------------c
c         End actions in first call
c----------------------------------------------------------------------c

      endif

      if ( active ) then
c----------------------------------------------------------------------c
c         ACTIONS IN SIMULATION TIME STEPS
c----------------------------------------------------------------------c

c         Initialise restart flag

          wrires = .false.

c         Check for exceedance of alarm levels, if Yes set flag
c         keep track of end of storm

          if ( h(gprott) .gt. rotter .or.
     j         h(gpdord) .gt. dordre       ) then
c              write ( io_mes , * ) 'flood   at ',time
              flood  = .true.
              tendst = time
          else
c              flood = .false.
c              write ( io_mes , * ) 'noflood at ',time
          endif

c         SVKO, alarmpeil +3.00 bij Roompot buiten
          if ( gpsvko .ne. -999 .and. gproom .ne. -999 ) then
              if ( h(gproom) .gt. alaros ) then
                  svkoflag  = .true.
                  tendst_os = time
c      write ( io_mes , * ) h(gproom),alaros,' SVKO flood   at ',time
              else
c      write ( io_mes , * ) h(gproom),alaros,' SVKO noflood at ',time
              endif
          endif

c         Calculate maxlevel HvH
          hmax_hvh = max(hmax_hvh,real(h(gpmamo)))

c----------------------------------------------------------------------c
c         For kentering mode, not in first timestep
c----------------------------------------------------------------------c

          if ( kenter .and. .not.first ) then
c             SVKW
              call check_structure (gpsvkw,time,timtop,flood,
     j                              q1,q,tsvkwo,tsvkwt,wrires)
c             SVKH
              call check_structure (gpsvkh,time,timtop,flood,
     j                              q1,q,tsvkho,tsvkht,wrires)
          endif

c----------------------------------------------------------------------c
c         For peil mode
c----------------------------------------------------------------------c
          if ( .not.kenter ) then
c             Check for peil SVKW, if Yes set time flags
              if ( h(gpsvkw) .gt. 2.00 .and. tsvkwt .lt. 0.0 ) then
                  tsvkwt = time
                  tsvkwo = time
                  wrires = .true.
              endif
c             Check for peil SVKH, if Yes
              if ( h(gpsvkh) .gt. 2.00 .and. tsvkht .lt. 0.0 ) then
                  tsvkht = time
                  tsvkho = time
                  wrires = .true.
              endif
          endif
          
c----------------------------------------------------------------------c
c             SVKO, sluit op laatste waterstand < grenswaarde,
c                   voorafgaand aan bereiken alarmpeil (svkoflag),
c                   of voorafgaand aan TOnterechtsluiten (piek storm)
c----------------------------------------------------------------------c
          if ( gpsvko .ne. -999 ) then
              if ( h(gpsvko) .lt. peilos ) then

c                 Sluiten t.b.v. veiligheid OS
                  if ( .not. svkoflag ) then
c                     set POTENTIAL time for sluiten
                      tsvkot = time
                      wrires = .true.
                  endif

c                 Sluiten t.b.v. doorvoer NDB (mogelijk "onterecht")
                  if ( time.le.timtop ) then
c                     set POTENTIAL time for sluiten
                      tsvkoo = time
                      wrires = .true.
                  endif
              endif
          endif
      endif

c----------------------------------------------------------------------c
c     Actions after STORM
c     We have to do that every time, since we do not know the form of 
c     the storm
c     Alternative solution: retrieve end time and write only at
c     end of simulation (see XXX)
c----------------------------------------------------------------------c

c     XXX test on end of simulation
      if ( time .gt. timtop .and. active ) then
          open ( io_sob , file = sobfil )
          write ( io_sob , '(''[Sobek]'')' )

c         Initialiseer sluitmomenten

          tclosw = -999d0
          tclosh = -999d0
          tcloso = -999d0
          tstartdd = -999d0

c         SVKW en SVKH
 
          if ( flood ) then
c             Terecht sluiten
              if ( tsvkwt .gt. 0d0 ) tclosw = tsvkwt
              if ( tsvkht .gt. 0d0 ) tclosh = tsvkht
          else
c             Onterecht sluiten
              if ( ontere ) then
                  if ( tsvkwo .gt. 0d0 ) tclosw = tsvkwo
                  if ( tsvkho .gt. 0d0 ) tclosh = tsvkho
              endif
          endif

c         Ze sluiten altijd allebei, SVKH nooit later dan SVKW 
c         (H. den Deugd, pers. comm.)

          if ( tclosw .gt. 0d0 .and. tclosh .le. 0d0 ) tclosh = tclosw
          if ( tclosh .gt. 0d0 .and. tclosw .le. 0d0 ) tclosw = tclosh
          if ( tclosh .gt. tclosw ) tclosh = tclosw

c         Oosterscheldekering (SVKO)

c         Sluiten omwille van veiligheid OS, alleen als SVKOFLAG = true
          if ( tsvkot .gt. 0d0 .and. .not.svkoflag ) 
     j    tsvkot = -999D0
c         Sluiten omwille van de doorvoer, alleen als voldaan is aan crit
          if ( tsvkoo .gt. 0d0 .and. hmax_hvh .le. hcrit ) 
     j    tsvkoo = -999D0
c         Bepaal vroegste positieve moment
          if ( tsvkot .gt. 0d0 .and. tsvkoo .gt. 0d0 ) then
              tcloso = min(tsvkot,tsvkoo)
          elseif ( tsvkot .gt. 0d0 ) then
              tcloso = tsvkot
          elseif ( tsvkoo .gt. 0d0 ) then
              tcloso = tsvkoo
          endif

c         Bepaal begin DD simulatie

          tstartdd = tclosh
          if ( tcloso .gt. 0d0 .and. tcloso .lt. tclosh ) 
     j         tstartdd = tcloso

c         Zet alles om in datum/tijd, en schrijf het weg

          if ( tclosh .ge. 0d0 ) then
              call makdat ( tclosh , tstart , c20 )
          else
              c20 = 'undefined'
          endif
          write ( io_sob , '(''SluitMomentSVKH = '',a20)' ) c20

          if ( tclosw .ge. 0d0 ) then
              call makdat ( tclosw , tstart , c20 )
          else
              c20 = 'undefined'
          endif
          write ( io_sob , '(''SluitMomentSVKW = '',a20)' ) c20

          if ( tendst .gt. 0d0 ) then
              call makdat ( tendst , tstart , c20 )
          else
              tendst = timtop + 2.5d0*3600d0
              call makdat ( tendst , tstart , c20 )
          endif
          write ( io_sob , '(''EindeStormvloed = '',a20)' ) c20

          if ( tsvkot .gt. 0d0 ) then
              call makdat ( tsvkot , tstart , c20 )
          else
              c20 = 'undefined'
          endif
          write ( io_sob , '(''SluitMomentSVKO = '',a20)' ) c20

          if ( tsvkoo .gt. 0d0 ) then
              call makdat ( tsvkoo , tstart , c20 )
          else
              c20 = 'undefined'
          endif
          write ( io_sob , '(''D_SluitMomentSVKO = '',a20)' ) c20

          if ( tendst_os .gt. 0d0 ) then
              call makdat ( tendst_os , tstart , c20 )
          else
              c20 = 'undefined'
          endif
          write ( io_sob , '(''OSEindeStormvloed = '',a20)' ) c20

          if ( tstartdd .gt. 0d0 ) then
              call makdat ( tstartdd , tstart , c20 )
          else
              c20 = 'undefined'
          endif
          write ( io_sob , '(''TDDStart = '',a20)' ) c20

          if ( hmax_hvh .gt. hcrit ) then
              c20 = 'yes'
          else
              c20 = 'no'
          endif
          write ( io_sob , '(''Doorvoer = '',a20)' ) c20

          close ( io_sob )
c         XXX remove
c          active = .false.

c     End actions after peak of storm
      endif

      first = .false.
      return

  900 stop 'IVBCLS: INI file not found'
c 901 stop 'IVBCLS: SOB file not found'
  902 stop 'IVBCLS: Error reading ZoekSluitMoment'
  903 stop 'IVBCLS: Error reading OnterechtSluiten'
  904 stop 'IVBCLS: Error reading QLobith'
  905 stop 'IVBCLS: Error reading HRotterdam'
  906 stop 'IVBCLS: Error reading HDordrecht'
c 907 stop 'IVBCLS: Error reading MaximumWaterstanden'
  908 stop 'IVBCLS: Error reading locations'
  909 stop 'IVBCLS: Error reading StormopzetDuur'
  910 stop 'IVBCLS: Error reading SluitPeilOS'
  911 stop 'IVBCLS: Error reading AlarmPeilOS'
  912 stop 'IVBCLS: Branch name not found'
  913 stop 'IVBCLS: Grid point not found'
  914 stop 'IVBCLS: Error reading Faseverschuiving'
  915 stop 'IVBCLS: SVKW not found'
  916 stop 'IVBCLS: SVKH not found'
  917 stop 'IVBCLS: Rotterdam not found'
  918 stop 'IVBCLS: Dordrecht not found'
  919 stop 'IVBCLS: Maasmond not found'
  920 stop 'IVBCLS: Local array space for OSkering exceeded'

      end

      subroutine fingp ( branch, nbran, ibran , x     , loca , igp )
      integer      igp   , ibran , nbran
      integer      branch(4,nbran)
      real         loca  , x(*)

      integer      ig
      real         dist  , distm

      distm = 1e30
      do 50 ig = branch(3,ibran) , branch(4,ibran)
          dist = abs(loca-x(ig))
          if ( dist .lt. distm ) then
              igp = ig
              distm = dist
          endif
   50 continue

      return
      end

      subroutine finibr ( brannm, gridnm, ngrid , branch,
     j                    nbran , ibran )
      integer      ngrid , nbran , ibran
      integer      branch(4,nbran)
      character(len=20) brannm
      character(len=40) gridnm(ngrid)

c     Deze subroutine probeert de naam van een branch af te leiden
c     uit de naam van het eerste gridpunt op de branch
c     De routine was niet in orde, in die zin dat de wijze  
c     om de taknaam van de KM te scheiden alleen werkte bij
c     modellen aangemaakt met Sobek v1.xx en NIET voor v2.xx

c     Deze versie beoogt dat te verbeteren
c     We zoeken nu naar de LAATSTE underscore
c     en we zorgen dat er iets crasht als het fout gaat

c     JvG, Okt 2004
c     De vorige versie beoogde zowel oud als nieuw te ondersteunen,
c     maar dat lukte niet.
c     Omdat de oude SOBEK in middels wel uitgefaseerd is, gaan we gewoon op
c     nieuw over!

      integer      i     , igp   , ihulp1, ihulp2, ihulp
      character(len=20) locnam

c      write (133,'(''Looking for '',a)') brannm
      ibran = -1

c     Loop over branches

      do 100 i = 1,nbran

c         Derive branch name from first gridpoint

          igp = branch(3,i)
c          write (133,'(''GripNA '',a)') gridnm(igp)

c         Oud model
c          ihulp1 = index ( gridnm(igp) , '_0' )
          ihulp1 = -1

c         Nieuw model 
          ihulp2 = index ( gridnm(igp) , '_.0' )

          if ( ihulp1 .le. 1 .and. ihulp2 .le. 1 )
     j    stop 'FINIBR: unable to identify branch name from gridp'
          if ( ihulp1 .le. 1 ) ihulp1 = 99
          if ( ihulp2 .le. 1 ) ihulp2 = 99
          ihulp = min ( ihulp1,ihulp2 )
          ihulp = min ( ihulp-1 , 20 )
          locnam = ' '
          locnam(1:ihulp) = gridnm(igp)(1:ihulp)

c          write (133,'(''Branch '',i4,'' '',a20,i4)') i,locnam,ihulp

c         Check agreement

          if ( locnam(1:ihulp) .eq. brannm(1:ihulp) ) then
              ibran = i
c              write (133,'(''Match!'')')
              return
          endif
c          write (133,'(''No match!'')')
  100 continue

      return
      end

      subroutine makdat ( time , instri , outstr )
      character(len=20)   instri , outstr
      double precision    time

      integer             year,month,day,hour,minute,sec
      integer             ihulp

      read ( instri(1:19),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)',
     j       err=910) year,month,day,hour,minute,sec
      ihulp = nint(time)
      day = day + ihulp/86400
c     write (*,*) ' days added ',ihulp/86400
      ihulp = ihulp - (ihulp/86400)*86400
      hour = hour + ihulp/3600
c     write (*,*) ' hours added ',ihulp/3600
      ihulp = ihulp - (ihulp/3600)*3600
      minute = minute + ihulp/60
c     write (*,*) ' minutes added ',ihulp/60
      ihulp = ihulp - (ihulp/60)*60
      sec = sec + ihulp
c     write (*,*) ' seconds added ',ihulp
      if (sec .ge. 60 ) then
          sec = sec - 60
          minute = minute + 1
      elseif (sec .lt. 0 ) then
          sec = sec + 60
          minute = minute - 1
      endif
      if (minute .ge. 60 ) then
          minute = minute - 60
          hour = hour + 1
      elseif (minute .lt. 0 ) then
          minute = minute + 60
          hour = hour - 1
      endif
      if (hour .ge. 24) then
          hour= hour- 24
          day = day + 1
      elseif (hour .lt. 0) then
          hour= hour+ 24
          day = day - 1
      endif
      if ( day .ge. 29 .or. day .lt. 0 ) goto 911
      write ( outstr,1000) year,month,day,hour,minute,sec
      return
  910 stop 'IVBCLS: Error parsing date time string'
  911 stop 'IVBCLS: Date and time conversion invalid'
 1000 format (i4.4,'/',i2.2,'/',i2.2,';',i2.2,':',i2.2,':',i2.2)
      end

      subroutine locate (io_ini, section, branchname, distancename,
     j                   gp, gridnm, ngrid, branch, nbran,
     j                   x, rtncod)
     
      integer      io_ini, ngrid , gp    , nbran , rtncod
      integer      branch(4,nbran)
      real         x(ngrid)
      character(len=*) section, branchname, distancename
      character(len=40) gridnm(ngrid)
      
      character(len=20) c20
      integer      ibran
      real         dist
      
      rtncod = 1
      
      call gkwini ( io_ini , section , branchname , c20 )
      if ( c20 .eq. '' ) return
c      write (133,*) branchname,' = ',c20
      call finibr ( c20, gridnm, ngrid, branch, nbran, ibran)
      if ( ibran .le. 0 ) return
c      write (133,*) 'ibran = ',ibran
      call gkwini ( io_ini , section , distancename , c20 )
      read ( c20 , '(f20.0)' , err=999 ) dist
c      write (133,*) distancename,' = ',dist
      call fingp ( branch, nbran, ibran , x     , dist , gp )
      if ( gp .le. 0 ) return
c      write (133,*) 'gp = ',gp

      rtncod = 0

  999 return
      end
      
      
      subroutine check_structure (gp,time,timtop,flood,
     j                            q1,q,tsvkwo,tsvkwt,wrires)

      integer          gp
      double precision q(*),q1(*)
      double precision time, timtop,tsvkwo,tsvkwt
      logical          flood, wrires

      if ( q1(gp).ge.0.0 .and. q(gp).lt.0.0 ) then

c         if before top of storm (T=0 is start of storm!!)

          if ( time .le. timtop ) then
c             set time for "onterecht sluiten"
              tsvkwo = time
              wrires = .true.
          endif

c         if no "storm surge": find last kentering before surge

          if ( .not. flood ) then
c             set time for "terecht sluiten"
              tsvkwt = time
              wrires = .true.
          endif
      endif
      return
      end

      function timediff (date1, date2)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Parse New Model Database (V2.0)
c
c Programmer:         P.R. Evans
c
c Module:             Calculate difference in date
c
c Module description: This function calculates the difference between
c                     the start date/time and the end date/time for
c                     sobek, including leap years. The result is
c                     returned as a double precision real value in
c                     seconds. The date/time inputs are read as
c                     character strings
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c
c***********************************************************************
c
      implicit none
c
c local variables
      double precision timediff
      character(len=*)     date1, date2
c      integer          convert_date
      integer          aday(12), nyear, nleap, nday, endday, startday,
     &                 year1, month1, day1, hour1, min1, sec1, hun1,
     &                 year2, month2, day2, hour2, min2, sec2, hun2,
     &                 iyear, err
      real             endtime, starttime
c
      data             aday /0, 31, 59, 90, 120, 151, 181, 212, 243,
     &                       273, 304, 334/

c     if ( date1 .eq. ' ' .or. date2 .eq. ' ' )
c    jcall generate_message(3,'Illegal date in Timediff module',
c    j                      ' ',' ',0)
c
c initialise variables
c     nyear = 0
      nleap = 0
      nday = 0
      endday = 0
      startday = 0
      endtime = 0
      starttime = 0
      timediff = 0D0
c
c first convert date/time string to integer values
      call readat (date1(1:19),year1,month1,day1,hour1,min1,sec1,err)
      if ( err .ne. 0 ) goto 901
      hun1 = 0
      call readat (date2(1:19),year2,month2,day2,hour2,min2,sec2,err)
      if ( err .ne. 0 ) goto 902
      hun2 = 0
c
c calculate number of years and then number of leap years
c year including end date is added later
      nyear = year2 - year1
c
      do 10 iyear=year1, year2-1
        if ((MOD (iyear, 4) .eq. 0
     &     .and. MOD (iyear, 100) .ne. 0)
     &     .or. MOD (iyear, 400) .eq. 0) then
          nleap = nleap + 1
        endif
 10   continue
c
c now determine number of days in start year
      startday = aday(month1) + day1
c
c is start year a leap year and month after february?
      if (month1 .gt. 2
     &    .and. ((MOD (year1, 4) .eq. 0
     &    .and. MOD (year1, 100) .ne. 0)
     &    .or. MOD (year1, 400) .eq. 0)) then
        startday = startday + 1
      endif
c
c now determine number of days in end year
      endday = aday(month2) + day2
c
c is end year a leap year and month after february?
      if (month2 .gt. 2
     &    .and. ((MOD (year2, 4) .eq. 0
     &    .and. MOD (year2, 100) .ne. 0)
     &    .or. MOD (year2, 400) .eq. 0)) then
        endday = endday + 1
      endif
c
c calculate total number of days
      nday = nyear*365 + nleap - startday + endday
c
c calculate difference in times (exclude date)
      starttime = hour1*3600 + min1*60 + sec1 + hun1/100
      endtime = hour2*3600 + min2*60 + sec2 + hun2/100
      timediff = endtime - starttime
c
c add up total time difference
      timediff = nday*86400 + timediff
c
      return
  901 write (*,*) date1
      write (*,*) 'illegal date/time format encountered'
      read (*,*)
      stop
  902 write (*,*) date2
      write (*,*) 'illegal date/time format encountered'
      read (*,*)
      stop
      end

      subroutine readat (dattim,year,month,day,hour,minute,sec,err)
      character(len=*) dattim
      integer year,month,day,hour,minute,sec,err
      err = 0
      read (dattim,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)',err=910)
     j        year,month,day,hour,minute,sec
      return
  910 read (dattim,'(i2,1x,i2,1x,i4,1x,i2,1x,i2,1x,i2)',err=920)
     j        day,month,year,hour,minute,sec
      return
  920 err = 1
      return
      end
