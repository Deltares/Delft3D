subroutine IVBCLS(time   ,timep  ,ngrid  ,nbran  ,branch ,&
&x      ,gridnm ,q      ,h      ,q1     )
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Flow Module
!
! Programmer:
!
! Module:             IVBCLS (                                )
!
! Module description:
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  1 time              I  Actual time level (at t=n+1) in sec.
!  2 timep             I  Previous time level (at t=n) in sec.
!  3 ngrid             I  Number of grid points in network.
!  4 nbran             I  Number of branches.
!  5 branch(4,nbran)   I  Branch information:
!                         (1,i) = Node number n1 at begin of branch i.
!                         (2,i) = Node number n2 at end of branch i.
!                         (3,i) = Grid point i1 at begin of branch i.
!                         (4,i) = Grid point i2 at end of branch i.
!  6 x(ngrid)          I  Coordinates in every grid point.
!  7 q(ngrid)          I  Discharge in every grid point at t=n+1
!                         (TIME).
!  8 h(ngrid)          I  Water level in every grid point at t=n+1
!                         (TIME).
!  9 q1(ngrid)         I  Discharge in every grid point at t=n
!                         (TIMEP).
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: ivbcls.pf,v $
! Revision 1.2  1999/03/17  14:22:46  kuipe_j
! correctie
!
! Revision 1.1  1999/03/15  14:30:13  kuipe_j
! IVBDOS
!
!
!***********************************************************************
!
!     Declaration of Parameters:
!
   integer          ngrid           ,nbran
   integer          branch(4,nbran)
   real             x     (ngrid)
   double precision q (ngrid)  ,h(ngrid)   ,q1(ngrid)
   double precision time            ,timep
   character*40     gridnm(ngrid)
!
!     Common block for communication with Sobek
!
!     NAME              IO DESCRIPTION
!     ivbact            O  Flag to indicate MHW processor run
!     makhmx            O  Flag to make Sobek write max levels
!     wrires            O  Flag to make Sobek write restart moment
!     they are initialised .false. by Sobek
   logical         ivbact, makhmx, wrires
   common /ivbdos/ ivbact, makhmx, wrires
!
!     Declaration of local variables:
!
   integer      io_ini, io_sob, ihulp , io_mes, nosdat,&
   &nosdam, io_osk, i
   parameter   (nosdam=99)
   real         qtab_os(nosdam), htab_os(nosdam)
   integer      gpsvkw, gpsvkh, gprott, gpdord, gpmamo,&
   &rtncod, gpsvko, gproom
   real         qlobit, rotter, dordre, hmax_hvh, hcrit ,&
   &hin   , qin   , peilos, alaros
   character*80 inifil, sobfil, mesfil, oskfil
   character*20 c20   , tstart
   logical      active, first , ontere, kenter, flood , svkoflag
   double precision&
   &tsvkwt, tsvkwo, tsvkht, tsvkho, tsvkot, tsvkoo,&
   &tclosw, tclosh, tcloso, tendst, timtop, tstartdd,&
   &tendst_os
   real*8       timediff
   save         active, first , ontere, kenter, flood , svkoflag
   save         rotter, dordre, hmax_hvh
   save         gpsvkw, gpsvkh, gpsvko, gprott, gpdord, gpmamo,&
   &gproom, peilos, alaros
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

!----------------------------------------------------------------------c
!     Actions in first call:
!----------------------------------------------------------------------c

   if ( first ) then
      open ( io_mes , file = mesfil )
      open ( io_sob , file = sobfil , status = 'old' , err = 10 )
      goto 20

!         Not an IVBDOS run
10    continue
      write (io_mes,*) ' Not an IVBDOS run '
      ivbact = .false.
      makhmx = .false.
      active = .false.
      first = .false.
      close (io_mes)
      return

20    continue
      ivbact = .true.

      makhmx = .true.
      write (io_mes,*) ' IVBDOS run: MAKHMX = ', makhmx

!         Check whether closure should be evaluated
!         If not RETURN and make sure that all subsequent calls are void

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

!----------------------------------------------------------------------c
!         READ SPECIFIC INPUT FROM MHWp
!----------------------------------------------------------------------c

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

!         Read last time for onterecht sluiten
      call gkwini ( io_sob , 'Sobek' , 'TOnterechtSluiten', c20)
      write (io_mes,*) ' TOnterechtsluiten ', c20
      timtop = timediff(tstart,c20)

      close ( io_sob )

!----------------------------------------------------------------------c
!         READ SPECIFIC INPUT FROM ini file
!----------------------------------------------------------------------c

      open ( io_ini , file = inifil , status = 'old' , err = 900 )

      call locate (io_ini, 'Sobek' ,&
      &'SVKWBranch', 'SVKWDistance', gpsvkw,&
      &gridnm, ngrid, branch, nbran, x     , rtncod)
      if ( rtncod .ne. 0 ) goto 915
      write (io_mes,*) ' GP SVKW: ', gpsvkw

      call locate (io_ini, 'Sobek' ,&
      &'SVKHBranch', 'SVKHDistance', gpsvkh,&
      &gridnm, ngrid, branch, nbran, x     , rtncod)
      if ( rtncod .ne. 0 ) goto 916
      write (io_mes,*) ' GP SVKH: ', gpsvkh

      call locate (io_ini, 'Sobek' ,&
      &'SVKOBranch', 'SVKODistance', gpsvko,&
      &gridnm, ngrid, branch, nbran, x     , rtncod)
      if ( rtncod .ne. 0 ) gpsvko = -999
      write (io_mes,*) ' GP SVKO: ', gpsvko

      call locate (io_ini, 'Sobek' ,&
      &'RotterdamBranch', 'RotterdamDistance', gprott,&
      &gridnm, ngrid, branch, nbran, x     , rtncod)
      if ( rtncod .ne. 0 ) goto 917
      write (io_mes,*) ' GP Rott: ', gprott

      call locate (io_ini, 'Sobek' ,&
      &'DordrechtBranch', 'DordrechtDistance', gpdord,&
      &gridnm, ngrid, branch, nbran, x     , rtncod)
      if ( rtncod .ne. 0 ) goto 918
      write (io_mes,*) ' GP Dord: ', gpdord

      call locate (io_ini, 'Sobek' ,&
      &'MaasmondBranch', 'MaasmondDistance', gpmamo,&
      &gridnm, ngrid, branch, nbran, x     , rtncod)
      if ( rtncod .ne. 0 ) goto 919
      write (io_mes,*) ' GP Mamo: ', gpmamo

      call locate (io_ini, 'Sobek' ,&
      &'RoompotBranch', 'RoompotDistance', gproom,&
      &gridnm, ngrid, branch, nbran, x     , rtncod)
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
      write (io_mes,*) ' OS sluit bij verwacht peil Roompot: ',&
      &alaros

      close ( io_ini )

!          if ( gpsvko .gt. 0 ) then

!             Read doorvoer data
      open ( io_osk, file = oskfil )
      nosdat = 0
      read ( io_osk, * , end = 30 )
25    read ( io_osk, * , end = 30 ) qin, hin
      nosdat = nosdat + 1
      if ( nosdat .gt. nosdam ) goto 920
      qtab_os(nosdat) = qin
      htab_os(nosdat) = hin
      goto 25
30    continue
      close ( io_osk )
      if ( qlobit .le. qtab_os(1) ) then
         hcrit = htab_os(1)
      elseif ( qlobit .ge. qtab_os(nosdat) ) then
         hcrit = htab_os(nosdat)
      else
         do i=2,nosdat
            if ( qlobit .le. qtab_os(i) ) then
               hcrit = htab_os(i-1)&
               &+  (qlobit    -qtab_os(i-1))&
               &/(qtab_os(i)-qtab_os(i-1))&
               &*(htab_os(i)-htab_os(i-1))
               goto 123
            endif
         enddo
123      continue
      endif
      write (io_mes,*) ' Critical level for doorvoer: ', hcrit
!          endif

!          close ( io_mes )

!         Write default file
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

!----------------------------------------------------------------------c
!         End actions in first call
!----------------------------------------------------------------------c

   endif

   if ( active ) then
!----------------------------------------------------------------------c
!         ACTIONS IN SIMULATION TIME STEPS
!----------------------------------------------------------------------c

!         Initialise restart flag

      wrires = .false.

!         Check for exceedance of alarm levels, if Yes set flag
!         keep track of end of storm

      if ( h(gprott) .gt. rotter .or.&
      &h(gpdord) .gt. dordre       ) then
!              write ( io_mes , * ) 'flood   at ',time
         flood  = .true.
         tendst = time
      else
!              flood = .false.
!              write ( io_mes , * ) 'noflood at ',time
      endif

!         SVKO, alarmpeil +3.00 bij Roompot buiten
      if ( gpsvko .ne. -999 .and. gproom .ne. -999 ) then
         if ( h(gproom) .gt. alaros ) then
            svkoflag  = .true.
            tendst_os = time
!      write ( io_mes , * ) h(gproom),alaros,' SVKO flood   at ',time
         else
!      write ( io_mes , * ) h(gproom),alaros,' SVKO noflood at ',time
         endif
      endif

!         Calculate maxlevel HvH
      hmax_hvh = max(hmax_hvh,h(gpmamo))

!----------------------------------------------------------------------c
!         For kentering mode, not in first timestep
!----------------------------------------------------------------------c

      if ( kenter .and. .not.first ) then
!             SVKW
         call check_structure (gpsvkw,time,timtop,flood,&
         &q1,q,tsvkwo,tsvkwt,wrires)
!             SVKH
         call check_structure (gpsvkh,time,timtop,flood,&
         &q1,q,tsvkho,tsvkht,wrires)
      endif

!----------------------------------------------------------------------c
!         For peil mode
!----------------------------------------------------------------------c
      if ( .not.kenter ) then
!             Check for peil SVKW, if Yes set time flags
         if ( h(gpsvkw) .gt. 2.00 .and. tsvkwt .lt. 0.0 ) then
            tsvkwt = time
            tsvkwo = time
            wrires = .true.
         endif
!             Check for peil SVKH, if Yes
         if ( h(gpsvkh) .gt. 2.00 .and. tsvkht .lt. 0.0 ) then
            tsvkht = time
            tsvkho = time
            wrires = .true.
         endif
      endif

!----------------------------------------------------------------------c
!             SVKO, sluit op laatste waterstand < grenswaarde,
!                   voorafgaand aan bereiken alarmpeil (svkoflag),
!                   of voorafgaand aan TOnterechtsluiten (piek storm)
!----------------------------------------------------------------------c
      if ( gpsvko .ne. -999 ) then
         if ( h(gpsvko) .lt. peilos ) then

!                 Sluiten t.b.v. veiligheid OS
            if ( .not. svkoflag ) then
!                     set POTENTIAL time for sluiten
               tsvkot = time
               wrires = .true.
            endif

!                 Sluiten t.b.v. doorvoer NDB (mogelijk "onterecht")
            if ( time.le.timtop ) then
!                     set POTENTIAL time for sluiten
               tsvkoo = time
               wrires = .true.
            endif
         endif
      endif
   endif

!----------------------------------------------------------------------c
!     Actions after STORM
!     We have to do that every time, since we do not know the form of
!     the storm
!     Alternative solution: retrieve end time and write only at
!     end of simulation (see XXX)
!----------------------------------------------------------------------c

!     XXX test on end of simulation
   if ( time .gt. timtop .and. active ) then
      open ( io_sob , file = sobfil )
      write ( io_sob , '(''[Sobek]'')' )

!         Initialiseer sluitmomenten

      tclosw = -999d0
      tclosh = -999d0
      tcloso = -999d0
      tstartdd = -999d0

!         SVKW en SVKH

      if ( flood ) then
!             Terecht sluiten
         if ( tsvkwt .gt. 0d0 ) tclosw = tsvkwt
         if ( tsvkht .gt. 0d0 ) tclosh = tsvkht
      else
!             Onterecht sluiten
         if ( ontere ) then
            if ( tsvkwo .gt. 0d0 ) tclosw = tsvkwo
            if ( tsvkho .gt. 0d0 ) tclosh = tsvkho
         endif
      endif

!         Ze sluiten altijd allebei, SVKH nooit later dan SVKW
!         (H. den Deugd, pers. comm.)

      if ( tclosw .gt. 0d0 .and. tclosh .le. 0d0 ) tclosh = tclosw
      if ( tclosh .gt. 0d0 .and. tclosw .le. 0d0 ) tclosw = tclosh
      if ( tclosh .gt. tclosw ) tclosh = tclosw

!         Oosterscheldekering (SVKO)

!         Sluiten omwille van veiligheid OS, alleen als SVKOFLAG = true
      if ( tsvkot .gt. 0d0 .and. .not.svkoflag )&
      &tsvkot = -999D0
!         Sluiten omwille van de doorvoer, alleen als voldaan is aan crit
      if ( tsvkoo .gt. 0d0 .and. hmax_hvh .le. hcrit )&
      &tsvkoo = -999D0
!         Bepaal vroegste positieve moment
      if ( tsvkot .gt. 0d0 .and. tsvkoo .gt. 0d0 ) then
         tcloso = min(tsvkot,tsvkoo)
      elseif ( tsvkot .gt. 0d0 ) then
         tcloso = tsvkot
      elseif ( tsvkoo .gt. 0d0 ) then
         tcloso = tsvkoo
      endif

!         Bepaal begin DD simulatie

      tstartdd = tclosh
      if ( tcloso .gt. 0d0 .and. tcloso .lt. tclosh )&
      &tstartdd = tcloso

!         Zet alles om in datum/tijd, en schrijf het weg

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
!         XXX remove
!          active = .false.

!     End actions after peak of storm
   endif

   first = .false.
   return

900 stop 'IVBCLS: INI file not found'
! 901 stop 'IVBCLS: SOB file not found'
902 stop 'IVBCLS: Error reading ZoekSluitMoment'
903 stop 'IVBCLS: Error reading OnterechtSluiten'
904 stop 'IVBCLS: Error reading QLobith'
905 stop 'IVBCLS: Error reading HRotterdam'
906 stop 'IVBCLS: Error reading HDordrecht'
! 907 stop 'IVBCLS: Error reading MaximumWaterstanden'
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

subroutine finibr ( brannm, gridnm, ngrid , branch,&
&nbran , ibran )
   integer      ngrid , nbran , ibran
   integer      branch(4,nbran)
   character*20 brannm
   character*40 gridnm(ngrid)

!     Deze subroutine probeert de naam van een branch af te leiden
!     uit de naam van het eerste gridpunt op de branch
!     De routine was niet in orde, in die zin dat de wijze
!     om de taknaam van de KM te scheiden alleen werkte bij
!     modellen aangemaakt met Sobek v1.xx en NIET voor v2.xx

!     Deze versie beoogt dat te verbeteren
!     We zoeken nu naar de LAATSTE underscore
!     en we zorgen dat er iets crasht als het fout gaat

!     JvG, Okt 2004
!     De vorige versie beoogde zowel oud als nieuw te ondersteunen,
!     maar dat lukte niet.
!     Omdat de oude SOBEK in middels wel uitgefaseerd is, gaan we gewoon op
!     nieuw over!

   integer      i     , igp   , ihulp1, ihulp2, ihulp
   character*20 locnam

!      write (133,'(''Looking for '',a)') brannm
   ibran = -1

!     Loop over branches

   do 100 i = 1,nbran

!         Derive branch name from first gridpoint

      igp = branch(3,i)
!          write (133,'(''GripNA '',a)') gridnm(igp)

!         Oud model
!          ihulp1 = index ( gridnm(igp) , '_0' )
      ihulp1 = -1

!         Nieuw model
      ihulp2 = index ( gridnm(igp) , '_.0' )

      if ( ihulp1 .le. 1 .and. ihulp2 .le. 1 )&
      &stop 'FINIBR: unable to identify branch name from gridp'
      if ( ihulp1 .le. 1 ) ihulp1 = 99
      if ( ihulp2 .le. 1 ) ihulp2 = 99
      ihulp = min ( ihulp1,ihulp2 )
      ihulp = min ( ihulp-1 , 20 )
      locnam = ' '
      locnam(1:ihulp) = gridnm(igp)(1:ihulp)

!          write (133,'(''Branch '',i4,'' '',a20,i4)') i,locnam,ihulp

!         Check agreement

      if ( locnam(1:ihulp) .eq. brannm(1:ihulp) ) then
         ibran = i
!              write (133,'(''Match!'')')
         return
      endif
!          write (133,'(''No match!'')')
100 continue

   return
end

subroutine makdat ( time , instri , outstr )
   character*20        instri , outstr
   double precision    time

   integer             year,month,day,hour,minute,sec
   integer             ihulp

   read ( instri(1:19),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)',&
   &err=910) year,month,day,hour,minute,sec
   ihulp = nint(time)
   day = day + ihulp/86400
!     write (*,*) ' days added ',ihulp/86400
   ihulp = ihulp - (ihulp/86400)*86400
   hour = hour + ihulp/3600
!     write (*,*) ' hours added ',ihulp/3600
   ihulp = ihulp - (ihulp/3600)*3600
   minute = minute + ihulp/60
!     write (*,*) ' minutes added ',ihulp/60
   ihulp = ihulp - (ihulp/60)*60
   sec = sec + ihulp
!     write (*,*) ' seconds added ',ihulp
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

subroutine locate (io_ini, section, branchname, distancename,&
&gp, gridnm, ngrid, branch, nbran,&
&x, rtncod)

   integer      io_ini, ngrid , gp    , nbran , rtncod
   integer      branch(4,nbran)
   real         x(ngrid)
   character*(*) section, branchname, distancename
   character*40 gridnm(ngrid)

   character*20 c20
   integer      ibran
   real         dist

   rtncod = 1

   call gkwini ( io_ini , section , branchname , c20 )
   if ( c20 .eq. '' ) return
!      write (133,*) branchname,' = ',c20
   call finibr ( c20, gridnm, ngrid, branch, nbran, ibran)
   if ( ibran .le. 0 ) return
!      write (133,*) 'ibran = ',ibran
   call gkwini ( io_ini , section , distancename , c20 )
   read ( c20 , '(f20.0)' , err=999 ) dist
!      write (133,*) distancename,' = ',dist
   call fingp ( branch, nbran, ibran , x     , dist , gp )
   if ( gp .le. 0 ) return
!      write (133,*) 'gp = ',gp

   rtncod = 0

999 return
end


subroutine check_structure (gp,time,timtop,flood,&
&q1,q,tsvkwo,tsvkwt,wrires)

   integer          gp
   double precision q(*),q1(*)
   double precision time, timtop,tsvkwo,tsvkwt
   logical          flood, wrires

   if ( q1(gp).ge.0.0 .and. q(gp).lt.0.0 ) then

!         if before top of storm (T=0 is start of storm!!)

      if ( time .le. timtop ) then
!             set time for "onterecht sluiten"
         tsvkwo = time
         wrires = .true.
      endif

!         if no "storm surge": find last kentering before surge

      if ( .not. flood ) then
!             set time for "terecht sluiten"
         tsvkwt = time
         wrires = .true.
      endif
   endif
   return
end

function timediff (date1, date2)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Parse New Model Database (V2.0)
!
! Programmer:         P.R. Evans
!
! Module:             Calculate difference in date
!
! Module description: This function calculates the difference between
!                     the start date/time and the end date/time for
!                     sobek, including leap years. The result is
!                     returned as a double precision real value in
!                     seconds. The date/time inputs are read as
!                     character strings
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
!
!***********************************************************************
!
   implicit none
!
! local variables
   double precision timediff
   character*(*)     date1, date2
!      integer          convert_date
   integer          aday(12), nyear, nleap, nday, endday, startday,&
   &year1, month1, day1, hour1, min1, sec1, hun1,&
   &year2, month2, day2, hour2, min2, sec2, hun2,&
   &iyear, err
   real             endtime, starttime
!
   data             aday /0, 31, 59, 90, 120, 151, 181, 212, 243,&
   &273, 304, 334/

!     if ( date1 .eq. ' ' .or. date2 .eq. ' ' )
!    jcall generate_message(3,'Illegal date in Timediff module',
!    j                      ' ',' ',0)
!
! initialise variables
!     nyear = 0
   nleap = 0
   nday = 0
   endday = 0
   startday = 0
   endtime = 0
   starttime = 0
   timediff = 0D0
!
! first convert date/time string to integer values
   call readat (date1(1:19),year1,month1,day1,hour1,min1,sec1,err)
   if ( err .ne. 0 ) goto 901
   hun1 = 0
   call readat (date2(1:19),year2,month2,day2,hour2,min2,sec2,err)
   if ( err .ne. 0 ) goto 902
   hun2 = 0
!
! calculate number of years and then number of leap years
! year including end date is added later
   nyear = year2 - year1
!
   do 10 iyear=year1, year2-1
      if ((MOD (iyear, 4) .eq. 0&
      &.and. MOD (iyear, 100) .ne. 0)&
      &.or. MOD (iyear, 400) .eq. 0) then
         nleap = nleap + 1
      endif
10 continue
!
! now determine number of days in start year
   startday = aday(month1) + day1
!
! is start year a leap year and month after february?
   if (month1 .gt. 2&
   &.and. ((MOD (year1, 4) .eq. 0&
   &.and. MOD (year1, 100) .ne. 0)&
   &.or. MOD (year1, 400) .eq. 0)) then
      startday = startday + 1
   endif
!
! now determine number of days in end year
   endday = aday(month2) + day2
!
! is end year a leap year and month after february?
   if (month2 .gt. 2&
   &.and. ((MOD (year2, 4) .eq. 0&
   &.and. MOD (year2, 100) .ne. 0)&
   &.or. MOD (year2, 400) .eq. 0)) then
      endday = endday + 1
   endif
!
! calculate total number of days
   nday = nyear*365 + nleap - startday + endday
!
! calculate difference in times (exclude date)
   starttime = hour1*3600 + min1*60 + sec1 + hun1/100
   endtime = hour2*3600 + min2*60 + sec2 + hun2/100
   timediff = endtime - starttime
!
! add up total time difference
   timediff = nday*86400 + timediff
!
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
   character*(*) dattim
   integer year,month,day,hour,minute,sec,err
   err = 0
   read (dattim,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)',err=910)&
   &year,month,day,hour,minute,sec
   return
910 read (dattim,'(i2,1x,i2,1x,i4,1x,i2,1x,i2,1x,i2)',err=920)&
   &day,month,year,hour,minute,sec
   return
920 err = 1
   return
end
