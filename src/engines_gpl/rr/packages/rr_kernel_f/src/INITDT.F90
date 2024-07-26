!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

 ! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 15-08-97 11:31a  $
!
! current revision: $Revision:: 3               $


      SUBROUTINE INITDT (GenerateNetCdfOutput)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.00                 Date: March 1995
! *********************************************************************
! *** Last update: January 21, 1997       By : Peter Schrier
! ***   conversion of dutch captions to english captions
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Initialiseer Mappix data
! ***
! *** Neem aan 14 maps:
! ***                1. verhardgebied
! ***                2. onverhardgebied
! ***                3. kasgebied
! ***                4. open water
! ***                5. structures
! ***                6. boundaries
! ***                7. pluvius knopen
! ***                8. alle knopen     (balans)
! ***                9. alle knopen (zout)
! ***               10. RWZI knopen
! ***               11. industrie knopen
! ***               12. Sacramento knopen
! ***               13.RR-links
! ***               14. cel
! ***               15. RRRunoff
! ***
! ***
! *** For kaart een aantal summary files/series: (afh. van de eenheden)
! ***
! *** I. verhard gebied:
! ***    1. maximale bergingsgraad riool (in %)
! ***    2. -maximum overstort debiet  (m3/s)
! ***       -maximum uitgemalen debiet naar open water of boundary (m3/s)
! ***       -maximum totaal uitgeslagen debiet naar openwater (m3/s)
! ***       -maximum totaal regenval (m3/s)
! ***    3. Detail bergingsgraad riool voor 1 event
! ***    4. Detail flows voor 1 event
! *** II. onverhard gebied:
! ***     1. -maximum oppervlakte afvoer (m3/s)
! ***        -maximum afvoer via bodem (m3/s)
! ***        -maximum regenval (m3/s)
! ***     2. maximum grondwaterstand (m)
! ***     3. Detail afvoer voor 1 event
! ***     4. Detail grondwaterstand voor 1 event
! *** III. kasgebied (totaal; niet per klasse)
! ***      1. maximum bergingsgraad bassins (%)
! ***      2. - maximum uitslag van bassins naar open water (m3/s)
! ***         - maximum regenval (m3/s)
! ***      3. Detail bergingsgraad voor 1 event
! ***      4. Detail uitslag (flow) voor 1 event
! *** IV.  open water
! ***      1. maximum waterstand (m)
! ***      2. -aantal keren boven maximum peil (-)
! ***         -totale duur boven maximum peil (-)
! ***      3. Detail peilen voor 1 event
! ***      4. Detail overschrijdingen voor 1 event
! ***  V.  structures
! ***      1. maximum debiet over kunstwerk (m3/s)
! ***      2. Detail debiet voor 1 event
! ***  VI  boundaries
! ***      1. maximum debiet naar boundary  (m3/s)
! ***      2. Detail debiet voor 1 event
! ***  VII Pluvius knopen
! ***      1. maximum berging   (   )
! ***      2. maximum debieten  (m3/s)
! ***      3. Detail berging voor 1 event
! ***      4. Detail debiet voor 1 event
! ***  VIII balans
! ***      1. per event: 3 series per knoop (in, uit, deltaberging)
! ***      2. per tijdstap: 3 series
! ***      3. per tijdstap: 3 series cumulatief
! ***  IX     zout
! ***      1. per event: max. zoutconcentraties per knoop
! ***      2. per tijdstap: zoutconcentratie op de knoop (evt. gem. over de bakj
! ***  X.  1. per event: max. flow door RWZI
!          2. per tijdstap: flow door RWZI
!      XI. 1. per event: max demand industry en max. discharge Industry
!          2. per tijdstap: demand en discharge Industry
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! *********************************************************************

      USE CONF_FIL
      USE CONF_ARR
      use Network
      use Messages
      use Unpaved
      use Link
      Use ReadLib
! ivm EmulateUnixOnPC
      Use ParallelData

      implicit none

      Character(Len=CharIdLength) name

      LOGICAL     GenerateNetCdfOutput
      LOGICAL     ERROUT

      Integer     iFile, lengte, ipos1, ipos2, idum, imap
!      Integer     iDebug
      Character*1 SLASH
#if (defined(HAVE_CONFIG_H))
      SLASH='/'
#else
      SLASH='\'
#endif
      iDebug = ConfFil_get_iDebug()

      IF (iDebug .ne. 0) WRITE (IDEBUG,*) ' INITDT'

! *********************************************************************
! *** NFLMAP (.) = NUMBER OF MAPPIX FILES PER MAP
! *** Oct 2001: aantal files = 0 of 1
! *********************************************************************

          Do Imap=1,NMap
             NFLMAP (imap) = 1
             If (.not. OutputDesired(imap) ) then
                NFLMAP (imap) = 0
             endif
          Enddo

! *********************************************************************
! *** NMFMAP (.,.) = NAME OF .DIR AND
! ***                 .MPX or .HIS-FILES PER MAPPIX FILE AND PER MAP
! *********************************************************************
! eerst inkorten namfil tot max. 8 karakters  (geen pad, geen extensie)
      do ifile = 48,119
! GP nietalleen 48, 76 maar ook file nr 83;
! Voor RWZI, Industry, Sacramento: nrs 89, 90, 91
! Link flows nr. 101
! Doe niets voor RR-Modflow files nr 102-103, en ook niet voor RRWLM his file nr 104
         IF (ifile .le. 76 .or. ifile .eq. 83 .or. &
              (ifile .ge. 89 .and. ifile .le. 91) .or. &
                 ifile .eq. 101 .or. ifile .eq. 119) then
           IPOS2 = 1
           Lengte = Len_Trim (ConfFil_get_Namfil(Ifile))
           DO Idum=1,Lengte
             name = ConfFil_get_Namfil(Ifile)
             name = name(iPos2:)
             IPOS1 = FNDFRST ( SLASH, name, .false. )
             IF (IPOS1 .LT. 0) GOTO 999
             IPOS2 = IPOS2 + IPOS1
           ENDDO
  999      CONTINUE
           name = ConfFil_get_Namfil(Ifile)
           name = name(IPOS2:)
! .HIS /.his issue March 2017: handhaaf filenamen uit fnm file, alleen check voor Sobek_Parallel vlag
#if (defined(SOBEK_PARALLEL))
! nu zoeken op de .HIS
           IPOS1 = FNDFRST ( '.', name, .false. )
           IF (IPOS1 .gt. 0) name(Ipos1:) = ' '
           call ConfFil_set_Namfil(Ifile, name)
           name = ConfFil_get_Namfil(Ifile)
! plak de .HIS direct achter de naam (dus geen spaties voor de punt)
           Lengte = Len_Trim (name)
           name (lengte+1:)='.his'
           call ConfFil_set_Namfil(Ifile, name)
#else
      if (EmulateUnixOnPC) then
          ! test UX versie
! nu zoeken op de .HIS
           IPOS1 = FNDFRST ( '.', name, .false. )
           IF (IPOS1 .gt. 0) name(Ipos1:) = ' '
           call ConfFil_set_Namfil(Ifile, name)
           name = ConfFil_get_Namfil(Ifile)
! plak de .HIS direct achter de naam (dus geen spaties voor de punt)
           Lengte = Len_Trim (name)
           name (lengte+1:)='.his'
           call ConfFil_set_Namfil(Ifile, name)
      else
           ! leave name as is, but only part after path (will be added again later)
           call ConfFil_set_Namfil(Ifile, name)
      endif
#endif

         ENDIF
      end do

      NMFMAP (1,1) = ConfFil_get_namfil(62)   !48)      VERHARD GEBIED
      NMFMAP (2,1) = ConfFil_get_namfil(64)   !50)      ONVERHARD GEBIED
      NMFMAP (3,1) = ConfFil_get_namfil(66)   !52)      KASGEBIED
      NMFMAP (4,1) = ConfFil_get_namfil(68)   !54)      OPEN WATER
      NMFMAP (5,1) = ConfFil_get_namfil(70)   !56)      STRUCTURE
      NMFMAP (6,1) = ConfFil_get_namfil(71)   !57)      BOUNDARY
      NMFMAP (7,1) = ConfFil_get_namfil(72)   !58)      PLUVIUS KNOPEN
      NMFMAP (8,1) = ConfFil_get_namfil(74)   !60)      BALANS
      NMFMAP (9,1) = ConfFil_get_namfil(76)   !61)      ZOUT
      NMFMAP (10,1) = ConfFil_get_namfil(90)  !88)      RWZI
      NMFMAP (11,1) = ConfFil_get_namfil(91)  !89)      Industry
      NMFMAP (12,1) = ConfFil_get_namfil(89)  !         Sacramento
      NMFMAP (13,1) = ConfFil_get_namfil(101) !89)      Link flows
      NMFMAP (14,1) = ConfFil_get_namfil(119) !         Cel
      NMFMAP (15,1) = ConfFil_get_namfil(88)  !         RRRunoff

      if (idebug .ne. 0) Write(idebugLunRR,*) ' na NMFMAP '
! *********************************************************************
! *** IXFMAP (.,.) = FILE REF. INDEX OF .MPX or .HIS-FILES PER MAPPIX FILE AND P
! ***   voor summary output: 20 + NFILE + i (i=1,16)  was 1, 15
! ***   voor detail  output: 40 + NFILE + i (idem)
! *********************************************************************

      IXFMAP (1,1) = 20 + NFILE + 1          ! VERHARD GEBIED
      IXFMAP (2,1) = 20 + NFILE + 3          ! ONVERHARD GEBIED
      IXFMAP (3,1) = 20 + NFILE + 5          ! KASGEBIED
      IXFMAP (4,1) = 20 + NFILE + 7          ! OPEN WATER
      IXFMAP (5,1) = 20 + NFILE + 9          ! STRUCTURE
      IXFMAP (6,1) = 20 + NFILE + 10         ! BOUNDARY
      IXFMAP (7,1) = 20 + NFILE + 11         ! PLUVIUS
      IXFMAP (8,1) = 20 + NFILE + 13         ! BALANS
      IXFMAP (9,1) = 20 + NFILE + 14         ! ZOUT
      IXFMAP (10,1) = 20 + NFILE + 15        ! RWZI
      IXFMAP (11,1) = 20 + NFILE + 16        ! Industry
      IXFMAP (12,1) = 20 + NFILE + 17        ! Sacramento
      IXFMAP (13,1) = 20 + NFILE + 18        ! Link Flows
      IXFMAP (14,1) = 20 + NFILE + 19        ! Cel
      IXFMAP (15,1) = 20 + NFILE + 20        ! RRRunoff

! *********************************************************************
! *** Generate .dir file with the names of the .MPX or .HIS files per map
! ***   sorteer per type knoop, per gebeurtenis of detail
! *********************************************************************
!
! Genereren van de DLF file is 9 april 1999 geschrapt; ARS 2450.

! *********************************************************************
! *** NSRMAP (.,.) = NUMBER OF SERIES PER MAP
! *********************************************************************
! Initialisatie
        NSRMAP = 0

        If (OutputDesired(1))  NSRMAP (1) = 17            ! Paved area

! Unpaved
        If (OutputDesired(2)) then
          if (UnSatZoneOption .ge. 1) then
            ! EMRP: added gw below surface
            NSRMAP (2) = 22  ! 19  ! was 3; nu met extra evap en caprise/perc, evap, inf.      extra irrigation!
          else
            NSRMAP (2) = 19  ! 16
          endif
        Endif

        If (OutputDesired(3))  NSRMAP (3) = 5             ! Greenhouse
        If (OutputDesired(4))  NSRMAP (4) = 9 ! was 7     ! Open water
        If (OutputDesired(5))  NSRMAP (5) = 4 ! was 1     ! RR-Structures
        If (OutputDesired(6))  NSRMAP (6) = 2             ! RR-boundaries

! NWRW
        If (OutputDesired(7))  then
          NSRMAP (7) = 54      ! Nov 2009: original value: 11;
                               ! 2010: increased with detailed flow output 12 types, and special area
                               ! Feb 2011: added storage special areas
                               ! Mar 2011: added wadi infiltration
                               ! 2020: added DWF firms (companies, factories, firms, offices)
!29042014 if (NEvent .gt. 1 .and. Iopt1(3) .ne. 1) NSRMAP(7) = 9
        endif
! Balance
        If (OutputDesired(8))  then
          NSRMAP (8) = 6
          if (NEvent .gt. 1) NSRMAP(8) = 3
          If (ExtendedBalanceOutput) then
            NSRMAP (8) = 12
            if (NEvent .gt. 1) NSRMAP(8) = 6
            if (NEvent .gt. 1 .and. Iopt1(3) .ne. 0) NSRMAP(8) = 12
          endif
        Else
! ARS xxxx 13June 2002
!   switch off next line
!         OutputDesired(8) = .true.
          NFLMAP (8) = 1
          ExtendedBalanceOutput=.false.
          NSRMAP (8) = 6
          if (NEvent .gt. 1) NSRMAP(8) = 3
        Endif
        If (OutputDesired(9))  NSRMAP (9) = 1             ! Salt
        If (OutputDesired(10))  NSRMAP (10) = 2           ! WWTP

! Nov 2001: Industry output includes allocation, shortage
        If (OutputDesired(11))  NSRMAP (11) = 4           ! Industry
        If (OutputDesired(12))  NSRMAP (12) = 21          ! Sacramento
        If (OutputDesired(13))  then
            NSRMAP (13) = 1                               ! Link flows flows
            if (RoutingLinkExists) NSRMAP(13) = NSRMAP(13) + 2    ! Link flows output, if routing links exist 3 parameters
            If (NcOwRain .gt. 0)   NSRMAP(13) = NSRMAP(13) + 2    ! Link flows, poss. routing link flows, ow-evap
        Endif
        If (OutputDesired(14))  NSRMAP (14) = 55          ! Cel
        If (OutputDesired(15))  NSRMAP (15) = 85          ! RRRunoff
        If (NcRRRunoff .gt. 0) then
           NSRMap(15) = 5
           if (NCRRRunoffHBV  .gt. 0)  then
               NStartHBV = NSRMap(15) + 1
               NSRMap(15) = NSRMap(15) + 16  ! 16 additional HBV parameters
           endif
           if (NCRRRunoffSCS  .gt. 0)  then
               NStartSCS = NSRMap(15) + 1
               NSRMap(15) = NSRMap(15) + 1   ! 1  additional SCS parameter
           endif
           if (NCRRRunoffNAM .gt. 0) then
               NStartNAM = NSRMap(15) + 1
               NSRMap(15) = NSRMap(15) + 32  !    additional NAM parameters
           endif
           if (NCRRRunoffLGSI .gt. 0) then
               NStartLGSI = NSRMap(15) + 1
               NSRMap(15) = NSRMap(15) + 30  !    additional LGSI parameter
           endif
           if (NCRRRunoffWagMod .gt. 0) then
               NStartWagmod = NSRMap(15) + 1
               NSRMap(15) = NSRMap(15) + 5   ! 5  additional Wagmod parameters
           endif
           if (NCRRRunoffWalrus .gt. 0) then
               NStartWalrus = NSRMap(15) + 1
               NSRMap(15) = NSRMap(15) + 19  ! 19 additional Walrus parameters
           endif
        Endif

        Do imap=1,nmap
           if (Nsrmap(imap) .gt. MaxSeriesPerMap(imap)) then
              call ErrMsgStandard (972, 0, '  Initdt', ' MaxSeriesPerMap dimension violation')
           Endif
        Enddo

! *********************************************************************
! *** NMPMAP (.,.) = NAME OF PARAMETER IN SERIES PER MAPPIX FILE AND PER MAP
! *********************************************************************

        NMPMAP (1,1)  = 'Paved'
        NMPMAP (2,1)  = 'Unpaved'
        NMPMAP (3,1)  = 'Greenhouse'
        NMPMAP (4,1)  = 'Open water level'
        NMPMAP (5,1)  = 'Structures'
        NMPMAP (6,1)  = 'Boundary'
        NMPMAP (7,1)  = 'NWRW'
        NMPMAP (8,1)  = 'Balance'
        NMPMAP (9,1)  = 'Salt Concentration'
        NMPMAP (10,1) = 'WWTP Flows'
        NMPMAP (11,1) = 'Industry flows'
        NMPMAP (12,1) = 'Sacramento'
        NMPMAP (13,1) = 'Flows'
        NMPMAP (14,1) = 'Cel'
        NMPMAP (15,1) = 'RRRunoff'

      ERROUT = .FALSE.


! *********************************************************************
! *** DSRMAP (.,.) = DESCRIPTION OF SERIES PER MAP and SERIES
! *** UNITS  (.,.) = UNITS OF SERIES PER MAP and SERIES
! *********************************************************************

      if (idebug .ne. 0) Write(idebugLunRR,*) ' voor DSRMAP '
!       DSRMAP = ' '
! ***                            VERHARD GEBIED
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
           DSRMAP(1,1)  = TranslateString (LanguageHandle,'Mx.Storage CSS/SWS [mm]')
           DSRMAP(1,2)  = TranslateString (LanguageHandle,'Mx.Storage WWS [mm]')
           DSRMAP(1,3)  = TranslateString (LanguageHandle,'Mx.Stor.Street [mm]')
           DSRMAP(1,4)  = TranslateString (LanguageHandle,'Mx.Spill     [m3/s]')
           DSRMAP(1,5)  = TranslateString (LanguageHandle,'Mx.Pump      [m3/s]')
           DSRMAP(1,6)  = TranslateString (LanguageHandle,'Mx.Q_OpenWater[m3/s]')
           DSRMAP(1,7)  = TranslateString (LanguageHandle,'Mx.Rainfall  [m3/s]')
           DSRMAP(1,8)  = TranslateString (LanguageHandle,'Mx.DWF to CSS/SWS [m3/s]')
           DSRMAP(1,9)  = TranslateString (LanguageHandle,'Mx.DWF to WWS [m3/s]')
           DSRMAP(1,10) = TranslateString (LanguageHandle,'Mx.Street to CSS/SWS [m3/s]')
           DSRMAP(1,11) = TranslateString (LanguageHandle,'Mx.SWS to WWS [m3/s]')
           DSRMAP(1,12) = TranslateString (LanguageHandle,'Mx.Spill. CSS/SWS [m3/s]')
           DSRMAP(1,13) = TranslateString (LanguageHandle,'Mx.Pump CSS/SWS [m3/s]')
           DSRMAP(1,14) = TranslateString (LanguageHandle,'Mx.Spill WWS [m3/s]')
           DSRMAP(1,15) = TranslateString (LanguageHandle,'Mx.Pump WWS  [m3/s]')
           DSRMAP(1,16) = TranslateString (LanguageHandle,'Mx.Street Evap. [m3/s]')
           DSRMAP(1,17) = TranslateString (LanguageHandle,'Mx.DynamicStorage[mm]')
        else
           DSRMAP(1,1)  = TranslateString (LanguageHandle,'Storage CSS/SWS[mm]')
           DSRMAP(1,2)  = TranslateString (LanguageHandle,'Storage WWS    [mm]')
           DSRMAP(1,3)  = TranslateString (LanguageHandle,'Storage Street [mm]')
           DSRMAP(1,4)  = TranslateString (LanguageHandle,'Spill        [m3/s]')
           DSRMAP(1,5)  = TranslateString (LanguageHandle,'Pump         [m3/s]')
           DSRMAP(1,6)  = TranslateString (LanguageHandle,'Q_OpenWater  [m3/s]')
           DSRMAP(1,7)  = TranslateString (LanguageHandle,'Rainfall     [m3/s]')
           DSRMAP(1,8)  = TranslateString (LanguageHandle,'DWF to CSS/SWS [m3/s]')
           DSRMAP(1,9)  = TranslateString (LanguageHandle,'DWF to WWS   [m3/s]')
           DSRMAP(1,10) = TranslateString (LanguageHandle,'Street to CSS/SWS [m3/s]')
           DSRMAP(1,11) = TranslateString (LanguageHandle,'SWS to WWS   [m3/s]')
           DSRMAP(1,12) = TranslateString (LanguageHandle,'Spill CSS/SWS [m3/s]')
           DSRMAP(1,13) = TranslateString (LanguageHandle,'Pump CSS/SWS [m3/s]')
           DSRMAP(1,14) = TranslateString (LanguageHandle,'Spill WWS    [m3/s]')
           DSRMAP(1,15) = TranslateString (LanguageHandle,'Pump WWS     [m3/s]')
           DSRMAP(1,16) = TranslateString (LanguageHandle,'Street evap. [m3/s]')
           DSRMAP(1,17) = TranslateString (LanguageHandle,'Dynamic Storage[mm]')
        endif
        Do idum=1,3
           UNITS (1,idum) = TranslateString (LanguageHandle,'mm')
        Enddo
        Do idum=4,16
           UNITS (1,idum) = TranslateString (LanguageHandle,'m3 s-1')
        Enddo
        UNITS (1,17) = TranslateString (LanguageHandle,'mm')

! ***                          ONVERHARD GEBIED
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
           DSRMAP(2,1)  = TranslateString (LanguageHandle,'Mx.SurfaceRunf[m3/s]')
           DSRMAP(2,2)  = TranslateString (LanguageHandle,'Mx.GW-Outflow [m3/s]')
           DSRMAP(2,3)  = TranslateString (LanguageHandle,'Mx.Rainfall   [m3/s]')
           DSRMAP(2,4)  = TranslateString (LanguageHandle,'Mx.Evap.Surf. [m3/s]')
           DSRMAP(2,5)  = TranslateString (LanguageHandle,'Mx.Infiltrat. [m3/s]')
           DSRMAP(2,6)  = TranslateString (LanguageHandle,'Mx.Net Seepage[m3/s]')
           DSRMAP(2,7)  = TranslateString (LanguageHandle,'Mx.ActualEvap [m3/s]')
           DSRMAP(2,8)  = TranslateString (LanguageHandle,'Mx.Pot. evap. [m3/s]')
           DSRMAP(2,9)  = TranslateString (LanguageHandle,'Mx.Percolation[m3/s]')
           DSRMAP(2,10) = TranslateString (LanguageHandle,'Mx.Cap.rise   [m3/s]')
           DSRMAP(2,11) = TranslateString (LanguageHandle,'Mx.Groundw.Level[m]')
           DSRMAP(2,12) = TranslateString (LanguageHandle,'Max.Inundation  [%]')
           DSRMAP(2,13) = TranslateString (LanguageHandle,'Max.Stor.land  [mm]')
           DSRMAP(2,14) = TranslateString (LanguageHandle,'Max.Volume GW  [m3]')
           DSRMAP(2,15) = TranslateString (LanguageHandle,'Max.Vol.onLand [m3]')
           DSRMAP(2,16) = TranslateString (LanguageHandle,'GW.Exceedance [hour]')
           DSRMAP(2,17) = TranslateString (LanguageHandle,'Mx.gwl below surf[m]')
           DSRMAP(2,18) = TranslateString (LanguageHandle,'Mx.Irr.Supply.[m3/s]')
           DSRMAP(2,19) = TranslateString (LanguageHandle,'Mx.Irr.GWdemnd[m3/s]')
           DSRMAP(2,20) = TranslateString (LanguageHandle,'Max. Stor.Coeff')
           DSRMAP(2,21) = TranslateString (LanguageHandle,'Max. UnsatZone  [mm]')
           DSRMAP(2,22) = TranslateString (LanguageHandle,'Mx.Vol.UnsatZone[m3]')
        else
           DSRMAP(2,1)  = TranslateString (LanguageHandle,'Surf. Runoff  [m3/s]')
           DSRMAP(2,2)  = TranslateString (LanguageHandle,'Groundw.outfl.[m3/s]')
           DSRMAP(2,3)  = TranslateString (LanguageHandle,'Rainfall     [m3/s]')
           DSRMAP(2,4)  = TranslateString (LanguageHandle,'Evap. surface[m3/s]')
           DSRMAP(2,5)  = TranslateString (LanguageHandle,'Infiltration [m3/s]')
           DSRMAP(2,6)  = TranslateString (LanguageHandle,'Net Seepage  [m3/s]')
           DSRMAP(2,7)  = TranslateString (LanguageHandle,'Actual Evap. [m3/s]')
           DSRMAP(2,8)  = TranslateString (LanguageHandle,'Pot. Evapor. [m3/s]')
           DSRMAP(2,9)  = TranslateString (LanguageHandle,'Percolation  [m3/s]')
           DSRMAP(2,10) = TranslateString (LanguageHandle,'Capill.Rise  [m3/s]')
           DSRMAP(2,11) = TranslateString (LanguageHandle,'Groundw.Level   [m]')
           DSRMAP(2,12) = TranslateString (LanguageHandle,'Inundation perc.[%]')
           DSRMAP(2,13) = TranslateString (LanguageHandle,'Storage Land   [mm]')
           if (GenerateNetCdfOutput) DSRMAP(2,13) = TranslateString (LanguageHandle,'Storage Land depth [mm]')
           DSRMAP(2,14) = TranslateString (LanguageHandle,'Groundw.Volume [m3]')
           DSRMAP(2,15) = TranslateString (LanguageHandle,'Storage Land   [m3]')
           if (GenerateNetCdfOutput) DSRMAP(2,15) = TranslateString (LanguageHandle,'Storage Land volume [m3]')
           DSRMAP(2,16) = TranslateString (LanguageHandle,'GW>Threshold [hour]')
           if (GenerateNetCdfOutput) DSRMAP(2,16) = TranslateString (LanguageHandle,'GW above Threshold [hour]')
           DSRMAP(2,17) = TranslateString (LanguageHandle,'GWLevel-Surface[m]')
           DSRMAP(2,18) = TranslateString (LanguageHandle,'IrrigationSup[m3/s]')
           DSRMAP(2,19) = TranslateString (LanguageHandle,'Irrigat.GWdem[m3/s]')
           DSRMAP(2,20) = TranslateString (LanguageHandle,'Storage coefficient')
           DSRMAP(2,21) = TranslateString (LanguageHandle,'Unsat.Zone    [mm]')
           DSRMAP(2,22) = TranslateString (LanguageHandle,'Vol.Unsat.Zone[m3]')
        endif
        Do idum=1,10
           UNITS (2,idum) = TranslateString (LanguageHandle,'m3 s-1')
        Enddo
        UNITS (2,11) = TranslateString (LanguageHandle,'m')
        UNITS (2,12) = TranslateString (LanguageHandle,'%')
        UNITS (2,13) = TranslateString (LanguageHandle,'mm')
        UNITS (2,14) = TranslateString (LanguageHandle,'m3')
        UNITS (2,15) = TranslateString (LanguageHandle,'m3')
        UNITS (2,16) = TranslateString (LanguageHandle,'hour')
        UNITS (2,17) = TranslateString (LanguageHandle,'m')
        UNITS (2,18) = TranslateString (LanguageHandle,'m3 s-1')
        UNITS (2,19) = TranslateString (LanguageHandle,'m3 s-1')
        UNITS (2,20) = TranslateString (LanguageHandle,'-')
        UNITS (2,21) = TranslateString (LanguageHandle,'mm')
        UNITS (2,22) = TranslateString (LanguageHandle,'m3')

! ***                          KASGEBIED
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
            DSRMAP(3,1) = TranslateString (LanguageHandle,'Max Stor.Basins[m3]')
            DSRMAP(3,2) = TranslateString (LanguageHandle,'Max Outflow  [m3/s]')
            DSRMAP(3,3) = TranslateString (LanguageHandle,'Max Rainfall [m3/s]')
            DSRMAP(3,4) = TranslateString (LanguageHandle,'Max Evapor.  [m3/s]')
            DSRMAP(3,5) = TranslateString (LanguageHandle,'Max WaterUse [m3/s]')
        else
            DSRMAP(3,1) = TranslateString (LanguageHandle,'Storage basins[m3]')
            DSRMAP(3,2) = TranslateString (LanguageHandle,'Flow basins [m3/s]')
            DSRMAP(3,3) = TranslateString (LanguageHandle,'Rainfall    [m3/s]')
            DSRMAP(3,4) = TranslateString (LanguageHandle,'Evaporation [m3/s]')
            DSRMAP(3,5) = TranslateString (LanguageHandle,'Water use   [m3/s]')
        endif
        UNITS (3,1) = TranslateString (LanguageHandle,'m3')
        Do idum=2,5
           UNITS (3,idum) = TranslateString (LanguageHandle,'m3 s-1')
        Enddo
! ***                          OPEN WATER MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
           DSRMAP(4,1) = TranslateString (LanguageHandle,'Max. WaterLevel [m]')
           DSRMAP(4,2) = TranslateString (LanguageHandle,'Max.Water Vol. [m3]')
           DSRMAP(4,3) = TranslateString (LanguageHandle,'Max.Rainfall [m3/s]')
           DSRMAP(4,4) = TranslateString (LanguageHandle,'Max.Evapor.  [m3/s]')
           DSRMAP(4,5) = TranslateString (LanguageHandle,'Mx.NetSeepage [m3/s]')
           DSRMAP(4,6) = TranslateString (LanguageHandle,'Mx.>Max.Level [hour]')
           if (GenerateNetCdfOutput) DSRMAP(4,6) = TranslateString (LanguageHandle,'Mx.above Max.Level [hour]')
           DSRMAP(4,7) = TranslateString (LanguageHandle,'Mx.Iter.BalanceError')
           DSRMAP(4,8) = TranslateString (LanguageHandle,'Mx.Filling percentage')
           DSRMAP(4,9) = TranslateString (LanguageHandle,'Mx.Target Level')
        else
           DSRMAP(4,1) = TranslateString (LanguageHandle,'Water Level [m]')
           DSRMAP(4,2) = TranslateString (LanguageHandle,'OpenWater Vol. [m3]')
           DSRMAP(4,3) = TranslateString (LanguageHandle,'Rainfall    [m3/s]')
           DSRMAP(4,4) = TranslateString (LanguageHandle,'Evaporation [m3/s]')
           DSRMAP(4,5) = TranslateString (LanguageHandle,'Net Seepage [m3/s]')
           DSRMAP(4,6) = TranslateString (LanguageHandle,'Exceed.Max.Lvl [m]')
           DSRMAP(4,7) = TranslateString (LanguageHandle,'IterationBal.Error')
           DSRMAP(4,8) = TranslateString (LanguageHandle,'Filling percentage')
           DSRMAP(4,9) = TranslateString (LanguageHandle,'Target level')
        endif
        UNITS (4,1) = TranslateString (LanguageHandle,'m')
        UNITS (4,2) = TranslateString (LanguageHandle,'m3')
        UNITS (4,3) = TranslateString (LanguageHandle,'m3 s-1')
        UNITS (4,4) = TranslateString (LanguageHandle,'m3 s-1')
        UNITS (4,5) = TranslateString (LanguageHandle,'m3 s-1')
        UNITS (4,6) = TranslateString (LanguageHandle,'hour')
        UNITS (4,7) = TranslateString (LanguageHandle,'-')
        UNITS (4,8) = TranslateString (LanguageHandle,'%')
        UNITS (4,9) = TranslateString (LanguageHandle,'m')
! ***                          STRUCTURES MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
           DSRMAP(5,1) = TranslateString (LanguageHandle,'Max. Flow [m3/s]')
           DSRMAP(5,2) = TranslateString (LanguageHandle,'Max. Crestlvl /Opening height [m]')
           DSRMAP(5,3) = TranslateString (LanguageHandle,'Max. Flow1 [m3/s]')
           DSRMAP(5,4) = TranslateString (LanguageHandle,'Max. Flow2 [m3/s]')
        else
           DSRMAP(5,1) = TranslateString (LanguageHandle,'Flow [m3/s]')
           DSRMAP(5,2) = TranslateString (LanguageHandle,'Crestlevel / Opening height [m]')
           DSRMAP(5,3) = TranslateString (LanguageHandle,'Flow1 [m3/s]')
           DSRMAP(5,4) = TranslateString (LanguageHandle,'Flow2 [m3/s]')
        endif
        UNITS(5,1) = TranslateString (LanguageHandle,'m3 s-1')
        UNITS(5,2) = TranslateString (LanguageHandle,'m')
        UNITS(5,3) = TranslateString (LanguageHandle,'m3 s-1')
        UNITS(5,4) = TranslateString (LanguageHandle,'m3 s-1')
! ***                          BOUNDARIES MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          DSRMAP(6,1) = TranslateString (LanguageHandle,'Max. Flow [m3/s]')
          DSRMAP(6,2) = TranslateString (LanguageHandle,'Max. Bnd.level [m]')
        else
          DSRMAP(6,1) = TranslateString (LanguageHandle,'Flow [m3/s]')
          DSRMAP(6,2) = TranslateString (LanguageHandle,'Boundary Level [m AD]')
        endif
        UNITS (6,1) = TranslateString (LanguageHandle,'m3 s-1')
        UNITS (6,2) = TranslateString (LanguageHandle,'m AD')
! ***                          PLUVIUS
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          DSRMAP(7,1) = TranslateString (LanguageHandle,'Mx.Disch_to_sewer [m3/s]')
          DSRMAP(7,2) = TranslateString (LanguageHandle,'Mx.Infil.Depr.[m3/s]')         ! March2017 stond nog mm/hr (fout?)
          DSRMAP(7,3) = TranslateString (LanguageHandle,'Mx.Infil.Runoff[m3/s]')        ! idem
          DSRMAP(7,4) = TranslateString (LanguageHandle,'Max Rainfall [m3/s]')
          DSRMAP(7,5) = TranslateString (LanguageHandle,'Max Evapor.  [m3/s]')
          DSRMAP(7,6) = TranslateString (LanguageHandle,'Max RWF [m3/s]')
          DSRMAP(7,7) = TranslateString (LanguageHandle,'Max DWF people [m3/s]')
          DSRMAP(7,8) = TranslateString (LanguageHandle,'Mx.Stor.Depress.[m3]')
          DSRMAP(7,9) = TranslateString (LanguageHandle,'Mx.DynamicStor.[m3]')
          DSRMAP(7,10) = TranslateString (LanguageHandle,'Mx.Inf.Surf.[mm/hr]')
          DSRMAP(7,11) = TranslateString (LanguageHandle,'Mx.Inf.Dyn.[mm/hr]')
          DSRMAP(7,12) = TranslateString (LanguageHandle,'Mx.Infl.sewer1 [m3/s]')
          DSRMAP(7,13) = TranslateString (LanguageHandle,'Mx.Infl.sewer2 [m3/s]')
          DSRMAP(7,14) = TranslateString (LanguageHandle,'Mx.Infl.sewer3 [m3/s]')
          DSRMAP(7,15) = TranslateString (LanguageHandle,'Mx.Infl.sewer4 [m3/s]')
          DSRMAP(7,16) = TranslateString (LanguageHandle,'Mx.Infl.sewer5 [m3/s]')
          DSRMAP(7,17) = TranslateString (LanguageHandle,'Mx.Infl.sewer6 [m3/s]')
          DSRMAP(7,18) = TranslateString (LanguageHandle,'Mx.Infl.sewer7 [m3/s]')
          DSRMAP(7,19) = TranslateString (LanguageHandle,'Mx.Infl.sewer8 [m3/s]')
          DSRMAP(7,20) = TranslateString (LanguageHandle,'Mx.Infl.sewer9 [m3/s]')
          DSRMAP(7,21) = TranslateString (LanguageHandle,'Mx.Infl.sewer10 [m3/s]')
          DSRMAP(7,22) = TranslateString (LanguageHandle,'Mx.Infl.sewer11 [m3/s]')
          DSRMAP(7,23) = TranslateString (LanguageHandle,'Mx.Infl.sewer12 [m3/s]')
          DSRMAP(7,24) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp1 [m3/s]')
          DSRMAP(7,25) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp2 [m3/s]')
          DSRMAP(7,26) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp3 [m3/s]')
          DSRMAP(7,27) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp4 [m3/s]')
          DSRMAP(7,28) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp5 [m3/s]')
          DSRMAP(7,29) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp6 [m3/s]')
          DSRMAP(7,30) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp7 [m3/s]')
          DSRMAP(7,31) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp8 [m3/s]')
          DSRMAP(7,32) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp9 [m3/s]')
          DSRMAP(7,33) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp10[m3/s]')
          DSRMAP(7,34) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp11[m3/s]')
          DSRMAP(7,35) = TranslateString (LanguageHandle,'Mx.Infl.sew.sp12[m3/s]')
          DSRMAP(7,36) = TranslateString (LanguageHandle,'Mx.Storage sp1 [m3]')
          DSRMAP(7,37) = TranslateString (LanguageHandle,'Mx.Storage sp2 [m3]')
          DSRMAP(7,38) = TranslateString (LanguageHandle,'Mx.Storage sp3 [m3]')
          DSRMAP(7,39) = TranslateString (LanguageHandle,'Mx.Storage sp4 [m3]')
          DSRMAP(7,40) = TranslateString (LanguageHandle,'Mx.Storage sp5 [m3]')
          DSRMAP(7,41) = TranslateString (LanguageHandle,'Mx.Storage sp6 [m3]')
          DSRMAP(7,42) = TranslateString (LanguageHandle,'Mx.Storage sp7 [m3]')
          DSRMAP(7,43) = TranslateString (LanguageHandle,'Mx.Storage sp8 [m3]')
          DSRMAP(7,44) = TranslateString (LanguageHandle,'Mx.Storage sp9 [m3]')
          DSRMAP(7,45) = TranslateString (LanguageHandle,'Mx.Storage sp10 [m3]')
          DSRMAP(7,46) = TranslateString (LanguageHandle,'Mx.Storage sp11 [m3]')
          DSRMAP(7,47) = TranslateString (LanguageHandle,'Mx.Storage sp12 [m3]')
          DSRMAP(7,48) = TranslateString (LanguageHandle,'Mx.Wadi inflow  [m3/s]')
          DSRMAP(7,49) = TranslateString (LanguageHandle,'Mx.Wadi infiltr [m3/s]')
          DSRMAP(7,50) = TranslateString (LanguageHandle,'Mx.Wadi spill   [m3/s]')
          DSRMAP(7,51) = TranslateString (LanguageHandle,'Mx.Wadi drain   [m3/s]')
          DSRMAP(7,52) = TranslateString (LanguageHandle,'Mx.Wadi storage [m3]')
          DSRMAP(7,53) = TranslateString (LanguageHandle,'Mx.Wadi level   [m]')
          DSRMAP(7,54) = TranslateString (LanguageHandle,'Mx.DWF companies[m3/s]')
        else
          DSRMAP(7,1)  = TranslateString (LanguageHandle,'Inflow sewer [m3/s]')
          DSRMAP(7,2)  = TranslateString (LanguageHandle,'Infil.Depress.[m3/s]')
          DSRMAP(7,3)  = TranslateString (LanguageHandle,'Infil.Runoff [m3/s]')
          DSRMAP(7,4)  = TranslateString (LanguageHandle,'Rainfall [m3/s]')
          DSRMAP(7,5)  = TranslateString (LanguageHandle,'Evaporation [m3/s]')
          DSRMAP(7,6)  = TranslateString (LanguageHandle,'RWF [m3/s]')
          DSRMAP(7,7)  = TranslateString (LanguageHandle,'DWF people [m3/s]')
          DSRMAP(7,8)  = TranslateString (LanguageHandle,'StorageDepress.[m3]')
          DSRMAP(7,9)  = TranslateString (LanguageHandle,'Dynamic storage [m3]')
          DSRMAP(7,10) = TranslateString (LanguageHandle,'Inf.cap.Surf.[mm/hr]')
          DSRMAP(7,11) = TranslateString (LanguageHandle,'Inf.cap.Dyn.[mm/hr]')
          DSRMAP(7,12) = TranslateString (LanguageHandle,'Infl.sewer 1 [m3/s]')
          DSRMAP(7,13) = TranslateString (LanguageHandle,'Infl.sewer 2 [m3/s]')
          DSRMAP(7,14) = TranslateString (LanguageHandle,'Infl.sewer 3 [m3/s]')
          DSRMAP(7,15) = TranslateString (LanguageHandle,'Infl.sewer 4 [m3/s]')
          DSRMAP(7,16) = TranslateString (LanguageHandle,'Infl.sewer 5 [m3/s]')
          DSRMAP(7,17) = TranslateString (LanguageHandle,'Infl.sewer 6 [m3/s]')
          DSRMAP(7,18) = TranslateString (LanguageHandle,'Infl.sewer 7 [m3/s]')
          DSRMAP(7,19) = TranslateString (LanguageHandle,'Infl.sewer 8 [m3/s]')
          DSRMAP(7,20) = TranslateString (LanguageHandle,'Infl.sewer 9 [m3/s]')
          DSRMAP(7,21) = TranslateString (LanguageHandle,'Infl.sewer 10 [m3/s]')
          DSRMAP(7,22) = TranslateString (LanguageHandle,'Infl.sewer 11 [m3/s]')
          DSRMAP(7,23) = TranslateString (LanguageHandle,'Infl.sewer 12 [m3/s]')
          DSRMAP(7,24) = TranslateString (LanguageHandle,'Infl.sewer sp1 [m3/s]')
          DSRMAP(7,25) = TranslateString (LanguageHandle,'Infl.sewer sp2 [m3/s]')
          DSRMAP(7,26) = TranslateString (LanguageHandle,'Infl.sewer sp3 [m3/s]')
          DSRMAP(7,27) = TranslateString (LanguageHandle,'Infl.sewer sp4 [m3/s]')
          DSRMAP(7,28) = TranslateString (LanguageHandle,'Infl.sewer sp5 [m3/s]')
          DSRMAP(7,29) = TranslateString (LanguageHandle,'Infl.sewer sp6 [m3/s]')
          DSRMAP(7,30) = TranslateString (LanguageHandle,'Infl.sewer sp7 [m3/s]')
          DSRMAP(7,31) = TranslateString (LanguageHandle,'Infl.sewer sp8 [m3/s]')
          DSRMAP(7,32) = TranslateString (LanguageHandle,'Infl.sewer sp9 [m3/s]')
          DSRMAP(7,33) = TranslateString (LanguageHandle,'Infl.sewer sp10 [m3/s]')
          DSRMAP(7,34) = TranslateString (LanguageHandle,'Infl.sewer sp11 [m3/s]')
          DSRMAP(7,35) = TranslateString (LanguageHandle,'Infl.sewer sp12 [m3/s]')
          DSRMAP(7,36) = TranslateString (LanguageHandle,'Storage sp1 [m3]')
          DSRMAP(7,37) = TranslateString (LanguageHandle,'Storage sp2 [m3]')
          DSRMAP(7,38) = TranslateString (LanguageHandle,'Storage sp3 [m3]')
          DSRMAP(7,39) = TranslateString (LanguageHandle,'Storage sp4 [m3]')
          DSRMAP(7,40) = TranslateString (LanguageHandle,'Storage sp5 [m3]')
          DSRMAP(7,41) = TranslateString (LanguageHandle,'Storage sp6 [m3]')
          DSRMAP(7,42) = TranslateString (LanguageHandle,'Storage sp7 [m3]')
          DSRMAP(7,43) = TranslateString (LanguageHandle,'Storage sp8 [m3]')
          DSRMAP(7,44) = TranslateString (LanguageHandle,'Storage sp9 [m3]')
          DSRMAP(7,45) = TranslateString (LanguageHandle,'Storage sp10 [m3]')
          DSRMAP(7,46) = TranslateString (LanguageHandle,'Storage sp11 [m3]')
          DSRMAP(7,47) = TranslateString (LanguageHandle,'Storage sp12 [m3]')
          DSRMAP(7,48) = TranslateString (LanguageHandle,'Wadi inflow  [m3/s]')
          DSRMAP(7,49) = TranslateString (LanguageHandle,'Wadi infiltr [m3/s]')
          DSRMAP(7,50) = TranslateString (LanguageHandle,'Wadi spill   [m3/s]')
          DSRMAP(7,51) = TranslateString (LanguageHandle,'Wadi drain   [m3/s]')
          DSRMAP(7,52) = TranslateString (LanguageHandle,'Wadi storage [m3]')
          DSRMAP(7,53) = TranslateString (LanguageHandle,'Wadi level   [m]')
          DSRMAP(7,54) = TranslateString (LanguageHandle,'DWF companies [m3/s]')
        endif
        Do idum=1,54
           UNITS (7,idum) = TranslateString (LanguageHandle,'m3')
        Enddo
        UNITS (7,10) = TranslateString (LanguageHandle,'mm hour-1')
        UNITS (7,11) = TranslateString (LanguageHandle,'mm hour-1')
        UNITS (7,53) = TranslateString (LanguageHandle,'m')
! ***                          BALANS MAP
        DSRMAP(8,1) = TranslateString (LanguageHandle,'Total In [m3]')
        DSRMAP(8,2) = TranslateString (LanguageHandle,'Total Out [m3]')
        DSRMAP(8,3) = TranslateString (LanguageHandle,'Delta Storage [m3]')
        DSRMAP(8,4) = TranslateString (LanguageHandle,'Cumulative In [m3]')
        DSRMAP(8,5) = TranslateString (LanguageHandle,'Cumulative Out [m3]')
        DSRMAP(8,6) = TranslateString (LanguageHandle,'CumDeltaStorage[m3]')

        If (ExtendedBalanceOutput) then
          DSRMAP(8,1)  = TranslateString (LanguageHandle,'TotalInAtNode  [m3]')
          DSRMAP(8,2)  = TranslateString (LanguageHandle,'TotalInViaLinks[m3]')
          DSRMAP(8,3)  = TranslateString (LanguageHandle,'TotalOutAtNode [m3]')
          DSRMAP(8,4)  = TranslateString (LanguageHandle,'TotalOutViaLinks[m3]')
          DSRMAP(8,5)  = TranslateString (LanguageHandle,'DeltaStorage [m3]')
          DSRMAP(8,6)  = TranslateString (LanguageHandle,'BalanceError [m3]')
          DSRMAP(8,7)  = TranslateString (LanguageHandle,'Cum.InAtNode [m3]')
          DSRMAP(8,8)  = TranslateString (LanguageHandle,'Cum.InViaLinks [m3]')
          DSRMAP(8,9)  = TranslateString (LanguageHandle,'Cum.OutAtNode  [m3]')
          DSRMAP(8,10) = TranslateString (LanguageHandle,'Cum.OutViaLinks[m3]')
          DSRMAP(8,11) = TranslateString (LanguageHandle,'CumDeltaStorage[m3]')
          DSRMAP(8,12) = TranslateString (LanguageHandle,'CumBalanceError[m3]')
        Endif
        Do idum=1,12
           UNITS (8,idum) = TranslateString (LanguageHandle,'m3')
        Enddo
! ***                          ZOUT MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          DSRMAP(9,1) = TranslateString (LanguageHandle,'Max.Concentration [g/m3]')
        else
          DSRMAP(9,1) = TranslateString (LanguageHandle,'Concentration [g/m3]')
        endif
        UNITS(9,1) = TranslateString (LanguageHandle,'g m-3')
! ***                          RWZI MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          DSRMAP(10,1) = TranslateString (LanguageHandle,'Max.InFlow [m3/s]')
          DSRMAP(10,2) = TranslateString (LanguageHandle,'Max.OutFlow [m3/s]')
        else
          DSRMAP(10,1) = TranslateString (LanguageHandle,'InFlow [m3/s]')
          DSRMAP(10,2) = TranslateString (LanguageHandle,'OutFlow [m3/s]')
        endif
        UNITS(10,1) = TranslateString (LanguageHandle,'m3 s-1')
        UNITS(10,2) = TranslateString (LanguageHandle,'m3 s-1')
! ***                          Industry MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          DSRMAP(11,1) = TranslateString (LanguageHandle,'Max.Demand [m3/s]')
          DSRMAP(11,2) = TranslateString (LanguageHandle,'Max.Allocation[m3/s]')
          DSRMAP(11,3) = TranslateString (LanguageHandle,'Max.Shortage [m3/s]')
          DSRMAP(11,4) = TranslateString (LanguageHandle,'Max.Discharge[m3/s]')
        else
          DSRMAP(11,1) = TranslateString (LanguageHandle,'Demand [m3/s]')
          DSRMAP(11,2) = TranslateString (LanguageHandle,'Allocation[m3/s]')
          DSRMAP(11,3) = TranslateString (LanguageHandle,'Shortage  [m3/s]')
          DSRMAP(11,4) = TranslateString (LanguageHandle,'Discharge [m3/s]')
        endif
        Do idum=1,4
           UNITS (11,idum) = TranslateString (LanguageHandle,'m3 s-1')
        Enddo
! ***                          Sacramento
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          DSRMAP(12,1) = TranslateString (LanguageHandle,'Mx.UpZoTensionC [mm]')
          DSRMAP(12,2) = TranslateString (LanguageHandle,'Mx.UpZoFreeWatC [mm]')
          DSRMAP(12,3) = TranslateString (LanguageHandle,'Mx.LoZoTensionC [mm]')
          DSRMAP(12,4) = TranslateString (LanguageHandle,'Mx.LoZoFreePriC [mm]')
          DSRMAP(12,5) = TranslateString (LanguageHandle,'Mx.LoZoFreeSecC [mm]')
          DSRMAP(12,6) = TranslateString (LanguageHandle,'Max.Act.Precip. [mm]')
          DSRMAP(12,7) = TranslateString (LanguageHandle,'Max.Pot.Evapor. [mm]')
          DSRMAP(12,8) = TranslateString (LanguageHandle,'Max.Act.Evapor. [mm]')
          DSRMAP(12,9) = TranslateString (LanguageHandle,'Max. Baseflow [mm]')
          DSRMAP(12,10)= TranslateString (LanguageHandle,'Max.RoutedTotalSurf.Runoff [mm]')
          DSRMAP(12,11)= TranslateString (LanguageHandle,'Max.Unrout.RunoffImperv[mm]')
          DSRMAP(12,12)= TranslateString (LanguageHandle,'Max.TotalRunoff [mm]')
          DSRMAP(12,13)= TranslateString (LanguageHandle,'Max.Chan.Inflw[m3/s]')
          DSRMAP(12,14)= TranslateString (LanguageHandle,'Max.Side+SSout  [mm]')
          DSRMAP(12,15)= TranslateString (LanguageHandle,'Max.ADIMPContent[mm]')
          DSRMAP(12,16)= TranslateString (LanguageHandle,'Max.UnroutedSurf.[mm]')
          DSRMAP(12,17)= TranslateString (LanguageHandle,'Max.UnroutedInterf[mm]')
          DSRMAP(12,18)= TranslateString (LanguageHandle,'Max.Rout.DirectImp[mm]')
          DSRMAP(12,19)= TranslateString (LanguageHandle,'Max.Rout.Surf.Unp[mm]')
          DSRMAP(12,20)= TranslateString (LanguageHandle,'Max.Rout.Interfl.Unp[mm]')
          DSRMAP(12,21)= TranslateString (LanguageHandle,'Max.Percolation [mm]')
        else
          DSRMAP(12,1) = TranslateString (LanguageHandle,'UppZoneTensionC [mm]')
          DSRMAP(12,2) = TranslateString (LanguageHandle,'UppZoneFreeWatC [mm]')
          DSRMAP(12,3) = TranslateString (LanguageHandle,'LowZoneTensionC [mm]')
          DSRMAP(12,4) = TranslateString (LanguageHandle,'LowZoneFreePriC [mm]')
          DSRMAP(12,5) = TranslateString (LanguageHandle,'LowZoneFreeSecC [mm]')
          DSRMAP(12,6) = TranslateString (LanguageHandle,'Precipitation [mm]')
          DSRMAP(12,7) = TranslateString (LanguageHandle,'Pot.Evaporation [mm]')
          DSRMAP(12,8) = TranslateString (LanguageHandle,'Act.Evaporation [mm]')
          DSRMAP(12,9) = TranslateString (LanguageHandle,'Comp. Baseflow [mm]')
          DSRMAP(12,10)= TranslateString (LanguageHandle,'TotalRoutedSurface Runoff [mm]')
          DSRMAP(12,11)= TranslateString (LanguageHandle,'UnroutedRunoff Imp.Area[mm]')
          DSRMAP(12,12)= TranslateString (LanguageHandle,'Total Runoff [mm]')
          DSRMAP(12,13)= TranslateString (LanguageHandle,'ChannelInflow[m3/s]')
          DSRMAP(12,14)= TranslateString (LanguageHandle,'Side+SSoutflow [mm]')
          DSRMAP(12,15)= TranslateString (LanguageHandle,'AdimC Contents [mm]')
          DSRMAP(12,16)= TranslateString (LanguageHandle,'Unrouted Surf. [mm]')
          DSRMAP(12,17)= TranslateString (LanguageHandle,'Unrouted Interf[mm]')
          DSRMAP(12,18)= TranslateString (LanguageHandle,'Rout.DirectImp [mm]')
          DSRMAP(12,19)= TranslateString (LanguageHandle,'Rout.Surf.Unp. [mm]')
          DSRMAP(12,20)= TranslateString (LanguageHandle,'Rout.Interfl.Unp[mm]')
          DSRMAP(12,21)= TranslateString (LanguageHandle,'Tot.Percolation[mm]')
        endif
        Do idum=1,21
           UNITS (12,idum) = TranslateString (LanguageHandle,'mm')
        Enddo
        UNITS (12,13)= TranslateString (LanguageHandle,'m3 s-1')
! ***                          Link Flows Map
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          DSRMAP(13,1) = TranslateString (LanguageHandle,'Max.LinkFlow [m3/s]')
          if (RoutingLinkExists) then
             DSRMAP(13,2) = TranslateString (LanguageHandle,'Max.Inflow [m3/s]')
             DSRMAP(13,3) = TranslateString (LanguageHandle,'Max.Outflow [m3/s]')
          endif
          If (NcOwRain .gt. 0) then
             if (RoutingLinkExists) then
                DSRMAP(13,4) = TranslateString (LanguageHandle,'Max.Precip. [m3/s]')
                DSRMAP(13,5) = TranslateString (LanguageHandle,'Max.Evap.   [m3/s]')
             else
                DSRMAP(13,2) = TranslateString (LanguageHandle,'Max.Precip. [m3/s]')
                DSRMAP(13,3) = TranslateString (LanguageHandle,'Max.Evap.   [m3/s]')
             endif
          endif
        else
          DSRMAP(13,1) = TranslateString (LanguageHandle,'Link flow [m3/s]')
          if (RoutingLinkExists) then
             DSRMAP(13,2) = TranslateString (LanguageHandle,'Link inflow [m3/s]')
             DSRMAP(13,3) = TranslateString (LanguageHandle,'Link outflow [m3/s]')
          endif
          If (NcOwRain .gt. 0) then
             if (RoutingLinkExists) then
                DSRMAP(13,4) = TranslateString (LanguageHandle,'OWprecipitat.[m3/s]')
                DSRMAP(13,5) = TranslateString (LanguageHandle,'OWevaporation[m3/s]')
              else
                DSRMAP(13,2) = TranslateString (LanguageHandle,'OWprecipitat.[m3/s]')
                DSRMAP(13,3) = TranslateString (LanguageHandle,'OWevaporation[m3/s]')
            endif
          endif
        endif
        Do idum=1,5
           UNITS (13,idum) = TranslateString (LanguageHandle,'m3 s-1')
        Enddo

! ***                          Cel
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
            ! to be added: series output
        else
!          node type inactivated
        endif
! ***                          RRRunoff Map
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          DSRMAP(15,1) = TranslateString  (LanguageHandle,'Max.Rainfall [mm]')
          DSRMAP(15,2) = TranslateString  (LanguageHandle,'Max.PotEvap [mm]')
          DSRMAP(15,3) = TranslateString  (LanguageHandle,'Max.ActEvap [mm]')
          DSRMAP(15,4) = TranslateString  (LanguageHandle,'Max.Runoff [mm]')
          DSRMAP(15,5) = TranslateString  (LanguageHandle,'Max.Outflow [m3/s]')
          UNITS (15,1) = TranslateString  (LanguageHandle,'mm')
          UNITS (15,2) = TranslateString  (LanguageHandle,'mm')
          UNITS (15,3) = TranslateString  (LanguageHandle,'mm')
          UNITS (15,4) = TranslateString  (LanguageHandle,'mm')
          UNITS (15,5) = TranslateString  (LanguageHandle,'m3 s-1')
          if (NCRRRUnoffHBV .gt. 0) then
             DSRMAP(15,NStartHBV) = TranslateString  (LanguageHandle,'Max.HBV-Snowfall [mm]')
             DSRMAP(15,NStartHBV+1) = TranslateString  (LanguageHandle,'Max.HBV-BaseFlow [mm]')
             DSRMAP(15,NStartHBV+2) = TranslateString  (LanguageHandle,'Max.HBV-InterFlow [mm]')
             DSRMAP(15,NStartHBV+3) = TranslateString  (LanguageHandle,'Max.HBV-QuickFlow [mm]')
             DSRMAP(15,NStartHBV+4) = TranslateString (LanguageHandle,'Max.HBV-DrySnowC. [mm]')
             DSRMAP(15,NStartHBV+5) = TranslateString (LanguageHandle,'Max.HBV-FreeWaterC.[mm]')
             DSRMAP(15,NStartHBV+6) = TranslateString (LanguageHandle,'Max.HBV-SoilMoisture [mm]')
             DSRMAP(15,NStartHBV+7) = TranslateString (LanguageHandle,'Max.HBV-UpperZoneC [mm]')
             DSRMAP(15,NStartHBV+8) = TranslateString (LanguageHandle,'Max.HBV-LowerZoneC [mm]')
             DSRMAP(15,NStartHBV+9) = TranslateString (LanguageHandle,'Max.HBV_Temperature [oC]')
             DSRMAP(15,NStartHBV+10) = TranslateString (LanguageHandle,'Max.HBV_Snowmelt   [mm]')
             DSRMAP(15,NStartHBV+11) = TranslateString (LanguageHandle,'Max.HBV_Refreezing [mm]')
             DSRMAP(15,NStartHBV+12) = TranslateString (LanguageHandle,'Max.HBV_Infiltration[mm]')
             DSRMAP(15,NStartHBV+13) = TranslateString (LanguageHandle,'Max.HBV_Seepage    [mm]')
             DSRMAP(15,NStartHBV+14) = TranslateString (LanguageHandle,'Max.HBV_DirectRunoff[mm]')
             DSRMAP(15,NStartHBV+15) = TranslateString (LanguageHandle,'Max.HBV_Percolation [mm]')
             Do idum=NStartHBV,NStartHBV+15
                UNITS (15,idum) = TranslateString (LanguageHandle,'mm')
             Enddo
             UNITS (15,NStartHBV+9)  = TranslateString (LanguageHandle,'oC')
          endif
          if (NCRRRUnoffSCS .gt. 0) then
              DSRMAP(15,NStartSCS) = TranslateString (LanguageHandle,'Max.SCS_Storage [mm]')
              UNITS (15,NStartSCS) = TranslateString (LanguageHandle,'mm')
          endif
          if (NCRRRUnoffNAM .gt. 0) then
              DSRMAP(15,NStartNAM   ) = TranslateString (LanguageHandle,'Max.D-NAM ExternalWaterLevel [m AD]')
              DSRMAP(15,NStartNAM+1 ) = TranslateString (LanguageHandle,'Max.D-NAM GWPumpDefinedDischarge [m3/s]')
              DSRMAP(15,NStartNAM+2 ) = TranslateString (LanguageHandle,'Max.D-NAM EvapSurfaceStorage [mm]')
              DSRMAP(15,NStartNAM+3 ) = TranslateString (LanguageHandle,'Max.D-NAM EvapRootZoneLayer [mm]')
              DSRMAP(15,NStartNAM+4 ) = TranslateString (LanguageHandle,'Max.D-NAM EvapRootZonePartGWStorage [mm]')
              DSRMAP(15,NStartNAM+5 ) = TranslateString (LanguageHandle,'Max.D-NAM EvapLowerZoneStorage [mm]')
              DSRMAP(15,NStartNAM+6 ) = TranslateString (LanguageHandle,'Max.D-NAM OverlandFlow [mm]')
              DSRMAP(15,NStartNAM+7 ) = TranslateString (LanguageHandle,'Max.D-NAM Interflow [mm]')
              DSRMAP(15,NStartNAM+8 ) = TranslateString (LanguageHandle,'Max.D-NAM FastBaseFlow [mm]')
              DSRMAP(15,NStartNAM+9 ) = TranslateString (LanguageHandle,'Max.D-NAM SlowBaseFlow [mm]')
              DSRMAP(15,NStartNAM+10) = TranslateString (LanguageHandle,'Max.D-NAM ExternalGWInflow[mm]')
              DSRMAP(15,NStartNAM+11) = TranslateString (LanguageHandle,'Max.D-NAM ExternalGWInflowLowerZoneStorage [mm]')
              DSRMAP(15,NStartNAM+12) = TranslateString (LanguageHandle,'Max.D-NAM ExternalGWInflowGroundwaterStorage [mm]')
              DSRMAP(15,NStartNAM+13) = TranslateString (LanguageHandle,'Max.D-NAM GWPumpAbsGroundwaterStorage [mm]')
              DSRMAP(15,NStartNAM+14) = TranslateString (LanguageHandle,'Max.D-NAM GWPumpSupLowerZoneStorage [mm]')
              DSRMAP(15,NStartNAM+15) = TranslateString (LanguageHandle,'Max.D-NAM GWPumpSupGroundwaterStorage [mm]')
              DSRMAP(15,NStartNAM+16) = TranslateString (LanguageHandle,'Max.D-NAM Infiltration [mm]')
              DSRMAP(15,NStartNAM+17) = TranslateString (LanguageHandle,'Max.D-NAM InfiltrationLowerZoneStorage [mm]')
              DSRMAP(15,NStartNAM+18) = TranslateString (LanguageHandle,'Max.D-NAM Percolation [mm]')
              DSRMAP(15,NStartNAM+19) = TranslateString (LanguageHandle,'Max.D-NAM CapillaryRise [mm]')
              DSRMAP(15,NStartNAM+20) = TranslateString (LanguageHandle,'Max.D-NAM DepthSurfaceStorage [mm]')
              DSRMAP(15,NStartNAM+21) = TranslateString (LanguageHandle,'Max.D-NAM DepthLowerZoneStorage [mm]')
              DSRMAP(15,NStartNAM+22) = TranslateString (LanguageHandle,'Max.D-NAM DepthGroundwaterStorage [mm]')
              DSRMAP(15,NStartNAM+23) = TranslateString (LanguageHandle,'Max.D-NAM VolumeSurfaceStorage [m3]')
              DSRMAP(15,NStartNAM+24) = TranslateString (LanguageHandle,'Max.D-NAM VolumeLowerZoneStorage [m3]')
              DSRMAP(15,NStartNAM+25) = TranslateString (LanguageHandle,'Max.D-NAM VolumeGroundwaterStorage [m3]')
              DSRMAP(15,NStartNAM+26) = TranslateString (LanguageHandle,'Max.D-NAM GroundWaterLevel [m AD]')
              DSRMAP(15,NStartNAM+27) = TranslateString (LanguageHandle,'Max.D-NAM GroundWaterTableDepth [m below surf]')
              DSRMAP(15,NStartNAM+28) = TranslateString (LanguageHandle,'Max.D-NAM AvailableSoilStorage [m3]')
              DSRMAP(15,NStartNAM+29) = TranslateString (LanguageHandle,'Max.D-NAM BaseFlow [mm]')
              DSRMAP(15,NStartNAM+30) = TranslateString (LanguageHandle,'Max.D-NAM GWPumpActualDischarge [m3/s]')
              DSRMAP(15,NStartNAM+31) = TranslateString (LanguageHandle,'Max.D-NAM GWPumpActual-DefinedDischarge[m3/s]')
              Do idum=NStartNAM+2,NStartNAM+22
                 UNITS (15,idum) = TranslateString (LanguageHandle,'mm')
              Enddo
              UNITS (15,NStartNAM   ) = TranslateString (LanguageHandle,'m AD')
              UNITS (15,NStartNAM+1 ) = TranslateString (LanguageHandle,'m3 s-1')
              UNITS (15,NStartNAM+23) = TranslateString (LanguageHandle,'m3')
              UNITS (15,NStartNAM+24) = TranslateString (LanguageHandle,'m3')
              UNITS (15,NStartNAM+25) = TranslateString (LanguageHandle,'m3')
              UNITS (15,NStartNAM+26) = TranslateString (LanguageHandle,'m AD')
              UNITS (15,NStartNAM+27) = TranslateString (LanguageHandle,'m below surface')
              UNITS (15,NStartNAM+28) = TranslateString (LanguageHandle,'m3')
              UNITS (15,NStartNAM+29) = TranslateString (LanguageHandle,'mm')
              UNITS (15,NStartNAM+30) = TranslateString (LanguageHandle,'m3/s')
              UNITS (15,NStartNAM+31) = TranslateString (LanguageHandle,'m3/s')
          endif
          if (NCRRRUnoffLGSI .gt. 0) then
              DSRMAP(15,NStartLGSI) = TranslateString (LanguageHandle,'Max.LGSIRainfall1 [mm]')
              DSRMAP(15,NStartLGSI+1) = TranslateString (LanguageHandle,'Max.LGSIRainfall2 [mm]')
              DSRMAP(15,NStartLGSI+2) = TranslateString (LanguageHandle,'Max.LGSIPotEvap1 [mm]')
              DSRMAP(15,NStartLGSI+3) = TranslateString (LanguageHandle,'Max.LGSIPotEvap2 [mm]')
              DSRMAP(15,NStartLGSI+4) = TranslateString (LanguageHandle,'Max.LGSIActEvap1 [mm]')
              DSRMAP(15,NStartLGSI+5) = TranslateString (LanguageHandle,'Max.LGSIActEvap2 [mm]')
              DSRMAP(15,NStartLGSI+6) = TranslateString (LanguageHandle,'Max.LGSIRecharge1  [mm]')
              DSRMAP(15,NStartLGSI+7) = TranslateString (LanguageHandle,'Max.LGSIRecharge2  [mm]')
              DSRMAP(15,NStartLGSI+8) = TranslateString (LanguageHandle,'Max.LGSIDrainflow1 [mm]')
              DSRMAP(15,NStartLGSI+9) = TranslateString (LanguageHandle,'Max.LGSIDrainflow2 [mm]')
              DSRMAP(15,NStartLGSI+10) = TranslateString (LanguageHandle,'Max.LGSISeepage2-1 [mm]')
              DSRMAP(15,NStartLGSI+11) = TranslateString (LanguageHandle,'Max.LGSIOvrlndflow1[mm]')
              DSRMAP(15,NStartLGSI+12) = TranslateString (LanguageHandle,'Max.LGSIOvrlndflow2[mm]')
              DSRMAP(15,NStartLGSI+13) = TranslateString (LanguageHandle,'Max.LGSIQpDirect1  [mm]')
              DSRMAP(15,NStartLGSI+14) = TranslateString (LanguageHandle,'Max.LGSIQpDirect2  [mm]')
              DSRMAP(15,NStartLGSI+15) = TranslateString (LanguageHandle,'Max.LGSIRiverflow1 [mm]')
              DSRMAP(15,NStartLGSI+16) = TranslateString (LanguageHandle,'Max.LGSIRiverflow2 [mm]')
              DSRMAP(15,NStartLGSI+17) = TranslateString (LanguageHandle,'Max.LGSIOverlndSto1[mm]')
              DSRMAP(15,NStartLGSI+18) = TranslateString (LanguageHandle,'Max.LGSIOverlndSto2[mm]')
              DSRMAP(15,NStartLGSI+19) = TranslateString (LanguageHandle,'Max.LGSIGwStor1    [mm]')
              DSRMAP(15,NStartLGSI+20) = TranslateString (LanguageHandle,'Max.LGSIGwStor2    [mm]')
              DSRMAP(15,NStartLGSI+21) = TranslateString (LanguageHandle,'Max.LGSINewVolume1 [m ]')
              DSRMAP(15,NStartLGSI+22) = TranslateString (LanguageHandle,'Max.LGSINewVolume2 [m ]')
              DSRMAP(15,NStartLGSI+23) = TranslateString (LanguageHandle,'Max.LGSIGWTableDepth1 mbs [m-surf]')
              DSRMAP(15,NStartLGSI+24) = TranslateString (LanguageHandle,'Max.LGSIGWTableDepth2 mbs [m-surf]')
              DSRMAP(15,NStartLGSI+25) = TranslateString (LanguageHandle,'Max.LGSIRunoffDelay[mm]')
              DSRMAP(15,NStartLGSI+26) = TranslateString (LanguageHandle,'Max.LGSIQtot     [mm]')
              DSRMAP(15,NStartLGSI+27) = TranslateString (LanguageHandle,'Max.LGSIQdelayed [mm]')
              DSRMAP(15,NStartLGSI+28) = TranslateString (LanguageHandle,'Max.LGSIGWLvl1 [m]')
              DSRMAP(15,NStartLGSI+29) = TranslateString (LanguageHandle,'Max.LGSIGWLvl2 [m]')
              Do idum=NStartLGSI,NStartLGSI+20
                 UNITS (15,idum) = TranslateString (LanguageHandle,'mm')
              Enddo
              UNITS (15,NStartLGSI+21) = TranslateString (LanguageHandle,'m ')
              UNITS (15,NStartLGSI+22) = TranslateString (LanguageHandle,'m ')
              UNITS (15,NStartLGSI+23) = TranslateString (LanguageHandle,'m below surface')
              UNITS (15,NStartLGSI+24) = TranslateString (LanguageHandle,'m below surface')
              UNITS (15,NStartLGSI+25) = TranslateString (LanguageHandle,'mm')
              UNITS (15,NStartLGSI+26) = TranslateString (LanguageHandle,'mm')
              UNITS (15,NStartLGSI+27) = TranslateString (LanguageHandle,'mm')
              UNITS (15,NStartLGSI+28) = TranslateString (LanguageHandle,'m')
              UNITS (15,NStartLGSI+29) = TranslateString (LanguageHandle,'m')
          endif
          if (NCRRRUnoffWagMod .gt. 0) then
             DSRMAP(15,NStartWagMod) = TranslateString (LanguageHandle,'Max.WagModBaseFlow[mm]')
             DSRMAP(15,NStartWagMod+1) = TranslateString (LanguageHandle,'Max.WagModQuickFlow[mm]')
             DSRMAP(15,NStartWagMod+2) = TranslateString (LanguageHandle,'Max.WagModSoilMoist[mm]')
             DSRMAP(15,NStartWagMod+3) = TranslateString (LanguageHandle,'Max.WagModGStore   [mm]')
             DSRMAP(15,NStartWagMod+4) = TranslateString (LanguageHandle,'Max.WagModSeepage  [mm]')
             Do idum=NStartWagMod,NStartWagMod+4
                UNITS (15,idum) = TranslateString (LanguageHandle,'mm')
             Enddo
          endif
          if (NCRRRUnoffWalrus .gt. 0) then
! to be adjusted
             DSRMAP(15,NStartWalrus)   = TranslateString (LanguageHandle,'Max.Walrus_StorageDeficit[mm]')
             DSRMAP(15,NStartWalrus+1) = TranslateString (LanguageHandle,'Max.Walrus_DepthGroundwater[mm below SL]')
             DSRMAP(15,NStartWalrus+2) = TranslateString (LanguageHandle,'Max.Walrus_DepthQuickflowReservoir[mm]')
             DSRMAP(15,NStartWalrus+3) = TranslateString (LanguageHandle,'Max.Walrus_DepthSurfaceWaterReservoir[mm]')
             DSRMAP(15,NStartWalrus+4) = TranslateString (LanguageHandle,'Max.Walrus_DefinedFXS [mm]')
             DSRMAP(15,NStartWalrus+5) = TranslateString (LanguageHandle,'Max.Walrus_FXG [mm]')
             DSRMAP(15,NStartWalrus+6) = TranslateString (LanguageHandle,'Max.Walrus_PrecipQuick [mm]')
             DSRMAP(15,NStartWalrus+7) = TranslateString (LanguageHandle,'Max.Walrus_PrecipVadoseZone [mm]')
             DSRMAP(15,NStartWalrus+8) = TranslateString (LanguageHandle,'Max.Walrus_PrecipSurfWater [mm]')
             DSRMAP(15,NStartWalrus+9) = TranslateString (LanguageHandle,'Max.Walrus_ETActLand [mm]')
             DSRMAP(15,NStartWalrus+10) = TranslateString (LanguageHandle,'Max.Walrus_ETActSurfWater [mm]')
             DSRMAP(15,NStartWalrus+11) = TranslateString (LanguageHandle,'Max.Walrus_dVequilibrium [mm]')
             DSRMAP(15,NStartWalrus+12) = TranslateString (LanguageHandle,'Max.Walrus_QuickFlow fQS [mm]')
             DSRMAP(15,NStartWalrus+13) = TranslateString (LanguageHandle,'Max.Walrus_GroundwaterDrainage (fGS) [mm]')
             DSRMAP(15,NStartWalrus+14) = TranslateString (LanguageHandle,'Max.Walrus_SurfWaterOutflow [mm]')
             DSRMAP(15,NStartWalrus+15) = TranslateString (LanguageHandle,'Max.Walrus_WetnessIndex [-]')
             DSRMAP(15,NStartWalrus+16) = TranslateString (LanguageHandle,'Max.Walrus_BetaEvapReductFact[-]')
             DSRMAP(15,NStartWalrus+17) = TranslateString (LanguageHandle,'Max.Walrus_hSmin[mm]')
             DSRMAP(15,NStartWalrus+18) = TranslateString (LanguageHandle,'Max.Walrus_ActualFXS[mm]')
             Do idum=NStartWalrus,NStartWalrus+18
                UNITS (15,idum) = TranslateString (LanguageHandle,'mm')
             Enddo
             UNITS (15,NStartWalrus+1) = TranslateString (LanguageHandle,'mm below surface level')
             UNITS (15,NStartWalrus+15) = TranslateString (LanguageHandle,' - ')
             UNITS (15,nStartWalrus+16) = TranslateString (LanguageHandle,' - ')
          endif
        else
          DSRMAP(15,1) = TranslateString  (LanguageHandle,'Rainfall [mm]')
          DSRMAP(15,2) = TranslateString  (LanguageHandle,'PotEvap [mm]')
          DSRMAP(15,3) = TranslateString  (LanguageHandle,'ActEvap [mm]')
          DSRMAP(15,4) = TranslateString  (LanguageHandle,'Runoff [mm]')
          DSRMAP(15,5) = TranslateString  (LanguageHandle,'Outflow [m3/s]')
          UNITS (15,1) = TranslateString  (LanguageHandle,'mm')
          UNITS (15,2) = TranslateString  (LanguageHandle,'mm')
          UNITS (15,3) = TranslateString  (LanguageHandle,'mm')
          UNITS (15,4) = TranslateString  (LanguageHandle,'mm')
          UNITS (15,5) = TranslateString  (LanguageHandle,'m3 s-1')
          if (NCRRRUnoffHBV .gt. 0) then
             DSRMAP(15,NStartHBV) = TranslateString  (LanguageHandle,'HBV-Snowfall [mm]')
             DSRMAP(15,NStartHBV+1) = TranslateString  (LanguageHandle,'HBV-BaseFlow [mm]')
             DSRMAP(15,NStartHBV+2) = TranslateString  (LanguageHandle,'HBV-InterFlow [mm]')
             DSRMAP(15,NStartHBV+3) = TranslateString  (LanguageHandle,'HBV-QuickFlow [mm]')
             DSRMAP(15,NStartHBV+4) = TranslateString (LanguageHandle,'HBV-DrySnowC. [mm]')
             DSRMAP(15,NStartHBV+5) = TranslateString (LanguageHandle,'HBV-FreeWaterC.[mm]')
             DSRMAP(15,NStartHBV+6) = TranslateString (LanguageHandle,'HBV-SoilMoisture [mm]')
             DSRMAP(15,NStartHBV+7) = TranslateString (LanguageHandle,'HBV-UpperZoneC [mm]')
             DSRMAP(15,NStartHBV+8) = TranslateString (LanguageHandle,'HBV-LowerZoneC [mm]')
             DSRMAP(15,NStartHBV+9) = TranslateString (LanguageHandle,'HBV_Temperature [oC]')
             DSRMAP(15,NStartHBV+10) = TranslateString (LanguageHandle,'HBV_Snowmelt   [mm]')
             DSRMAP(15,NStartHBV+11) = TranslateString (LanguageHandle,'HBV_Refreezing [mm]')
             DSRMAP(15,NStartHBV+12) = TranslateString (LanguageHandle,'HBV_Infiltration[mm]')
             DSRMAP(15,NStartHBV+13) = TranslateString (LanguageHandle,'HBV_Seepage    [mm]')
             DSRMAP(15,NStartHBV+14) = TranslateString (LanguageHandle,'HBV_DirectRunoff[mm]')
             DSRMAP(15,NStartHBV+15) = TranslateString (LanguageHandle,'HBV_Percolation [mm]')
             Do idum=NStartHBV,NStartHBV+15
                UNITS (15,idum) = TranslateString (LanguageHandle,'mm')
             Enddo
             UNITS (15,NStartHBV+9)  = TranslateString (LanguageHandle,'oC')
          endif
          if (NCRRRUnoffSCS .gt. 0) then
              DSRMAP(15,NStartSCS) = TranslateString (LanguageHandle,'SCS_Storage [mm]')
              UNITS (15,NStartSCS) = TranslateString (LanguageHandle,'mm')
          endif
          if (NCRRRUnoffNAM .gt. 0) then
              DSRMAP(15,NStartNAM   ) = TranslateString (LanguageHandle,'D-NAM ExternalWaterLevel [m AD]')
              DSRMAP(15,NStartNAM+1 ) = TranslateString (LanguageHandle,'D-NAM GWPumpDefinedDischarge [m3/s]')
              DSRMAP(15,NStartNAM+2 ) = TranslateString (LanguageHandle,'D-NAM EvapSurfaceStorage [mm]')
              DSRMAP(15,NStartNAM+3 ) = TranslateString (LanguageHandle,'D-NAM EvapRootZoneLayer [mm]')
              DSRMAP(15,NStartNAM+4 ) = TranslateString (LanguageHandle,'D-NAM EvapRootZonePartGWStorage [mm]')
              DSRMAP(15,NStartNAM+5 ) = TranslateString (LanguageHandle,'D-NAM EvapLowerZoneStorage [mm]')
              DSRMAP(15,NStartNAM+6 ) = TranslateString (LanguageHandle,'D-NAM OverlandFlow [mm]')
              DSRMAP(15,NStartNAM+7 ) = TranslateString (LanguageHandle,'D-NAM Interflow [mm]')
              DSRMAP(15,NStartNAM+8 ) = TranslateString (LanguageHandle,'D-NAM FastBaseFlow [mm]')
              DSRMAP(15,NStartNAM+9 ) = TranslateString (LanguageHandle,'D-NAM SlowBaseFlow [mm]')
              DSRMAP(15,NStartNAM+10) = TranslateString (LanguageHandle,'D-NAM ExternalGWInflow[mm]')
              DSRMAP(15,NStartNAM+11) = TranslateString (LanguageHandle,'D-NAM ExternalGWInflowLowerZoneStorage [mm]')
              DSRMAP(15,NStartNAM+12) = TranslateString (LanguageHandle,'D-NAM ExternalGWInflowGroundwaterStorage [mm]')
              DSRMAP(15,NStartNAM+13) = TranslateString (LanguageHandle,'D-NAM GWPumpAbsGroundwaterStorage [mm]')
              DSRMAP(15,NStartNAM+14) = TranslateString (LanguageHandle,'D-NAM GWPumpSupLowerZoneStorage [mm]')
              DSRMAP(15,NStartNAM+15) = TranslateString (LanguageHandle,'D-NAM GWPumpSupGroundwaterStorage [mm]')
              DSRMAP(15,NStartNAM+16) = TranslateString (LanguageHandle,'D-NAM Infiltration [mm]')
              DSRMAP(15,NStartNAM+17) = TranslateString (LanguageHandle,'D-NAM InfiltrationLowerZoneStorage [mm]')
              DSRMAP(15,NStartNAM+18) = TranslateString (LanguageHandle,'D-NAM Percolation [mm]')
              DSRMAP(15,NStartNAM+19) = TranslateString (LanguageHandle,'D-NAM CapillaryRise [mm]')
              DSRMAP(15,NStartNAM+20) = TranslateString (LanguageHandle,'D-NAM DepthSurfaceStorage [mm]')
              DSRMAP(15,NStartNAM+21) = TranslateString (LanguageHandle,'D-NAM DepthLowerZoneStorage [mm]')
              DSRMAP(15,NStartNAM+22) = TranslateString (LanguageHandle,'D-NAM DepthGroundwaterStorage [mm]')
              DSRMAP(15,NStartNAM+23) = TranslateString (LanguageHandle,'D-NAM VolumeSurfaceStorage [m3]')
              DSRMAP(15,NStartNAM+24) = TranslateString (LanguageHandle,'D-NAM VolumeLowerZoneStorage [m3]')
              DSRMAP(15,NStartNAM+25) = TranslateString (LanguageHandle,'D-NAM VolumeGroundwaterStorage [m3]')
              DSRMAP(15,NStartNAM+26) = TranslateString (LanguageHandle,'D-NAM GroundWaterLevel [m AD]')
              DSRMAP(15,NStartNAM+27) = TranslateString (LanguageHandle,'D-NAM GroundWaterTableDepth [m below surf]')
              DSRMAP(15,NStartNAM+28) = TranslateString (LanguageHandle,'D-NAM AvailableSoilStorage [m3]')
              DSRMAP(15,NStartNAM+29) = TranslateString (LanguageHandle,'D-NAM BaseFlow [mm]')
              DSRMAP(15,NStartNAM+30) = TranslateString (LanguageHandle,'D-NAM GWPumpActualDischarge [m3/s]')
              DSRMAP(15,NStartNAM+31) = TranslateString (LanguageHandle,'D-NAM GWPumpActual-DefinedDischarge[m3/s]')
              Do idum=NStartNAM+2,NStartNAM+22
                 UNITS (15,idum) = TranslateString (LanguageHandle,'mm')
              Enddo
              UNITS (15,NStartNAM   ) = TranslateString (LanguageHandle,'m AD')
              UNITS (15,NStartNAM+1 ) = TranslateString (LanguageHandle,'m3 s-1')
              UNITS (15,NStartNAM+23) = TranslateString (LanguageHandle,'m3')
              UNITS (15,NStartNAM+24) = TranslateString (LanguageHandle,'m3')
              UNITS (15,NStartNAM+25) = TranslateString (LanguageHandle,'m3')
              UNITS (15,NStartNAM+26) = TranslateString (LanguageHandle,'m AD')
              UNITS (15,NStartNAM+27) = TranslateString (LanguageHandle,'m below surface')
              UNITS (15,NStartNAM+28) = TranslateString (LanguageHandle,'m3')
              UNITS (15,NStartNAM+29) = TranslateString (LanguageHandle,'mm')
              UNITS (15,NStartNAM+30) = TranslateString (LanguageHandle,'m3/s')
              UNITS (15,NStartNAM+31) = TranslateString (LanguageHandle,'m3/s')
          endif
          if (NCRRRUnoffLGSI .gt. 0) then
              DSRMAP(15,NStartLGSI) = TranslateString (LanguageHandle,'LGSIRainfall1 [mm]')
              DSRMAP(15,NStartLGSI+1) = TranslateString (LanguageHandle,'LGSIRainfall2 [mm]')
              DSRMAP(15,NStartLGSI+2) = TranslateString (LanguageHandle,'LGSIPotEvap1 [mm]')
              DSRMAP(15,NStartLGSI+3) = TranslateString (LanguageHandle,'LGSIPotEvap2 [mm]')
              DSRMAP(15,NStartLGSI+4) = TranslateString (LanguageHandle,'LGSIActEvap1 [mm]')
              DSRMAP(15,NStartLGSI+5) = TranslateString (LanguageHandle,'LGSIActEvap2 [mm]')
              DSRMAP(15,NStartLGSI+6) = TranslateString (LanguageHandle,'LGSIRecharge1  [mm]')
              DSRMAP(15,NStartLGSI+7) = TranslateString (LanguageHandle,'LGSIRecharge2  [mm]')
              DSRMAP(15,NStartLGSI+8) = TranslateString (LanguageHandle,'LGSIDrainflow1 [mm]')
              DSRMAP(15,NStartLGSI+9) = TranslateString (LanguageHandle,'LGSIDrainflow2 [mm]')
              DSRMAP(15,NStartLGSI+10) = TranslateString (LanguageHandle,'LGSISeepage2-1 [mm]')
              DSRMAP(15,NStartLGSI+11) = TranslateString (LanguageHandle,'LGSIOvrlndflow1[mm]')
              DSRMAP(15,NStartLGSI+12) = TranslateString (LanguageHandle,'LGSIOvrlndflow2[mm]')
              DSRMAP(15,NStartLGSI+13) = TranslateString (LanguageHandle,'LGSIQpDirect1  [mm]')
              DSRMAP(15,NStartLGSI+14) = TranslateString (LanguageHandle,'LGSIQpDirect2  [mm]')
              DSRMAP(15,NStartLGSI+15) = TranslateString (LanguageHandle,'LGSIRiverflow1 [mm]')
              DSRMAP(15,NStartLGSI+16) = TranslateString (LanguageHandle,'LGSIRiverflow2 [mm]')
              DSRMAP(15,NStartLGSI+17) = TranslateString (LanguageHandle,'LGSIOverlndSto1[mm]')
              DSRMAP(15,NStartLGSI+18) = TranslateString (LanguageHandle,'LGSIOverlndSto2[mm]')
              DSRMAP(15,NStartLGSI+19) = TranslateString (LanguageHandle,'LGSIGwStor1    [mm]')
              DSRMAP(15,NStartLGSI+20) = TranslateString (LanguageHandle,'LGSIGwStor2    [mm]')
              DSRMAP(15,NStartLGSI+21) = TranslateString (LanguageHandle,'LGSINewVolume1 [m ]')
              DSRMAP(15,NStartLGSI+22) = TranslateString (LanguageHandle,'LGSINewVolume2 [m ]')
              DSRMAP(15,NStartLGSI+23) = TranslateString (LanguageHandle,'LGSIGWTableDepth1 mbs [m-surf]')
              DSRMAP(15,NStartLGSI+24) = TranslateString (LanguageHandle,'LGSIGWTableDepth2 mbs [m-surf]')
              DSRMAP(15,NStartLGSI+25) = TranslateString (LanguageHandle,'LGSIRunoffDelay[mm]')
              DSRMAP(15,NStartLGSI+26) = TranslateString (LanguageHandle,'LGSIQtot     [mm]')
              DSRMAP(15,NStartLGSI+27) = TranslateString (LanguageHandle,'LGSIQdelayed [mm]')
              DSRMAP(15,NStartLGSI+28) = TranslateString (LanguageHandle,'LGSIGWLvl1 [m]')
              DSRMAP(15,NStartLGSI+29) = TranslateString (LanguageHandle,'LGSIGWLvl2 [m]')
              Do idum=NStartLGSI,NStartLGSI+20
                 UNITS (15,idum) = TranslateString (LanguageHandle,'mm')
              Enddo
              UNITS (15,NStartLGSI+21) = TranslateString (LanguageHandle,'m ')
              UNITS (15,NStartLGSI+22) = TranslateString (LanguageHandle,'m ')
              UNITS (15,NStartLGSI+23) = TranslateString (LanguageHandle,'m below surface')
              UNITS (15,NStartLGSI+24) = TranslateString (LanguageHandle,'m below surface')
              UNITS (15,NStartLGSI+25) = TranslateString (LanguageHandle,'mm')
              UNITS (15,NStartLGSI+26) = TranslateString (LanguageHandle,'mm')
              UNITS (15,NStartLGSI+27) = TranslateString (LanguageHandle,'mm')
              UNITS (15,NStartLGSI+28) = TranslateString (LanguageHandle,'m')
              UNITS (15,NStartLGSI+29) = TranslateString (LanguageHandle,'m')
          endif
          if (NCRRRUnoffWagMod .gt. 0) then
             DSRMAP(15,NStartWagMod) = TranslateString (LanguageHandle,'WagModBaseFlow[mm]')
             DSRMAP(15,NStartWagMod+1) = TranslateString (LanguageHandle,'WagModQuickFlow[mm]')
             DSRMAP(15,NStartWagMod+2) = TranslateString (LanguageHandle,'WagModSoilMoist[mm]')
             DSRMAP(15,NStartWagMod+3) = TranslateString (LanguageHandle,'WagModGStore   [mm]')
             DSRMAP(15,NStartWagMod+4) = TranslateString (LanguageHandle,'WagModSeepage  [mm]')
             Do idum=NStartWagMod,NStartWagMod+4
                 UNITS (15,idum) = TranslateString (LanguageHandle,'mm')
             Enddo
          endif
          if (NCRRRUnoffWalrus .gt. 0) then
             DSRMAP(15,NStartWalrus)   = TranslateString (LanguageHandle,'Walrus_StorageDeficit[mm]')
             DSRMAP(15,NStartWalrus+1) = TranslateString (LanguageHandle,'Walrus_DepthGroundwater[mm below SL]')
             DSRMAP(15,NStartWalrus+2) = TranslateString (LanguageHandle,'Walrus_DepthQuickflowReservoir[mm]')
             DSRMAP(15,NStartWalrus+3) = TranslateString (LanguageHandle,'Walrus_DepthSurfaceWaterReservoir[mm]')
             DSRMAP(15,NStartWalrus+4) = TranslateString (LanguageHandle,'Walrus_DefinedFXS [mm]')
             DSRMAP(15,NStartWalrus+5) = TranslateString (LanguageHandle,'Walrus_FXG [mm]')
             DSRMAP(15,NStartWalrus+6) = TranslateString (LanguageHandle,'Walrus_PrecipQuick [mm]')
             DSRMAP(15,NStartWalrus+7) = TranslateString (LanguageHandle,'Walrus_PrecipVadoseZone [mm]')
             DSRMAP(15,NStartWalrus+8) = TranslateString (LanguageHandle,'Walrus_PrecipSurfWater [mm]')
             DSRMAP(15,NStartWalrus+9) = TranslateString (LanguageHandle,'Walrus_ETActLand [mm]')
             DSRMAP(15,NStartWalrus+10) = TranslateString (LanguageHandle,'Walrus_ETActSurfWater [mm]')
             DSRMAP(15,NStartWalrus+11) = TranslateString (LanguageHandle,'Walrus_dVequilibrium [mm]')
             DSRMAP(15,NStartWalrus+12) = TranslateString (LanguageHandle,'Walrus_QuickFlow fQS [mm]')
             DSRMAP(15,NStartWalrus+13) = TranslateString (LanguageHandle,'Walrus_GroundwaterDrainage (fGS) [mm]')
             DSRMAP(15,NStartWalrus+14) = TranslateString (LanguageHandle,'Walrus_SurfWaterOutflow [mm]')
             DSRMAP(15,NStartWalrus+15) = TranslateString (LanguageHandle,'Walrus_WetnessIndex [-]')
             DSRMAP(15,NStartWalrus+16) = TranslateString (LanguageHandle,'Walrus_BetaEvapReductFact[-]')
             DSRMAP(15,NStartWalrus+17) = TranslateString (LanguageHandle,'Walrus_hSmin[mm]')
             DSRMAP(15,NStartWalrus+18) = TranslateString (LanguageHandle,'Walrus_ActualFXS [mm]')
             Do idum=NStartWalrus,NStartWalrus+18
                 UNITS (15,idum) = TranslateString (LanguageHandle,'mm')
             Enddo
             UNITS (15,NStartWalrus+1) = TranslateString (LanguageHandle,'mm below surface level')
             UNITS (15,NStartWalrus+15) = TranslateString (LanguageHandle,' - ')
             UNITS (15,NStartWalrus+16) = TranslateString (LanguageHandle,' - ')
          endif
        endif

    IF (iDebug .ne. 0) WRITE (IDEBUG,*) ' END OF INITDT'

    RETURN
  END subroutine Initdt


      SUBROUTINE INITDTLong

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.00                 Date: March 1995
! *********************************************************************
! *** Last update: January 21, 1997       By : Peter Schrier
! ***   conversion of dutch captions to english captions
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Initialiseer LongDSRMAP
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! *********************************************************************

      USE CONF_FIL
      USE CONF_ARR
      use Network
      use Messages
      use Unpaved
      use Link
      Use ReadLib
! ivm EmulateUnixOnPC
      Use ParallelData

      implicit none

! ***                            VERHARD GEBIED
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
           LongDSRMAP(1,1)  = TranslateString (LanguageHandle,'Maximum storage combined sewer system / storm water system')
           LongDSRMAP(1,2)  = TranslateString (LanguageHandle,'Maximum storage waste water system')
           LongDSRMAP(1,3)  = TranslateString (LanguageHandle,'Maximum storage street')
           LongDSRMAP(1,4)  = TranslateString (LanguageHandle,'Maximum spill flow')
           LongDSRMAP(1,5)  = TranslateString (LanguageHandle,'Maximum pump flow')
           LongDSRMAP(1,6)  = TranslateString (LanguageHandle,'Maximum flow to OpenWater')
           LongDSRMAP(1,7)  = TranslateString (LanguageHandle,'Maximum rainfall')
           LongDSRMAP(1,8)  = TranslateString (LanguageHandle,'Maximum dry weather flow to combined sewer / storm water sewer')
           LongDSRMAP(1,9)  = TranslateString (LanguageHandle,'Maximum dry weather flow to waste water system')
           LongDSRMAP(1,10) = TranslateString (LanguageHandle,'Maximum flow from street to combined sewer/storm water sewer')
           LongDSRMAP(1,11) = TranslateString (LanguageHandle,'Maximum flow from storm water system to waste water system')
           LongDSRMAP(1,12) = TranslateString (LanguageHandle,'Maximum spill of combined sewer / storm water sewer')
           LongDSRMAP(1,13) = TranslateString (LanguageHandle,'Maximum pumped flow of combined sewer / storm water system')
           LongDSRMAP(1,14) = TranslateString (LanguageHandle,'Maximum spill flow of waste water system')
           LongDSRMAP(1,15) = TranslateString (LanguageHandle,'Maximum pumped flow of waste water system')
           LongDSRMAP(1,16) = TranslateString (LanguageHandle,'Maximum evaporation from street')
           LongDSRMAP(1,17) = TranslateString (LanguageHandle,'Maximum Dynamic Storage')
        else
           LongDSRMAP(1,1)  = TranslateString (LanguageHandle,'Storage in combined sewer / storm water system')
           LongDSRMAP(1,2)  = TranslateString (LanguageHandle,'Storage in waste water system')
           LongDSRMAP(1,3)  = TranslateString (LanguageHandle,'Storage on street')
           LongDSRMAP(1,4)  = TranslateString (LanguageHandle,'Spill flow')
           LongDSRMAP(1,5)  = TranslateString (LanguageHandle,'Pumped flow')
           LongDSRMAP(1,6)  = TranslateString (LanguageHandle,'Flow to open water')
           LongDSRMAP(1,7)  = TranslateString (LanguageHandle,'Rainfall')
           LongDSRMAP(1,8)  = TranslateString (LanguageHandle,'Dry weather flow into combined sewer / storm water sewer system')
           LongDSRMAP(1,9)  = TranslateString (LanguageHandle,'Dry weather flow into waste water system')
           LongDSRMAP(1,10) = TranslateString (LanguageHandle,'Flow from street to combined sewer / storm water system')
           LongDSRMAP(1,11) = TranslateString (LanguageHandle,'Flow from storm water system to waste water system')
           LongDSRMAP(1,12) = TranslateString (LanguageHandle,'Spill from combined sewer / storm water sewer system')
           LongDSRMAP(1,13) = TranslateString (LanguageHandle,'Pumped flow from combined sewer / storm water system')
           LongDSRMAP(1,14) = TranslateString (LanguageHandle,'Spill flow from waste water system')
           LongDSRMAP(1,15) = TranslateString (LanguageHandle,'Pumped flow from waste water system')
           LongDSRMAP(1,16) = TranslateString (LanguageHandle,'Evaporation from street')
           LongDSRMAP(1,17) = TranslateString (LanguageHandle,'Dynamic Storage')
        endif

! ***                          ONVERHARD GEBIED
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
           LongDSRMAP(2,1)  = TranslateString (LanguageHandle,'Maximum SurfaceRunoff')
           LongDSRMAP(2,2)  = TranslateString (LanguageHandle,'Maximum Groundwater outflow')
           LongDSRMAP(2,3)  = TranslateString (LanguageHandle,'Maximum Rainfall')
           LongDSRMAP(2,4)  = TranslateString (LanguageHandle,'Maximum Evaporation from surface')
           LongDSRMAP(2,5)  = TranslateString (LanguageHandle,'Maximum Infiltration')
           LongDSRMAP(2,6)  = TranslateString (LanguageHandle,'Maximum Net Seepage')
           LongDSRMAP(2,7)  = TranslateString (LanguageHandle,'Maximum Actual evapotranspiration')
           LongDSRMAP(2,8)  = TranslateString (LanguageHandle,'Maximum Potential evapotranspiration')
           LongDSRMAP(2,9)  = TranslateString (LanguageHandle,'Maximum Percolation')
           LongDSRMAP(2,10) = TranslateString (LanguageHandle,'Maximum Capillary rise')
           LongDSRMAP(2,11) = TranslateString (LanguageHandle,'Maximum Groundwater level')
           LongDSRMAP(2,12) = TranslateString (LanguageHandle,'Maximum Inundation percentage')
           LongDSRMAP(2,13) = TranslateString (LanguageHandle,'Maximum Storage depth on land')
           LongDSRMAP(2,14) = TranslateString (LanguageHandle,'Maximum Volume Groundwater')
           LongDSRMAP(2,15) = TranslateString (LanguageHandle,'Maximum Volume on land')
           LongDSRMAP(2,16) = TranslateString (LanguageHandle,'Groundwater threshold exceedance')
           LongDSRMAP(2,17) = TranslateString (LanguageHandle,'Maximum groundater level below surface')
           LongDSRMAP(2,18) = TranslateString (LanguageHandle,'Maximum Irrigation Supply')
           LongDSRMAP(2,19) = TranslateString (LanguageHandle,'Maximum Irrigation Groundwater demand')
           LongDSRMAP(2,20) = TranslateString (LanguageHandle,'Maximum Storage Coefficient')
           LongDSRMAP(2,21) = TranslateString (LanguageHandle,'Maximum Unsaturated Zone depth in mm')
           LongDSRMAP(2,22) = TranslateString (LanguageHandle,'Maximum Volume unsaturated zone')
        else
           LongDSRMAP(2,1)  = TranslateString (LanguageHandle,'Surface Runoff')
           LongDSRMAP(2,2)  = TranslateString (LanguageHandle,'Groundwater outflow')
           LongDSRMAP(2,3)  = TranslateString (LanguageHandle,'Rainfall')
           LongDSRMAP(2,4)  = TranslateString (LanguageHandle,'Evaporation from surface')
           LongDSRMAP(2,5)  = TranslateString (LanguageHandle,'Infiltration')
           LongDSRMAP(2,6)  = TranslateString (LanguageHandle,'Net Seepage ')
           LongDSRMAP(2,7)  = TranslateString (LanguageHandle,'Actual Evapotranspiration')
           LongDSRMAP(2,8)  = TranslateString (LanguageHandle,'Potential Evapotranspiration')
           LongDSRMAP(2,9)  = TranslateString (LanguageHandle,'Percolation')
           LongDSRMAP(2,10) = TranslateString (LanguageHandle,'Capillary rise')
           LongDSRMAP(2,11) = TranslateString (LanguageHandle,'Groundwater level')
           LongDSRMAP(2,12) = TranslateString (LanguageHandle,'Inundation percentage')
           LongDSRMAP(2,13) = TranslateString (LanguageHandle,'Storage on land - depth in mm')
           LongDSRMAP(2,14) = TranslateString (LanguageHandle,'Groundwater volume')
           LongDSRMAP(2,15) = TranslateString (LanguageHandle,'Storage on land - volume in m3')
           LongDSRMAP(2,16) = TranslateString (LanguageHandle,'Groundwater above threshold')
           LongDSRMAP(2,17) = TranslateString (LanguageHandle,'Groundwater level below Surface')
           LongDSRMAP(2,18) = TranslateString (LanguageHandle,'Irrigation supply')
           LongDSRMAP(2,19) = TranslateString (LanguageHandle,'Irrigation groundwater demand')
           LongDSRMAP(2,20) = TranslateString (LanguageHandle,'Storage coefficient')
           LongDSRMAP(2,21) = TranslateString (LanguageHandle,'Unsaturated zone - depth in mm')
           LongDSRMAP(2,22) = TranslateString (LanguageHandle,'Unsaturated zone - volume in m3')
        endif

! ***                          KASGEBIED
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
            LongDSRMAP(3,1) = TranslateString (LanguageHandle,'Maximum Storage Basins')
            LongDSRMAP(3,2) = TranslateString (LanguageHandle,'Maximum Outflow')
            LongDSRMAP(3,3) = TranslateString (LanguageHandle,'Maximum Rainfall')
            LongDSRMAP(3,4) = TranslateString (LanguageHandle,'Maximum Evaporation')
            LongDSRMAP(3,5) = TranslateString (LanguageHandle,'Maximum WaterUse')
        else
            LongDSRMAP(3,1) = TranslateString (LanguageHandle,'Storage in basins')
            LongDSRMAP(3,2) = TranslateString (LanguageHandle,'Flow from basins')
            LongDSRMAP(3,3) = TranslateString (LanguageHandle,'Rainfall inflow basins')
            LongDSRMAP(3,4) = TranslateString (LanguageHandle,'Evaporation from basins')
            LongDSRMAP(3,5) = TranslateString (LanguageHandle,'Water use from basins')
        endif
! ***                          OPEN WATER MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
           LongDSRMAP(4,1) = TranslateString (LanguageHandle,'Maximum WaterLevel')
           LongDSRMAP(4,2) = TranslateString (LanguageHandle,'Maximum Water Volume')
           LongDSRMAP(4,3) = TranslateString (LanguageHandle,'Maximum Rainfall')
           LongDSRMAP(4,4) = TranslateString (LanguageHandle,'Maximum Evaporation')
           LongDSRMAP(4,5) = TranslateString (LanguageHandle,'Maximum NetSeepage')
           LongDSRMAP(4,6) = TranslateString (LanguageHandle,'Maximum above Maximum Level')
           LongDSRMAP(4,7) = TranslateString (LanguageHandle,'Maximum Iteration BalanceError')
           LongDSRMAP(4,8) = TranslateString (LanguageHandle,'Maximum Filling percentage')
           LongDSRMAP(4,9) = TranslateString (LanguageHandle,'Maximum Target Level')
        else
           LongDSRMAP(4,1) = TranslateString (LanguageHandle,'Water Level')
           LongDSRMAP(4,2) = TranslateString (LanguageHandle,'OpenWater Volume')
           LongDSRMAP(4,3) = TranslateString (LanguageHandle,'Rainfall')
           LongDSRMAP(4,4) = TranslateString (LanguageHandle,'Evaporation')
           LongDSRMAP(4,5) = TranslateString (LanguageHandle,'Net Seepage')
           LongDSRMAP(4,6) = TranslateString (LanguageHandle,'Exceedance of maximum level')
           LongDSRMAP(4,7) = TranslateString (LanguageHandle,'Iteration BalanceError')
           LongDSRMAP(4,8) = TranslateString (LanguageHandle,'Filling percentage')
           LongDSRMAP(4,9) = TranslateString (LanguageHandle,'Target level')
        endif
! ***                          STRUCTURES MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
           LongDSRMAP(5,1) = TranslateString (LanguageHandle,'Maximum Flow')
           LongDSRMAP(5,2) = TranslateString (LanguageHandle,'Maximum Crestlevel (for weir) or Opening height (for gate)')
           LongDSRMAP(5,3) = TranslateString (LanguageHandle,'Maximum Flow pump, orifice or weir first stage')
           LongDSRMAP(5,4) = TranslateString (LanguageHandle,'Maximum Flow second stage of 2-stage weir')
        else
           LongDSRMAP(5,1) = TranslateString (LanguageHandle,'Flow')
           LongDSRMAP(5,2) = TranslateString (LanguageHandle,'Weir crest level or gate opening height')
           LongDSRMAP(5,3) = TranslateString (LanguageHandle,'Flow at RR-pump, orifice, or first stage weir')
           LongDSRMAP(5,4) = TranslateString (LanguageHandle,'Flow of 2nd stage at 2-stage weir')
        endif
! ***                          BOUNDARIES MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          LongDSRMAP(6,1) = TranslateString (LanguageHandle,'Maximum  Flow')
          LongDSRMAP(6,2) = TranslateString (LanguageHandle,'Maximum  Boundary level')
        else
          LongDSRMAP(6,1) = TranslateString (LanguageHandle,'Flow')
          LongDSRMAP(6,2) = TranslateString (LanguageHandle,'Boundary Level')
        endif
! ***                          PLUVIUS
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          LongDSRMAP(7,1) = TranslateString (LanguageHandle,'Maximum Discharge to sewer')
          LongDSRMAP(7,2) = TranslateString (LanguageHandle,'Maximum Infiltration from depressions')
          LongDSRMAP(7,3) = TranslateString (LanguageHandle,'Maximum Infiltration from runoff')
          LongDSRMAP(7,4) = TranslateString (LanguageHandle,'Maximum Rainfall')
          LongDSRMAP(7,5) = TranslateString (LanguageHandle,'Maximum Evaporation')
          LongDSRMAP(7,6) = TranslateString (LanguageHandle,'Maximum storm water flow')
          LongDSRMAP(7,7) = TranslateString (LanguageHandle,'Maximum dry weather flow - people')
          LongDSRMAP(7,8) = TranslateString (LanguageHandle,'Maximum Storage Depressions')
          LongDSRMAP(7,9) = TranslateString (LanguageHandle,'Maximum Dynamic Storage')
          LongDSRMAP(7,10) = TranslateString (LanguageHandle,'Maximum Infiltration from surface')
          LongDSRMAP(7,11) = TranslateString (LanguageHandle,'Maximum Infiltration from dynamic storage / runoff')
          LongDSRMAP(7,12) = TranslateString (LanguageHandle,'Maximum inflow sewer1')
          LongDSRMAP(7,13) = TranslateString (LanguageHandle,'Maximum inflow sewer2')
          LongDSRMAP(7,14) = TranslateString (LanguageHandle,'Maximum inflow sewer3')
          LongDSRMAP(7,15) = TranslateString (LanguageHandle,'Maximum inflow sewer4')
          LongDSRMAP(7,16) = TranslateString (LanguageHandle,'Maximum inflow sewer5')
          LongDSRMAP(7,17) = TranslateString (LanguageHandle,'Maximum inflow sewer6')
          LongDSRMAP(7,18) = TranslateString (LanguageHandle,'Maximum inflow sewer7')
          LongDSRMAP(7,19) = TranslateString (LanguageHandle,'Maximum inflow sewer8')
          LongDSRMAP(7,20) = TranslateString (LanguageHandle,'Maximum inflow sewer9')
          LongDSRMAP(7,21) = TranslateString (LanguageHandle,'Maximum inflow sewer10')
          LongDSRMAP(7,22) = TranslateString (LanguageHandle,'Maximum inflow sewer11')
          LongDSRMAP(7,23) = TranslateString (LanguageHandle,'Maximum inflow sewer12')
          LongDSRMAP(7,24) = TranslateString (LanguageHandle,'Maximum inflow sewer special area1')
          LongDSRMAP(7,25) = TranslateString (LanguageHandle,'Maximum inflow sewer special area2')
          LongDSRMAP(7,26) = TranslateString (LanguageHandle,'Maximum inflow sewer special area3')
          LongDSRMAP(7,27) = TranslateString (LanguageHandle,'Maximum inflow sewer special area4')
          LongDSRMAP(7,28) = TranslateString (LanguageHandle,'Maximum inflow sewer special area5')
          LongDSRMAP(7,29) = TranslateString (LanguageHandle,'Maximum inflow sewer special area6')
          LongDSRMAP(7,30) = TranslateString (LanguageHandle,'Maximum inflow sewer special area7')
          LongDSRMAP(7,31) = TranslateString (LanguageHandle,'Maximum inflow sewer special area8')
          LongDSRMAP(7,32) = TranslateString (LanguageHandle,'Maximum inflow sewer special area9')
          LongDSRMAP(7,33) = TranslateString (LanguageHandle,'Maximum inflow sewer special area10')
          LongDSRMAP(7,34) = TranslateString (LanguageHandle,'Maximum inflow sewer special area11')
          LongDSRMAP(7,35) = TranslateString (LanguageHandle,'Maximum inflow sewer special area12')
          LongDSRMAP(7,36) = TranslateString (LanguageHandle,'Maximum Storage sewer special area1')
          LongDSRMAP(7,37) = TranslateString (LanguageHandle,'Maximum Storage sewer special area2')
          LongDSRMAP(7,38) = TranslateString (LanguageHandle,'Maximum Storage sewer special area3')
          LongDSRMAP(7,39) = TranslateString (LanguageHandle,'Maximum Storage sewer special area4')
          LongDSRMAP(7,40) = TranslateString (LanguageHandle,'Maximum Storage sewer special area5')
          LongDSRMAP(7,41) = TranslateString (LanguageHandle,'Maximum Storage sewer special area6')
          LongDSRMAP(7,42) = TranslateString (LanguageHandle,'Maximum Storage sewer special area7')
          LongDSRMAP(7,43) = TranslateString (LanguageHandle,'Maximum Storage sewer special area8')
          LongDSRMAP(7,44) = TranslateString (LanguageHandle,'Maximum Storage sewer special area9')
          LongDSRMAP(7,45) = TranslateString (LanguageHandle,'Maximum Storage sewer special area10')
          LongDSRMAP(7,46) = TranslateString (LanguageHandle,'Maximum Storage sewer special area11')
          LongDSRMAP(7,47) = TranslateString (LanguageHandle,'Maximum Storage sewer special area12')
          LongDSRMAP(7,48) = TranslateString (LanguageHandle,'Maximum Wadi inflow')
          LongDSRMAP(7,49) = TranslateString (LanguageHandle,'Maximum Wadi infiltration')
          LongDSRMAP(7,50) = TranslateString (LanguageHandle,'Maximum Wadi spill')
          LongDSRMAP(7,51) = TranslateString (LanguageHandle,'Maximum Wadi drain outflow')
          LongDSRMAP(7,52) = TranslateString (LanguageHandle,'Maximum Wadi storage')
          LongDSRMAP(7,53) = TranslateString (LanguageHandle,'Maximum Wadi level')
          LongDSRMAP(7,54) = TranslateString (LanguageHandle,'Maximum dry weather flow - companies')
        else
          LongDSRMAP(7,1)  = TranslateString (LanguageHandle,'Inflow sewer')
          LongDSRMAP(7,2)  = TranslateString (LanguageHandle,'Infiltration from storage depressions')
          LongDSRMAP(7,3)  = TranslateString (LanguageHandle,'Infiltration from runoff')
          LongDSRMAP(7,4)  = TranslateString (LanguageHandle,'Rainfall')
          LongDSRMAP(7,5)  = TranslateString (LanguageHandle,'Evaporation')
          LongDSRMAP(7,6)  = TranslateString (LanguageHandle,'storm water flow')
          LongDSRMAP(7,7)  = TranslateString (LanguageHandle,'dry weather flow - people')
          LongDSRMAP(7,8)  = TranslateString (LanguageHandle,'Storage in depressions')
          LongDSRMAP(7,9)  = TranslateString (LanguageHandle,'Dynamic storage')
          LongDSRMAP(7,10) = TranslateString (LanguageHandle,'Infiltration capacity surface storage depressions')
          LongDSRMAP(7,11) = TranslateString (LanguageHandle,'Infiltration capacity from dynamic storage / runoff')
          LongDSRMAP(7,12) = TranslateString (LanguageHandle,'inflow sewer 1')
          LongDSRMAP(7,13) = TranslateString (LanguageHandle,'inflow sewer 2')
          LongDSRMAP(7,14) = TranslateString (LanguageHandle,'inflow sewer 3')
          LongDSRMAP(7,15) = TranslateString (LanguageHandle,'inflow sewer 4')
          LongDSRMAP(7,16) = TranslateString (LanguageHandle,'inflow sewer 5')
          LongDSRMAP(7,17) = TranslateString (LanguageHandle,'inflow sewer 6')
          LongDSRMAP(7,18) = TranslateString (LanguageHandle,'inflow sewer 7')
          LongDSRMAP(7,19) = TranslateString (LanguageHandle,'inflow sewer 8')
          LongDSRMAP(7,20) = TranslateString (LanguageHandle,'inflow sewer 9')
          LongDSRMAP(7,21) = TranslateString (LanguageHandle,'inflow sewer 10')
          LongDSRMAP(7,22) = TranslateString (LanguageHandle,'inflow sewer 11')
          LongDSRMAP(7,23) = TranslateString (LanguageHandle,'inflow sewer 12')
          LongDSRMAP(7,24) = TranslateString (LanguageHandle,'inflow sewer special area1')
          LongDSRMAP(7,25) = TranslateString (LanguageHandle,'inflow sewer special area2')
          LongDSRMAP(7,26) = TranslateString (LanguageHandle,'inflow sewer special area3')
          LongDSRMAP(7,27) = TranslateString (LanguageHandle,'inflow sewer special area4')
          LongDSRMAP(7,28) = TranslateString (LanguageHandle,'inflow sewer special area5')
          LongDSRMAP(7,29) = TranslateString (LanguageHandle,'inflow sewer special area6')
          LongDSRMAP(7,30) = TranslateString (LanguageHandle,'inflow sewer special area7')
          LongDSRMAP(7,31) = TranslateString (LanguageHandle,'inflow sewer special area8')
          LongDSRMAP(7,32) = TranslateString (LanguageHandle,'inflow sewer special area9')
          LongDSRMAP(7,33) = TranslateString (LanguageHandle,'inflow sewer special area10')
          LongDSRMAP(7,34) = TranslateString (LanguageHandle,'inflow sewer special area11')
          LongDSRMAP(7,35) = TranslateString (LanguageHandle,'inflow sewer special area12')
          LongDSRMAP(7,36) = TranslateString (LanguageHandle,'Storage sewer special area1')
          LongDSRMAP(7,37) = TranslateString (LanguageHandle,'Storage sewer special area2')
          LongDSRMAP(7,38) = TranslateString (LanguageHandle,'Storage sewer special area3')
          LongDSRMAP(7,39) = TranslateString (LanguageHandle,'Storage sewer special area4')
          LongDSRMAP(7,40) = TranslateString (LanguageHandle,'Storage sewer special area5')
          LongDSRMAP(7,41) = TranslateString (LanguageHandle,'Storage sewer special area6')
          LongDSRMAP(7,42) = TranslateString (LanguageHandle,'Storage sewer special area7')
          LongDSRMAP(7,43) = TranslateString (LanguageHandle,'Storage sewer special area8')
          LongDSRMAP(7,44) = TranslateString (LanguageHandle,'Storage sewer special area9')
          LongDSRMAP(7,45) = TranslateString (LanguageHandle,'Storage sewer special area10')
          LongDSRMAP(7,46) = TranslateString (LanguageHandle,'Storage sewer special area11')
          LongDSRMAP(7,47) = TranslateString (LanguageHandle,'Storage sewer special area12')
          LongDSRMAP(7,48) = TranslateString (LanguageHandle,'Wadi inflow')
          LongDSRMAP(7,49) = TranslateString (LanguageHandle,'Wadi infiltration')
          LongDSRMAP(7,50) = TranslateString (LanguageHandle,'Wadi spill')
          LongDSRMAP(7,51) = TranslateString (LanguageHandle,'Wadi drain outflow')
          LongDSRMAP(7,52) = TranslateString (LanguageHandle,'Wadi storage')
          LongDSRMAP(7,53) = TranslateString (LanguageHandle,'Wadi level')
          LongDSRMAP(7,54) = TranslateString (LanguageHandle,'dry weather flow - companies')
        endif
! ***                          BALANS MAP
        LongDSRMAP(8,1) = TranslateString (LanguageHandle,'Total Inflow')
        LongDSRMAP(8,2) = TranslateString (LanguageHandle,'Total Outflow')
        LongDSRMAP(8,3) = TranslateString (LanguageHandle,'Delta Storage')
        LongDSRMAP(8,4) = TranslateString (LanguageHandle,'Cumulative Inflow')
        LongDSRMAP(8,5) = TranslateString (LanguageHandle,'Cumulative Outflow')
        LongDSRMAP(8,6) = TranslateString (LanguageHandle,'Cumulative Delta Storage')

        If (ExtendedBalanceOutput) then
          LongDSRMAP(8,1)  = TranslateString (LanguageHandle,'Total In At Node')
          LongDSRMAP(8,2)  = TranslateString (LanguageHandle,'Total In Via Links')
          LongDSRMAP(8,3)  = TranslateString (LanguageHandle,'Total Out At Node')
          LongDSRMAP(8,4)  = TranslateString (LanguageHandle,'Total Out Via Links')
          LongDSRMAP(8,5)  = TranslateString (LanguageHandle,'Delta Storage')
          LongDSRMAP(8,6)  = TranslateString (LanguageHandle,'Balance Error')
          LongDSRMAP(8,7)  = TranslateString (LanguageHandle,'Cumulative In At Node')
          LongDSRMAP(8,8)  = TranslateString (LanguageHandle,'Cumulative In Via Links')
          LongDSRMAP(8,9)  = TranslateString (LanguageHandle,'Cumulative Out At Node')
          LongDSRMAP(8,10) = TranslateString (LanguageHandle,'Cumulative Out Via Links')
          LongDSRMAP(8,11) = TranslateString (LanguageHandle,'Cumulative Delta Storage')
          LongDSRMAP(8,12) = TranslateString (LanguageHandle,'Cumulative Balance Error')
        Endif
! ***                          ZOUT MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          LongDSRMAP(9,1) = TranslateString (LanguageHandle,'Maximum Concentration')
        else
          LongDSRMAP(9,1) = TranslateString (LanguageHandle,'Concentration')
        endif
! ***                          RWZI MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          LongDSRMAP(10,1) = TranslateString (LanguageHandle,'Maximum InFlow')
          LongDSRMAP(10,2) = TranslateString (LanguageHandle,'Maximum OutFlow')
        else
          LongDSRMAP(10,1) = TranslateString (LanguageHandle,'Inflow')
          LongDSRMAP(10,2) = TranslateString (LanguageHandle,'Outflow')
        endif
! ***                          Industry MAP
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          LongDSRMAP(11,1) = TranslateString (LanguageHandle,'Maximum Demand')
          LongDSRMAP(11,2) = TranslateString (LanguageHandle,'Maximum Allocation')
          LongDSRMAP(11,3) = TranslateString (LanguageHandle,'Maximum Shortage')
          LongDSRMAP(11,4) = TranslateString (LanguageHandle,'Maximum Discharge')
        else
          LongDSRMAP(11,1) = TranslateString (LanguageHandle,'Demand')
          LongDSRMAP(11,2) = TranslateString (LanguageHandle,'Allocation')
          LongDSRMAP(11,3) = TranslateString (LanguageHandle,'Shortage')
          LongDSRMAP(11,4) = TranslateString (LanguageHandle,'Discharge')
        endif
! ***                          Sacramento
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          LongDSRMAP(12,1) = TranslateString (LanguageHandle,'Maximum Upper zome Tension water content')
          LongDSRMAP(12,2) = TranslateString (LanguageHandle,'Maximum Upper zone Free water content')
          LongDSRMAP(12,3) = TranslateString (LanguageHandle,'Maximum Lower zone Tension water content')
          LongDSRMAP(12,4) = TranslateString (LanguageHandle,'Maximum Lower zone free primary water content')
          LongDSRMAP(12,5) = TranslateString (LanguageHandle,'Maximum Lower zone free secondary water content')
          LongDSRMAP(12,6) = TranslateString (LanguageHandle,'Maximum precipitation')
          LongDSRMAP(12,7) = TranslateString (LanguageHandle,'Maximum Potential Evapotranspiration')
          LongDSRMAP(12,8) = TranslateString (LanguageHandle,'Maximum Actual Evapotranpiration')
          LongDSRMAP(12,9) = TranslateString (LanguageHandle,'Maximum Baseflow')
          LongDSRMAP(12,10)= TranslateString (LanguageHandle,'Maximum Total routed Surface runoff ')
          LongDSRMAP(12,11)= TranslateString (LanguageHandle,'Maximum Unrouted Runoff Impervious area')
          LongDSRMAP(12,12)= TranslateString (LanguageHandle,'Maximum Total runoff ')
          LongDSRMAP(12,13)= TranslateString (LanguageHandle,'Maximum Channel inflow')
          LongDSRMAP(12,14)= TranslateString (LanguageHandle,'Maximum Side+SSout')
          LongDSRMAP(12,15)= TranslateString (LanguageHandle,'Maximum ADIMPContent')
          LongDSRMAP(12,16)= TranslateString (LanguageHandle,'Maximum Unrouted Surface Runoff')
          LongDSRMAP(12,17)= TranslateString (LanguageHandle,'Maximum Unrouted Interflow runoff')
          LongDSRMAP(12,18)= TranslateString (LanguageHandle,'Maximum Routed Direct runoff Impervious area')
          LongDSRMAP(12,19)= TranslateString (LanguageHandle,'Maximum Routed Surface runoff')
          LongDSRMAP(12,20)= TranslateString (LanguageHandle,'Maximum Routed Interflow runoff')
          LongDSRMAP(12,21)= TranslateString (LanguageHandle,'Maximum total percolation to lower zone')
        else
          LongDSRMAP(12,1) = TranslateString (LanguageHandle,'Upper Zone Tension Water Content')
          LongDSRMAP(12,2) = TranslateString (LanguageHandle,'Upper Zone Free Water Content')
          LongDSRMAP(12,3) = TranslateString (LanguageHandle,'Lower Zone Tension Water Content')
          LongDSRMAP(12,4) = TranslateString (LanguageHandle,'Lower Zone Free Primary water Content')
          LongDSRMAP(12,5) = TranslateString (LanguageHandle,'Lower Zone Free Secondary water Content')
          LongDSRMAP(12,6) = TranslateString (LanguageHandle,'Precipitation ')
          LongDSRMAP(12,7) = TranslateString (LanguageHandle,'Potential Evapotranspiration')
          LongDSRMAP(12,8) = TranslateString (LanguageHandle,'Actual Evapotranspiration')
          LongDSRMAP(12,9) = TranslateString (LanguageHandle,'Computed Baseflow')
          LongDSRMAP(12,10)= TranslateString (LanguageHandle,'Total routed Surface Runoff')
          LongDSRMAP(12,11)= TranslateString (LanguageHandle,'Unrouted Runoff Impervious area')
          LongDSRMAP(12,12)= TranslateString (LanguageHandle,'Total Runoff')
          LongDSRMAP(12,13)= TranslateString (LanguageHandle,'Channel Inflow')
          LongDSRMAP(12,14)= TranslateString (LanguageHandle,'Side+SSoutflow')
          LongDSRMAP(12,15)= TranslateString (LanguageHandle,'AdimC Contents')
          LongDSRMAP(12,16)= TranslateString (LanguageHandle,'Unrouted Surface runoff')
          LongDSRMAP(12,17)= TranslateString (LanguageHandle,'Unrouted Interflow runoff')
          LongDSRMAP(12,18)= TranslateString (LanguageHandle,'Routed Direct runoff Impervious area')
          LongDSRMAP(12,19)= TranslateString (LanguageHandle,'Routed Surface runoff ')
          LongDSRMAP(12,20)= TranslateString (LanguageHandle,'Routed Interflow runoff')
          LongDSRMAP(12,21)= TranslateString (LanguageHandle,'Total Percolation to lower zone')
        endif
! ***                          Link Flows Map
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          LongDSRMAP(13,1) = TranslateString (LanguageHandle,'Maximum LinkFlow')
          if (RoutingLinkExists) then
             LongDSRMAP(13,2) = TranslateString (LanguageHandle,'Maximum Inflow')
             LongDSRMAP(13,3) = TranslateString (LanguageHandle,'Maximum Outflow')
          endif
          If (NcOwRain .gt. 0) then
             if (RoutingLinkExists) then
                LongDSRMAP(13,4) = TranslateString (LanguageHandle,'Maximum Precipitation')
                LongDSRMAP(13,5) = TranslateString (LanguageHandle,'Maximum Evaporation. ')
             else
                LongDSRMAP(13,2) = TranslateString (LanguageHandle,'Maximum Precipitation')
                LongDSRMAP(13,3) = TranslateString (LanguageHandle,'Maximum Evaporation. ')
             endif
          endif
        else
          DSRMAP(13,1) = TranslateString (LanguageHandle,'Link flow [m3/s]')
          LongDSRMAP(13,1) = TranslateString (LanguageHandle,'Link flow')
          if (RoutingLinkExists) then
             LongDSRMAP(13,2) = TranslateString (LanguageHandle,'Link inflow')
             LongDSRMAP(13,3) = TranslateString (LanguageHandle,'Link outflow')
          endif
          If (NcOwRain .gt. 0) then
             if (RoutingLinkExists) then
                LongDSRMAP(13,4) = TranslateString (LanguageHandle,'Open water precipitation')
                LongDSRMAP(13,5) = TranslateString (LanguageHandle,'Open water evaporation')
              else
                LongDSRMAP(13,2) = TranslateString (LanguageHandle,'Open water precipitatation')
                LongDSRMAP(13,3) = TranslateString (LanguageHandle,'Open water evaporation')
            endif
          endif
        endif

! ***                          Cel
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
            ! to be added: series output
        else
!          node type inactivated
        endif
! ***                          RRRunoff Map
        if (Nevent .gt. 1 .and. Iopt1(3) .ne. 1) then
          LongDSRMAP(15,1) = TranslateString  (LanguageHandle,'Maximum Rainfall')
          LongDSRMAP(15,2) = TranslateString  (LanguageHandle,'Maximum Potential Evapotranspiration')
          LongDSRMAP(15,3) = TranslateString  (LanguageHandle,'Maximum Actual Evapotranspiration')
          LongDSRMAP(15,4) = TranslateString  (LanguageHandle,'Maximum total catchment runoff')
          LongDSRMAP(15,5) = TranslateString  (LanguageHandle,'Maximum total catchment outflow')
          if (NCRRRUnoffHBV .gt. 0) then
             LongDSRMAP(15,NStartHBV) = TranslateString  (LanguageHandle,'Maximum HBV Snowfall ')
             LongDSRMAP(15,NStartHBV+1) = TranslateString  (LanguageHandle,'Maximum HBV BaseFlow ')
             LongDSRMAP(15,NStartHBV+2) = TranslateString  (LanguageHandle,'Maximum HBV InterFlow')
             LongDSRMAP(15,NStartHBV+3) = TranslateString  (LanguageHandle,'Maximum HBV QuickFlow')
             LongDSRMAP(15,NStartHBV+4) = TranslateString (LanguageHandle,'Maximum HBV Dry Snow Content')
             LongDSRMAP(15,NStartHBV+5) = TranslateString (LanguageHandle,'Maximum HBV Free Water Content')
             LongDSRMAP(15,NStartHBV+6) = TranslateString (LanguageHandle,'Maximum HBV Soil Moisture')
             LongDSRMAP(15,NStartHBV+7) = TranslateString (LanguageHandle,'Maximum HBV Upper Zone Content')
             LongDSRMAP(15,NStartHBV+8) = TranslateString (LanguageHandle,'Maximum HBV Lower Zone Content')
             LongDSRMAP(15,NStartHBV+9) = TranslateString (LanguageHandle,'Maximum HBV_Temperature')
             LongDSRMAP(15,NStartHBV+10) = TranslateString (LanguageHandle,'Maximum HBV Snowmelt')
             LongDSRMAP(15,NStartHBV+11) = TranslateString (LanguageHandle,'Maximum HBV Refreezing')
             LongDSRMAP(15,NStartHBV+12) = TranslateString (LanguageHandle,'Maximum HBV Infiltration')
             LongDSRMAP(15,NStartHBV+13) = TranslateString (LanguageHandle,'Maximum HBV Seepage')
             LongDSRMAP(15,NStartHBV+14) = TranslateString (LanguageHandle,'Maximum HBV Direct Runoff')
             LongDSRMAP(15,NStartHBV+15) = TranslateString (LanguageHandle,'Maximum HBV Percolation')
          endif
          if (NCRRRUnoffSCS .gt. 0) then
              LongDSRMAP(15,NStartSCS) = TranslateString (LanguageHandle,'Maximum SCS_Storage')
          endif
          if (NCRRRUnoffNAM .gt. 0) then
              LongDSRMAP(15,NStartNAM   ) = TranslateString (LanguageHandle,'Maximum D-NAM External Water Level')
              LongDSRMAP(15,NStartNAM+1 ) = TranslateString (LanguageHandle,'Maximum D-NAM Groundwater Pump DefinedDischarge')
              LongDSRMAP(15,NStartNAM+2 ) = TranslateString (LanguageHandle,'Maximum D-NAM Evaporation from SurfaceStorage')
              LongDSRMAP(15,NStartNAM+3 ) = TranslateString (LanguageHandle,'Maximum D-NAM Evapotranspiration from Rootzone layer ')
              LongDSRMAP(15,NStartNAM+4 ) = TranslateString (LanguageHandle,'Maximum D-NAM Evapotranspiration from Rootzone part of groundwater storage')
              LongDSRMAP(15,NStartNAM+5 ) = TranslateString (LanguageHandle,'Maximum D-NAM Evapotranspiration from Lower zone storage')
              LongDSRMAP(15,NStartNAM+6 ) = TranslateString (LanguageHandle,'Maximum D-NAM Overland flow')
              LongDSRMAP(15,NStartNAM+7 ) = TranslateString (LanguageHandle,'Maximum D-NAM Interflow')
              LongDSRMAP(15,NStartNAM+8 ) = TranslateString (LanguageHandle,'Maximum D-NAM Fast Baseflow ')
              LongDSRMAP(15,NStartNAM+9 ) = TranslateString (LanguageHandle,'Maximum D-NAM Slow Baseflow ')
              LongDSRMAP(15,NStartNAM+10) = TranslateString (LanguageHandle,'Maximum D-NAM External Groundwater inflow')
              LongDSRMAP(15,NStartNAM+11) = TranslateString (LanguageHandle,'Maximum D-NAM External Groundwater Inflow into Lower zone storage')
              LongDSRMAP(15,NStartNAM+12) = TranslateString (LanguageHandle,'Maximum D-NAM External Groundwater Inflow into Groundwater storage')
              LongDSRMAP(15,NStartNAM+13) = TranslateString (LanguageHandle,'Maximum D-NAM Groundwater Pump Abstraction from Groundwater storage')
              LongDSRMAP(15,NStartNAM+14) = TranslateString (LanguageHandle,'Maximum D-NAM Groundwater Pump Supply to Lower zone storage')
              LongDSRMAP(15,NStartNAM+15) = TranslateString (LanguageHandle,'Maximum D-NAM Groundwater Pump Supply to Groundwater storage')
              LongDSRMAP(15,NStartNAM+16) = TranslateString (LanguageHandle,'Maximum D-NAM Infiltration ')
              LongDSRMAP(15,NStartNAM+17) = TranslateString (LanguageHandle,'Maximum D-NAM Infiltration Lower zone storage ')
              LongDSRMAP(15,NStartNAM+18) = TranslateString (LanguageHandle,'Maximum D-NAM Percolation ')
              LongDSRMAP(15,NStartNAM+19) = TranslateString (LanguageHandle,'Maximum D-NAM Capillary Rise ')
              LongDSRMAP(15,NStartNAM+20) = TranslateString (LanguageHandle,'Maximum D-NAM Depth Surface storage ')
              LongDSRMAP(15,NStartNAM+21) = TranslateString (LanguageHandle,'Maximum D-NAM Depth LowerZone storage ')
              LongDSRMAP(15,NStartNAM+22) = TranslateString (LanguageHandle,'Maximum D-NAM Depth Groundwater storage ')
              LongDSRMAP(15,NStartNAM+23) = TranslateString (LanguageHandle,'Maximum D-NAM Volume Surface storage ')
              LongDSRMAP(15,NStartNAM+24) = TranslateString (LanguageHandle,'Maximum D-NAM Volume Lower zone storage ')
              LongDSRMAP(15,NStartNAM+25) = TranslateString (LanguageHandle,'Maximum D-NAM Volume Groundwater storage ')
              LongDSRMAP(15,NStartNAM+26) = TranslateString (LanguageHandle,'Maximum D-NAM GroundWater level')
              LongDSRMAP(15,NStartNAM+27) = TranslateString (LanguageHandle,'Maximum D-NAM GroundWater table depth ')
              LongDSRMAP(15,NStartNAM+28) = TranslateString (LanguageHandle,'Maximum D-NAM Available soil storage ')
              LongDSRMAP(15,NStartNAM+29) = TranslateString (LanguageHandle,'Maximum D-NAM BaseFlow ')
              LongDSRMAP(15,NStartNAM+30) = TranslateString (LanguageHandle,'Maximum D-NAM Groundwater Pump Actual Discharge ')
              LongDSRMAP(15,NStartNAM+31) = TranslateString (LanguageHandle,'Maximum D-NAM Groundwater Pump Actual Discharge minus Defined Discharge')
          endif
          if (NCRRRUnoffLGSI .gt. 0) then
              LongDSRMAP(15,NStartLGSI) = TranslateString (LanguageHandle,'Maximum LGSI Rainfall1 ')
              LongDSRMAP(15,NStartLGSI+1) = TranslateString (LanguageHandle,'Maximum LGSI Rainfall2 ')
              LongDSRMAP(15,NStartLGSI+2) = TranslateString (LanguageHandle,'Maximum LGSI Potential Evapotranspiration1 ')
              LongDSRMAP(15,NStartLGSI+3) = TranslateString (LanguageHandle,'Maximum LGSI Potential Evapotranspiration2 ')
              LongDSRMAP(15,NStartLGSI+4) = TranslateString (LanguageHandle,'Maximum LGSI Actual Evapotranspiration1 ')
              LongDSRMAP(15,NStartLGSI+5) = TranslateString (LanguageHandle,'Maximum LGSI Actual Evapotranspiration2 ')
              LongDSRMAP(15,NStartLGSI+6) = TranslateString (LanguageHandle,'Maximum LGSI Recharge1  ')
              LongDSRMAP(15,NStartLGSI+7) = TranslateString (LanguageHandle,'Maximum LGSI Recharge2  ')
              LongDSRMAP(15,NStartLGSI+8) = TranslateString (LanguageHandle,'Maximum LGSI Drainage flow1 ')
              LongDSRMAP(15,NStartLGSI+9) = TranslateString (LanguageHandle,'Maximum LGSI Drainage flow2 ')
              LongDSRMAP(15,NStartLGSI+10) = TranslateString (LanguageHandle,'Maximum LGSI Seepage from 2-1 ')
              LongDSRMAP(15,NStartLGSI+11) = TranslateString (LanguageHandle,'Maximum LGSI Overland flow1')
              LongDSRMAP(15,NStartLGSI+12) = TranslateString (LanguageHandle,'Maximum LGSI Overland flow2')
              LongDSRMAP(15,NStartLGSI+13) = TranslateString (LanguageHandle,'Maximum LGSI QpDirect1  ')
              LongDSRMAP(15,NStartLGSI+14) = TranslateString (LanguageHandle,'Maximum LGSI QpDirect2  ')
              LongDSRMAP(15,NStartLGSI+15) = TranslateString (LanguageHandle,'Maximum LGSI Riverflow1 ')
              LongDSRMAP(15,NStartLGSI+16) = TranslateString (LanguageHandle,'Maximum LGSI Riverflow2 ')
              LongDSRMAP(15,NStartLGSI+17) = TranslateString (LanguageHandle,'Maximum LGSI Overland Storage1')
              LongDSRMAP(15,NStartLGSI+18) = TranslateString (LanguageHandle,'Maximum LGSI Overland Storage2')
              LongDSRMAP(15,NStartLGSI+19) = TranslateString (LanguageHandle,'Maximum LGSI Groundwater Storage1')
              LongDSRMAP(15,NStartLGSI+20) = TranslateString (LanguageHandle,'Maximum LGSI Groundwater Storage2')
              LongDSRMAP(15,NStartLGSI+21) = TranslateString (LanguageHandle,'Maximum LGSI New Volume1')
              LongDSRMAP(15,NStartLGSI+22) = TranslateString (LanguageHandle,'Maximum LGSI New Volume2')
              LongDSRMAP(15,NStartLGSI+23) = TranslateString (LanguageHandle,'Maximum LGSI Groundwater table depth1 in meter below surface')
              LongDSRMAP(15,NStartLGSI+24) = TranslateString (LanguageHandle,'Maximum LGSI Groundwater table depth2 in meter below surface')
              LongDSRMAP(15,NStartLGSI+25) = TranslateString (LanguageHandle,'Maximum LGSI RunoffDelay')
              LongDSRMAP(15,NStartLGSI+26) = TranslateString (LanguageHandle,'Maximum LGSI Qtot     ')
              LongDSRMAP(15,NStartLGSI+27) = TranslateString (LanguageHandle,'Maximum LGSI Qdelayed ')
              LongDSRMAP(15,NStartLGSI+28) = TranslateString (LanguageHandle,'Maximum LGSI Groundwater level1')
              LongDSRMAP(15,NStartLGSI+29) = TranslateString (LanguageHandle,'Maximum LGSI Groundwater level2')
          endif
          if (NCRRRUnoffWagMod .gt. 0) then
             LongDSRMAP(15,NStartWagMod) = TranslateString (LanguageHandle,'Maximum WagMod BaseFlow')
             LongDSRMAP(15,NStartWagMod+1) = TranslateString (LanguageHandle,'Maximum WagMod QuickFlow')
             LongDSRMAP(15,NStartWagMod+2) = TranslateString (LanguageHandle,'Maximum WagMod SoilMoisture')
             LongDSRMAP(15,NStartWagMod+3) = TranslateString (LanguageHandle,'Maximum WagMod Groundwater store   ')
             LongDSRMAP(15,NStartWagMod+4) = TranslateString (LanguageHandle,'Maximum WagMod Seepage  ')
          endif
          if (NCRRRunoffWalrus .gt. 0) then
             LongDSRMAP(15,NStartWalrus)   = TranslateString (LanguageHandle,'Max. Walrus storage deficit in the vadose zone of the soil water reservoir')
             LongDSRMAP(15,NStartWalrus+1) = TranslateString (LanguageHandle,'Max. Walrus groundwater table depth')
             LongDSRMAP(15,NStartWalrus+2) = TranslateString (LanguageHandle,'Max. Walrus water depth in the quickflow reservoir (hQ)')
             LongDSRMAP(15,NStartWalrus+3) = TranslateString (LanguageHandle,'Max. Walrus water depth in the surface water reservoir (hS)')
             LongDSRMAP(15,NStartWalrus+4) = TranslateString (LanguageHandle,'Max. Walrus defined ext.supply to/ extraction from surface water reservoir (fXS)')
             LongDSRMAP(15,NStartWalrus+5) = TranslateString (LanguageHandle,'Max. Walrus defined ext.supply to/ extraction from soil water reservoir (fXG)')
             LongDSRMAP(15,NStartWalrus+6) = TranslateString (LanguageHandle,'Max. Walrus precipitation added to quickflow reservoir ')
             LongDSRMAP(15,NStartWalrus+7) = TranslateString (LanguageHandle,'Max. Walrus precipitation added to the vadose zone of the soil water reservoir')
             LongDSRMAP(15,NStartWalrus+8) = TranslateString (LanguageHandle,'Max. Walrus precipitation added to surface water')
             LongDSRMAP(15,NStartWalrus+9) = TranslateString (LanguageHandle,'Max. Walrus actual transpiration from land / vadose zone of soil water reservoir')
             LongDSRMAP(15,NStartWalrus+10) = TranslateString (LanguageHandle,'Max. Walrus actual evaporation from surface water')
             LongDSRMAP(15,NStartWalrus+11) = TranslateString (LanguageHandle,'Max. Walrus equilibrium water-storage deficit in the vadose zone ')
             LongDSRMAP(15,NStartWalrus+12) = TranslateString (LanguageHandle,'Max. Walrus discharge from quickflow reservoir to surface water reservoir (fQS)')
             LongDSRMAP(15,NStartWalrus+13) = TranslateString (LanguageHandle,'Max. Walrus drainage to / infiltration from surface water reservoir (fGS)')
             LongDSRMAP(15,NStartWalrus+14) = TranslateString (LanguageHandle,'Max. Walrus discharge from the surface water reservoir ')
             LongDSRMAP(15,NStartWalrus+15) = TranslateString (LanguageHandle,'Max. Walrus soil wetness index ')
             LongDSRMAP(15,NStartWalrus+16) = TranslateString (LanguageHandle,'Max. Walrus transpiration reduction factor')
             LongDSRMAP(15,NStartWalrus+17) = TranslateString (LanguageHandle,'Max. Walrus depth in surface water reservoir for which discharge=0 (hSmin)')
             LongDSRMAP(15,NStartWalrus+18) = TranslateString (LanguageHandle,'Max. Walrus actual ext.supply to / extraction from surface water reservoir (fXS)')
          endif
        else
          LongDSRMAP(15,1) = TranslateString  (LanguageHandle,'Rainfall')
          LongDSRMAP(15,2) = TranslateString  (LanguageHandle,'Potential Evapotranspiration')
          LongDSRMAP(15,3) = TranslateString  (LanguageHandle,'Actual Evapotranspiration')
          LongDSRMAP(15,4) = TranslateString  (LanguageHandle,'Total catchment runoff')
          LongDSRMAP(15,5) = TranslateString  (LanguageHandle,'Catchment outflow')
          if (NCRRRUnoffHBV .gt. 0) then
             LongDSRMAP(15,NStartHBV) = TranslateString  (LanguageHandle,'HBV Snowfall ')
             LongDSRMAP(15,NStartHBV+1) = TranslateString  (LanguageHandle,'HBV BaseFlow ')
             LongDSRMAP(15,NStartHBV+2) = TranslateString  (LanguageHandle,'HBV InterFlow')
             LongDSRMAP(15,NStartHBV+3) = TranslateString  (LanguageHandle,'HBV QuickFlow')
             LongDSRMAP(15,NStartHBV+4) = TranslateString (LanguageHandle,'HBV Dry Snow Content')
             LongDSRMAP(15,NStartHBV+5) = TranslateString (LanguageHandle,'HBV Free Water Content')
             LongDSRMAP(15,NStartHBV+6) = TranslateString (LanguageHandle,'HBV Soil Moisture')
             LongDSRMAP(15,NStartHBV+7) = TranslateString (LanguageHandle,'HBV Upper Zone Content')
             LongDSRMAP(15,NStartHBV+8) = TranslateString (LanguageHandle,'HBV Lower Zone Content')
             LongDSRMAP(15,NStartHBV+9) = TranslateString (LanguageHandle,'HBV_Temperature')
             LongDSRMAP(15,NStartHBV+10) = TranslateString (LanguageHandle,'HBV Snowmelt')
             LongDSRMAP(15,NStartHBV+11) = TranslateString (LanguageHandle,'HBV Refreezing')
             LongDSRMAP(15,NStartHBV+12) = TranslateString (LanguageHandle,'HBV Infiltration')
             LongDSRMAP(15,NStartHBV+13) = TranslateString (LanguageHandle,'HBV Seepage')
             LongDSRMAP(15,NStartHBV+14) = TranslateString (LanguageHandle,'HBV Direct Runoff')
             LongDSRMAP(15,NStartHBV+15) = TranslateString (LanguageHandle,'HBV Percolation')
          endif
          if (NCRRRUnoffSCS .gt. 0) then
              LongDSRMAP(15,NStartSCS) = TranslateString (LanguageHandle,'SCS_Storage')
          endif
          if (NCRRRUnoffNAM .gt. 0) then
              LongDSRMAP(15,NStartNAM   ) = TranslateString (LanguageHandle,'D-NAM External Water Level')
              LongDSRMAP(15,NStartNAM+1 ) = TranslateString (LanguageHandle,'D-NAM Groundwater Pump DefinedDischarge')
              LongDSRMAP(15,NStartNAM+2 ) = TranslateString (LanguageHandle,'D-NAM Evaporation from SurfaceStorage')
              LongDSRMAP(15,NStartNAM+3 ) = TranslateString (LanguageHandle,'D-NAM Evapotranspiration from Rootzone layer ')
              LongDSRMAP(15,NStartNAM+4 ) = TranslateString (LanguageHandle,'D-NAM Evapotranspiration from Rootzone part of groundwater storage')
              LongDSRMAP(15,NStartNAM+5 ) = TranslateString (LanguageHandle,'D-NAM Evapotranspiration from Lower zone storage')
              LongDSRMAP(15,NStartNAM+6 ) = TranslateString (LanguageHandle,'D-NAM Overland flow')
              LongDSRMAP(15,NStartNAM+7 ) = TranslateString (LanguageHandle,'D-NAM Interflow')
              LongDSRMAP(15,NStartNAM+8 ) = TranslateString (LanguageHandle,'D-NAM Fast Baseflow ')
              LongDSRMAP(15,NStartNAM+9 ) = TranslateString (LanguageHandle,'D-NAM Slow Baseflow ')
              LongDSRMAP(15,NStartNAM+10) = TranslateString (LanguageHandle,'D-NAM External Groundwater inflow')
              LongDSRMAP(15,NStartNAM+11) = TranslateString (LanguageHandle,'D-NAM External Groundwater Inflow into Lower zone storage')
              LongDSRMAP(15,NStartNAM+12) = TranslateString (LanguageHandle,'D-NAM External Groundwater Inflow into Groundwater storage')
              LongDSRMAP(15,NStartNAM+13) = TranslateString (LanguageHandle,'D-NAM Groundwater Pump Abstraction from Groundwater storage')
              LongDSRMAP(15,NStartNAM+14) = TranslateString (LanguageHandle,'D-NAM Groundwater Pump Supply to Lower zone storage')
              LongDSRMAP(15,NStartNAM+15) = TranslateString (LanguageHandle,'D-NAM Groundwater Pump Supply to Groundwater storage')
              LongDSRMAP(15,NStartNAM+16) = TranslateString (LanguageHandle,'D-NAM Infiltration ')
              LongDSRMAP(15,NStartNAM+17) = TranslateString (LanguageHandle,'D-NAM Infiltration Lower zone storage ')
              LongDSRMAP(15,NStartNAM+18) = TranslateString (LanguageHandle,'D-NAM Percolation ')
              LongDSRMAP(15,NStartNAM+19) = TranslateString (LanguageHandle,'D-NAM Capillary Rise ')
              LongDSRMAP(15,NStartNAM+20) = TranslateString (LanguageHandle,'D-NAM Depth Surface storage ')
              LongDSRMAP(15,NStartNAM+21) = TranslateString (LanguageHandle,'D-NAM Depth LowerZone storage ')
              LongDSRMAP(15,NStartNAM+22) = TranslateString (LanguageHandle,'D-NAM Depth Groundwater storage ')
              LongDSRMAP(15,NStartNAM+23) = TranslateString (LanguageHandle,'D-NAM Volume Surface storage ')
              LongDSRMAP(15,NStartNAM+24) = TranslateString (LanguageHandle,'D-NAM Volume Lower zone storage ')
              LongDSRMAP(15,NStartNAM+25) = TranslateString (LanguageHandle,'D-NAM Volume Groundwater storage ')
              LongDSRMAP(15,NStartNAM+26) = TranslateString (LanguageHandle,'D-NAM GroundWater level')
              LongDSRMAP(15,NStartNAM+27) = TranslateString (LanguageHandle,'D-NAM GroundWater table depth ')
              LongDSRMAP(15,NStartNAM+28) = TranslateString (LanguageHandle,'D-NAM Available soil storage ')
              LongDSRMAP(15,NStartNAM+29) = TranslateString (LanguageHandle,'D-NAM BaseFlow ')
              LongDSRMAP(15,NStartNAM+30) = TranslateString (LanguageHandle,'D-NAM Groundwater Pump Actual Discharge ')
              LongDSRMAP(15,NStartNAM+31) = TranslateString (LanguageHandle,'D-NAM Groundwater Pump Actual Discharge minus Defined Discharge')
          endif
          if (NCRRRUnoffLGSI .gt. 0) then
              LongDSRMAP(15,NStartLGSI) = TranslateString (LanguageHandle,'LGSI Rainfall1 ')
              LongDSRMAP(15,NStartLGSI+1) = TranslateString (LanguageHandle,'LGSI Rainfall2 ')
              LongDSRMAP(15,NStartLGSI+2) = TranslateString (LanguageHandle,'LGSI Potential Evapotranspiration1 ')
              LongDSRMAP(15,NStartLGSI+3) = TranslateString (LanguageHandle,'LGSI Potential Evapotranspiration2 ')
              LongDSRMAP(15,NStartLGSI+4) = TranslateString (LanguageHandle,'LGSI Actual Evapotranspiration1 ')
              LongDSRMAP(15,NStartLGSI+5) = TranslateString (LanguageHandle,'LGSI Actual Evapotranspiration2 ')
              LongDSRMAP(15,NStartLGSI+6) = TranslateString (LanguageHandle,'LGSI Recharge1  ')
              LongDSRMAP(15,NStartLGSI+7) = TranslateString (LanguageHandle,'LGSI Recharge2  ')
              LongDSRMAP(15,NStartLGSI+8) = TranslateString (LanguageHandle,'LGSI Drainage flow1 ')
              LongDSRMAP(15,NStartLGSI+9) = TranslateString (LanguageHandle,'LGSI Drainage flow2 ')
              LongDSRMAP(15,NStartLGSI+10) = TranslateString (LanguageHandle,'LGSI Seepage from 2-1 ')
              LongDSRMAP(15,NStartLGSI+11) = TranslateString (LanguageHandle,'LGSI Overland flow1')
              LongDSRMAP(15,NStartLGSI+12) = TranslateString (LanguageHandle,'LGSI Overland flow2')
              LongDSRMAP(15,NStartLGSI+13) = TranslateString (LanguageHandle,'LGSI QpDirect1  ')
              LongDSRMAP(15,NStartLGSI+14) = TranslateString (LanguageHandle,'LGSI QpDirect2  ')
              LongDSRMAP(15,NStartLGSI+15) = TranslateString (LanguageHandle,'LGSI Riverflow1 ')
              LongDSRMAP(15,NStartLGSI+16) = TranslateString (LanguageHandle,'LGSI Riverflow2 ')
              LongDSRMAP(15,NStartLGSI+17) = TranslateString (LanguageHandle,'LGSI Overland Storage1')
              LongDSRMAP(15,NStartLGSI+18) = TranslateString (LanguageHandle,'LGSI Overland Storage2')
              LongDSRMAP(15,NStartLGSI+19) = TranslateString (LanguageHandle,'LGSI Groundwater Storage1')
              LongDSRMAP(15,NStartLGSI+20) = TranslateString (LanguageHandle,'LGSI Groundwater Storage2')
              LongDSRMAP(15,NStartLGSI+21) = TranslateString (LanguageHandle,'LGSI New Volume1')
              LongDSRMAP(15,NStartLGSI+22) = TranslateString (LanguageHandle,'LGSI New Volume2')
              LongDSRMAP(15,NStartLGSI+23) = TranslateString (LanguageHandle,'LGSI Groundwater table depth1 in meter below surface')
              LongDSRMAP(15,NStartLGSI+24) = TranslateString (LanguageHandle,'LGSI Groundwater table depth2 in meter below surface')
              LongDSRMAP(15,NStartLGSI+25) = TranslateString (LanguageHandle,'LGSI RunoffDelay')
              LongDSRMAP(15,NStartLGSI+26) = TranslateString (LanguageHandle,'LGSI Qtot     ')
              LongDSRMAP(15,NStartLGSI+27) = TranslateString (LanguageHandle,'LGSI Qdelayed ')
              LongDSRMAP(15,NStartLGSI+28) = TranslateString (LanguageHandle,'LGSI Groundwater level1')
              LongDSRMAP(15,NStartLGSI+29) = TranslateString (LanguageHandle,'LGSI Groundwater level2')
          endif
          if (NCRRRUnoffWagMod .gt. 0) then
             LongDSRMAP(15,NStartWagMod) = TranslateString (LanguageHandle,'WagMod BaseFlow')
             LongDSRMAP(15,NStartWagMod+1) = TranslateString (LanguageHandle,'WagMod QuickFlow')
             LongDSRMAP(15,NStartWagMod+2) = TranslateString (LanguageHandle,'WagMod SoilMoisture')
             LongDSRMAP(15,NStartWagMod+3) = TranslateString (LanguageHandle,'WagMod Groundwater store   ')
             LongDSRMAP(15,NStartWagMod+4) = TranslateString (LanguageHandle,'WagMod Seepage  ')
          endif
          if (NCRRRUnoffWalrus .gt. 0) then
             LongDSRMAP(15,NStartWalrus)   = TranslateString (LanguageHandle,'Walrus water-storage deficit in the vadose zone of the soil water reservoir')
             LongDSRMAP(15,NStartWalrus+1) = TranslateString (LanguageHandle,'Walrus groundwater table depth')
             LongDSRMAP(15,NStartWalrus+2) = TranslateString (LanguageHandle,'Walrus water depth in the quickflow reservoir')
             LongDSRMAP(15,NStartWalrus+3) = TranslateString (LanguageHandle,'Walrus water depth in the surface water reservoir')
             LongDSRMAP(15,NStartWalrus+4) = TranslateString (LanguageHandle,'Walrus defined ext.supply to/ extraction from surface water reservoir (fXS)')
             LongDSRMAP(15,NStartWalrus+5) = TranslateString (LanguageHandle,'Walrus defined ext.supply to/ extraction from soil water reservoir (fXG) ')
             LongDSRMAP(15,NStartWalrus+6) = TranslateString (LanguageHandle,'Walrus precipitation added to the quickflow reservoir')
             LongDSRMAP(15,NStartWalrus+7) = TranslateString (LanguageHandle,'Walrus precipitation added to the vadose zone of the soil water reservoir')
             LongDSRMAP(15,NStartWalrus+8) = TranslateString (LanguageHandle,'Walrus precipitation added to the surface water reservoir')
             LongDSRMAP(15,NStartWalrus+9) = TranslateString (LanguageHandle,'Walrus actual transpiration from land or vadose zone of soil water reservoir')
             LongDSRMAP(15,NStartWalrus+10) = TranslateString (LanguageHandle,'Walrus actual evaporation from the surface water reservoir')
             LongDSRMAP(15,NStartWalrus+11) = TranslateString (LanguageHandle,'Walrus equilibrium water-storage deficit in the vadose zone')
             LongDSRMAP(15,NStartWalrus+12) = TranslateString (LanguageHandle,'Walrus discharge from quickflow reservoir to surface water reservoir (fQS)')
             LongDSRMAP(15,NStartWalrus+13) = TranslateString (LanguageHandle,'Walrus groundwater drainage to/ infiltration from surface water reservoir (fGS)')
             LongDSRMAP(15,NStartWalrus+14) = TranslateString (LanguageHandle,'Walrus discharge from the surface water reservoir ')
             LongDSRMAP(15,NStartWalrus+15) = TranslateString (LanguageHandle,'Walrus soil wetness index')
             LongDSRMAP(15,NStartWalrus+16) = TranslateString (LanguageHandle,'Walrus transpiration reduction factor')
             LongDSRMAP(15,NStartWalrus+17) = TranslateString (LanguageHandle,'Walrus depth in surface water reservoir for which discharge=0 (hSmin)')
             LongDSRMAP(15,NStartWalrus+18) = TranslateString (LanguageHandle,'Walrus actual ext.supply to / extraction from surface water reservoir (fXS)')
          endif
        endif


    RETURN
  END subroutine InitdtLong
