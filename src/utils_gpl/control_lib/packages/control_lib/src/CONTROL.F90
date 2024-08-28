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

module M_control

 implicit none
 
 CHARACTER(len=256) IODir
 CHARACTER(len=26)  Procs, Deps, IniDeps
 INTEGER   ::    NrDep, NrProc, IniNrDep
 LOGICAL   ::    ProcFirst = .false.
 logical   ::    IniFirst  = .false.    !hk: is this ok?

end module M_control

!                                 control.for
!
!     (C) 1996 Deltares
!
!     E.A. Verschuur
!
!     MS-FORTRAN versie van stuurmodule.
!     Deze versie is bedoeld om met MS-FORTRAN/QuickWin applicaties mee
!     te linken (C en Fortran QuickWin libraries zijn incompatibel).
!     Er wordt in deze code derhalve volop gebruik gemaakt van
!     MS-FORTRAN specifieke functionaliteit !
!
!     Voor andere platforms de C-versie gebruiken !
!
!     Subroutines:
!       InitFp
!       InitCt
!       CrashCt
!       StepCt
!       EndCt
!
! Augustus 1999: modified Geert Prinsen
!          Extra parameters voor initialisatie t=0 berekeningen
!          [inidepends] en [inifirst]
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!

! C-functie system() zit standaard in *libfor* libraries
!
!      INTERFACE TO INTEGER [C] FUNCTION SYSTEM [C] (TEKST)
!      CHARACTER*(*)   TEKST [REFERENCE, FAR]
!      END
!
! InitFP()
!
!    Als Initmode=false: InitFp initialiseert variabele ProcFirst voor rekenproces
!    deze variabele geeft aan of het aanroepende proces het eerste is (=de eerste tijdstap zet) of niet
!
!    Als Initmode=true dan initialieer ProcFirst voor initialisatieproces
!
!
      SUBROUTINE INITFP (FirstProc, Initmode)
      use M_control

      implicit none
      
      Logical FirstProc, InitMode

      If (Initmode) then
         FirstProc = IniFirst
      else
         FirstProc = ProcFirst
      endif

      END


!-------------------------------------------------------------------
! InitCt()
!
!    InitCt start de controller functies op voor het aanroepende programma
!    en geeft een unieke Id code terug.
!    InitCt moet 1x worden aangeroepen aan het begin van het hoofdprogramma.
!
! invoer:
!    CmdLine     bevat alle commandline argumenten die aan het programma
!                zijn meegegeven, inclusief argument 0 de programmanaam
!                aan elkaar geplakt in een enkele string.
!                In elk geval moet de string vooraan de programmanaam
!                en achteraan pad+naam van stuurmodule control file bevatten.
! uitvoer:
!    Id          geeft een ID-code terug die moet worden meegegeven bij
!                het aanroepen van EndCt.
!    status      geeft een foutcode <0 terug bij problemen, of een
!                code >= 0 bij juiste verwerking.
!
!-------------------------------------------------------------------
!
      SUBROUTINE INITCT (CmdLine, Id, Status)
      use M_control
      implicit none
      
      CHARACTER(len=*) CmdLine
      INTEGER*4 Id
      INTEGER*2 Status

      INTEGER         :: Ctl = 149
      INTEGER*2       i, clen, cbegin, cend, Pos, POS1
      INTEGER         DepLen, Res, ilwr
      CHARACTER(len=13)  Name
      CHARACTER(len=256) CtlNam, Regel
      CHARACTER(len=32)  Key
      CHARACTER(len=26)  RawDep
      CHARACTER(len=1)   MyID
      LOGICAL         raak
!   de volgende strings zijn precies passend gedefinieerd, om padding
!   met blanks te voorkomen:
      CHARACTER(len=7) :: FirstKop    = '[first]'
      CHARACTER(len=8) :: ProcKop     = '[proces]'
      CHARACTER(len=8) :: DirKop      = '[common]'
      CHARACTER(len=9) :: DepKop      = '[depends]'
      Character(len=10) :: IniFirstKop = '[inifirst]'
      Character(len=12) :: IniDepKop   = '[inidepends]'

!
!   CmdLine bevat naam aanroepende programma als eerste substring
!   en naam stuurfile als laatste substring, splits deze af
!
!   NOTE: LEN_TRIM is MS specifiek, gebruik LEN voor andere compilers
!
      MyId = ' '
      Raak = .false.

!
      clen = len_trim (CmdLine)
      cend = INDEX (CmdLine, ' ')
!
!   splits pad af van naam aanroepend programma
!   NOTE: 3e parameter INDEX is MS specifiek
      DO 48 i=cend, 1, -1
          IF (CmdLine(i:i) .EQ. '\' .OR. CmdLine(i:i) .EQ. '/') GOTO 49
 48   CONTINUE
 49   cbegin = i+1
      Name = CmdLine(cbegin:cend)
!
!   converteer naam naar lowercase
      ilwr = ICHAR(' ')
      DO 50 i=1, len_trim(Name)
          IF ( LLE(Name(i:i),'Z')) THEN
              Name(i:i) = CHAR(IOR(ICHAR(Name(i:i)),ilwr))
          ELSE
              Name(i:i) = Name(i:i)
          END IF
 50   CONTINUE

!
!   zoek achterwaarts naar laatste spatie
!   NOTE: 3e parameter INDEX is MS specifiek
!
      cbegin = 0
      DO 108 Pos1 = len_trim(CmdLine), 1, -1
          IF (CmdLine(Pos1:Pos1) .EQ. ' ') THEN
              cbegin = Pos1
              GOTO 109
          ENDIF
 108  CONTINUE
 109  IF (cbegin .LT. 1) THEN
          Status = -1
          RETURN
      ENDIF
!
!   kopieer stuurfilenaam en probeer de file te lezen
      CtlNam = CmdLine((cbegin+1):clen)
1091  CONTINUE

      OPEN (Ctl, FILE=CtlNam, STATUS='OLD', ACTION='READ', ERR=290)
!
!   zoek kop
      DO WHILE (.TRUE.)
          READ (Ctl, '(A)', Err=1111,END=1111) Key
          Res = INDEX(Key, ProcKop)
          IF (Res .GT. 0) GOTO 75
      END DO
 1111 CONTINUE
!
!   kop niet gevonden
      WRITE(*,*) 'Kan prockop niet vinden'
      CLOSE (Ctl)
      Status = -3
      RETURN
!
!   Zoek exe-naam aanroepend programma
!   Achterste letter van keyword is volg-letter
!   Bouw lijst van andere processen op en bewaar de volg-letter van
!   het aanroepende proces.
!
  75  NrProc = 0
      DO WHILE (.TRUE.)

          READ (Ctl, '(A)', Err=2222, END=2222) Key
          Pos = INDEX(Key, ' ') - 1
          IF (Pos .GT. 0) THEN
              IF (INDEX(Key, Name) .GT. 0) THEN
                  MyID = Key(Pos:Pos)
                  raak = .TRUE.
              ENDIF

              NrProc = NrProc + 1
              Procs(NrProc:NrProc) = Key(Pos:Pos)
          ELSE
              GOTO 80
          ENDIF
      END DO
 2222 CONTINUE
  80  IF (.NOT. raak) THEN
!   exenaam niet gevonden
          WRITE (*,*) 'exenaam >', Name, '< niet gevonden'
          CLOSE (Ctl)
          Status = -4
          RETURN
      ENDIF

!
!   zet volg-letter om naar Id code.
      Id = ICHAR(MyID)
!
!   zoek first
      ProcFirst = .FALSE.
      REWIND (Ctl)
      DO WHILE (.TRUE.)
          READ (Ctl, '(A)', Err=3333, END=3333) Key
          IF (INDEX(Key, FirstKop) .GT. 0) THEN
              READ (Ctl, '(A)',Err=3333, End=3333) Regel
              IF (Regel(1:1) .EQ. MyID) ProcFirst=.True.
              GOTO 81
          ENDIF
      ENDDO
 3333 CONTINUE
!   first indicator niet gevonden
      WRITE (*,*) 'First indicator niet gevonden in CTRL.INI'
 81   CONTINUE

!   zoek Inifirst
      IniFirst = .FALSE.
      REWIND (Ctl)
      DO WHILE (.TRUE.)
          READ (Ctl, '(A)', Err=4444,END=4444) Key
          IF (INDEX(Key, IniFirstKop) .GT. 0) THEN
              READ (Ctl, '(A)', Err=4444,End=4444) Regel
              IF (Regel(1:1) .EQ. MyID) IniFirst=.True.
              GOTO 91
          ENDIF
      ENDDO
 4444 CONTINUE
!   first indicator niet gevonden
      WRITE (*,*) 'IniFirst indicator niet gevonden in CTRL.INI'
 91   CONTINUE

!   zoek common directory
      REWIND (Ctl)
      DO WHILE (.TRUE.)
          READ (Ctl, '(A)', Err=5555,END=5555) Key
           IF (INDEX(Key, DirKop) .GT. 0) GOTO 100
      END DO
 5555 CONTINUE
!   common directory niet gevonden
      WRITE (*,*) 'common directory niet gevonden'
      CLOSE (Ctl)
      Status = -5
      RETURN

 100  READ (Ctl, '(A)',Err=5556,End=5556) Regel
      Pos = INDEX (Regel, ' ')
      IF (Pos .GT. 0) GOTO 105
!   probleem met common directory
 5556 CONTINUE
      WRITE(*,*) 'probleem met common directory'
      CLOSE (Ctl)
      Status = -6
      RETURN

!   splits directory naam af van regel en voeg evt. \ toe
 105  IODir = Regel((Pos+1):)
      Pos = len_trim(IODir)
      IF (IODir(Pos:Pos) .NE. '\' .AND. IODir(Pos:Pos) .NE. '/' ) THEN
          IODir(Pos+1:Pos+1) = '\'
      ENDIF

!   zoek dependencies
 110  REWIND (Ctl)
      DO WHILE (.TRUE.)
          READ (Ctl, '(A)', Err=6666,END=6666) Key
          IF (INDEX(Key, DepKop) .GT. 0) GOTO 120
      END DO
 6666 CONTINUE
!   kop voor dependencies niet gevonden
      WRITE(*,*) 'kop voor dependencies niet gevonden'
      CLOSE (Ctl)
      Status = -9
      RETURN

 120  CONTINUE
!   zoek regel met dependencies voor aanroepende proces
!   letter proces is 1e letter op de regel
      NRDEP = 0

      DO WHILE (.TRUE.)
         READ (Ctl, '(A)',Err=6667,End=6667) Regel
         IF (Regel(1:1) .EQ. '[') goto 199
         IF (Regel(1:1) .EQ. MyID) THEN
 130       Pos = INDEX (Regel, ' ')
           IF (Pos .GT. 0) THEN
!   splits dependencies lijst af van regel
 140         RawDep = Regel((Pos+1):)
!   test op bestaan proces ID's en vul lijstje 'Deps'
             DepLen = LEN(RawDep)
             IF (DepLen .EQ. 0) GOTO 200
             DO I=1,DepLen
               IF (INDEX(Procs(:NrProc), RawDep(I:I)) .GT. 0) THEN
                  NrDep = NrDep+1
                  Deps(NrDep:NrDep) = RawDep(I:I)
                  ENDIF
              ENDDO
                   ELSE
!            probleem met dependencies lijst (geen spatie gevonden tussen key en
             WRITE(*,*) ' probleem met dependencies lijst ', &
                   ' (geen spatie gevonden tussen key en lijst)'
             CLOSE (Ctl)
             Status = -9
             RETURN
           ENDIF
         ENDIF
      ENDDO

 6667 CONTINUE
 199  CONTINUE
!   lijst van dependencies niet gevonden
      IF (NRDEP .LE. 0) THEN
        WRITE(*,*)'lijst van dependencies niet gevonden'
        CLOSE (Ctl)
        Status = -10
        RETURN
      ENDIF


!   eind goed, al goed
 200  CONTINUE

!   zoek Ini dependencies
 210  REWIND (Ctl)
      DO WHILE (.TRUE.)
          READ (Ctl, '(A)', Err=7777,END=7777) Key
          IF (INDEX(Key, IniDepKop) .GT. 0) GOTO 220
      END DO
 7777 CONTINUE
!   kop voor dependencies niet gevonden
      WRITE(*,*) 'kop voor Ini dependencies niet gevonden'
      CLOSE (Ctl)
      Status = -9
      RETURN

 220  CONTINUE
!   zoek regel met dependencies voor aanroepende proces
!   letter proces is 1e letter op de regel
      IniNRDEP = 0

      DO WHILE (.TRUE.)
         READ (Ctl, '(A)', Err=8888,END=8888) Regel
         IF (Regel(1:1) .EQ. '[') goto 300
         IF (Regel(1:1) .EQ. MyID) THEN
 230        Pos = INDEX (Regel, ' ')
            IF (Pos .GT. 0) THEN
!   splits dependencies lijst af van regel
 240           RawDep = Regel((Pos+1):)
!   test op bestaan proces ID's en vul lijstje 'Deps'
               DepLen = LEN(RawDep)
               IF (DepLen .EQ. 0) GOTO 300
               DO I=1,DepLen
                  IF (INDEX(Procs(:NrProc), RawDep(I:I)) .GT. 0) THEN
                     IniNrDep = IniNrDep+1
                     IniDeps(IniNrDep:IniNrDep) = RawDep(I:I)
                  ENDIF
               ENDDO
            ELSE
!              probleem met Ini dependencies lijst (geen spatie gevonden tussen key en
               WRITE(*,*) ' probleem met Ini dependencies lijst ', &
                          ' (geen spatie gevonden tussen key en lijst)'
               CLOSE (Ctl)
               Status = -9
               RETURN
            ENDIF
         ENDIF
      ENDDO
 8888 CONTINUE

  300 CONTINUE

!   control file niet meer nodig
      CLOSE (Ctl)

      Status = 0
      RETURN

!   foutcondities
!
!   status -2: probleem met stuurfile
 290  Status = -2
      CALL JZSLEEP(1)
      GOTO 1091
       WRITE(*,*) 'probleem met stuurfile'

      END

!
! CrashCt
!
!    Subroutine die Crash file schrijft t.b.v. andere processen
!      id = id van het proces
!      selfcrash = true  als module zelf op reguliere wijze crashed
!                  false als een andere module is gecrashed
!
!
      SUBROUTINE CrashCt (Id, selfcrash)
      use M_control
      implicit none
      
      INTEGER*4 Id
!
      CHARACTER(len=1) Ch
      CHARACTER(len=128) SigNam
      LOGICAL       Selfcrash
      INTEGER       Ifout, len_trim, ilen, ilenio
      INTEGER       :: Sig   = 148
      integer       :: NKeer = 9999

!     vertaal Id naar character
      Ch = CHAR(Id)
      signam = ' '

!    schrijf crash file
      ilenio = len_trim (IODir)
      ilen = ilenio
      SigNam(1:ilen) = IODir(1:ilenio)
      SigNam(ilen+1:ilen+8) = 'crashed.'
      SigNam(ilen+9:ilen+9) = Ch

      IFout = 0
250   Continue
      IFout = Ifout+1
      If (Ifout .gt. NKeer) stop ! ' Control Lib-CrashCt; could not write Crashed file'
      CALL JZSLEEP(1)

 50   OPEN (Sig, FILE=SigNam, ACTION='WRITE', STATUS='UNKNOWN', Err=250 )

      If (selfcrash) Then
         WRITE(Sig,'(A)') ' Module has crashed; check log file'
      Else
         WRITE(Sig,'(A)') ' Other Module has crashed; stop '
      Endif
      CLOSE(Sig)

      RETURN
      END


!-------------------------------------------------------------------
! StepCt()
!
!    StepCt maakt een signaalfile aan waarin de oude tijdstap wordt
!    weggeschreven en kijkt of de nieuwe tijdstap uitgevoerd kan worden
!    ahv de signaalfiles van de programma's waarvan het aanroepende
!    programma afhankelijk is.
!    Als de nieuwe tijdstap groter is dan de tijdstap in de andere
!    signaalfiles dan wacht StepCt en test de waardes in de signaalfiles
!    opnieuw totdat de juiste waardes in de signaalfiles gevonden zijn, of
!    de programma's waarvan het aanroepende programma afhankelijk is,
!    zijn beeindigd.
!    StepCt moet voor elke tijdstap in de berekening worden aangeroepen.
!
! invoer:
!    timold      bevat de waarde van de tijdstap (eindtijd) waarvoor het
!                programma resultaten berekend heeft, of -1 indien
!                nog niets berekend is.
!    timnew      bevat de waarde van de volgende te berekenen tijdstap
!                (eindtijd) of -1 indien er geen volgende tijdstap meer is.
!    Id          ID-code om het aanroepende programma te identificeren
!    Initmode    true=initialisatiemode
!                false = rekenmode
!
! uitvoer:
!    status      geeft een foutcode <0 terug bij problemen, of een
!                code == 0 bij juiste verwerking
!                code == 1 als te testen proces klaar is ('done' file bestaat).
!    Crashed     true=als een van de andere modules is gecrashd
!                false = niet
!
!-------------------------------------------------------------------
!
      SUBROUTINE STEPCT (timold, timnew, Id, Status, InitMode, crashed)
      use M_control
      implicit none
      
      Double Precision  timold, timnew
      INTEGER*4 Id
      INTEGER*2 Status
!
      CHARACTER(len=1)   Ch, Ch2
      CHARACTER(len=128) SigNam
      Double precision  SigTim
      LOGICAL       InitMode, Crashed, Wacht, FnmXt, Einde, firsttime
      INTEGER       i, Fout1, ret, len_trim, ilen, ilenio
      INTEGER       NDone, NSignalOK
      Integer       Local_nrdep
      INTEGER       :: Sig   = 148  ! Sig is UNIT nummer Signal file
      INTEGER       :: NKeer = 9999 ! NKeer is aantal pogingen schrijven signalfile (nieuw)

      firsttime = .true.
      Crashed = .false.
      signam = ' '

      If (Initmode) then
         Local_nrdep = IniNrDep
      else
         Local_nrdep = NrDep
      endif

!     vertaal Id naar character
      Ch = CHAR(Id)

!    schrijf signal file
!    correctie ivm geheugen 'eten' van //
!    SigNam = IODir(1:len_trim(IODir)) // 'signal.' // Ch
      ilenio = len_trim (IODir)
      ilen = ilenio
      SigNam(1:ilen) = IODir(1:ilenio)
      SigNam(ilen+1:ilen+7) = 'signal.'
      SigNam(ilen+8:ilen+8) = Ch
!     write(*,*) Signam(1:ilen+8)

!  teller Fout1 op 0 zetten bij eerste poging
      Fout1 = 0
 50   OPEN (Sig, FILE=SigNam, ACTION='WRITE', STATUS='UNKNOWN',ERR=250 )
      WRITE(Sig,'(F15.6)') timold
      CLOSE(Sig)
!
!  loop dependencies langs, en herhaal zolang een of meer signalfiles
!  nog niet de juiste waarde bevatten
!
      IF (Local_NrDep .NE. 0 .AND. TimNew .GE. 0.0) THEN
          ret = 1
          Wacht = .FALSE.
          Einde = .FALSE.
          DO 170 WHILE (.NOT. Einde)
              NDone = 0
              NSignalOK=0
              DO 160 i=1, Local_NrDep
                  If (Initmode) then
                     Ch2 = IniDeps(i:i)
                  else
                     Ch2 = Deps(i:i)
                  endif

! check Done file
                  SigNam = ' '
                  SigNam(1:ilenio) = IODir(1:ilenio)
                  SigNam(ilenio+1:ilenio+5) = 'done.'
                  SigNam(ilenio+6:ilenio+6) = Ch2
! was met gebruik van //, dit eet echter geheugen op
!                 SigNam = IODir(1:len_trim(IODir)) // 'done.' // Ch2
!                 WRITE(*,*) 'test bestand', SigNam
                  INQUIRE (FILE=SigNam, EXIST=FnmXt, ERR=150)
!
                  IF (FnmXt) THEN
                      NDone = NDone + 1
                      Status = 1
                      GOTO 160
                  ENDIF

! check Crashed file
                  SigNam = ' '
                  SigNam(1:ilenio) = IODir(1:ilenio)
                  SigNam(ilenio+1:ilenio+8) = 'crashed.'
                  SigNam(ilenio+9:ilenio+9) = Ch2
                  INQUIRE (FILE=SigNam, EXIST=FnmXt, ERR=150)
                  IF (FnmXt) THEN
                      crashed = .true.
                      einde   = .true.
                      GOTO 170
                  ENDIF

! check Signal file
! was met // gebruik, eet echter geheugen op
!150              SigNam = IODir(1:len_trim(IODir)) // 'signal.' // Ch2
 150              SigNam = ' '
                  SigNam(1:ilenio) = IODir(1:ilenio)
                  SigNam(ilenio+1:ilenio+7) = 'signal.'
                  SigNam(ilenio+8:ilenio+8) = Ch2

                  OPEN (Sig, FILE=SigNam, STATUS='OLD', ACTION='READ', ERR=167)
                  READ(Sig,*, ERR=165,End=165) SigTim
                  CLOSE(Sig)

                  IF (TimNew .GT. SigTim +0.0000005D0) THEN
                      if (firsttime) then
                         firsttime = .false.
                      endif
                      Wacht = .TRUE.
                  Else
                      NSignalOK=NSignalOK + 1
                  ENDIF
 160          CONTINUE
              IF (NDone .EQ. Local_NrDep) THEN
                  Status = 1
                  RETURN
              ENDIF

            GOTO 168
 165          CLOSE (Sig)
!   wacht als een of meer signalfiles geen juiste waarde bevatten

 167          Wacht = .TRUE.
 168          IF (Wacht) THEN
                  Wacht = .FALSE.
              ELSE
                  if (NSignalOK .eq. Local_NrDep) Einde = .TRUE.
              ENDIF
 170     CONTINUE
      ENDIF

      Status = 0
      RETURN
!   error bij schrijven signal file, wacht en probeer opnieuw
 250  CONTINUE
      IF (Fout1 < NKeer) THEN
          Fout1 = Fout1 + 1
          CALL JZSLEEP(1)
          GOTO 50
      ELSE
          Status = -5
          WRITE (*,*) 'Signal file niet aangemaakt'
      ENDIF
      END

!-------------------------------------------------------------------
! EndCt()
!
!    EndCt geeft aan de stuurmodule door dat het aanroepende programma
!    beeindigd wordt.
!    Als het aanroepende proces niet het laatst overgebleven proces is
!    (check ahv 'done.*' signal files) wordt een 'done.*' signal geschreven.
!    Is dit wel het laatste proces, dan worden alle 'signal.*' en 'done.*'
!    signal files in de common directory weggegooid.
!    Noot: als in de stuurfile niet-bestaande processen staan, dan worden
!    de signal files niet weggegooid ('done.*' van niet bestaand proces
!    wordt nooit aangemaakt).
!
!    EndCt moet 1x worden aangeroepen nadat alle berekeningen zijn voltooid.
!
! invoer:
!    Id          ID-code om het aanroepende programma te identificeren
!
! uitvoer:
!    status      geeft een foutcode <0 terug bij problemen, of een
!                code >= 0 bij juiste verwerking.
!
!-------------------------------------------------------------------
!
      SUBROUTINE ENDCT (Id, Status)
      use M_control
      implicit none
      
      INTEGER*4 Id
      INTEGER*2 Status
!
      INTEGER         :: Sig = 148
      CHARACTER(len=1)  MyId
      CHARACTER(len=128) SigBase, DoneBase, DoneName
      INTEGER         ilen

!   vertaal Id naar character
      MyId = CHAR(Id)

      Status = 0

!   loop lijst van processen langs en check of er een 'done' signal van
!   elk proces in de common dir staat
      ilen = len_trim (IODir)
      SigBase = ' '
      SigBase(1:ilen) = IODir(1:len_trim(IODir))
      SigBase(ilen+1:ilen+7) = 'signal.'
      DoneBase = ' '
      DoneBase(1:ilen) = IODir(1:len_trim(IODir))
      DoneBase(ilen+1:ilen+5) = 'done.'


!   in Quick Win library zit geen system() routine.
!   een poging om de files te deleten door openen en sluiten met
!   status SCRATCH heeft geen resultaat (files verdwijnen niet)
!   dus dan maar regelrecht naar het aanmaken van de done file springen
!
!     GOTO 350
! 350 CONTINUE
!   er draaien nog andere processen, dus 'done' file maken en exit

      DoneName = DoneBase(1:len_trim(DoneBase))
      ilen = 1 + len_trim (DoneName)
      DoneName (ilen:ilen) = MyID
      OPEN (Sig, FILE=DoneName, STATUS='NEW', ERR=400)
      CLOSE (Sig)

      Status = 0
      RETURN

!   foutcondities
!
!   probleem bij maken 'done' file
  400 Status = -1
      WRITE(*,*) ' Status=-1 bij maken done file lbl 400'
      END

! GPSLEEP renamed to JZSLEEP to prevent conflicts in RTC
subroutine JZSLEEP(sleeptime)

! *********************************************************************
! *** OPERATIONAL RIVER BASIN WATER MANAGEMENT MODEL
! **********************************************************************
! *** Sleep routine: wait sleeptime milliseconds
! **********************************************************************

!#ifdef HAVE_CONFIG_H
!#include "config.h"
!#ifdef HAVE_IFPORT
!  use ifport
!#endif
!#elif (defined(WIN32))
!    USE MSFWIN
!#endif

    implicit none

! Global variables

!   in seconds
    integer  ::  sleeptime

! Define the conversion factor to milliseconds, which is platform dependent

#ifdef __INTEL_COMPILER
    call SLEEPQQ(sleeptime)
#else
    ! never more than a second
    ! TODO: Must be modified into routine that also supports milliseconds
    call sleep(1)
#endif


end subroutine JZSLEEP
