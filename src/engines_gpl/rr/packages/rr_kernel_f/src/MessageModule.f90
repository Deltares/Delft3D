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
! at:               $Modtime:: 15-08-97 4:11p   $
!
! current revision: $Revision:: 10              $


module Messages

  !use
  use Conf_fil  ! Iout1 wordt gebruikt in sub ErrMsg
  Use Dh_Alloc
  Use LanguageModule
  Use ReadLib
  use MessageHandling
  use globals

  ! constants

  implicit none

  integer, parameter :: STEP = 1

  ! variables

  Integer    IdControlModule

contains

  subroutine writeHeader (IScren)
    Character(10) spaties
    Integer       IScren

    spaties = ""
    write(ISCREN, *)
    write(ISCREN, *)
    write(ISCREN, *)
    write(ISCREN, *) spaties // '    Simulation started .....'
    write(ISCREN, *)
    write(ISCREN, *)
    write(ISCREN, *)
    write(ISCREN, *) spaties // '    Copyright (c) 2025              DELTARES        '
    write(ISCREN, *) spaties // "    Rainfall-Runoff Module          Version 3.216.73"
    write(ISCREN, *)

    write(ISCREN, *)
    write(ISCREN, *)

! juli 1999 Paramaribo: ARS 1887 S curve: inundatie vanuit open water
!                                         OpenWaterlevelComputations=Advanced.
!                       ARS 2915 link flows HIS;
!

  end subroutine writeHeader


  SUBROUTINE WRLOGO (ISCREN, ISTEP, ISTOP, NEVENT, IBar0, EstimateRemainingDuration, RemTime)

    ! *********************************************************************
    ! *** WRLOGO voor WINDOWS
    ! **********************************************************************
    ! *** Parameters :
    ! ***
    ! *** Name     Type   Size      In/Out    Description
    ! *** ------   ------ -------   ------    ------------------------------
    ! *** ISTEP    INT              IN        PRESENT STEP IN RUNNING
    ! *** ISTOP    INT              IN        MAX.NUMBER OF STEPS
    ! *** NEVENT   INT              IN        MAX.NUMBER OF EVENTS
    ! **********************************************************************

    CHARACTER  BANNER*60, BAR*60, RemTime*9
    Logical    EstimateRemainingDuration
    INTEGER    IScren, ISTEP, ISTOP, nEvent, INUM1, INUM2, IBAR, IBar0

     DATA BANNER /'------------------------------------------------------------'/
     DATA BAR    /'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'/

     ! If not in Standalone Fortran Mode, Do Nothing
     if (.not. in_f90_runner) return

  If (EstimateRemainingDuration) then
    IF (ISTEP .EQ. 0 .AND. ISTOP .EQ. 0) THEN
    ELSEIF (ISTEP .EQ. 0 .AND. ISTOP .GT. 0) THEN
      IF (NEVENT .EQ. 1) THEN
        INUM1 = NINT ( 0.5 * FLOAT(ISTOP))
        INUM2 = NINT (       FLOAT(ISTOP))
        WRITE (ISCREN, 200)
200     FORMAT (/,5X,'Timestep :',37X,' Time remaining (hhh:mm:ss)')
        WRITE (ISCREN, 201 ) INUM1, INUM2
201     FORMAT (5X,'0',22X,I7,23X,I7)
        WRITE (ISCREN, 202 ) BANNER
202     FORMAT (5X,A60 )
      ELSE
        INUM1 = NINT ( 0.5 * FLOAT(ISTOP))
        INUM2 = NINT (       FLOAT(ISTOP))
        WRITE (ISCREN, 206 )
206     FORMAT (/,5X,'Event :',40X,' Time remaining (hhh:mm:ss)')
        WRITE (ISCREN, 201 ) INUM1, INUM2
        WRITE (ISCREN, 202 ) BANNER
      ENDIF
! Digital
!!    WRITE (ISCREN,'(''          '',\)')
    ELSE
    ! laat balk voortschrijden
       IBAR = INT ( 60 * FLOAT(ISTEP)/ISTOP )
       IBAR0 = max ( 0, IBar0)
       IBAR0 = min ( 60, IBar0)
       IF ( IBAR .GT. IBar0 .AND. IBAR .LT. 60 ) &
! Powerstation     WRITE(*,'(''+    '',A)') BAR(1:IBAR)
          WRITE(ISCREN,'(''+    '',A,A,4X,A)') BAR(1:IBAR),Banner(IBar+1:60),RemTime
! Digital
!       WRITE(ISCREN,'(A,\)')  BAR(IBar0+1:IBAR)
       IF ( IBAR .EQ. 60 ) &
           WRITE(ISCREN,'(''+    '',A,4X,A)') BAR(1:IBAR),RemTime
       IBar0 = IBar
   ENDIF
 Else
    IF (ISTEP .EQ. 0 .AND. ISTOP .EQ. 0) THEN
    ELSEIF (ISTEP .EQ. 0 .AND. ISTOP .GT. 0) THEN
      IF (NEVENT .EQ. 1) THEN
        INUM1 = NINT ( 0.5 * FLOAT(ISTOP))
        INUM2 = NINT (       FLOAT(ISTOP))
        WRITE (ISCREN, 1200)
1200     FORMAT (/,10X,'Timestep :')
        WRITE (ISCREN, 1201 ) INUM1, INUM2
1201     FORMAT (10X,'0',22X,I7,23X,I7)
        WRITE (ISCREN, 1202 ) BANNER
1202     FORMAT (10X,A60 )
      ELSE
        INUM1 = NINT ( 0.5 * FLOAT(ISTOP))
        INUM2 = NINT (       FLOAT(ISTOP))
        WRITE (ISCREN, 1206 )
1206     FORMAT (/,10X,'Event :')
        WRITE (ISCREN, 1201 ) INUM1, INUM2
        WRITE (ISCREN, 1202 ) BANNER
      ENDIF
! Digital
!!    WRITE (ISCREN,'(''          '',\)')
    ELSE
    ! laat balk voortschrijden
       IBAR = INT ( 60 * FLOAT(ISTEP)/ISTOP )
       IBAR0 = max ( 0, IBar0)
       IBAR0 = min ( 60, IBar0)
       IF ( IBAR.GT. IBar0 .AND. IBAR .LT. 60 ) &
! Powerstation     WRITE(*,'(''+         '',A)') BAR(1:IBAR)
          WRITE(ISCREN,'(''+         '',A)') BAR(1:IBAR)
! Digital
!       WRITE(ISCREN,'(A,\)')  BAR(IBar0+1:IBAR)
       IBar0 = IBar
   ENDIF
  Endif

  RETURN
  END subroutine wrLogo


  Subroutine WriteMessage (Iout1, Messg)

  INTEGER             iOut1
  CHARACTER(Len=999)  MESSG

  if (iOut1 .ne. 0)  then
     WRITE(IOUT1,'(A)') MESSG(1:Len_trim(Messg))
  else
!    ARS 9051: alleen als er niets in de logfile komt, een melding op scherm geven
     WRITE(*,'(A)') MESSG(1:Len_trim(Messg))
  endif

  Return
  End Subroutine WriteMessage

  SUBROUTINE ErrMsgStandard (ICODE, IECODE, STR1, STR2)
    ! *********************************************************************
    ! *** DELFT_3B model
    ! *** LAST UPDATE : January 21, 1997          BY : Peter Schrier
    ! ***   conversion of dutch captions to english captions
    ! **********************************************************************
    ! *** Write error messages
    ! **********************************************************************
    ! ***  ICODE  = code foutboodschap
    ! ***  IECODE = Fortran error code of knoop-id
    ! ***  STR1   = naam subroutine
    ! ***  STR2   = string met verdere identificatie van de fout
    ! ***  STR3   = string met verdere identificatie van de fout
    ! ***  IOUT1  = message file
    ! **********************************************************************


    INTEGER         ICODE, IECODE, iOut1
    CHARACTER(*)    STR1, STR2
    CHARACTER(Len=999)  STRING(10), MESSG

    iOut1 = ConfFil_get_iOut1()

    if (.not. dimr_mode) then
       if (Icode .ne. 909 .and. &
            Icode .ne. 914 .and. ICode .ne. 9141 .and. iCode .ne. 9142 .and.  &
             Icode .ne. 917 .and. Icode .ne. 919 .and. &
             icode .ne. 920 .and. icode .ne. 927 .and. &
              icode .ne. 936 .and. icode .ne. 937 .and. &
               icode .ne. 969 .and.  &
                icode .ne. 974 .and. icode .ne. 975 .and. icode .ne. 977 .and. &
                 icode .ne. 978 .and. icode .ne. 979 .and. icode .ne. 982 .and. &
                  icode .ne. 994 .and. icode .ne. 995) then
         Write (IFLRTN,'(I5)') Icode
         if (IdControlModule .ge. 0) Call CrashCt (IdControlModule, .true.)
       endif
    endif

!   Write(*,*) ICode, IECODE, STR1, STR2
    IF (ICODE .EQ. 902) THEN
      STRING(1)= ' Error found in subroutine'
      STRING(2)= STR1
      STRING(3)= ' Fortran error code = '
      STRING(4)= INTCH8 (IECODE)
      STRING(5)= ' while reading'
      STRING(6)= STR2
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      MESSG = CONSTR (STRING, 6)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 903) THEN
      STRING(1)= ' Usage: Sobek_3B [Name of file with file names][Name of return code file]'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      MESSG = CONSTR (STRING, 1)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 904) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' :'
      STRING(4)=' file '
      STRING(5)=STR2
      STRING(6)= ' with inputdata not available.'
      STRING(7)= ' Create this file.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      STRING(7) = TranslateString (LanguageHandle,STRING(7))
      MESSG = CONSTR (STRING, 7)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 905) THEN
      STRING(1)=' Error: Reading input from Binary Input file not supported anymore.'
      STRING(2)=' This was functionality of Sobek 2.04.'
      STRING(3)=' Make sure SkipBinFile=-1 in Delft_3B.Ini file.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      MESSG = CONSTR (STRING, 3)
      call SetMessage(LEVEL_FATAL, Messg)
! error 914 split into 909 and 914
    ELSEIF (ICODE .EQ. 909) THEN
      STRING(1)='Warning:'
      STRING(2)= STR1
      STRING(3)='Incorrect number of weatherstations in rainfall file'
      STRING(4)=' At some nodes there is no corresponding weatherstation '
      string(5) =  'Took the first meteostation instead'
      STRING(6)=' Check inputfiles.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      MESSG = CONSTR (STRING, 6)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 910) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Large Balance error'
      STRING(4)=' Check schematisation and simulation timestep'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 911) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Unexpected end of file in inputfile:'
      STRING(4)=STR2
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 912) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' number '
      STRING(4)=STR2
      STRING(5)=' too big for array-dimensions.'
      STRING(6)=' Adjust parameter in mainmodule.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      MESSG = CONSTR (STRING, 6)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 914) THEN
      STRING(1)='Warning:'
      STRING(2)= ' Meteostation not found in rainfallfile. '
      String(3) = ' Node id ='
      STRING(4)= STR1
      String(5) = ' meteostation id ='
      STRING(6)= STR2
      string(7) = ' Took the first meteostation instead'
      STRING(8)=' Check inputfiles.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(7) = TranslateString (LanguageHandle,STRING(7))
      STRING(8) = TranslateString (LanguageHandle,STRING(8))
      MESSG = CONSTR (STRING, 8)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 9140) THEN
      STRING(1)=' Note: for external runoff-nodes using external runoff time series the warning on missing rainfall station can be neglected.'
      MESSG = CONSTR (STRING, 1)
      call SetMessage(LEVEL_INFO, Messg)
    ELSEIF (ICODE .EQ. 9141) THEN
      STRING(1)='Warning:'
      STRING(2)= ' Runoffstation not found in runofffile. '
      String(3) = ' Node id ='
      STRING(4)= STR1
      String(5) = ' runoffstation id ='
      STRING(6)= STR2
      string(7) = ' Took the first runoff station instead'
      STRING(8)=' Check inputfiles.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(7) = TranslateString (LanguageHandle,STRING(7))
      STRING(8) = TranslateString (LanguageHandle,STRING(8))
      MESSG = CONSTR (STRING, 8)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 9142) THEN
      STRING(1)='Warning:'
      STRING(2)= ' Temperaturestation not found in temperaturefile. '
      String(3) = ' Node id ='
      STRING(4)= STR1
      String(5) = ' Temperaturestation id ='
      STRING(6)= STR2
      string(7) = ' Took the first Temperature station instead'
      STRING(8)=' Check inputfiles.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(7) = TranslateString (LanguageHandle,STRING(7))
      STRING(8) = TranslateString (LanguageHandle,STRING(8))
      MESSG = CONSTR (STRING, 8)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 915) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Event'
      STRING(4)= INTCH4 (IECODE)
      STRING(5)=' Date not found in '
      STRING(6)=STR2
      STRING(7)=' file'
      STRING(8)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      STRING(7) = TranslateString (LanguageHandle,STRING(7))
      STRING(8) = TranslateString (LanguageHandle,STRING(8))
      MESSG = CONSTR (STRING, 8)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 916) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Duration of timestep longer than 1 day or longer than timestep rainfall or evaporation data'
      STRING(4)=' Maximum duration of timestep is 86400 seconds'
      STRING(5)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 917) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)= STR2
      STRING(4)=' Configuration error:'
      STRING(5)=' Downstream of paved, rural or greenhouse-area should be an open water node'
          STRING(6)= ' or a boundary node'
      STRING(7)=' Paved area with a sewage-pump to boundary requires a boundary node downstream'
      STRING(8)=' Paved area with a sewage-pump to WWTP requires a WWTP node downstream'
      STRING(9)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      STRING(7) = TranslateString (LanguageHandle,STRING(7))
      STRING(8) = TranslateString (LanguageHandle,STRING(8))
      STRING(9) = TranslateString (LanguageHandle,STRING(9))
      MESSG = CONSTR (STRING, 9)
      call SetMessage(LEVEL_ERROR, Messg)
    ELSEIF (ICODE .EQ. 918) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)= STR2
      STRING(4)= ' Configuration error:'
      STRING(5)= ' Downstream of open water should be a structure node'
      STRING(6)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      MESSG = CONSTR (STRING, 6)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 919) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)= STR2
      STRING(4)=' Configuration error:'
      STRING(5)=' Downstream of structure should be an open water node or a boundary node.'
      STRING(6)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      MESSG = CONSTR (STRING, 6)
      call SetMessage(LEVEL_ERROR, Messg)
    ELSEIF (ICODE .EQ. 920) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=STR2
      STRING(4)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_ERROR, Messg)
    ELSEIF (ICODE .EQ. 921) THEN
      STRING(1)=' Error: '
      STRING(2)=STR1
      STRING(3)=STR2
      STRING(4)=' Configuration error:'
      STRING(5)=' Industry node with demand and no upstream node.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 922) THEN
      STRING(1)=' Error: '
      STRING(2)=STR1
      STRING(3)=STR2
      STRING(4)=' Configuration error:'
      STRING(5)=' Industry node with discharge and no downstream node.'
!     STRING(6)=' Discharge put equal to zero.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
!     STRING(6) = TranslateString (LanguageHandle,STRING(6))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 923) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Crop factors negative (not allowed) or very high (>2.5 is unlikely)'
      STRING(4)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 924) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' for node: '
      STRING(4)= INTCH8 (IECODE)
      STRING(5)=' or initial groundwaterlevel above surfacelevel,'
      STRING(6)=' or alfa-coefficient or infiltration rate is negative,'
      STRING(7)=' or inconsistent primary/sec/ter.drainagelevels,'
      STRING(8)=' or inconsistent summerperiod.'
      STRING(9)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      STRING(7) = TranslateString (LanguageHandle,STRING(7))
      STRING(8) = TranslateString (LanguageHandle,STRING(8))
      STRING(9) = TranslateString (LanguageHandle,STRING(9))
      MESSG = CONSTR (STRING, 9)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 925) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Minimum storage in basin of greenhouse (below reduction is applied), should be given as a percentage'
      STRING(4)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 926) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Detailed output required for event '
      STRING(4)=INTCH4(IECODE)
      STRING(5)=' Number exceeds number of events'
      STRING(6)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      MESSG = CONSTR (STRING, 6)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 927) THEN
      STRING(1)=' Warning in sub '
      STRING(2)=STR1
      STRING(3)=' for node: '
      STRING(4)=STR2
      STRING(5)=' Warning bottomlevel-open water area-level data'
      STRING(6)=' Bottom level above lowest level of interpolation table'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      MESSG = CONSTR (STRING, 6)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 928) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Fixed data file not consistent.'
      STRING(4)=' Probably containing no data for 29 Feb.'
      STRING(5)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 931) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Error in desired units output'
      STRING(4)=STR2
      STRING(5)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 932) THEN
      STRING(1)=' Error in sub '
      STRING(2)= STR1
      STRING(3)=' Time-coefficient should be in range 0 - 1'
      STRING(4)=' Check control data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 933) THEN
      STRING(1)=' Error in sub '
      STRING(2)= STR1
      STRING(3)=' Inconsistent Computation timestep and Exchange Timestep'
      STRING(4)=' Exchange Timestep should be a multiple of the Computation Timestep.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 934) THEN
      STRING(1)=' Error in sub '
      STRING(2)= STR1
      STRING(3)=' Unrecognized Exchange option'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      MESSG = CONSTR (STRING, 3)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 935) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' for node: '
      STRING(4)=INTCH8(IECODE)
      STRING(5)=' Hydraulic radius becomes less than 0 in computation of Manning-formula'
      STRING(6)=' Average level dropped below given bottomlevel'
      STRING(7)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      STRING(7) = TranslateString (LanguageHandle,STRING(7))
      MESSG = CONSTR (STRING, 7)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 936) THEN
! Nieuwe geaggregeerde melding igv volume check bij debieten van/naar rand
      STRING(1)=' Warning in sub:'
      STRING(2)=STR1
      STRING(3)=' for node: '
      STRING(4)(1:)= STR2
      STRING(5)=' Volume check on structure flow is active to prevent oscillations.'
      STRING(6)=' You may need to use a smaller computation timestep.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      MESSG = CONSTR (STRING, 6)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 937) THEN
      STRING(1)=' Warning in sub:'
      STRING(2)=STR1
      STRING(3)=' for node: '
!     STRING(4)(1:8)= INTCH8(IECODE)
      STRING(4) = STR2
      STRING(5) = '. Targetlevel open water node below or at bottomlevel'
      STRING(6) = ' at targetlevel table row '
      STRING(7) = INTCH4 (IECODE)
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      MESSG = CONSTR (STRING, 7)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 938) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' for node: '
      STRING(4)(1:8)= INTCH8(IECODE)
      STRING(4)(9:) = STR2
      STRING(5)= ' Switch-off level pump is below bottom level open water'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 942) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' : node'
 !    STRING(4)=INTCH8(IECODE) // STR2
      STRING(4)(1:8)= INTCH8(IECODE)
      STRING(4)(9:) = STR2
      STRING(5)=' Configuration error:'
      STRING(6)=' Node upstream of inlet must be of type boundary'
      STRING(7)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      STRING(7) = TranslateString (LanguageHandle,STRING(7))
      MESSG = CONSTR (STRING, 7)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 951) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Node-id not found in branch-file.'
      STRING(4)=' Node_id=' // STR2
      STRING(5)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(6) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 952) THEN
      STRING(1)=' Error in sub '
      STRING(2)= STR1
      STRING(3)=' Error in Restart-file, block'
      STRING(4)= INTCH2(IECODE)
      STRING(5)= STR2
      STRING(6)=' Restart file network schematisation not the same as current network.'
      STRING(7)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      STRING(7) = TranslateString (LanguageHandle,STRING(7))
      MESSG = CONSTR (STRING, 7)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 966) THEN
      STRING(1)=' Error weatherstation or salt-file:'
      STRING(2)=STR1
      STRING(3)=STR2
      STRING(4)=' Unknown ID or name of node'
      STRING(5)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 967) THEN
      STRING(1)=' Warning file:'
      STRING(2)=STR1
      STRING(3)=' Name of node'
      STRING(4)=STR2
      STRING(5)=' node-id is found, but inconsistent names'
      STRING(6)=' Check inputdata.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      STRING(6) = TranslateString (LanguageHandle,STRING(6))
      MESSG = CONSTR (STRING, 6)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 968) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' Invalid month/day in Cropfactor or Cropfactor-open water data file'
      STRING(4)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 969) THEN
      STRING(1)= "Error: "
      STRING(2)= STR1
      STRING(3) = STR2
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
!     STRING(4) = ' '
!     STRING(4) = INTCH8(IECODE)
      MESSG = CONSTR (STRING, 3)
      call SetMessage(LEVEL_ERROR, Messg)
    ELSEIF (ICODE .EQ. 970) THEN
      STRING(1)='Error:'
      STRING(2)=STR2
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      MESSG = CONSTR (STRING(1), 2)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 971) THEN
      STRING(1)=' Fatal Error:'
      STRING(2)= STR1
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      MESSG = CONSTR (STRING(1), 2)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 972) THEN
      STRING(1) =' Fatal Error:'
      STRING(2) = STR1
      STRING(3) = STR2
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      MESSG = CONSTR (STRING(1), 3)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 973) THEN
      STRING(1)= 'Fatal Error:'
      STRING(2)= STR1
      STRING(3)= STR2
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      MESSG = CONSTR (STRING, 3)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 974) THEN
      STRING(1)= 'Warning:'
      STRING(2)= STR1
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      MESSG = CONSTR (STRING, 2)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 975) THEN
      STRING(1)='Warning:'
      STRING(2)=' Only one weatherstation in rainfall/runoff/temperature file. '
      STRING(3)=' No check of meteostation names performed; all nodes use this one and only meteostation.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      MESSG = CONSTR (STRING, 3)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 976) THEN
      STRING(1)=' Error in sub '
      STRING(2)=STR1
      STRING(3)=' No downstream open water or boundary for RWZI node'
      STRING(4)= STR2
      STRING(5)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 977) THEN
      STRING(1)= 'Warning:'
      STRING(2)= STR1
      STRING(3)= STR2
      STRING(4)= ''
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      if (IECode .ne. 0) then
         String(4) = INTCH8 (IECODE)
         MESSG = CONSTR (STRING, 4)
      Else
         MESSG = CONSTR (STRING, 3)
      Endif
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 978) THEN
      STRING(1)= 'Warning:'
      STRING(2)= STR1
      STRING(3)= INTCH8(IECODE)
      STRING(4)= STR2
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 979) THEN
      STRING(1)= 'Warning from sub:'
      STRING(2)= STR1
      String(3)= ' Mode=Run simultaneously, but no on-line input data for RR required'
      String(4)= ' Only on-line data for other modules will be generated'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_WARN, Messg)
    ELSEIF (ICODE .EQ. 980) THEN
      STRING(1)= ' Error from sub:'
      STRING(2)= STR1
      String(3)= ' Online connnections with other modules needed, but Run mode is not simultaneously'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      MESSG = CONSTR (STRING, 3)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 981) THEN
      STRING(1) = STR1
      STRING(2) = STR2
      STRING(3) = ' ErrorCode ' // INTCH8(IECode)
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      MESSG = CONSTR (STRING(1), 3)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 982) THEN
      STRING(1)=' Warning: Volume check active for flow between nodes:'
      STRING(2)= STR1
      STRING(3)=' and: '
      STRING(4)= Str2
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_WARN, Messg)
!     geen stop!
    ELSEIF (ICODE .EQ. 983) THEN
      STRING(1)=' Error in unpaved area:'
      STRING(2)=' Specified soil type not known in record:'
      STRING(3)= STR2
      STRING(4)=' Check input data.'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      MESSG = CONSTR (STRING, 4)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 990) THEN
      STRING(1)=' Error in sub'
      STRING(2)=STR1
      STRING(3)='  Error'
      STRING(4)=STR2
      STRING(5)=' in Controlmodule'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 991) THEN
      STRING(1)=' Error in sub'
      STRING(2)=STR1
      STRING(3)='  Error'
      STRING(4)=STR2
      STRING(5)=' Sobek-id in HIS file does not match with 3B-id'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      STRING(4) = TranslateString (LanguageHandle,STRING(4))
      STRING(5) = TranslateString (LanguageHandle,STRING(5))
      MESSG = CONSTR (STRING, 5)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 994) THEN
      STRING(1)= STR1
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(2)= STR2
      STRING(2) = TranslateString (LanguageHandle,STRING(2))
      MESSG = CONSTR (STRING, 2)
      call SetMessage(LEVEL_ERROR, Messg)
    ELSEIF (ICODE .EQ. 995) THEN
      STRING(1)= STR1
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      MESSG = CONSTR (STRING, 1)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 996) THEN
      STRING(1)= STR1
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      MESSG = CONSTR (STRING, 1)
      call SetMessage(LEVEL_FATAL, Messg)
    ELSEIF (ICODE .EQ. 997) THEN
      STRING(1)=' Error in sub '
      STRING(2)= STR1
      STRING(3)=' Error in Restart-file, premature end'
      STRING(1) = TranslateString (LanguageHandle,STRING(1))
      STRING(3) = TranslateString (LanguageHandle,STRING(3))
      MESSG = CONSTR (STRING, 3)
      call SetMessage(LEVEL_FATAL, Messg)
    ENDIF
! request KJ March 2024 to add message also to return code file
    if (Icode .ne. 909 .and. &
         Icode .ne. 914 .and. ICode .ne. 9141 .and. iCode .ne. 9142 .and.  &
          Icode .ne. 917 .and. Icode .ne. 919 .and. &
          icode .ne. 920 .and. icode .ne. 927 .and. &
           icode .ne. 936 .and. icode .ne. 937 .and. &
            icode .ne. 969 .and.  &
             icode .ne. 974 .and. icode .ne. 975 .and. icode .ne. 977 .and. &
              icode .ne. 978 .and. icode .ne. 979 .and. icode .ne. 982 .and. &
               icode .ne. 994 .and. icode .ne. 995) then
         write(iflrtn,'(A)') Messg
    endif
999 Continue

  END subroutine ErrMsgStandard

end Module Messages
