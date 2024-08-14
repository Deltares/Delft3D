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
! at:               $Modtime:: 18-08-97 9:48a   $
!
! current revision: $Revision:: 3               $


      module Files

      use CONF_FIL
      use CONF_ARR
      use Network
      use Crop
      use Greenhouse
      use Messages
      use ReadLib
      use RR_meteo


      implicit none

      contains

      Character(3*CharIdLength) function getFil()

          INTEGER         iDebug, nrArg
          INTEGER         iunit, IECODE, teller !length, length2, j
          LOGICAL         ENDFIL
          CHARACTER(Len=CharIdLength)   FILNAM, FILNM2, STRING
          CHARACTER*8     NAMSUB
          Character(Len=CharIdLength)   nameFile
! voor controller!
          CHARACTER(Len=3*CharIdLength)  CMDLINE !, Buffr1
! end voor controller!

          NAMSUB = '  Getfil'
          Idebug = ConfFil_get_iDebug()
          if (idebug .ne. 0) WRITE (IDEBUG,*) ' GETFIL'

! *** Check if there are Command line arguments

           NRARG = 3
!get command line and filename files (HarmonIT)
           Call Conffil_get_OpenMIModelname (Filnam)
           Call Conffil_get_OpenMIReturnname(Filnm2)
           Call Conffil_get_Commandline (CmdLine)

! *** first argument: file with filenames
          IF (iDebug .ne.  0) WRITE (IDEBUG,'(A)') ' FILNAM = ',FILNAM
          if (FilNAM .eq. '') FILNAM = 'sobek_3b.fnm'

! *** second argument: file name of return code file
          IF (iDebug .ne.  0) WRITE (IDEBUG,'(A)') ' FILNM2 = ',FILNM2
! for dll (inappropriate command line passed): set FILNM2
          if (FilNM2 .eq. '') FILNM2 = 'sobek_3b.rtn'
          Call OpenFl (Iflrtn, FILNM2, 1,2)

! *** OPEN AND READ  FILE WITH FILE NAMES

          Call OpenFl (iunit, FILNAM, 1,2)
          CALL SKPCOM (iunit,ENDFIL, 'ODS ')
          IF (ENDFIL) GOTO 23

          DO teller=1,NFILE
             READ(iunit, *, ERR=555, IOSTAT=IECODE) nameFile
             if (IECODE < 0) exit
             If (NameFile .eq. 'not used') nameFile = ''
             call ConfFil_set_NamFil(teller, nameFile)
          ENDDO

          Call CloseGP(iunit)

!         WRITE(*,*) ' Command line ', CMDLINE(1:Len_Trim(CMDLINE))

!         Read additional file 121 - coupling with Salt - WQ
!         NameFile = ''
!         READ(IN, *, END=666, ERR=666, IOSTAT=IECODE) nameFile
! 666     Continue
!         if (NameFile .eq. '') then
!            nameFile = '..\work\WQrtc.His'
!            call ConfFil_set_NamFil(121, nameFile)
!         endif

          getFil = cmdLine
          RETURN

! *** Error messages

  555 CONTINUE
      call ErrMsgStandard (902, IECODE, NAMSUB, STRING)

   23 CONTINUE
      call ErrMsgStandard (902, IECODE, NAMSUB, STRING)

      Return
      END function getFil





      SUBROUTINE GETFIL2 (IDEBUG)

!C *********************************************************************
!C *** Program :  DELFLAND version 1.00                Date: March 1995
!C *** Module  :
!C *** Get from command line input file with filenames the Directory names of the HIS files
!C *********************************************************************

      IMPLICIT NONE

      INTEGER*2     i  !, istat
      INTEGER       IDEBUG, IN, IECODE, teller
      LOGICAL       ENDFIL
      CHARACTER(Len=CharIdLength) FILNAM, STRING
      CHARACTER*999 Namefile
      CHARACTER*8   NAMSUB
      CHARACTER*1   SLASH

#if ( defined(HAVE_CONFIG_H) )
      SLASH='/'
#else
      SLASH='\'
#endif

      NAMSUB = ' Getfil2'
      if (idebug .ne. 0) Then
         WRITE (IDEBUG,*) ' GETFIL2'
         WRITE (IDEBUG,*) ' SLASH =', SLASH
      Endif

! get file with filenames, skip first 61 files hard gecodeerd.
! Get Dirmap voor files van filenamen 62, 64, 66, 68, 70. 71, 72,74,76,89,90,91, 101,119
      IN = 1
      Call Conffil_get_OpenMIModelname (Filnam)
      If (iDebug .ne.  0) WRITE (IDEBUG,'(A)') ' FILNAM = ',FILNAM
      Call OpenFl (In, FILNAM, 1,2)
      CALL SKPCOM (IN,ENDFIL, 'ODS ')
      If (endfil) Call ERRMSGSTANDARD (902, IECODE, NAMSUB, STRING)
      Do teller=1,61   ! hard gecodeerd
         READ(IN, *, END=23, ERR=555, IOSTAT=IECODE) nameFile
      EndDo

      Call GetDirMap(In,1,slash)
      READ(IN,*,END=23,ERR=555) STRING
      Call GetDirMap(In,2,slash)
      READ(IN,*,END=23,ERR=555) STRING
      Call GetDirMap(In,3,slash)
      READ(IN,*,END=23,ERR=555) STRING
      Call GetDirMap(In,4,slash)
      READ(IN,*,END=23,ERR=555) STRING
      Call GetDirMap(In,5,slash)
      Call GetDirMap(In,6,slash)
      Call GetDirMap(In,7,slash)
      READ(IN,*,END=23,ERR=555) STRING
      Call GetDirMap(In,8,slash)
      READ(IN,*,END=23,ERR=555) STRING
      Call GetDirMap(In,9,slash)
      DO I=1,12
         READ(IN,*,END=23,ERR=555) STRING
      ENDDO
      Call GetDirMap(In,12,slash)
      Call GetDirMap(In,10,slash)
      Call GetDirMap(In,11,slash)
      DO I=1,9
         READ(IN,*,END=23,ERR=555) STRING
      ENDDO
      Call GetDirMap(In,13,slash)
      DO I=1,17
         READ(IN,*,END=23,ERR=555) STRING
      ENDDO
      Call GetDirMap(In,14,slash)

      Call CloseGP(IN)


      RETURN

!C *** Error messages

  555 CONTINUE
      call ErrMsgStandard (902, IECODE, NAMSUB, STRING)

   23 CONTINUE
      call ErrMsgStandard (902, IECODE, NAMSUB, STRING)

      RETURN
      END Subroutine Getfil2



      Subroutine GetDirMap(In, Imap, slash)
      Integer       In, Imap, imfl, slen, ipos, iecode
      CHARACTER(Len=CharIdLength) STRING
      CHARACTER*9   NAMSUB
      CHARACTER*1   SLASH


      NAMSUB = ' GetDirMap'
      imfl = 1

      READ(IN, *, END=23, ERR=555, IOSTAT=IECODE) DIRMAP(IMAP,IMFL)
      SLEN = Len_Trim(DIRMAP(IMAP,IMFL))
!     zoek positie van backslash '\' vanaf einde van de string
      ipos = INDEX(DIRMAP(imap,imfl)(1:slen),SLASH, .true.)
!     if (ipos .gt. 0 .and. ipos .lt. slen) Dirmap(imap,imfl)(ipos+1:) = ' '
      if (ipos .gt. 0 .and. ipos .lt. slen) then
          Dirmap(imap,imfl)(ipos+1:) = ' '
      else
          Dirmap(imap,imfl)= ' '
      endif
!     if (idebug .ne. 0)  WRITE (IDEBUG,'(2I3,A)') IMap, Imfl, DIRMAP(IMAP,IMFL)

      Do imfl=2,nmfl
         Dirmap(imap,imfl) = Dirmap(imap,1)
      Enddo
      Return

  555 CONTINUE
      call ErrMsgStandard (902, IECODE, NAMSUB, STRING)

   23 CONTINUE
      call ErrMsgStandard (902, IECODE, NAMSUB, STRING)


      Return
      End Subroutine GetDirMap




      Subroutine InitFixedFilesAndSetTimestep  ( InRain, InRunoff, InTemperature, Inevap, NrEvapStations, &
                                                 Inkini, Ingebr, Incrf,  InCrow,  InSbk, &
                                                 IDH, IDM, IDS, EvapFormat2, MeteoNetcdfInput)

!*********************************************************************
!*** Open hydrological files and spool to first data record
!***   13: rainfall
!***   14: evaporation: net als
!***   15: kwel       : voorlopig constant in de tijd, variatie in ruimte
!***   16: wegzijging : idem
!***   17: initiele berging kasklassen volgens Staring centrum
!***   18: verbruik water kassen volgens Staring centrum
!***   19: crop factoren gewassen
!***   21: initialisatie grondwaterstand/zomerberging
!***       (wordt wel gelezen, echter door INITB!overruled)
!***   38: waterstanden randknopen (via Sobek)
!***   40: crop factoren open water
!*********************************************************************


      Integer        InRain, InRunoff, InTemperature, InEvap, InKini, InGebr, InCrf, InCrow, InSbk
      Integer        NrEvapStations, NcStat
      Integer        IDH, IDM, IDS
      Character(CharIdLength) String, String1
      Character(1)   Quote
      Logical        Endfil, OldFormat, ReadError, EvapFormat2, MeteoNetCdfInput

      Integer Idum, IeCode, NrSecRain, NrSecEvap, Idebug, iOut1
      integer NrSecRunoff, IdefltRunoff
      integer NrSecTemperature, IdefltTemperature


      Idebug = ConfFil_get_IDEBUG()
      IOut1  = ConfFil_get_IOut1 ()

      QUOTE = ''''

! Read header rainfall File

      OldFormat = .false.
      ReadError = .false.
      EndFil    = .false.

! UNST-5103
      If (MeteoNetCdfInput .and. RainfallNcid .gt. 0) then
         IDEFLT = 1
         NCSTAT = NrPrecipitationStations
         !station names set already
         NEVENT = 1
         NEVENTTemperature = 1
         NRSecRain = NrSecsRai
      Else  ! old situation: bui / rks file
         CALL OPENFL(INRAIN, ConfFil_get_NAMFIL(13),1,1)
!** indicator default data ja/nee
!** als default data: dan bij lezen evapor/cropfact etc. niet checken op jaar
         CALL SKPCOM (INRAIN, ENDFIL, 'ODS ')
         If (Endfil) call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')
         READ(INRAIN,*,END=99,ERR=991,IOSTAT=IECODE) IDEFLT
!June 1996: add number of stations and station names
         CALL SKPCOM (INRAIN, ENDFIL, 'ODS ')
         If (Endfil) call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')
         READ(INRAIN,*,END=99,ERR=981,IOSTAT=IECODE) NCSTAT
         CALL SKPCOM (INRAIN, ENDFIL, 'ODS ')
         If (Endfil) call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')

         DO IDUM=1,NCSTAT
           READ(INRAIN,'(A)',Err=99,END=99,IOSTAT=IECODE) STRING
           IF  (STRING(1:1) .NE. QUOTE) Then
              OldFormat = .true.
              GOTO 982
           Endif
         ENDDO

         IF (NCSTAT .LT. NCMET) call ErrMsgStandard (909, 0, 'Sobek_3B', ' ')
         GOTO 982
  981    CONTINUE
         BACKSPACE(INRAIN)
  982    CONTINUE

!Apparently file is in old format, without nr. of stations and the names
         If (OldFormat) then
           REWIND (INRAIN)
           CALL SKPCOM (INRAIN, ENDFIL, 'ODS ')
           If (Endfil) call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')
           READ(INRAIN,*,END=99,ERR=991,IOSTAT=IECODE) IDUM
         Endif

!Nov 1995
!nr. events, rainfall timestep size
         CALL SKPCOM (INRAIN, ENDFIL, 'ODS ')
         READ(INRAIN,*,END=99,ERR=990,IOSTAT=IECODE) NEVENT, nrSecRain
         call ConfArr_set_nrsRai(nrSecRain)

         IF (IDEBUG .ne.  0) WRITE (IDEBUG,*) ' Nr. events:',NEVENT,nrSecRain
         GOTO 9901
  990    CONTINUE
         BACKSPACE(INRAIN)
 9901    CONTINUE
         IF (timeSettings%timestepSize .GT. ConfArr_get_NRSRAI())  then
            call ErrMsgStandard (916, 0, 'Sobek_3B',' ')
         Endif
!End Nov 1995
      Endif
!end UNST-5103


!Set delta timestep

      IDH = 1
      IDM = 0
      IDS = 0
      IF (timeSettings%timestepSize .EQ. NRSDAY) THEN
         IDH = 24
      ELSEIF ((timeSettings%timestepSize .lt. NRSDAY) .AND. (timeSettings%timestepSize .ge. NRSHR)) THEN
         IDH = timeSettings%timestepSize / NRSHR
         IDM = (timeSettings%timestepSize - IDH*NRSHR) / NRSMIN
         IDS = timeSettings%timestepSize - IDH*NRSHR - IDM*NRSMIN
      ELSEIF ((timeSettings%timestepSize .lt. NRSHR) .AND.  (timeSettings%timestepSize .ge. NRSMIN)) THEN
         IDH = 0
         IDM = timeSettings%timestepSize / NRSMIN
         IDS = timeSettings%timestepSize - IDM*NRSMIN
      ELSEIF (timeSettings%timestepSize .LT. NRSMIN) THEN
         IDH = 0
         IDM = 0
         IDS = timeSettings%timestepSize
      ELSE
         call ErrMsgStandard (916, 0, 'Sobek_3B',' ')
      ENDIF

      IF (IOPT1(1) .EQ. 1 .AND. IOPT1(4) .GT. NEVENT) call ErrMsgStandard (926, IOPT1(4), 'Sobek_3B', ' ')
      GOTO 992
   99 call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')
  991 call ErrMsgStandard (911, IECODE, 'Sobek_3B', ' ')
  992 CONTINUE
! UNST-5103
      If (.not. MeteoNetCdfInput) CALL SPLFIL (INRAIN, ' Rainfall file')


! Read header Evaporation file

! UNST-5103
      If (MeteoNetcdfInput .and. EvapNcid .gt. 0) then
        ! set to values read from the NetCdf file
        nrEvapStations = NrEvaporationStations
        NrSecEvap = NrSecsEvap  ! was default 86400
      else
         CALL OPENFL(INEVAP, ConfFil_get_NAMFIL(14),1,1)
         Read(Inevap,'(A)') string1
         EvapFormat2 = (String1(1:6) .eq. 'EVP2.0')
         Rewind(InEvap)
         If (EvapFormat2) then
             Call CloseGP (InEvap)
             Call ScanFile (InEvap, Conffil_Get_Namfil(14), 1, 1, 'EVAP ', NrEvapStations, .true. )
             CALL OPENFL(INEVAP, ConfFil_get_NAMFIL(14),1,1)
             NrSecEvap = 86400  ! to be overwritten by EvapTimestep(station1)
         Else
             CALL SPLFIL (INEVAP, ' Evaporation file')
             read(INEVAP, '(A120)') string1
             nrEvapStations = counts(string1) - 3       ! eerste 3 velden zijn datumvelden; oud format
             Rewind (inEvap)   ! herstellen begintoestand file
             NrSecEvap = 86400   ! old files: default daily basis
         Endif
         nrEvapStations = min(nrEvapStations, ncMet)
         nrEvapStations = max(1, nrEvapStations)
         CALL SPLFIL (INEVAP, ' Evaporation file')
      endif
      call ConfArr_set_nrsEvap(nrSecEvap)
      IF (timeSettings%timestepSize .GT. ConfArr_get_NRSEvap())  then
         call ErrMsgStandard (916, 0, 'Sobek_3B',' ')
      Endif
! UNST-5103

! Read Headers Kas Initialisatie and Kas Gebruik file
      IF (NCKAS .GT. 0) THEN
        If (.not. NewFormatKasdata) then
           CALL OPENFL(INKINI, ConfFil_get_NAMFIL(17),1,1)
           CALL SPLFIL (INKINI, ' Glasshouse initialisation file')
        else
          CALL OPENFL(INKINI, ConfFil_get_NAMFIL(109),1,1)
        endif
        If (.not. NewFormatKasdata) then
          CALL OPENFL(INGEBR, ConfFil_get_NAMFIL(18),1,1)
          CALL SPLFIL (INGEBR, ' Glasshouse water use file')
        else
          CALL OPENFL(INGEBR, ConfFil_get_NAMFIL(110),1,1)
        endif
      ENDIF

! Read Headers Crop factors
      IF (NCOVHG +NcCell .GT. 0) THEN
        If (.not. NewFormatCropFactors) then
           CALL OPENFL(INCRF, ConfFil_get_NAMFIL(19),1,1)
           CALL RDHDR  (INCRF, 2)
           CALL SPLFIL (INCRF, ' Crop factor file')
        Else
           CALL OPENFL(INCRF, ConfFil_get_NAMFIL(111),1,1)
           CALL RDHDR  (INCRF, 2)
        Endif
      ENDIF

! Read Header Crop factors open water

      IF (NcNode .ne. NcPluv)  THEN
        If (.not. NewFormatCropFactors) then
           CALL OPENFL(INCROW, ConfFil_get_NAMFIL(40),1,1)
           CALL RDHDR2 (INCROW)
           CALL SPLFIL (INCROW, ' Crop factor openwater')
        Else
           CALL OPENFL(INCROW, ConfFil_get_NAMFIL(112),1,1)
           CALL RDHDR2 (INCROW)
        Endif
      ENDIF

! Read header runoff File

      if (NcRRRunoffExternal .gt. 0) then
         CALL OPENFL(INRunoff, ConfFil_get_NAMFIL(80),1,1)
         CALL SKPCOM (INRunoff, ENDFIL, 'ODS ')
         If (Endfil) call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')
         READ(INRunoff,*,END=99,ERR=3991,IOSTAT=IECODE) IDEFLTRunoff
         CALL SKPCOM (INRunoff, ENDFIL, 'ODS ')
         If (Endfil) call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')
         READ(INRunoff,*,END=99,ERR=3981,IOSTAT=IECODE) NCSTATRunoff
         CALL SKPCOM (INRunoff, ENDFIL, 'ODS ')
         If (Endfil) call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')

         DO IDUM=1,NCSTATRunoff
           READ(INRunoff,'(A)',Err=399,END=399,IOSTAT=IECODE) STRING
         ENDDO

         IF (NCSTATRunoff .LT. NCMET) call ErrMsgStandard (909, 0, 'Sobek_3B', ' ')
         GOTO 3982
  3981   CONTINUE
         BACKSPACE(INRunoff)
  3982   CONTINUE


!nr. events, runoff timestep size
         CALL SKPCOM (INRunoff, ENDFIL, 'ODS ')
         READ(INRunoff,*,END=399,ERR=3990,IOSTAT=IECODE) NEVENTRunoff, nrSecRunoff
         call ConfArr_set_nrsRunoff(nrSecRunoff)

         IF (IDEBUG .ne.  0) WRITE (IDEBUG,*) ' Nr. events:',NEVENTRunoff,nrSecRunoff
         GOTO 39901
  3990   CONTINUE
         BACKSPACE(INRunoff)
 39901   CONTINUE
         IF (timeSettings%timestepSize .GT. ConfArr_get_NRSRunoff())  then
             call ErrMsgStandard (916, 0, 'Sobek_3B',' ')
         Endif
         goto 3992
   399   call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')
  3991   call ErrMsgStandard (911, IECODE, 'Sobek_3B', ' ')
  3992   CONTINUE

      EndIf

! Read header temperature File

! UNST-5103
      if (MeteoNetcdfInput .and. TemperatureNcid .gt. 0) then
         ! Not supported yet
         ! NeventTemperature =
         ! NrSecTemperature  =
      else
         if (NcRRRunoffHBV .gt. 0) then
            CALL OPENFL(INTemperature, ConfFil_get_NAMFIL(79),1,1)
            CALL SKPCOM (InTemperature, ENDFIL, 'ODS ')
            If (Endfil) call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')
            READ(InTemperature,*,END=99,ERR=4991,IOSTAT=IECODE) IDEFLTTemperature
            CALL SKPCOM (InTemperature, ENDFIL, 'ODS ')
            If (Endfil) call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')
            READ(InTemperature,*,END=99,ERR=4981,IOSTAT=IECODE) NCSTATTemperature
            CALL SKPCOM (InTemperature, ENDFIL, 'ODS ')
            If (Endfil) call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')

            DO IDUM=1,NCSTATTemperature
              READ(InTemperature,'(A)',Err=499,END=499,IOSTAT=IECODE) STRING
            ENDDO

            IF (NCSTATTemperature .LT. NCMET) call ErrMsgStandard (909, 0, 'Sobek_3B', ' ')
            GOTO 4982
  4981      CONTINUE
            BACKSPACE(InTemperature)
  4982      CONTINUE


!nr. events, temperature timestep size
            CALL SKPCOM (InTemperature, ENDFIL, 'ODS ')
            READ(InTemperature,*,END=499,ERR=4990,IOSTAT=IECODE) NEVENTTemperature, nrSecTemperature
            call ConfArr_set_nrsTemperature(nrSecTemperature)

            IF (IDEBUG .ne.  0) WRITE (IDEBUG,*) ' Nr. events:',NEVENTTemperature,nrSecTemperature
            GOTO 49901
  4990      CONTINUE
            BACKSPACE(InTemperature)
 49901      CONTINUE
            IF (timeSettings%timestepSize .GT. ConfArr_get_NRSTemperature())  then
               call ErrMsgStandard (916, 0, 'Sobek_3B',' ')
            endif
            goto 4992
   499      call ErrMsgStandard (902, IECODE, 'Sobek_3B', ' ')
  4991      call ErrMsgStandard (911, IECODE, 'Sobek_3B', ' ')
  4992      CONTINUE

         EndIf
      EndIf
! end UNST-5103

! Set nr. Seconds Sobek NrsSbk

      NRSSBK = ConfArr_get_NRSRAI()
      IF (IDEBUG .ne.  0) WRITE (IDEBUG,*) ' INSBK = ',INSBK

! UNST-5103
      if (.not. MeteoNetcdfInput) then
        Call CloseGP(InRain)
        Call CloseGP(InTemperature)
      endif
        Call CloseGP(InRunoff)
! end UNST-5103

      Return
      End Subroutine InitFixedFilesAndSetTimestep



      Subroutine SetFileNames (Ievent)

! In case Ievent >=1: set file names with additional _001, _002 etc. for Ievent=1,2 etc.
!                     and also adjust NmfMap with these extensions
! In case Ievent =0 : set SOME file names to original file name plus extension _00

      Integer       Ievent, Teller, ILen, ILenExt, ILenPrevExt, IMap, Imfl
      Character*5   Prev
      Character*4   Ext, PrevExt
      Character(Len=CharIdLength) NameFile

      If (IEvent .ge. 1) then
         If (Nevent+1 .gt. 9999) then
            call ErrMsgStandard (971, 0, ' Too many events in series; Maximum allowed = 9999',' SetFileNames')
         ElseIf (Nevent+1 .gt. 999) then
            Ext = IntCh4(IEvent)
            PrevExt = IntCh4(Ievent-1)
         ElseIf (Nevent+1 .gt. 99) then
            Ext = IntChr(IEvent)
            PrevExt = IntChr(Ievent-1)
         Else
            Ext = IntCh2(IEvent)
            PrevExt = IntCh2(Ievent-1)
         Endif
         Prev = ' '
         If (Ievent .gt. 1) Prev = '_'// PrevExt
! Adjust file names in Namfil
         ILenExt = Len_trim(Ext)
         ILenPrevExt = Len_trim(Prev)
         Do teller=1,NFILE
            if (teller .ne. 99) then
              NameFile = Conffil_get_Namfil(teller)
              ILen  = Len_trim(Namefile)
! remove original added _00 if needed
!             If (Ievent .eq. 1) then
! Unix; niet checken op =1, maar >=1
              If (Ievent .ge. 1) then
                 If (ILen-3 .gt. 0) then
                    if (Namefile(ILen-2:ILen) .eq. '_00') then
                        Namefile(ILen-2:ILen) = ' '
                        ILen = Ilen-3
                    Endif
                 Endif
              Else
! Nov 2002 Taiwan: remove postfix previous event (_001, _002, .,_099, etc)
                 If (ILen-ILenPrevExt .gt. 0) then
                    if (Namefile(ILen-ILenPrevExt+1:ILen) .eq. Prev) then
                       Namefile(ILen-ILenPrevExt+1:ILen) = ' '
                       ILen = ILen-ILenPrevExt
                    Endif
                 Endif
              Endif
! Add extension
              NameFile = Namefile (1:ILen) // '_' // Ext(1:ILenExt)
              If (NameFile .eq. 'not used') nameFile = ''
              call ConfFil_set_NamFil(teller, nameFile)
            Endif
         Enddo
! Adjust NMFMAP
! Nov 2002 Taiwan: first remove postfix previous event (_001, _002, .,_099, etc)
         Do IMap=1,NMap
            Do Imfl=1,Nmfl
!              first remove old postfix
               NameFile = NmfMap(Imap,imfl)
               ILen  = Len_trim(Namefile)
               If (ILen-ILenPrevExt .gt. 0) then
                  if (Namefile(ILen-ILenPrevExt+1:ILen) .eq. Prev) then
                     Namefile(ILen-ILenPrevExt+1:ILen) = ' '
                     ILen = Ilen-ILenPrevExt
                  Endif
               Endif
               NmfMap(imap,imfl) = Namefile
!              add new postfix
               NmfMap(Imap,Imfl) = Nmfmap(imap,imfl) (1:Len_trim(NmfMap(imap,imfl))) // '_' // Ext(1:ILenExt)
            Enddo
         Enddo
      Else
!        Extend filenames only for some outputfiles
         Ext = '_00'
         DO teller=1,NFILE
            If (teller .eq. 22 .or. teller .eq. 32 .or. teller .eq. 37 .or. &
                 teller .eq. 45 .or. teller .eq. 46 .or. teller .eq. 47 .or. &
                  teller .eq. 81 .or. teller .eq. 85 .or. &
                   (teller .ge. 24 .and. teller .le. 30)) then
               NameFile = Conffil_get_Namfil(teller)
               If (NameFile .ne. 'not used') then
                  NameFile = Namefile (1:Len_trim(Namefile)) // Ext(1:3)
                  call ConfFil_set_NamFil(teller, nameFile)
               Endif
            Endif
         ENDDO
      Endif

      Return
      END Subroutine SetFilenames


      Subroutine SetLastLogFileName(NameFile)

!     Set file name of log file (Namfil(22)) for last messages
!     Routine only used on UNIX Parallel version and on PC with EmulateUnixonPC switch

      Integer       ILen, ILenExt, ILenPrevExt
      Character*5   Prev
      Character*4   Ext, PrevExt
      Character(Len=CharIdLength) NameFile

         If (Nevent+1 .gt. 9999) then
            call ErrMsgStandard (971, 0, ' Too many events in series; Maximum allowed = 9999',' SetLastLogFileName')
         ElseIf (Nevent+1 .gt. 999) then
            Ext = IntCh4(NEvent+1)
            PrevExt = IntCh4(Nevent)
         ElseIf (Nevent+1 .gt. 99) then
            Ext = IntChr(NEvent+1)
            PrevExt = IntChr(Nevent)
         Else
            Ext = IntCh2(NEvent+1)
            PrevExt = IntCh2(Nevent)
         Endif
         Prev = ' '
         If (Nevent .gt. 1) Prev = '_'// PrevExt
! Adjust file names in Namfil
         ILenExt = Len_trim(Ext)
         ILenPrevExt = Len_trim(Prev)
         NameFile = Conffil_get_Namfil(22)
         ILen  = Len_trim(Namefile)
! remove original added _00 if needed
!        If (Nevent .eq. 1) then
! Unix
         If (Nevent .ge. 1) then
            If (ILen-3 .gt. 0) then
               if (Namefile(ILen-2:ILen) .eq. '_00') then
                   Namefile(ILen-2:ILen) = ' '
                   ILen = Ilen-3
               Endif
            Endif
         Else
! Nov 2002 Taiwan: remove postfix previous event (_001, _002, .,_099, etc)
            If (ILen-ILenPrevExt .gt. 0) then
               if (Namefile(ILen-ILenPrevExt+1:ILen) .eq. Prev) then
                  Namefile(ILen-ILenPrevExt+1:ILen) = ' '
                  ILen = ILen-ILenPrevExt
               Endif
            Endif
         Endif
! Add extension
         NameFile = Namefile (1:ILen) // '_' // Ext(1:ILenExt)
         If (NameFile .eq. 'not used') nameFile = ''
         call ConfFil_set_NamFil(22, nameFile)

      Return
      END Subroutine SetLastLogFileName


      Subroutine Close00Files

        Call CloseGP (Conffil_get_Inxfil(22))
        Call CloseGP (Conffil_get_Inxfil(32))
        Call CloseGP (Conffil_get_Inxfil(37))
        Call CloseGP (Conffil_get_Inxfil(45))
        Call CloseGP (Conffil_get_Inxfil(46))
        Call CloseGP (Conffil_get_Inxfil(47))
!        Close (Conffil_get_Inxfil(81)) ! daily totals at boundaries HIS file

        Call CloseGP (Conffil_get_Inxfil(85))
        Call CloseGP (Conffil_get_Inxfil(24))
        Call CloseGP (Conffil_get_Inxfil(25))
        Call CloseGP (Conffil_get_Inxfil(26))
        Call CloseGP (Conffil_get_Inxfil(27))
        Call CloseGP (Conffil_get_Inxfil(28))
        Call CloseGP (Conffil_get_Inxfil(29))
        Call CloseGP (Conffil_get_Inxfil(30))

      Return
      END Subroutine Close00Files


      subroutine doclose(minp)
         implicit none

         integer, intent(inout) :: minp

         integer :: error

         if (minp == 0) return
         close (minp, iostat=error)
        minp = 0

      end subroutine doclose

      end module Files
