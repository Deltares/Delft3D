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


     module Crop

      Use CONF_FIL
      Use CONF_Arr
      use Messages
! Nov 2001: option to specify crop factors using standard Sobek time tables
      use NewTables
      use DH_Alloc
      use ReadLib

      implicit none
      
      ! gewassen
      ! ***  NCRP   = max. aantal gewassen
      ! ***  NCROP  = act. aantal gewassen
      ! ***  NAMGW  = namen per gewas

      Integer nCrp, nCrop

      REAL, Pointer, SAVE :: CROPF(:)
      CHARACTER(Len=30), Pointer, SAVE ::   NAMGW(:)

      REAL, Pointer, SAVE ::  CropFactdata (:,:,:), CropOwdata(:,:)
      REAL CROPO

! Nov 2001      Cropfactor file in new format, with choice of crop factor set
      Logical       NewFormatCropFactors
      Character(Len=CharIdLength) CropDefinition, OpenWaterCropDefinition
      Character(Len=CharIdLength) CropDef, CropFactDef, OpenWaterCropFactDef
      Integer       CropFactTableNr, OpenWaterCropFactTableNr


      contains


      Subroutine Crop_confAr1_part1

      implicit none
      
      Logical Success

! Nov 2001 first part of allocating arrays, hard-coded fixed dimension 16
       NCRP = 16
       Success = Dh_AllocInit (NCrp, CROPF, 0E0)
       Success = Success .and. Dh_AllocInit (NCrp, NAMGW, ' ')
       If (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Crop_ConfAr1' )
!       ALLOCATE ( CROPF(NCRP), Stat=Allocation_Error )
!       ALLOCATE ( NAMGW(NCRP), Stat=Allocation_Error )

      Return
      End subroutine Crop_confAr1_part1


      subroutine Crop_confAr1

      implicit none
      
      Logical Success

       Success = Dh_AllocInit (12,31, NCrop, CropFactData, 0E0)
       Success = Success .and. Dh_AllocInit (12, 31, CropOWData, 0E0)
       If (.not. Success) call ErrMsgStandard (981, 0, ' Error allocating arrays in subroutine ', ' Crop_ConfAr1' )
!       Allocate ( CropFactdata (12, 31, NCrop), Stat=Allocation_Error )
!       Allocate ( CropOwdata   (12, 31), Stat=Allocation_Error )

      Return
      End Subroutine Crop_confAr1


      Subroutine CheckCropFactors (Err923, icount)
! *********************************************************************
! *** Check Crop Factors
! ***   1. Error: <0 and > 2.5  --> error 923
! ***   2. Warning if values are zero or > 1.7
! *********************************************************************
      implicit none
      
      Logical Err923
      Integer i, icount
      Character(Len=80) String

      Err923 = .false.

        DO I=1,NCROP
           IF (CropF(I) .LT. 0 .OR. CropF(I) .GT. 2.5) Err923 = .true.
           If (CropF(I) .LE. 0 .OR. CropF(I) .GT. 2.0) then
             if (Icount .lt. 10) then
                String = ' Crop factor <=0.0 or >=2.0 for crop ' // NAMGW(i)
                call ErrMsgStandard(974, 0, String, ' ')
                icount = icount + 1
             elseif (Icount .eq. 10) then
                String = ' Crop factor <=0.0 or >=2.0 also for more crops/timesteps'
                call ErrMsgStandard(974, 0, String, ' ')
                icount = icount + 1
             Endif
           Endif
        ENDDO

      Return
      End Subroutine CheckCropFactors


      Subroutine CheckCropFactorOpenWater (CropO, Err923, icount)
! *********************************************************************
! *** Check Crop Factors
! ***   1. Error: <0 and > 2.5  --> error 923
! ***   2. Warning if values are zero or > 2.0
! *********************************************************************
      implicit none
      
      Real CropO
      Logical Err923
      Integer Icount

      Err923 = .false.

      IF (CROPO .LT. -0.001 .OR. CROPO .GT. 2.5)  Err923 = .true.
      If (Cropo .LE. 0 .OR. Cropo .GT. 2.0) then
         if (Icount .lt. 10) then
            call ErrMsgStandard(974, 0, ' Crop factor <=0.0 or >=2.0 for open water ',' ')
            icount = icount + 1
         elseif (Icount .eq. 10) then
            call ErrMsgStandard(974, 0, ' Crop factor <=0.0 or >=2.0 for open water for more timesteps',' ')
            icount = icount + 1
         Endif
      Endif

      Return
      End Subroutine CheckCropFactorOpenWater


      Subroutine RdCrf (iDebug, IMonth, IDay)
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read crop factor file
! ***   Jan 96: data per gewas ipv per meteostation!
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************
      implicit none
      
      Integer       iECode, I, iDebug, iOut1
      CHARACTER(Len=CharIdLength) STRING
      Integer       IMonth, IDay
      Integer       RowNr
      logical       DateTimeOutsideTable  !, Err923

      type (Date) currentDate
      type (Time) currentTime

      String = ' '
      iOut1 = ConfFil_get_iOut1()


      IF (iDebug /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDCRF')

! *********************************************************************
! *** read crop factors
! *********************************************************************

      If (.not. NewFormatCropFactors) then
! Sobek-parallell
        DO I=1,NCROP
           CropF(i) = CropFactData (Imonth, iDay, i)
        ENDDO
!       Call CheckCropFactors (Err923)
!       If (Err923) call ErrMsgStandard (923, IECODE, '  Rdcrf', STRING)
      Else
        currentDate%year = ConfArr_get_IYear()
        currentDate%month = ConfArr_get_iMonth()
        currentDate%day = ConfArr_get_iDay()
        currentTime%hour = ConfArr_get_iHour()
        currentTime%minute = ConfArr_get_iMinute()
        currentTime%second = 0
        DO I=1,NCROP
           RowNr = -1
           CROPF(i) = GetNewValue(TableHandle, CropFactTableNr, i, RowNr, CurrentDate, CurrentTime, &
                                  Idebug, iout1, DateTimeOutsideTable, .true. )
        ENDDO
        if (idebug .ne. 0) Write(IdebugLunRR,*) ' GetNewValue Cropf(I)', (Cropf(i),i=1,ncrop)
      Endif
      GOTO 999

! *********************************************************************
! *** Error during reading of file
! *********************************************************************

  150 CONTINUE
      call ErrMsgStandard (902, IECODE, '  Rdcrf', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30 CONTINUE
      call ErrMsgStandard (911, IECODE, '  Rdcrf', STRING)

  999 CONTINUE

      RETURN
      END subroutine rdCrf


!     Routine SplCf removed, since it was not used; cleaned in April 2002



      Subroutine RdCrfO (IMonth, IDay)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read crop factor file OPEN WATER
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  INCROW  = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************
      implicit none
!
      INTEGER       iECode
      CHARACTER(Len=CharIdLength) STRING
      Integer       iDebug, IOut1, IMonth, IDay
      Integer       RowNr
      logical       DateTimeOutsideTable  !, Err923

      type (Date) currentDate
      type (Time) currentTime

      String = ' '
      iOut1 = ConfFil_get_iOut1()

!
      iDebug = ConfFil_get_iDebug()

      IF (iDebug /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDCRFO')
!
! *********************************************************************
! *** read crop factors
! *********************************************************************
!
      If (.not. NewFormatCropFactors) then
        Cropo = CropOwData (Imonth, IDay)
!       Call CheckCropFactorOpenWater(Cropo,Err923)
!       IF (Err923) call ErrMsgStandard (923, IECODE, '  RdCrfO', STRING)
      Else
        currentDate%year = ConfArr_get_IYear()
        currentDate%month = ConfArr_get_iMonth()
        currentDate%day = ConfArr_get_iDay()
        currentTime%hour = ConfArr_get_iHour()
        currentTime%minute = ConfArr_get_iMinute()
        currentTime%second = 0
        RowNr = -1
        Cropo = GetNewValue(TableHandle, OpenWaterCropFactTableNr, 1, RowNr, CurrentDate, CurrentTime, &
                            Idebug, iout1, DateTimeOutsideTable, .true.)
        if (idebug .ne. 0) Write(idebugLunRR,*) ' GetNewValue CropOw', Cropo
      Endif
      GOTO 999

!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      call ErrMsgStandard (902, IECODE, '  Rdcrfo', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30 CONTINUE
      call ErrMsgStandard (911, IECODE, '  Rdcrfo', STRING)
!
  999 CONTINUE
!
      RETURN
      END subroutine RdCrfO




      Subroutine RdHdr (INCRF, ICALL)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read header crop factor file
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  INCRF  = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************

      implicit none
      
      INTEGER ::    RetVal

      INTEGER       INCRF, ICALL
      LOGICAL       ENDFIL
      CHARACTER(Len=CharIdLength) STRING
      CHARACTER(Len=9999) BufString
      Integer       iECode, iCrop, iDebug, iOut1
! Nov 2001
      Logical       Allow, Found, TabYesNo, Success
      Integer       NHLP, teller1, len1, len2
      Parameter     (NHLP=25)   ! moet >= NCRP zijn
      Integer       IDUM(NHLP)
      Real          RDUM(NHLP)
      Character(Len=CharIdLength) CDUM(NHLP), TableName

      String = ' '
      iDebug = ConfFil_get_iDebug()
      iOut1  = ConfFil_get_iOut1()
!     iflRtn =
      If (NHLP .lt. NCRP) then
         call SetMessage(LEVEL_FATAL, 'Local dimension error RdCropFact file')
      endif

      IF (iDebug /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDHDR')
!
! *********************************************************************
! *** read header
! *********************************************************************
!
      STRING = ' crop factor file'

      If (.not. NewFormatCropFactors) then
        CALL SKPCOM (INCRF, ENDFIL, 'ODS ')
        IF (ENDFIL) call ErrMsgStandard (911, 0, '  Rdhdr', STRING)
        READ(INCRF,*,END=30,ERR=150,IOSTAT=IECODE)  NCROP
        IF (ICALL .EQ. 1) GOTO 999

        CALL SKPCOM (INCRF, ENDFIL, 'ODS ')
        IF (ENDFIL) call ErrMsgStandard (911, 0, '  Rdhdr', STRING)

        DO ICROP=1,NCROP
           READ(INCRF,*,END=30,ERR=150,IOSTAT=IECODE) NAMGW(ICROP)
        ENDDO
        GOTO 999
      Else
!       New format Cropfactor file, find selected id CropDefinition, associated nr of crops, crop names, crop factors
!       Assume always fixed order: first CROP, then NAME, then CRF records
        CALL SKPCOM (INCRF, ENDFIL, 'ODS ')
        Allow = .false.
        Found = .false.
        Do While (.not. Endfil)
           Success = GetRecord (InCrf,'CRPD', Endfil, Idebug, Iout1) ! get record van keyword CRPD tot crpd
           If (Endfil .or. .not. success) goto 30
           Success = GetStringFromBuffer(BufString)
           If (Endfil .or. .not. success) goto 30
           if (idebug .ne. 0) Write(idebug,*) ' Read buffer: ', BufString(1:100)
           Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
           If (.not. success) goto 150
           if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ', TableName(1:Len_Trim(TableName))
           Call UpperC (TableName)   ! Id put in UPPERCASE, since all data from INI file also converted to UPPERCASE
           Len1 = Len_Trim(TableName)
           Len2 = Len_Trim(CropDefinition)
           if (idebug .ne. 0) Write(idebug,*) ' TableName length and CropDefinition length', len1, len2
           If (TableName(1:Len1) .eq. CropDefinition(1:Len2)) then
! specified Crop definition (e.g. 'Default', 'Parbo', or 'Taiwan' found
! get id's of cropname definition and crop factor definition
              if (idebug .ne. 0) Write(idebug,*) ' TableName = CropDefinition found!: ',&
                                                   CropDefinition(1:Len_Trim(CropDefinition))
              TabYesNo = .true.
              RetVal = GetVAR2 (BufString(1:nbuf),' nc ',3,' RdHdr_CropFact',' Cropfact file',IOUT1, &
                            CDUM(1), RDUM(1), IDUM(1), ALLOW, FOUND, IflRtn)
              if (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error Reading New Crop factors ', ' Error getting CropFactor header')
              NCROP = IDUM(1)
              if (idebug .ne. 0) Write(idebug,*) ' NCrop = ', NCrop
              IF (NCROP .gt. NCRP)  call ErrMsgStandard (912, IECODE, '  Rdhdr', ' Number of crops')
              IF (ICALL .EQ. 1) GOTO 999
              CropDef = ' '
              Success = GetTableName (TabYesNo, CropDef, ' cn ', IOut1)
              If (.not. success) goto 150
              if (idebug .ne. 0) Write(idebug,*) ' CropDef ', CropDef
              CropFactDef = ' '
              Success = GetTableName (TabYesNo, CropFactDef, ' cf ', IOut1)
              If (.not. success) goto 150
              if (idebug .ne. 0) Write(idebug,*) ' CropFactDef ', CropFactDef
! get crop names
              Do While (.not. endfil)
                Success = GetRecord (InCrf,'NAME', Endfil, Idebug, Iout1)  ! get record van keyword CROP tot crop
                If (.not. success) goto 150
                if (idebug .ne. 0) Write(idebug,*) ' Read buffer: ', BufString(1:100)
                Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
                If (.not. success) goto 150
                if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ', &
                                                     TableName(1:Len_Trim(TableName))
                if (idebug .ne. 0) Write(idebug,*) ' Test on CropDef : ', CropDef(1:100)
                Len1 = Len_Trim(TableName)
                Len2 = Len_Trim(CropDef)
                if (idebug .ne. 0) Write(idebug,*) ' TableName length and CropNameDefinition length', len1, len2
                If (TableName(1:Len1) .eq. CropDef(1:Len2)) then
                   if (idebug .ne. 0) Write(idebug,*) ' TableName = CropDef found!: ', CropDef(1:Len_Trim(CropDef))
                   RetVal = GetVRS2 (BufString(1:nbuf),' nm ',1,' RdHdr_CropFact',' Cropfact file', &
                                 IOUT1, CDUM(1), RDUM(1), IDUM(1), NCROP, IflRtn)
                   if (RetVal .gt. 0) call ErrMsgStandard (972, 0, ' Error Reading New Crop names ', ' Error getting CropFactor header - crop names')
                   Do teller1 = 1, NCROP
                      NAMGW (teller1) = CDUM(teller1)
                   Enddo
                   TabYesNo = .true.
                   GOTO 999
                Endif
              Enddo
           Else
              TabYesNo = .false.
           Endif
        Enddo
        If (.not. TabYesNo) GoTo 30
      Endif

      If (iDebug .ne. 0) Then
        Do ICROP=1,NCROP
           Write(IDEBUG,'(I4,1X,A)')  ICROP, NAMGW(ICROP)
        Enddo
      Endif
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      call ErrMsgStandard (902, IECODE, '  Rdhdr', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30 CONTINUE
      call ErrMsgStandard (911, IECODE, '  Rdhdr', STRING)
!
  999 CONTINUE
!
      RETURN
      END subroutine RdHdr




      Subroutine RdHdr2 (INCROW)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  DELFT_3B version 1.0.                 Date: March 1995
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read header crop factor file OPEN WATER
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  INCROW  = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************
      implicit none
      
      INTEGER       INCROW, iECode, iDum
      LOGICAL       ENDFIL
      CHARACTER(Len=CharIdLength) STRING
      Integer        iDebug, iOut1
! Nov 2001
      Logical       Allow, Found, TabYesNo, Success
      Integer       len1, len2
      Character(Len=CharIdLength) TableName

      String = ' '
      iDebug = ConfFil_get_iDebug()
      iOut1  = ConfFil_get_iOut1()

      IF (IDEBUG /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' RDHDR2')
!
! *********************************************************************
! *** read header
! *********************************************************************
!
      STRING = ' crop factor openwater'

      If (.not. NewFormatCropFactors) then
        CALL SKPCOM (INCROW, ENDFIL, 'ODS ')
        IF (ENDFIL) call ErrMsgStandard (911, 0, '  Rdhdr2', STRING)
        READ(INCROW,*,END=30,ERR=150,IOSTAT=IECODE)  IDUM
        CALL SKPCOM (INCROW, ENDFIL, 'ODS ')
        IF (ENDFIL) call ErrMsgStandard (911, 0, '  Rdhdr2', STRING)
        READ(INCROW,*,END=30,ERR=150,IOSTAT=IECODE) STRING
        GOTO 999
      Else
!       New format OpenWaterCropfactor file, find selected id OpenWaterCropDefinition, crop factors
!       Assume always fixed order: first CROW, then CRFO records
        CALL SKPCOM (INCROW, ENDFIL, 'ODS ')
        Allow = .false.
        Found = .false.
        Do While (.not. Endfil)
           Success = GetRecord (InCrow,'CROW', Endfil, Idebug, Iout1)  ! get record van keyword CROW tot crow
           If (Endfil) goto 30
           Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
           If (.not. success) goto 150
           if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ', &
                                                TableName(1:Len_Trim(TableName))
           Call UpperC (TableName)   ! Id put in UPPERCASE, since all data from INI file also converted to UPPERCASE
           Len1 = Len_Trim(TableName)
           Len2 = Len_Trim(OpenWaterCropDefinition)
           if (idebug .ne. 0) Write(idebug,*) ' TableName length and OpenWaterCropDefinition length', len1, len2
           If (TableName(1:Len1) .eq. OpenWaterCropDefinition(1:Len2)) then
! specified Crop definition (e.g. 'Default', 'Parbo', or 'Taiwan' found
! get id's of cropname definition and crop factor definition
              if (idebug .ne. 0) Write(idebug,*) ' TableName = OpenWaterCropDefinition found!: ', &
                                 OpenWaterCropDefinition(1:Len_Trim(OpenWaterCropDefinition))
              OpenWaterCropFactDef = ' '
              Success = GetTableName (TabYesNo, OpenWaterCropFactDef, ' cf ', IOut1)
              If (.not. success) goto 150
              if (idebug .ne. 0) Write(idebug,*) ' OpenWaterCropFactDef ', OpenWaterCropFactDef
              TabYesNo = .true.
              GOTO 999
           Else
              TabYesNo = .false.
           Endif
        Enddo
        If (.not. TabYesNo) GoTo 30
      Endif
!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      call ErrMsgStandard (902, IECODE, '  Rdhdr2', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30 CONTINUE
      call ErrMsgStandard (911, IECODE, '  Rdhdr2', STRING)

  999 CONTINUE

      RETURN
      END Subroutine RdHdr2


      Subroutine ReadCropFactorFile (INCRF, iDebug, iDefLt)
! *********************************************************************
! *** Last update: March  1995       By : Geert Prinsen
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read crop factor file
! ***   Jan 96: data per gewas ipv per meteostation!
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  INCRF  = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************
      implicit none
      
      INTEGER       INCRF, IDUM1, IDUM2, IDUM3, iCount, iECode, I, iDebug, Iout1, IDeflt
      Integer       Imonth, iday, Icrop, iwarningcount
      LOGICAL       ENDFIL
      CHARACTER(Len=CharIdLength) STRING
! Nov 2001
      Logical       Allow, Found, TabYesNo, Doorgaan, Err923, success
      Integer       len1, len2
      Character(Len=CharIdLength) TableName

      String = ' '
      iout1  = ConfFil_get_iOut1 ()
      iwarningcount = 0

      IF (iDebug /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadCropFactorFile')

      ! *********************************************************************
      ! *** read crop factors
      ! *********************************************************************

      If (.not. NewFormatCropFactors) then
        ICOUNT = 0
  11    Continue
        DoorGaan = .true.
        Do While (Doorgaan)
          CALL SKPCOM (INCRF, ENDFIL, 'ODS ')
          STRING = ' crop factor file'
          IF (ENDFIL .AND. IDEFLT .EQ. 0) THEN
            ICOUNT = ICOUNT +1
            REWIND(INCRF)
            CALL RDHDR  (INCRF, 2)
            Doorgaan = .true.
          ELSE
            Doorgaan = .false.
            IF (ENDFIL .AND. IDEFLT .EQ. 1 .AND. ICOUNT .EQ. 0) THEN
              ICOUNT = ICOUNT +1
              REWIND(INCRF)
              CALL RDHDR  (INCRF, 2)
              Doorgaan = .true.
            ENDIF
          ENDIF
        Enddo

        READ(INCRF,*,END=30,ERR=150,IOSTAT=IECODE)  IDUM1, IDUM2, IDUM3, (CROPF(I),I=1,NCROP)
        If (Idum2 .gt. 12 .or. idum3 .gt. 31) call ErrMsgStandard (968, IECODE, '  ReadCropFactorFile', STRING)
        Do I=1,NCrop
           CropFactData (Idum2, IDum3, i) = CROPF(i)
        Enddo
        IF (iDebug /= 0) WRITE(IDEBUG,*) ' Crop factors ', &
                  IDEFLT, IDUM1, IDUM2, IDUM3, (CROPF(I),I=1,NCROP)
        Call CheckCropFactors (Err923, iwarningcount)
        If (Err923) call ErrMsgStandard (923, IECODE, ' ReadCropFactorFile', STRING)
        if (.not. endfil .and. icount .eq. 0) GOTO 11
        if (idebug .ne. 0) then
           DO Imonth=1,12
             DO IDay  =1,31
               write(Idebug,*) ' Crop factors Imonth iDay ',IMonth, IDay, &
                                (CropFactData (Imonth, iday, icrop), Icrop=1,Ncrop)
             ENDDO
           ENDDO
        Endif
      Else
!       New format Cropfactor file, find selected id CropFactDef and read table in CRF records
        CALL SKPCOM (INCRF, ENDFIL, 'ODS ')
        Allow = .false.
        Found = .false.
        Do While (.not. Endfil)
           Success = GetRecord (InCrf,'CRF ', Endfil, Idebug, Iout1)  ! get record van keyword CRF tot crf
           If (Endfil) goto 30
           Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
           If (.not. Success) Goto 150
           if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ', &
                                                TableName(1:Len_Trim(TableName))
           Len1 = Len_Trim(TableName)
           Len2 = Len_Trim(CropFactDef)
           if (idebug .ne. 0) Write(idebug,*) ' TableName length and CropFactDef length', len1, len2
           If (TableName(1:Len1) .eq. CropFactDef(1:Len2)) then
! specified CropFact definition found
! get table of cropfactors with name TableName, NrColumns=NCrop, TableNr found=Table
             if (idebug .ne. 0) Write(idebug,*) ' TableName = CropFactDef found!: ', CropFactDef
             Success = GetTable (TableHandle, TableName, NCrop, CropFactTableNr, Idebug, Iout1)
             If (.not. Success) Goto 150
             TabYesNo = .true.
             Endfil = .true.
           Else
              TabYesNo = .false.
           Endif
        Enddo
        If (.not. TabYesNo) GoTo 30
      Endif
      GOTO 999


! *********************************************************************
! *** Error during reading of file
! *********************************************************************

  150 CONTINUE
      call ErrMsgStandard (902, IECODE, '  ReadCropFactorFile', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30 CONTINUE
      call ErrMsgStandard (911, IECODE, '  ReadCropFactorFile', STRING)

  999 CONTINUE

      RETURN
      END subroutine ReadCropFactorFile



      Subroutine ReadOpenWaterCropFactorFile (INCROW, IDeflt)

! *********************************************************************
! ***                D E L F T         H Y D R A U L I C S
! ***
! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
! *********************************************************************
! *** Program :  Sobek-RR Parallell version    March 2000
! *********************************************************************
! *********************************************************************
! *** Brief description:
! *** ------------------
! ***   Read crop factor file OPEN WATER, save data in array
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  IDEBUG = file unit number of debug file
! ***  INCROW  = file unit number of input file
! ***  IOUT1  = file unit number of output file with messages
! *********************************************************************
!
      implicit none
      
      INTEGER       INCROW, Idebug, IOUt1, iCount, iECode
      Integer       Imonth, iday
      LOGICAL       ENDFIL
      CHARACTER(Len=CharIdLength) STRING
      Integer       iDum1, iDum2, iDum3, iwarningcount
      Integer       IDeflt
! Nov 2001
      Logical       Allow, Found, TabYesNo, Doorgaan, Err923, Success
      Integer       len1, len2
      Character(Len=CharIdLength) TableName

!
      String = ' '
      iDebug = ConfFil_get_iDebug()
      iout1  = ConfFil_get_iOut1 ()
      iwarningcount = 0

      IF (iDebug /= 0) WRITE (IDEBUG,1)
    1 FORMAT (' ReadOpenwaterCropfactorfile')
!
! *********************************************************************
! *** read crop factors Open Water
! *********************************************************************
!
      If (.not. NewFormatCropFactors) then
        ICOUNT = 0
     11 CONTINUE
        Doorgaan = .true.
        Do While (Doorgaan)
           CALL SKPCOM (INCROW, ENDFIL, 'ODS ')
           STRING = ' cropfactor openwater'
           IF (ENDFIL .AND. IDEFLT .EQ. 0) THEN
               ICOUNT = ICOUNT +1
               REWIND(INCROW)
               CALL RDHDR2 (INCROW)
               Doorgaan = .true.
           ELSE
             Doorgaan = .false.
             IF (ENDFIL .AND. IDEFLT .EQ. 1 .AND. ICOUNT .EQ. 0) THEN
               ICOUNT = ICOUNT +1
               REWIND(INCROW)
               CALL RDHDR2 (INCROW)
               Doorgaan = .true.
             ENDIF
           ENDIF
        Enddo
        READ(INCROW,*,END=30,ERR=150,IOSTAT=IECODE) IDUM1, IDUM2, IDUM3, CROPO
        IF (iDebug /= 0) WRITE(IDEBUG,*) ' Crop factors openwater', IDEFLT, IDUM1, IDUM2, IDUM3, CROPO
        If (Idum2 .gt. 12 .or. idum3 .gt. 31) call ErrMsgStandard (968, IECODE, '  ReadCropFactOpenWater', STRING)
        CropOwData (Idum2, IDum3) = CROPO
        Call CheckCropFactorOpenWater (Cropo, Err923, iwarningcount)
        IF (Err923) call ErrMsgStandard (923, IECODE, '  ReadCropFactorFileOpenWater', STRING)
        if (.not. endfil .and. icount .eq. 0) GOTO 11
        if (idebug .ne. 0) then
          DO Imonth=1,12
            DO IDay  =1,31
              write(Idebug,*) ' Crop factor open water Imonth iDay ',IMonth, IDay, CropOwData(Imonth, iday)
            ENDDO
          ENDDO
        Endif
      Else
!       New format OpenWaterCropfactor file, find selected id OpenWaterCropFactDefinition and read table in CRFO records
        CALL SKPCOM (INCROW, ENDFIL, 'ODS ')
        Allow = .false.
        Found = .false.
        Do While (.not. Endfil)
           Success = GetRecord (InCrow,'CRFO', Endfil, Idebug, Iout1) ! get record van keyword CRFO tot crfo
           If (Endfil) goto 30
           Success = GetTableName (TabYesNo, TableName, ' id ', IOut1)
           If (.not. Success) goto 150
           if (idebug .ne. 0) Write(idebug,*) ' TableName got from buffer: ', &
                                                TableName(1:Len_Trim(TableName))
           Len1 = Len_Trim(TableName)
           Len2 = Len_Trim(OpenWaterCropFactDef)
           if (idebug .ne. 0) Write(idebug,*) ' TableName length and OpenWaterCropFactDef length', len1, len2
           If (TableName(1:Len1) .eq. OpenWaterCropFactDef(1:Len2)) then
! specified OpenWaterCropFact definition found
! get table of cropfactors with name TableName, NrColumns=NCrop, TableNr found=Table
             if (idebug .ne. 0) Write(idebug,*) ' TableName = OpenWaterCropFactDefinition found!: ', OpenWaterCropFactDef
             Success = GetTable (TableHandle, TableName, 1, OpenWaterCropFactTableNr, Idebug, Iout1)
             If (.not. Success) goto 150
             TabYesNo = .true.
             Endfil = .true.
           Else
              TabYesNo = .false.
           Endif
        Enddo
        If (.not. TabYesNo) GoTo 30
      Endif
      GOTO 999

!
! *********************************************************************
! *** Error during reading of file
! *********************************************************************
!
  150 CONTINUE
      call ErrMsgStandard (902, IECODE, '  ReadCropFactorFileOpenWater', STRING)

! *********************************************************************
! *** end of file
! *********************************************************************

   30 CONTINUE
      call ErrMsgStandard (911, IECODE, '  ReadCropFactorFileOpenWater', STRING)
!
  999 CONTINUE
!
      RETURN
      END subroutine ReadOpenWaterCropFactorFile



      end module Crop
