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

    Module FileModule


     Use Dh_Alloc

      Implicit None

! Constants
! NFIL = max. aantal files
      integer                 :: NFILE
      integer, parameter	  :: NFIL = 50
      integer, parameter     :: FileCharIdLength = 256

! Variables
! *** Number of input- and output data files

      CHARACTER(len=FileCharIdLength), Pointer, Save :: NAMFIL(:)
      INTEGER, Pointer, Save                         :: INXFIL(:)
      INTEGER                                        :: IFlRtnRtc

     Contains


      Function InitFiles ()  result (RetVal)

        Integer RetVal

        Logical success
        Integer Iout1

        RetVal = 0
        Iout1 = 10

        Success = DH_AllocInit(NFil, NamFil,'')
        Success = Success .and. DH_AllocInit(NFil, InxFil,0)
        If (.not. success) then
           call write_error_message_rtc (911, 1, '  Error allocating arrays in InitFiles', ' ', IOUT1)
           RetVal = 911
        Endif

      Return
      End function Initfiles


      Subroutine OPENFL (IN, NAME, IOUT1, IOPT)

! *********************************************************************
! *** Sobek_RTC
! *** LAST UPDATE : March 2000          BY : Geert Prinsen
! **********************************************************************
! *** Check existence of inputfile
! *** if exists: Open file at unit IN with name NAME
! *** if IOPT=1 then as Sequentiall formatted file, if IOPT=2 as Binary file.
! **********************************************************************

      Integer                         :: IN, IOPT, IOUT1
      CHARACTER(len=FileCharIdLength) :: NAME
      Logical                         :: FNMEXT

      Inquire (FILE = NAME, EXIST = FNMEXT)
      If (.NOT. FNMEXT) call write_error_message_rtc (911, 0, '  Openfl', NAME, IOUT1)
      If (IOPT .EQ. 1) Then
!        OPEN(IN, FILE=NAME, STATUS='OLD')
!
#if (defined(HAVE_CONFIG_H))
         OPEN(IN, FILE=NAME, STATUS='OLD',ACTION='READ')
#else
!        Test with extra MODE and SHARE option; Microsoft specific!
         OPEN(IN, FILE=NAME, STATUS='OLD',MODE='READ',SHARE='DENYWR')
#endif
      Else
!Lahey   OPEN(IN, FILE=NAME, ACCESS='TRANSPARENT', STATUS='OLD')
         OPEN(IN, FILE=NAME, form='unformatted', access='stream', STATUS='OLD')
      Endif

!
      Return
      End subroutine Openfl



      Subroutine ScanFile (In, FileName, Iout1, IOpt, KeyWord, NrRecords, Minimaal1)

! *********************************************************************
! *** Sobek_RTC
! *** LAST UPDATE : March 2000          BY : Geert Prinsen
! **********************************************************************
! *** Precondition: file FileName exists
! *** Scan number of records with keyword KEYWORD in file FILENAME on unit IN
! *** Output: NrRecords (altijd >= 1)
! **********************************************************************

      Integer                         :: In, IOut1, IOpt
      CHARACTER(len=FileCharIdLength) :: FileName
      Character*4                     :: KeyWord, String
      Integer                         :: NrRecords
      Logical                         :: Eof, Minimaal1


      Eof = .false.
      NrRecords = 0

      Call OpenFl(In, FileName, Iout1, IOpt)
      Do While (.not. eof)
         Read (In,'(A4)',Err=999,End=999) String
         If (String(1:4) .eq. KeyWord(1:4)) NrRecords = NrRecords + 1
      Enddo
 999  Eof = .true.

      Close(IN)

      If (Minimaal1) NrRecords = max (1, NrRecords)

      RETURN
      END Subroutine ScanFile


      Subroutine ScanFile2 (In, FileName, Iout1, IOpt, KeyWord, NrRecords)

! *********************************************************************
! *** Sobek_RTC
! *** LAST UPDATE : March 2000          BY : Geert Prinsen
! **********************************************************************
! *** Scan number of records with keyword KEYWORD in file FILENAME on unit IN
! *** Compare with subroutine ScanFile
! *** Differences: file may not exist, keyword length is different, position keyword also flexible
! *** Output: NrRecords (altijd >= 1)
! **********************************************************************

      Integer                         :: In, IOut1, IOpt
      CHARACTER(len=FileCharIdLength) :: FileName
      Character*100                   :: String
      Character*5                     :: KeyWord
      Integer                         :: NrRecords, Pos1
      Logical                         :: Eof, FnmExt


      Eof = .false.
      NrRecords = 0

      Inquire (FILE = FileName, EXIST = FnmExt)
      If (.NOT. FnmExt) Then
!        Write(*,*) ' File does not exist', FileName
         NrRecords = 1
      Else
         Call OpenFl(In, FileName, Iout1, IOpt)
         Do While (.not. eof)
            Read (In,'(A100)',Err=999,End=999) String
            POS1 = INDEX(STRING, KeyWord)
            If (Pos1 .gt. 0) NrRecords = NrRecords + 1
         Enddo
      Endif
 999  Eof = .true.

      Close(IN)

      NrRecords = max (1, NrRecords)

      RETURN
      END Subroutine ScanFile2



      Subroutine ScanFile3 (In, FileName, Idebug, Iout1, IOpt, RecordKeyword, KeyWordArray, MaxNumberArray, ArrayLength)

! *********************************************************************
! *** Sobek_RTC
! *** LAST UPDATE : March 2000          BY : Geert Prinsen
! **********************************************************************
! *** Scan records with keyword RECORDKEYWORD in file FILENAME on unit IN
! *** Find maximum number after keyword KEYWORD
! *** Compare with subroutine ScanFile
! *** Differences: file may not exist, keyword length is different, position keyword also flexible
! *** Output: MaxNumber (should be initialised before calling this routine!!)
! **********************************************************************

      use NewTables_rtc
      use ParseToken_rtc

      Integer                         :: In, IOut1, Idebug, Iopt
      CHARACTER(len=FileCharIdLength) :: FileName
      Integer                         :: ArrayLength
      Integer                         :: MaxNumberArray(ArrayLength)
      Character*4                     :: RecordKeyWord
      Character*10						  :: KeyWordArray(ArrayLength)
      Character*9999                  :: String
      Logical								  :: FnmExt, success

      Integer        i, ReadValue
! Additional variables for ParseToken_rtc
      Integer           ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical           ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive
      Type (TokenArray) RecordData

      IStart = 1
      ScanToTk = 999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive

      Eof = .false.

      Inquire (FILE = FileName, EXIST = FnmExt)
      If (.NOT. FnmExt) Then
!        Write(*,*) ' File does not exist', FileName
      Else
         Call OpenFl(In, FileName, Iout1, IOpt)
         Do While (.not. eof)
            Success = GetRecord (In, RecordKeyword, Eof, Idebug, Iout1)
            Success = GetStringFromBuffer (String)
            Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
            Do i=1,ArrayLength
               if (Getkey (KeywordArray(i), IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                   Read (RecordData%Token(ReturnIndx+1),*,Err=991) ReadValue
                   MaxNumberArray(i) = max (MaxNumberArray(i), ReadValue)
               endif
            Enddo
         Enddo
      Endif
 999  Eof = .true.

      Close(IN)

 991  Continue    ! read error, but no action?

      RETURN
      END Subroutine ScanFile3



      Subroutine ScanFile3b (In, FileName, Idebug, Iout1, IOpt, RecordKeyword, KeyWordArray, MaxNumberArray, ArrayLength)

! *********************************************************************
! *** Sobek_RTC
! *** LAST UPDATE : March 2000          BY : Geert Prinsen
! **********************************************************************
! *** Scan records with keyword RECORDKEYWORD in file FILENAME on unit IN
! *** Find maximum number of times that keyword KEYWORD is found, and KEYWORD is enclosed by quotes
! *** Output: MaxNumber (should be initialised before calling this routine!!)
! **********************************************************************

      use NewTables_rtc
      use ParseToken_rtc

      Integer                         :: In, IOut1, Idebug, Iopt
      CHARACTER(len=FileCharIdLength) :: FileName
      Integer                         :: ArrayLength
      Integer                         :: MaxNumberArray(ArrayLength)
      Character*4                     :: RecordKeyWord
      Character*10                    :: KeyWordArray(ArrayLength), SearchString
      Character*9999                  :: String
      Logical                         :: FnmExt, success

      Integer        i, ReadValue
! Additional variables for ParseToken_rtc
      Integer           ScanToTk, IStart, jStart, ReturnIndx, NumberOfTokens
      Logical           ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive
      Type (TokenArray) RecordData

      IStart = 1
      ScanToTk = 999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive

      Eof = .false.

      Inquire (FILE = FileName, EXIST = FnmExt)
      If (.NOT. FnmExt) Then
!        Write(*,*) ' File does not exist', FileName
      Else
         Call OpenFl(In, FileName, Iout1, IOpt)
         Do While (.not. eof)
            Success = GetRecord (In, RecordKeyword, Eof, Idebug, Iout1)
            Success = GetStringFromBuffer (String)
            Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
            Do i=1,ArrayLength
               jStart = IStart
               ReadValue = 0
               do while (jStart .lt. NumberOfTokens)
                  SearchString = KeywordArray(i)(1:len_trim(KeyWordArray(i)))
                  if (GetkeyEnclosedByQuotes (SearchString, jStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                     ReadValue = ReadValue + 1
                     jStart = ReturnIndx+1
                  else
                     jStart = NumberOfTokens
                  endif
               Enddo
               MaxNumberArray(i) = max (MaxNumberArray(i), ReadValue)
            Enddo
         Enddo
      Endif
 999  Eof = .true.

      Close(IN)

      RETURN
      END Subroutine ScanFile3b


      Subroutine ScanFile4 (In, FileName, Idebug, Iout1, IOpt, RecordKeyword, KeyWordArray, MaxNumberArray, CheckArray, ArrayLength)

! *********************************************************************
! *** Sobek_RTC
! *** LAST UPDATE : March 2000          BY : Geert Prinsen
! **********************************************************************
! *** Scan records with keyword RECORDKEYWORD in file FILENAME on unit IN
! *** First set of checks (CheckArray(i)=true)
! *** If CheckArray(i)=true : check keyword from KEYWORDARRAY and corresponding value in MaxNumberArray
! *** For other members:
! *** If CheckArray(i)=false: find maximum number after keywordArray()
! **********************************************************************

      use NewTables_rtc
      use ParseToken_rtc

      Integer                         :: In, IOut1, Idebug, Iopt
      CHARACTER(len=FileCharIdLength) :: FileName
      Integer                         :: ArrayLength
      Integer                         :: MaxNumberArray(ArrayLength)
      Character*4                     :: RecordKeyWord
      Character*10                    :: KeyWordArray(ArrayLength)
      logical                         :: CheckArray(ArrayLength)
      Character*9999						  :: String
      Logical                         :: FnmExt, success

      Integer        i, ReadValue
! Additional variables for ParseToken_rtc
      Integer           ScanToTk, IStart, ReturnIndx, NumberOfTokens
      Logical           ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive
      Type (TokenArray) RecordData

      IStart = 1
      ScanToTk = 999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive

      Eof = .false.

      Inquire (FILE = FileName, EXIST = FnmExt)
      If (.NOT. FnmExt) Then
!        Write(*,*) ' File does not exist', FileName
      Else
         Call OpenFl(In, FileName, Iout1, IOpt)
         Do While (.not. eof)
            Success = GetRecord (In, RecordKeyword, Eof, Idebug, Iout1)
            Success = GetStringFromBuffer (String)
            Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
            Do i=1,ArrayLength
               if (CheckArray(i)) then
                   if (Getkey (KeywordArray(i), IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                       Read (RecordData%Token(ReturnIndx+1),*,Err=991) ReadValue
                       If (MaxNumberArray(i) .ne. MaxNumberArray(i)) goto 99   ! means to skip loop over index i
                   else
                       goto 99   ! means to skip loop over index i
                   endif
               elseif (Getkey (KeywordArray(i), IStart, RecordData, NumberOfTokens, ReturnIndx, ParseTokenSearchCaseSensitive)) then
                   Read (RecordData%Token(ReturnIndx+1),*,Err=991) ReadValue
                   MaxNumberArray(i) = max (MaxNumberArray(i), ReadValue)
               endif
  99           Continue
            Enddo
         Enddo
      Endif
 999  Eof = .true.

      Close(IN)

 991  Continue    ! read error, but no action?

      RETURN
      END Subroutine ScanFile4


    End Module FileModule
