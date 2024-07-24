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

    module LanguageModule

! August 2004: updated for multiple use

! use
    use Dh_Alloc
! Constants

! Variables
  implicit none
  
!     MxLang = max. aantal items in languagefile
!     NrLanguages = aantal talen

  type SobekLanguageStore
     integer modelHandle
     integer MxLang
     integer NrLanguages
     Integer nrLanguageItems, IActiveLanguage
     Integer, dimension(:), Pointer :: LanguageArray (:)
     CHARACTER(Len=999), dimension(:), Pointer   :: CaptionLanguage (:)
     CHARACTER(Len=999), dimension(:,:), Pointer :: LanguageStrings (:,:)
  end type SobekLanguageStore

!     Integer MxLang
!     Integer NrLanguages
!     Integer nrLanguageItems, IActiveLanguage

!     Integer, Pointer, Save :: LanguageArray (:)
!     CHARACTER(Len=999), Pointer, Save :: CaptionLanguage (:), LanguageStrings (:,:)

  type T_SobekLanguageStore
     type (SobekLanguageStore), pointer, dimension(:) :: SbkLanguages
     integer  :: numModels
     integer  :: initialized = -999
  end type T_SobekLanguageStore

  type (T_SobekLanguageStore), private, save :: SobekLanguages


!
! PRIVATE FUNCTIONS
!
integer, parameter, private  :: MaxNumModels = 5
integer, parameter, private  :: LanguagesInitializedValue = 1234
integer, parameter, private  :: Languages_Undefined       = -999

integer, private :: currentModelHandle = 0


private LanguagesModelFind_ByHandle
private LanguagesModelAdd
private LanguagesCleanup
! private ReadString



   Contains


! Create / Destroy SobekLanguage DataStore

function LanguagesInitialized() result(retVal)

    implicit none
    
    logical :: retVal

    retVal = .false.
    if ( SobekLanguages % initialized == LanguagesInitializedValue ) retVal = .true.

end function LanguagesInitialized


subroutine LanguagesCreate

    if ( LanguagesInitialized() ) return

    allocate(SobekLanguages % SbkLanguages(MaxNumModels) )

    SobekLanguages % SbkLanguages(:) % modelHandle = Languages_UNDEFINED
    SobekLanguages % numModels   = 0
    SobekLanguages % initialized = LanguagesInitializedValue

end subroutine LanguagesCreate


subroutine LanguagesDestroy

    integer :: i

    if ( .not. LanguagesInitialized() ) return

    Do i=1, SobekLanguages % NumModels
       Call LanguagesCleanup (SobekLanguages%SbkLanguages(i))
    Enddo
    deallocate(SobekLanguages % SbkLanguages)

    SobekLanguages % numModels    = 0
    SobekLanguages % initialized  = Languages_Undefined

end subroutine LanguagesDestroy


subroutine LanguagesCleanup (SobekLanguage)

    type (SobekLanguageStore) SobekLanguage

    if ( .not. LanguagesInitialized() ) return

    SobekLanguage % ModelHandle  = Languages_Undefined
    SobekLanguage % MxLang       = 0
    SobekLanguage % NrLanguages  = 0
    SobekLanguage % NrLanguageItems = 0
    SobekLanguage % IActiveLanguage = 0
    deallocate (SobekLanguage % LanguageArray)
    deallocate (SobekLanguage % CaptionLanguage)
    deallocate (SobekLanguage % LanguageStrings)

end subroutine LanguagesCleanup


!-----------------------------------------------------------------
! CREATE / DESTROY / GET HANDLE TO MODEL-(=COMP./SCHEM.COMBI
!-----------------------------------------------------------------


function LanguagesModelFindOrCreate (inputHandle)  result(modelHandle)

    integer                      :: inputHandle, modelHandle

    type(SobekLanguageStore), pointer     :: model

    ! body
    ! provide valid handle by default
    modelHandle = 1

    if ( .not. LanguagesInitialized() ) return

    ! find or create model handle
    modelHandle = Languages_UNDEFINED
    nullify(model)
    model => LanguagesModelFind_ByHandle (inputHandle)

    if ( associated(model) ) then
        modelHandle = model % modelHandle
    else
        currentModelHandle = currentModelHandle + 1
        model => LanguagesModelAdd(currentModelHandle)
    endif

    if ( associated(model) ) modelHandle = model % modelHandle

end function LanguagesModelFindOrCreate


!
! Find / add models
!

function LanguagesModelAdd(modelHandle) result(model)

    ! return value
    type (SobekLanguageStore) , pointer    :: model

    ! arguments
    integer         , intent(in) :: modelHandle

    nullify(model) ; if ( .not. LanguagesInitialized() ) return

    if ( SobekLanguages % numModels >= MaxNumModels ) then
        write (*, '(A,I4,A)')  'ERROR: LanguagemodelCreate: max #models (', MaxNumModels, ') exceeded'
    else
        SobekLanguages % numModels = SobekLanguages % numModels + 1
        model => SobekLanguages % SbkLanguages (SobekLanguages%numModels)
        model % modelHandle = modelHandle
        model % MxLang      = 0
        model % NrLanguages = 0
        model % NrLanguageItems = 0
        model % IActiveLanguage = 0

    endif

end function LanguagesModelAdd


function LanguagesModelFind_ByHandle(modelHandle)  result(model)

    ! return value
    type(SobekLanguageStore) , pointer  :: model

    ! arguments
    integer, intent(in)        :: modelHandle

    ! locals
    integer :: m

    ! body

    nullify(model) ; if ( .not. LanguagesInitialized() ) return

    do m = 1 , SobekLanguages % numModels
        if ( SobekLanguages % SbkLanguages(m) % modelHandle /= Languages_UNDEFINED) then
            if ( SobekLanguages% SbkLanguages(m) % modelHandle ==  modelHandle ) then
                model => SobekLanguages % SbkLanguages(m)
                exit
            endif
        endif
    enddo

end function LanguagesModelFind_ByHandle






      Subroutine SetActiveLanguage (ModelHandle, ActiveLanguage)
        ! *********************************************************************
        ! ***                D E L F T         H Y D R A U L I C S
        ! ***
        ! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
        ! *********************************************************************
        ! *** Program :  DELFT_3B version 1.0.                 Date: Nov 1996
        ! *********************************************************************
        ! *** Last update: Nov    1996       By : Geert Prinsen
        ! *********************************************************************
        ! *** Brief description:
        ! *** ------------------
        ! ***   Inlezen language file
        ! *********************************************************************
        ! *** Input/output parameters:
        ! *** ------------------------
        ! ***  IDEBUG = file unit number of debug file
        ! ***  IN     = file unit number of input file
        ! *********************************************************************

      integer         , intent(in) :: modelHandle, ActiveLanguage

      type (SobekLanguageStore), pointer :: model


      model => LanguagesModelFind_ByHandle(modelHandle)
      model%IActiveLanguage = 0
      if (ActiveLanguage .le. model%NrLanguages .and. ActiveLanguage .ge. 1) then
         if (model%LanguageArray(ActiveLanguage+1) .eq. -1) model%IActiveLanguage = ActiveLanguage
      endif

      Return
      END subroutine SetActiveLanguage


      Subroutine ReadLanguageFile (ModelHandle, IN, iDebug, Iout1, ireturncode)
        ! *********************************************************************
        ! ***                D E L F T         H Y D R A U L I C S
        ! ***
        ! ***              WATER RESOURCES AND ENVIRONMENT DIVISION
        ! *********************************************************************
        ! *** Program :  DELFT_3B version 1.0.                 Date: Nov 1996
        ! *********************************************************************
        ! *** Last update: Nov    1996       By : Geert Prinsen
        ! *********************************************************************
        ! *** Brief description:
        ! *** ------------------
        ! ***   Inlezen language file
        ! *********************************************************************
        ! *** Input/output parameters:
        ! *** ------------------------
        ! ***  IDEBUG = file unit number of debug file
        ! ***  IN     = file unit number of input file
        ! *********************************************************************

      use ParseToken
      use ReadLib

      integer         , intent(in) :: modelHandle, in, Idebug, Iout1

      type (SobekLanguageStore), pointer :: model


      INTEGER       i, j, L0, L1, teller, ireturncode
      CHARACTER(Len=999) CaptionString, STRING  !ReadString
      Character(Len=3) Version
      Integer       dummy
      Logical       Success

! Additional variables for ParseToken
      Integer        ScanToTk, IStart, NumberOfTokens
      Logical        ParseTokenReadCaseSensitive, ParseTokenSearchCaseSensitive, ReadError
      Type (TokenArray) RecordData
! end of additional variables ParseToken

! *********************************************************************
! *** check header of file
! *********************************************************************

      model => LanguagesModelFind_ByHandle(modelHandle)

      ReadError = .true.     ! default value = true
      Ireturncode = 972

! Parsetoken settings
      IStart   = 1     ! Scan from token 1
      ScanToTk = 999   ! Scan up to Token 999
      ParseTokenReadCaseSensitive = .true.      ! no conversion to upper case; ParseToken fills array in original case
      ParseTokenSearchCaseSensitive = .false.   ! find keywords case-insensitive

! read file
      READ(IN, '(A)',Err=101,End=101) STRING
      IF (STRING(5:11) .NE. 'Version') THEN
          ireturnCode=972
          Goto 999
      ENDIF
      Version = String(15:17)
!     Language file Header, 2 versions
      if (Version .eq. '1.0') then
          ! determine number of languages, first one is always caption language
          READ(IN,*,ERR=101,END=101) Dummy, model%NrLanguages, String
          dummy = 0
          ! allocation of memory voor for the LanguageArray, since we now know the number of languages
          Success = Dh_AllocInit (model%NrLanguages, model%LanguageArray, 0)
          If (.not. success) then
            ireturnCode=981
            goto 999
          Endif
          ! read active languages
          read(IN, *,ERR=101, END=101) Dummy, model%nrLanguages, model%LanguageArray
          dummy = 0
          model%IActiveLanguage = 0
       elseif (Version .eq. '2.0') then
          ! determine number of languages, first one is always caption language
          READ(IN,*,ERR=101,END=101) Dummy, model%NrLanguages, String
          dummy = 0
          ! allocation of memory voor for the LanguageArray, since we now know the number of languages
          Success = Dh_AllocInit (model%NrLanguages, model%LanguageArray, 0)
          If (.not. success) then
            ireturnCode=981
            goto 999
          Endif
          ! read active languages
          READ(IN,'(A999)',ERR=101,END=101) String
          Success = ParseTokenArrayWithKeywords (String, ScanToTk, RecordData, NumberOfTokens, ParseTokenReadCaseSensitive)
          If (.not. success) then
            ireturnCode=972
            goto 999
          Endif
          model%LanguageArray = 0
          Do i=1,model%NrLanguages
             if(RecordData%Token(i+2) .eq. 'True' .or. RecordData%Token(i+2) .eq. 'true') model%LanguageArray(i)= -1
          Enddo
          ! skip 3 lines
          read(IN, *,ERR=101, END=101) Dummy
          read(IN, *,ERR=101, END=101) Dummy
          read(IN, *,ERR=101, END=101) Dummy
          dummy = 0
          model%IActiveLanguage = 0
       else
          goto 101
       endif

      Ireturncode = 0
      ReadError = .false.
  101 Continue
      If (ReadError) then
          ireturnCode=972
          goto 999
      Endif

! *********************************************************************
! *** read data
! *********************************************************************

      ReadError = .true.     ! default value = true

      DO teller =1,9999
         READ (IN,'(A)', Err=21,END=21) CaptionString
      ENDDO
   21 CONTINUE
      model%nrLanguageItems = teller-1

      If (iDebug .ne. 0) Write(IDEBUG,*) ' Number of items=',model%nrLanguageItems

      model%MxLang = model%nrLanguageItems

! alloceren van geheugenruimte voor de captionlanguage
! en de LanguageStrings

      Success = Dh_AllocInit (model%NrLanguageItems, model%CaptionLanguage, '')
      Success = Dh_AllocInit (model%NrLanguageItems, model%NrLanguages, model%LanguageStrings, '')
      If (.not. success) then
          ireturnCode=981
          goto 999
      Endif


      REWIND(IN)

! skip first 3 lines in languagefile => header
      do teller = 1, 3
        READ(IN, *, Err=21,END=21) string
      Enddo

! filter the subsequent strings out of the line
      Do teller = 1, model%nrLanguageItems
          ! read whole line
         READ (IN, '(A)', ERR=150,End=150) CaptionString
         IF (iDebug .ne. 0) WRITE(IDEBUG,'(A)') CaptionString
          ! stop de 1e string in de CaptionLanguage-array
          model%CaptionLanguage(teller) =  ReadString(CaptionString, 1)
          ! stop de IActiveLanguage-de string in de LanguageStrings-array
          Do i=1,model%NrLanguages
             model%LanguageStrings(teller,i) = ReadString(CaptionString, i+1)
          Enddo
      Enddo
      ReadError = .false.

        ! *********************************************************************
        ! *** Error during reading of file
        ! *********************************************************************

  150 CONTINUE
      If (ReadError) then
          ireturnCode=972
          goto 999
      Endif

        ! *********************************************************************
        ! *** end of file
        ! *********************************************************************

   30 CONTINUE

      IF (iDebug .ne. 0) THEN
          WRITE (IDEBUG,*) ' Language file data: caption language, language j=',j
          DO j=1,model%nrLanguages
            DO I=1,model%nrLanguageItems
              L0 = MAX (1,Len_Trim(model%CaptionLanguage(I)) )
              L1 = MAX (1,Len_Trim(model%LanguageStrings(I,model%iActiveLanguage)) )
              WRITE (IDEBUG,'(I3,/,1X,I3,A,/,1X,I3,A)') &
                       I, Len_Trim(model%CaptionLanguage(I)), model%CaptionLanguage(I)(1:L0), &
                        Len_Trim(model%LanguageStrings(I,j)), model%LanguageStrings(I,j)(1:L1)
            ENDDO
          ENDDO
      ENDIF

!      Call Lng_Errmsg (981, 0, ' Error allocating arrays in subroutine ', ' ReadLanguage ',Iout1)
!      Call Lng_ErrMsg(972, 0,'Error in header language file',' ',IOUT1)
!      call Lng_ErrMsg(972, 0, ' Error in Language file', '',Iout1)
  999 Continue

      Return
      END subroutine ReadLanguageFile



      Character(999) FUNCTION TranslateString(ModelHandle, InputString)
      !--------------------------------------------------------------------
      !     FNDST2.FOR: Find string InputString in caption language array
      !                 en zet InputString op het resultaat LanguageStrings(iactivelanguage)
      !                 Als het language array voor dit item niet gevuld, geef de oorspronkelijke string terug
      !--------------------------------------------------------------------

      use ReadLib

      integer         , intent(in) :: modelHandle

      type (SobekLanguageStore), pointer :: model

      CHARACTER(*) InputString
      CHARACTER(999) OutputString
      INTEGER  IPOS, index, lb, la
      Logical  WithSpaces

      model => LanguagesModelFind_ByHandle(modelHandle)
      if (.not. associated(model)) then
          TranslateString = InputString
          return
      endif

      OutputString = ''
      WithSpaces = .False.
      TranslateString = ' '
      IPOS = -1
      LA = Len_Trim(InputString)
      DO INDEX=1, model%NrLanguageItems
        LB = Len_Trim(model%CaptionLanguage(INDEX))
        IF (LA .EQ. LB) IPOS = FNDFRST(InputString,model%CaptionLanguage(INDEX),WithSpaces)
        IF (IPOS .GT. 0) GOTO 101
      ENDDO
!     Not found
      INDEX = -1
  101 CONTINUE

      IF (INDEX .GT. 0 .and. model%IActiveLanguage .gt. 0)  THEN
        LA = Len_Trim(model%LanguageStrings(INDEX,model%IActiveLanguage))
        IF (LA .GT. 0) then
           OutputString = model%LanguageStrings(INDEX,model%IActiveLanguage)
        else
           OutputString = InputString (1:Len_Trim(InputString))
        endif
      ELSE
        LA = Len_Trim (InputString)
        OutputString = InputString (1:Len_Trim(InputString))
      ENDIF

      TranslateString = OutputString

      Return
      END FUNCTION TranslateString




      Character(999) Function ReadString(line, teller)
     ! variables
      Character(999) line, tmpLine
      Integer teller, tmpTeller, position1, position2

      tmpLine = line
      do tmpTeller = 1, teller
         position1 = scan(tmpLine, ",")
         tmpLine = tmpLine(position1 + 1: len(tmpLine))
         position2 = scan(tmpLine, ",")
      enddo
      tmpLine = tmpLine(2: )
      position2 = scan(tmpLine, '"')
      tmpLine = tmpLine(1: position2 - 1) ! -1 to avoid the quotes and comma's
      ReadString = tmpLine

      Return
      End Function ReadString



    END Module LanguageModule
