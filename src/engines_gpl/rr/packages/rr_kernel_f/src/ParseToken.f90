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

     Module ParseToken

    use ReadLib
!  Module based on ParseToken VB code, received from JanJaapBrinkman March 2002

    implicit none
!   Maximum number of tokens in a string   (60 is large enough for Unpaved.3b, Pluvius.alg, DWA files paved/Nwrw)
!   Maximum number of tokens in a string for RTC increased (necessary for PARA records with many locations)
    Integer     MaxTk
    Parameter  (MaxTk = 1999)
    Integer     MaxStrLength
    Parameter  (MaxStrLength=9999)

!   Type TokenArray = 2 arrays: array with tokens, and indicator whether token is between quotes
!   GP Nov 2002 : added 3rd array, the startposition of the token in the original string
!   GP Nov 2002 : modified checks with CaseSensitivity
!   GP Oct 2004 : added SetOneVariable, SetVariables
!   GP Jan 2005 : added SetVariables for 2D and 3D arrays
    Type TokenArray
      Logical       IsEnclosedByQuotes (MaxTk)
!     Character*256 Token (MaxTk)   ! now limited to max. Sobek token length of 80 char
      Character(len=80) Token (MaxTk)
      Integer       StartPositionOfToken(MaxTk)
    End Type

    interface SetOneVariable
          module procedure SetOneIntegerVariable
          module procedure SetOneRealVariable
          module procedure SetOneDoubleVariable
          module procedure SetOneCharVariable
    end interface


    interface SetVariables
          module procedure SetIntegerVariables
          module procedure SetRealVariables
          module procedure SetDoubleVariables
          module procedure SetCharVariables
          module procedure Set2DIntegerVariables
          module procedure Set2DRealVariables
          module procedure Set2DDoubleVariables
          module procedure Set2DCharVariables
          module procedure Set2DCharOrIntegerVariables
          module procedure Set3DRealVariablesRunningIndex2
          module procedure Set3DDoubleVariablesRunningIndex2
          module procedure Set3DCharVariablesRunningIndex3
    end interface


    contains


!--------------------------------------------------------------------
!     Own routine
!     FindFirstPos: Finds position of first occurrence of string A in string B
!     Diff. with FndFrst from Readlib: String B may be a blank (' ')
!--------------------------------------------------------------------

   Integer Function FindFirstPos (STRNGA, STRNGB, WithSpaces)

   implicit none
   
   Character(len=*) STRNGA, STRNGB
   Logical WithSpaces

   INTEGER I, IPOS, LA, LB

   IPOS = -1

! zoekstring STRNGA in- of ex-clusief spaties aan het eind
! in string STRNGB kan altijd exclusief spaties aan het eind; na de
   IF (WithSpaces) Then
      LA=LENSTRING(STRNGA)
      LB=LENSTRING(STRNGB)
   Else
      LA=Len_Trim(STRNGA)
      LB=Len_Trim(STRNGB)
   Endif
!  WRITE(*,'(A)')' Fndfrst'
!  WRITE(*,'(A,1X,A)') ' Zoek string', STRNGA(1:LA), LA
!  WRITE(*,'(A,1X,A)') ' in string', STRNGB(1:LB), LB

   DO I=1,LB
       IF (I+LA-1 .GT. LB) GOTO 999
       IF (STRNGB(I:I+LA-1) .EQ. STRNGA) THEN
           IPOS = I
           GOTO 999
       ENDIF
   ENDDO

  999 CONTINUE

   FindFirstPos=IPOS

   Return
   End Function FindFirstPos



    Logical Function ParseTokenArrayWithKeywords(Buffr, ScanToTk,ResultArr, NumberOfTokens, CaseSensitive)
!   Routine based on VB code
!   Buffr = input string
!   ScantoTk = number of tokens to scan
!   ResultArr = result array (tokens and quoteindicator)
!   NumberOfTokens = actual number of tokens read from string (may be smaller than ScanToTk)
!   CaseSensitive  = indicator whether string comparison should be case sensitive or not

    implicit none
    
    Character(len=*)   Buffr
    type (TokenArray)  ResultArr
    integer            ScanToTk, NumberOfTokens, NumTk
    Logical            CaseSensitive

    Integer    r, r2, ilen, StartPos
!   Integer    i, Idebug
    Logical    WithSpaces
    Logical    IsEnclosed, ActiveSepC


!   Separators: space, quote, double quote, comma
    Character(len=1) cSpace, cSepQ, cSepDQ, cSepC
!   Local copy of input string
    Character(Len=MaxStrLength) LocalBuffer

    ParseTokenArrayWithKeywords= .false.

    LocalBuffer = ' '
    LocalBuffer(1:) = Buffr
    If (Len_Trim(Buffr) .gt. Len_Trim(LocalBuffer)) Return   ! local buffer too small
    NumTk = 0
    ActiveSepC = .true.

    cSpace = ' '
    cSepQ  = ''''
    cSepDQ = '"'
    cSepC  = ','
    WithSpaces = .true.

!   Parse token Ix from string using Space or Comma
!   Token enclosed by quote or double quote

    r = 0
    StartPos = 1
    if (.not. CaseSensitive) Call UPPERC(LocalBuffer)

!   Idebug = 42
!   Write(Idebug,*) ' TokenArray results'
!   Write(Idebug,*) ' Buffer'
!   Write(Idebug,'(A)') Buffr(1:Len_Trim(Buffr))
!   Write(Idebug,'(A,X,L)') 'CaseSensitive=',CaseSensitive
!   Write(Idebug,*) ' LocalBuffer'
!   Write(Idebug,'(A)') LocalBuffer(1:Len_Trim(LocalBuffer))

    Do while (LocalBuffer .ne. '')

      IsEnclosed = .False.
      StartPos = StartPos + r
      LocalBuffer = LocalBuffer(r+1:)
!      Call Ltrim(LocalBuffer)   ivm probleem met bepaling StartPos meerdere spaties tussen keywords
      Do While (LocalBuffer(1:1) .eq. cSpace .and. LocalBuffer .ne. ' ')
         StartPos = StartPos + 1
         LocalBuffer = LocalBuffer(2:)
      Enddo
      If (LocalBuffer(1:1) .eq. cSepC) Then
        If (.not. ActiveSepC) then
           StartPos = StartPos + 1
           LocalBuffer = LocalBuffer(2:)
        Endif
        Do While (LocalBuffer(1:1) .eq. cSepC)
          numTk = numTk + 1
          If (numTk .gt. maxTk) then
!              call SetMessage(LEVEL_FATAL, 'Error: dimension MaxTk too small')
              Return
          Endif
          ResultArr%Token(numTk) = ''
          ResultArr%IsEnclosedByQuotes(NumTk) = .False.
          ResultArr%StartPositionOfToken(NumTk) = StartPos
          If (ScanToTk .eq. numTk) Then
             Goto 999
          Else
             StartPos = StartPos + 1
             LocalBuffer = LocalBuffer(2:)
          Endif
        Enddo
      EndIf
      ActiveSepC = .False.
      ilen = Len_Trim(LocalBuffer)
      LocalBuffer(ilen+1:ilen+1) = CSpace
      If (LocalBuffer(1:1) .eq. cSepQ) Then
        StartPos = StartPos + 1
        LocalBuffer = LocalBuffer(2:)
        r = Index(LocalBuffer,cSepQ)
        IsEnclosed = .True.
      ElseIf (LocalBuffer(1:1) .eq. cSepDQ) Then
        StartPos = StartPos + 1
        LocalBuffer = LocalBuffer(2:)
        r = Index(LocalBuffer,cSepDQ)
        IsEnclosed = .True.
      Else
        r  = Index(LocalBuffer,cSpace)
        r2 = Index(LocalBuffer,cSepC )
        If (r2 .gt. 0 .and. r2 .lt. r) Then
           r = r2
           ActiveSepC = .True.
        EndIf
      EndIf
      If (r .eq. 0) Goto 999
      numTk = numTk + 1
      If (numTk .gt. maxTk) then
!          call SetMessage(LEVEL_FATAL, 'Error: dimension MaxTk too small')
          Return
      Endif
      ResultArr%Token(numTk) = LocalBuffer(1:r-1)
      ResultArr%IsEnclosedByQuotes(NumTk) = IsEnclosed
      ResultArr%StartPositionOfToken(NumTk) = StartPos
!     If (IsEnclosed) StartPos = StartPos + 1
      If (ScanToTk .eq. numTk) Goto 999
    Enddo

999 continue
    NumberOfTokens = NumTk

!   Idebug = 42
!   Write(Idebug,*) ' ParseToken'
!   Write(idebug,*) ' Nr  StartPos  Quotes Token '
!   Do i=1,NumTk
!      write(idebug,'(I3,I5,L,1X,A)') i, ResultArr%StartPositionOfToken(i), &
!                                     ResultArr%IsEnclosedByQuotes(i), ResultArr%Token(i)
!   Enddo

    ParseTokenArrayWithKeywords= .true.

   Return
   End Function ParseTokenArrayWithKeywords




  Logical Function GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndex, CaseSensitive)

! Routine based on VB code
! FindToken = token to look for
! Start = Start index
! TokenArr = Array with Tokens
! nrOfTokens = number of tokens in array
! ReturnIndex = if >0 : found position in TokenArr containing the FindToken
! CaseSensitive  = indicator whether string comparison should be case sensitive or not

  implicit none
  
  type (TokenArray)  TokenArr

  integer      i, Start, NrOfTokens, ReturnIndex
  Character(*) FindToken
! maximale lengte zoekstring (keyword) 10 karakters (meestal is 2 of 4 genoeg)
  Character(len=10) LocalFindToken, CheckToken
  Logical CaseSensitive
  Integer      LocalStart, LenCheck, LenFind
! Integer      Idebug

! Idebug = 42
  LocalStart = Start
  If (LocalStart .le. 0) LocalStart = 1
  LocalFindToken = FindToken
  if (.not. CaseSensitive) Call UPPERC (LocalFindToken)
  LenFind=Len_Trim(LocalFindToken)
! If (Idebug .gt. 0) write(Idebug,*) ' LocalFindToken=',LocalFindToken(1:LenFind)
  If (LocalFindToken .ne. '') Then
    Do i=LocalStart, NrOfTokens
      CheckToken = TokenArr%Token(i)
      If (.not. TokenArr%IsEnclosedByQuotes(i))  Then
         if (.not. CaseSensitive) Call UPPERC (CheckToken)
!        If (Idebug .gt. 0) write(Idebug,*) ' Test index ', i
         LenCheck=Len_Trim(CheckToken)
!        If (Idebug .gt. 0) write(Idebug,*) ' CheckToken=',CheckToken(1:LenCheck)
         If (CheckToken(1:LenCheck) .eq. LocalFindToken(1:LenFind) ) Then
!           If (Idebug .gt. 0) write(Idebug,*) ' Found index ', i
            ReturnIndex = i
            GetKey = .True.
            Goto 999
         EndIf
      EndIf
    Enddo
  EndIf

  ReturnIndex = 0
  GetKey = .False.


999 Continue
   Return
   End Function GetKey


  Logical Function GetKeyEnclosedByQuotes(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndex, CaseSensitive)

! Routine based on VB code
! FindToken = token to look for
! Start = Start index
! TokenArr = Array with Tokens
! nrOfTokens = number of tokens in array
! ReturnIndex = if >0 : found position in TokenArr containing the FindToken
! CaseSensitive  = indicator whether string comparison should be case sensitive or not

! Difference with GetKey: Now only look in the 'enclosed by Quotes' fields
! Change request: make overloaded function GetKey combining both functions

  implicit none
  
  type (TokenArray)  TokenArr

  integer      i, Start, NrOfTokens, ReturnIndex
  Character(*) FindToken
! maximale lengte zoekstring (keyword) 10 karakters (meestal is 2 of 4 genoeg)
  Character(len=10) LocalFindToken, CheckToken
  Logical CaseSensitive
  Integer      LocalStart, LenCheck, LenFind
! Integer      Idebug

! Idebug = 42
  LocalStart = Start
  If (LocalStart .le. 0) LocalStart = 1
  LocalFindToken = FindToken
  if (.not. CaseSensitive) Call UPPERC (LocalFindToken)
  LenFind=Len_Trim(LocalFindToken)
! If (Idebug .gt. 0) write(Idebug,*) ' LocalFindToken=',LocalFindToken(1:LenFind)
  If (LocalFindToken .ne. '') Then
    Do i=LocalStart, NrOfTokens
      CheckToken = TokenArr%Token(i)
      If (TokenArr%IsEnclosedByQuotes(i))  Then
         if (.not. CaseSensitive) Call UPPERC (CheckToken)
!        If (Idebug .gt. 0) write(Idebug,*) ' Test index ', i
         LenCheck=Len_Trim(CheckToken)
!        If (Idebug .gt. 0) write(Idebug,*) ' CheckToken=',CheckToken(1:LenCheck)
         If (CheckToken(1:LenCheck) .eq. LocalFindToken(1:LenFind) ) Then
!           If (Idebug .gt. 0) write(Idebug,*) ' Found index ', i
            ReturnIndex = i
            GetKeyEnclosedByQuotes = .True.
            Goto 999
         EndIf
      EndIf
    Enddo
  EndIf

  ReturnIndex = 0
  GetKeyEnclosedByQuotes = .False.


999 Continue
   Return
   End Function GetKeyEnclosedByQuotes



   Logical Function SetOneIntegerVariable(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, ReturnVariable)

   implicit none
   
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx
   Integer      ReturnVariable

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     Read (TokenArr%Token(ReturnIndx+1),*) ReturnVariable
     SetOneIntegerVariable = .true.
   else
     SetOneIntegerVariable = .false.
   endif

   Return
   End Function SetOneIntegerVariable


   Logical Function SetOneRealVariable(FindToken, Start,TokenArr, NrOfTokens, CaseSensitive, ReturnVariable)

   implicit none
   
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx
   Real         ReturnVariable

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     Read (TokenArr%Token(ReturnIndx+1),*) ReturnVariable
     SetOneRealVariable = .true.
   else
     SetOneRealVariable = .false.
   endif

   Return
   End Function SetOneRealVariable


   Logical Function SetOneDoubleVariable(FindToken, Start,TokenArr, NrOfTokens, CaseSensitive, ReturnVariable)

   implicit none
   
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx
   Double Precision ReturnVariable

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     Read (TokenArr%Token(ReturnIndx+1),*) ReturnVariable
     SetOneDoubleVariable = .true.
   else
     SetOneDoubleVariable = .false.
   endif

   Return
   End Function SetOneDoubleVariable


   Logical Function SetOneCharVariable(FindToken, Start,TokenArr, NrOfTokens, CaseSensitive, ReturnVariable)

   implicit none
   
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx
   Character(*) ReturnVariable

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     ReturnVariable = TokenArr%Token(ReturnIndx+1)
     SetOneCharVariable = .true.
   else
     SetOneCharVariable = .false.
   endif

   Return
   End Function SetOneCharVariable


   Logical Function SetTwoVariables(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, ReturnVariable1, ReturnVariable2)

   ! first variable is integer (eg. computation option)
   ! second variable is character, regerence to parameter definition associated with chosen option
   implicit none
   
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx
   Integer      ReturnVariable1
   Character(*) ReturnVariable2

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     Read (TokenArr%Token(ReturnIndx+1),*) ReturnVariable1
     ReturnVariable2 = TokenArr%Token(ReturnIndx+2)
     SetTwoVariables = .true.
   else
     SetTwoVariables = .false.
   endif

   Return
   End Function SetTwoVariables



   Logical Function SetRealVariables(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, ReturnVariable, NrVars)

   ! set array of real variables
   implicit none
   
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, NrVars
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx, i
   Real      :: ReturnVariable(NrVars)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        Read (TokenArr%Token(ReturnIndx+i),*) ReturnVariable(i)
     enddo
     SetRealVariables = .true.
   else
     SetRealVariables = .false.
   endif

   Return
   End Function SetRealVariables


   Logical Function SetIntegerVariables(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, ReturnVariable, NrVars)

   implicit none
   
   ! set array of integer variables
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, NrVars, i
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx
   Integer   :: ReturnVariable(NrVars)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        Read (TokenArr%Token(ReturnIndx+i),*) ReturnVariable(i)
     enddo
     SetIntegerVariables = .true.
   else
     SetIntegerVariables = .false.
   endif

   Return
   End Function SetIntegerVariables



   Logical Function SetDoubleVariables(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, ReturnVariable, NrVars)

   implicit none
   
   ! set array of integer variables
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, NrVars, i
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx
   Double Precision ::  ReturnVariable(NrVars)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        Read (TokenArr%Token(ReturnIndx+i),*) ReturnVariable(i)
     enddo
     SetDoubleVariables = .true.
   else
     SetDoubleVariables = .false.
   endif

   Return
   End Function SetDoubleVariables



   Logical Function SetCharVariables(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, ReturnVariable, NrVars)

   implicit none
   
   ! set array of integer variables
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, NrVars, i
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx
   Character(*) :: ReturnVariable(NrVars)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        ReturnVariable(i) = TokenArr%Token(ReturnIndx+i)
     enddo
     SetCharVariables = .true.
   else
     SetCharVariables = .false.
   endif

   Return
   End Function SetCharVariables


   Logical Function Set2DRealVariables(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, ReturnVariable, &
                                       Dim1, Dim2, Index1, NrVars)

   ! set array of real variables
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, Index1, NrVars
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx, i, Dim1, Dim2
   Real      :: ReturnVariable(Dim1,Dim2)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        Read (TokenArr%Token(ReturnIndx+i),*) ReturnVariable(index1,i)
     enddo
     Set2DRealVariables = .true.
   else
     Set2DRealVariables = .false.
   endif

   Return
   End Function Set2DRealVariables


   Logical Function Set2DIntegerVariables(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, ReturnVariable, &
                                          Dim1, Dim2, Index1, NrVars)

   implicit none
   
   ! set array of integer variables
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, Index1, NrVars, i
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx, Dim1, Dim2
   Integer   :: ReturnVariable(Dim1,Dim2)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        Read (TokenArr%Token(ReturnIndx+i),*) ReturnVariable(Index1,i)
     enddo
     Set2DIntegerVariables = .true.
   else
     Set2DIntegerVariables = .false.
   endif

   Return
   End Function Set2DIntegerVariables



   Logical Function Set2DDoubleVariables(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, ReturnVariable, &
                                         Dim1, Dim2, Index1, NrVars)

   implicit none
   
   ! set array of integer variables
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, Index1, NrVars, i
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx, Dim1, Dim2
   Double Precision  ::  ReturnVariable (Dim1,Dim2)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        Read (TokenArr%Token(ReturnIndx+i),*) ReturnVariable(Index1,i)
     enddo
     Set2DDoubleVariables = .true.
   else
     Set2DDoubleVariables = .false.
   endif

   Return
   End Function Set2DDoubleVariables



   Logical Function Set2DCharVariables(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, ReturnVariable, &
                                       Dim1, Dim2, Index1, NrVars)

   implicit none
   
   ! set array of integer variables
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, Index1, NrVars, i
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx, Dim1, Dim2
   Character(*)  :: ReturnVariable(Dim1,Dim2)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        ReturnVariable(Index1,i) = TokenArr%Token(ReturnIndx+i)
     enddo
     Set2DCharVariables = .true.
   else
     Set2DCharVariables = .false.
   endif

   Return
   End Function Set2DCharVariables


   Logical Function Set2DCharOrIntegerVariables(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, &
                                                ReturnVariableChar, ReturnVariableInt, Dim1, Dim2, Index1, NrVars)

   implicit none
   
   ! set array of integer variables
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, Index1, NrVars, i
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx, Dim1, Dim2
   Integer      ::  ReturnVariableInt (Dim1,Dim2)
   Character(*) ::  ReturnVariableChar(Dim1,Dim2)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     if ((TokenArr%IsEnclosedByQuotes(ReturnIndx+1))) then
        do i=1,NrVars
           ReturnVariableChar(Index1,i) = TokenArr%Token(ReturnIndx+i)
        enddo
     else
        do i=1,NrVars
           Read (TokenArr%Token(ReturnIndx+i),*) ReturnVariableInt(Index1,i)
        enddo
     endif
     Set2DCharOrIntegerVariables = .true.
   else
     Set2DCharOrIntegerVariables = .false.
   endif

   Return
   End Function Set2DCharOrIntegerVariables


   Logical Function Set3DRealVariablesRunningIndex2(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, &
                                                    ReturnVariable, Dim1, Dim2, Dim3, Index1, NrVars, Index3)

   ! 3D real array, index 1 and 3 are fixed
   ! set array of real variables
   implicit none
   
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, Index1, NrVars, Index3
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx, i, Dim1, Dim2, Dim3
   Real      :: ReturnVariable(Dim1,Dim2,Dim3)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        Read (TokenArr%Token(ReturnIndx+i),*) ReturnVariable(index1,i,index3)
     enddo
     Set3DRealVariablesRunningIndex2 = .true.
   else
     Set3DRealVariablesRunningIndex2 = .false.
   endif

   Return
   End Function Set3DRealVariablesRunningIndex2


   Logical Function Set3DDoubleVariablesRunningIndex2(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, &
                                                    ReturnVariable, Dim1, Dim2, Dim3, Index1, NrVars, Index3)

   ! 3D real array, index 1 and 3 are fixed
   ! set array of real variables
   implicit none
   
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, Index1, NrVars, Index3
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx, i, Dim1, Dim2, Dim3
   Double Precision  :: ReturnVariable(Dim1,Dim2,Dim3)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        Read (TokenArr%Token(ReturnIndx+i),*) ReturnVariable(index1,i,index3)
     enddo
     Set3DDoubleVariablesRunningIndex2 = .true.
   else
     Set3DDoubleVariablesRunningIndex2 = .false.
   endif

   Return
   End Function Set3DDoubleVariablesRunningIndex2


   Logical Function Set3DCharVariablesRunningIndex3(FindToken, Start, TokenArr, NrOfTokens, CaseSensitive, &
                                                    ReturnVariable, Dim1, Dim2, Dim3, Index1, Index2, NrVars)

   ! 3D real array, index 1 and 2 are fixed
   ! set array of real variables
   implicit none
   
   type (TokenArray)  TokenArr

   integer      Start, NrOfTokens, Index1, Index2, NrVars
   Character(*) FindToken
   Logical      CaseSensitive
   Integer      ReturnIndx, i, Dim1, Dim2, Dim3
   Character(*)  :: ReturnVariable(Dim1,Dim2, Dim3)

   if (GetKey(FindToken, Start, TokenArr,  NrOfTokens, ReturnIndx, CaseSensitive)) then
     do i=1,NrVars
        ReturnVariable(Index1,Index2,i) = TokenArr%Token(ReturnIndx+i)
     enddo
     Set3DCharVariablesRunningIndex3 = .true.
   else
     Set3DCharVariablesRunningIndex3 = .false.
   endif

   Return
   End Function Set3DCharVariablesRunningIndex3


   End Module ParseToken
