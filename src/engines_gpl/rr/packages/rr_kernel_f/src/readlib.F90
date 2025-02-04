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

! DOC
!
!  readlib.f90  Subroutines for flexible reading from file; = module version  of old Readlib.Lib
!
!  Copyright (C) 2003 Geert Prinsen WL|Deltares
!
!  General information:
!  This module contains a number of auxilary subroutines that can be
!  used to handle allocation and initialisation
!  Public functions include:
!  -
!  -
!
!
!
!
! ENDDOC
!
!  $Author$
!  $Date$
!  $Source$
!
!
    module Readlib
    
    use MessageHandling

    implicit none

    private ErrMsgLib
    private ErrMsgLib2
    private MyFndFrst
    private MyOwnLentrim
!   private MyOwnCntStr     !is used in NewTables, dus niet private

    contains

!--------------------------------------------------------------------
!         CHRTRIM.FOR : left-justify string, special.
!     removes leading characters from  string
!
!     input: STRING, CHAR1
!     output: STRING with leading characters CHAR1 removed
!--------------------------------------------------------------------

      SUBROUTINE CHRTRIM (STRING, CHAR1)
      CHARACTER(len=*)             :: STRING
      CHARACTER(len=1), intent(in) :: CHAR1

      CHARACTER(len=999) TEMP
      INTEGER I

      IF (STRING.EQ.' ') RETURN

      TEMP=STRING
      I=1
10    IF (TEMP(I:I) .EQ. CHAR1) THEN
        I=I+1
        GOTO 10
      ENDIF
      STRING=TEMP(I:)

      RETURN
      END Subroutine ChrTrim

! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 4               $


!--------------------------------------------------------------------
!     CNTSTR.FOR: Count nr. of occurences of string A in string B
!--------------------------------------------------------------------

      INTEGER FUNCTION CNTSTR(STRNGA, STRNGB)
      CHARACTER(*) , intent(in) ::  STRNGA, STRNGB

      INTEGER I, ICOUNT, KB, IPOS1, IPOS2
      ICOUNT = 0
      IF (STRNGB.EQ.' ') GOTO 999

      KB=Len_Trim(STRNGB)

      IPOS2 = 1
      DO I=1,KB
          IPOS1 = FNDFRST (STRNGA, STRNGB(IPOS2:),.false. )
          IF (IPOS1 .LT. 0) GOTO 999
          IPOS2 = IPOS2 + IPOS1
          ICOUNT  = ICOUNT+1
      ENDDO

  999 CONTINUE
      CNTSTR=ICOUNT

      RETURN
      END Function CntStr


!--------------------------------------------------------------------
!     CONSTR.FOR: Concatenate string elements from array to 1 string
!--------------------------------------------------------------------

      CHARACTER(len=999) FUNCTION CONSTR(STRING, N)

      INTEGER             , intent(in) :: N
      CHARACTER(Len=999)  , intent(in) :: STRING(N)

      INTEGER                INDEX, L, LR
      Character(Len=999)     RESULTString

      RESULTString = STRING(1)(1:Len_Trim(STRING(1)))
      DO INDEX=2,N
         L = Len_Trim(STRING(INDEX))
         LR= Len_Trim(RESULTString)
         IF (L .GT. 0) THEN
! omdat MS-Powerstation geheugen 'opeet' bij gebruik van concatenate via //, de volgende aanpassing:
!            RESULTString = RESULTString(1:LR) // STRING(INDEX)(1:L)
! ARS 7623: check op array bounds
             if (lr+l .gt. 999) then
                  Write(*,*) ' Error in concatenating strings'
!                 CALL ERRMSGLIB2 (802, 0, FILSUB, SEARCH, IOUT1, IflRtn)
             endif
             RESULTString(LR+1:LR+L) = STRING(INDEX)(1:L)
         ELSE
            GOTO 101
         ENDIF
      ENDDO
 101  CONTINUE

      LR= Len_Trim(RESULTString)
      CONSTR=RESULTString(1:LR)

      RETURN
      END Function ConStr


!--------------------------------------------------------------------
!     CONSTR2.FOR: Concatenate string elements from array to 1 string
!--------------------------------------------------------------------

      CHARACTER(len=999) FUNCTION CONSTR2(STRING, N, Iout1, IflRtn)

      INTEGER            , intent(in) :: N
      CHARACTER(Len=999) , intent(in) :: STRING(N)

      INTEGER               INDEX, L, LR, Iout1, IflRtn
      CHARACTER(len=999)    RESULTString

      RESULTString = STRING(1)(1:Len_Trim(STRING(1)))
      DO INDEX=2,N
         L = Len_Trim(STRING(INDEX))
         LR= Len_Trim(RESULTString)
         IF (L .GT. 0) THEN
! omdat MS-Powerstation geheugen 'opeet' bij gebruik van concatenate via //, de volgende aanpassing:
!            RESULTString = RESULTString(1:LR) // STRING(INDEX)(1:L)
! ARS 7623: check op array bounds
             if (lr+l .gt. 999) then
               call SetMessage(LEVEL_ERROR, 'Error in concatenating strings')
               CALL ERRMSGLIB2 (802, 0, ' ConStr2', ' ', IOUT1, IflRtn)
             endif
             RESULTString(LR+1:LR+L) = STRING(INDEX)(1:L)
         ELSE
            GOTO 101
         ENDIF
      ENDDO
 101  CONTINUE

      LR= Len_Trim(RESULTString)
      CONSTR2=RESULTString(1:LR)

      RETURN
      END Function ConStr2


!--------------------------------------------------------------------
!     CONSTR2Long.FOR: Concatenate string elements from array to 1 string
!--------------------------------------------------------------------

      CHARACTER(len=99999) FUNCTION CONSTR2Long(STRING, N, Iout1, IflRtn)

      INTEGER        , intent(in) :: N, Iout1, IflRtn
      CHARACTER(len=999), intent(in) :: STRING(N)

      INTEGER          INDEX, L, LR
      CHARACTER(len=99999) RESULTString

      RESULTString = STRING(1)(1:Len_Trim(STRING(1)))
      DO INDEX=2,N
         L = Len_Trim(STRING(INDEX))
         LR= Len_Trim(RESULTString)
         IF (L .GT. 0) THEN
! omdat MS-Powerstation geheugen 'opeet' bij gebruik van concatenate via //, de volgende aanpassing:
!            RESULTString = RESULTString(1:LR) // STRING(INDEX)(1:L)
! ARS 7623: check op array bounds
             if (lr+l .gt. 99999) then
                call SetMessage(LEVEL_ERROR, 'Error in concatenating strings')
                CALL ERRMSGLIB2 (802, 0, ' ConStr2', ' ', IOUT1, IflRtn)
             endif
             RESULTString(LR+1:LR+L) = STRING(INDEX)(1:L)
         ELSE
            GOTO 101
         ENDIF
      ENDDO
 101  CONTINUE

      LR= Len_Trim(RESULTString)
      CONSTR2Long=RESULTString(1:LR)

      RETURN
      END Function ConStr2Long


!--------------------------------------------------------------------
!     COUNTS.FOR: Determine number of items on a string
!--------------------------------------------------------------------

      INTEGER FUNCTION COUNTS(STRING)
      CHARACTER(len=*) STRING

      INTEGER I, ICOUNT, K2, K1

      ICOUNT = 0
      IF (STRING.EQ.' ') GOTO 999

! remove leading blanks
      CALL CHRTRIM (STRING,' ')

      K2=LEN(STRING)

      DO I=1,9999
          K1=Len_Trim (STRING(1:K2))
          IF (K1 .LE. 1) THEN
              K2=0
          ELSE
              K2=LENTR2 (STRING(1:K1-1))
          ENDIF
          ICOUNT  = ICOUNT+1
          IF (K2 .LE. 0) GOTO 999
      ENDDO

  999 CONTINUE
      COUNTS=ICOUNT

      RETURN
      END Function Counts


    Character(256) function deleteSpaces(string)

    Character(Len=*) string
    Integer teller, teller1

    do teller = 1, len(string)
      if (string(teller: teller) == ' ') then
        do teller1 = teller, len(string) - 1
          string(teller1: teller1) = string(teller1 + 1: teller1 + 1)
        end do
      end if
    end do

    deleteSpaces = string

    return
    end function deleteSpaces


      SUBROUTINE ERRMSGLIB (ICODE, IECODE, NAMSUB, STRING, IOUT1)

!C *********************************************************************
!C *** RTC module for use with SOBEK, Delft_3B
!C *** LAST UPDATE : June  1997          BY : Geert Prinsen
!C **********************************************************************
!C *** Write error message
!C **********************************************************************
!C ***  ICODE  = code foutboodschap
!C ***  IECODE = Fortran error code of knoop-id
!C ***  NAMSUB = naam subroutine
!C ***  STRING = string met verdere identificatie
!C ***  IOUT1  = message file
!C **********************************************************************


      INTEGER        , intent(in) :: IOUT1, IECODE, ICODE
      CHARACTER(len=*)  , intent(in) :: NAMSUB
      CHARACTER(len=*)  , intent(in) :: STRING

      CHARACTER(Len=999) STR(10), Messg

!C **********************************************************************
!C *** Write error message
!C **********************************************************************

      IF (ICODE .EQ. 912) THEN
         STR(1) = ' String '
         STR(2) = NAMSUB
         STR(3) = ' not found in file'
         STR(4) = STRING
         Messg = CONSTR (STR,4)
         call SetMessage(LEVEL_ERROR, MESSG)
      ELSEIF (ICODE .EQ. 928) THEN
         STR(1) = ' Search String '
         STR(2) = NAMSUB
         STR(3) = ' found twice in 1 record in file'
         STR(4) = STRING
         Messg = CONSTR (STR,4)
         call SetMessage(LEVEL_ERROR, MESSG)
      ELSEIF (ICODE .EQ. 801) THEN
         STR(1) = ' Error getting data for keyword '
         STR(2) = STRING
         STR(3) = ' in '
         STR(4) = NAMSUB
         Messg = CONSTR (STR,4)
         call SetMessage(LEVEL_ERROR, MESSG)
      ENDIF
!
!
      RETURN
      END Subroutine ErrMsgLib


      SUBROUTINE ERRMSGLIB2(ICODE, IECODE, NAMSUB, STRING, IOUT1, IFlRtn)

!C *********************************************************************
!C *** RTC module for use with SOBEK, Delft_3B
!C *** LAST UPDATE : June  1997          BY : Geert Prinsen
!C **********************************************************************
!C *** Write error message
!C **********************************************************************
!C ***  ICODE  = code foutboodschap
!C ***  IECODE = Fortran error code of knoop-id
!C ***  NAMSUB = naam subroutine
!C ***  STRING = string met verdere identificatie
!C ***  IOUT1  = message file
!C ***  IFlRtn = Return code file
!C **********************************************************************


      INTEGER        , intent(in) :: IOUT1, IECODE, ICODE, Iflrtn
      CHARACTER(len=*)  , intent(in) :: NAMSUB
      CHARACTER(len=*)  , intent(in) :: STRING

      CHARACTER(Len=999) STR(10), Messg

!C **********************************************************************
!C *** Write error message
!C **********************************************************************

      IF (ICODE .EQ. 912) THEN
         STR(1) = ' String '
         STR(2) = NAMSUB
         STR(3) = ' not found in file'
         STR(4) = STRING
         Messg = CONSTR (STR,4)
         call SetMessage(LEVEL_ERROR, MESSG)
         WRITE(IflRtn,*) Icode
      ELSEIF (ICODE .EQ. 928) THEN
         STR(1) = ' Search String '
         STR(2) = NAMSUB
         STR(3) = ' found twice in 1 record in file'
         STR(4) = STRING
         Messg = CONSTR (STR,4)
         call SetMessage(LEVEL_ERROR, MESSG)
         WRITE(IflRtn,*) Icode
      ELSEIF (ICODE .EQ. 801) THEN
         STR(1) = ' Error getting data for keyword '
         STR(2) = STRING
         STR(3) = ' in '
         STR(4) = NAMSUB
         Messg = CONSTR (STR,4)
         call SetMessage(LEVEL_ERROR, MESSG)
         WRITE(IflRtn,*) Icode
      ELSEIF (ICODE .EQ. 802) THEN
         STR(1) = ' Error concatenating strings.'
           STR(2) = ' Resulting string too long'
         Messg = CONSTR (STR,2)
         call SetMessage(LEVEL_ERROR, MESSG)
         WRITE(IflRtn,*) Icode
      ENDIF
!
      write(iflrtn,*) ' ErrMsgLib2'
!
      RETURN
      END Subroutine ErrMsgLib2


! Find string in string array, strings are any length

      INTEGER FUNCTION FindString (MaxStrings, StringArray, &
                                   XString   , NrStrings, CaseSensitive)

! *********************************************************************
! ***    This function is used for finding item X in an unsorted
! ***    character array by linear search.
! *********************************************************************
! *** Input/output parameters:
! *** ------------------------
! ***  NA           = dimension of character array A
! ***  StringArray  = array A with character strings
! ***  XString      = string/element to be searched
! ***  NrStrings    = actual number of elements in array A
! ***  CaseSensitive= true  : zoeken is case sensitive
! ***                 false : zoeken is niet case sensitive
! *********************************************************************
! *** Return value in FindString:
! ***     = -1  then value X not an element of the array A
! ***     > 0   then index in array A of element X
! *********************************************************************

      INTEGER           , intent(in) :: MaxStrings, NrStrings
      CHARACTER(Len=*)  , intent(in) :: XString,  StringArray(MaxStrings)
      Logical           , intent(in) :: CaseSensitive

      CHARACTER(len=9999) TempX, TempAr

      INTEGER        IPOS, INDX, LA, LB


      IPOS = -1
      LA = Len_Trim(XString)
      DO INDX=1,NrStrings
         LB = Len_Trim(StringArray(INDX))
         If (LA .EQ. LB) Then
           if (CaseSensitive) then
!            Zoeken is case-sensitive
             IPOS = FNDFRST(XString(1:LA), StringArray(INDX)(1:LB), .False.)
           else
!            Zoeken is niet case-sensitive, converteer alles eerst naar upper case
             TempX  = XString
             TempAr = StringArray(Indx)
             Call UpperC(TempX)
             Call UpperC(TempAr)
             IPOS = FNDFRST(TempX(1:LA), TempAr(1:LB), .False.)
           endif
         Endif
         IF (IPOS .GT. 0) GOTO 101
      ENDDO
! Not found
      INDX = -1
 101  CONTINUE

      FindString=INDX

      RETURN
      END Function FindString


!--------------------------------------------------------------------
!     FNDFRST.FOR: Find position of first occurrence of string A in string B
!--------------------------------------------------------------------

      INTEGER FUNCTION FNDFRST(STRNGA, STRNGB, WithSpaces)

      CHARACTER(len=*)  , intent(in) :: STRNGA, STRNGB
      Logical        , intent(in) :: WithSpaces

      INTEGER IPOS

      IPOS = -1
      IF (STRNGB.EQ.' ') GOTO 999

#if (defined(HAVE_CONFIG_H))
!        Unix version: use my own routine

         IPos = MyFndFrst(StrngA, StrngB, WithSpaces)
         FNDFRST=IPOS

#else
!        Pc version: use intrinsic routine Index to find substring STRNGA in string STRNGB
         Ipos = Index(STRNGB, STRNGA)
         If (Ipos .eq. 0) Ipos = -1
#endif


  999   CONTINUE
        FNDFRST=IPOS


      RETURN
      END Function FndFrst


!C--------------------------------------------------------------------
!C     GETDouble.FOR: Get real value from position IPOS from string
!C--------------------------------------------------------------------

      Double Precision FUNCTION GetDouble(IPOS, STRING, ReadError)

      CHARACTER(len=*), intent(in) :: STRING
      Logical                        ReadError
      CHARACTER(len=9999)            STRNGB

      Double Precision RVAL
      INTEGER IPOS

      ReadError = .false.
      STRNGB = STRING(IPOS:)
      READ (STRNGB,*,Err=801) RVAL

 999  CONTINUE
      GETDouble=RVAL

      Return

 801  ReadError = .true.

      RETURN
      END Function GetDouble


!C--------------------------------------------------------------------
!C     GETINT.FOR: Get integer value from position IPOS from string
!C--------------------------------------------------------------------

      INTEGER FUNCTION GETINT(IPOS, STRING, ReadError)

      CHARACTER(len=*), intent(in) :: STRING
      Logical                        ReadError
      CHARACTER(len=9999)            STRNGB

      INTEGER IPOS, IVAL

      ReadError = .false.
      STRNGB = STRING(IPOS:)
      READ (STRNGB,*,Err=801) IVAL
!c     WRITE(*,*) IVAL

 999  Continue
      GETINT=IVAL
      Return

 801  ReadError = .true.

      Return
      END Function GetInt


!--------------------------------------------------------------------
!     GETREAL.FOR: Get real value from position IPOS from string
!--------------------------------------------------------------------

      REAL FUNCTION GETREAL(IPOS, STRING, ReadError)

      CHARACTER(len=*), intent(in) :: STRING
      Logical                        ReadError
      CHARACTER(len=9999)            STRNGB

      REAL    RVAL
      INTEGER IPOS

      ReadError = .false.
!     WRITE(*,*) STRING, IPOS
      STRNGB = STRING(IPOS:)
      READ (STRNGB,*,Err=801) RVAL
!     WRITE(*,*) RVAL

 999  CONTINUE
      GETREAL=RVAL

      Return

 801  ReadError = .true.

      RETURN
      END Function GetReal


!--------------------------------------------------------------------
!     GETSTR.FOR: Get string from position IPOS to first space from string
!--------------------------------------------------------------------

      CHARACTER(len=999) FUNCTION GETSTR(IPOS, STRING, ReadError)

      CHARACTER(len=*), intent(in) :: STRING
      Logical                        ReadError
      CHARACTER(len=9999)            STRNGA, STRNGB

      INTEGER IPOS

      StrngA = ''
      ReadError = .false.
      STRNGB = STRING(IPOS:)

      READ (STRNGB,*,Err=801) STRNGA
!     WRITE(*,*) ' result from GETSTR=', STRNGA

!     Altijd conversie naar uppercase  (ARS 4988) toch niet ivm grote impact (eist dat overal in alle modules dit gebeurt)
!     Call UpperC(StrngA)

 999  CONTINUE
      GETSTR=STRNGA

      Return

 801  ReadError = .true.
      RETURN
      END Function GetStr


!C--------------------------------------------------------------------
!C    GETVAR.FOR: Get variable from STRING
!C--------------------------------------------------------------------
!C    STRING = input string
!C    SEARCH = search string
!C    ITYPE  = type of search; 1=string, 2=real, 3=integer
!C    NAMSUB = parameter used in call ERRMSG if error message occurs
!C    FILSUB = parameter used in call ERRMSG if error message occurs
!C    IOUT1  = unit nr. of output (message) file
!C    CHARRS = output character variable
!C    REALRS = output real value
!C    INTRS  = output integer value
!C    Allow  = logical input variable indicating whether search string is allowed to be missing
!C              (true=missing is allowed, false=missing causes fatal error)
!C    Found  = logical output variable indicating whether search was successfull
!C    RetVal = Return value, 0=ok
!C--------------------------------------------------------------------
!C    Use FNDFRST to find first position of search string
!C    Use GETSTR  to get a string
!C    Use GETREAL to get a real value
!C    Use GETINT  to get an integer value
!--------------------------------------------------------------------

      Function GETVAR (STRING, SEARCH, ITYPE, NAMSUB, FILSUB, IOUT1, &
                       CHARRS, REALRS, INTRS, ALLOW, FOUND)  Result(RetVal)

      Integer :: RetVal

      CHARACTER(len=*), intent(in) :: STRING, SEARCH, NAMSUB, FILSUB
      CHARACTER(len=*)            :: CHARRS
      INTEGER                     :: ITYPE, IOUT1, INTRS
      REAL                        :: REALRS
      LOGICAL                     :: ALLOW, FOUND

      LOGICAL                        WithSpaces, ReadError
      INTEGER                        IPOS, IPOS2, lengtezoekstring

      RetVal = 0
      FOUND = .FALSE.
      WithSpaces = .TRUE.
      ReadError  = .false.

      INTRS = 0
      REALRS = 0.
      CHARRS = ' '
      Lengtezoekstring = Lenstring(search)
!     write(*,*) ' lengtezoekstring = ', lengtezoekstring

!     WRITE(*,*) ' before call FNDFRST/GETSTR ', STRING(1:)
      IPOS = FNDFRST (SEARCH,STRING, WithSpaces)
!     WRITE(*,*) ' after FNDFRST, before GETSTR ', STRING(1:)
!     WRITE(*,*) ' Search string ', Search
!     WRITE(*,*) ' Search type (1=string,2=real,3=integer) ', ITYPE
!     WRITE(*,*) ' FndPosition ', IPOS,' Allow not found ', Allow
      IF (IPOS .GT. -1) THEN
         FOUND = .TRUE.
         IF (ITYPE .EQ. 1) THEN                            ! character variable
            CHARRS = GETSTR (IPOS+lengtezoekstring, STRING, ReadError)
!           WRITE(*,*) ' after call FNDFRST/GETSTR ', STRING(1:)
         ELSEIF (ITYPE .EQ. 2) THEN                          ! real variable
            REALRS = GETREAL (IPOS+lengtezoekstring, STRING, ReadError)
         ELSEIF (ITYPE .EQ. 3) THEN                          ! integer variable
            INTRS = GETINT (IPOS+lengtezoekstring, STRING, ReadError)
         ENDIF
         If (ReadError) then
            CALL ERRMSGLIB (801, 0, FILSUB, SEARCH, IOUT1)
            RetVal = 801
            Return
         Endif

!        March 99: check of zoekstring niet vaker voorkomt; geef dan een warning/foutmelding
         IPOS2 = FNDFRST (SEARCH,STRING(Ipos+lengtezoekstring:),WithSpaces)
         If (IPOS2 .GT. -1) then
           CALL ERRMSGLIB (928, 0, SEARCH, FILSUB, IOUT1)
           RetVal = 928
           Return
         Endif

      ENDIF

! search string not found, but not allowed to be missing
      IF (.NOT. FOUND .AND. .NOT. ALLOW) THEN
           CALL ERRMSGLIB (912, 0, SEARCH, FILSUB, IOUT1)
           RetVal = 912
      ENDIF

      RETURN
      END Function GetVar


!C--------------------------------------------------------------------
!C    GETVAR2.FOR: Get variable from STRING
!C--------------------------------------------------------------------
!C    STRING = input string
!C    SEARCH = search string
!C    ITYPE  = type of search; 1=string, 2=real, 3=integer
!C    NAMSUB = parameter used in call ERRMSG if error message occurs
!C    FILSUB = parameter used in call ERRMSG if error message occurs
!C    IOUT1  = unit nr. of output (message) file
!C    CHARRS = output character variable
!C    REALRS = output real value
!C    INTRS  = output integer value
!C    Allow  = logical input variable indicating whether search string is allowed to be missing
!C              (true=missing is allowed, false=missing causes fatal error)
!C    Found  = logical output variable indicating whether search was successfull
!C    IflRtn = unit nr of return code file
!C    RetVal = return value, 0=ok
!C--------------------------------------------------------------------
!C    Use FNDFRST to find first position of search string
!C    Use GETSTR  to get a string
!C    Use GETREAL to get a real value
!C    Use GETINT  to get an integer value
!C--------------------------------------------------------------------

      Function GETVAR2(STRING, SEARCH, ITYPE, NAMSUB, FILSUB, IOUT1, &
                       CHARRS, REALRS, INTRS, ALLOW, FOUND, IflRtn) result(RetVal)

      Integer :: RetVal
      CHARACTER(len=*), intent(in) :: StriNG, SEARCH, NAMSUB, FILSUB
      CHARACTER(len=*)            :: CHARRS
      INTEGER                     :: ITYPE, IOUT1, INTRS
      INTEGER                     :: IflRtn
      REAL                        :: REALRS
      LOGICAL                     :: ALLOW, FOUND

      LOGICAL                        WithSpaces, ReadError
      INTEGER                        IPOS, IPOS2, lengtezoekstring, lengtestring

      RetVal = 0
      FOUND = .FALSE.
      WithSpaces = .TRUE.
      ReadError  = .false.

      INTRS = 0
      REALRS = 0.
      CHARRS = ' '
      Lengtezoekstring = Lenstring(search)
      Lengtestring = Len_Trim (string)
!     write(*,*) ' lengtezoekstring = ', lengtezoekstring

!     WRITE(*,*) ' before call FNDFRST/GETSTR ', STRING(1:)
      IPOS = FNDFRST (SEARCH,STRING, WithSpaces)
!     WRITE(*,*) ' after FNDFRST, before GETSTR ', STRING(1:)
!     WRITE(*,*) ' Search string ', Search
!     WRITE(*,*) ' Search type (1=string,2=real,3=integer) ', ITYPE
!     WRITE(*,*) ' FndPosition ', IPOS,' Allow not found ', Allow
      IF (IPOS .GT. -1) THEN
         FOUND = .TRUE.
         IF (ITYPE .EQ. 1) THEN                            ! character variable
            CHARRS = GETSTR (IPOS+lengtezoekstring, STRING, ReadError)
!           WRITE(*,*) ' after call FNDFRST/GETSTR ', STRING(1:)
         ELSEIF (ITYPE .EQ. 2) THEN                          ! real variable
            REALRS = GETREAL (IPOS+lengtezoekstring, STRING, ReadError)
         ELSEIF (ITYPE .EQ. 3) THEN                          ! integer variable
            INTRS = GETINT (IPOS+lengtezoekstring, STRING, ReadError)
         ENDIF
         If (ReadError .and. .not. allow) Then
           call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
              CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
              RetVal = 801
              Return
         Endif
!        March 99: check of zoekstring niet vaker voorkomt; geef dan een warning/foutmelding
         IPOS2 = FNDFRST (SEARCH,STRING(Ipos+lengtezoekstring:),WithSpaces)
         If (IPOS2 .GT. -1) then
           call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
           CALL ERRMSGLIB2(928, 0, SEARCH, FILSUB, IOUT1, Iflrtn)
           RetVal = 928
           Return
         Endif

      ENDIF

! search string not found, but not allowed to be missing
      IF (.NOT. FOUND .AND. .NOT. ALLOW) THEN
           call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
           CALL ERRMSGLIB2(912, 0, SEARCH, FILSUB, IOUT1, Iflrtn)
           RetVal = 912
      ENDIF


      RETURN
      END Function GetVar2


!--------------------------------------------------------------------
!    GETVAR3.FOR: Get variable from STRING
!--------------------------------------------------------------------
!    1998 Creation of library routine
!    First version from Sobek-RR/RTC
!
!    April 2000
!    2 = second version, with error message and code in return code file
!
!    September 2000
!    3 = third version, including also double precision variables
!--------------------------------------------------------------------
!--------------------------------------------------------------------
! Variables in argument list:
!    STRING = input string
!    SEARCH = search string
!    ITYPE  = type of search; 1=string, 2=real, 3=integer, 4=double precision
!    NAMSUB = parameter used in call ERRMSG if error message occurs
!    FILSUB = parameter used in call ERRMSG if error message occurs
!    IOUT1  = unit nr. of output (message) file
!    CHARRS = output character variable
!    REALRS = output real value
!    INTRS  = output integer value
!    Allow  = logical input variable indicating whether search string is allowed to be missing
!              (true=missing is allowed, false=missing causes fatal error)
!    Found  = logical output variable indicating whether search was successfull
!    IflRtn = unit nr of return code file
!    RetVal = return value, 0=ok
!-------------------------------------------------------------------
!    Use FNDFRST to find first position of search string
!    Use GETSTR  to get a string
!    Use GETREAL to get a real value
!    Use GETDouble to get a double precision value
!    Use GETINT  to get an integer value
!--------------------------------------------------------------------

      Function GETVAR3(STRING, SEARCH, ITYPE, NAMSUB, FILSUB, IOUT1, &
                       CHARRS, REALRS, INTRS, DoublRs, ALLOW, FOUND, IflRtn)  result(RetVal)

      integer :: RetVal

      CHARACTER(len=*), intent(in) :: StriNG, SEARCH, NAMSUB, FILSUB
      CHARACTER(len=*)            :: CHARRS
      INTEGER                     :: ITYPE, IOUT1, INTRS
      INTEGER                     :: IflRtn
      REAL                        :: REALRS
      Double Precision            :: DoublRs
      LOGICAL                     :: ALLOW, FOUND

      LOGICAL                        WithSpaces, ReadError
      INTEGER                        IPOS, IPOS2, lengtezoekstring, lengtestring
!     CHARACTER*999 GETSTR

      RetVal = 0
      FOUND = .FALSE.
      WithSpaces = .TRUE.
      ReadError  = .false.

      INTRS = 0
      REALRS = 0.
      CHARRS = ' '
      Lengtezoekstring = Lenstring(search)
      Lengtestring = Len_Trim (string)

      IPOS = FNDFRST (SEARCH,STRING, WithSpaces)
      IF (IPOS .GT. -1) THEN
         FOUND = .TRUE.
         IF (ITYPE .EQ. 1) THEN                            ! character variable
            CHARRS = GETSTR (IPOS+lengtezoekstring, STRING, ReadError)
         ELSEIF (ITYPE .EQ. 2) THEN                          ! real variable
            REALRS = GETREAL (IPOS+lengtezoekstring, STRING, ReadError)
         ELSEIF (ITYPE .EQ. 3) THEN                          ! integer variable
            INTRS = GETINT (IPOS+lengtezoekstring, STRING, ReadError)
         ELSEIF (ITYPE .EQ. 4) THEN                          ! double precision variable
            DoublRS = GetDouble (IPOS+lengtezoekstring,STRING,ReadError)
         ENDIF
         If (ReadError .and. .not. allow) Then
              call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
              CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
              RetVal = 801
              Return
         Endif
!        March 99: check of zoekstring niet vaker voorkomt; geef dan een warning/foutmelding
         IPOS2 = FNDFRST (SEARCH,STRING(Ipos+lengtezoekstring:),WithSpaces)
         If (IPOS2 .GT. -1) then
           call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
           CALL ERRMSGLIB2(928, 0, SEARCH, FILSUB, IOUT1, Iflrtn)
           RetVal = 928
           Return
         Endif

      ENDIF

! search string not found, but not allowed to be missing
      IF (.NOT. FOUND .AND. .NOT. ALLOW) THEN
           call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
           CALL ERRMSGLIB2(912, 0, SEARCH, FILSUB, IOUT1, Iflrtn)
           RetVal = 928
      ENDIF


      RETURN
      END Function GetVar3


!C--------------------------------------------------------------------
!C     GETVRS.FOR: Get variables from STRING
!C--------------------------------------------------------------------
!C     STRING = input string
!C     SEARCH = search string
!C     ITYPE  = type of output variables: 1= char, 2=real, 3=integer
!C              so, all variables read are of the same type
!C     NAMSUB = parameter used in call ERRMSG if error message occurs
!C     FILSUB = parameter used in call ERRMSG if error message occurs
!C     IOUT1  = unit nr. of output (message) file
!C     CHARRS = output variables of type char
!C     REALRS = output variables of type real
!C     INTRS  = output variables of type integer
!C     NVAL   = number of values to read
!C     RetVal = return value, 0=ok
!C--------------------------------------------------------------------
!C     compare with sub GETVAR for getting one variable only
!C--------------------------------------------------------------------

      Function GETVRS (STRING, SEARCH, ITYPE, NAMSUB, FILSUB, IOUT1, &
                       CHARRS, REALRS, INTRS, NVAL)  result(RetVal)

      Integer :: RetVal

      CHARACTER(len=*), intent(in) :: String, SEARCH, NAMSUB, FILSUB
      INTEGER                     :: NVAL
      CHARACTER(len=*)            :: CHARRS(NVAL)
      INTEGER                     :: ITYPE, IOUT1, INTRS(NVAL)
      REAL                        :: REALRS(NVAL)

      INTEGER                        I, IPOS, IPOS2, Lengtezoekstring
      Logical                        WithSpaces, ReadError

!      WRITE(*,*) ' GETVRS Search string ', Search
!      WRITE(*,*) ' Search type (1=string,2=real,3=integer) ', ITYPE

      RetVal = 0
      Lengtezoekstring = Lenstring(search)
      WithSpaces = .true.
      ReadError  = .false.

      IF (ITYPE .EQ. 1) THEN
! character variable
         IPOS = FNDFRST (SEARCH, STRING, WithSpaces)
         IF (IPOS .GT. -1) THEN
           IPOS = IPOS+Lengtezoekstring
!          WRITE(*,*) STRING(1:)
           DO I=1,NVAL
!             WRITE(*,*) IPOS, STRING(IPOS:)
              CHARRS(I) = GETSTR (IPOS, STRING, ReadError)
              If (ReadError)  then
                 CALL ERRMSGLIB (801, 0, FILSUB, SEARCH, IOUT1)
                 RetVal = 801
                 Return
              Endif
!             WRITE(*,*) ' found ', CHARRS(I)
!             aanname : 'string1' 'string2' etc, dus slechts met 1 spatie ertussen!!!!
              IPOS = IPOS + Len_Trim(CHARRS(I)) + 3
           ENDDO
         ELSE
           CALL ERRMSGLIB (912, 0, SEARCH, FILSUB, IOUT1)
           RetVal = 912
           Return
         ENDIF
      ELSEIF (ITYPE .EQ. 2) THEN
! real
         IPOS = FNDFRST (SEARCH,STRING, WithSpaces)
         IF (IPOS .GT. -1) THEN
           CALL GTREAL (IPOS+Lengtezoekstring,STRING, REALRS, NVAL, ReadError)
           If (ReadError) then
              CALL ERRMSGLIB (801, 0, FILSUB, SEARCH, IOUT1)
              RetVal = 801
              Return
           Endif
         ELSE
            CALL ERRMSGLIB (912, 0, SEARCH, FILSUB, IOUT1)
            RetVal = 912
            Return
         ENDIF
      ELSEIF (ITYPE .EQ. 3) THEN
! integer
         IPOS = FNDFRST (SEARCH,STRING, WithSpaces)
         IF (IPOS .GT. -1) THEN
           CALL GTINTS (IPOS+Lengtezoekstring,STRING, INTRS, NVAL, ReadError)
           If (ReadError) then
              CALL ERRMSGLIB (801, 0, FILSUB, SEARCH, IOUT1)
              RetVal = 801
              Return
           Endif
         ELSE
           CALL ERRMSGLIB (912, 0, SEARCH, FILSUB, IOUT1)
           RetVal = 912
           Return
         ENDIF
      ENDIF

!     March 99: check of zoekstring niet vaker voorkomt; geef dan een warning/foutmelding
      IPOS2 = FNDFRST (SEARCH,STRING(Ipos+Lengtezoekstring:),WithSpaces)
      If (IPOS2 .GT. -1) then
         CALL ERRMSGLIB (928, 0, SEARCH, FILSUB, IOUT1)
         RetVal = 928
      Endif

      RETURN
      END Function GetVrs


!C--------------------------------------------------------------------
!C     GETVRS.FOR: Get variables from STRING
!C--------------------------------------------------------------------
!C     STRING = input string
!C     SEARCH = search string
!C     ITYPE  = type of output variables: 1= char, 2=real, 3=integer
!C              so, all variables read are of the same type
!C     NAMSUB = parameter used in call ERRMSG if error message occurs
!C     FILSUB = parameter used in call ERRMSG if error message occurs
!C     IOUT1  = unit nr. of output (message) file
!C     CHARRS = output variables of type char
!C     REALRS = output variables of type real
!C     INTRS  = output variables of type integer
!C     NVAL   = number of values to read
!C    IflRtn = unit nr of return code file
!C    RetVal = return value, 0=ok
!C--------------------------------------------------------------------
!C     compare with sub GETVAR2 for getting one variable only
!C--------------------------------------------------------------------

      Function GETVRS2(STRING, SEARCH, ITYPE, NAMSUB, FILSUB, IOUT1, &
                         CHARRS, REALRS, INTRS, NVAL, Iflrtn)  result(RetVal)

      Integer :: RetVal

      CHARACTER(len=*), intent(in) ::  STRING, SEARCH, NAMSUB, FILSUB
      INTEGER                     ::  NVAL, Iflrtn
      CHARACTER(len=*)            ::  CHARRS(NVAL)
      INTEGER                     ::  ITYPE, IOUT1, INTRS(NVAL)
      REAL                        ::  REALRS(NVAL)

      INTEGER       I, IPOS, IPOS2, Lengtezoekstring, lengtestring
      Logical       WithSpaces, ReadError

!      WRITE(*,*) ' GETVRS Search string ', Search
!      WRITE(*,*) ' Search type (1=string,2=real,3=integer) ', ITYPE

      RetVal = 0
      Lengtezoekstring = Lenstring(search)
      Lengtestring = Len_Trim(string)
      WithSpaces = .true.
      ReadError  = .false.

      IF (ITYPE .EQ. 1) THEN
! character variable
         IPOS = FNDFRST (SEARCH, STRING, WithSpaces)
         IF (IPOS .GT. -1) THEN
           IPOS = IPOS+Lengtezoekstring
!          WRITE(*,*) STRING(1:)
           DO I=1,NVAL
!             WRITE(*,*) IPOS, STRING(IPOS:)
              CHARRS(I) = GETSTR (IPOS, STRING, ReadError)
              If (ReadError) Then
                 call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
                 CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
                 RetVal = 801
                 Return
              Endif
!             WRITE(*,*) ' found ', CHARRS(I)
!             zet Ipos onder de aanname dat string wordt afgsloten met ', dan 1 spatie, dan volgende string begint met '
              IPOS = IPOS + Len_Trim(CHARRS(I)) + 3
           ENDDO
         ELSE
           call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
           CALL ERRMSGLIB2(912, 0, SEARCH, FILSUB, IOUT1, IflRtn)
           RetVal = 912
           Return
         ENDIF
      ELSEIF (ITYPE .EQ. 2) THEN
! real
         IPOS = FNDFRST (SEARCH,STRING, WithSpaces)
         IF (IPOS .GT. -1) THEN
           CALL GTREAL (IPOS+Lengtezoekstring,STRING, REALRS, NVAL, ReadError)
           If (ReadError) Then
              call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
              CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
              RetVal = 801
              Return
           Endif
         ELSE
           call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
           CALL ERRMSGLIB2(912, 0, SEARCH, FILSUB, IOUT1, IflRtn)
           RetVal = 912
           Return
         ENDIF
      ELSEIF (ITYPE .EQ. 3) THEN
! integer
         IPOS = FNDFRST (SEARCH,STRING, WithSpaces)
         IF (IPOS .GT. -1) THEN
           CALL GTINTS (IPOS+Lengtezoekstring, STRING, INTRS, NVAL, ReadError)
           If (ReadError) Then
              call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
              CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
              RetVal = 801
              Return
           Endif
         ELSE
           call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
           CALL ERRMSGLIB2(912, 0, SEARCH, FILSUB, IOUT1, IflRtn)
           RetVal = 912
           Return
         ENDIF
      ENDIF

!     March 99: check of zoekstring niet vaker voorkomt; geef dan een warning/foutmelding
      IPOS2 = FNDFRST (SEARCH,STRING(Ipos+Lengtezoekstring:),WithSpaces)
      If (IPOS2 .GT. -1) then
         call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
         CALL ERRMSGLIB2(928, 0, SEARCH, FILSUB, IOUT1, IflRtn)
         RetVal = 928
      Endif

      RETURN
      END Function GetVrs2


!C--------------------------------------------------------------------
!C     GETVRS3.FOR: Get variables from STRING
!C--------------------------------------------------------------------
!C  April 2000: 2. add error message in file / return code
!C  Sept  2000: 3. add reading double precision variables
!C--------------------------------------------------------------------
!C     STRING = input string
!C     SEARCH = search string
!C     ITYPE  = type of output variables: 1= char, 2=real, 3=integer
!C              so, all variables read are of the same type
!C     NAMSUB = parameter used in call ERRMSG if error message occurs
!C     FILSUB = parameter used in call ERRMSG if error message occurs
!C     IOUT1  = unit nr. of output (message) file
!C     CHARRS = output variables of type char
!C     REALRS = output variables of type real
!C     INTRS  = output variables of type integer
!C     DoublRS = output variables of type double precision
!C     NVAL   = number of values to read
!      Allow  = logical input variable indicating whether search string is allowed to be missing
!                (true=missing is allowed, false=missing causes fatal error)
!      Found  = logical output variable indicating whether search was successfull
!C     IflRtn = unit nr of return code file
!C     RetVal = return value
!C--------------------------------------------------------------------
!C     compare with sub GETVAR3 for getting one variable only
!C March 2001
!C    Additional possibility: call with itype=0
!C    GetVrs3 will determine itself what itype should be, character or not.
!C--------------------------------------------------------------------

      Function GETVRS3(STRING, SEARCH, ITYPE, NAMSUB, FILSUB, IOUT1, &
                       CHARRS, REALRS, INTRS, DoublRs, NVAL, allow, found, Iflrtn) result(RetVal)

      Integer :: RetVal

      CHARACTER(len=*), intent(in) :: String, SEARCH, NAMSUB, FILSUB
      INTEGER                     :: NVAL, Iflrtn
      CHARACTER(len=*)            :: CHARRS(NVAL)
      INTEGER                     :: ITYPE, IOUT1, INTRS(NVAL)
      REAL                        :: REALRS(NVAL)
      Double Precision            :: DoublRS(NVAL)
      LOGICAL                     :: ALLOW, FOUND

      INTEGER       I, IPOS, IPOS2, Lengtezoekstring, lengtestring
      CHARACTER(len=1) Quote, DblQuote
      Logical       WithSpaces, ReadError

      FOUND = .FALSE.
      Quote = ''''
      DblQuote = """"
!     Write(*,*) ' quote    =', quote
!     Write(*,*) ' Dblquote =', dblquote
!     WRITE(*,*) ' GETVRS Search string ', Search
!     WRITE(*,*) ' Search type (1=string,2=real,3=integer) ', ITYPE

      RetVal = 0
      Lengtezoekstring = Lenstring(search)
      Lengtestring = Len_Trim(string)
      WithSpaces = .true.
      ReadError  = .false.

      IPOS = FNDFRST (SEARCH, STRING, WithSpaces)
!     Write(*,*) ' Getvrs3 search string ', Search
!     Write(*,*) ' Getvrs3 in string ', String
!     Write(*,*) ' Getvrs3 found string on position ', ipos

      IF (IPOS .GT. -1) THEN
        found = .true.
! character variable
        IF (ITYPE .EQ. 1) THEN
           IPOS = IPOS+Lengtezoekstring
           DO I=1,NVAL
              CHARRS(I) = GETSTR (IPOS, STRING, ReadError)
              If (ReadError) Then
                 call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
                 CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
                 RetVal = 801
                 Return
              Endif
!             zet ipos onder de aanname dat strings gescheiden worden door ' met 1 spatie ertussen
              IPOS = IPOS + Len_Trim(CHARRS(I)) + 3
           ENDDO
        ELSEIF (ITYPE .EQ. 2) THEN
! real
           CALL GTREAL (IPOS+Lengtezoekstring,STRING, REALRS, NVAL, ReadError)
           If (ReadError) Then
              call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
              CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
              RetVal = 801
              Return
           Endif
        ELSEIF (ITYPE .EQ. 3) THEN
! integer
           CALL GTINTS (IPOS+Lengtezoekstring,STRING, INTRS, NVAL, ReadError)
           If (ReadError) Then
              call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
              CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
              RetVal = 801
              Return
           Endif
        ELSEIF (ITYPE .EQ. 4) THEN
! Double precision
           CALL GTDoubles (IPOS+Lengtezoekstring,STRING, DoublRS, NVAL, ReadError)
           If (ReadError) Then
              call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
              CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
              RetVal = 801
              Return
           Endif
        ELSEIF (ITYPE .EQ. 0) THEN
! GetVrs3 should determine itself what type of variable it is
!          Write(*,*) ' GetVRS3 itype=0'
           IPos = IPos + Lengtezoekstring
  101      Continue
!          Write(*,*) ' STRING = ',String(Ipos:)
           If (String(Ipos:IPos) .eq. ' ') then
!             on ipos is a blank, so try next position
              IPos = IPos + 1
              goto 101
           ElseIf (String(Ipos:IPos) .ne. ' ' .and. &
                    String(Ipos:IPos) .ne. quote .and. &
                     String(Ipos:IPos) .ne. dblquote) then
!              on ipos is not a blank or quote, should try integer
!             Write(*,*) ' not blank or quote ',String(Ipos:ipos)
              itype = 3
              CALL GTINTS (IPOS, STRING, INTRS, NVAL, ReadError)
              If (ReadError) Then
                  call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
                  CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
                  RetVal = 801
                  Return
              Endif
           Else
!             Write(*,*) ' character ',String(Ipos:ipos)
!              quote or doublequote: character
               itype = 1
               DO I=1,NVAL
               CHARRS(I) = GETSTR (IPOS, STRING, ReadError)
               If (ReadError) Then
                  call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
                  CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
                  RetVal = 801
                  Return
               Endif
               IPOS = IPOS + Len_Trim(CHARRS(I)) + 3
             ENDDO
           Endif
        ELSE
!          Unknown type ITYPE
           call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring))//' with unknown type')
           CALL ERRMSGLIB2(801, 0, FILSUB, SEARCH, IOUT1, IflRtn)
           RetVal = 801
           Return
        ENDIF
      ELSE
! search string not found
         IF (.NOT. FOUND .AND. .NOT. ALLOW) THEN
            ! search string not found, while it should be found (not allowed to be missing)
            call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
            CALL ERRMSGLIB2(912, 0, SEARCH, FILSUB, IOUT1, IflRtn)
            RetVal = 801
            Return
         ENDIF
      ENDIF

!     March 99: check of zoekstring niet vaker voorkomt; geef dan een warning/foutmelding
      IPOS2 = FNDFRST (SEARCH,STRING(Ipos+Lengtezoekstring:),WithSpaces)
      If (IPOS2 .GT. -1) then
         call SetMessage(LEVEL_ERROR, 'Error at reading from string '//trim(String(1:lengtestring)))
         CALL ERRMSGLIB2(928, 0, SEARCH, FILSUB, IOUT1, IflRtn)
         RetVal = 801
      Endif

      RETURN
      END Function GetVrs3


!--------------------------------------------------------------------
!  GTDoubles.FOR: Get double precision values from position IPOS from string STRING
!              Total NVAL values
!              Output in real array DoublRS (Double precision results)
!--------------------------------------------------------------------

      SUBROUTINE GTDoubles (IPOS, STRING, DoublRS, NVAL, ReadError)
      CHARACTER(len=*), intent(in) :: String
      Logical                        ReadError

      INTEGER                     :: NVAL
      Double Precision            :: DoublRS(NVAL)

      CHARACTER(len=9999)            STRNGB
      INTEGER                        IPOS, I

      ReadError = .false.
      STRNGB = STRING(IPOS:)
      READ (STRNGB,*,Err=801) (DoublRS(I),I=1,NVAL)
      Return

 801  ReadError = .true.
      RETURN
      END Subroutine GtDoubles


!C--------------------------------------------------------------------
!C  GTINTS.FOR: Get integer values from position IPOS from string STRING
!C              Total NVAL values
!C              Output in integer array INTRS (integer results)
!C--------------------------------------------------------------------

      SUBROUTINE GTINTS(IPOS, STRING, INTRS, NVAL, ReadError)
      CHARACTER(len=*), intent(in) :: STRING
      INTEGER                     :: NVAL
      INTEGER                     :: INTRS(NVAL)

      INTEGER        IPOS, I
      CHARACTER(len=9999) STRNGB
      LOGICAL        ReadError

      ReadError = .false.
      STRNGB = STRING(IPOS:)
      READ (STRNGB,*,Err=801) (INTRS(I),I=1,NVAL)
!     WRITE(*,*) ' Gtints',(INTRS(I),I=1,NVAL)
      Return

 801  ReadError = .true.
      RETURN
      END Subroutine GtInts


!C--------------------------------------------------------------------
!C  GTREAL.FOR: Get real values from position IPOS from string STRING
!C              Total NVAL values
!C              Output in real array REALRS (real results)
!C--------------------------------------------------------------------

      SUBROUTINE GTREAL(IPOS, STRING, REALRS, NVAL, ReadError)
      CHARACTER(len=*), intent(in) :: STRING
      INTEGER                    :: NVAL
      REAL                       :: REALRS(NVAL)
      Logical                    :: ReadError

      CHARACTER(len=9999) STRNGB
      INTEGER        IPOS, I

      ReadError = .false.
      STRNGB = STRING(IPOS:)
!c      WRITE(*,'(A,A1500)') ' Gtreal; read from string:',Strngb(1:)
!c      Write(*,*) ' read ', NVal,' values'
      READ (STRNGB,*,Err=801) (REALRS(I),I=1,NVAL)
!C     WRITE(*,*) ' Gtreal',(REALRS(I),I=1,NVAL)
      Return

 801  ReadError = .true.
      RETURN
      END Subroutine GtReal
! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 3               $


    CHARACTER(2) FUNCTION INTCH2(I)

      INTEGER     I
      CHARACTER(3) A    !, INTCHR

      A = INTCHR (I)
      INTCH2 = A(2:3)

      RETURN
    END function IntCh2


    CHARACTER(4) FUNCTION INTCH4(I)

      INTEGER     I
      CHARACTER(4) A

      if (i .le. 9999 .and. i .gt. -999) then
         WRITE (A,'(I4.4)') I
      else
         A = '****'
      endif
      READ  (A,'(A)') INTCH4

      RETURN
    END function IntCh4
! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 3               $


    CHARACTER(8) FUNCTION INTCH8(I)

      INTEGER     I
      CHARACTER(8) A

      if (i .le. 99999999 .and. i .gt. -9999999) then
         WRITE (A,'(I8.8)') I
      else
         A = '****'
      endif

      READ  (A,'(A)') INTCH8

      RETURN
        END function IntCH8
! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 3               $


    CHARACTER(3) FUNCTION INTCHR(I)

      INTEGER     I
      CHARACTER(3) A

      if (i .le. 999 .and. i .gt. -99) then
         WRITE (A,'(I3.3)') I
      else
         A = '***'
      endif

      READ  (A,'(A)') INTCHR

      RETURN
    END function IntChr


!C--------------------------------------------------------------------
!C     LENSTRING.FOR: Determine length of string, including trailing blanks
!C--------------------------------------------------------------------

      INTEGER FUNCTION LENSTRING(STRING)
      CHARACTER(len=*), intent(in) :: STRING

      INTEGER I

      LENSTRING = 1
      IF (STRING.EQ.' ') RETURN

      I=LEN(STRING)
      LENSTRING = I

      RETURN
      END Function LenString


!C--------------------------------------------------------------------
!C     LENTR1.FOR: Determine length of string, less trailing blanks
!C                  Minimum length=1 instead of 0 as in Len_Trim
!C--------------------------------------------------------------------

      INTEGER FUNCTION LENTR1(STRING)
      CHARACTER(len=*), intent(in) :: STRING

      INTEGER I

      LENTR1 = 1
      IF (STRING.EQ.' ') RETURN

      I=LEN(STRING)

10    IF (STRING(I:I).EQ.' ') THEN
        I=I-1
        GOTO 10
      ENDIF

      LENTR1 = MAX (1,I)

      RETURN
      END Function Lentr1


!C--------------------------------------------------------------------
!C     LENTR2.FOR: Determine first blank or TAB (of group of blanks/Tabs)
!C                 from backside of string
!C--------------------------------------------------------------------

      INTEGER FUNCTION LENTR2(STRING)
      CHARACTER(len=*), intent(in) :: STRING

      INTEGER I

      LENTR2 = 0
      IF (STRING.EQ.' ') RETURN

      I=LEN(STRING)

!C terug tot eerste blank/tab

10    IF (STRING(I:I).NE.' ' .AND. STRING(I:I).NE.'     ') THEN
        I=I-1
        IF (I .GT. 0)  GOTO 10
      ENDIF

!C nog verder terug naar de eerste blank/tab van deze groep

      IF (I .GT. 0) THEN
20      IF (STRING(I:I).EQ.' ' .OR. STRING(I:I).EQ.'    ') THEN
           I=I-1
           IF (I .GT. 0)  GOTO 20
        ENDIF
      ENDIF

      LENTR2 = I

      RETURN
      END Function Lentr2


!C--------------------------------------------------------------------
!C     Len_Trim.FOR: Determine length of string, less trailing blanks
!C
!C--------------------------------------------------------------------

!      INTEGER FUNCTION LENTRIM(STRING)
!      CHARACTER*(*)  , intent(in) :: STRING
!
!
!#if (defined(HAVE_CONFIG_H))
!!         Unix version: my own routine
!         LENTRIM = MyOwnLentrim(String)
!#else
!!         Pc version: use intrinsic routine Len_trim
!         LenTrim = Len_trim(String)
!#endif
!
!      RETURN
!      END Function Lentrim


!C--------------------------------------------------------------------
!C     LOWERC.FOR : convert string to lowercase characters
!C--------------------------------------------------------------------

      SUBROUTINE LOWERC(STRING)

      CHARACTER(len=*) STRING
      INTEGER L

      L=LEN(STRING)

! Use Small routine from Delft_3D; platform independant routine, no hard-coded ASCII codes
      Call Small (String, L)

      RETURN
      END Subroutine LowerC


!C--------------------------------------------------------------------
!C           LTRIM.FOR : left-justify string
!C
!C     removes leading spaces from  string
!C
!C     POPLIB library function, B.L.Adriaanse, Deltares 1989
!C--------------------------------------------------------------------

      SUBROUTINE LTRIM(STRING)
      CHARACTER(len=*)  :: STRING

      IF (STRING.EQ.' ') GOTO 99

#if (defined(HAVE_CONFIG_H))
!C        Unix version: use my own routine

!C jan 2000: aanpassing initialisatie, testen op lengte string
         TEMP=' '
         I = Len_Trim(STRING)
         IF (I .GT. 0) TEMP=STRING(1:I)

         I=1
10       IF (TEMP(I:I).EQ.' ') THEN
           I=I+1
           GOTO 10
         ENDIF
         STRING=TEMP(I:)
#else
!C         Pc version: use intrinsic routine AdjustL to left justfify string; extra blanks at the end are insered
          String = AdjustL(String)
#endif
99    CONTINUE

      RETURN
      END Subroutine LTrim


!C--------------------------------------------------------------------
!C     MyFNDFRST.FOR: Find position of first occurrence of string A in string B
!C--------------------------------------------------------------------

      INTEGER FUNCTION MyFNDFRST(STRNGA, STRNGB, WithSpaces)
      CHARACTER(len=*), intent(in) :: STRNGA, STRNGB

      INTEGER I, IPOS, LA, LB
      Logical WithSpaces

      IPOS = -1
      IF (STRNGB.EQ.' ') GOTO 999

!C        my own routine

!C zoekstring STRNGA in- of ex-clusief spaties aan het eind
!C in string STRNGB kan altijd exclusief spaties aan het eind; na de
        IF (WithSpaces) Then
           LA=LENSTRING(STRNGA)
        Else
           LA=Len_Trim(STRNGA)
        Endif
        LB=Len_Trim(STRNGB)
!c       WRITE(*,'(A)')' Fndfrst'
!c       WRITE(*,'(A,1X,A)') ' Zoek string', STRNGA(1:LA), LA
!c       WRITE(*,'(A,1X,A)') ' in string', STRNGB(1:LB), LB

        DO I=1,LB
            IF (I+LA-1 .GT. LB) GOTO 998
            IF (STRNGB(I:I+LA-1) .EQ. STRNGA) THEN
                IPOS = I
                GOTO 998
            ENDIF
        ENDDO

  998   CONTINUE
!c       WRITE(*,*) ' Fndfrst', IPOS
        MyFNDFRST=IPOS


  999   CONTINUE
        MyFNDFRST=IPOS


      RETURN
      END Function MyFndFrst


! Last changed
! by:               $Author:: Schrier           $
! at:               $Modtime:: 22-07-97 11:53a  $
!
! current revision: $Revision:: 4               $


!--------------------------------------------------------------------
!     CNTSTR.FOR: Count nr. of occurences of string A in string B
!     MyOwnCNTSTR.FOR  = copy of CntStr, but always using MyOwnLentrim instead of Lentrim
!--------------------------------------------------------------------

      INTEGER FUNCTION MyOwnCNTSTR(STRNGA, STRNGB)
      CHARACTER(*)  , intent(in) :: STRNGA, STRNGB

      INTEGER I, ICOUNT, KB, IPOS1, IPOS2
      ICOUNT = 0
      IF (STRNGB.EQ.' ') GOTO 999

      KB=MyOwnLENTRIM(STRNGB)

      IPOS2 = 1
      DO I=1,KB
          IPOS1 = FNDFRST (STRNGA, STRNGB(IPOS2:),.false. )
          IF (IPOS1 .LT. 0) GOTO 999
          IPOS2 = IPOS2 + IPOS1
          ICOUNT  = ICOUNT+1
      ENDDO

  999 CONTINUE
      MyOwnCNTSTR=ICOUNT

      RETURN
      END Function MyOwnCntStr

!C--------------------------------------------------------------------
!C    MyOwnLENTRIM.FOR: Determine length of string, less trailing blanks
!C--------------------------------------------------------------------
      INTEGER FUNCTION MyOwnLentrim(STRING)
      CHARACTER(len=*), intent(in) :: STRING

      INTEGER I

!C        my own routine, not using Compaq 6.1 intrinsic routine
         MyOwnLentrim = 0
         IF (STRING.EQ.' ') RETURN
         I=LEN(STRING)
  10     IF (STRING(I:I).EQ.' ') THEN
           I=I-1
           GOTO 10
         ENDIF
         MyOwnLentrim = I


      RETURN
      END Function MyOwnLentrim

      Subroutine noextspaces(name,length )
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    E & Z
!
!             Module: SUBROUTINE NOEXTSPACES
!           Function: Removes the spaces at the front and the
!                     end of the name. Also returns the length of the
!                     cleaned name.
!                     WARNING: do not call this subroutine with a
!                     constant character string
!        Method used:
!               Date: 19-12-2000
!         Programmer: A.J. Mourits
!         CVS header
!            $Author: Mourits $
!              $Date: 21-02-01 15:58 $
!            $Source: /u/trisula/cvsroot/trisula/alg/filfmt.f,v $
!          $Revision: 14 $
!-----------------------------------------------------------------------
!   Calling routines:              various
!-----------------------------------------------------------------------
!   Called  routines:              none
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! NAME    IO CH*(*)                Name to be cleaned
! LENGTH   O  I*4                  Length of cleaned name
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! ACHAR      CH*1                  Help variable to check for spaces
! ENDPOS      I*4                  Last non-space position
! BEGPOS      I*4                  First non-space position
! POS         I*4                  Help counter
!-----------------------------------------------------------------------
!
! DECLARATIONS AND SPECIFICATIONS
!
      implicit none
!
      character(len=*) name
      character(len=1) achar
!
      integer         length,endpos,begpos,pos
!-----------------------------------------------------------------------
!-----initialization
!-----------------------------------------------------------------------
      begpos = 1
      endpos   = LEN (name)
!-----------------------------------------------------------------------
!-----skip backside spaces
!-----------------------------------------------------------------------
  100 achar = name(endpos:endpos)
      if ((achar .EQ. ' ') .AND. (endpos .GT. 0)) then
        endpos = endpos - 1
        goto 100
      endif
!-----------------------------------------------------------------------
!-----skip frontside spaces
!-----------------------------------------------------------------------
  200 achar = name(begpos:begpos)
      if ((achar .EQ. ' ') .AND. (begpos .LT. endpos)) then
        begpos = begpos + 1
        goto 200
      endif
!-----------------------------------------------------------------------
!-----adjust output
!-----------------------------------------------------------------------
      length         = endpos - begpos + 1
      name(1:length) = name(begpos:endpos)
      do 300 pos = length+1,LEN(name)
        name(pos:pos) = ' '
  300 continue
!----------------------------------------------------------------------
      end subroutine NoExtSpaces


      SUBROUTINE NULLSTRING(STRING, N, CHAR)

!-----------------------------------------------------------------------
!
!     Sobeksim
!     Computational core of Sobek-Rural and Sobek-Urban
!
!     Subroutine originally written by G. Prinsen
!
!     Concatenate string elements from array to 1 string.
!
!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N
      CHARACTER(LEN=N) STRING
      CHARACTER(len=1) CHAR
      INTEGER INDEX

      DO INDEX=1,N
         IF (STRING(INDEX:INDEX) .EQ. ' ') THEN
             STRING(INDEX:INDEX) = CHAR
         ENDIF
      ENDDO
 101  CONTINUE

      RETURN
      END Subroutine NullString


      Character(999) function readStr(line, teller)
        ! variables
      Character(Len=999)  , intent(in) :: Line
      Character(Len=999)                  tmpLine
      Integer                         teller, tmpTeller, position1, position2

      tmpLine = line
      do tmpTeller = 1, teller
         position1 = scan(tmpLine, ",")
         tmpLine = tmpLine(position1 + 1: len(tmpLine))
         position2 = scan(tmpLine, ",")
      end do
      tmpLine = tmpLine(2: )
      position2 = scan(tmpLine, '"')
      tmpLine = tmpLine(1: position2 - 1) ! -1 to avoid the quotes and comma's
      readStr = tmpLine

      return
      end function readStr


!--------------------------------------------------------------------
!     RPLSTR.FOR: Replace all occurences of string B in string A with String C
!--------------------------------------------------------------------

!      CHARACTER(*) FUNCTION RPLSTR(STRNGA, STRNGB, STRNGC)
      SUBROUTINE RPLSTR(STRNGA, STRNGB, STRNGC)

      CHARACTER(len=*)               :: STRNGA
      CHARACTER(len=1)  , intent(in) :: STRNGB, STRNGC

      INTEGER I, KA, IPOS1, IPOS2
      IF (STRNGA.EQ.' ') GOTO 999

      KA=Len_Trim(STRNGA)
!     Write(*,*) ' Rplstr'
!     Write(*,'(A9999)') STrngA
!     Write(*,'(A,I5)') ' Length of stringA=',KA
!      Write(*,'(A9)') STrngB
!      Write(*,'(A9)') STrngC

      IPOS2 = 1
      DO I=1,KA
!29 May 2002
! MyFndFrst works ok for finding spaces (it does not include spaces at the end of StrngA,
! but FndFrst = Index does not work fine for finding spaces (it does include spaces at the end of StrngA)
          If (StrngB .eq. ' ') then
            IPOS1 = MyFndFRST (STRNGB, STRNGA(IPOS2:),.TRUE.)
          else
            IPOS1 = FNDFRST (STRNGB, STRNGA(IPOS2:),.TRUE.)
          Endif
          IF (IPOS1 .LT. 0) GOTO 999
!          Write(*,*) ' Found string at position', ipos1, ipos2
          IPOS2 = IPOS2 + IPOS1 -1
          STRNGA(IPOS2:IPOS2) = STRNGC
      ENDDO

  999 CONTINUE
!      RPLSTR=STRNGA

      RETURN
      END Subroutine RplStr


      SUBROUTINE SKPCOM (IN, ENDFIL,SkipString)

!C *********************************************************************
!C *** OPERATIONAL RIVER BASIN WATER MANAGEMENT MODEL
!C *** LAST UPDATE : JULY 1993           BY : WIL VAN DER KROGT
!C **********************************************************************
!C ***            SKIP COMMENT RECORDS IN FILE : * - RECORDS
!C ***            ==========================================
!C ***
!C *** IN     : FILE REF. INDEX
!C ***
!C *** IF END OF FILE ENCOUNTERED THEN RETURN TO CALLING SUB
!C **********************************************************************

      INTEGER       IN, Lenskip
      CHARACTER(len=1) STAR, COMENT
      CHARACTER(len=*) SkipString
      CHARACTER(len=10) STRING
      LOGICAL       ENDFIL
      DATA          STAR /'*'/

!C *** SKIP COMMENT LINES : LINES STARTING WITH '*' or string SkipString

      LenSkip = Len (SkipString)
      ENDFIL = .FALSE.
    1 CONTINUE
      READ ( IN,'(A1)',Err=10,END=10) COMENT
      IF (COMENT .EQ. STAR) THEN
         GOTO 1
      ELSE
         BACKSPACE (IN)
         READ (IN,'(A10)') STRING
         CALL UPPERC (STRING)
         If (LenSkip .gt. 0) then
           IF (STRING (1:LenSkip) .EQ. SkipString(1:LenSkip) .OR. &
                   STRING (1:LenSkip) .EQ. SkipString(1:LenSkip)) GOTO 1
         Endif
         BACKSPACE (IN)
         GOTO 20
      ENDIF

   10 CONTINUE
         ENDFIL = .TRUE.
   20 CONTINUE

      RETURN
      END Subroutine SkpCom


      Subroutine small (string,lenstr)
!***********************************************************************
! Externe programmanaam  : SMALL.FOR
! Programmeur            : Cor van der Schelde, Jaap Zeekant
! Funktie omschrijving   : Omzetten van hoofdletters (in een string)
!                          naar kleine letters (ivm UNIX)
! Aangeroepen door       : Various routines
!                    Date: 30-07-1996
!              CVS header
!                 $Author: Mourits $
!                   $Date: 25-07-00 14:55 $
!                 $Source: /u/trisula/cvsroot/trisula/alg/small.f,v $
!               $Revision: 11 $
!
! verklaring externe variabelen/parameters
! ----------------------------------------
! naam    type      lengte   omschrijving
! ------  --------  ------   ------------
! lenstr  i*4       1        lengte van de string
! string  ch*(*)    1        string
!
!
! verklaring lokale variabelen
! ----------------------------
! naam    type      lengte   omschrijving
! ------  --------  ------   ------------
! BIGA    i*4       1        Code for 'A'
! BIGZ    i*4       1        Code for 'Z'
! i       i*4       1        loop variabele
! j       i*4       1        hulp variabele
! newlen  i*4       1        echte lengte string
! OFFSET  i*4       1        Difference between lower and upper case
! SMALLA  i*4       1        Code for 'a'
!
!
!-----------------------------------------------------------------------
! common blocks          : geen
!-----------------------------------------------------------------------
! subroutines            : ichar  (intrinsic function)
!                          char   (intrinsic function)
!-----------------------------------------------------------------------
! lun                    : geen
!***********************************************************************
      implicit none
      character(len=*) string
      integer       i, j, newlen, lenstr, biga, bigz, smalla, offset
!
      biga   = ICHAR('A')
      bigz   = ICHAR('Z')
      smalla = ICHAR('a')

      offset = smalla - biga

      newlen = MIN (lenstr,LEN (string))
      do 10 i=1,newlen
         j = ichar (string(i:i))
         if ( (j .ge. biga) .and. (j .le. bigz) ) then
            j = j + offset
            string(i:i) = char (j)
         endif
   10 continue
!
      return
      end subroutine small


      Logical FUNCTION StringComp (StringA, StringB, CaseSensitive)

!C *********************************************************************
!C ***    This function is used for comparing StringA and StringB
!C *********************************************************************
!C *** Input/output parameters:
!C *** ------------------------
!C ***  StringA      = string A
!C ***  StringB      = string B
!C ***  CaseSensitive= true  : compare case sensitive
!C ***                 false : compare not case sensitive
!C *********************************************************************
!C *** Return value in FindString:
!C ***     = true   strings are equal
!C ***       false  strings are not equal
!C *********************************************************************

      CHARACTER(len=*) StringA,  StringB
      CHARACTER(len=999) TempA, TempB
      Logical       CaseSensitive, Resultlog
!     INTEGER       LenTrim, LA, LB

!     LA = Len_Trim(StringA)
!     LB = Len_Trim(StringB)
!     If (LA .Ne. LB) Then
!        Resultlog = .false.
!     Else

         if (CaseSensitive) then
!           Zoeken is case-sensitive
            Resultlog = (StringA .eq. StringB)
         else
!           Zoeken is niet case-sensitive, converteer alles eerst naar upper case
            TempA  = StringA
            TempB  = StringB
            Call UpperC(TempA)
            Call UpperC(TempB)
            Resultlog = (TempA .eq. TempB)
         Endif

!     EndIf

      StringComp=Resultlog

      RETURN
      END Function StringComp


!C--------------------------------------------------------------------
!C     UPPERC.FOR : convert string to uppercase characters
!C--------------------------------------------------------------------

      SUBROUTINE UPPERC(STRING)

      implicit none

      CHARACTER(len=*) STRING
      integer       i, j, l, smalla, smallz, biga, offset

      L=LEN(STRING)

! oude code, met hard gecodeerde ASCII codes
!      I=0
!100   IF (STRING(I+1:).NE.' ') THEN
!       I=I+1
!       J=ICHAR(STRING(I:I))
!       IF (J.GT.96.AND.J.LT.123) STRING(I:I)=CHAR(J-32)
!       IF (I.LT.L) GOTO 100
!     ENDIF
!
!     RETURN
!     END Subroutine UpperC

! nieuw: onafhankelijk van character code set, alleen aanname dat a..z opeenvolgend en A..Z opeenvolgend
      smalla = ICHAR('a')
      smallz = ICHAR('z')
      biga   = ICHAR('A')

      offset = biga - smalla

      do i=1,l
         j = ichar (string(i:i))
         if ( (j .ge. smalla) .and. (j .le. smallz) ) then
            j = j + offset
            string(i:i) = char (j)
         endif
      enddo
!
      return
      end subroutine UpperC



    end module ReadLib

