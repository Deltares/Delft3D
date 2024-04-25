!----- LGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This library is free software; you can redistribute it and/or                
!  modify it under the terms of the GNU Lesser General Public                   
!  License as published by the Free Software Foundation version 2.1.                 
!                                                                               
!  This library is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
!  Lesser General Public License for more details.                              
!                                                                               
!  You should have received a copy of the GNU Lesser General Public             
!  License along with this library; if not, see <http://www.gnu.org/licenses/>. 
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
!  
!  
module Dio_Ini

use Dio_Prop

implicit none


!*
!* CONSTANTS AND DATATYPES
!*


!
! Sizes
!

integer, parameter :: DioMaxIniFileLen = 256 ! max file name length
integer, parameter :: DioMaxIniLineLen = 256 ! max length of line in file
integer, parameter :: DioMaxIniKeyLen  = 80  ! max length of key
integer, parameter :: MaxIntFormatLen  = 15  ! max length of integer format
integer, parameter :: MaxFormatLen     = 25  ! max length of format string
!
! IniFile Line Types
!

integer, parameter :: IniEmptyLine    =  1
integer, parameter :: IniGroupLine    =  2
integer, parameter :: IniItemLine     =  3
integer, parameter :: IniEnd          =  4
integer, parameter :: IniUnknownLine  =  5
integer, parameter :: IniNumLineTypes       =  5


!
! Data Type for ini file
!
type TDioIniFile
    character(Len=DioMaxIniFileLen):: name         ! iniFile Name
    character(Len=1)               :: mode         ! 'r' or 'w'
    integer                        :: lun          ! i/o handle
    character(Len=DioMaxIniLineLen):: lastLine     ! last read line 
    integer                        :: lastLineType ! type of last read line 
    logical                        :: blockParse   ! block parse at a
                                                   ! certain group?
    character(Len=DioMaxIniKeyLen) :: blockGroup   ! group to block on
    character(Len=DioMaxIniKeyLen) :: lastGroup    ! last group found

end type TDioIniFile


!*
!* Declaration of private functions
!*

private :: DioIniCheckOnGroup
private :: DioIniCheckOnItem


contains


!*
!* OPEN/CLOSE INI FILE
!*

!
! Open Ini File for read or write
!

function DioIniFileOpen(iniFile, name, mode) result(retVal)

    ! return value
    logical           :: retVal  ! .true.: succes

    ! arguments
    type(TDioIniFile) :: iniFile ! Dio IniFile Handle
    character(Len=*)  :: name    ! ini file name
    character(Len=*)  :: mode    ! 'r', 'w' or 'a'

    ! locals
    integer           :: lun     ! i/o handle
    integer           :: ierr    ! i/o status

    retVal = .false.

    iniFile % name         = ''
    iniFile % mode         = ''
    iniFile % lun          = 0
    iniFile % lastLine     = ''
    iniFile % lastLineType = IniUnknownLine
    iniFile % blockParse   = .false.
    iniFile % blockGroup   = ''
    iniFile % lastGroup    = ''

    ierr = -1
    if ( mode .eq. 'r') then
       open (newunit=lun, file=name, action='read', &
             status='old', form='formatted', iostat=ierr)
    else if ( mode .eq. 'w') then
       open (newunit=lun, file=name, action='write', &
             form='formatted', iostat=ierr)
    else if ( mode .eq. 'a') then
       open (newunit=lun, file=name, action='write', position='append', &
             status='old', form='formatted', iostat=ierr)
    endif
    if (ierr == 0 ) then
       iniFile % name  = name
       iniFile % mode  = mode
       iniFile % lun   = lun
       retVal = .true.
    endif

end function DioIniFileOpen
    

!
! Close Ini File
!
subroutine DioIniFileClose(iniFile)

    ! arguments
    type(TDioIniFile)  :: iniFile ! Dio IniFile Handle

    ! locals
    logical            :: opend   ! lun already open?

    ! body:
    ! - If lun opened, close it.
    ! - Reset iniFile info

    if ( iniFile % lun .ne. 0 ) then
        inquire( iniFile % lun, opened = opend )
        if ( opend ) then
            close(iniFile % lun)
        endif
    endif
    iniFile % name     = ''
    iniFile % mode     = ''
    iniFile % lun      = 0
    iniFile % lastLine = ''

end subroutine DioIniFileClose


!
! Close and delete Ini File
!
subroutine DioIniFileCloseAndDelete(iniFile)

    ! arguments
    type(TDioIniFile)  :: iniFile ! Dio IniFile Handle

    ! locals

    logical            :: opend   ! lun already open?

    ! body:
    ! - If lun opened, close and delete it.
    ! - Reset iniFile info

    if ( iniFile % lun .ne. 0 ) then
        inquire( iniFile % lun, opened = opend )
        if ( opend ) then
           close(iniFile % lun, status='delete')
        endif
    endif
    iniFile % name     = ''
    iniFile % mode     = ''
    iniFile % lun      = 0
    iniFile % lastLine = ''

end subroutine DioIniFileCloseAndDelete


!*
!* PERFORMANCE SUPPORT
!*


!
! Stop parsing at a group
!

subroutine DioIniSetStopAtGroup(iniFile, groupName)

    ! arguments

    type(TDioIniFile) :: iniFile   ! Dio IniFile Handle
    character(Len=*)  :: groupName ! group to block on

    ! locals

    iniFile % blockParse = .true.
    iniFile % blockGroup = groupName

end subroutine DioIniSetStopAtGroup


!
! Unset stop parsing at a group
!

subroutine DioIniUnsetStopAtGroup(iniFile)

    ! arguments

    type(TDioIniFile) :: iniFile   ! Dio IniFile Handle

    ! locals

    iniFile % blockParse = .false.
    iniFile % blockGroup = ''

end subroutine DioIniUnsetStopAtGroup


!*
!* EXTERNAL PARSE FUNCTIONS
!*

!
! Get next ini line
!
function DioIniNextLine(iniFile, keyStr,   keyLen,   &
                                 valueStr, valueLen) result(lineType)
    ! return value
    integer                       :: lineType ! line type found in IniFile 
    ! arguments
    type(TDioIniFile)             :: iniFile  ! Dio IniFile Handle
    character(Len=*), intent(out) :: keyStr   ! group or item string
    integer                       :: keyLen   ! length of group or item string
    character(Len=*), intent(out) :: valueStr ! item's value string
    integer                       :: valueLen ! length of item's value string

    ! body: initialize, read, check contents

    lineType = IniEmptyLine
    keyStr  = ''
    valueStr = ''
    keyLen  = 0
    valueLen = 0

    ! read line, check for error or end

    iniFile % lastLineType = IniEmptyLine
    iniFile % lastLine = ''
    read(iniFile % lun, '(A)', err=999, end=999) iniFile % lastLine

    ! succesful read, check on:
    ! 1. <key> = <value>
    ! 2. empty line
    ! 3. [group] line

    if ( DioIniCheckOnItem(iniFile % lastLine, keyStr, valueStr) ) then
        keyLen   = len_trim(keyStr)
        valueLen = len_trim(valueStr)
        iniFile % lastLineType = IniItemLine
    else
        if ( iniFile % lastLine .ne. '') then
            if ( DioIniCheckOnGroup(iniFile % lastLine, keyStr) ) then
                iniFile % lastLineType = IniGroupLine
            endif
        endif
    endif

    ! return result of line parsing

    lineType = iniFile % lastLineType
    return

    ! Error handling (Could not read line or End Of File )
    ! set return lineType to 'Ini-End-of-file'

999 continue
    iniFile % lastLineType = IniEnd
    lineType = IniEnd

end function DioIniNextLine


!
! Get next ini line with an integer key.
!
function DioIniNextIntKeyLine(iniFile, keyValue, valueStr) result(retVal)

    ! return value
    logical                        :: retVal    ! .true.: success

    ! arguments
    type(TDioIniFile)              :: iniFile   ! Dio IniFile Handle
    integer         , intent(out)  :: keyValue  ! integer value of key
    character(Len=*), intent(out)  :: valueStr  ! item's value string

    ! locals
    integer                        :: lineType  ! line type found in IniFile 
    character(Len=DioMaxIniKeyLen) :: keyStr    ! group or item string
    integer                        :: keyLen    ! length of group or item string
    integer                        :: valueLen  ! length of item's value string
    character(Len=20)              :: keyFormat ! key format for reading int value

    retVal = .false.
    lineType = IniItemLine
    do while ( (lineType .ne. IniEnd) .and. (lineType .ne. IniGroupLine) .and. &
               (.not. retVal) )
        lineType = DioIniNextLine(iniFile, keyStr, keyLen, valueStr, valueLen)
        if (lineType == IniItemLine) then
            write(keyFormat, '(A2,I2.2,A1)') '(I', keyLen, ')'
            read(keyStr, keyFormat, err=999) keyValue
            retVal = .true.
        endif
    enddo

    ! return result
    return

    ! Error handling (Could not read line or End Of File )
    ! return value already set to false, so no action required.
999 continue

end function DioIniNextIntKeyLine


function DioIniSkipToGroup(iniFile, groupName) result(retVal)

    ! return value
    logical                        :: retVal    ! .true.: succes

    ! arguments

    type(TDioIniFile)              :: iniFile   ! Dio IniFile Handle
    character(Len=*), intent(in)   :: groupName ! group to go to

    ! locals
    character(Len=DioMaxIniKeyLen) :: keyStr   ! group or item string
    character(Len=DioMaxIniLineLen):: valueStr ! item's value string
    integer                        :: keyLen   ! length of group or item string
    integer                        :: valueLen ! length of item's value string
    integer                        :: lineType ! line type found in IniFile 
    logical                        :: goOn     ! Go On parsing

    ! body:
    ! - check if last read line contains group == groupName; if so: success
    ! - if not, search groups in remainder of file

    retVal = .false.

    if ( iniFile % lun == 0 ) return

    if ( iniFile % lastLineType == IniGroupLine ) then
        keyStr = groupName
        if ( DioIniCheckOnGroup(iniFile % lastLine, keyStr) ) then
            retVal = .true.
        endif

    endif
    
    if ( .not. retVal ) then
        lineType = IniUnknownLine
        goOn = .true.
        do while ( lineType .ne. IniEnd .and. goOn)
            lineType = DioIniNextLine(iniFile, keyStr, keyLen, valueStr, valueLen)
            if ( lineType == IniGroupLine ) then
                if ( StringsEqual(CaseInsens, groupName, keyStr) ) then
                     retVal = .true.
                     goOn   = .false.
                else if ( StringsEqual(CaseInsens, iniFile % blockGroup, keyStr) ) then
                     goOn   = .false.
                endif
            endif 
        end do
    endif

    if ( retVal ) then
        iniFile % lastGroup = groupName
    else
        iniFile % lastGroup = ''
    endif

end function DioIniSkipToGroup


function DioIniGoToGroup(iniFile, groupName) result(retVal)

    ! return value
    logical                      :: retVal    ! .true.: succes

    ! arguments
    type(TDioIniFile)            :: iniFile   ! Dio IniFile Handle
    character(Len=*), intent(in) :: groupName ! group to go to

    ! locals

    ! body: rewind and skip to group

    retVal = .false.

    if ( iniFile % lun == 0 ) return

    rewind( iniFile % lun )
    retVal = DioIniSkipToGroup(iniFile, groupName) 

end function DioIniGoToGroup


function DioIniFindGroup(iniFile, groupName) result(retVal)
    ! return value
    logical                      :: retVal    ! .true.: succes

    ! arguments
    type(TDioIniFile)            :: iniFile   ! Dio IniFile Handle
    character(Len=*), intent(in) :: groupName ! group to go to

    ! body: first try skip to group, if not succesful, search from start
    retVal = DioIniSkipToGroup(iniFile, groupName)
    if (.not. retVal) then
        retVal = DioIniGoToGroup(iniFile, groupName)
    endif
end function DioIniFindGroup


function DioIniSkipToItem(iniFile, itemName, valueStr) result(retVal)

    ! return value
    logical                        :: retVal   ! .true.: succes

    ! arguments

    type(TDioIniFile)              :: iniFile  ! Dio IniFile Handle
    character(Len=*), intent(in)   :: itemName ! item to go to
    character(Len=*), intent(out)  :: valueStr ! item's value string

    ! locals
    character(Len=DioMaxIniKeyLen) :: keyStr   ! group or item string
    integer                        :: keyLen   ! length of item or item string
    integer                        :: valueLen ! length of item's value string
    integer                        :: lineType ! line type found in IniFile 

    ! body:
    ! - check if last read line contains item == itemName; if so: success
    ! - if not, search items in remainder of file

    retVal = .false.

    if ( iniFile % lun == 0 ) return

    lineType = IniUnknownLine
    do while ( lineType .ne. IniEnd .and. lineType .ne. IniGroupLine &
                                  .and. (.not. retVal)              )
        lineType = DioIniNextLine(iniFile, keyStr, keyLen, valueStr, valueLen)
        if ( lineType == IniItemLine ) then
            if ( StringsEqual(CaseInsens, itemName, keyStr) ) then
                 retVal = .true.
            endif
        else if ( lineType == IniGroupLine ) then
            iniFile % lastGroup = ''
        endif 
    end do

end function DioIniSkipToItem


function DioIniFindItem(iniFile, groupName, itemName, &
                                            valueStr ) result(retVal)

    ! return value
    logical                      :: retVal    ! .true.: succes
    ! arguments
    type(TDioIniFile)            :: iniFile   ! Dio IniFile Handle
    character(Len=*), intent(in) :: groupName ! group to find
    character(Len=*), intent(in) :: itemName  ! item to find
    character(Len=*), intent(out):: valueStr  ! value string for item

    ! locals

    ! body: Go To group again, and skip to item

    retVal = .false.

    if ( iniFile % lastGroup /= groupName ) then
        retVal = DioIniSkipToGroup(iniFile, groupName)
        if ( .not. retVal ) then
            retVal = DioIniGoToGroup(iniFile, groupName)
        endif
    else
        retVal = .true.
    endif

    if ( retVal ) then
        retVal = DioIniSkipToItem(iniFile, itemName, valueStr)
        if ( .not. retVal ) then
            retVal = DioIniGoToGroup(iniFile, groupName)
            if ( retVal ) then
                retVal = DioIniSkipToItem(iniFile, itemName, valueStr)
            endif
        endif
    endif

end function DioIniFindItem


function DioIniFindIntItem(iniFile, groupName, itemName, &
                                            intValue) result(retVal)

    ! return value
    logical                        :: retVal    ! .true.: succes
    ! arguments
    type(TDioIniFile)              :: iniFile   ! Dio IniFile Handle
    character(Len=*), intent(in)   :: groupName ! group to find
    character(Len=*), intent(in)   :: itemName  ! item to find
    integer         , intent(inout):: intValue  ! length of item's value string

    ! locals
    character(Len=DioMaxIniLineLen):: valueStr  ! item's value string
    integer                        :: valueLen  ! length of item's value string
    character(Len=20)              :: valFormat ! key format for reading int value

    ! body: Go To group again, and skip to item

    retVal = DioIniFindItem(iniFile, groupName, itemName, valueStr)

    if (retVal ) then
        valueLen = len_trim(valueStr)
        write(valFormat, '(A2,I2.2,A1)') '(I', valueLen, ')'
        read(valueStr, valFormat, err=999) intValue
    endif

    return
    
    ! Error handling (Could not read value)
999 continue
    retVal = .false.

end function DioIniFindIntItem


!*
!* INTERNAL SUPPORT FUNCTIONS
!*


function DioIniMakPosIntFormat(intVal, intFormat) result(retVal)

    ! return value
    logical                      :: retVal    ! .true.: succes

    ! arguments
    integer                      :: intVal    ! int to write
    character(Len=*), intent(out):: intFormat ! format to write int

    ! locals
    integer                      :: nDigits   ! #digits in format

    retVal = .false.
    if (intVal >= 0 ) then
        retVal = .true.
        if ( intVal < 10 ) then
            nDigits = 1
        else if ( intVal < 100 ) then
            nDigits = 2
        else if ( intVal < 1000 ) then
            nDigits = 3
        else if ( intVal < 10000 ) then
            nDigits = 4
        else if ( intVal < 100000 ) then
            nDigits = 5
        else if ( intVal < 1000000 ) then
            nDigits = 6
        else if ( intVal < 10000000 ) then
            nDigits = 7
        else if ( intVal < 100000000 ) then
            nDigits = 8
        else
            retVal = .false.
        endif
        if ( retVal ) then
            write(intFormat, '(A,I1)' ) 'I', nDigits
        endif
    endif
end function DioIniMakPosIntFormat


function DioIniCheckOnGroup(iniLine, groupName) result(retVal)

    ! return value

    logical                      :: retVal    ! .true.: succes

    ! arguments
    character(Len=*), intent(in) :: iniLine   ! line ini file
    character(Len=*), &
         intent(inout), optional :: groupName ! group to check on

    ! locals
    integer                      :: leftPos  ! '[' position
    integer                      :: rightPos ! ']' position

    ! body:
    ! - check if iniLine Contains a group; if so:
    ! - if groupname present but empty:  provide the iniLine group name
    ! - if groupname present and filled: check if iniLine contains groupName

    retVal = .false.

    rightPos = 0

    leftPos = index(iniLine, '[')
    if ( leftPos > 0 ) rightPos = index(iniLine, ']')

    if ( rightPos > 0 ) then
        retVal = .true.
    endif

    if ( retVal .and. present(groupName) ) then
        if ( groupName == '' ) then
            groupName = iniLine(leftPos+1:rightPos-1)
        else
            if ( StringSEqual( CaseInsens, IniLine(leftPos+1:rightPos-1),&
                               groupName                                 ) ) then
                retVal = .true.
            else
                retVal = .false.
            endif
        endif
    endif

end function DioIniCheckOnGroup


function DioIniCheckOnItem(iniLine, itemName, valueStr) result(retVal)

    ! return value

    logical                      :: retVal   ! .true.: succes

    ! arguments
    character(Len=*), intent(in) :: iniLine  ! line ini file
    character(Len=*), &
         intent(inout), optional :: itemName ! item to go to
    character(Len=*), &
         intent(inout), optional :: valueStr ! group to go to

    ! locals
    integer                      :: isPos    ! '=' position

    ! body:
    ! - check if iniLine Contains an item; if so:
    ! - if itemName present but empty:  provide the iniLine item name
    ! - if itemName present and filled: check if iniLine contains itemName

    retVal = .false.

    isPos = index(iniLine, '=')
    if ( isPos > 0 ) then
        retVal = .true.
    endif

    if ( retVal .and. present(itemName) .and. present(valueStr) ) then
        if ( itemName == '' ) then
            itemName = iniLine(:isPos-1)
            valueStr = iniLine(isPos+1:)
        else
            if ( StringSEqual( CaseInsens, IniLine(:isPos-1), itemName) ) then
                valueStr = iniLine(isPos+1:)
                retVal = .true.
            else
                retVal = .false.
            endif
        endif
    endif

end function DioIniCheckOnItem


!*
!* EXTERNAL SUPPORT FUNCTIONS
!*

!
! Write an <Int>=<String> line in free format
!

subroutine DioIniWriteIntIsStrLine(lun, intVal, strVal)

    ! arguments
    integer                        :: lun        ! file handle
    integer                        :: intVal     ! int to write
    character(Len=*), intent(in)   :: strVal     ! string to write

    ! locals
    character(Len=MaxIntFormatLen) :: intFormat  ! format for int part
    character(Len=MaxFormatLen)    :: lineFormat ! format to write line

    if ( DioIniMakPosIntFormat(intVal, intFormat) ) then
        lineFormat = '(' // trim(intFormat) // ',A,A)'
        write(lun, lineFormat) intVal, '=', trim(strVal)
    endif

end subroutine DioIniWriteIntIsStrLine


subroutine DioIniWriteStrIsIntLine(lun, strVal, intVal)

    ! arguments
    integer                        :: lun        ! file handle
    character(Len=*), intent(in)   :: strVal     ! string to write
    integer                        :: intVal     ! int to write

    ! locals
    character(Len=MaxIntFormatLen) :: intFormat  ! format for int part
    character(Len=MaxFormatLen)    :: lineFormat ! format to write line

    if ( DioIniMakPosIntFormat(intVal, intFormat) ) then
        lineFormat = '(A,A,' // trim(intFormat) // ')'
        write(lun, lineFormat) trim(strVal), '=', intVal
    endif

end subroutine DioIniWriteStrIsIntLine


subroutine DioIniWriteStrIsStrLine(lun, keyVal, strVal)

    ! arguments
    integer                      :: lun        ! file handle
    character(Len=*), intent(in) :: keyVal     ! key to write
    character(Len=*), intent(in) :: strVal     ! string to write

    ! locals
    write(lun, '(A,A,A)') trim(keyVal), '=', trim(strVal)

end subroutine DioIniWriteStrIsStrLine


subroutine DioIniWriteGroupLine(lun, grpName)

    ! arguments
    integer                      :: lun        ! file handle
    character(Len=*), intent(in) :: grpName    ! group to write

    ! locals
    write(lun, '(A,A,A)') '[', trim(grpName), ']'

end subroutine DioIniWriteGroupLine


end module Dio_Ini


