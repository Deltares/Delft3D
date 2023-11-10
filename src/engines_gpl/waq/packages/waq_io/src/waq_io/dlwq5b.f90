!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
module m_dlwq5b

    implicit none

    contains


    subroutine dlwq5b (lunut  , iposr  , npos   , cchar  , car    ,&
                          iar    , icmax  , iimax  , aname  , atype  ,&
                          ntitm  , nttype , noitm  , noits  , chkflg ,&
                          callr  , ilun   , lch    , lstack ,&
                          itype  , rar    , nconst , itmnr  , chulp  ,&
                                            ioutpt , ierr   , iwar)
!     Logical Units      : Lunut   = Unit Formatted Output File
!
!     Parameters    :
!
!     Name    Kind     Length     Funct.  Description
!     ---------------------------------------------------------
!     iposr   integer    1         in/out  Start position on input line
!     npos    integer    1         input   Nr of significant characters
!     cchar   char*1     1         input   Comment character
!     car     character  *         output  Character workspace
!     iar     integer  iimax       output  Integer   workspace
!     icmax   integer    1         input   Max. Char workspace dimension
!     iimax   integer    1         input   Max. Int. Workspace dimension
!     aname   char*20    *         input   Id's of the boundaries/wastes
!     atype   char*20    *         input   Types of the boundaries/wastes
!     ntitm   integer    1         input   Number of bounds/wastes
!     nttype  integer    1         input   Number of bound/waste types
!     noitm   integer    1         output  Number of items read
!     noits   integer    1         output  Number of items for scale
!     chkflg  integer    1         input   Check on input or add items
!     callr   char*(6)   1         input   Calling subject
!     ilun    integer   lstack     in/out  Unitnumb include stack
!     lch     char*(*)  lstack     in/out  File name stack, 4 deep
!     lstack  integer    1         input   Include file stack size
!     itype   integer    1         output  Type of the token at exit
!     rar     real       *         output  Array with real values
!     nconst  real       *         output  Number of those values
!     chulp   char*(*)   1         output  Input string at end of routine
!     ioutpt  integer    1         input   Output file option
!     ierr    integer    1         output  Error indicator
!     iwar    integer    1         output  Cumulative warning count
!
!
    use m_string_utils, only: location_of

    use m_movint
    use m_movchr
    use timers       !   performance timers

    integer       icmax   , iimax    , chkflg
    character*(*) car(*)  , aname(*) , atype(*) , lch(lstack) ,&
                  chulp
    character*1   cchar*1 , callr*10
    logical       usefor, setnam, comput, signon
    integer(4) :: ithndl = 0
    integer    :: i, ihulp, ierr, iabs, iar(:), ifound, i2
    integer    :: namset, ioutpt, icm, ntitm, nttype, iwar, lstack
    real       :: vrsion
    integer    :: itmnr, ioff, ioffc, nconst, itype, lunut, ilun(lstack)
    integer    :: iposr, npos, noitm, noits, ioffi
    real       :: rar(:), rhulp

    if (timon) call timstrt("dlwq5b", ithndl)

    !
    ! some initialisations

    usefor = .false.
    setnam = .false.
    comput = .false.
    signon = .false.
    noitm  = 0
    noits  = 0
    itmnr  = 0
    ioff   = 0
    ioffc  = 0
    ioffi  = 0
    nconst = 0
    !
    !          Get a token string (and return if something else was found)
10  itype = -3
    if (signon .or. (usefor .and. setnam)) itype = 0
    call rdtok1 (lunut  , ilun   , lch    , lstack , cchar  ,&
                    iposr  , npos   , chulp  , ihulp  , rhulp  ,&
                                               itype  , ierr)
    if (ierr .ne. 0) goto 9999

    
    ! if a keyword was met
    if (iabs(itype) == 1 .and. &
        (any(['BLOCK'        ,&
              'LINEAR'       ,&
              'ITEM'         ,&
              'IDENTICALITEM',&
              'USEDATA_ITEM' ,&
              'FORITEM'      ,&
              'DATA_ITEM'    ,&
              'CONCEN'       ,&
              'DATA'         ,&
              'TIME_DELAY'   ,&
              'ODS_FILE'     ,&
              'BINARY_FILE'  ,&
              'ABSOLUTE'     ,&
              'TIME'         ,&
              'HARMONICS'    ,&
              'FOURIERS'     ,&
              'SCALE'        ,&
              'DEFAULTS'     ,&
              'ALL'          ,&
              'SEGMENTS'     ,&
              'CONSTANTS'    ,&
              'PARAMETERS'   ,&
              'FUNCTIONS'    ,&
              'SEG_FUNCTIONS' ] == trim(chulp)))) then
        if (usefor) then
            write (lunut, 1035) chulp
            goto 40
        else
            goto 9999
        end if
    end if

    ! if computation
    if (iabs(itype) == 1 .and.&
           (any(['*', '/', '+', '-', 'MIN', 'MAX'] == trim(chulp)))) then
        if (.not. comput) then
            write (lunut , 1070)
            goto 40
        end if
        if (signon) then
            write (lunut , 1080)
            goto 40
        end if
        noitm = noitm + 1
        noits = noits + 1
        call movint (iar   , itmnr+noitm , itmnr+noitm*2)
        iar(itmnr+noitm+noitm) = 0
        select case(chulp)
            case ('*')
                iar(itmnr+noitm) = -1000000
            case ('/')
                iar(itmnr+noitm) = -10000000
            case ('+')
                iar(itmnr+noitm) = -100000000
            case ('-')
                iar(itmnr+noitm) = -1000000000
            case ('MIN')
                iar(itmnr+noitm) = -1100000000
            case ('MAX')
                iar(itmnr+noitm) = -1200000000
        end select
        signon = .true.
        goto 10
    end if

    ! if an item used in computations
    if (iabs(itype) == 1 .and. signon) then
        do 15 i=1, itmnr-1
            if (iar(i) == -1300000000) goto 15
            ifound = location_of(chulp, car(i+ioff:i+ioff))
            if (ifound == 1) then
                noits = noits - 1
                i2 = iar(itmnr+noitm)
                select case(i2)
                    case (-1000000)
                        write(lunut,1120)i,chulp
                    case (-10000000)
                        write(lunut,1110)i,chulp
                    case (-100000000)
                        write(lunut,1100)i,chulp
                    case (-1000000000)
                        write(lunut,1090)i,chulp
                    case (-1100000000)
                        write(lunut,1092)i,chulp
                    case (-1200000000)
                        write(lunut,1094)i,chulp
                end select
                iar(itmnr+noitm) = i2 + i
                car(itmnr+noitm+ioff) = '&$&$SYSTEM_NAME&$&$!'
                signon = .false.
                goto 10
            end if
        15 end do
        i2 = iar(itmnr + noitm)
        select case(i2)
            case (-1000000)
                write(lunut,1130)chulp
            case (-10000000)
                write(lunut,1140)chulp
            case (-100000000)
                write(lunut,1150)chulp
            case (-1000000000)
                write(lunut,1160)chulp
            case (-1100000000)
                write(lunut,1162)chulp
            case (-1200000000)
                write(lunut,1164)chulp
        end select

        iar (itmnr+noitm + noitm) = noits
        car (itmnr+noitm + ioff) = chulp
        signon = .false.
        goto 10
    end if

    ! if a number is used in computations
    if (iabs(itype) == 2 .or. iabs(itype) == 3) then
        if (setnam .or. signon) then
            nconst = nconst + 1
            rar(nconst) = rhulp
            noits = noits - 1
            i2 = iar(itmnr+noitm)
            car(itmnr+noitm+ioff) = '&$&$SYSTEM_NAME&$&$!'
            if (signon) then
                select case (i2)
                    case (-1000000)
                        write(lunut,1170)rhulp
                    case (-10000000)
                        write(lunut,1180)rhulp
                    case (-100000000)
                        write(lunut,1190)rhulp
                    case (-1000000000)
                        write(lunut,1200)rhulp
                    case (-1100000000)
                        write(lunut,1210)rhulp
                    case (-1200000000)
                        write(lunut,1220)rhulp
                end select
               iar(itmnr+noitm) = i2 - nconst
               signon = .false.
            end if
            if (setnam) then
                namset = iar(itmnr)
                if (namset > 0 .and. ioutpt >= 3) then
                    write (lunut , 1001) callr, itmnr, callr, namset ,&
                                         aname(namset) , rhulp
                else if (namset == 0 .and. ioutpt >= 3) then
                    write (lunut , 1001) callr, itmnr, callr, namset ,&
                                         'FLOW'        , rhulp
                else if (namset == -1300000000 .and. ioutpt >= 3) then
                    write (lunut , 1001) callr, itmnr, callr, namset ,&
                                         'Ignored'     , rhulp
                else if (ioutpt >= 3) then
                    write (lunut , 1011) callr, itmnr, callr,-namset ,&
                                         atype(-namset) , rhulp
                end if
               iar(itmnr+noitm) =  -nconst
               iar(itmnr+noitm+noitm) = 0
               usefor = .false.
               setnam = .false.
               comput = .true.
            end if
            goto 10
        end if
    end if

    !if a local redirection of the name of an item or substance
    if (iabs(itype) == 1 .and. chulp == 'USEFOR') then
        if (usefor) then
            write (lunut , 1035) chulp
            goto 40
        else
            usefor = .true.
            setnam = .false.
            goto 10
        end if
    end if
    !
    ! Getting the items of this block
    !               NOITM  is the order number in the series
    !               NAMSET is the ID number of NOITMth name
    !               ANAME/ATYPE(NAMSET) is the corresponding
    !               reserved name or type
    !               CHULP is the name that should be used.
    !               IARR(ITMNR) stores NAMSET
    !
    if (itype == 1) then
        if (usefor .and. setnam) then
            namset = iar(itmnr)
            if (ioutpt>=3) then
                if (namset>0) then
                    write (lunut , 1000) callr , itmnr , callr , namset ,&
                                      aname(namset) , chulp
                else if (namset==0) then
                    write (lunut , 1000) callr , itmnr , callr , namset ,&
                                      'FLOW'        , chulp
                else if (namset == -1300000000) then
                    write (lunut , 1000) callr , itmnr , callr , namset ,&
                                      'Ignored'     , chulp
                else
                    write (lunut , 1010) callr , itmnr , callr ,-namset ,&
                                      atype(-namset) , chulp
                end if
            end if
            iar (itmnr + noitm + noitm) = noits
            car (itmnr + noitm + ioff) = chulp
            usefor = .false.
            setnam = .false.
            ! it is now possible to compute
            comput = .true.
            goto 10
        end if
        ! fill in a string value if an empty string is provided
        if (chkflg      == -1 .and. chulp(1:20) == repeat(' ', 20)) then
            chulp = 'Item-'
            write (chulp(6:12) , '(I7)') noitm+1
        end if
        ! FLOW is only valid as CONCENTR. and item number is 0
        ifound = location_of(chulp, ['FLOW                '])
        if (ifound == 1 .and. callr == 'CONCENTR. ') then
            noitm = noitm + 1
            noits = noits + 1
            itmnr = itmnr + 1
            icm = itmnr + noitm + ioff
            call movint(iar, itmnr      , itmnr+noitm*2)
            call movint(iar, itmnr+noitm, itmnr+noitm*2)
            call movchr(car, itmnr+ioff , icm)
            iar (itmnr) =  0
            iar (itmnr + noitm) = itmnr
            iar (itmnr + noitm + noitm) = noits
            car (itmnr + ioff) = chulp
            car (itmnr + noitm + ioff) = chulp
            if (usefor) setnam = .true.
            if (ioutpt >= 3 .and. .not. usefor)&
            write (lunut , 1020) callr , itmnr , callr , 0 , 'FLOW'
            goto 10
        end if

        ! CHULP equals an item-NAME
        i2 = location_of(chulp, aname(1:ntitm))
        if (i2 >= 1) then
            noitm = noitm + 1
            noits = noits + 1
            itmnr = itmnr + 1
            icm = itmnr + noitm + ioff
            call movint(iar, itmnr      , itmnr+noitm*2)
            call movint(iar, itmnr+noitm, itmnr+noitm*2)
            call movchr(car, itmnr+ioff , icm)
            iar(itmnr) =  i2
            iar(itmnr + noitm) = itmnr
            iar(itmnr + noitm + noitm) = noits
            car(itmnr + ioff) = chulp
            car(itmnr + noitm + ioff) = chulp
            if (usefor) setnam = .true.
            if (ioutpt >= 3 .and. .not. usefor) then
                write (lunut , 1020) callr, itmnr, callr, i2, aname(i2)
            end if
            goto 10
        end if

        ! CHULP equals an item-TYPE. IAR now is negative.
        i2 = location_of(chulp,atype(1:nttype))
        if (i2 >= 1) then
            noitm = noitm + 1
            noits = noits + 1
            itmnr = itmnr + 1
            icm = itmnr + noitm + ioff
            call movint(iar, itmnr      , itmnr+noitm*2)
            call movint(iar, itmnr+noitm, itmnr+noitm*2)
            call movchr(car, itmnr+ioff , icm)
            iar(itmnr) = -i2
            iar(itmnr + noitm) = itmnr
            iar(itmnr + noitm + noitm) = noits
            car(itmnr + ioff) = chulp
            car(itmnr + noitm + ioff) = chulp
            if (usefor) setnam = .true.
            if (ioutpt >= 3 .and. .not. usefor) then
                write (lunut , 1030) callr, itmnr, callr, i2, atype(i2)
            end if
            goto 10
        end if

        ! If only existing names or types are allowed then
        !        this is the place for an error massage
        ! JVB stick to just a warning keep on reading IAR = 0?, or used for flow??

        if (chkflg == 1) then
            noitm = noitm + 1
            noits = noits + 1
            itmnr = itmnr + 1
            icm = itmnr + noitm + ioff
            call movint(iar, itmnr      , itmnr+noitm*2)
            call movint(iar, itmnr+noitm, itmnr+noitm*2)
            call movchr(car, itmnr+ioff , icm)
            iar (itmnr) = -1300000000
            iar (itmnr + noitm) = 1300000000
            iar (itmnr + noitm + noitm) = noits
            car (itmnr + ioff) = chulp
            car (itmnr + noitm + ioff) = chulp
            if (usefor) setnam = .true.
            write(lunut , 1040) callr, itmnr, chulp
            iwar = iwar + 1
            goto 10
        else

            ! Now a new name is added to the list of names
            !        the rest is moved upward since it is all 1 array
            ntitm = ntitm + 1
            ioff  = ioff  + 1
            icm   = icmax + ntitm
            call movchr (aname, ntitm, icm)
            aname(ntitm) = chulp
            ! plus normal procedure
            noitm = noitm + 1
            noits = noits + 1
            itmnr = itmnr + 1
            icm = itmnr + noitm + ioff
            call movint(iar, itmnr      , itmnr+noitm*2)
            call movint(iar, itmnr+noitm, itmnr+noitm*2)
            call movchr(car, itmnr+ioff , icm)
            iar(itmnr) = ntitm
            iar(itmnr + noitm) = itmnr
            iar(itmnr + noitm + noitm) = noits
            car(itmnr + ioff) = chulp
            car(itmnr + noitm + ioff) = chulp
            if (usefor) setnam = .true.
            if (ioutpt >= 3 .and. .not. usefor) then
                write (lunut , 1020) callr, itmnr, callr, ntitm, aname(ntitm)
            end if
            goto 10
        end if
    end if

    ! If no item name was given, but an item number
    if (itype == 2) then
        if (ihulp <=  ntitm .and. ihulp >= -nttype) then
            noitm = noitm + 1
            noits = noits + 1
            itmnr = itmnr + 1
            icm = itmnr + noitm + ioff
            call movint (iar   , itmnr       , itmnr+noitm*2)
            call movint (iar   , itmnr+noitm , itmnr+noitm*2)
            call movchr (car   , itmnr+ioff  , icm)
            iar (itmnr) = ihulp
            iar (itmnr + noitm) = itmnr
            iar (itmnr + noitm + noitm) = noits
            if (callr == 'segment') then
                if (ihulp <= 0) then
                    write (lunut , 1060) ihulp
                    goto 40
                end if
                if (ioutpt >= 3 .and. .not. usefor)&
                    write (lunut , 1015) callr, itmnr, callr,  ihulp
                write (chulp , '(''Segment '',I8)') ihulp
            else if (ihulp == 0 .and. callr .ne. 'CONCENTR. ') then
                write (lunut , 1060) ihulp
                goto 40
            else if (ihulp > 0) then
                if (ioutpt >= 3 .and. .not. usefor)&
                    write (lunut , 1020) callr, itmnr, callr,  ihulp,&
                                                         aname(ihulp)
                chulp = aname(ihulp)
            else if (ihulp == 0 .and. callr == 'CONCENTR. ') then
                if (ioutpt >= 3 .and. .not. usefor)&
                write (lunut , 1020) callr, itmnr, callr, ihulp,&
                                                          'FLOW'
                chulp = 'FLOW'
            else
                if (ioutpt >= 3 .and. .not. usefor)&
                write (lunut , 1030) callr, itmnr, callr, -ihulp,&
                                                         atype(-ihulp)
                chulp = atype(-ihulp)
            end if
            car (itmnr + ioff) = chulp
            car (itmnr + noitm + ioff) = chulp
            if (usefor) setnam = .true.
            goto 10
        else
            write (lunut , 1060) ihulp
            goto 40
        end if
    end if
!
   40 ierr = 1
 9999 if (timon) call timstop(ithndl)
      return
!
 1000 format(' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',&
               A20,' and local substitution: ',A20)
 1001 format(' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',&
               A20,' and local substitution: ',E15.6)
 1010 format(' Input ',A,' nr:',I5,' is ',A,' type:',I5,&
               ' with type: ',A20,' and local substitution: ',A20)
 1011 format(' Input ',A,' nr:',I5,' is ',A,' type:',I5,&
               ' with type: ',A20,' and local substitution: ',E15.6)
 1015 format(' Input ',A,' nr:',I5,' is ',A,' nr:',I5)
 1020 format(' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with ID  : ',&
               A20)
 1030 format(' Input ',A,' nr:',I5,' is ',A,' nr:',I5,' with type: ',&
               A20)
 1035 format(' ERROR: no reserved keyword expected: ', A20)
 1040 format(' WARNING: Input ',A,' nr:',I5,' with name: ',A20,&
               ' is not a valid ID, data ignored')
 1050 format(' ERROR: string is no valid item ID: ',A)
 1060 format(' ERROR: number: ',I5,' is not a valid item number !')
 1070 format(' ERROR: multiplication is only allowed in USEFOR',&
               ' context !')
 1080 format(' ERROR: arithmetics should be separated by items !')
 1090 format(' Subtracted by item nr: ',I6,' Name: ',A20)
 1092 format(' Minimum value is item nr: ',I6,' Name: ',A20)
 1094 format(' Maximum value is item nr: ',I6,' Name: ',A20)
 1100 format(' Summed with item nr: ',I6,' Name: ',A20)
 1110 format(' Divided by item nr: ',I6,' Name: ',A20)
 1120 format(' Multiplied by item nr: ',I6,' Name: ',A20)
 1130 format(' Multiplied by local substitution: ',A20)
 1140 format(' Divided by local substitution: ',A20)
 1150 format(' Summed with local substitution: ',A20)
 1160 format(' Subtracted by local substitution: ',A20)
 1162 format(' Minimum value is local substitution: ',A20)
 1164 format(' Maximum value is local substitution: ',A20)
 1169 format(' Substituted by: ',E15.6)
 1170 format(' Multiplied by: ',E15.6)
 1180 format(' Divided by: ',E15.6)
 1190 format(' Summed with: ',E15.6)
 1200 format(' Subtracted by: ',E15.6)
 1210 format(' Minimum value is: ',E15.6)
 1220 format(' Maximum value is: ',E15.6)
!
    end subroutine dlwq5b

end module m_dlwq5b
