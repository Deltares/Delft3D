!!  Copyright (C)  Stichting Deltares, 2012-2024.
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

module rd_token

    implicit none

    ! publicly accessable ( this allows for mixed use with direct calls to rdtok1/2 )
    integer, parameter :: lchmax = 255    !< string length file name variables
    integer, parameter :: lstack = 6      !< size include files stack
    integer :: ilun(lstack)    !< unit numbers include files stack
    character(lchmax) :: lch (lstack)    !< file names include files stack
    character(1) :: cchar           !< comment character
    integer :: file_unit           !< unit number output file
    integer :: iposr           !< location on the record
    integer :: npos            !< record length
    logical :: push            !< use previous token if true

    integer, private :: type              ! type to be expected from rdtok1/2
    character(lchmax), private :: cdummy  ! character dummy argument
    integer(kind = 8), private :: idummy  ! integer dummy argument
    real(kind = 8), private :: rdummy     ! real dummy argument

    interface gettoken
        module procedure get_char_tok
        module procedure get_char_untileol_tok
        module procedure get_int_tok
        module procedure get_int8_tok
        module procedure get_real_tok
        module procedure get_double_tok
        module procedure get_nochar_tok
        module procedure get_noreal_tok
        module procedure get_all_tok
    end interface

    private
    public :: force_include_file, puttoken, gettoken, gettok, rdtok1
    public :: ilun, lch, cchar, file_unit, iposr, npos, push, lchmax, lstack

contains

    function force_include_file (achar) result (ierr)
        !! force the opening of a new include file
        character(len = *), intent(in) :: achar
        integer(4) :: ierr

        type = -999
        call rdtok2(file_unit, ilun, lch, lstack, cchar, iposr, npos, achar, idummy, rdummy, type, ierr)
    end function force_include_file

    function get_char_tok(achar, ierr2) result (ierr)
        !! get a character string
        character(len = *), intent(out) :: achar
        integer   (4), intent(inout) :: ierr2
        integer   (4)                   ierr

        if (push) then
            achar = cdummy
            ierr2 = 0                   ! this is always possible
            push = .false.
        else
            type = 1
            call rdtok2(file_unit, ilun, lch, lstack, cchar, iposr, npos, cdummy, idummy, rdummy, type, ierr2)
            achar = cdummy
        endif
        ierr = ierr2

    end function get_char_tok

    function get_char_untileol_tok(achar, untileol, ierr2) result (ierr)
        !! get a character string until the end of the line

        character*(*), intent(out) :: achar
        logical, intent(in) :: untileol
        integer   (4), intent(inout) :: ierr2
        integer   (4)                   ierr

        if (push) then
            achar = cdummy
            ierr2 = 0                   ! this is always possible
            push = .false.
        else
            type = 4
            call rdtok2 (file_unit, ilun, lch, lstack, cchar, &
                    &                    iposr, npos, cdummy, idummy, rdummy, &
                    &                    type, ierr2)
            achar = cdummy
        endif
        ierr = ierr2

    end function get_char_untileol_tok

    function get_int_tok(anint, ierr2) result (ierr)
        !! get an integer

        integer   (4), intent(out) :: anint
        integer   (4), intent(inout) :: ierr2
        integer   (4)                   ierr

        if (push) then
            if (type /= 2) then
                ierr2 = 1                ! there is no integer on the stack
            else
                anint = idummy
                ierr2 = 0
            endif
            push = .false.
        else
            type = 2
            call rdtok2(file_unit, ilun, lch, lstack, cchar, iposr, npos, cdummy, idummy, rdummy, type, ierr2)
            anint = idummy
            rdummy = idummy
        endif
        ierr = ierr2

    end function get_int_tok

    function get_int8_tok(anint8, ierr2) result (ierr)
        !! get an integer
        integer   (8), intent(out) :: anint8
        integer   (4), intent(inout) :: ierr2
        integer   (4)                   ierr

        if (push) then
            if (type /= 2) then
                ierr2 = 1                ! there is no integer on the stack
            else
                anint8 = idummy
                ierr2 = 0
            endif
            push = .false.
        else
            type = 2
            call rdtok2 (file_unit, ilun, lch, lstack, cchar, iposr, npos, cdummy, idummy, rdummy, type, ierr2)
            anint8 = idummy
            rdummy = idummy
        endif
        ierr = ierr2

    end function get_int8_tok

    function get_real_tok(areal, ierr2) result (ierr)
        !! get a real

        real      (4), intent(out) :: areal
        integer   (4), intent(inout) :: ierr2
        integer                         ierr

        if (push) then
            if (type /= 2 .and. type /= 3) then
                ierr2 = 1                ! there is no number on the stack
            else
                areal = real(rdummy)
                ierr2 = 0
            endif
            push = .false.
        else
            type = 3
            call rdtok2 (file_unit, ilun, lch, lstack, cchar, iposr, npos, cdummy, idummy, rdummy, type, ierr2)
            areal = real(rdummy)
        endif
        ierr = ierr2

    end function get_real_tok

    function get_double_tok(adouble, ierr2) result (ierr)

        real      (8), intent(out) :: adouble
        integer   (4), intent(inout) :: ierr2
        integer                         ierr

        if (push) then
            if (type /= 2 .and. type /= 3) then
                ierr2 = 1                ! there is no number on the stack
            else
                adouble = rdummy
                ierr2 = 0
            endif
            push = .false.
        else
            type = 3
            call rdtok2 (file_unit, ilun, lch, lstack, cchar, iposr, npos, cdummy, idummy, rdummy, type, ierr2)
            adouble = rdummy
        endif
        ierr = ierr2

    end function get_double_tok

    function get_nochar_tok(anint, areal, itype, ierr2) result (ierr)
        ! get a number

        integer   (4), intent(out) :: anint
        real      (4), intent(out) :: areal
        integer   (4), intent(out) :: itype
        integer   (4), intent(inout) :: ierr2
        integer                         ierr

        if (push) then
            if (type /= 2 .and. type /= 3) then
                ierr2 = 1                ! there is no number on the stack
            else
                anint = idummy
                areal = rdummy
                itype = type
                ierr2 = 0
            endif
            push = .false.
        else
            type = -1
            call rdtok2(file_unit, ilun, lch, lstack, cchar, iposr, npos, cdummy, idummy, rdummy, type, ierr2)
            anint = idummy
            areal = rdummy
            itype = type
        endif
        ierr = ierr2

    end function get_nochar_tok

    function get_noreal_tok(achar, anint, itype, ierr2) result (ierr)
        !! get anything but a real

        character*(*), intent(out) :: achar
        integer   (4), intent(out) :: anint
        integer   (4), intent(out) :: itype
        integer   (4), intent(inout) :: ierr2
        integer                         ierr

        if (push) then
            if (type == 3) then
                ierr2 = 1           ! there is a real on the stack
            else
                achar = cdummy
                anint = idummy
                itype = type
                ierr2 = 0
            endif
            push = .false.
        else
            type = -3
            call rdtok2(file_unit, ilun, lch, lstack, cchar, iposr, npos, cdummy, idummy, rdummy, type, ierr2)
            achar = cdummy
            anint = idummy
            itype = type
        endif
        ierr = ierr2

    end function get_noreal_tok

    function get_all_tok(achar, anint, areal, itype, ierr2) result (ierr)
        ! get anything

        character*(*), intent(out) :: achar
        integer   (4), intent(out) :: anint
        real      (4), intent(out) :: areal
        integer   (4), intent(out) :: itype
        integer   (4), intent(inout) :: ierr2
        integer                         ierr

        if (push) then
            achar = cdummy
            anint = idummy
            areal = rdummy
            itype = type
            ierr2 = 0
            push = .false.
        else
            type = 0
            call rdtok2 (file_unit, ilun, lch, lstack, cchar, iposr, npos, cdummy, idummy, rdummy, type, ierr2)
            achar = cdummy
            anint = idummy
            areal = rdummy
            itype = type
        endif
        ierr = ierr2

    end function get_all_tok

    function puttoken(achar) result (ierr)

        character*(*), intent(in) :: achar
        integer :: ierr

        ierr = 0
        cdummy = achar
        type = 1
        push = .true.

    end function puttoken

    subroutine gettok (lun, line, str, int, reel, itype, iposl, iposr, iwidth, comc, &
            grpsep, lineno, ierr)
        !> Parses a token from the input line
        !>
        !>  - Gets the next token from line
        !>  - reads new line if no token left
        !>  - returns the type of the token and the value
        !>  - strips quotes from strings

        integer  (4), intent(in) :: lun               !< logical unitnumber input file
        character(*), intent(inout) :: line              !< space to store line to read
        character(*), intent(out) :: str               !< token if itype = 1
        integer  (8), intent(out) :: int               !< token if itype = 2
        real     (8), intent(out) :: reel              !< token if itype = 3
        integer  (4), intent(out) :: itype    !< type of the token ( 0 = none, 1 = character, 2 = integer, 3 = real )
        integer  (4), intent(out) :: iposl             !< left  position in LINE of token
        integer  (4), intent(out) :: iposr             !< right position in LINE of token
        integer  (4), intent(in) :: iwidth            !< Line length of input lines
        character(1), intent(in) :: comc              !< Comment character
        character(1), intent(in) :: grpsep            !< Group separation character
        integer  (4), intent(inout) :: lineno            !< line number in file
        integer  (4), intent(out) :: ierr              !< not 0 error during processing

        ! local variables
        character(1) limc        !   Limit char. for token, space ' and "
        logical        num         !   True if a number was detected
        logical        dot         !   True if a decimal point was detected
        logical        exp         !   True if an 'E','e','D' or 'd' detected
        integer        ntot        !   Repeat factor of a token
        integer        itypes      !   Stored previous type
        character(128) strs        !   Stored previous string
        integer  (8) ints        !   Stored previous integer
        real     (8) reels       !   Stored previous real
        integer        iposls      !   Stored previous left
        integer        iposrs      !   Stored previous right
        character(1) ctrlz       !   Ctrl_Z character
        character(1) chtab       !   Tab character
        character(1) ch_cr       !   Cariage return character
        integer        i, i2, j    !   loop counters
        integer        iexp, ihlp  !   help variables in the parsing process

        save
        data        ntot / 0 /

        chtab = char(9)
        ch_cr = char(13)
        ctrlz = char(26)
        str = ' '
        int = 0
        reel = 0.0

        ! see if we are in a repeat cycle
        if (ntot > 0) then
            itype = itypes
            str = strs
            int = ints
            reel = reels
            iposl = iposls
            iposr = iposrs
            ntot = ntot - 1
            return
        endif

        ! read an initialisation line
        num = .false.
        itype = 0
        ierr = 0
        10 if (iposr == 0) then
            read (lun, '(a)', end = 100, err = 110) line
            iposr = 0
            ntot = 0
            lineno = lineno + 1
        endif
        iposr = iposr + 1

        ! get a non-space character
        do i = iposr, iwidth
            iposl = i
            if (line(i:i) /= ' '   .and. line(i:i) /= ctrlz .and. line(i:i) /= ch_cr .and. &
                    line(i:i) /= chtab) goto 30
        enddo
        iposr = 0
        goto 10

        ! character found, see if it is a ' or "
        30 if (line(iposl:iposl) == comc) then
            iposr = 0
            goto 10
        endif
        limc = ' '
        i = iposl
        if (line(i:i) == '''' .or. line(i:i) == '"') then
            limc = line(i:i)
            itype = -1
        endif

        !  find the end of the token
        iposr = iposl
        do i = iposl + 1, iwidth
            if (limc /= ' ') then
                if (line(i:i) == limc) goto 50
            else
                if (line(i:i) == ' '   .or.&
                        line(i:i) == ctrlz .or.&
                        line(i:i) == ch_cr .or.&
                        line(i:i) == chtab .or.&
                        line(i:i) == comc) goto 50
            endif
            iposr = i
        enddo

        !  no delimiting quote found
        if (limc /= ' ') then
            str = line(iposl:iposr)
            ierr = -1
            iposr = iwidth
            ntot = 0
            goto 95
        endif

        !  detect what the token is
        ! a delimited string (with optionally embedded blanks)
        50 if (limc /= ' ') then
            iposl = iposl + 1
            str = line(iposl:iposr)
            iposr = iposr + 1
            goto 95
        endif

        ! see if it is an integer (it is allowed to start with a sign)
        str = line(iposl:iposr)
        do i = iposl, iposr
            j = ichar(line(i:i))
            if (i == iposl .and. (line(i:i) == '+' .or. line(i:i) == '-')) then
                if (iposl == iposr) goto 90
                cycle
            endif
            if (line(i:i) == '*' .and. ntot /= 0) then
                ntot = 0
                iposl = iposls
                goto 90
            endif
            if (line(i:i) == '*' .and. num) then
                read (line(iposl:i - 1), '(i40)') ntot
                if (ntot <= 0) goto 140
                iposls = iposl
                iposr = i
                ntot = ntot - 1
                goto 10
            endif
            if (j < ichar('0') .or. j > ichar('9')) goto 70
            num = .true.
        enddo
        read (line(iposl:iposr), '(i40)', err = 130) int
        itype = 2
        goto 95

        !     see if it is a real ( this one is the hardest )
        70 iexp = iposl - 1
        dot = .false.
        exp = .false.
        num = .false.
        do i = iposl, iposr
            limc = line(i:i)
            !  sign only at the beginning or right after exponent
            if (limc == '+' .or. limc == '-')  then
                if (iexp /= i - 1) goto 90
                cycle
            endif
            !  only one dot allowed, before the exponent
            if (limc == '.') then
                if (dot .or. exp) goto 90
                dot = .true.
                cycle
            endif
            !  only one exp allowed, after at least one numeric value
            if ((limc == 'e' .or. limc == 'E' .or.&
                    limc == 'd' .or. limc == 'D')) then
                if  (exp .or. .not. num) goto 90
                exp = .true.
                iexp = i
                do i2 = i + 1, iposr
                    j = ichar(line(i2:i2))
                    if (i2 == i + 1 .and. (line(i2:i2) == '+' .or.&
                            line(i2:i2) == '-')) cycle
                    if (j < ichar('0') .or. j > ichar('9')) goto 90
                enddo
                read (line(i + 1:iposr), '(i40)') ihlp

                if (ihlp > +30) then
                    ierr = -3
                    goto 90
                else
                    exit
                endif
            endif
            !  now only numerical values should remain
            j = ichar(limc)
            if (j < ichar('0') .or. j > ichar('9')) goto 90
            num = .true.
        enddo
        read (line(iposl:iposr), *, err = 120) reel
        itype = 3
        goto 95
        ! it apparently is a string
        90 str = line(iposl:iposr)
        if (line(iposl:iposl) == grpsep) then
            ierr = -2
        else
            itype = 1
        endif
        95 itypes = itype
        strs = str
        ints = int
        reels = reel
        iposls = iposl
        iposrs = iposr
        return

        100 ierr = 1
        return

        110 ierr = 2
        return

        ! real overflow of underflow
        120 ierr = -3
        return

        ! integer overflow of underflow
        130 ierr = -4
        return

        ! negative or zero repeats(*)
        140 ierr = -5

    end subroutine gettok

    subroutine rdtok1(file_unit, file_unit_list, file_names_list, num_files, comment_character, position_in_line, &
            num_line_characters, character_output, integer_output, real_output, expected_type, error_code)

        integer :: file_unit, file_unit_list(num_files), num_files, position_in_line, num_line_characters, &
                expected_type, error_code
        character(len = 1) :: comment_character
        character(len = *) :: file_names_list(num_files), character_output
        integer(kind = 8) :: integer_output_i
        real(kind = 8) :: real_output_i
        integer(kind = 4), intent(out) :: integer_output
        real, intent(out) :: real_output

        call rdtok2(file_unit, file_unit_list, file_names_list, num_files, comment_character, position_in_line, &
                num_line_characters, character_output, integer_output_i, real_output_i, expected_type, error_code)

        integer_output = integer_output_i
        real_output = real(real_output_i)

    end subroutine rdtok1

    subroutine rdtok2(file_unit, file_unit_list, file_names_list, num_files, comment_character, position_in_line, &
            num_line_characters, character_output, integer_output, real_output, expected_type, error_code)
        !! Reads a token and handles messages
        !! ERROR CODES:
        !!
        !! From GETTOK:From this routine:
        !!     -5 Negative or zero repeats(*) 1 General error/error reading etc.
        !!     -4 Integer overflow            1 General error/error reading etc.
        !!     -3 Exponent out of range       1 General error/error reading etc.
        !!     -2 Group separator found       2 End of data group / include stack exceeded
        !!     -1 No delimiting quote         1 General error/error reading etc.
        !!      0 Normal result               0 Normal end or (if other than expected)
        !!                                    4 Unexpected input data
        !!      1 End of file encountered     3 End of file * only if expected_type >0 at entry!
        !!      2 Read error encountered      1 General error/error reading etc.

        integer :: expected_type
        integer, intent(in) :: num_line_characters                 !! number of characters in the line
        integer, intent(inout) :: file_unit_list(num_files)
        integer, intent(in) :: num_files, file_unit
        character(len = 1), intent(in) :: comment_character
        character(len = *), intent(inout) :: file_names_list(num_files)
        character(len = *) :: character_output!! string to be delivered (for expected_type = -1:input!)
        integer(kind = 8), intent(out) :: integer_output
        real(kind = 8), intent(out) :: real_output
        integer, intent(inout) :: position_in_line                 !! start position on line

        integer :: file_unit_i, ioerr, itype, iposl, i, non_zero_file_unit
        integer, intent(out) :: error_code

        save
        character  line*1000, line2*80, chulp*1000
        integer, dimension(100) :: curline = 0 ! line number in the current file

        character(1) :: ctrlz = char(26)      !   tab character
        character(1) :: chtab = char(9)       !   cariage return character
        character(1) :: ch_cr = char(13)      !   ctrl_z character

        do i = 1, num_files
            if (file_unit_list(i) /= 0) then
                non_zero_file_unit = i
                file_unit_i = file_unit_list(i)
            endif
        end do

        ! Force the opening of a new include file - special case
        if (expected_type == -999) then
            write (file_unit, 1040) character_output
            non_zero_file_unit = non_zero_file_unit + 1
            open (newunit = file_unit_i, file = character_output, status = 'old', iostat = ioerr)
            if (ioerr > 0) then
                non_zero_file_unit = non_zero_file_unit - 1
                write (file_unit, 1050)
                error_code = 1
                goto 20
            else
                file_names_list(non_zero_file_unit) = character_output
                file_unit_list(non_zero_file_unit) = file_unit_i
                error_code = 0
                return
            endif
        endif

        ! Get the data
        chulp = ' '
        character_output = ' '
        10 error_code = 0

        if (expected_type == 4) then
            ! return a string from the first non space caracter until the end of the line (could be empty)
            do i = position_in_line + 1, num_line_characters
                iposl = i
                if (line(i:i) /= ' '   .and. line(i:i) /= ctrlz .and. &
                        line(i:i) /= ch_cr .and. line(i:i) /= chtab) exit
            enddo
            position_in_line = max(iposl, len_trim(line))
            character_output = line(iposl:position_in_line)
            iposl = 0
            position_in_line = 0
            return
        endif

        call gettok(file_unit_i, line, chulp, integer_output, real_output, itype, iposl, position_in_line, &
                num_line_characters, comment_character, '#', curline(non_zero_file_unit), error_code)
        character_output = chulp

        ! Deal with errors
        ! Negative or zero repeats(*)
        if (error_code == -5) then
            line2 = ' ERROR Negative or zero repeats at repeat sign (*)'
            call create_error_message(file_unit, file_unit_i, file_names_list(non_zero_file_unit), line, iposl, &
                    position_in_line, num_line_characters, line2, 0, itype, curline(non_zero_file_unit), 0)
            expected_type = 2
            error_code = 1
            goto 20
        endif

        ! Integer overflow
        if (error_code==-4 .and. (expected_type== 2 .or. expected_type== 0 .or. expected_type==-1 .or. &
                expected_type==-3)) then

            line2 = ' ERROR integer value too large or too small (OVERFLOW)'
            call create_error_message(file_unit, file_unit_i, file_names_list(non_zero_file_unit), line, iposl, &
                    position_in_line, num_line_characters, line2, 0, itype, curline(non_zero_file_unit), 0)
            expected_type = 2
            error_code = 1
            goto 20
        endif
        ! exponent out of range and real value allowed
        if (error_code==-3 .and. (expected_type== 3 .or. expected_type== 0 .or. expected_type==-1 .or. &
                expected_type==-2)) then
            line2 = ' ERROR exponent too positive or too negative'
            call create_error_message(file_unit, file_unit_i, file_names_list(non_zero_file_unit), line, iposl, &
                    position_in_line, num_line_characters, line2, 0, itype, curline(non_zero_file_unit), 0)
            expected_type = 3
            error_code = 1
            goto 20
        endif

        ! End of data block found
        if (error_code == -2) then
            if (.not. (expected_type == 0 .or. expected_type == 1)) then
                line2 = ' ERROR unexpected end of datagroup on unit'
                call create_error_message(file_unit, file_unit_i, file_names_list(non_zero_file_unit), ' ', 0, &
                        position_in_line, num_line_characters, line2, 0, itype, curline(non_zero_file_unit), 0)
                goto 20
            endif
            error_code = 2
            return
        endif

        if (error_code == -1) then
            line2 = ' No delimiting quote found !'
            call create_error_message(file_unit, file_unit_i, file_names_list(non_zero_file_unit), line, iposl, &
                    position_in_line, num_line_characters, line2, 0, itype, curline(non_zero_file_unit), 0)
            expected_type = 1
            error_code = 1
            goto 20
        endif

        if (error_code == 1) then
            if (expected_type > 0) then
                line2 = ' End of file on the input unit'
                if (non_zero_file_unit == 1) then
                    call create_error_message(file_unit, file_unit_i, file_names_list(non_zero_file_unit), ' ', iposl, &
                            position_in_line, num_line_characters, line2, 0, itype, curline(non_zero_file_unit), 0)
                end if
            endif
            error_code = 3
            goto 20
        end if

        if (error_code == 2) then
            line2 = ' ERROR reading from the input unit'
            call create_error_message(file_unit, file_unit_i, file_names_list(non_zero_file_unit), ' ', iposl, &
                    position_in_line, num_line_characters, line2, 0, itype, curline(non_zero_file_unit), 0)
            expected_type = 0
            error_code = 1
            goto 20
        endif

        ! write(*, *)  'Skip to a new file', ' ', chulp(:40)
        if (itype == 1 .and. chulp == 'INCLUDE') THEN
            if (non_zero_file_unit == num_files) then
                write (file_unit, 1020) num_files
                error_code = 1
                goto 20
            endif
            error_code = 0
            call gettok(file_unit_i, line, chulp, integer_output, real_output, itype, iposl, position_in_line, &
                    num_line_characters, comment_character, '#', curline(non_zero_file_unit), error_code)
            if (itype /= 1 .and. itype /= -1) then
                call create_error_message(file_unit, file_unit_i, file_names_list(non_zero_file_unit), line, iposl, &
                        position_in_line, num_line_characters, ' ', 1, itype, curline(non_zero_file_unit), 0)
                write (file_unit, 1030)
                error_code = 1
                goto 20
            endif
            write (file_unit, 1040) chulp
            non_zero_file_unit = non_zero_file_unit + 1
            open (newunit = file_unit_i, file = chulp, status = 'old', iostat = ioerr)
            if (ioerr > 0) then
                non_zero_file_unit = non_zero_file_unit - 1
                write (file_unit, 1050)
                error_code = 1
                goto 20
            else
                file_names_list (non_zero_file_unit) = chulp
                file_unit_list(non_zero_file_unit) = file_unit_i
                goto 10
            endif
        endif
        if ((expected_type == 2 .and. itype/=2) .or. &
                (expected_type == 3 .and. itype/=2 .and. itype/= 3) .or.&
                (expected_type == 1 .and. itype/=1 .and. itype/=-1) .or.&
                (expected_type ==-1 .and. itype==1) .or. &
                (expected_type ==-2 .and. itype==2) .or. &
                (expected_type ==-3 .and. itype==3)) then
            call create_error_message(file_unit, file_unit_i, file_names_list(non_zero_file_unit), line, iposl, &
                    position_in_line, num_line_characters, ' ', expected_type, itype, curline(non_zero_file_unit), 0)
            error_code = 4
        endif

        if (itype == 2) then
            real_output = integer_output
        endif
        if (itype == 1) then
        endif
        expected_type = itype
        if (expected_type < 0) expected_type = -expected_type

        if (error_code /= 0) goto 20
        return

        20 if (error_code == 3) then
            if (non_zero_file_unit > 1) then
                write (file_unit, 1000) file_names_list(non_zero_file_unit)(:78)
                close (file_unit_list(non_zero_file_unit))
                file_names_list (non_zero_file_unit) = ' '
                file_unit_list(non_zero_file_unit) = 0
                curline(non_zero_file_unit) = 0
                non_zero_file_unit = non_zero_file_unit - 1
                write (file_unit, 1010) file_names_list(non_zero_file_unit)(:78)
                file_unit_i = file_unit_list(non_zero_file_unit)
                position_in_line = 0
                goto 10
            else
                return
            endif
        else
            if (non_zero_file_unit > 1) then
                do i = non_zero_file_unit, 2, -1
                    write (file_unit, 1000) file_names_list(non_zero_file_unit)(:78)
                    close (file_unit_list(non_zero_file_unit))
                    file_names_list (non_zero_file_unit) = ' '
                    file_unit_list(non_zero_file_unit) = 0
                end do
                position_in_line = 0
            endif
        endif

        return

        1000 format (/' Closing file: ', A)
        1010 format (/' Continuing on file: ', A)
        1020 format (/' ERROR: nr of include stack levels (', I2, ') exceeded !')
        1030 format (/' Expected character string should be a valid ', ' ASCII filename !')
        1040 format (/' Including file: ', A)
        1050 format (/' ERROR: Include file does not exist !')
    end subroutine rdtok2

    subroutine create_error_message(file_unit, error_file_unit, file_name, line, left_position, right_position, &
            line_width, linerr, expected_type, found_type, line_number, error_code)
        !! Produces an error message

        !     LINE    CHAR*(*)      1     INPUT   line read on unit in error
        !     LINERR  CHAR*(*)      1     INPUT   text string to produce as error
        !     error_code    INTEGER       1     INPUT   error number returned from read

        integer :: file_unit, error_file_unit, left_position, right_position, line_width, expected_type, found_type, &
                line_number, error_code
        character*(*) :: file_name, line, linerr
        character :: line2*80

        integer :: i, ilim, j, iwidth_trim
        character(len = 10), parameter :: comment_character = 'character '
        character(len = 8), parameter :: CINT = 'integer '
        character(len = 5), parameter :: CREAL = 'real '

        WRITE (file_unit, *)

        ! file information when error_file_unit unequal to 0
        if (error_file_unit /= 0) then
            if (file_name /= ' ') then
                write (file_unit, 1000) error_file_unit, file_name
            else
                write (file_unit, 1010) error_file_unit
            endif
        endif

        ! line information when line is not blank
        if (line /= ' ') then
            write (file_unit, 1020) line_number, ' '
            if (left_position /= 0 .and. right_position /= 0) then
                iwidth_trim = len_trim(line)
                do j = 1, iwidth_trim, 80
                    write (file_unit, '(a)') line (j:min(j + 79, iwidth_trim))
                    line2 = ' '
                    if (left_position - j < 80) then
                        ilim = right_position
                        if (found_type == -1) ilim = ilim - 1
                        do i = max(left_position - j + 1, 1), min(ilim - j + 1, 80)
                            line2(i:i) = '^'
                        end do
                    endif
                    write (file_unit, '(a)') line2
                end do
            endif
        endif

        if (error_code /= 0) write (file_unit, 1030) error_code

        ! you can't always get what you want
        if (expected_type == 1) write (file_unit, 1040) comment_character
        if (expected_type == 2) write (file_unit, 1050) cint
        if (expected_type == 3) write (file_unit, 1040) creal
        if (found_type  == 1 .or. found_type == -1) write (file_unit, 1060) comment_character
        if (found_type  == 2) write (file_unit, 1070) cint
        if (found_type  == 3) write (file_unit, 1060) creal
        if (expected_type < 0) write (file_unit, 1080)

        ! something else to say ?
        if (linerr /= ' ') write (file_unit, '(a)') linerr

        return

        1000 format (' ERROR reading file on unit:', I4, ', filename: ', A)
        1010 format (' ERROR reading file on unit:', I4, ' !')
        1020 format (' Line on input file was (at line no. ', I0, '):', A)
        1030 format (' Error code from input processor was: ', I2)
        1040 format (' Expected was a ', A, '!')
        1050 format (' Expected was an ', A, '!')
        1060 format (' Detected was a ', A, '!')
        1070 format (' Detected was an ', A, '!')
        1080 format (' This item is NOT allowed at this location !')

    end subroutine create_error_message

end module rd_token
