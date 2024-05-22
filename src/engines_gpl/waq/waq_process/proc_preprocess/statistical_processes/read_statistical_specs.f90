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
module m_rdstat
    use m_waq_precision
    use m_string_utils, only : index_in_array, string_equals
    use m_error_status

    implicit none

contains


    SUBROUTINE RDSTAT (LUNREP, input_file_start_position, NPOS, CCHAR, &
            ILUN, LCH, LSTACK, output_verbose_level, is_date_format, &
            is_yyddhh_format, status, NOSTAT, NKEY, NOKEY, &
            KEYNAM, KEYVAL, NPERIOD, PERNAM, PERSFX, &
            PSTART, PSTOP)
        ! Reads statistical output spec. block 10
        !
        !     SUBROUTINES CALLED : RDTOK1 tokenized data file reading
        !
        !     LOGICAL UNITS      : input_file = unit formatted inputfile
        !                          LUNREP= unit formatted outputfile
        !
        !     PARAMETERS         :
        !
        !     NAME    KIND     LENGTH      FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     input_file_start_position   INTEGER(kind=int_wp) ::1           IN/OUT  position on input record
        !     NPOS    INTEGER(kind=int_wp) ::1           INPUT   length of input record
        !     CCHAR   CHAR*1   1           INPUT   comment character
        !     VERSION REAL(kind=real_wp) ::1           INPUT   program version number
        !     ILUN    INTEGER(kind=int_wp) ::LSTACK      IN/OUT  unit number stack
        !     LCH     CHAR*(*) LSTACK      IN/OUT  Filename stack
        !     LSTACK  INTEGER(kind=int_wp) ::1           INPUT   size of the stack
        !     output_verbose_level  INTEGER(kind=int_wp) ::1           INPUT   output file option
        !     is_date_format  LOGICAL  1           INPUT   'date'-format 1st timescale
        !     is_yyddhh_format  LOGICAL  1           INPUT   'date'-format (F;ddmmhhss,T;yydddhh)
        !     NOSTAT  INTEGER(kind=int_wp) ::1           OUTPUT  number of statistical processes
        !     NKEY    INTEGER(kind=int_wp) ::1           OUTPUT  total number of keywords
        !     NOKEY   INTEGER(kind=int_wp) ::NOSTAT      OUTPUT  number of keywords per stat. proc.
        !     KEYNAM  CHAR*20  NKEY        OUTPUT  names of the keywords read
        !     KEYVAL  CHAR*20  NKEY        OUTPUT  values of the keywords
        !     NPERIOD INTEGER(kind=int_wp) ::1           OUTPUT  number of periods
        !     PERNAM  CHAR*20  NPERIOD     OUTPUT  period name
        !     PERSFX  CHAR*20  NPERIOD     OUTPUT  period suffix
        !     PSTART  INTEGER(kind=int_wp) ::NPERIOD     OUTPUT  period start
        !     PSTOP   INTEGER(kind=int_wp) ::NPERIOD     OUTPUT  period stop
        !
        use timers
        use m_array_manipulation, only : resize_integer_array, resize_character_array
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time
        use rd_token, only : rdtok1

        implicit none

        INTEGER(kind = int_wp) :: LUNREP, input_file_start_position, NPOS, LSTACK, output_verbose_level, &
                NOSTAT, NKEY
        LOGICAL :: is_date_format, is_yyddhh_format
        INTEGER(kind = int_wp) :: ILUN(*)
        character(len=*) :: LCH  (*)
        character(len=1) :: CCHAR
        character(len=20), POINTER :: KEYNAM(:)
        character(len=20), POINTER :: KEYVAL(:)
        INTEGER(kind = int_wp), POINTER :: NOKEY(:)
        INTEGER(kind = int_wp) :: NPERIOD
        character(len=20), POINTER :: PERNAM(:)
        character(len=20), POINTER :: PERSFX(:)
        INTEGER(kind = int_wp), POINTER :: PSTART(:)
        INTEGER(kind = int_wp), POINTER :: PSTOP(:)

        type(error_status), intent(inout) :: status !< current error status

        ! Local

        INTEGER(kind = int_wp) :: NPKEY, NKEYPER, NKEYPAR, IPAR
        PARAMETER   (NPKEY = 4)
        PARAMETER   (NKEYPER = 4)
        PARAMETER   (NKEYPAR = 3)
        character(len=20) :: KEY, KEYS(NPKEY)
        character(len=20) :: KEYPER(NKEYPER)
        character(len=20) :: KEYPAR(NKEYPAR)
        character(len=20) :: KNAM, CDUMMY
        character(len=20) :: KVAL
        REAL(kind = real_wp) :: ADUMMY
        INTEGER(kind = int_wp) :: IDUMMY, IERR2, IKEY, ITYPE, MAXKEY, &
                MAXSTAT, VERSTAT, MINSTAT, IKEY2, ITSTRT, &
                ITSTOP, MPERIOD, IKEY3
        integer(kind = int_wp) :: istart, istop
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("rdstat", ithndl)

        NOSTAT = 0
        NKEY = 0
        MAXSTAT = 10
        MAXKEY = 50
        ALLOCATE(NOKEY(MAXSTAT))
        ALLOCATE(KEYNAM(MAXKEY))
        ALLOCATE(KEYVAL(MAXKEY))
        NPERIOD = 0
        MPERIOD = 2
        ALLOCATE(PERNAM(MPERIOD))
        ALLOCATE(PERSFX(MPERIOD))
        ALLOCATE(PSTART(MPERIOD))
        ALLOCATE(PSTOP (MPERIOD))

        KEYS(1) = 'VERSION'
        KEYS(2) = 'MINOR'
        KEYS(3) = 'PERIOD'
        KEYS(4) = 'OUTPUT-OPERATION'

        KEYPAR(1) = 'real-parameter'
        KEYPAR(2) = 'time-parameter'
        KEYPAR(3) = 'logical-parameter'

        CALL convert_string_to_time_offset ('START               ', itstrt, .FALSE., .FALSE., IERR2)
        CALL convert_string_to_time_offset ('STOP                ', itstop, .FALSE., .FALSE., IERR2)

        100 CONTINUE
        ITYPE = 0
        CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                input_file_start_position, NPOS, KNAM, IDUMMY, ADUMMY, &
                ITYPE, IERR2)

        IF (IERR2 == 2) GOTO 500
        IF (IERR2 == 3 .AND. NOSTAT == 0) GOTO 500
        IF (IERR2 == 3) THEN
            WRITE(LUNREP, *) 'ERROR : closing delimiter block 10 not found'
            call status%increase_error_count()
            GOTO 500
        ENDIF
        IF (IERR2 /= 0) THEN
            WRITE(LUNREP, *) 'ERROR : reading block 10'
            call status%increase_error_count()
            GOTO 500
        ENDIF

        IKEY = index_in_array(KNAM, KEYS)
        IF (IKEY <= 0) THEN
            WRITE(LUNREP, *) 'ERROR : unexpected keyword found'
            WRITE(LUNREP, *) 'found    :', KNAM
            WRITE(LUNREP, *) 'expected : OUTPUT-OPERATION'
            call status%increase_error_count()
            GOTO 100
        ELSEIF (IKEY == 1) THEN

            ! version

            ITYPE = 2
            CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                    input_file_start_position, NPOS, CDUMMY, VERSTAT, ADUMMY, &
                    ITYPE, IERR2)
            IF (IERR2 /= 0) GOTO 900
        ELSEIF (IKEY == 2) THEN

            ! minor

            ITYPE = 2
            CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                    input_file_start_position, NPOS, CDUMMY, MINSTAT, ADUMMY, &
                    ITYPE, IERR2)
            IF (IERR2 /= 0) GOTO 900
        ELSEIF (IKEY == 3) THEN

            ! period

            NPERIOD = NPERIOD + 1
            IF (NPERIOD > MPERIOD) THEN
                MPERIOD = 2 * MPERIOD
                CALL resize_character_array(PERNAM, MPERIOD, NPERIOD - 1)
                CALL resize_character_array(PERSFX, MPERIOD, NPERIOD - 1)
                CALL resize_integer_array(PSTART, MPERIOD, NPERIOD - 1)
                CALL resize_integer_array(PSTOP, MPERIOD, NPERIOD - 1)
            ENDIF
            ITYPE = 0
            CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                    input_file_start_position, NPOS, KNAM, IDUMMY, ADUMMY, &
                    ITYPE, IERR2)
            IF (IERR2 /= 0) GOTO 900
            PERNAM(NPERIOD) = KNAM
            KEY = 'START'
            CALL convert_string_to_time_offset (KEY, istart, .FALSE., .FALSE., IERR2)
            PSTART(NPERIOD) = istart
            KEY = 'STOP'
            CALL convert_string_to_time_offset (KEY, istop, .FALSE., .FALSE., IERR2)
            PSTOP (NPERIOD) = istop
            WRITE(PERSFX(NPERIOD), '(''period'',i2.2)') NPERIOD

            ! suffix,start, stop, more ?

            200       CONTINUE
            ITYPE = 0
            CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                    input_file_start_position, NPOS, KNAM, IDUMMY, ADUMMY, &
                    ITYPE, IERR2)
            IF (IERR2 /= 0) GOTO 900

            KEYPER(1) = 'SUFFIX'
            KEYPER(2) = 'START-TIME'
            KEYPER(3) = 'STOP-TIME'
            KEYPER(4) = 'END-PERIOD'
            IKEY2 = index_in_array(KNAM, KEYPER)
            IF (IKEY2 <= 0) THEN
                WRITE(LUNREP, *) 'ERROR : unexpected keyword found'
                WRITE(LUNREP, *) 'found    :', KNAM
                call status%increase_error_count()
                GOTO 200
            ELSEIF (IKEY2 == 1) THEN

                !  SUFFIX

                ITYPE = 0
                CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                        input_file_start_position, NPOS, KNAM, IDUMMY, ADUMMY, &
                        ITYPE, IERR2)
                IF (IERR2 /= 0) GOTO 900
                PERSFX(NPERIOD) = KNAM

            ELSEIF (IKEY2 == 2) THEN

                ! START-TIME

                ITYPE = -3
                CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                        input_file_start_position, NPOS, KNAM, IDUMMY, ADUMMY, &
                        ITYPE, IERR2)
                istart = IDUMMY
                IF (IERR2 /= 0) GOTO 900
                IF (ITYPE == 1) THEN
                    CALL convert_string_to_time_offset (KNAM, istart, .FALSE., .FALSE., IERR2)
                    IF (IERR2 /= 0) THEN
                        WRITE(LUNREP, *)'ERROR interpreting start time:', KNAM
                        call status%increase_error_count()
                    ENDIF
                ELSE
                    call convert_relative_time (istart, 1, is_date_format, is_yyddhh_format)
                ENDIF
                PSTART(NPERIOD) = max(itstrt, istart)

            ELSEIF (IKEY2 == 3) THEN

                !  STOP-TIME

                ITYPE = -3
                CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                        input_file_start_position, NPOS, KNAM, IDUMMY, ADUMMY, &
                        ITYPE, IERR2)
                istop = IDUMMY
                IF (IERR2 /= 0) GOTO 900
                IF (ITYPE == 1) THEN
                    CALL convert_string_to_time_offset (KNAM, istop, .FALSE., .FALSE., IERR2)
                    IF (IERR2 /= 0) THEN
                        WRITE(LUNREP, *)'ERROR interpreting stop time:', KNAM
                        call status%increase_error_count()
                    ENDIF
                ELSE
                    call convert_relative_time (istop, 1, is_date_format, is_yyddhh_format)
                ENDIF
                PSTOP(NPERIOD) = min(itstop, istop)

            ELSEIF (IKEY2 == 4) THEN

                ! END-PERIOD

                GOTO 100

            ENDIF

            GOTO 200

        ELSEIF (IKEY == 4) THEN

            ! statistical operation

            NOSTAT = NOSTAT + 1
            IF (NOSTAT > MAXSTAT) THEN
                MAXSTAT = 2 * MAXSTAT
                CALL resize_integer_array(NOKEY, MAXSTAT, NOSTAT - 1)
            ENDIF
            NOKEY(NOSTAT) = 0

            300       CONTINUE

            ! check if it a parameter with extra key word real-parameter, time-parameter, logical-parameter, ?integer-parameter

            IPAR = index_in_array(KNAM, KEYPAR)
            IF (IPAR > 0) THEN

                ! get real KNAM

                ITYPE = 0
                CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                        input_file_start_position, NPOS, KNAM, IDUMMY, ADUMMY, &
                        ITYPE, IERR2)
                IF (IERR2 /= 0) GOTO 900

            ENDIF

            ITYPE = 0
            CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                    input_file_start_position, NPOS, KVAL, IDUMMY, ADUMMY, &
                    ITYPE, IERR2)
            IF (IERR2 /= 0) GOTO 900

            NOKEY(NOSTAT) = NOKEY(NOSTAT) + 1
            NKEY = NKEY + 1
            IF (NKEY > MAXKEY) THEN
                MAXKEY = 2 * MAXKEY
                CALL resize_character_array(KEYNAM, MAXKEY, NKEY - 1)
                CALL resize_character_array(KEYVAL, MAXKEY, NKEY - 1)
            ENDIF
            KEYNAM(NKEY) = KNAM
            KEYVAL(NKEY) = KVAL

            ITYPE = 0
            CALL RDTOK1 (LUNREP, ILUN, LCH, LSTACK, CCHAR, &
                    input_file_start_position, NPOS, KNAM, IDUMMY, ADUMMY, &
                    ITYPE, IERR2)
            IF (IERR2 /= 0) GOTO 900

            KEY = 'END-OUTPUT-OPERATION'
            if (.not. string_equals(KNAM, KEY)) then
                GOTO 300
            ENDIF
        ENDIF

        ! next keyword

        GOTO 100

        500 CONTINUE

        if (timon) call timstop(ithndl)
        RETURN

        900 CONTINUE
        IF (IERR2 == 3) THEN
            WRITE(LUNREP, *) 'ERROR : unexpected end of input file'
        ELSEIF (IERR2 == 2) THEN
            WRITE(LUNREP, *) 'ERROR : unexpected end of block 10'
        ELSE
            WRITE(LUNREP, *) 'ERROR : reading block 10'
        ENDIF
        call status%increase_error_count()
        if (timon) call timstop(ithndl)
        RETURN

    END SUBROUTINE RDSTAT

end module m_rdstat
