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
module m_delwaq_statistical_process
    use m_waq_precision, only : int_wp, real_wp
    use m_string_utils, only : string_equals, index_in_array
    use m_setqtl
    use m_setprc
    use m_setgeo
    use m_setdsc
    use m_setdpt
    use m_setday
    use m_rdstat
    use m_error_status

    implicit none

contains


    SUBROUTINE setup_statistical(LUNREP, NPOS, &
            CCHAR, &
            ILUN, LCH, &
            LSTACK, output_verbose_level, &
            is_date_format, is_yyddhh_format, &
            StatProcesDef, AllItems, &
            status)

        !! Defines process steering for statistical output processing
        !! This routine deals with the set up of statistical processes

        use timers
        use ProcesSet

        implicit none

        !     kind           function         name                Descriptipon

        integer(kind = int_wp), intent(in) :: lunrep            !! unit nr of output report file
        integer(kind = int_wp), intent(in) :: npos              !! significant line length of input file
        character(1), intent(in) :: cchar            !! comment character
        integer(kind = int_wp), intent(inout) :: ilun (*)          !! unitnumber include stack
        character(*), intent(inout) :: lch  (*)         !! filename include stack for input
        integer(kind = int_wp), intent(in) :: lstack            !! include file stack size
        integer(kind = int_wp), intent(out) :: output_verbose_level            !! flag for more or less output
        logical, intent(in) :: is_date_format           !! 'date'-format 1st timescale
        logical, intent(in) :: is_yyddhh_format           !! 'date'-format (F;ddmmhhss,T;yydddhh)
        type(ProcesPropColl) :: StatProcesDef    !! the statistical proces definition
        type(ItemPropColl) :: AllItems         !! all items of the proces system

        type(error_status), intent(inout) :: status !< current error status

        ! local

        type(ProcesProp) :: aProcesProp !! one statistical proces definition

        INTEGER(kind = int_wp), POINTER :: STA_NO_IN(:)
        INTEGER(kind = int_wp), POINTER :: STA_NO_OUT(:)
        INTEGER(kind = int_wp), POINTER :: STA_SWITR(:)
        CHARACTER*20, POINTER :: STA_IN_NAM(:)
        CHARACTER*50, POINTER :: STA_IN_TXT(:)
        REAL(kind = real_wp), POINTER :: STA_IN_DEF(:)
        CHARACTER*20, POINTER :: STA_OUT_NAM(:)
        CHARACTER*50, POINTER :: STA_OUT_TXT(:)
        CHARACTER*10, POINTER :: STA_MODNAM(:)

        INTEGER(kind = int_wp) :: NKEY, input_file_start_position, NSPROC
        CHARACTER*20, POINTER :: KEYNAM(:)
        CHARACTER*20, POINTER :: KEYVAL(:)
        CHARACTER*20, ALLOCATABLE :: KEYNAM2(:)
        CHARACTER*20, ALLOCATABLE :: KEYVAL2(:)
        INTEGER(kind = int_wp), POINTER :: NOKEY(:)

        INTEGER(kind = int_wp) :: NPERIOD
        CHARACTER*20, POINTER :: PERNAM(:)
        CHARACTER*20, POINTER :: PERSFX(:)
        INTEGER(kind = int_wp), POINTER :: PSTART(:)
        INTEGER(kind = int_wp), POINTER :: PSTOP(:)

        INTEGER(kind = int_wp) :: NSVAI, NSVAO, ISWITR
        CHARACTER*20, POINTER :: VAINAM(:)
        CHARACTER*50, POINTER :: VAITXT(:)
        REAL(kind = real_wp), POINTER :: VAIDEF(:)
        CHARACTER*20, POINTER :: VAONAM(:)
        CHARACTER*50, POINTER :: VAOTXT(:)

        INTEGER(kind = int_wp) :: IKSTAT, ISTAT, IKEY, IFOUND, IERR_ALLOC, &
                NOSTAT, ISPROC, IPERIOD, IRET, IHULP1, &
                IHULP2
        CHARACTER*20 :: KEY
        CHARACTER*4 :: CH4
        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("setup_statistical", ithndl)

        WRITE(LUNREP, 2000)
        input_file_start_position = 0
        CALL RDSTAT (LUNREP, input_file_start_position, NPOS, CCHAR, &
                ILUN, LCH, LSTACK, output_verbose_level, is_date_format, &
                is_yyddhh_format, status, NOSTAT, NKEY, NOKEY, &
                KEYNAM, KEYVAL, NPERIOD, PERNAM, PERSFX, &
                PSTART, PSTOP)

        ! set number of statistical processes (some once , some per period )

        IKSTAT = 1
        NSPROC = 0
        DO ISTAT = 1, NOSTAT
            KEY = 'OUTPUT-OPERATION'
            IKEY = index_in_array(KEY, KEYNAM(IKSTAT:IKSTAT + NOKEY(ISTAT)))
            IF (IKEY > 0) THEN
                IKEY = IKSTAT + IKEY - 1
                KEY = 'STADAY'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    NSPROC = NSPROC + 1
                    GOTO 10
                ENDIF
                KEY = 'STADPT'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    NSPROC = NSPROC + 1
                    GOTO 10
                ENDIF
                KEY = 'STADSC'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    NSPROC = NSPROC + NPERIOD
                    GOTO 10
                ENDIF
                KEY = 'STAGEO'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    NSPROC = NSPROC + NPERIOD
                    GOTO 10
                ENDIF
                KEY = 'STAPRC'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    NSPROC = NSPROC + NPERIOD
                    GOTO 10
                ENDIF
                KEY = 'STAQTL'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    NSPROC = NSPROC + NPERIOD
                    GOTO 10
                ENDIF
                10       CONTINUE
            ENDIF
            IKSTAT = IKSTAT + NOKEY(ISTAT)
        ENDDO

        ALLOCATE (STA_NO_IN(NSPROC))
        ALLOCATE(STA_NO_OUT(NSPROC))
        ALLOCATE(STA_SWITR(NSPROC))
        ALLOCATE(STA_MODNAM(NSPROC))

        ! emergency solution
        ALLOCATE(KEYNAM2(NKEY), KEYVAL2(NKEY))
        KEYNAM2 = KEYNAM
        KEYVAL2 = KEYVAL

        ! report on periods

        WRITE(LUNREP, '(A,I6)') 'Number of periods defined:', NPERIOD
        WRITE(LUNREP, *)
        DO IPERIOD = 1, NPERIOD
            WRITE(LUNREP, '(3A)') 'PERIOD [', PERNAM(IPERIOD), ']'
            WRITE(LUNREP, '(3A)') 'SUFFIX [', PERSFX(IPERIOD), ']'
            IF (is_date_format) THEN
                IHULP1 = PSTART(IPERIOD)
                IHULP2 = PSTOP(IPERIOD)
                WRITE(LUNREP, 2020)  IHULP1 / 31536000, MOD(IHULP1, 31536000) / 86400, &
                        MOD(IHULP1, 86400) / 3600, MOD(IHULP1, 3600) / 60, &
                        MOD(IHULP1, 60), &
                        IHULP2 / 31536000, MOD(IHULP2, 31536000) / 86400, &
                        MOD(IHULP2, 86400) / 3600, MOD(IHULP2, 3600) / 60, &
                        MOD(IHULP2, 60)
            ELSE
                WRITE(LUNREP, 2030) PSTART(IPERIOD), PSTOP(IPERIOD)
            ENDIF
        ENDDO

        ! loop over the output operations, setup administration and report
        IKSTAT = 1
        ISPROC = 0
        DO ISTAT = 1, NOSTAT
            WRITE(LUNREP, *)
            DO IKEY = 1, NOKEY(ISTAT)
                WRITE(LUNREP, '(A,1X,A)') KEYNAM(IKSTAT + IKEY - 1), &
                        KEYVAL(IKSTAT + IKEY - 1)
            ENDDO
            KEY = 'OUTPUT-OPERATION'
            IKEY = index_in_array(KEY, KEYNAM(IKSTAT:IKSTAT + NOKEY(ISTAT)))
            IF (IKEY > 0) THEN
                IKEY = IKSTAT + IKEY - 1
                KEY = 'STADAY'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    ISPROC = ISPROC + 1
                    CALL SETDAY (LUNREP, NOKEY(ISTAT), &
                            KEYNAM2(IKSTAT), KEYVAL2(IKSTAT), &
                            is_date_format, is_yyddhh_format, &
                            ISPROC, aProcesProp, &
                            AllItems, status)

                    iret = ProcesPropCollAdd(StatProcesDef, aProcesProp)
                    GOTO 100
                ENDIF
                KEY = 'STADPT'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    ISPROC = ISPROC + 1
                    CALL SETDPT (LUNREP, NOKEY(ISTAT), &
                            KEYNAM2(IKSTAT), KEYVAL2(IKSTAT), &
                            ISPROC, aProcesProp, &
                            AllItems, status)
                    iret = ProcesPropCollAdd(StatProcesDef, aProcesProp)
                    GOTO 100
                ENDIF
                KEY = 'STADSC'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    DO IPERIOD = 1, NPERIOD
                        WRITE(LUNREP, '(3A)') 'For period [', PERNAM(IPERIOD), ']:'
                        ISPROC = ISPROC + 1
                        CALL SETDSC (LUNREP, NOKEY(ISTAT), &
                                KEYNAM2(IKSTAT), KEYVAL2(IKSTAT), &
                                PERNAM(IPERIOD), PERSFX(IPERIOD), &
                                PSTART(IPERIOD), PSTOP(IPERIOD), &
                                ISPROC, aProcesProp, &
                                AllItems, status)
                        iret = ProcesPropCollAdd(StatProcesDef, aProcesProp)
                    ENDDO
                    GOTO 100
                ENDIF
                KEY = 'STAGEO'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    DO IPERIOD = 1, NPERIOD
                        ISPROC = ISPROC + 1
                        WRITE(LUNREP, '(3A)') 'For period [', PERNAM(IPERIOD), ']:'
                        CALL SETGEO (LUNREP, NOKEY(ISTAT), &
                                KEYNAM2(IKSTAT), KEYVAL2(IKSTAT), &
                                PERNAM(IPERIOD), PERSFX(IPERIOD), &
                                PSTART(IPERIOD), PSTOP(IPERIOD), &
                                ISPROC, aProcesProp, &
                                AllItems, status)
                        iret = ProcesPropCollAdd(StatProcesDef, aProcesProp)
                    ENDDO
                    GOTO 100
                ENDIF
                KEY = 'STAPRC'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    DO IPERIOD = 1, NPERIOD
                        ISPROC = ISPROC + 1
                        WRITE(LUNREP, '(3A)') 'For period [', PERNAM(IPERIOD), ']:'
                        CALL SETPRC (LUNREP, NOKEY(ISTAT), &
                                KEYNAM2(IKSTAT), KEYVAL2(IKSTAT), &
                                PERNAM(IPERIOD), PERSFX(IPERIOD), &
                                PSTART(IPERIOD), PSTOP(IPERIOD), &
                                ISPROC, aProcesProp, &
                                AllItems, status)
                        iret = ProcesPropCollAdd(StatProcesDef, aProcesProp)
                    ENDDO
                    GOTO 100
                ENDIF
                KEY = 'STAQTL'
                if (string_equals(KEY, KEYVAL(IKEY))) then
                    DO IPERIOD = 1, NPERIOD
                        ISPROC = ISPROC + 1
                        WRITE(LUNREP, '(3A)') 'For period [', PERNAM(IPERIOD), ']:'
                        CALL SETQTL (LUNREP, NOKEY(ISTAT), &
                                KEYNAM2(IKSTAT), KEYVAL2(IKSTAT), &
                                PERNAM(IPERIOD), PERSFX(IPERIOD), &
                                PSTART(IPERIOD), PSTOP(IPERIOD), &
                                ISPROC, aProcesProp, &
                                AllItems, status)
                        iret = ProcesPropCollAdd(StatProcesDef, aProcesProp)
                    ENDDO
                    GOTO 100
                ENDIF
                WRITE(LUNREP, *) 'ERROR unrecognised operation:', KEYVAL(IKEY:)
                call status%increase_error_count()
                100       CONTINUE
            ELSE
                WRITE(LUNREP, *) 'ERROR no operation defined for output-operation'
                call status%increase_error_count()
            ENDIF
            WRITE(LUNREP, '(A)') 'END-OUTPUT-OPERATION'
            IKSTAT = IKSTAT + NOKEY(ISTAT)
        ENDDO

        DEALLOCATE (KEYNAM, KEYVAL, NOKEY, STAT = IERR_ALLOC)
        DEALLOCATE (KEYNAM2, KEYVAL2, STAT = IERR_ALLOC)
        NSPROC = ISPROC

        500 CONTINUE
        WRITE (LUNREP, 3000) 10

        if (timon) call timstop(ithndl)
        RETURN
        2000 FORMAT (/, ' Output operations')
        2020 FORMAT (' Start of period :', I2, 'Y-', I3, 'D-', I2, 'H-', I2, &
                'M-', I2, 'S.', /' End of period   :', I2, 'Y-', I3, 'D-', I2, 'H-', I2, &
                'M-', I2, 'S.')
        2030 FORMAT (' Start of period :        ', I10, ' End of period   :        ', I10)
        3000 FORMAT (/1X, 59('*'), ' B L O C K -', I2, ' ', 5('*')/)
    END subroutine setup_statistical

end module m_delwaq_statistical_process
