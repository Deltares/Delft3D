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
module m_gettme
    use m_waq_precision

    implicit none

contains


    SUBROUTINE GETTME (FNAME, ITYPE, TIMDEF, MAXDEF, IPRDEP, &
            LOCDEP, MAXLST, TIMLST, ITMTYP, NRLST, &
            IERROR, OPTION)
        !
        !
        !     Deltares        MARINE & COASTAL MANAGEMENT
        !
        !     CREATED            : May '96  by L. Postma
        !
        !     MODIFIED           :
        !
        !     FUNCTION           : ODS GETTME routine for DELWAQ HIS-files
        !
        !     SUBROUTINES CALLED :
        !
        !     LOGICAL UNITS      :
        !
        !     PARAMETERS    :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     FNAME   CHAR*256   3        IN/LOC  Complete file name
        !     ITYPE   INTEGER    1        INPUT   File type
        !     TIMDEF  REAL*8   2,MAXDEF   INPUT   Wanted start and stop time
        !     MAXDEF  INTEGER    1        INPUT   Wanted time dimension
        !     IPRDEP  INTEGER    1        INPUT   Par code for dimensions
        !     LOCDEP  INTEGER    1        INPUT   Par code for dimensions
        !     MAXLST  INTEGER    1        INPUT   Dimension output arrays
        !     TIMLST  REAL*8   MAXLST     OUTPUT  List with times found
        !     ITMTYP  INTEGER  MAXLST     OUTPUT  List with time types
        !     NRLST   INTEGER    1        OUTPUT  Nr of times found
        !     IERROR  INTEGER    1        OUTPUT  Error code
        !     OPTION  CHAR*256   1        IN/OUT  For future use
        !
        !
        use time_module
        use m_string_manipulation, only : upper_case
        use m_open_waq_files
        use m_file_path_utils, only : extract_file_extension

        CHARACTER*256    FNAME (3), OPTION
        INTEGER(kind = int_wp) :: ITMTYP(*)
        real(kind = dp) :: TIMLST(*), TIMDEF(2, *), ATIME, OTIME, SECOND
        LOGICAL          SETALL
        !
        REAL(kind = real_wp), ALLOCATABLE :: RDATA(:)
        character*256 :: ext     ! file extension
        integer(kind = int_wp) :: extpos   ! position of extension
        integer(kind = int_wp) :: extlen   ! length of file extension
        logical :: mapfil  ! true if map file extension
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: NODUMP
        integer(kind = int_wp) :: k, I, NOTOT, NTT
        integer(kind = int_wp) :: ierror, nrlst, iprcod, iprtyp
        integer(kind = int_wp) :: itype, maxdef, itmdep, locdep, maxlst, lang
        integer(kind = int_wp) :: iyear, imonth, iday, ihour, iminut, isecnd
        integer(kind = int_wp) :: isfact, idummy, idate, itime, iprdep
        !
        !         Open the DELWAQ .HIS file
        !
        CALL open_waq_files (lun, FNAME(1), 24, 2, IERROR)
        IF (IERROR /= 0) RETURN

        ! map or his

        call extract_file_extension(fname(1), ext, extpos, extlen)
        call upper_case(ext, ext, extlen)
        if (ext == 'MAP') then
            mapfil = .true.
        else
            mapfil = .false.
        endif
        !
        !         Read primary system characteristics
        !
        READ (lun, ERR = 100)   FNAME(3)(1:160)
        IF (FNAME(3)(121:123) /= 'T0: ' .AND. &
                FNAME(3)(121:123) /= 't0: ' .AND. &
                FNAME(3)(121:123) /= 'T0= ' .AND. &
                FNAME(3)(121:123) /= 't0= ') THEN
            GOTO 150
        ENDIF
        READ (FNAME(3)(125:128), '(I4)') IYEAR
        READ (FNAME(3)(130:131), '(I2)') IMONTH
        READ (FNAME(3)(133:134), '(I2)') IDAY
        READ (FNAME(3)(136:137), '(I2)') IHOUR
        READ (FNAME(3)(139:140), '(I2)') IMINUT
        READ (FNAME(3)(142:143), '(I2)') ISECND
        READ (FNAME(3)(151:158), '(I8)') ISFACT
        READ (lun, ERR = 110)   NOTOT, NODUMP
        READ (lun, ERR = 120) (FNAME(3)(181:200), K = 1, NOTOT)
        if (.not. mapfil) then
            READ (lun, ERR = 130) (IDUMMY, FNAME(3)(221:240), K = 1, NODUMP)
        endif
        IDATE = IYEAR * 10000 + IMONTH * 100 + IDAY
        ITIME = IHOUR * 10000 + IMINUT * 100 + ISECND
        OTIME = julian_with_leapyears (IDATE, ITIME)
        SECOND = 1 / 864.00D+02
        !
        !         Read the values at all times
        !
        NTT = NODUMP * NOTOT
        ALLOCATE(RDATA(NTT))
        NRLST = 0
        SETALL = .FALSE.
        IF (TIMDEF(1, 1) < 0.5) SETALL = .TRUE.

        10 READ (lun, ERR = 140, END = 200) IDUMMY, (RDATA(K), K = 1, NTT)
        DO I = 1, MAXDEF
            ATIME = OTIME + IDUMMY * ISFACT * SECOND
            IF ((ATIME>TIMDEF(1, I) .AND. ATIME<TIMDEF(2, I)) .OR. &
                    SETALL) THEN
                NRLST = NRLST + 1
                IF (NRLST > MAXLST) GOTO 160
                TIMLST(NRLST) = ATIME
                ITMTYP(NRLST) = 2
                GOTO 10
            ENDIF
        end do
        GOTO 10
        !
        !         Error messages
        !
        100 IERROR = 10
        GOTO 200
        110 IERROR = 11
        GOTO 200
        120 IERROR = 12
        GOTO 200
        130 IERROR = 13
        GOTO 200
        140 IERROR = 14
        GOTO 200
        150 IERROR = 15
        GOTO 200
        160 IERROR = 16
        !
        200 CLOSE (lun)
        IF (ALLOCATED(RDATA)) DEALLOCATE(RDATA)
        RETURN
        !
    END

end module m_gettme
