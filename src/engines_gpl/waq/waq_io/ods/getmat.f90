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
module m_getmat
    use m_waq_precision

    implicit none

contains


    SUBROUTINE GETMAT (FNAME, ITYPE, IPRCOD, LOC, TIM, &
            AMISS, MAXDIM, DATA, IERROR, &
            OPTION)
        !
        !
        !     Deltares        MARINE & COASTAL MANAGEMENT
        !
        !     CREATED            : May '96  by L. Postma
        !
        !     MODIFIED           :
        !
        !     FUNCTION           : ODS GETMAT routine for DELWAQ HIS-files
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
        !     IPRCOD  INTEGER  IERROR     INPUT   List of wanted parameters
        !     LOC     INTEGER   3*3       INPUT   List of indices of locations
        !     TIM     REAL*8     3        INPUT   Interval and step for data
        !     AMISS   REAL*4     2        INPUT   Missing value in output/input
        !     MAXDIM  INTEGER    1        INPUT   Maximum dimension of output arr
        !     DATA    REAL*4   MAXDIM     OUTPUT  The produced information
        !     IERROR  INTEGER    1        IN/OUT  Error code
        !     OPTION  CHAR*256   1        IN/OUT  For future use
        !
        !
        use time_module
        use m_string_manipulation, only : upper_case
        use m_open_waq_files
        use m_file_path_utils, only : extract_file_extension

        CHARACTER*256 FNAME (3), OPTION
        REAL(kind = real_wp) :: DATA(*)
        INTEGER(kind = int_wp) :: LOC(*)
        REAL(kind = dp) :: TIM(2), OTIME, ATIME, SECOND
        real(kind = real_wp) :: amiss
        character*256 :: ext     ! file extension
        integer(kind = int_wp) :: extpos   ! position of extension
        integer(kind = int_wp) :: extlen   ! length of file extension
        logical :: mapfil  ! true if map file extension
        integer(kind = int_wp) :: lun
        integer(kind = int_wp) :: I1
        integer(kind = int_wp) :: NODUMP, NOTOT
        integer(kind = int_wp) :: k, L, i2, i3, i4, ierror, iyear, imonth, iday
        integer(kind = int_wp) :: ihour, iminut, isecnd, isfact, idummy, idate
        integer(kind = int_wp) :: itime, ntt, iset, iprcod, maxdim
        integer(kind = int_wp) :: adummy, itype

        !         Open the DELWAQ .HIS file if needed
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
            GOTO 140
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
            READ (10, ERR = 130) (IDUMMY, FNAME(3)(221:240), K = 1, NODUMP)
        endif
        IDATE = IYEAR * 10000 + IMONTH * 100 + IDAY
        ITIME = IHOUR * 10000 + IMINUT * 100 + ISECND
        OTIME = julian_with_leapyears (IDATE, ITIME)
        SECOND = ISFACT / 864.00D+02
        !
        !         Standard ODS processing
        !
        NTT = NODUMP * NOTOT
        ISET = 0
        10 I1 = (LOC(1) - 1) * NOTOT + IPRCOD - 1
        I2 = (LOC(2) - LOC(1)) / LOC(3)
        I3 = LOC(3) * NOTOT - 1
        I4 = NTT - I1 - (1 + I3) * I2 - 1
        IF (ISET + I2 + 1 > MAXDIM) GOTO 150
        READ (lun, ERR = 150, END = 200) IDUMMY, (ADUMMY, K = 1, I1), &
                (DATA(ISET + K), (ADUMMY, L = 1, I3), K = 1, I2), &
                DATA(ISET + I2 + 1), (ADUMMY, L = 1, I4)
        ATIME = OTIME + IDUMMY * SECOND
        IF (ATIME > TIM(1) .AND. ATIME < TIM(2)) THEN
            ISET = ISET + I2 + 1
        ENDIF
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
        !
        200 CLOSE (lun)
        RETURN
        !
    END
    SUBROUTINE GETMAT2(FNAME, ITYPE, IPRCOD, LOC, TIM, &
            AMISS, MAXDIM, DATA, IERROR, &
            OPTION)
        !
        !
        !     Deltares        MARINE & COASTAL MANAGEMENT
        !
        !     CREATED            : May '96  by L. Postma
        !
        !     MODIFIED           :
        !
        !     FUNCTION           : ODS GETMAT routine for DELWAQ HIS-files
        !
        !     SUBROUTINES CALLED :

        use time_module
        use m_string_manipulation, only : upper_case
        use m_file_path_utils, only : extract_file_extension
        use m_open_waq_files
        !
        !     LOGICAL UNITS      :
        !
        !     PARAMETERS    :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     FNAME   CHAR*256   3        IN/LOC  Complete file name
        !     ITYPE   INTEGER    1        INPUT   File type
        !     IPRCOD  INTEGER  IERROR     INPUT   List of wanted parameters
        !     LOC     INTEGER   3*3       INPUT   List of indices of locations
        !     TIM     REAL*8     2        INPUT   Interval and step for data
        !     AMISS   REAL*4     2        INPUT   Missing value in output/input
        !     MAXDIM  INTEGER    1        INPUT   Maximum dimension of output arr
        !     DATA    REAL*4   MAXDIM     OUTPUT  The produced information
        !     IERROR  INTEGER    1        IN/OUT  Error code
        !     OPTION  CHAR*256   1        IN/OUT  For future use
        !
        !
        CHARACTER*256 FNAME (3), OPTION
        REAL(kind = real_wp) :: DATA(*)
        integer(kind = int_wp) :: LOC(*)
        REAL(kind = dp) :: TIM(2), OTIME, ATIME, SECOND
        real(kind = real_wp) :: amiss
        character*256 :: ext     ! file extension
        integer(kind = int_wp) :: extpos   ! position of extension
        integer(kind = int_wp) :: extlen   ! length of file extension
        logical :: mapfil  ! true if map file extension
        integer(kind = int_wp) :: lun, NODUMP, NOTOT, k, NTT
        integer(kind = int_wp) :: ierror, iyear, imonth, iday
        integer(kind = int_wp) :: ihour, iminut, isecnd, isfact, idummy, idate
        integer :: itime, iset, iprcod(:), maxdim
        integer(kind = int_wp) :: adummy, itype
        !
        !         Open the DELWAQ .HIS file if needed
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
            GOTO 140
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
        SECOND = ISFACT / 864.00D+02
        !
        !         Standard ODS processing
        !
        NTT = NODUMP * NOTOT
        ISET = 0
        DO
            IF (ISET + NTT > MAXDIM) GOTO 150
            READ (lun, ERR = 150, END = 200) IDUMMY, (DATA(ISET + K), K = 1, NTT)
            ATIME = OTIME + IDUMMY * SECOND
            IF (ATIME > TIM(1) .AND. ATIME < TIM(2)) THEN
                ISET = ISET + NTT
            ENDIF
        ENDDO
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
        !
        200 CLOSE (lun)
        RETURN
        !
    END

end module m_getmat
