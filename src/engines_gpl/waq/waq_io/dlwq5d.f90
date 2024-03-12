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
module m_dlwq5d
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQ5D (LUNUT, IAR, RAR, IIMAX, IRMAX, &
            IPOSR, NPOS, ILUN, LCH, LSTACK, &
            CCHAR, CHULP, NOTOT, NOTOTC, time_dependent, NOBRK, &
            IOPT, DTFLG1, DTFLG3, ITFACT, ITYPE, &
            IHULP, RHULP, IERR, ierr3)
        !
        !
        !     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED            : May '96  by L. Postma
        !
        !     MODIFIED           :
        !
        !     FUNCTION           : Boundary and waste data new style
        !
        !     SUBROUTINES CALLED : CNVTIM - converting times of breakpoints
        !
        !     LOGICAL UNITS      : LUN(27) = unit stripped DELWAQ input file
        !                          LUN(29) = unit formatted output file
        !                          LUN( 2) = unit intermediate file (system)
        !                          LUN(14) = unit intermediate file (boundaries)
        !                          LUN(15) = unit intermediate file (wastes)
        !
        !     PARAMETERS    :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     LUNUT   INTEGER    1         INPUT   unit number for ASCII output
        !     IAR     INTEGER  IIMAX       IN/OUT  integer   workspace
        !     RAR     REAL     IRMAX       IN/OUT  real      workspace
        !     IIMAX   INTEGER    1         INPUT   max. int. workspace dimension
        !     IRMAX   INTEGER    1         INPUT   max. real workspace dimension
        !     IPOSR   INTEGER    1         IN/OUT  Start position on input line
        !     NPOS    INTEGER    1         INPUT   nr of significant characters
        !     ILUN    INTEGER   LSTACK     INPUT   unitnumb include stack
        !     LCH     CHAR*(*)  LSTACK     INPUT   file name stack, 4 deep
        !     LSTACK  INTEGER    1         INPUT   include file stack size
        !     CCHAR   CHAR*1     1         INPUT   comment character
        !     CHULP   CHAR*(*)   1         OUTPUT  space for limiting token
        !     NOTOT   INTEGER    1         INPUT   size of the matrix to be read
        !     ITTIM   INTEGER    1         INPUT   0 if steady, 1 if time function
        !     NOBRK   INTEGER    1         OUTPUT  number of records read
        !     IOPT    INTEGER    1         INPUT   3 is harmonics, 4 is fourier
        !     DTFLG1  LOGICAL    1         INPUT   True if time in 'date' format
        !     DTFLG3  LOGICAL    1         INPUT   True if YYetc instead of DDetc
        !     ITFACT  INTEGER    1         INPUT   factor between clocks
        !     ITYPE   INTEGER    1         OUTPUT  type of info at end
        !     IERR    INTEGER    1         OUTPUT  return code
        !     IERR3   INTEGER    1         OUTPUT  actual error indicator
        !
        !
        use timers       !   performance timers
        use date_time_utils, only : convert_string_to_time_offset, convert_relative_time

        logical, intent(in) :: time_dependent !< True if the BC or Waste load definition is time dependent (linear, harmonic or Fourier), and false if it is constant.

        INTEGER(kind = int_wp) :: IIMAX, IRMAX, I
        CHARACTER*(*) LCH(LSTACK), CHULP
        CHARACTER*1   CCHAR
        DIMENSION     IAR(*), ILUN(LSTACK)
        LOGICAL       NEWREC, DTFLG1, DTFLG3, IGNORE
        integer(kind = int_wp) :: ihulp
        integer(kind = int_wp) :: ithndl = 0
        integer(kind = int_wp) :: nobrk, itel, itel2, ierr3, itype
        integer(kind = int_wp) :: lunut, ilun, iposr, npos, ierr, itfact
        integer(kind = int_wp) :: iar, notot, nototc, lstack, iopt
        real :: rar(:), rhulp

        if (timon) call timstrt("dlwq5d", ithndl)
        !
        !     Some initialisation
        !
        IGNORE = .FALSE.
        NEWREC = .FALSE.
        IF (time_dependent) NEWREC = .TRUE.                          ! it is a time function
        NOBRK = 0
        ITEL = 1
        ITEL2 = 1
        ierr3 = 0
        IF (ITYPE /= 0) GOTO 20                                  ! it was called with an argument
        !
        !     Read loop
        !
        10 IF (NEWREC) THEN
            ITYPE = 0                                                 ! everything is valid
        ELSE
            ITYPE = 3                                                 ! a real value schould follow
        ENDIF
        CALL RDTOK1 (LUNUT, ILUN, LCH, LSTACK, CCHAR, &
                IPOSR, NPOS, CHULP, IHULP, RHULP, &
                ITYPE, IERR)
        !          A read error
        IF (IERR  /= 0) goto 9999
        !          A token has arrived
        IF (ITYPE == 1) THEN                                     ! that must be an absolute timer string
            CALL convert_string_to_time_offset (CHULP, IHULP, .FALSE., .FALSE., IERR)    !  2^31 =  2147483648
            IF (IHULP == -999) THEN                              !       YYYYDDDHHMMSS so 64 bits integer
                IERR = 1
                WRITE (LUNUT, 1020) TRIM(CHULP)
                goto 9999
            ENDIF
            IF (IERR   /=    0) THEN                              ! the found entry is not a new time value
                if (nobrk <= 1) then
                    write (lunut, 1040) nobrk
                    !ierr3 = ierr3 + 1
                endif
                IERR = 0
                goto 9999
            ENDIF
            IHULP = ITFACT * IHULP
        ELSEIF (ITYPE == 2) THEN
            call convert_relative_time (IHULP, 1, DTFLG1, DTFLG3)
        else
            ihulp = 0
        ENDIF
        !          Getting the data of this block (no strings any more)
        20 IF (time_dependent .AND. NEWREC) THEN
            !          it was a non-real and characters has been caught
            IF (IHULP == -999) THEN
                IGNORE = .TRUE.
            ELSE                                                      ! a new breakpoint found
                IGNORE = .FALSE.
                NOBRK = NOBRK + 1
                IF (NOBRK <= IIMAX) THEN
                    IAR(NOBRK) = IHULP
                    if (nobrk > 1) then
                        if (ihulp <= iar(nobrk - 1)) then ! times not strinctly ascending
                            write (lunut, 1030) ihulp, iar(nobrk - 1)
                            ierr3 = ierr3 + 1
                        endif
                    endif
                ELSE
                    WRITE (LUNUT, 1000) IIMAX
                    IERR = 100
                    goto 9999
                ENDIF
            ENDIF
            NEWREC = .FALSE.
            GOTO 10
        ENDIF
        !
        IF (.NOT. IGNORE) THEN
            DO I = 1, NOTOT / NOTOTC
                RAR(ITEL + (I - 1) * NOTOTC) = RHULP
            END DO
        ENDIF
        !        are we to expect a new record ?
        IF (MOD(ITEL2, NOTOTC) == 0) THEN
            NEWREC = .TRUE.
            ITEL = ITEL + NOTOT - NOTOTC
        END IF
        !        it was a constant, so we can now return.
        IF (NEWREC .AND. (.not. time_dependent)) THEN
            NOBRK = 1
            IAR(1) = 0
            goto 9999
        ENDIF
        !        increase the counter for the next real and go to input
        IF (.NOT. IGNORE) ITEL = ITEL + 1
        ITEL2 = ITEL2 + 1
        GOTO 10
        9999 if (timon) call timstop(ithndl)
        return
        !
        1000 FORMAT (' ERROR ! Number of breakpoints exceeds system', &
                ' maximum of: ', I10)
        1020 FORMAT (' ERROR ! Absolute timer does not fit in timer ', &
                'format: ', A, / &
                ' Is your T0 setting in block #1 correct?'/, &
                ' Allowed difference with T0 is usually ca. 68 years.')
        1030 FORMAT (/' ERROR ! Time value ', I10, ' not larger than previous time value ', I10)
        1040 FORMAT (/' WARNING ! There are only ', I2, ' breakpoints found for this time series')
        !
    END

end module m_dlwq5d
