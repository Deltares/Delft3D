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
module m_dlwqj3
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQJ3 (LUNWR, LUNUT, IWIDTH, NOBRK, IAR, &
            RAR, RMAT, NOITM, NODIM, IORDER, &
            SCALE, ODS, BINFIL, IOPT, IPRO, &
            ITFACT, DTFLG1, DTFLG3, IFILSZ, JFILSZ, &
            CAR, STRNG1, STRNG2, STRNG3, IOUTPT)
        !
        !
        !     Deltares        SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED            : May '96  by L. Postma
        !
        !     MODIFIED           :
        !     jvb  ook voor constanten hebben we in delwaq2 initieel werkruimte nodig als deze kleiner is als de werkruimte
        !     voor de tijdsafhankelijke zaken. Maar als er bijvoorbeeld geen tijdsafhankelijke zaken zijn komen we in
        !     delwaq2 in de problemen. Voorlopig tel ik al het werkgeheugen van de contstanten mee.
        !                          March 2000 L. Postma simplified
        !
        !     FUNCTION           : Prints and writes blocks of data
        !
        !     SUBROUTINES CALLED : CONVER - converting times of breakpoints
        !
        !     LOGICAL UNITS      : LUNWR   = unit intermediate file
        !                          LUNUT   = unit formatted output file
        !
        !     PARAMETERS    :
        !
        !     NAME    KIND      LENGTH    FUNCT.  DESCRIPTION
        !     ---------------------------------------------------------
        !     LUNWR   INTEGER     1       INPUT   Unit of binary work file
        !     LUNUT   INTEGER     1       INPUT   Unit of ASCII output file
        !     IWIDTH  INTEGER     1       INPUT   Width of the output file
        !     NOBRK   INTEGER     1       INPUT   number of blocks to write
        !     IAR     INTEGER     *       INPUT   integer   workspace
        !     RAR     REAL        *       INPUT   real      workspace
        !     RMAT    REAL        *       INPUT   matrix of values
        !     NOITM   INTEGER     1       INPUT   number of items to write
        !     NODIM   INTEGER     1       INPUT   number of subs  to write
        !     IORDER  INTEGER     1       INPUT   1 = groups of subs per item
        !     SCALE   LOGICAL     1       INPUT   T = NODIM scale factors present
        !     ODS     LOGICAL     1       INPUT   T = Breakpoints are converted
        !     BINFIL  LOGICAL     1       INPUT   T = Info from binary file
        !     IOPT    INTEGER     1       INPUT   1 is block     2 is linear
        !                                         3 is harmonics 4 is fourier
        !     IPRO    INTEGER     1       INPUT   0 is non permanent memory
        !     ITFACT  INTEGER     1       INPUT   factor between clocks
        !     DTFLG1  LOGICAL     1       INPUT   'date'-format 1st time scale
        !     DTFLG3  LOGICAL     1       INPUT   'date'-format (F;ddmmhhss,T;yydddhh)
        !     IFILSZ  INTEGER     1       IN/OUT  cumulative integer space count
        !     JFILSZ  INTEGER     1       IN/OUT  cumulative real space count
        !     CAR     CHARACTER   *       INPUT   character workspace
        !     STRNG1  CHAR*(*)    1       INPUT   write string 1 (items)
        !     STRNG2  CHAR*(*)    1       INPUT   write string 2 (values/concs)
        !     STRNG3  CHAR*(*)    1       INPUT   write string 3 (brkp/harm)
        !     IOUTPT  INTEGER     1       INPUT   output file option
        !
        !
        use m_dlwqj2
        use m_conver
        use timers       !   performance timers

        LOGICAL :: SCALE, ODS, BINFIL, defaults_on
        LOGICAL :: DTFLG1, DTFLG3
        CHARACTER*(*) :: STRNG1, STRNG2, STRNG3, CAR(:)
        integer(kind = int_wp) :: NODIM
        integer(kind = int_wp) :: k, ie, IE2, NOITM, i1, i2
        integer(kind = int_wp) :: nodi2, iorder, lunut, lunwr, iar(:)
        integer(kind = int_wp) :: iopt, ipro
        integer(kind = int_wp) :: ifilsz, jfilsz, nobrk, ioffb, ioffi, ioffs
        integer(kind = int_wp) :: iskip, iskp2, notot, iss, ioutpt, iwidth
        integer(kind = int_wp) :: itel2, itfact, i1dum, i2dum
        integer(kind = int_wp) :: itels, itel, i3
        real(kind = real_wp) :: rar(:), rmat(:)
        !
        !     Local declarations

        integer(kind = int_wp) :: ithndl = 0
        if (timon) call timstrt("dlwqj3", ithndl)
        !
        !     Write headers
        !
        defaults_on = .FALSE.
        IF (NODIM < 0) defaults_on = .TRUE.
        NODI2 = NODIM
        IF (NODIM <= 0) NODI2 = 1
        IF (IORDER == 1) THEN
            WRITE (LUNUT, 1000) NOITM, NODI2, STRNG2
            WRITE (LUNWR) IORDER, &
                    NOITM, (IAR(K), K = 1, NOITM), &
                    NODIM, (IAR(K), K = NOITM + 1, NOITM + NODIM), &
                    IOPT, IPRO
        ELSEIF (IORDER == 2) THEN
            WRITE (LUNUT, 1000) NODI2, NOITM, STRNG1
            IF (LUNWR > 0) &
                    WRITE (LUNWR) IORDER, &
                            NODIM, (IAR(K), K = 1, NODIM), &
                            NOITM, (IAR(K), K = NODIM + 1, NODIM + NOITM), &
                            IOPT, IPRO
        ENDIF

        IFILSZ = IFILSZ + NOITM + MAX(0, NODIM) + 5
        !     jvb1
        !
        !          just declare array space for binary files and return
        !
        IF (BINFIL) THEN
            WRITE (LUNUT, 1130) IPRO
            IFILSZ = IFILSZ + 3
            JFILSZ = JFILSZ + MAX(1, NODIM) * MAX(1, NOITM) * 3
            GOTO 70
        ENDIF
        !
        !       Initialisation
        !
        IF (NOBRK == 0) THEN
            SCALE = .FALSE.
            GOTO 70
        ENDIF
        IOFFB = NOITM + NODI2 + 1
        IOFFI = 0
        IOFFS = NOITM
        ISKIP = 1
        ISKP2 = NODI2
        NOTOT = NOITM * NODI2
        IF (IOPT == 3 .OR. IOPT == 4) NOTOT = NOTOT + 1
        IF (IORDER == 2) THEN
            IOFFI = MAX(NODIM, 0)
            IOFFS = 0
            ISKIP = NOITM
            ISKP2 = 1
        ENDIF
        !
        !       Scale factors
        !
        ISS = 1
        IF (SCALE) THEN
            SCALE = .FALSE.
            ISS = 1
            IF (IOUTPT >= 4) THEN
                WRITE (LUNUT, 1010)
                DO I2 = 1, NODIM, IWIDTH
                    IE = MIN(I2 + IWIDTH - 1, NODIM)
                    WRITE (LUNUT, 1020) (IAR(IOFFS + K), K = I2, IE)
                    WRITE (LUNUT, 1025) (CAR_OF_DUM(CAR, IAR(IOFFS + K)), K = I2, IE)
                    WRITE (LUNUT, 1030) (RAR(K), K = I2, IE)
                end do
            ENDIF
            DO I1 = 1, NOBRK
                DO I2 = 0, NOTOT - 1
                    IF (IORDER == 1) ITEL2 = MOD(I2, NODIM) + 1
                    IF (IORDER == 2) ITEL2 = I2 / NODIM + 1
                    RMAT(ISS + I2) = RMAT(ISS + I2) * RAR(ITEL2)
                end do
                ISS = ISS + NOTOT
            end do
        ENDIF
        !
        !       Convert breakpoints
        !
        IF (NOBRK > 1) THEN
            IF (IOUTPT >= 4) WRITE (LUNUT, 1040) STRNG3, NOBRK
            IF (.NOT. ODS) &
                    CALL CONVER (IAR(IOFFB:), NOBRK, ITFACT, DTFLG1, DTFLG3)
            IF (defaults_on .AND. IOUTPT >= 4) WRITE (LUNUT, 1050)
        ELSE
            IF (defaults_on) THEN
                IF (IOUTPT >= 4) WRITE (LUNUT, 1050)
            ELSE
                IF (IOUTPT >= 4) WRITE (LUNUT, 1060)
            ENDIF
        ENDIF
        !
        !       Write binary file
        !
        IF (LUNWR > 0) THEN
            I1DUM = 0
            I2DUM = 0
            CALL DLWQJ2 (LUNWR, NOBRK, NOTOT, 1, IAR(IOFFB:), & ! write table in binary format to wrk file.
                    RMAT, I1DUM, I2DUM)
            !     jvb1    IF ( IOPT .NE. 0 ) THEN
            IFILSZ = IFILSZ + I1DUM
            JFILSZ = JFILSZ + I2DUM
            !     jvb1    ENDIF
        ENDIF
        !
        !       Write formatted output
        !
        IF (IOUTPT >= 4) THEN
            ITELS = 0
            DO I1 = 1, NOBRK
                IF (NOBRK > 1) THEN
                    IF (IOPT == 1) &
                            WRITE (LUNUT, 1070) STRNG3, I1, IAR(IOFFB + I1 - 1)
                    IF (IOPT == 2) &
                            WRITE (LUNUT, 1070) STRNG3, I1, IAR(IOFFB + I1 - 1)
                    IF (IOPT == 3) THEN
                        ITELS = ITELS + 1
                        WRITE (LUNUT, 1080) I1, IAR(IOFFB + I1 - 1), RMAT(ITELS)
                    ENDIF
                    IF (IOPT == 4) THEN
                        ITELS = ITELS + 1
                        WRITE (LUNUT, 1090) I1, IAR(IOFFB + I1 - 1), RMAT(ITELS)
                    ENDIF
                ENDIF
                DO I2 = 1, NODI2, IWIDTH
                    IE2 = MIN(I2 + IWIDTH - 1, NODI2)
                    IF (NODIM > 0) THEN
                        WRITE (LUNUT, 1100) STRNG2, (IAR(IOFFS + K), K = I2, IE2)
                        WRITE (LUNUT, 1150) STRNG1, &
                                (CAR_OF_DUM(CAR, IAR(IOFFS + K)), K = I2, IE2)
                    ENDIF
                    ITEL = ITELS
                    DO I3 = 1, NOITM
                        WRITE (LUNUT, 1120)  ABS(IAR(IOFFI + I3)), &
                                (RMAT(ITEL + K), K = (I2 - 1) * ISKIP + 1, (IE2 - 1) * ISKIP + 1, ISKIP)
                        ITEL = ITEL + ISKP2
                    end do
                end do
                ITELS = ITELS + NODI2 * NOITM
            end do
        ENDIF
        !
        70 WRITE (LUNUT, 1140)
        if (timon) call timstop(ithndl)
        RETURN
        !
        1000 FORMAT (/' DATA grouped in', I10, ' blocks of', I10, ' ', A)
        1010 FORMAT (' Scale factors for this block of data: ')
        1020 FORMAT (' Scale    :', I6, 9I12)
        1025 FORMAT (' Substance:', 10('  ', A10))
        1030 FORMAT (' values   :', 10E12.4)
        1040 FORMAT (/' Number of ', A, 's with full data:', I5)
        1050 FORMAT (' Default values in this block.')
        1060 FORMAT (' Constant values in this block.')
        1070 FORMAT (' ', A, ' ', I7, ' :', I10)
        1080 FORMAT (' Harmonic: ', I3, ' :', I10, ' Phase: ', 10E12.4)
        1090 FORMAT (' Fourier : ', I3, ' :', I10, ' Phase: ', 10E12.4)
        1100 FORMAT (' ', A, I20, 9I12)  ! ( ' ',A,I6,9I12)
        1150 FORMAT (' ', A, ' ', 10('  ', A10))
        1120 FORMAT (I10, 2X, 1P, 10E12.4)
        1130 FORMAT (' Info comes at runtime from binary file at unit: ', I3)
        1140 FORMAT(/' ====> input item completed <==== '//)
        !
    END
    !
    CHARACTER*20 FUNCTION CAR_OF_DUM(CAR, I)
        INTEGER(kind = int_wp) :: I
        CHARACTER*(*) CAR(*)
        IF (I > 0) THEN
            CAR_OF_DUM = CAR(I)
        ELSEIF (I == 0) THEN
            CAR_OF_DUM = 'FLOW'
        ELSE
            CAR_OF_DUM = 'ignored'
        ENDIF
        RETURN
    END

end module m_dlwqj3
