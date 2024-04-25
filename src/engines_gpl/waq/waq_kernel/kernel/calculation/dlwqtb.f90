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
module m_dlwqtb
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQTB (LUNUT, IOFF, A, J, IIPNT, &
            IRPNT, max_int_size, ITIME, KTYPE, AVAL, &
            IVAL, IERR)
        !
        !     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
        !
        !     CREATED             : april 1996 by L. Postma
        !     MODIFIED            : march 2000 by L. Postma
        !                                 skip assignments if aal values missing
        !
        !     FUNCTION            : Updates the boundary and waste arrays
        !
        !     LOGICAL UNITNUMBERS : LUNUT - monitoring file
        !
        !     SUBROUTINES CALLED  : none
        !
        !     PARAMETERS          :
        !
        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     LUNUT   INTEGER    1        INPUT   unit number monitoring file
        !     IOFF    INTEGER    1        INPUT   index of first concentration
        !     A       REAL       ?        INPUT   Real    boundary workspace
        !     J       INTEGER    ?        INPUT   Integer boundary workspace
        !     IIPNT   INTEGER    1        IN/OUT  Offset in integer array space
        !     IRPNT   INTEGER    1        IN/OUT  Offset in real array space
        !     max_int_size   INTEGER    1        INPUT   Maximum integer array size
        !     ITIME   INTEGER    1        INPUT   Time in units of the system clock
        !     KTYPE   INTEGER   NOITM     INPUT   Type of items
        !     AVAL    REAL    NOTOT,NOITM OUTPUT  Values of the bounds/wastes
        !     IVAL    INTEGER NOTOT,NOITM LOCAL   Count array for averages
        !     IERR    INTEGER    1        IN/OUT  error count
        !
        !     Declaration of arguments
        !
        use timers

        real(kind = real_wp), PARAMETER :: TWOPI = 6.28319
        integer(kind = int_wp) :: J(*), KTYPE(*), IVAL(*)
        real(kind = real_wp) :: A(*), AVAL(*)
        integer(kind = int_wp) :: IERR, LUNUT, IRPNT, max_int_size, ITIME, IOFF, IIPNT

        !     local
        real(kind = real_wp) :: missing_value, aa, ab, aphase, func
        integer(kind = int_wp) :: noitm, notot, nobrk
        integer(kind = int_wp) :: i, i1, i2, i3, ia, ib, ic, ij, ii
        integer(kind = int_wp) :: iopt, ipro, iord, itim1, itim2
        integer(kind = int_wp) :: irec, idt, itimf, it1c, it2c, idtc, iperio
        integer(kind = int_wp) :: npnt, npst, ndim, ndst, ntt

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqtb", ithandl)
        !
        !         initialise the system
        !
        missing_value = -999.
        !       Number of items
        NOITM = J(1)
        NOTOT = J(2)
        IA = 0
        IJ = 2
        !
        !       Determine switch for this block 1 = items,subst
        !                                       2 = subst,items
        !       Start of the loop over blocks
        !
        !       Determine parameters NPNT = nr of items
        !                            NPST = start of item nr's in the J-array
        !                            NDIM = nr of substances
        !                            NDST = start of subs nr's in the J-array
        !                            IOPT = option 1 and 2 at breakpoints etc.
        !                            IPRO = procedure (overrule or not)
        !
        10 IJ = IJ + 1
        IORD = J(IJ)
        IJ = IJ + 1
        IF (IORD == 1) THEN
            NPNT = J(IJ)
            NPST = IJ
            NDIM = J(NPST + NPNT + 1)
            NDST = NPST + NPNT + 1
        ENDIF
        IF (IORD == 2) THEN
            NDIM = J(IJ)
            NDST = IJ
            NPNT = J(NDST + NDIM + 1)
            NPST = NDST + NDIM + 1
        ENDIF
        IJ = IJ + NDIM + NPNT + 4
        IOPT = J(IJ - 2)
        IPRO = J(IJ - 1)
        NTT = NDIM * NPNT
        !
        !       Nr of breakpoints or harmonics
        !
        NOBRK = J(IJ)
        !
        !           Setting of default values
        !
        IF (NPNT == 0) THEN
            DO I2 = 1, NDIM
                IA = IA + 1
                IB = IOFF + J(NDST + I2)
                DO I1 = 1, NOITM
                    AVAL(IB) = A(IA)
                    IB = IB + NOTOT
                end do
            end do
            IJ = IJ + 1
            GOTO 150
        ENDIF
        !
        !       IOPT = 1 : Block function , IOPT = 2 : Linearly interpolated
        !
        IF (IOPT == 1 .OR. IOPT == 2) THEN
            !
            !           Get the right time in the block
            !
            IF (NOBRK > 1) THEN
                ITIM1 = J(IJ + 1)
                ITIM2 = J(IJ + NOBRK)
                IDT = ITIM2 - ITIM1
                IF (ITIME < ITIM1) THEN
                    IREC = 1
                    ITIM1 = 0
                    ITIM2 = 1
                    IDT = ITIM1 + ITIM2
                    GOTO 50
                ENDIF
                ITIMF = ITIME
                IF (ITIME >= ITIM2) &
                        ITIMF = ITIME - ((ITIME - ITIM2) / IDT + 1) * IDT
                !
                !           Make interpolation constants if IOPT = 2
                !
                DO I = 2, NOBRK
                    IF (J(IJ + I) > ITIMF) THEN
                        IF (IOPT == 2) THEN
                            ITIM1 = ITIMF - J(IJ + I - 1)
                            ITIM2 = J(IJ + I) - ITIMF
                        ELSE
                            ITIM1 = 0
                            ITIM2 = 1
                        ENDIF
                        IDT = ITIM1 + ITIM2
                        IREC = I - 1
                        GOTO 50
                    ENDIF
                end do
            ELSE
                IREC = 1
                ITIM2 = 1
                ITIM1 = 0
                IDT = 1
            ENDIF
            !
            !           Set or interpolate the correct values
            !
            50    I = IA + (IREC - 1) * NTT
            !           Inner loop in A over the substances
            IF (IORD == 1) THEN
                DO I1 = 1, NPNT
                    II = J(NPST + I1)
                    IB = (II - 1) * NOTOT
                    DO I2 = 1, NDIM
                        IC = IOFF + J(NDST + I2)
                        !
                        !                 Ignore negative indexes (0 equal to flow wastes??)
                        !
                        IF (IC >= 0) THEN
                            I = I + 1
                            AA = A(I)
                            IF (NOBRK > 1) THEN
                                AB = A(I + NTT)
                            ELSE
                                AB = 0.0
                            ENDIF
                            IT1C = ITIM1
                            IT2C = ITIM2
                            IDTC = IDT
                            !     Dealing with missing values
                            IF (AA == missing_value .OR. AB == missing_value) &
                                    CALL DLWMIS(A, I, missing_value, NTT, IREC, &
                                            J, IJ, NOBRK, ITIMF, IOPT, &
                                            IT1C, IT2C, IDTC, AA, AB)
                            !           If no value is found, then skip the assignment, except flow set missing
                            IF (IT1C /= 0 .OR. IT2C /= 0) THEN
                                !           Make the wanted value
                                AA = (IT2C * AA + IT1C * AB) / IDTC
                                IF (II > 0) THEN
                                    AVAL(IB + IC) = AA
                                ELSE
                                    !              Set a whole type
                                    DO I3 = 1, NOITM
                                        IF (KTYPE(I3) == -II) THEN
                                            AVAL ((I3 - 1) * NOTOT + IC) = AA
                                        ENDIF
                                    end do
                                ENDIF
                            ELSEIF (IC - IOFF == 0) THEN
                                !                       for flow accept missing (detected flow)
                                IF (II > 0) THEN
                                    AVAL(IB + IC) = missing_value
                                ELSE
                                    !                          Set a whole type
                                    DO I3 = 1, NOITM
                                        IF (KTYPE(I3) == -II) THEN
                                            AVAL ((I3 - 1) * NOTOT + IC) = missing_value
                                        ENDIF
                                    ENDDO
                                ENDIF

                            ENDIF
                        ELSE
                            !
                            !                    Ignore value
                            !
                            I = I + 1
                        ENDIF
                    end do
                end do
                !           Inner loop in A over the items
            ELSE
                DO I1 = 1, NDIM
                    IC = IOFF + J(NDST + I1)
                    IF (IC >= 0) THEN
                        DO I2 = 1, NPNT
                            I = I + 1
                            AA = A(I)
                            IF (NOBRK > 1) THEN
                                AB = A(I + NTT)
                            ELSE
                                AB = 0.0
                            ENDIF
                            IT1C = ITIM1
                            IT2C = ITIM2
                            IDTC = IDT
                            !     Dealing with missing values
                            IF (AA == missing_value .OR. AB == missing_value) &
                                    CALL DLWMIS(A, I, missing_value, NTT, IREC, &
                                            J, IJ, NOBRK, ITIMF, IOPT, &
                                            IT1C, IT2C, IDTC, AA, AB)
                            !           If no value is found, then skip the assignment
                            IF (IT1C /= 0 .OR. IT2C /= 0) THEN
                                !           Make the wanted value
                                AA = (IT2C * AA + IT1C * AB) / IDTC
                                II = J(NPST + I2)
                                IF (II > 0) THEN
                                    IB = (J(NPST + I2) - 1) * NOTOT
                                    AVAL(IB + IC) = AA
                                ELSE
                                    DO I3 = 1, NOITM
                                        IF (KTYPE(I3) == -II) &
                                                AVAL ((I3 - 1) * NOTOT + IC) = AA
                                    end do
                                ENDIF
                            ELSEIF (IC - IOFF == 0) THEN
                                !                       for flow accept missing (detected flow)
                                IF (II > 0) THEN
                                    IB = (J(NPST + I2) - 1) * NOTOT
                                    AVAL(IB + IC) = missing_value
                                ELSE
                                    DO I3 = 1, NOITM
                                        IF (KTYPE(I3) == -II) &
                                                AVAL ((I3 - 1) * NOTOT + IC) = missing_value
                                    ENDDO
                                ENDIF
                            ENDIF
                        end do
                    ELSE
                        !
                        !                 Ignore value
                        !
                        I = I + NPNT
                    ENDIF
                end do
            ENDIF
            IJ = IJ + NOBRK
            IA = IA + NOBRK * NTT
        ENDIF
        !
        !       IOPT = 3 and 4 : Harmonics and fouriers, treated equally
        !
        IF (IOPT == 3 .OR. IOPT == 4) THEN
            !
            DO I = 1, NOBRK
                !
                !            harmonic function
                !
                IJ = IJ + 1
                IPERIO = J(IJ)
                IA = IA + 1
                APHASE = A(IA)
                IF (I == 1) THEN
                    FUNC = 1.0
                ELSE
                    FUNC = SIN((real(ITIME) / IPERIO - APHASE) * TWOPI)
                ENDIF
                !
                !            multiply with amplitudes and set values
                !
                !              Inner loop in A over the substances
                IF (IORD == 1) THEN
                    DO I1 = 1, NPNT
                        IB = (J(NPST + I1) - 1) * NOTOT
                        DO I2 = 1, NDIM
                            IC = IOFF + J(NDST + I2)
                            IA = IA + 1
                            IF (I == 1) THEN
                                AVAL(IB + IC) = FUNC * A(IA)
                            ELSE
                                AVAL(IB + IC) = FUNC * A(IA) + AVAL(IB + IC)
                            ENDIF
                        end do
                    end do
                    !              Inner loop in A over the items
                ELSE
                    DO I1 = 1, NDIM
                        IC = IOFF + J(NDST + I1)
                        DO I2 = 1, NPNT
                            IB = (J(NPST + I2) - 1) * NOTOT
                            IA = IA + 1
                            IF (I == 1) THEN
                                AVAL(IB + IC) = FUNC * A(IA)
                            ELSE
                                AVAL(IB + IC) = FUNC * A(IA) + AVAL(IB + IC)
                            ENDIF
                        end do
                    end do
                ENDIF
            end do
        ENDIF
        !
        !       Return until finished
        !
        150 IF (IJ < max_int_size) GOTO 10
        IF (IJ == max_int_size) THEN
            IIPNT = IIPNT + IJ
            IRPNT = IRPNT + IA
            goto 9999    !   RETURN
        ENDIF
        WRITE (LUNUT, 2010)
        IERR = IERR + 1
        9999 if (timon) call timstop (ithandl)
        RETURN
        !
        2010 FORMAT (' ERROR, updating time functions new style !')
        !
    END
    !
    SUBROUTINE DLWMIS (A, I, missing_value, NTT, IREC, &
            J, IJ, NOBRK, ITIMF, IOPT, &
            IT1C, IT2C, IDTC, AA, AB)
        use timers
        !
        real(kind = real_wp) :: A(*)
        integer(kind = int_wp) :: J(*)
        integer(kind = int_wp) :: I, IJ, NTT, IREC, NOBRK, ITIMF, IOPT, IT1C, IT2C, IDTC
        real(kind = real_wp) :: missing_value, AA, AB

        !  	local
        integer(kind = int_wp) :: ll, jj, kk

        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("dlwmis", ithandl)
        !           Search backward for the first valid point
        LL = I
        DO JJ = IREC, 1, -1
            IF (A(LL) /= missing_value) GOTO 20
            LL = LL - NTT
        end do
        JJ = 0
        !           Search forward for the first valid point
        20 LL = I + NTT
        DO KK = IREC + 1, NOBRK
            IF (A(LL) /= missing_value) GOTO 40
            LL = LL + NTT
        end do
        KK = 0
        40 AA = 0.0
        AB = 0.0
        IT1C = 0
        IT2C = 0
        !           There was a backward valid point
        IF (JJ /= 0) THEN
            AA = A(I + (JJ - IREC) * NTT)
            IF (IOPT == 1) IT2C = 1
            IF (IOPT == 2) THEN
                IF (KK /= 0) THEN
                    IT1C = ITIMF - J(IJ + JJ)
                ELSE
                    IT2C = 1
                ENDIF
            ENDIF
        ENDIF
        !           There was a forward valid point
        IF (KK /= 0) THEN
            AB = A(I + (KK - IREC) * NTT)
            IF (IOPT == 1 .AND. JJ == 0) IT1C = 1
            IF (IOPT == 2) THEN
                IF (JJ /= 0) THEN
                    IT2C = J(IJ + KK) - ITIMF
                ELSE
                    IT1C = 1
                ENDIF
            ENDIF
        ENDIF
        IDTC = IT1C + IT2C
        IF (IDTC == 0) IDTC = 1
        !
        if (timon) call timstop (ithandl)
        RETURN
    END

end module m_dlwqtb
