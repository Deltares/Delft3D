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
module m_dlwqd2
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQD2 (LUNUT, NOSYS, NOTOT, NOSEG, NOQ3, &
            KMAX, CONC, ALENG, NOWARN)
        !
        !
        !       Forester filter for the vertical.
        !
        !       Then loops over the number of horizontal segments en over the
        !       the number of active substances are made. All these verticals
        !       are filtered at most MAXFIL times.
        !
        !       Everything starts with layer 2 (from the top in WAQ). IL is
        !       the counter here with starting values ISEG+NOSEG. ILU points
        !       to the layer upper (IL-NOSEG) en ILD points to the layer down-
        !       wards (IL+NOSEG).
        !
        !       The from- and to- lengthes are in the ALENG array. The third
        !       direction last. Than means that NOQT-NOQ3+ISEG is the pointer
        !       for the exchange with the layer above and that value plus NOSEG
        !       is the pointer to the exchange downward. The from- value is
        !       the first one that is in the higher layer, The to- value is in
        !       the lower layer. You can check that the to- value for the upper
        !       exchange should equal the from- value for the downward exchange.
        !
        !       The filter starts action if the layer value is over DD larger
        !       than or over DD smaller than both bordering values. If that
        !       the IFIL flag signals a filter step.
        !       The filter corrects the difference that is largest. It does so
        !       by taking half of that difference, or the other difference
        !       which one is smallest. It multiplies this with the smallest
        !       thickness (and unknown A thus with the smallest volume). It
        !       corrects with this mass and divides by the volume (the unknown
        !       A and the thickness) to get a concentration again. This means
        !       that per step at most 0.5 times the largest difference is
        !       corrected.
        !
        !       Because WAQ has halflengthes, you must read "half the volume"
        !       but that does not differ because 0.5/0.5 = 1.0. There is an
        !       upperbound to the coorection that is at 1.0 m thickness in the
        !       original code. Because we work with half distances, it is 0.5
        !       here.
        !
        !       A maximum/minimum of DD remains. This value is a somewhat
        !       strange construct. If you got 0.01 mg Cadmium per liter in
        !       your water you better call on an environmental specialist.
        !
        use timers
        !
        INTEGER(kind = int_wp) :: LUNUT, NOSYS, NOTOT, NOSEG, NOQ3, NOWARN, KMAX
        REAL(kind = real_wp) :: CONC(NOTOT, NOSEG), ALENG(2, NOQ3)

        real(kind = real_wp) :: dd, dr, dr1, dr2, dz1, dz2, coef

        integer(kind = int_wp) :: iseg, isys, ifilt, il, is, ilu, ifil, ild, ilay
        integer(kind = int_wp) :: nhor, maxfil

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqd2", ithandl)
        !
        DD = 1.0E-02
        MAXFIL = 100
        !
        !     Only for structured 3D
        !
        IF (KMAX <= 1) goto 9999
        !
        NHOR = NOSEG / KMAX
        !          for all horizontal segments
        DO ISEG = 1, NHOR
            !          for all active substances
            DO ISYS = 1, NOSYS
                !          do until maximum iteration or untill satisfied
                DO IFILT = 1, MAXFIL
                    IFIL = 0
                    IL = ISEG
                    ILU = IL - NHOR
                    ILD = IL + NHOR
                    DO ILAY = 2, KMAX - 1
                        IL = IL + NHOR
                        ILU = ILU + NHOR
                        ILD = ILD + NHOR
                        DR1 = CONC(ISYS, IL) - CONC(ISYS, ILU)
                        DR2 = CONC(ISYS, IL) - CONC(ISYS, ILD)
                        !                    test for local maximum
                        IF (DR1 >  DD .AND. DR2 >  DD) THEN
                            IFIL = 1
                            IF (DR1 > DR2) THEN
                                DR = MIN (0.5 * DR1, DR2)
                                DZ1 = ALENG(1, ILU)
                                DZ2 = ALENG(2, ILU)
                                COEF = MIN (DZ1, DZ2, 0.5) * DR
                                CONC(ISYS, ILU) = CONC(ISYS, ILU) + COEF / DZ1
                                CONC(ISYS, IL) = CONC(ISYS, IL) - COEF / DZ2
                            ELSE
                                DR = MIN (DR1, 0.5 * DR2)
                                DZ1 = ALENG(1, IL)
                                DZ2 = ALENG(2, IL)
                                COEF = MIN (DZ1, DZ2, 0.5) * DR
                                CONC(ISYS, IL) = CONC(ISYS, IL) - COEF / DZ1
                                CONC(ISYS, ILD) = CONC(ISYS, ILD) + COEF / DZ2
                            ENDIF
                        ENDIF
                        !     test for local minimum
                        IF (DR1 < -DD .AND. DR2 < -DD) THEN
                            IFIL = 1
                            IF (DR1 < DR2) THEN
                                DR = MAX (0.5 * DR1, DR2)
                                DZ1 = ALENG(1, ILU)
                                DZ2 = ALENG(2, ILU)
                                COEF = MIN (DZ1, DZ2, 0.5) * DR
                                CONC(ISYS, ILU) = CONC(ISYS, ILU) + COEF / DZ1
                                CONC(ISYS, IL) = CONC(ISYS, IL) - COEF / DZ2
                            ELSE
                                DR = MAX (DR1, 0.5 * DR2)
                                DZ1 = ALENG(1, IL)
                                DZ2 = ALENG(2, IL)
                                COEF = MIN (DZ1, DZ2, 0.5) * DR
                                CONC(ISYS, IL) = CONC(ISYS, IL) - COEF / DZ1
                                CONC(ISYS, ILD) = CONC(ISYS, ILD) + COEF / DZ2
                            ENDIF
                        ENDIF
                    end do
                    IF (IFIL == 0) GOTO 30
                end do
                IF (IFIL == 1) THEN
                    IF (NOWARN < 1000) WRITE (LUNUT, 1010) ISYS, ISEG, ILAY
                    NOWARN = NOWARN + 1
                ENDIF
                30       CONTINUE
            end do
        end do
        !
        9999 if (timon) call timstop (ithandl)
        RETURN
        !
        1010 FORMAT (' WARNING: Forester filter max. iterations reached for substance: ', &
                I2, '; segment: ', I6, '; layer: ', I2, ' !')
        !
    END

end module m_dlwqd2
