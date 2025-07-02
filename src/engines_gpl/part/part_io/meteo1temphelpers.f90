!!  Copyright (C)  Stichting Deltares, 2012-2025.
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

!! NOTE: subroutines below are 'undressed' routines from Unstruc.
!! Long term: EC-module should replace meteo1.f90 (and be free of all these cross-dependencies)


module m_missing_meteo
    implicit none
    double precision :: dmiss = -999d0   !
    double precision :: xymis = -999d0   !
    double precision :: dxymis = -999d0
end module m_missing_meteo


module m_meteo1temphelpers
    implicit none

contains
    !> Opens an existing file for reading.
    !!
    !! When file does not exist or is already open, program stops with
    !! an error message.
    subroutine oldfil(minp, filename)
        implicit none
        integer, intent(out) :: minp     !< New file pointer to opened file.
        character(*), intent(in) :: filename !< Name of the file to open.

        integer :: istat_
        integer :: i
        integer :: l2, l1
        integer :: l3
        logical :: jawel

        istat_ = 0

        l1 = max(1, ifirstchar(filename))
        l2 = len_trim(filename)
        if (l2==0) then
            write (*, *) 'Filename is empty'
            istat_ = 1 ! ERR_FILENOTEXIST
            goto 999
        endif
        inquire (file = filename(l1:l2), exist = jawel)
        if (jawel) then
            open (newunit = minp, file = filename(l1:l2))
            write (*, *) 'Opened file :', filename(l1:l2)
        elseif (ifirstchar(filename)==0) then
            write (*, *) 'Filename is empty'
            istat_ = 1 ! ERR_FILENOTEXIST
            goto 999
        else
            write (*, *) 'File: ', filename(l1:l2), ' does not exist'
            istat_ = 1 ! ERR_FILENOTEXIST
            goto 999
        endif
        return

        999 continue
        ! Upon error, reset file pointer.
        if (istat_ /= 0) then
            minp = 0
        end if
        !    if (present(istat)) then
        !        istat = istat_
        !        return
        !    endif
    end subroutine oldfil


    !> Opens a new file for writing (and reading).
    !!
    !! When file already exists, it will be overwritten.
    !! When access is denied, program stops with an error message.
    subroutine newfil(minp, filename)!, istat)
        implicit none
        integer, intent(out) :: minp     !< New file pointer to opened file.
        character(*), intent(in) :: filename !< Name of the file to open.

        integer :: istat_
        integer :: i
        integer :: l2, l1
        integer :: l3
        character(*) RW*20

        istat_ = 0

        l1 = max(1, ifirstchar(filename))
        l2 = len_trim(filename)
        if (l2==0) then
            write (*, *) 'filename is empty'
            istat_ = 1 ! ERR_FILENOTEXIST
            goto 999
        endif

        open (newunit = minp, file = filename(l1:l2), action = 'readwrite', IOSTAT = istat_)
        inquire(minp, readwrite = rw)
        IF (istat_ > 0 .or. trim(rw)/='YES') THEN
            write (*, *) 'File: ', filename(l1:l2), ' could not be opened for writing.'
            istat_ = 1 ! ERR_FILEACCESSDENIED
            goto 999
        end if

        write (*, *) 'Opened file : ', filename(l1:l2)
        return

        999 continue
        !    if (present(istat)) then
        !        istat = istat_
        !        return
        !    endif
    end subroutine newfil


    !> Closes a filepointer with proper bookkeeping.
    subroutine doclose(minp)
        implicit none
        integer, intent(inout) :: minp
        if (minp == 0) return
        close (minp)
        minp = 0
    end subroutine doclose


    !> Error when reading incorrectly formatted data from file.
    subroutine readerror(w1, w2, minp)
        implicit none
        integer, intent(in) :: minp
        character(*), intent(in) :: w1
        character(*), intent(in) :: w2

        write (*, *) w1, w2, ' in file unit ', minp
    end subroutine readerror

    !> Error when a premature EOF is encountered.
    subroutine eoferror(minp)
        implicit none
        integer, intent(in) :: minp

        write(*, *) 'unexpected end of file in file unit ', minp
    end subroutine eoferror


    subroutine zoekja(minp, rec, text, ja)
        implicit none
        integer, intent(out) :: ja
        integer, intent(in) :: minp
        character(*) :: rec
        character(*), intent(in) :: text

        !
        !     voorwaarts zoeken, geen error als mislukt
        write (*, '(a,a)') 'looking for keyword: ', trim(text)
        10 continue
        read (minp, '(a)', end = 999) rec
        if (rec(1:1) == '*') goto 10
        if (index(rec, trim(text))/=0) then
            ja = 1
            return
        endif
        goto 10
        999 continue
        write (*, *) 'keyword', trim(text), 'NOT found!'
        ja = 0
    end subroutine zoekja


    !> Searches for an optional keyword on current line and returns the text value.
    !! 'key=text'. Rewinds the file pointer to the original line.
    subroutine zoekopt(minp, value, key, ja)
        implicit none
        integer, intent(out) :: ja    !< Whether key was found or not.
        integer, intent(in) :: minp  !< File pointer
        character(*), intent(out) :: value !< value behind '=' character.
        character(*), intent(in) :: key   !<

        character(len = 255) :: rec
        integer :: l1

        ja = 0

        read (minp, '(a)', end = 999) rec
        if (index(rec, trim(key)) /= 0) then
            ja = 1
            l1 = index(rec, '=') + 1
            read(rec(l1:), *) value
            write (*, *) 'Found optional keyword', trim(key)
            return
        else
            backspace(minp)
        endif

        999 continue
    end subroutine zoekopt


    function ifirstchar(rec)
        implicit none
        !
        ! Global variables
        !
        integer :: ifirstchar
        character(*), intent(in) :: rec
        !
        !
        ! Local variables
        !
        integer :: i
        !
        !
        !! executable statements -------------------------------------------------------
        !
        !
        do i = 1, len(rec)
            if (index(rec(i:i), ' ')==0) then
                ifirstchar = i
                return
            endif
        enddo
        ifirstchar = 0
    end function ifirstchar

    function ifirstletter(rec)
        implicit none
        !
        ! Global variables
        !
        integer :: ifirstletter
        character(*) :: rec
        !
        !
        ! Local variables
        !
        integer :: i
        integer :: i1
        integer :: i2
        integer :: i3
        integer :: len_trim
        integer :: l
        !
        !
        !! executable statements -------------------------------------------------------
        !
        !
        !     geeft positie van eerste letter, niet nummer
        l = len_trim(rec)
        ifirstletter = 0
        do i = 1, l
            i1 = index('qwertyuiopasdfghjklzxcvbnm', rec(i:i))
            i2 = index('QWERTYUIOPASDFGHJKLZXCVBNM', rec(i:i))
            i3 = max(i1, i2)
            if (i3/=0) then
                ifirstletter = i
                return
            endif
        enddo
    end function ifirstletter


    ! Own implementation of Interacter's ilocatestring
    subroutine ilocatestring(rec, i1, i2)
        implicit none
        character(len = *), intent(in) :: rec
        integer, intent(out) :: i1, i2

        i1 = ifirstchar(rec)
        i2 = index(rec(i1:), ' ')
        if (i2 == 0) then
            i2 = len(rec)
        else
            i2 = i1 + i2 - 2
        end if
    end subroutine ilocatestring

    integer function julday(mm, id, iyyy)
        implicit none
        integer :: igreg
        integer :: mm, id, iyyy
        integer :: jy, jm, ja
        parameter (igreg = 15 + 31 * (10 + 12 * 1582))
        !     if (iyyy==0) pause 'there is no year zero.'
        if (iyyy<0) iyyy = iyyy + 1
        if (mm>2) then
            jy = iyyy
            jm = mm + 1
        else
            jy = iyyy - 1
            jm = mm + 13
        endif
        julday = int(365.25 * jy) + int(30.6001 * jm) + id + 1720995
        if (id + 31 * (mm + 12 * iyyy)>=igreg) then
            ja = int(0.01 * jy)
            julday = julday + 2 - ja + int(0.25 * ja)
        endif
        return
    end function julday

    !> Checks whether lines 1-2 and 3-4 intersect.
    !! @param[in] x1,y1,x2,y2,x3,y3,x4,y4 x- and y-coords of line endpoints.
    !! @param[out] jacros 1 if lines cross (intersect), 0 if not.
    !! @param[out] sl lambda in [0,1] on line segment 1-2 (outside [0,1] if no intersection). Unchanged if no intersect!!
    !! @param[out] sm lambda in [0,1] on line segment 3-4 (outside [0,1] if no intersection). Unchanged if no intersect!!
    !! @param[out] xcr,ycr x-coord. of intersection point.
    SUBROUTINE CROSS(x1, y1, x2, y2, x3, y3, x4, y4, JACROS, SL, SM, XCR, YCR, CRP)
        use m_missing_meteo
        implicit none
        double precision :: crp
        double precision :: det
        double precision :: eps
        integer :: jacros
        double precision :: sl
        double precision :: sm
        double precision, intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
        double precision :: x21, y21, x31, y31, x43, y43, xcr, ycr

        ! Set defaults for no crossing at all:
        JACROS = 0
        EPS = 0.00001d0
        SL = DMISS
        SM = DMISS
        !     SL     = LABDA TUSSEN 0 EN 1 OP EERSTE PAAR
        !     Sm     = LABDA TUSSEN 0 EN 1 OP TWEEDE PAAR
        X21 = getdx(x1, y1, x2, y2)
        Y21 = getdy(x1, y1, x2, y2)
        X43 = getdx(x3, y3, x4, y4)
        Y43 = getdy(x3, y3, x4, y4)
        X31 = getdx(x1, y1, x3, y3)
        Y31 = getdy(x1, y1, x3, y3)

        DET = X43 * Y21 - Y43 * X21
        IF (ABS(DET) < EPS) THEN
            RETURN
        ELSE
            SM = (Y31 * X21 - X31 * Y21) / DET
            IF (ABS(X21) > EPS) THEN
                SL = (SM * X43 + X31) / X21
            ELSE IF (ABS(Y21) > EPS) THEN
                SL = (SM * Y43 + Y31) / Y21
            ELSE
                SL = 0d0
            ENDIF
            IF (SM >= 0d0 .AND. SM <= 1d0 .AND. &
                    SL >= 0d0 .AND. SL <= 1d0) THEN
                JACROS = 1
            ENDIF
            XCR = X1 + SL * (X2 - X1)
            YCR = Y1 + SL * (Y2 - Y1)
            CRP = -DET
        ENDIF
        RETURN
    END subroutine cross

    SUBROUTINE CROSSPOLY(xa, ya, xb, yb, xpl, ypl, npl, XM, YM, CRPM, JA)
        implicit none
        integer :: npl, ja
        DOUBLE PRECISION :: xa, xb, ya, yb, xm, ym, crpm
        DOUBLE PRECISION :: xpl(npl), ypl(npl)

        integer :: jacros
        integer :: k
        integer :: k1
        integer :: k2
        integer :: ku
        DOUBLE PRECISION :: XP1, YP1, XP2, YP2, SL, SM, XCR, YCR, CRP

        JA = 0
        DO K = 1, NPL - 1
            KU = K + 1
            XP1 = XPL(K) ; YP1 = YPL(K)
            XP2 = XPL(KU) ; YP2 = YPL(KU)
            CALL CROSS (XP1, YP1, XP2, YP2, Xa, Ya, Xb, Yb, jacros, SL, SM, XCR, YCR, CRP)
            if (jacros == 1) then
                JA = JA + 1
                XM = XCR
                YM = YCR
                crpm = crp
            end if
        ENDDO

    END SUBROUTINE CROSSPOLY


    double precision function getdx(x1, y1, x2, y2)
        implicit none
        double precision :: x1, y1, x2, y2
        getdx = x2 - x1
    end function getdx

    double precision function getdy(x1, y1, x2, y2)
        implicit none
        double precision :: x1, y1, x2, y2
        getdy = y2 - y1
    end function getdy

    double precision function dbdistance(x1, y1, x2, y2)                  ! distance point 1 -> 2
        use m_missing_meteo
        implicit none
        double precision :: x1, y1, x2, y2
        ! locals
        double precision :: ddx, ddy, rr

        ddx = getdx(x1, y1, x2, y2)
        ddy = getdy(x1, y1, x2, y2)
        rr = ddx * ddx + ddy * ddy
        if (rr == 0d0) then
            dbdistance = 0d0
        else
            dbdistance = sqrt(rr)
        endif
    end function dbdistance

    SUBROUTINE PINPOK(XL, YL, N, X, Y, INSIDE)
        USE m_missing_meteo
        implicit none
        integer :: N, INSIDE
        double precision :: X(N), Y(N), XL, YL

        integer :: i, i1, i2, np, jins
        double precision :: rechts, x1, x2, y1, y2, rm, rl
        jins = 1 ! Don't use 'invert selection' now

        IF (N <= 2) THEN
            INSIDE = 1 ; IF (jins == 0) INSIDE = 1 - INSIDE
        ELSE
            NP = 0
            5    CONTINUE
            NP = NP + 1
            IF (NP <= N) THEN
                IF (X(NP) /= dmiss) GOTO 5
            ENDIF
            NP = NP - 1
            INSIDE = 0
            RECHTS = 0
            I = 0
            10    CONTINUE
            I1 = I + 1  !MOD(I,NP) + 1
            I2 = I1 + 1 !MOD(I1,NP) + 1
            IF (I2 > NP) I2 = 1
            X1 = X(I1)
            X2 = X(I2)
            Y1 = Y(I1)
            Y2 = Y(I2)
            IF (XL >= MIN(X1, X2) .AND. XL <= MAX(X1, X2)) THEN
                !           tussen of op lijnstuk
                IF (XL == X1 .AND. YL == Y1 .OR.                                     &
                        !              op punt 1
                        (X1 == X2 .AND. YL >= MIN(Y1, Y2) .AND. YL <= MAX(Y1, Y2)) .OR.  &
                        !              op verticale lijn
                        (YL == Y1 .AND. Y1 == Y2)) THEN
                    !              op horizontale lijn
                    INSIDE = 1 ; IF (jins == 0) INSIDE = 1 - INSIDE
                    RETURN
                ELSE IF (X1 /= X2) THEN
                    !              scheve lijn
                    RL = (XL - X1) / (X2 - X1)
                    RM = (Y1 - YL) + RL * (Y2 - Y1)
                    IF (RM == 0) THEN
                        !                 op scheve lijn
                        INSIDE = 1 ; IF (jins == 0) INSIDE = 1 - INSIDE
                        RETURN
                    ELSE IF (RM > 0d0) THEN
                        !                 onder scheve lijn
                        IF (XL == X1 .OR. XL == X2) THEN
                            IF (X1 > XL .OR. X2 > XL) THEN
                                RECHTS = RECHTS + 1
                            ENDIF
                        ENDIF
                        INSIDE = 1 - INSIDE
                    ENDIF
                ENDIF
            ENDIF
            I = I + 1
            IF (I < NP) GOTO 10
            IF (MOD(RECHTS, 2d0) /= 0) INSIDE = 1 - INSIDE
        ENDIF
        IF (jins == 0) INSIDE = 1 - INSIDE
        RETURN
    END SUBROUTINE PINPOK

end module m_meteo1temphelpers
