!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

module m_fm_aux_routines

    implicit none

contains

    !> determine sample bounding box
    subroutine get_samples_boundingbox()
        use m_samples
        use m_missing
        implicit none

        integer :: i

        xsammin = huge(1d0)
        xsammax = -huge(1d0)
        ysammin = huge(1d0)
        ysammax = -huge(1d0)

        do i = 1, NS
            if (xs(i)/=DMISS .and. ys(i)/=DMISS .and. zs(i)/=DMISS) then
                xsammin = min(xsammin, xs(i))
                xsammax = max(xsammax, xs(i))
                ysammin = min(ysammin, ys(i))
                ysammax = max(ysammax, ys(i))
            end if
        end do
    end subroutine get_samples_boundingbox


    !> return common node of links L1 and L2
    subroutine find_common_node(L1, L2, node)

        use network_data
        use m_missing

        implicit none

        integer, intent(in) :: L1, L2           !< links
        integer, intent(out) :: node             !< common node

        integer, dimension(4) :: a                ! dummy array with nodes of L1 and L2
        ! integer, parameter    :: IMISS = -999999

        a(1:2) = kn(1:2, L1)
        a(3:4) = kn(1:2, L2)

        do
            node = IMISS

            if (a(1)==a(3) .or. a(1)==a(4)) node = a(1)
            if (a(2)==a(3) .or. a(2)==a(4)) node = a(2)

            if (node/=IMISS) exit

            write(*, *) 'find_common_node: no common node found'
            exit
        end do

    end subroutine find_common_node


    subroutine dlinedis2(x3, y3, x1, y1, x2, y2, ja, dis, xn, yn, rl)
        use m_sferic
        use geometry_module, only: getdx, getdy, dbdistance, sphertocart3d, cart3dtospher
        use m_missing, only: dmiss

        implicit none
        integer :: ja
        double precision :: x1, y1, x2, y2, x3, y3, dis, xn, yn, zn, d2
        double precision :: r2, rl, x21, y21, z21, x31, y31, z31
        double precision :: xx1, xx2, xx3, yy1, yy2, yy3, zz1, zz2, zz3, xxn, yyn, zzn

        ja = 0

        if (jsferic == 0 .or. jasfer3d == 0) then

            x21 = getdx(x1, y1, x2, y2, jsferic)
            y21 = getdy(x1, y1, x2, y2, jsferic)
            x31 = getdx(x1, y1, x3, y3, jsferic)
            y31 = getdy(x1, y1, x3, y3, jsferic)
            r2 = dbdistance(x2, y2, x1, y1, jsferic, jasfer3d, dmiss)
            r2 = r2 * r2
            if (r2 /= 0) then
                rl = (x31 * x21 + y31 * y21) / r2
                if (0d0 <= rl .and. rl <= 1d0) then
                    ja = 1
                endif
                xn = x1 + rl * (x2 - x1)
                yn = y1 + rl * (y2 - y1)
                dis = dbdistance(x3, y3, xn, yn, jsferic, jasfer3d, dmiss)
            else
                dis = dbdistance(x3, y3, x1, y1, jsferic, jasfer3d, dmiss)
            endif

        else

            call sphertocart3d(x1, y1, xx1, yy1, zz1)
            call sphertocart3d(x2, y2, xx2, yy2, zz2)
            call sphertocart3d(x3, y3, xx3, yy3, zz3)

            x21 = xx2 - xx1
            y21 = yy2 - yy1
            z21 = zz2 - zz1
            x31 = xx3 - xx1
            y31 = yy3 - yy1
            z31 = zz3 - zz1

            r2 = x21 * x21 + y21 * y21 + z21 * z21
            if (r2 /= 0d0) then
                rl = (x31 * x21 + y31 * y21 + z31 * z21) / r2
                if (0d0 <= rl .and. rl <= 1d0) then
                    ja = 1
                endif
                xxn = xx1 + rl * x21
                yyn = yy1 + rl * y21
                zzn = zz1 + rl * z21
                x31 = xxn - xx3
                y31 = yyn - yy3
                z31 = zzn - zz3
                dis = sqrt(x31 * x31 + y31 * y31 + z31 * z31)

                call cart3dtospher(xxn, yyn, zzn, xn, yn, maxval((/x1, x2, x3/)))
            else
                dis = dbdistance(x3, y3, x1, y1, jsferic, jasfer3d, dmiss)
            endif

        endif

    end subroutine dlinedis2


    subroutine dlinedis3D(xx3, yy3, zz3, xx1, yy1, zz1, xx2, yy2, zz2, JA, DIS, xxn, yyn, zzn, rl)

        implicit none

        integer :: ja
        double precision :: dis, xn, yn, zn, d2
        double precision :: r2, rl, x21, y21, z21, x31, y31, z31
        double precision :: xx1, xx2, xx3, yy1, yy2, yy3, zz1, zz2, zz3, xxn, yyn, zzn

        !  korste afstand tot lijnelement
        ja = 0
        x21 = xx2 - xx1
        y21 = yy2 - yy1
        z21 = zz2 - zz1
        x31 = xx3 - xx1
        y31 = yy3 - yy1
        z31 = zz3 - zz1
        r2 = x21 * x21 + y21 * y21 + z21 * z21
        if (r2 /= 0d0) then
            rl = (x31 * x21 + y31 * y21 + z31 * z21) / r2
            if (0d0 <= rl .and. rl <= 1d0) then
                ja = 1
            endif
            xxn = xx1 + rl * x21
            yyn = yy1 + rl * y21
            zzn = zz1 + rl * z21
            x31 = xxn - xx3
            y31 = yyn - yy3
            z31 = zzn - zz3
            dis = sqrt(x31 * x31 + y31 * y31 + z31 * z31)
        else
            dis = 0d0
        endif

    end subroutine dlinedis3d


    SUBROUTINE REASAM(MSAM, JADOORLADEN)
        USE M_MISSING
        USE M_SAMPLES
        use m_alloc
        use MessageHandling
        use m_meteo1temphelpers
        use stdlib_sorting, only: sort_index
        implicit none
        integer, intent(inout) :: msam        !< already opened file pointer to sample file
        integer, intent(in) :: jadoorladen !< whether to append to global set (1) or start empty (0)
        integer :: ierr
        integer :: jflow
        integer :: jqn
        integer :: mcs
        integer :: ncs
        integer :: ndraw
        integer :: nkol
        integer :: nrow
        integer :: ns1
        integer :: nsm
        integer :: num
        integer :: K, K0
        double precision :: x, y, z
        double precision :: XX, YY, ZZ, ZZ2
        double precision, allocatable :: xs_copy(:)

        COMMON /PHAROSFLOW/  JFLOW
        COMMON /PHAROSLINE/  REC1
        COMMON /SAMPLESADM/  MCS, NCS, NS1
        COMMON /QNRGF/ JQN
        COMMON /DRAWTHIS/ ndraw(50)

        CHARACTER(len = 132) :: REC, REC1
        CHARACTER(len = 10) :: TEX

        CALL SAVESAM()
        NSM = 0
        MXSAM = 0
        MYSAM = 0
        IPSTAT = IPSTAT_NOTOK
        nkol = 0
        11 READ (MSAM, '()', END = 31)
        NSM = NSM + 1
        GOTO 11
        31 NSMAX = 1.2d0 * (NSM + JADOORLADEN * NS)
        IF (ALLOCATED (XS)) DEALLOCATE (XS, YS, ZS)
        ALLOCATE (XS(NSMAX), YS(NSMAX), ZS(NSMAX), STAT = IERR)
        IF (IERR/=0) CALL MESS(LEVEL_ERROR, 'XS(NSMAX),YS(NSMAX),ZS(NSMAX)', IERR, NSMAX)
        if (allocated(ipsam)) deallocate(ipsam)
        allocate(ipsam(NSMAX), stat = ierr)
        IF (IERR/=0) CALL MESS(LEVEL_ERROR, 'ipsam(NSMAX)', ierr, NSMAX)

        REWIND(MSAM)

        WRITE(TEX, '(I10)') NSM
        IF (JADOORLADEN == 0) THEN
            XS = dmiss
            YS = dmiss
            ZS = dmiss
            K = 0
        ELSE
            CALL RESTORESAM()
            K = NS
            NS1 = NS
        ENDIF
        K0 = K

        !    check of dit een PHAROS file is
        JFLOW = 1
        14 READ (MSAM, '(A)', END = 30) REC1
        if (rec1(1:1) == '*') goto 14

        IF (.NOT. (THISISANUMBER(REC1))) THEN
            READ (MSAM, '(A)', END = 30) REC
            IF (THISISANUMBER(REC)) THEN
                READ (REC, *, ERR = 16) NROW, NKOL
                GOTO 15
                16       CONTINUE
                READ (MSAM, '(A)', END = 30) REC
                READ (REC, *, ERR = 15) NUM, X, Y, Z
                JFLOW = 3
            ENDIF
        ENDIF
        15 CONTINUE

        REWIND (MSAM)

        KMOD = max(1, NSM / 100)
        10 CONTINUE
        READ (MSAM, '(A)', END = 30) REC
        IF (REC(1:1) == '*') GOTO 10
        IF (.NOT. (THISISANUMBER(REC))) THEN
            !        we nemen aan dat er net een blokcode is gelezen
            !        en we lezen meteen de nrow ncol regel, maar checken die regel niet
            READ  (MSAM, '(A)', END = 30) REC
        ELSE

            IF (JFLOW == 3) THEN
                READ (REC, *, ERR = 40) NUM, XX, YY, ZZ
            ELSE IF (NKOL == 4) THEN
                READ (REC, *, ERR = 40) XX, YY, ZZ, ZZ2
                if (zz /= -999d0) then
                    zz = sqrt(zz * zz + zz2 * zz2)
                endif
            ELSE
                READ (REC, *, end = 40) XX, YY, ZZ
                READ (REC, *, ERR = 40) XX, YY, ZZ
            ENDIF

            IF (K  <= NSMAX - 1 .AND. XX /= XYMIS .AND.   &
                    ZZ /= dmiss .AND. ZZ /= 999.999d0 .and. &
                    .not.(isnan(XX) .or. isnan(YY) .or. isnan(ZZ))) THEN
                K = K + 1
                NS = K
                XS(K) = XX
                YS(K) = YY
                ZS(K) = ZZ
            ENDIF
        ENDIF
        GOTO 10

        40 CONTINUE
        WRITE(TEX, '(I10)') K
        CALL MESS(LEVEL_ERROR, 'ERROR READING SAMPLES FILE LINE NR ', TEX, REC)

        30 CONTINUE
        IF (K > NSMAX) THEN
            WRITE(TEX, '(I8)') NSMAX
            CALL MESS(LEVEL_WARN, 'ONLY', TEX, 'SAMPLE POINTS CAN BE LOADED')
            WRITE(TEX, '(I8)') K
            CALL MESS(LEVEL_ERROR, 'YOU TRIED TO LOAD', TEX, 'SAMPLE POINTS')
        ENDIF
        WRITE(TEX, '(I10)') NS
        IF (NS > 1) THEN
            xs_copy = XS(1:NS)
            call sort_index(xs_copy, IPSAM(1:NS))
            call get_samples_boundingbox()
            IPSTAT = IPSTAT_OK
        END IF
        call doclose(MSAM)

    END subroutine reasam


    !>    delete samples
    !>      jaconfirm=0: do not prompt for confirmation,       keep arrays,        make copy
    !>                1:        prompt for confirmation,       keep arrays,        make copy
    !>               -1: do not prompt for confirmation, deallocate arrays, do not make copy
    SUBROUTINE DELSAM(JACONFIRM)      ! SPvdP: need promptless delsam in orthogonalisenet
        USE M_SAMPLES
        use m_polygon
        USE m_missing
        use geometry_module, only: dbpinpol

        implicit none

        integer, intent(in) :: JACONFIRM  !< prompt for confirmation (1) or not (0)

        integer :: i
        integer :: inhul
        integer :: ja
        integer :: k
        integer :: key
        integer :: nsol
        double precision :: rd
        double precision :: xi
        double precision :: yi

        if (jaconfirm == -1) then
            if (nsmax > 0) then
                nsmax = 0 ; ns = 0
                if (allocated(xs)) deallocate (xs, ys, zs)
                if (allocated(ipsam)) deallocate(ipsam)
            endif
            return
        endif

        IF (Npl <= 2) THEN
            JA = 1
            IF (JA == 0) THEN
                KEY = 0
                RETURN
            ENDIF
            CALL SAVESAM()
            DO I = 1, NS
                XS(I) = DMISS
                YS(I) = DMISS
                ZS(I) = DMISS
                ipsam(i) = 0
            end do
            NS = 0
            RETURN
        ENDIF
        ! Else: check in polygon
        CALL SAVESAM()
        INHUL = -1
        DO I = 1, NS
            RD = ZS(I)
            XI = XS(I)
            YI = YS(I)
            CALL DBPINPOL(xI, yI, INHUL, dmiss, JINS, NPL, xpl, ypl, zpl)
            IF (INHUL == 1) ZS(I) = dmiss
        enddo

        K = 0
        NSOL = NS
        DO I = 1, NS
            IF (ZS(I) /= dmiss) THEN
                K = K + 1
                XS(K) = XS(I)
                YS(K) = YS(I)
                ZS(K) = ZS(I)
                ipsam(k) = ipsam(i)
            ENDIF
        enddo
        NS = K

        DO I = NS + 1, NSOL
            XS(I) = DMISS
            YS(I) = DMISS
            ZS(I) = DMISS
            ipsam(i) = 0
        enddo

    END SUBROUTINE DELSAM


    function thisisanumber(rec)
        use string_module, only: find_first_char
        implicit none
        !
        ! Global variables
        !
        logical :: thisisanumber
        character(len = *), intent(in) :: rec
        !
        !
        ! Local variables
        !
        integer :: ich
        integer :: l
        !
        !
        !! executable statements -------------------------------------------------------
        !
        !
        !     is waar als eerste character van rec een getal is.
        l = find_first_char(rec)
        if (l==0) then
            thisisanumber = .false.
        else
            ich = ichar(rec(l:l))
            if (ich==43 .or. ich==45 .or. ich==46 .or. ich>=48 .and. ich<=57) then
                thisisanumber = .true.
            else
                thisisanumber = .false.
            endif
        endif
    end function thisisanumber

end module m_fm_aux_routines
