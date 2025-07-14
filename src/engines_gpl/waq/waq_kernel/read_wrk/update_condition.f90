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
module m_update_condition
    use m_waq_precision
    use timers

    use m_dlwqt4
    use m_dlwqt3

    implicit none

    private
    public :: update_condition

contains

    !! Makes values at ITIME for time dependent aspects
    subroutine update_condition(file_unit_list, itime, itimel, iharm, harmat, &
            farray, ipoint, result, nosub, nrharm, &
            ntot, nrftot, ipa, iph, ipf, &
            ipi, luntxt, is, isflag, ifflag, &
            update, newset, ioff, iwork, lstrec, &
            lrewin, reclst, ftype, dlwqd)

        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     file_unit_list     INTEGER       *     INPUT   unit numbers
        !     ITIME   INTEGER       1     INPUT   Model timer
        !     ITIMEL  INTEGER       1     INPUT   Model timer previous time step
        !     IHARM   INTEGER   NRHARM    IN/OUT  integer harmonics space
        !                           *     INPUT   integer array space new version
        !     HARMAT  REAL    (NRHARM,*)  INPUT   matrix with harmonic info
        !                           *     INPUT   real array space new version
        !     FARRAY  REAL    (NRFTOT,2)  INPUT   double file buffer
        !     IPOINT  INTEGER   NTOT+3    INPUT   pointer to result array + ...
        !                                 INPUT   type definition of items
        !     RESULT  REAL          *     OUTPUT  result array at time ITIME
        !     NOSUB   INTEGER       1     INPUT   amount of values per item
        !     NRHARM  INTEGER       1     INPUT   amount of harmonic records
        !     NTOT    INTEGER       1     INPUT   number of items to be filled
        !     NRFTOT  INTEGER       1     INPUT   record lengt file
        !     IPA     INTEGER       1     IN/OUT  pointer in FARRAY
        !                                 INPUT   array space IHARM (new version)
        !     IPH     INTEGER       1     IN/OUT  pointer in HARMAT
        !                                 INPUT   array space HARMAT (new version)
        !     IPF     INTEGER       1     IN/OUT  pointer in IHARM
        !     IPI     INTEGER       1     IN/OUT  pointer in IPOINT
        !     LUNTXT  CHAR*(*)      ?     INPUT   txt with the unit numbers
        !     IS      INTEGER       1     INPUT   offset in file_unit_list and LUNTXT
        !     ISFLAG  INTEGER       1     INPUT   = 1 then 'ddhhmmss' format
        !     IFFLAG  INTEGER       1     INPUT   = 1 then first invocation
        !     UPDATE  LOGICAL       1     OUTPUT  set to T if function is updated
        !                                         else set to F
        !     NEWSET  LOGICAL       1     INPUT   T if new function processing
        !     IOFF    INTEGER       1     LOCAL   offset in the concentration array
        !     IWORK   INTEGER       *     LOCAL   workspace
        !     LSTREC  LOGICAL       1     INPUT   Switch last record on rewind wanted
        !     LREWIN  LOGICAL       1     OUTPUT  Then rewind took place
        !     RECLST  REAL          *     OUTPUT  Last record before rewind

        use m_dlwqt2
        use m_dlwqib
        use m_logger_helper, only: stop_with_error
        use m_open_waq_files
        use delwaq2_data

        integer(kind = int_wp), intent(in) :: ftype  (*) !< type of files to be opened
        type(delwaq_data), intent(inout) :: dlwqd      !< derived type for persistent storage

        integer(kind = int_wp) :: IHARM (*), IPOINT(*), file_unit_list   (*), IWORK (*)
        real(kind = real_wp) :: HARMAT(*), FARRAY(*), RESULT(*), RECLST(*)
        character(len = *) LUNTXT(*)
        character(len = 12)  CHLP
        LOGICAL       UPDATE, NEWSET, LSTREC, LREWIN
        integer(kind = int_wp) :: IPA, IPH, IPF, ITIME, ITIMEL, NOSUB, NRHARM, &
                NTOT, NRFTOT, IS, ISFLAG, IFFLAG, IOFF, IPI
        !
        !     Local
        !
        LOGICAL       UPDATH, UPDATB
        LOGICAL       ONLINE

        !     Common to define external communications in SOBEK
        !     OLCFWQ             Flag indicating ONLINE running of CF and WQ
        !     SRWACT             Flag indicating active data exchange with SRW
        !     RTCACT             Flag indicating output for RTC

        LOGICAL            OLCFWQ, SRWACT, RTCACT
        COMMON /COMMUN/    OLCFWQ, SRWACT, RTCACT

        integer(kind = int_wp) :: ierr, ioerr, ipsi, ipsa, ipb, k, i, i2, j2
        integer(kind = int_wp) :: ntotal, nospac, npoint

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("update_condition", ithandl)
        !
        !         Prescribe ONLINE mode for selected files
        !
        ONLINE = .FALSE.
        IF (OLCFWQ .OR. SRWACT) &
                ONLINE = (IS==11 .OR. IS==7 .OR. IS==10)
        !
        !         If NRHARM =  0 and NRFTOT= 0, one record per time step,
        !                                       no harmonics and interpolation.
        UPDATE = .FALSE.
        NTOTAL = NOSUB * NTOT
        IERR = 0
        IF (NRHARM + NRFTOT >  0) GOTO 10
        IF (NTOTAL > 0) THEN
            IF (IFFLAG == 1) THEN
                IF (.NOT. NEWSET) THEN
                    CALL open_waq_files (file_unit_list(IS), LUNTXT(IS), IS, 2 + ftype(is), IERR)
                    IF (IERR /= 0) THEN
                        WRITE(file_unit_list(19), *) 'ERROR in update_condition, opening file'
                        WRITE(file_unit_list(19), *) 'number  :', IS
                        WRITE(file_unit_list(19), *) 'file    :', LUNTXT(IS)
                        WRITE(file_unit_list(19), *) 'unit    :', file_unit_list(IS)
                        CALL stop_with_error()
                    ENDIF
                    READ (file_unit_list(IS), IOSTAT = IOERR) CHLP
                    IF (IOERR==0 .AND. CHLP(1:6) == ' 4.900') THEN
                        NEWSET = .TRUE.
                        goto 9999        !  RETURN
                    ELSE
                        CLOSE (file_unit_list(IS))
                        CALL open_waq_files (file_unit_list(IS), LUNTXT(IS), IS, 2 + ftype(is), IERR)
                    ENDIF
                ELSE
                    IPSI = IPA
                    IPSA = IPH
                    CALL DLWQIB (file_unit_list(IS), file_unit_list(19), HARMAT, IHARM, IS, &
                            IPA, IPH, IERR)
                    IPI = IPA - IPSI
                    IPA = IPSI
                    IPH = IPSA
                    CLOSE (file_unit_list(IS))
                    IF (IERR /= 0) THEN
                        WRITE(file_unit_list(19), *) 'ERROR in update_condition'
                        WRITE(file_unit_list(19), *) 'after call to DLWQIB'
                        CALL stop_with_error()
                    ENDIF
                ENDIF
            ENDIF
            !
            !        If new time setting processing, only take this:
            !
            IF (NEWSET) THEN
                IPB = IPH
                CALL update_boundary_waste_array (file_unit_list(19), IOFF, HARMAT, IHARM, IPA, &
                        IPH, IPI, ITIME, IPOINT, RESULT, &
                        IWORK, IERR)
                IF (IERR /= 0) THEN
                    WRITE(file_unit_list(19), *) 'ERROR in update_condition'
                    WRITE(file_unit_list(19), *) 'after call to update_boundary_waste_array'
                    CALL stop_with_error()
                ENDIF
                goto 9999        !  RETURN
            ENDIF

            CALL DLWQT2 (file_unit_list(IS), file_unit_list(19), ITIME, RESULT, NTOTAL, &
                    LUNTXT(IS), ISFLAG, IFFLAG, ONLINE)
            IF (IFFLAG == -1) THEN
                NRHARM = -1
                IFFLAG = 1
                CLOSE (file_unit_list(IS))
            ENDIF
            UPDATE = .TRUE.
        ELSE
            NRHARM = -1
        ENDIF
        goto 9999    !   return
        !
        !         first set result zero and evaluate the harmonic components
        !
        10 IF (IFFLAG == 1) THEN
            READ (file_unit_list(4)) (IPOINT(K), K = 1, NTOT + 3)
        ENDIF
        DO I = 1, NTOTAL
            RESULT(I) = 0.0
        end do
        !
        I2 = NRHARM + 1
        CALL DLWQT3 (ITIME, IHARM, HARMAT, HARMAT(I2), NRHARM, &
                NOSUB, NOSPAC, IPOINT, NPOINT, RESULT, &
                LUNTXT(3), file_unit_list(3), file_unit_list(19), ISFLAG, IFFLAG, &
                UPDATH)
        IF (UPDATH) UPDATE = .TRUE.
        !
        NPOINT = NPOINT + 1
        !
        !         then evaluate the block- and linear functions
        !
        I2 = NTOT + 1
        J2 = NRFTOT + 1
        !         5 arguments of integer and real array space removed
        !         opening of binary file moved inside DLWQT4         July 2002
        CALL DLWQT4 (file_unit_list, LUNTXT, ftype, file_unit_list(19), IS, &
                ITIME, RESULT, IPOINT(NPOINT), NOSUB, NRFTOT, &
                ISFLAG, IFFLAG, UPDATB, NTOTAL, LSTREC, &
                LREWIN, RECLST, dlwqd)
        IF (UPDATB) UPDATE = .TRUE.
        !
        !         update the pointers
        !
        IPH = IPH + NOSPAC + NRHARM
        IPF = IPF + NRHARM
        IPA = IPA + NRFTOT * 2
        IPI = IPI + NTOT + 3
        9999 if (timon) call timstop (ithandl)

    end subroutine update_condition


    !! Updates the boundary and waste arrays
    subroutine update_boundary_waste_array(file_unit, ioff, a, j, iipnt, irpnt, max_int_size, itime, ktype, aval, &
            ival, ierr)

        !     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
        !     ----    -----    ------     ------- -----------
        !     file_unit   INTEGER    1        INPUT   unit number monitoring file
        !     IOFF    INTEGER    1        INPUT   index of first concentration
        !     A       REAL       ?        INPUT   Real    boundary workspace
        !     J       INTEGER    ?        INPUT   Integer boundary workspace
        !     IIPNT   INTEGER    1        IN/OUT  Offset in integer array space
        !     IRPNT   INTEGER    1        IN/OUT  Offset in real array space
        !     max_int_size   INTEGER    1        INPUT   Maximum integer array size
        !     ITIME   INTEGER    1        INPUT   Time in units of the system clock
        !     KTYPE   INTEGER   NOITM     INPUT   Type of items
        !     AVAL    REAL    num_substances_total,NOITM OUTPUT  Values of the bounds/wastes
        !     IVAL    INTEGER num_substances_total,NOITM LOCAL   Count array for averages
        !     IERR    INTEGER    1        IN/OUT  error count

        real(kind = real_wp), PARAMETER :: TWOPI = 6.28319
        integer(kind = int_wp) :: J(*), KTYPE(*), IVAL(*)
        real(kind = real_wp) :: A(*), AVAL(*)
        integer(kind = int_wp) :: IERR, file_unit, IRPNT, max_int_size, ITIME, IOFF, IIPNT

        ! local
        real(kind = real_wp) :: missing_value, aa, ab, aphase, func
        integer(kind = int_wp) :: noitm, num_substances_total, nobrk
        integer(kind = int_wp) :: i, i1, i2, i3, ia, ib, ic, ij, ii
        integer(kind = int_wp) :: integration_id, ipro, iord, itim1, itim2
        integer(kind = int_wp) :: irec, idt, itimf, it1c, it2c, idtc, iperio
        integer(kind = int_wp) :: npnt, npst, ndim, ndst, ntt

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("update_boundary_waste_array", ithandl)

        missing_value = -999.
        !       number of items
        noitm = j(1)
        num_substances_total = j(2)
        ia = 0
        ij = 2
        !
        !       Determine switch for this block 1 = items,subst
        !                                       2 = subst,items
        !       Start of the loop over blocks
        !
        !       Determine parameters NPNT = nr of items
        !                            NPST = start of item nr's in the J-array
        !                            NDIM = nr of substances
        !                            NDST = start of subs nr's in the J-array
        !                            integration_id = option 1 and 2 at breakpoints etc.
        !                            IPRO = procedure (overrule or not)
        !
        10 ij = ij + 1
        iord = j(ij)
        ij = ij + 1
        if (iord == 1) then
            npnt = j(ij)
            npst = ij
            ndim = j(npst + npnt + 1)
            ndst = npst + npnt + 1
        endif
        if (iord == 2) then
            ndim = j(ij)
            ndst = ij
            npnt = j(ndst + ndim + 1)
            npst = ndst + ndim + 1
        endif
        ij = ij + ndim + npnt + 4
        integration_id = j(ij - 2)
        ipro = j(ij - 1)
        ntt = ndim * npnt
        !
        !       nr of breakpoints or harmonics
        !
        nobrk = j(ij)
        !
        !           setting of default values
        !
        if (npnt == 0) then
            do i2 = 1, ndim
                ia = ia + 1
                ib = ioff + j(ndst + i2)
                do i1 = 1, noitm
                    aval(ib) = a(ia)
                    ib = ib + num_substances_total
                end do
            end do
            ij = ij + 1
            goto 150
        endif

        ! integration_id = 1 : Block function , integration_id = 2 : Linearly interpolated
        if (integration_id == 1 .or. integration_id == 2) then
            !
            !           get the right time in the block
            !
            if (nobrk > 1) then
                itim1 = j(ij + 1)
                itim2 = j(ij + nobrk)
                idt = itim2 - itim1
                if (itime < itim1) then
                    irec = 1
                    itim1 = 0
                    itim2 = 1
                    idt = itim1 + itim2
                    goto 50
                endif
                itimf = itime
                if (itime >= itim2) &
                        itimf = itime - ((itime - itim2) / idt + 1) * idt
                !
                !           make interpolation constants if integration_id = 2
                !
                do i = 2, nobrk
                    if (j(ij + i) > itimf) then
                        if (integration_id == 2) then
                            itim1 = itimf - j(ij + i - 1)
                            itim2 = j(ij + i) - itimf
                        else
                            itim1 = 0
                            itim2 = 1
                        endif
                        idt = itim1 + itim2
                        irec = i - 1
                        goto 50
                    endif
                end do
            else
                irec = 1
                itim2 = 1
                itim1 = 0
                idt = 1
            endif
            !
            !           set or interpolate the correct values
            !
            50    i = ia + (irec - 1) * ntt
            !           Inner loop in A over the substances
            if (iord == 1) then
                do i1 = 1, npnt
                    ii = j(npst + i1)
                    ib = (ii - 1) * num_substances_total
                    do i2 = 1, ndim
                        ic = ioff + j(ndst + i2)
                        !
                        !                 ignore negative indexes (0 equal to flow wastes??)
                        !
                        if (ic >= 0) then
                            i = i + 1
                            aa = a(i)
                            if (nobrk > 1) then
                                ab = a(i + ntt)
                            else
                                ab = 0.0
                            endif
                            it1c = itim1
                            it2c = itim2
                            idtc = idt
                            !     dealing with missing values
                            if (aa == missing_value .or. ab == missing_value) &
                                    call calculate_missing_values(a, i, missing_value, ntt, irec, &
                                            j, ij, nobrk, itimf, integration_id, &
                                            it1c, it2c, idtc, aa, ab)
                            !           if no value is found, then skip the assignment, except flow set missing
                            if (it1c /= 0 .or. it2c /= 0) then
                                !           make the wanted value
                                aa = (it2c * aa + it1c * ab) / idtc
                                if (ii > 0) then
                                    aval(ib + ic) = aa
                                else
                                    !              set a whole type
                                    do i3 = 1, noitm
                                        if (ktype(i3) == -ii) then
                                            aval ((i3 - 1) * num_substances_total + ic) = aa
                                        endif
                                    end do
                                endif
                            elseif (ic - ioff == 0) then
                                !                       for flow accept missing (detected flow)
                                if (ii > 0) then
                                    aval(ib + ic) = missing_value
                                else
                                    !                          set a whole type
                                    do i3 = 1, noitm
                                        if (ktype(i3) == -ii) then
                                            aval ((i3 - 1) * num_substances_total + ic) = missing_value
                                        endif
                                    enddo
                                endif

                            endif
                        else
                            !
                            !                    ignore value
                            !
                            i = i + 1
                        ENDIF
                    end do
                end do
                !           Inner loop in A over the items
            else
                do i1 = 1, ndim
                    ic = ioff + j(ndst + i1)
                    if (ic >= 0) then
                        do i2 = 1, npnt
                            i = i + 1
                            aa = a(i)
                            if (nobrk > 1) then
                                ab = a(i + ntt)
                            else
                                ab = 0.0
                            endif
                            it1c = itim1
                            it2c = itim2
                            idtc = idt
                            !     dealing with missing values
                            if (aa == missing_value .or. ab == missing_value) &
                                    call calculate_missing_values(a, i, missing_value, ntt, irec, &
                                            j, ij, nobrk, itimf, integration_id, &
                                            it1c, it2c, idtc, aa, ab)
                            !           if no value is found, then skip the assignment
                            if (it1c /= 0 .or. it2c /= 0) then
                                !           make the wanted value
                                aa = (it2c * aa + it1c * ab) / idtc
                                ii = j(npst + i2)
                                if (ii > 0) then
                                    ib = (j(npst + i2) - 1) * num_substances_total
                                    aval(ib + ic) = aa
                                else
                                    do i3 = 1, noitm
                                        if (ktype(i3) == -ii) &
                                                aval ((i3 - 1) * num_substances_total + ic) = aa
                                    end do
                                endif
                            elseif (ic - ioff == 0) then
                                !                       for flow accept missing (detected flow)
                                if (ii > 0) then
                                    ib = (j(npst + i2) - 1) * num_substances_total
                                    aval(ib + ic) = missing_value
                                else
                                    do i3 = 1, noitm
                                        if (ktype(i3) == -ii) &
                                                aval ((i3 - 1) * num_substances_total + ic) = missing_value
                                    enddo
                                endif
                            endif
                        end do
                    else
                        !
                        !                 ignore value
                        !
                        i = i + npnt
                    endif
                end do
            endif
            ij = ij + nobrk
            ia = ia + nobrk * ntt
        endif
        !
        !       integration_id = 3 and 4 : harmonics and fouriers, treated equally
        !
        if (integration_id == 3 .or. integration_id == 4) then
            !
            do i = 1, nobrk
                !
                !            harmonic function
                !
                ij = ij + 1
                iperio = j(ij)
                ia = ia + 1
                aphase = a(ia)
                if (i == 1) then
                    func = 1.0
                else
                    func = sin((real(itime) / iperio - aphase) * twopi)
                endif
                !
                !            multiply with amplitudes and set values
                !
                !              Inner loop in A over the substances
                if (iord == 1) then
                    do i1 = 1, npnt
                        ib = (j(npst + i1) - 1) * num_substances_total
                        do i2 = 1, ndim
                            ic = ioff + j(ndst + i2)
                            ia = ia + 1
                            if (i == 1) then
                                aval(ib + ic) = func * a(ia)
                            else
                                aval(ib + ic) = func * a(ia) + aval(ib + ic)
                            endif
                        end do
                    end do
                    !              inner loop in a over the items
                else
                    do i1 = 1, ndim
                        ic = ioff + j(ndst + i1)
                        do i2 = 1, npnt
                            ib = (j(npst + i2) - 1) * num_substances_total
                            ia = ia + 1
                            if (i == 1) then
                                aval(ib + ic) = func * a(ia)
                            else
                                aval(ib + ic) = func * a(ia) + aval(ib + ic)
                            endif
                        end do
                    end do
                endif
            end do
        endif
        !
        !       Return until finished
        !
        150 if (ij < max_int_size) goto 10
        if (ij == max_int_size) then
            iipnt = iipnt + ij
            irpnt = irpnt + ia
            goto 9999    !   return
        endif
        write (file_unit, 2010)
        ierr = ierr + 1
        9999 if (timon) call timstop (ithandl)
        return

        2010 FORMAT (' ERROR, updating time functions new style !')

    end subroutine update_boundary_waste_array

    subroutine calculate_missing_values(a, i, missing_value, ntt, irec, &
            j, ij, nobrk, itimf, integration_id, &
            it1c, it2c, idtc, aa, ab)

        real(kind = real_wp) :: a(*)
        integer(kind = int_wp) :: j(*)
        integer(kind = int_wp) :: i, ij, ntt, irec, nobrk, itimf, integration_id, it1c, it2c, idtc
        real(kind = real_wp) :: missing_value, aa, ab

        !  	local
        integer(kind = int_wp) :: ll, jj, kk

        integer(kind = int_wp) :: ithandl = 0

        if (timon) call timstrt ("calculate_missing_values", ithandl)
        ! search backward for the first valid point
        ll = i
        do jj = irec, 1, -1
            if (a(ll) /= missing_value) goto 20
            ll = ll - ntt
        end do
        jj = 0
        ! Search forward for the first valid point
        20 ll = i + ntt
        do kk = irec + 1, nobrk
            if (a(ll) /= missing_value) goto 40
            ll = ll + ntt
        end do
        kk = 0
        40 aa = 0.0
        ab = 0.0
        it1c = 0
        it2c = 0
        ! There was a backward valid point
        if (jj /= 0) then
            aa = a(i + (jj - irec) * ntt)
            if (integration_id == 1) it2c = 1
            if (integration_id == 2) then
                if (kk /= 0) then
                    it1c = itimf - j(ij + jj)
                else
                    it2c = 1
                endif
            endif
        endif
        ! There was a forward valid point
        if (kk /= 0) then
            ab = a(i + (kk - irec) * ntt)
            if (integration_id == 1 .and. jj == 0) it1c = 1
            if (integration_id == 2) then
                if (jj /= 0) then
                    it2c = j(ij + kk) - itimf
                else
                    it1c = 1
                endif
            endif
        endif
        idtc = it1c + it2c
        if (idtc == 0) idtc = 1

        if (timon) call timstop (ithandl)

    end subroutine calculate_missing_values
end module m_update_condition
