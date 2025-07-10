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
module m_blmeff
    use m_waq_precision
    use m_string_utils

    implicit none

contains


    subroutine blmeff (lunrep, lunblm, verspe, lunfrm, grname, nuecog, typnam, noalg)
        !
        use m_bleffpro
        use m_logger_helper, only : stop_with_error
        use timers        !   performance timers

        implicit none
        integer(kind = int_wp) :: lunrep, lunblm
        real(kind = real_wp) :: verspe
        integer(kind = int_wp) :: lunfrm, nuecog, noalg
        character(len=10)  grname(nuecog)
        character(len=10)  typnam(noalg)

        !
        integer(kind = int_wp), parameter :: maxlin = 1000
        integer(kind = int_wp), parameter :: maxspe = 30
        integer(kind = int_wp), parameter :: maxtok = 8
        integer(kind = int_wp), parameter :: maxnz = 51
        integer(kind = int_wp) :: ifnd (maxspe)
        real(kind = dp) :: power(51), effic(51, maxspe), fun(51, maxspe), der(51, maxspe), zvec(51), daymul(24, maxspe), dl(24)
        character(len=8) spnam2 (maxspe)
        integer(kind = int_wp) :: numtyp, i, j, nfnd, npoint, nz
        real(kind = real_wp) :: tefcur
        character(len=1000) line
        integer(kind = int_wp) :: ithndl = 0

        if (timon) call timstrt("blmeff", ithndl)

        if (verspe<2.0) then
            !
            ! read efficiency database
            ! Read the first record. This contains the names of
            ! all species for whome information is available.
            ! Note: this should be consistent with the process coefficient data base
            ! but this is not checked!
            !
            20    format (a1000)
            read (lunblm, 20) line
            numtyp = 0
            spnam2 = ' '
            read (line, *, err = 100, end = 100) spnam2(1:maxspe)
            100    continue
            do i = 1, maxspe
                if (spnam2(i)(1:1)==' ') exit
                numtyp = numtyp + 1
            end do
            !
            ! Match the selected group names (GRNAME) with those stored in the date
            ! base (SPNAM2). If a match is found, store the matching number in IFND.
            !
        else
            read (lunblm, *) tefcur
            read (lunblm, *) numtyp
            read (lunblm, *) (spnam2(i), i = 1, numtyp)
        end if

        do i = 1, nuecog
            nfnd = index_in_array(grname(i)(:8), spnam2(:numtyp))
            if (nfnd >= 1) then
                ifnd (i) = nfnd
            else
                write(lunrep, '(3A)') 'ERROR: Could not find species ', trim(grname(i)), ' in the efficicy tables of the bloom.spe file'
                call stop_with_error()
            end if
        end do
        !
        ! Sort the record pointers to get them in the apprpriate order for the
        ! output! This is necessary as the user may use a random input order
        ! for the species names in BLOING.DAT.
        !
        call insort (ifnd, nuecog)

        if (verspe<2.0) then
            !
            !  Read the entire efficiency data base file using the same statements
            !  as in INPUT2 of BLOOM II
            !
            read (lunblm, 290) nz, tefcur
            290    format (i5, 5x, f10.2)
            291    format (i5, 5x, f10.2, 5x, a)
            read (lunblm, 300) (zvec(i), i = 1, nz)
            300    format (10(d15.8, 3x))
            301    format (30(d15.8, 3x))
            read (lunblm, 290) nz
            do i = 1, nz
                read (lunblm, 301) (fun(i, j), j = 1, numtyp)
                read (lunblm, 301) (der(i, j), j = 1, numtyp)
            end do
        else
            !
            !  Let bleffpro read the lightcurves, and calculate the efficiency database from that
            !
            call bleffpro(lunrep, lunblm, numtyp, npoint, power, effic, nz, zvec, fun, der)
        end if

        do i = 1, 24
            read (lunblm, *) dl(i), (daymul(i, j), j = 1, numtyp)
        end do
        330 format (31f5.2)
        !
        ! Write names of those groups and types that were selected.
        !
        if (verspe<2.0) then
            write (lunfrm, 245)
        else
            write (lunfrm, 246)
        end if
        245 format ('BLOOMFRM_VERSION_2.00') ! frm version 2.00: added a list of the selected group and type names
        246 format ('BLOOMFRM_VERSION_2.01') ! frm version 2.01: added the light curves of the selected groups
        write (lunfrm, 250) grname(1:nuecog)
        write (lunfrm, 250) typnam(1:noalg)
        250 format (30(A10, X))
        !
        ! Write the light curves the groups that were selected.
        !
        if (verspe>=2.0) then
            write (lunfrm, 291) npoint, tefcur, 'lightintensity_efficiency_curves'
            do i = 1, npoint
                write (lunfrm, 301) power(i), (effic(i, ifnd(j)), j = 1, nuecog)
            end do
        end if
        !
        ! Write the efficiency data for those species that were selected.
        !
        if (verspe>=2.0) then
            write (lunfrm, 291) nz, tefcur, 'zvec_fun_lookuptable'
            do i = 1, nz
                write (lunfrm, 301) zvec(i), (fun(i, ifnd(j)), j = 1, nuecog)
            enddo
            write (lunfrm, 291) nz, tefcur, 'zvec_der_lookuptable'
            do i = 1, nz
                write (lunfrm, 301) zvec(i), (der(i, ifnd(j)), j = 1, nuecog)
            enddo
        else
            write (lunfrm, 290) nz, tefcur
            write (lunfrm, 300) (zvec(i), i = 1, nz)
            write (lunfrm, 290) nz
            do i = 1, nz
                write (lunfrm, 301) (fun(i, ifnd(j)), j = 1, nuecog)
                write (lunfrm, 301) (der(i, ifnd(j)), j = 1, nuecog)
            end do
        endif
        do i = 1, 24
            write (lunfrm, 330) dl(i), (daymul(i, ifnd(j)), j = 1, nuecog)
        end do
        if (timon) call timstop(ithndl)
        return
    end

    ! INSORT subroutine.
    ! Purpose: sort an integer array.

    subroutine insort (inarr, lenarr)
        integer(kind = int_wp) :: inarr (*), lenarr
        integer(kind = int_wp) :: i, ihelp
        logical ready
        !
        10    continue
        ready = .true.
        do i = 1, lenarr - 1
            if (inarr(i) > inarr(i + 1)) then
                ready = .false.
                ihelp = inarr(i)
                inarr(i) = inarr(i + 1)
                inarr(i + 1) = ihelp
            end if
        end do
        if (.not. ready) go to 10
        return
    end

end module m_blmeff
