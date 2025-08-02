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
module m_rdlgri
    use m_stop_exit

    implicit none

contains


    subroutine rdlgri (nfiles, lunit, fname)

        !     Deltares Software Centre

        !>/File
        !>            Reads all grid related info, determines active grid pointers and layer thickness
        !>
        !>            - reads lgrid with active grid information from .lga file
        !>            - reads lgrid2 with total grid information from .lgt file
        !>            - determines pointer from nr of active gridcells num_cells to matrix (num_rows,num_columns)
        !>            - reads from-to pointer from .poi for to determine pointer from num_exchanges to matrix
        !>            - reads a volume record to determine layer thickness at the deepest point

        !     system administration : Antoon Koster

        !     created               : February 1990 by Leo Postma

        !     modified              : June     2011 by Leo Postma: pointers from num_cells and num_exchanges added
        !                                                          to support active only hydrodynamic files
        !                             Octobel  2011 by Leo Postma: support Domain Decomposition

        !     logical unit numbers  : lunit( 1), the delpar input file
        !                             lunit( 2), output report file
        !                             lunit( 3), active grid table
        !                             lunit( 4), total grid table
        !                             lunit(19), from-to pointer table

        !     subroutines called    : stop_exit   - ends the simulation with return code

        !     functions   called    : none.

        use m_waq_precision               ! single and double precision
        use timers
        use rd_token                ! tokenized reading like in DELWAQ
        use partmem
        use m_part_regular
        use m_rdhyd
        use alloc_mod
        use dd_prepare_mod
        use openfl_mod
        use m_hydmod

        implicit none

        !     Arguments

        !     kind           function         name                 description

        integer  (int_wp), intent(in) :: nfiles            !< nr. of files
        integer  (int_wp), intent(inout) :: lunit(nfiles)     !< unit nrs of all files
        character(256), intent(inout) :: fname(nfiles)     !< file names of all files

        !     locals

        integer, allocatable :: frm_to (:, :)     !  Delwaq exchange pointer
        integer, allocatable :: ibnd   (:)     !  locations of boundaries in the grid
        integer  (int_wp) num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir      !  dimensions of the active grid
        integer  (int_wp) nmax2, mmax2, layt2     !  dimensions of the total grid
        integer  (int_wp) i, j, k         !  loop counters in the 3 dimensions
        integer  (int_wp) n, ne, m, me                !  help variables grid index
        integer  (int_wp) i1                          !  help variable grid index
        integer  (int_wp) iseg                        !  help variable grid index
        integer  (int_wp) nobndl                      !  number of boundaries in 1 layer

        integer(4) ithndl              ! handle to time this subroutine
        integer(int_wp) lun
        data       ithndl / 0 /
        if (timon) call timstrt("rdlgri", ithndl)

        !       initialize the allocation system

        call init_alloc(lun, lunit(2))

        !       initialize the tokenized reading facility

        close (lunit(1))
        call rdhyd (nfiles, lunit, fname, hyd, layt, zmodel, ihdel, &
                &             tcktot, zlbot, zltop, ndoms, nbnds, doms, bnds)

        if (zmodel) then
            write (lunit(2), *)
            write (lunit(2), *) ' Zlayer defintion of z-layer model'
            write (lunit(2), *)
            write (lunit(2), *) ' Layer number     top of layer   bottom of layer'
            write (lunit(2), *) ' -----------------------------------------------'
            do k = 1, layt
                write (lunit(2), '(i12,f15.3,f15.3)') k, zltop(k), zlbot(k)
            enddo
        endif
        !     reading active table

        write (lunit(2), *)
        write (lunit(2), *) ' Opening the active grid file file:', fname(3)(1:len_trim(fname(3)))
        call openfl (lunit(3), fname(3), 0)

        read (lunit(3), end = 10, err = 10) nmaxp, mmaxp, noseglp, layt2, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir
        write(lunit(2), *) '   '
        write(lunit(2), *) ' Exchanges read from active grid file (*.lga) '
        write(lunit(2), *) '    No. of exchanges in first  direction  : ', num_exchanges_u_dir
        write(lunit(2), *) '    No. of exchanges in second direction  : ', num_exchanges_v_dir
        write(lunit(2), *) '    No. of exchanges in third  direction  : ', num_exchanges_z_dir
        goto 20

        !     you were working with an old version of with the standard lgridact??
        !     try again for that case

        10 rewind(lunit(3))
        read  (lunit(3), end = 100, err = 100) nmaxp, mmaxp, noseglp, layt2
        20 write (lunit(2), *) '   '
        write (lunit(2), 1000) mmaxp, nmaxp, layt

        !     reading the active grid file

        call alloc ("lgrid ", lgrid, nmaxp, mmaxp)
        call alloc ("lgrid2", lgrid2, nmaxp, mmaxp)
        call alloc ("lgrid3", lgrid3, nmaxp, mmaxp)
        read (lunit(3), end = 100, err = 100) lgrid
        nobndl = 0
        do j = 1, mmaxp
            do i = 1, nmaxp
                if (nobndl > lgrid(i, j)) nobndl = lgrid(i, j)
            enddo
        enddo
        nobndl = -nobndl
        nbmax = nobndl * layt2
        if (ndoms /= 0) then
            call dd_prepare (lunit(2), nmaxp, mmaxp, lgrid, nbmax, &
                    &                     ndoms, nbnds, nconn, doms, bnds, &
                    &                     conn)
        endif
        write (lunit(2), '(//a,a)') '  Succesful reading of the active grid file: ', fname(3)(:len_trim(fname(3)))
        close (lunit(3))

        !     reading the total grid file

        nmax2 = nmaxp
        mmax2 = mmaxp
        mnmax2 = nmaxp * mmaxp
        k = 1
        do j = 1, mmaxp
            do i = 1, nmaxp
                lgrid2 (i, j) = k
                k = k + 1
            enddo
        enddo
        if (noseglp == nmax2 * mmax2) then            ! map file on full matrix
            do j = 1, mmaxp
                do i = 1, nmaxp
                    lgrid3 (i, j) = lgrid2 (i, j)
                enddo
            enddo
        else                                            ! map file on condensed num_cells volumes
            do j = 1, mmaxp
                do i = 1, nmaxp
                    lgrid3 (i, j) = lgrid (i, j)
                enddo
            enddo
        endif

        ! Allocations necessary for both sigma and zlayers, despite only being used for zlayers
        call alloc ("laytop", laytop, nmaxp, mmaxp)
        call alloc ("laytopp", laytopp, nmaxp, mmaxp)
        call alloc ("laybot", laybot, nmaxp, mmaxp)
        call alloc ("pagrid", pagrid, nmaxp, mmaxp, layt)
        call alloc ("aagrid", aagrid, nmaxp, mmaxp, layt)
        laytop = 0
        laytopp = 0
        laybot = 0
        pagrid = 0
        aagrid = 0
        if (zmodel) then
            do i = 1, nmaxp
                do j = 1, mmaxp
                    if (lgrid(i, j) > 0) then
                        do k = 1, layt
                            pagrid(i, j, k) = mod(hyd%attributes(lgrid(i, j) + (k - 1) * noseglp), 10)
                            if (pagrid(i, j, k) == 1) then
                                laybot(i, j) = k
                            endif
                        enddo
                    endif
                enddo
            enddo
        end if
        mnmaxk = mnmax2 * layt
        nflow = 2 * mnmaxk + (layt - 1) * nmaxp * mmaxp
        nosegp = noseglp * layt2
        noqp = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir

        !     make the pointers from volumes to grid locations

        call alloc ("cellpnt", cellpntp, nosegp)
        call alloc ("flowpnt", flowpntp, noqp, 2)
        cellpntp = 0
        do j = 1, mmax2
            do i = 1, nmax2
                if (lgrid(i, j) > 0) then
                    do k = 1, layt2
                        cellpntp(lgrid(i, j) + (k - 1) * noseglp) = lgrid2(i, j) + (k - 1) * mnmax2
                    enddo
                endif
            enddo
        enddo
        do i = 1, nosegp
            if (cellpntp(i) <= 0) cellpntp(i) = i       ! happens if 1-1 coupling
        enddo

        !     make the pointers from flows to grid locations

        flowpntp = 0
        if (noseglp == nmax2 * mmax2) then            ! map file on full matrix
            do i = 1, noqp
                flowpntp(i, 1) = i
            enddo
        else                                           ! map file on active only
            allocate (frm_to(4, noqp), ibnd(nbmax))
            do j = 1, mmax2
                do i = 1, nmax2
                    if (lgrid(i, j) < 0 .and. lgrid(i, j) >= -nobndl) then
                        do k = 1, layt2
                            ibnd(-lgrid(i, j) + (k - 1) * nobndl) = lgrid2(i, j) + (k - 1) * mnmax2
                        enddo
                    endif
                enddo
            enddo
            write (lunit(2), *) ' Opening the pointers file:', fname(19)(1:len_trim(fname(19)))
            call openfl (lunit(19), fname(19), 0)
            read  (lunit(19)) frm_to
            close (lunit(19))
            write (lunit(2), '(a,a)') '  Succesful reading of the pointers file   : ', fname(19)(:len_trim(fname(19)))
            do i = 1, num_exchanges_u_dir
                j = frm_to(1, i)
                if (j > 0) flowpntp(i, 1) = cellpntp(j)
                if (j < 0) flowpntp(i, 1) = ibnd    (-j)
            enddo
            do i = num_exchanges_u_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir
                j = frm_to(1, i)
                if (j > 0) flowpntp(i, 1) = cellpntp(j) + mnmaxk
                if (j < 0) flowpntp(i, 1) = ibnd    (-j) + mnmaxk
            enddo
            do i = num_exchanges_u_dir + num_exchanges_v_dir + 1, noqp
                j = frm_to(1, i)
                if (j > 0) flowpntp(i, 1) = cellpntp(j) + mnmaxk * 2
            enddo
            do i = 1, nconn
                if (conn(i)%in1 == 0) then
                    ne = conn(i)%n1
                    m = conn(i)%m1
                    if (conn(i)%i1 == 0) ne = ne + conn(i)%f1 - 1
                    do n = conn(i)%n1, ne
                        i1 = lgrid(n, m)
                        if (i1 == 0) cycle
                        do k = 1, layt2
                            iseg = i1 + (k - 1) * noseglp
                            do j = num_exchanges_u_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir
                                if (frm_to(2, j) == iseg) then
                                    flowpntp(j, 2) = (m - 2) * nmaxp + n + (k - 1) * mnmax2 + mnmaxk
                                endif
                            enddo
                        enddo
                    enddo
                endif
                if (conn(i)%in2 == 0) then
                    n = conn(i)%n2
                    me = conn(i)%m2
                    if (conn(i)%i2 == 0) me = me + conn(i)%f2 - 1
                    do m = conn(i)%m2, me
                        i1 = lgrid(n, m)
                        if (i1 == 0) cycle
                        do k = 1, layt2
                            iseg = i1 + (k - 1) * noseglp
                            do j = 1, num_exchanges_u_dir
                                if (frm_to(2, j) == iseg) then
                                    flowpntp(j, 2) = (m - 1) * nmaxp + n - 1 + (k - 1) * mnmax2
                                endif
                            enddo
                        enddo
                    enddo
                endif
            enddo

            !        re-adjust the active grid table to 1-1 numbering, lgrid3 preserves lgrid

            do j = 1, mmax2
                do i = 1, nmax2
                    if (lgrid(i, j) > 0) lgrid(i, j) = lgrid2(i, j)
                enddo
            enddo
        endif

        !     some additional allocations

        call alloc ("angle  ", angle, mnmaxk)
        call alloc ("area   ", area, mnmaxk)
        call alloc ("depth  ", depth, mnmaxk)
        call alloc ("dpsp   ", dpsp, mnmax2)
        call alloc ("dx     ", dx, mnmax2)
        call alloc ("dy     ", dy, mnmax2)
        call alloc ("flow   ", flow, nflow)
        call alloc ("flow2m ", flow2m, nflow)
        call alloc ("flow1  ", flow1, noqp)
        call alloc ("flow2  ", flow2, noqp)
        call alloc ("ipnt   ", ipntp, mnmaxk)
        call alloc ("nplay  ", nplay, layt)
        call alloc ("vdiff  ", vdiff, mnmaxk)
        call alloc ("vdiff1 ", vdiff1, nosegp)
        call alloc ("tau    ", tau, mnmaxk)
        call alloc ("tau1   ", tau1, nosegp)
        call alloc ("salin  ", salin, mnmaxk)
        call alloc ("salin1 ", salin1, nosegp)
        call alloc ("temper ", temper, mnmaxk)
        call alloc ("rhowatc ", rhowatc, nosegp)
        call alloc ("temper1", temper1, nosegp)
        call alloc ("velo   ", velo, mnmaxk)
        call alloc ("vel1   ", vel1, noseglp)
        call alloc ("vel2   ", vel2, noseglp)
        call alloc ("vol1   ", vol1, nosegp)
        call alloc ("vol2   ", vol2, nosegp)
        call alloc ("volume ", volumep, mnmaxk)
        call alloc ("xb     ", xb, mnmax2)
        call alloc ("yb     ", yb, mnmax2)
        call alloc ("zlevel ", zlevel, mnmax2)
        area = 0

        !     normal end of routine

        if (timon) call timstop (ithndl)
        return

        !     stop with error

        100 write (lunit(2), *) ' Error 4502. reading the file of active grid cells: ', fname(3)
        call stop_exit(1)

        call stop_exit(1)

        1000 format ('     No. of gridpoints in x direction      :', i13/  &
                &        '     No. of gridpoints in y direction      :', i13/  &
                &        '     No. of layers                         :', i13)

    end subroutine

end module m_rdlgri
