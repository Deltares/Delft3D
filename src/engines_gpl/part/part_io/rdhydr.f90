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

module rdhydr_mod
    !
    !  module declarations
    !
    !
    !  data definition module(s)
    !
    use m_stop_exit
    use m_parttd
    use m_dlwqfl
    use m_dlwqbl
    use m_waq_precision      ! single and double precision
    use timers
    !
    !  module procedure(s)
    !
    use openfl_mod     ! explicit interface
    use rd_token       ! tokenized reading like in DELWAQ
    use partmem
    use m_part_regular
    use m_part_modeltypes
    use alloc_mod
    use m_densty
    !
    implicit none
    !
contains
    subroutine rdhydr (num_rows, num_columns, mnmaxk, nflow, num_cells, &
            num_exchanges, itime, itstrt, idelt, volume, &
            vdiff, hsurf, flow, vol1, vol2, &
            flow1, flow2m, vdiff1, update, cellpnt, flowpnt, &
            tau, tau1, caltau, salin, salin1, &
            temper, temper1, nfiles, lunit, fname, &
            flow2, rhowatc)
        !
        !     READING HYDRODYNAMICS FILE (*.hyd)
        !              (initially)
        !
        !     system administration : r. j. vos
        !
        !
        !     created               : february 1990, by l. postma
        !
        !     note                  : 3d version may 1996
        !                             see the -2 option coupling in loop 35
        !                             was not present in vs3.00, but in v2.3 ok
        !
        !
        !     logical unit numbers  : lun    - array with lu-numbers
        !
        !
        !     subroutines called    : stop_exit
        !                             parttd.
        !                             dlwqbl.
        !
        !     functions   called    : none.
        !
        save
        !
        !     parameters            :

        integer  (int_wp), intent(in) :: num_rows             !< size of first index in the cube
        integer  (int_wp), intent(in) :: num_columns             !< size of second index in the cube
        integer  (int_wp), intent(in) :: mnmaxk           !< number of cells in the cube
        integer  (int_wp), intent(in) :: nflow            !< size of 3*3d flow array
        integer  (int_wp), intent(in) :: num_cells            !< nr of computational volumes
        integer  (int_wp), intent(in) :: num_exchanges              !< total number of exchanges
        integer  (int_wp), intent(in) :: itime            !< current time
        integer  (int_wp), intent(out) :: itstrt           !< start of the hydrodynamic files
        integer  (int_wp), intent(out) :: idelt            !< time step of hydrodynamic files
        real     (sp), intent(out) :: volume (mnmaxk)  !< a grid with volumes
        real     (sp), intent(out) :: vdiff  (mnmaxk)  !< a grid with vertical diffusions
        real     (sp), intent(in) :: hsurf  (mnmaxk)  !< a grid with horizontal surfaces
        real     (sp), intent(out) :: flow   (nflow)  !< a grid with flows
        real     (sp), intent(out) :: vol1   (num_cells)  !< first volume record
        real     (sp), intent(out) :: vol2   (num_cells)  !< second volume record
        real     (sp), intent(out) :: flow1  (num_exchanges)  !< flow record in file
        real     (sp), intent(out) :: flow2m (nflow)  !< a grid with flows
        real     (sp), intent(out) :: vdiff1 (num_cells)  !< vertical diffusion record in file
        logical, intent(out) :: update           !< values have been updated
        integer  (int_wp), intent(in) :: cellpnt(num_cells)  !< backpointering from volumes to grid
        integer  (int_wp), intent(in) :: flowpnt(num_exchanges, 2)  !< backpointering from flows to grid
        real     (sp), intent(out) :: tau    (mnmaxk)  !< tau record on the grid
        real     (sp), intent(out) :: tau1   (num_cells)  !< tau record in file
        logical, intent(out) :: caltau           !< should the tau be calculated ?
        real     (sp), intent(out) :: salin  (mnmaxk)  !< salinity record on the grid
        real     (sp), intent(out) :: salin1 (num_cells)  !< salinity record in file
        real     (sp), intent(out) :: temper (mnmaxk)  !< temperature record on the grid
        real     (sp), intent(out) :: temper1(num_cells)  !< temperature record in file
        real     (sp), intent(out) :: rhowatc(num_cells)  !< density water calculated from temperature and salinity on the active grid
        integer  (int_wp), intent(in) :: nfiles           !< nr. of files
        integer  (int_wp), intent(inout) :: lunit(nfiles)    !< unit nrs of all files
        character(*), intent(inout) :: fname(nfiles)    !< file names of all files
        real     (sp), intent(out) :: flow2  (num_exchanges)  !< flow record in file second record

        !     locals

        logical :: updatv, updatf, updatd, lblock
        !
        logical :: first = .true.
        integer(int_wp) :: i, i0, i03d, i1, i2, idelt1, ifflag, iocond, isflag, num_layers_grid
        integer(int_wp) :: it1, it2, max, mod, lunut
        integer(int_wp) :: mnmax                        ! number of cells per layer in the cube
        integer(int_wp) :: idtimv, itimv1, itimv2     ! timings of the volumes file
        integer(int_wp) :: idtimf, itimf1, itimf2     ! timings of the flow file
        integer(int_wp) :: idtimd, itimd1, itimd2     ! timings of the vertical diffusion file
        integer(int_wp) :: idtimt, itimt1, itimt2     ! timings of the tau file
        real   (sp) :: depmin
        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        real(int_wp) :: ideltold !AddedMarc
        if (timon) call timstrt("rdhydr", ithndl)
        !
        lunut = lunit(2)
        if (zmodel) then
            depmin = 0.05
        else
            depmin = 0.05 * num_rows * num_columns / mnmaxk
            depmin = max(depmin, 0.001)
        end if
        num_layers_grid = mnmaxk / num_rows / num_columns
        if (idelt == -999) then
            !
            !        Save current value of idelt for later
            ideltold = idelt
            write (lunut, *) ' Opening the volume file:', fname(6)(1:len_trim(fname(6)))
            call openfl (lunit(6), fname(6), 0)
            !        if (iocond  /=  0) goto 100
            write (lunut, *) ' Opening the flow   file:', fname(7)(1:len_trim(fname(7)))
            call openfl (lunit(7), fname(7), 0)

            if (lunit(20) /= 0 .and. fname(20) /= 'none') then
                write (lunut, *) ' Opening the vdf    file:', fname(20)(1:len_trim(fname(20)))
                call openfl (lunit(20), fname(20), 3)
                if (lunit(20) == 0) write (lunut, *) ' Warning the vdf file does not exist !'
            else
                lunit(20) = 0
            endif
            if (lunit(20) == 0) vdiff = 0.0

            if (lunit(21) /= 0 .and. fname(21)(1:4) /= 'none') then
                write (lunut, *) ' Opening the tau    file:', fname(21)(1:len_trim(fname(21)))
                call openfl (lunit(21), fname(21), 3)
                if (lunit(21) == 0) write (lunut, *) ' Warning the tau file does not exist !'
            else
                lunit(21) = 0
            endif
            caltau = .false.
            if (lunit(21) == 0) caltau = .true.

            if (lunit(22) /= 0 .and. fname(22)(1:4) /= 'none') then
                write (lunut, *) ' Opening the salinity file:', fname(22)(1:len_trim(fname(22)))
                call openfl (lunit(22), fname(22), 3)
                if (lunit(22) == 0) write (lunut, *) ' Warning the salinity file does not exist !'
            else
                lunit(22) = 0
            endif

            if (lunit(23) /= 0 .and. fname(23)(1:4) /= 'none') then
                write (lunut, *) ' Opening the temperature file:', fname(23)(1:len_trim(fname(23)))
                call openfl (lunit(23), fname(23), 3)
                if (lunit(23) == 0) write (lunut, *) ' Warning the temperature file does not exist !'
            else
                lunit(23) = 0
            endif
            !
            read (lunit(6), iostat = iocond) it1, vol1
            if (iocond  /=  0) goto 120
            read (lunit(6), iostat = iocond) it2, vol2
            if (iocond  /=  0) goto 130
            idelt = it2 - it1
            !
            read (lunit(7), iostat = iocond) it1, flow1
            if (iocond  /=  0) goto 140
            read (lunit(7), iostat = iocond) it2, flow2
            if (iocond  /=  0) goto 150
            !        idelt = it2 - it1
            !        if (idelt   /=  idelt1) goto 160
            itstrt = it1
            rewind (lunit(6))
            rewind (lunit(7))
            !
            ! This caused a stack overflow - 300000 grid cells
            !volume(cellpnt(:)) = vol1 (:)
            do i = 1, size(vol1)
                volume(cellpnt(i)) = vol1(i)
            enddo

            flow = 0.0
            flow2m = 0.0
            do i = 1, num_exchanges
                if (flowpnt(i, 1) > 0) flow(flowpnt(i, 1)) = flow(flowpnt(i, 1)) + flow1(i)
                if (flowpnt(i, 2) > 0) flow(flowpnt(i, 2)) = flow(flowpnt(i, 2)) + flow1(i)
                if (flowpnt(i, 1) > 0) flow2m(flowpnt(i, 1)) = flow2m(flowpnt(i, 1)) + flow2(i)
                if (flowpnt(i, 2) > 0) flow2m(flowpnt(i, 2)) = flow2m(flowpnt(i, 2)) + flow2(i)
            enddo
        else
            !
            if (first) then
                ifflag = 1
            else
                ifflag = 0
            endif
            isflag = 1

            !.. volumes

            lblock = .false.
            call parttd (lunit(6), lunut, itime, idtimv, itimv1, &
                    itimv2, num_cells, mnmaxk, vol1, vol2, &
                    volume, cellpnt, lblock, fname(6), isflag, &
                    ifflag, updatv)

            !.. flows
            !.. note that flows must be block since no space is reserved for flow-interpolation
            call dlwqfl (lunit(7), lunut, itime, idtimf, itimf1, &
                    itimf2, idelt, num_exchanges, nflow, flow1, &
                    flow2, flow, flowpnt, fname(7), isflag, ifflag, &
                    updatf, flow2m)

            !.. vertical diffusions

            if (lunit(20) /= 0) then
                call dlwqbl (lunit(20), lunut, itime, idtimd, itimd1, &
                        itimd2, idelt, num_cells, mnmaxk, vdiff1, &
                        vdiff, cellpnt, fname(20), isflag, ifflag, &
                        updatd)
                if (num_layers_grid > 1) then                                    ! fill the zero last layer with the
                    mnmax = num_rows * num_columns
                    do i = mnmaxk - mnmax + 1, mnmaxk
                        vdiff(i) = vdiff(i - mnmax)                        ! values above
                    end do
                endif
            endif

            !.. tau

            if (.not. caltau) then
                call dlwqbl (lunit(21), lunut, itime, idtimt, itimt1, &
                        itimt2, idelt, num_cells, mnmaxk, tau1, &
                        tau, cellpnt, fname(21), isflag, ifflag, &
                        updatd)
            endif

            !.. salinity

            if (lunit(22) /= 0) then
                call dlwqbl (lunit(22), lunut, itime, idtimt, itimt1, &
                        itimt2, idelt, num_cells, mnmaxk, salin1, &
                        salin, cellpnt, fname(22), isflag, ifflag, &
                        updatd)
            endif

            !.. temperature

            if (lunit(23) /= 0) then
                call dlwqbl (lunit(23), lunut, itime, idtimt, itimt1, &
                        itimt2, idelt, num_cells, mnmaxk, temper1, &
                        temper, cellpnt, fname(23), isflag, ifflag, &
                        updatd)
            endif

            first = .false.
            if (itimv1  /=  itimf1) goto 170
            !
            !
            if ((updatv.and..not.updatf) .or.  &
                    (.not.updatv.and.updatf)) goto 180
            update = updatv
            !
        endif
        !
        if (zmodel) then
            ! only make sure the deepest
            do i2 = 2, num_columns
                do i1 = 2, num_rows
                    i0 = lgrid(i1, i2)
                    if (i0  >  0) then
                        i03d = i0 + (laybot(i1, i2) - 1) * num_rows * num_columns
                        volume(i03d) = max(volume(i03d), hsurf(i0) * depmin)
                    end if
                end do
            end do
        else
            do i = 1, mnmaxk

                !          limit volume to 5cm

                i2 = mod(i, num_rows * num_columns)
                if(i2==0) i2 = num_rows * num_columns
                volume(i) = max(volume(i), hsurf(i2) * depmin)

                !          apply scaling to vertical diffusion
                !          the .vdf file at the moment contains the D3D-FLOW dicww array in m2/s.
                !          typically: doffset = vicmol/sigmol(substance)
                !                     dscale  = 1.0   /sigdif(substance)
                !                     dminim  = gdp%gdphysco%dicoww       if I interpreted D3D-FLOW correctly (lp)
                !          off course only do this if the vertical diffusion was updated with the file-values

            end do
        end if
        !
        !     end of routine
        !
        if ((lunit(22) /= 0) &
                .and. (lunit(23) /= 0) &
                .and. (ideltold /= -999) &
                .and. (modtyp == model_prob_dens_settling)) then
            do i = 1, num_cells
                rhowatc(i) = densty(max(0.0e0, salin1(i)), temper1(i))
            enddo
        else
            do i = 1, num_cells
                rhowatc(i) = rhow
            enddo
        endif

        if (timon) call timstop (ithndl)
        return
        !
        !     exit with formats
        !
        120 write (lunut, *) ' Error 4403. Reading the volume file:', fname(6)
        call stop_exit(1)
        130 write (lunut, *) ' Error 4404. Reading the volume file:', fname(6)
        call stop_exit(1)
        140 write (lunut, *) 'Error 4405. Reading the flow file  :', fname(7)(:12)
        call stop_exit(1)
        150 write (lunut, *) 'Error 4406. Reading the flow file  :', fname(7)(:12)
        call stop_exit(1)
        write (lunut, *) '             Time step in volume file:', idelt1
        write (lunut, *) '             Time step in flow   file:', idelt
        write (lunut, *) '             Time step in depth  file:', idelt
        call stop_exit(1)
        170 write (lunut, *) ' Error 4410. Times in files do not match!'
        write (lunut, *) '             Volumes:', itimv1, ' flows:', itimf1
        call stop_exit(1)
        180 write (lunut, *) ' Error 4411. Files have not both been updated  '
        write (lunut, *) ' Volumes-update:', updatv, ' flows-update:', updatf
        call stop_exit(1)
    end subroutine
end module
