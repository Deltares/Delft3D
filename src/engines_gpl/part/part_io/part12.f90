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

module part12_mod

    use m_stop_exit
    use m_array_manipulation, only: fill_element_dimensions
    use openfl_mod

contains
    subroutine part12 (lun1, lname, lun2, title, subst, &
            lgrid, lgrid2, lgrid3, num_rows, num_columns, &
            conc, volume, npart, mpart, wpart, &
            nopart, itime, idelt, icwsta, icwsto, &
            icwste, atotal, npwndw, kpart, pblay, &
            iptime, npwndn, modtyp, nosubs, num_layers, &
            iyear, imonth, iofset, pg, rbuffr, &
            nosta, mnmax2, nosegl, isfile, mapsub, &
            layt, area, nfract, use_settling, mstick, &
            elt_names, elt_types, elt_dims, elt_bytes, locdep, &
            nosub_max, bufsize)

        !     CREATING MAP FILE FOR CURVILINEAR GRID
        !          (Nefis and binary files / per time step)

        !     function              : generates a standard delwaq - map-file,
        !                             and concentration-array for partwq
        !                             3d version...........
        !
        !     note                  : include of file 'crefd.inc'
        !                             include is no standard (ansi) fortran77!!
        !                             check if this include facility is available!
        !
        !     logical unit numbers  : lun1 - unit nr delwaq - map-file
        !                             lun2 - output log file

        !     subroutines called    : stop_exit
        !                             putget
        !                             putget_chars

        !     functions   called    : none.

        use m_waq_precision          ! single and double precision
        use m_part_modeltypes       ! part model definitions
        use timers
        use filldm_mod         ! explicit interface
        use genfil_mod         ! explicit interface
        use delete_file_mod
        use putget_mod         ! generic procedure for putget routines
        use typos

        implicit none

        !     Arguments

        !     kind           function         name                      description

        integer  (int_wp), intent(in) :: lun1                    !< unit nr of the Delwaq .map file
        character(*), intent(in) :: lname                   !< name of the .map file
        integer  (int_wp), intent(in) :: lun2                    !< unit nr of the output log file
        character(40), intent(in) :: title (4)               !< model- and run titles
        integer  (int_wp), intent(in) :: num_rows                    !< first dimension of the grid
        integer  (int_wp), intent(in) :: num_columns                    !< second dimension of the grid
        integer  (int_wp), intent(in) :: num_layers                   !< number of layers of the grid
        integer  (int_wp), intent(in) :: nosubs                  !< number of substances to plot
        integer  (int_wp), intent(in) :: nopart                  !< number of particles
        character(20), intent(in) :: subst (nosubs + 2)        !< substance names with layer extension
        integer  (int_wp), intent(in) :: lgrid (num_rows, num_columns)       !< active grid table
        integer  (int_wp), intent(in) :: lgrid2(num_rows, num_columns)       !< total grid table
        integer  (int_wp), intent(in) :: lgrid3(num_rows, num_columns)       !< plot grid either total or active condensed
        integer  (int_wp), intent(in) :: nosub_max               !< maximum number of substances
        real     (real_wp), intent(out) :: conc  (nosub_max, num_rows * num_columns * num_layers) !< computed concentrations
        real     (sp), intent(in) :: volume(*)             !< volumes of the grid cells
        integer  (int_wp), intent(inout) :: npart (nopart)        !< n-values of particles
        integer  (int_wp), intent(inout) :: mpart (nopart)        !< m-values of particles
        integer  (int_wp), intent(inout) :: kpart (nopart)        !< k-values of particles
        real     (sp), intent(inout) :: wpart (nosubs, nopart)   !< weights of particles
        integer  (int_wp), intent(in) :: itime                   !< model time
        integer  (int_wp), intent(in) :: idelt                   !< model time step
        integer  (int_wp), intent(in) :: icwsta                  !< start time map-file
        integer  (int_wp), intent(in) :: icwsto                  !< stop  time map-file
        integer  (int_wp), intent(in) :: icwste                  !< time step map-file
        real     (real_wp), intent(out) :: atotal(num_layers, nosubs)    !< total mass per subst/per layer
        integer  (int_wp), intent(inout) :: npwndw                  !< start of active particle number
        real     (sp), intent(in) :: pblay                   !< relative thickness lower layer
        integer  (int_wp), intent(inout) :: iptime(nopart)        !< age of particles
        integer  (int_wp), intent(in) :: npwndn                  !< new start of active particle number - 1
        integer  (int_wp), intent(in) :: modtyp                  !< model-run-type
        integer  (int_wp), intent(in) :: iyear                   !< year
        integer  (int_wp), intent(in) :: imonth                  !< month
        integer  (int_wp), intent(in) :: iofset                  !< offset in time
        type(PlotGrid)                   pg                      !< first plot grid information
        integer  (int_wp), intent(in) :: bufsize                 !< size of rbuffr
        real     (real_wp) :: rbuffr(bufsize)         !< work storage
        integer  (int_wp), intent(in) :: nosta                   !< number of observation points
        integer  (int_wp), intent(in) :: mnmax2                  !< number of grid cells in one grid layer
        integer  (int_wp), intent(in) :: nosegl                  !< number of computational elements per layer
        integer  (int_wp), intent(in) :: isfile(nosub_max)       !< file output for the substance?
        integer  (int_wp), intent(in) :: mapsub(nosub_max)
        integer  (int_wp), intent(in) :: layt                    !< number of hydrodynamic layers
        real     (sp), intent(in) :: area  (mnmax2)
        integer  (int_wp), intent(in) :: nfract                  !< number of oil fractions
        logical, intent(in) :: use_settling                  !< if .true. settling occurs in an extra layer
        integer  (int_wp), intent(in) :: mstick(nosub_max)
        character(*), pointer :: elt_names(:)            !<  NEFIS
        character(*), pointer :: elt_types(:)            !<  NEFIS
        integer  (int_wp), pointer :: elt_dims (:, :)          !<  NEFIS
        integer  (int_wp), pointer :: elt_bytes(:)            !<  NEFIS
        real     (real_wp) :: locdep (num_rows * num_columns, num_layers)

        !     save values between invocations: specified precisely those needed!!
        !
        save first, type, dname, itoff, num_cells
        !
        !     declarations
        !
        character(len = 6) :: subnam = 'part12'
        character(len = 20) :: dname(1)
        logical :: mapfil
        character(len = 256) :: filnam
        character(len = 20) :: type
        !
        !     declarations for in order to use putget
        !
        integer(int_wp), parameter :: itofmx = 7
        integer(int_wp), parameter :: noparm = 8
        !
        character(len = 20) :: substance
        character(len = 16) :: grnam1, grnam2
        integer(int_wp), dimension(itofmx) :: itoff
        integer(int_wp), dimension(6) :: nosize
        real     (sp) :: window(4)              !< first plotgrid window
        !
        save          grnam1, grnam2, &
                nosize, filnam, first1, nefis, &
                celid1, celid2
        !
        !     element names
        !
        logical :: wrswch = .true.
        logical :: first = .true.
        logical :: first1 = .true.
        logical :: nefis = .true.
        integer(int_wp) :: celid1 = 1
        integer(int_wp) :: celid2 = 1
        !
        !     local scalars
        !
        integer(int_wp) :: i, j, ierr, ierrem, indx
        integer(int_wp) :: i1, i2, ic, ilay, ipos, iseg
        integer(int_wp) :: isub, jsub, layts, m, n
        integer(int_wp) :: nelmax
        integer(int_wp)    num_cells                         !  number of computational volumes per layer
        real   (sp) :: ptlay, pxlay, vnorm

        integer(4) ithndl              ! handle to time this subroutine
        data       ithndl / 0 /
        if (timon) call timstrt("part12", ithndl)

        !
        elt_names(1) = 'TYPE'
        elt_names(2) = 'TITLE'
        elt_names(3) = 'SUBST_NAMES'
        elt_names(4) = 'LOCATION_NAMES'
        elt_names(5) = 'SIZES'
        elt_names(6) = 'PLOT_WINDOW'
        elt_names(7) = 'TIME_OFFSET'
        elt_names(8) = 'TIME'
        elt_types(1) = 'CHARACTER'
        elt_types(2) = 'CHARACTER'
        elt_types(3) = 'CHARACTER'
        elt_types(4) = 'CHARACTER'
        elt_types(5) = 'INTEGER'
        elt_types(6) = 'REAL'
        elt_types(7) = 'INTEGER'
        elt_types(8) = 'INTEGER'
        elt_bytes(1) = 20
        elt_bytes(2) = 40
        elt_bytes(3) = 20
        elt_bytes(4) = 20
        elt_bytes(5) = 4
        elt_bytes(6) = 4
        elt_bytes(7) = 4
        elt_bytes(8) = 4

        !     nosubs + 2 : local depths and number of particles

        do i = 1, nosubs + 2
            write (substance, '(a,i3.3)') 'SUBST_', i
            elt_names(i + noparm) = substance
            elt_types(i + noparm) = 'REAL'
            elt_bytes(i + noparm) = 4
        enddo

        num_cells = nosegl * num_layers
        if (first1) then

            !     adapt dimensions

            nelmax = noparm + (nosubs + 2) * num_layers

            !     first inquire file name (via monitoring file)

            inquire(unit = lun2, name = filnam)

            !     generate file name for map file

            call genfil(filnam, 'map-', indx)
            call delete_file(filnam(:indx) // '.dat', ierr)
            call delete_file(filnam(:indx) // '.def', ierr)
            first1 = .false.
        endif

        ierrem = 0

        !     determine if map file must be produced

        mapfil = .true.
        if (icwste                     < 1) mapfil = .false.
        if (itime                      < icwsta) mapfil = .false.
        if (itime - idelt              >=  icwsto) mapfil = .false.
        if (mod(itime - icwsta, icwste)  >=  idelt) mapfil = .false.
        !
        !     in case there is no output required then
        !     check if concentration is to be calculated
        !     for use in process subsroutines
        !
        if (.not. mapfil) then
            !
            !       no map-file is to be created
            !
            if (modtyp /= model_two_layer_temp .and. nosta <1 .and. modtyp /= model_red_tide .and.       &
                    modtyp /= model_oil .and. modtyp /= model_2d3d_temp) then
                !
                !         no concentration is to be calculated, except for:
                !         - no. of observations points is larger zero
                !         - modeltype is two-layer temperature model
                !         - modeltype is 3d temperature model
                !
                goto 9999
            endif
            !
        endif

        !     determine this is the first time

        if (first) then
            if (mapfil) then
                first = .false.
                write (lun2, *) ' Writing to new map file:', lname(1:len_trim(lname))
                call openfl (lun1, lname, 1)
                write(lun1) (title(i), i = 1, 4)
                write(lun1) nosubs + 2, num_cells
                write(lun1) (subst(i), i = 1, nosubs + 2)

                if (nefis) then
                    !           group names etc.
                    grnam1 = 'DELPAR_PARAMS'
                    grnam2 = 'DELPAR_RESULTS'
                    type = 'MAP-FILE[PART]'
                    dname(1) = '  '
                    indx = index(filnam, ' ')
                    call delete_file (filnam(:indx - 1) // '.dat', ierr)
                    call delete_file (filnam(:indx - 1) // '.def', ierr)
                    itoff(1) = iyear
                    itoff(2) = imonth
                    itoff(3) = iofset
                    itoff(4) = icwsta
                    itoff(5) = icwsto
                    itoff(6) = icwste
                    itoff(itofmx) = 0
                    !           initialize sizes; 1 - nosubt
                    !                             2 - num_cells
                    !                             3 - nodmp (0 for .map)
                    !                             4 - num_layers
                    !                             5 - nocol (.plo)
                    !                             6 - norow (.plo)
                    !..
                    !.. check this later/ must this be nosubt and mnmax2
                    !.. or must this be nosubs and num_cells ?
                    !.. decided on 26/7 to stick to delwaq mapfiles
                    !.. i.e layers are segments and not substances
                    !..
                    nosize(1) = nosubs + 2
                    nosize(2) = num_cells
                    nosize(3) = 0
                    nosize(4) = num_layers
                    nosize(5) = 0
                    nosize(6) = 0
                    !           set up the element dimensions
                    !           group 1
                    call fill_element_dimensions (elt_dims, 1, 1, 1, 0, 0, 0, 0)
                    call fill_element_dimensions (elt_dims, 2, 1, 4, 0, 0, 0, 0)
                    call fill_element_dimensions (elt_dims, 3, 1, nosubs + 2, 0, 0, 0, 0)
                    call fill_element_dimensions (elt_dims, 4, 1, 1, 0, 0, 0, 0)
                    call fill_element_dimensions (elt_dims, 5, 1, 6, 0, 0, 0, 0)
                    call fill_element_dimensions (elt_dims, 6, 1, 4, 0, 0, 0, 0)
                    call fill_element_dimensions (elt_dims, 7, 1, itofmx, 0, 0, 0, 0)
                    !           group 2
                    call fill_element_dimensions(elt_dims, noparm, 1, 1, 0, 0, 0, 0)
                    do i = 1, nosubs
                        call fill_element_dimensions(elt_dims, noparm + i, 1, num_cells, 0, 0, 0, 0)
                    enddo
                    !           local depths per layer
                    call fill_element_dimensions(elt_dims, noparm + nosubs + 1, 1, num_cells, 0, 0, 0, 0)
                    !           number or particles
                    call fill_element_dimensions(elt_dims, noparm + nosubs + 2, 1, num_cells, 0, 0, 0, 0)
                    !           now write nefis header
                    !           write all elements to file; all definition and creation of files,
                    !           data groups, cells and elements is handled by putget.
                    call putget(filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                            elt_types, elt_bytes, elt_names(1), celid1, wrswch, &
                            ierrem, type)
                    if (ierrem /= 0) then
                        write (lun2, *) 'number 1: ', elt_names(1)
                        goto 100
                    endif
                    call putget(filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                            elt_types, elt_bytes, elt_names(2), celid1, wrswch, &
                            ierrem, title)
                    if (ierrem /= 0) then
                        write (lun2, *) 'number 2: ', elt_names(2)
                        goto 100
                    endif
                    call putget(filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                            elt_types, elt_bytes, elt_names(3), celid1, wrswch, &
                            ierrem, subst)
                    if (ierrem /= 0) then
                        write (lun2, *) 'number 3: ', elt_names(3)
                        goto 100
                    endif
                    call putget(filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                            elt_types, elt_bytes, elt_names(4), celid1, wrswch, &
                            ierrem, dname)
                    if (ierrem /= 0) then
                        write (lun2, *) 'number 4: ', elt_names(4)
                        goto 100
                    endif
                    call putget(filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                            elt_types, elt_bytes, elt_names(5), celid1, wrswch, &
                            ierrem, nosize)
                    if (ierrem /= 0) then
                        write (lun2, *) 'number 5: ', elt_names(5)
                        goto 100
                    endif
                    window(1) = pg%xlow
                    window(2) = pg%xhigh
                    window(3) = pg%ylow
                    window(4) = pg%yhigh
                    call putget(filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                            elt_types, elt_bytes, elt_names(6), celid1, wrswch, &
                            ierrem, window)
                    if (ierrem /= 0) then
                        write (lun2, *) 'number 6: ', elt_names(6)
                        goto 100
                    endif
                    call putget(filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                            elt_types, elt_bytes, elt_names(7), celid1, wrswch, &
                            ierrem, itoff)
                    if (ierrem /= 0) then
                        write (lun2, *) 'number 7: ', elt_names(7)
                    endif
                endif
            endif
        endif

        100 if (use_settling) then
            layts = layt + 1
        else
            layts = layt
        endif

        do m = 1, num_columns
            do n = 1, num_rows
                iseg = lgrid3(n, m)
                if (iseg > 0) then
                    do ilay = 1, layts
                        iseg = lgrid3(n, m) + (ilay - 1) * nosegl
                        do isub = 1, nosubs + 2
                            if (isfile(isub) /= 1) then
                                conc(isub, iseg) = 0.0
                            else
                                if (lgrid(n, m) <= 0) conc(isub, iseg) = 0.0
                            endif
                        enddo
                    enddo
                endif
            enddo
        enddo

        !     add the particles

        do i = npwndw, nopart
            ic = lgrid3(npart(i), mpart(i))
            if (ic >  0) then
                ilay = kpart(i)
                if (ilay > num_layers) then
                    write (lun2, 1000) subnam
                    call stop_exit(1)
                endif
                do isub = 1, nosubs
                    if (isfile(isub) /= 1) then
                        if (modtyp /= model_two_layer_temp) then
                            iseg = (ilay - 1) * nosegl + ic
                            conc(isub, iseg) = conc(isub, iseg) + wpart(isub, i)
                        else
                            ipos = (isub - 1) * num_layers + ilay
                            conc(ipos, ic) = conc(ipos, ic) + wpart(isub, i)
                        endif
                    endif
                enddo
                !           add number of particles as substance nosubs+2
                conc(nosubs + 2, iseg) = conc(nosubs + 2, iseg) + 1
            endif
        enddo

        !     add local depths as substance nosubs+1

        if (isfile(nosubs + 1) /= 1) then
            !$OMP PARALLEL DO PRIVATE   ( m, ic, ilay, iseg, ipos )
            do n = 1, num_rows
                do m = 1, num_columns                             ! there was originally a wrong if statement
                    ic = lgrid3(n, m)                        ! deeper in this set of loops !!
                    if (ic > 0) then
                        do ilay = 1, layt
                            if (modtyp /= model_two_layer_temp) then
                                iseg = (ilay - 1) * nosegl + ic
                                conc(nosubs + 1, iseg) = locdep(lgrid2(n, m), ilay)
                            else
                                ipos = nosubs * num_layers + ilay
                                conc(ipos, ic) = locdep(lgrid2(n, m), ilay)
                            endif
                        enddo
                    endif
                enddo
            enddo
            !$OMP END PARALLEL DO
        endif

        !     sum masses and make concentrations

        atotal = 0.0   ! whole array assignment
        if (nopart - npwndw > -1) then      !  at least one particle is active
            ptlay = 1.0 - pblay
            do isub = 1, nosubs
                do i1 = 1, num_layers
                    pxlay = 1.0                     !  set correct relative thickness layer
                    if (modtyp == model_two_layer_temp) then
                        if (i1 == 1) then        !  top layer
                            pxlay = ptlay
                        elseif (i1 == 2) then    !  bottom layer
                            pxlay = pblay
                        endif
                    endif
                    if (pxlay > 0.0) then              !  don't divide by zero
                        do m = 1, num_columns
                            do n = 1, num_rows
                                i2 = lgrid3(n, m)
                                if (i2 <= 0) cycle
                                iseg = i2 + (i1 - 1) * nosegl
                                ipos = i1 + (isub - 1) * num_layers
                                if (modtyp /= model_two_layer_temp) then
                                    if (conc(isub, iseg) > 0.0) then !  mass found, determine concentration
                                        if (isfile(isub) /= 1) then
                                            atotal(i1, isub) = atotal(i1, isub) + conc(isub, iseg)
                                        elseif (use_settling .and. i1 == num_layers) then
                                            atotal(i1, isub) = atotal(i1, isub) + conc(isub, iseg)
                                        else
                                            atotal(i1, isub) = atotal(i1, isub) + conc(isub, iseg) * &
                                                    &                                                               volume(lgrid2(n, m) + (i1 - 1) * mnmax2)
                                        endif
                                        !.. in oil model (modtyp=model_oil) some substances are floating..
                                        !.. odd ones are floating: concentrations per m2
                                        !.. also for deposited substances on the bottom (extra layer)
                                        !.. also for sticking material
                                        if (modtyp == model_oil .and. isub < 3 * nfract) then       !.. oil module
                                            jsub = mod(isub, 3)
                                            if (jsub == 0) jsub = 3                !.. jsub is 1, 2 or 3 (2 is stick)
                                            if (jsub == 2) then                               !.. dispersed
                                                if (use_settling .and. i1 == num_layers) then
                                                    vnorm = area  (lgrid2(n, m))
                                                else
                                                    vnorm = volume(lgrid2(n, m) + (i1 - 1) * mnmax2)
                                                endif
                                            elseif (mstick(isub) < 0) then                   !.. sticky
                                                vnorm = area(lgrid2(n, m))
                                            else                                                  !.. floating
                                                vnorm = area(lgrid2(n, m))
                                            endif
                                        elseif (use_settling .and. i1 == num_layers) then               !.. other cases
                                            vnorm = area  (lgrid2(n, m))
                                        elseif (mstick(isub) < 0) then
                                            vnorm = area  (lgrid2(n, m))
                                        else
                                            vnorm = volume(lgrid2(n, m) + (i1 - 1) * mnmax2)
                                        endif
                                        !     update only when isfile(isub) ne 1
                                        if (vnorm > 1.0e-25 .and. isfile(isub) /= 1) then
                                            conc(isub, iseg) = conc(isub, iseg) / vnorm
                                        endif
                                    endif
                                else
                                    if (conc(ipos, i2) > 0.0) then
                                        if(isfile(isub) /= 1) then
                                            atotal(i1, isub) = atotal(i1, isub) + conc(ipos, i2)
                                        else
                                            atotal(i1, isub) = atotal(i1, isub) + conc(ipos, i2) * volume(lgrid2(n, m)) / pxlay
                                        endif
                                        if (volume(lgrid2(n, m)) > 1.0e-25 .and. isfile(isub) /= 1) then
                                            conc(ipos, i2) = conc(ipos, i2) / (volume(lgrid2(n, m)) * pxlay)
                                        endif
                                    endif
                                endif
                            end do
                        end do
                    endif
                end do
            end do
        endif

        !     insert missing values

        do m = 1, num_columns
            do n = 1, num_rows
                ic = lgrid3(n, m)
                if (ic > 1) then                    ! this should probably be "> 0"
                    if (lgrid(n, m) <= 0) then
                        do ilay = 1, num_layers
                            iseg = (ilay - 1) * nosegl + ic
                            do isub = 1, nosubs
                                if (isfile(isub) /= 1) then
                                    if (modtyp /= model_two_layer_temp) then
                                        conc(isub, iseg) = -999.0
                                    else
                                        ipos = ilay + (isub - 1) * num_layers
                                        conc(ipos, ic) = -999.0
                                    endif
                                endif
                            enddo
                        enddo
                    endif
                endif
            enddo
        enddo
        !
        !
        if (npwndn > 0) then
            !       reinitialize removed particles; this isn't needed but is't
            !       neater
            do i = npwndw, npwndn
                iptime(i) = 0
                npart (i) = 1
                mpart (i) = 1
                kpart (i) = 1
                do isub = 1, nosubs
                    wpart(isub, i) = 0.0
                enddo
            enddo
            npwndw = npwndn + 1
        endif

        !     check if mapfile must be produced

        if (mapfil) then
            write(lun1) itime, ((conc(i, j), i = 1, nosubs + 2), j = 1, num_cells)
            if (nefis) then
                if (ierrem == 0) then
                    itoff(itofmx) = celid1
                    call putget(filnam, grnam1, noparm - 1, elt_names, elt_dims, &
                            elt_types, elt_bytes, elt_names(7), 1, wrswch, &
                            ierr, itoff)
                    if (ierr  /=  0) then
                        write (lun2, *) 'number 7a: ', elt_names(7)
                        go to 620
                    endif
                    !           write all elements to file; all definition and creation of
                    !           files, data groups, cells and elements is handled by putget.
                    call putget(filnam, grnam2, (nosubs + 2) + 1, &
                            elt_names(noparm:), elt_dims(:, noparm:), elt_types(noparm:), &
                            elt_bytes(noparm:), elt_names(noparm), celid1, &
                            wrswch, ierr, itime)
                    if (ierr  /=  0) then
                        write (lun2, *) 'number 8a: ', elt_names(noparm)
                        go to 620
                    endif
                    celid1 = celid1 + 1
                    do i = 1, nosubs + 2
                        do j = 1, num_cells
                            rbuffr(j) = conc(i, j)
                        enddo
                        call putget(filnam, grnam2, (nosubs + 2) + 1, &
                                elt_names(noparm:), elt_dims(1:, noparm:), elt_types(noparm:), &
                                elt_bytes(noparm:), elt_names(noparm + i), celid2, &
                                wrswch, ierr, rbuffr)
                        if (ierr  /=  0) then
                            write (lun2, *) 'number 9a: ', elt_names(noparm + i)
                            go to 620
                        endif
                    enddo
                    celid2 = celid2 + 1
                    goto 9999
                    620          ierrem = ierr
                endif
            endif
        endif

        !     check possible nefis errors

        if (ierrem  /=  0) then
            write (lun2, 1010)ierrem, subnam, itime / 86400, mod(itime, 86400) / 3600, &
                    &                              mod(itime, 3600) / 60, mod(itime, 60)
        endif
        !
        !     end of subroutine
        !
        9999 if (timon) call timstop (ithndl)
        return
        !
        !     formats
        !
        1000 format(/' Error 4801. Programming error ', a6, &
                ': number of substances!')
        1010 format(' Error 4802. Writing NEFIS file', i10, '; in ', a6, &
                ' at simulation time :'        &
                , i3, 'd ', i2.2, 'h ', i2.2, 'm ', i2.2, 's !')
        !
    end subroutine
end module part12_mod
