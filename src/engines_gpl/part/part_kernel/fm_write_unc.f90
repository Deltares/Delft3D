module m_fm_write_unc

    use netcdf
    use m_waq_precision            ! single and double precision
    use m_partfm_map_netcdf
    use m_partfm_trk_netcdf
    use fileinfo, only: filebase
    use timers
    use m_part_times
    use m_part_geom
    use MessageHandling
    use m_missing
    use m_alloc

    implicit none

contains

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

    subroutine unc_init_trk()

        integer :: ierr

        trkncfilename = trim(filebase) // '_trk.nc'
        ierr = nf90_create(trkncfilename, 0, itrkfile)
        if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'Could not create tracks file.')
        end if
        call unc_addglobalatts(itrkfile)
        ierr = nf90_redef(itrkfile)
        ierr = nf90_def_dim(itrkfile, 'time', nf90_unlimited, id_trk_timedim)
        ierr = nf90_def_var(itrkfile, 'time', nf90_double, id_trk_timedim, id_trk_time)
        ierr = nf90_put_att(itrkfile, id_trk_time, 'units', trim(Tudunitstr))
        ierr = nf90_put_att(itrkfile, id_trk_time, 'standard_name', 'time')

        call unc_write_part_header(itrkfile, id_trk_timedim, id_trk_partdim, id_trk_parttime, &
                id_trk_partx, id_trk_party, id_trk_partz)
        ierr = nf90_enddef(itrkfile)
        it_trk = 0

    end subroutine unc_init_trk


    subroutine unc_write_trk()

        integer :: ierr, time_trk

        integer(kind = int_wp), save :: ithndl = 0              ! handle to time this subroutine

        if (timon) then
            call timstrt("unc_write_trk", ithndl)
        endif

        ! Increment output counters in m_part_flowtimes.
        time_trk = nint(time1)
        it_trk = it_trk + 1
        ierr = nf90_put_var(itrkfile, id_trk_time, time_trk, [ it_trk ])

        call unc_write_part(itrkfile, it_trk, id_trk_parttime, id_trk_partx, id_trk_party, id_trk_partz)

        if (timon) then
            call timstop (ithndl)
        endif

    end subroutine unc_write_trk


    subroutine unc_close_trk()

        integer :: ierr

        ierr = nf90_close(itrkfile)
    end subroutine unc_close_trk


    !> Writes the (possibly aggregated) unstructured network and edge type to a netCDF file for DelWAQ.
    !! If file exists, it will be overwritten.
    subroutine unc_init_map(crs, meshgeom, nosegl, num_layers)

        use partmem, only: nosubs, nfract, oil, substi
        use io_ugrid
        use m_alloc

        type(t_crs), intent(in) :: crs      !< Optional crs containing metadata of unsupported coordinate reference systems
        type(t_ug_meshgeom), intent(in) :: meshgeom !< The complete mesh geometry in a single struct.
        integer, intent(in) :: nosegl   !< Number of segments per layer
        integer, intent(in) :: num_layers    !< Number of layers (meshgeom%numlayer turns out to be -1)

        character(len = 10) :: cell_method   !< cell_method for this variable (one of 'mean', 'point', see CF for details).
        character(len = 50) :: cell_measures !< cell_measures for this variable (e.g. 'area: mesh2d_ba', see CF for details).
        integer :: ierr     !< Result status (UG_NOERR==NF90_NOERR if successful).
        logical :: success  !< Helper variable.
        character(len = 3), dimension(nosubs) :: unit          !< Unit of this variable (CF-compliant) (use empty string for dimensionless quantities).
        integer :: ifract, isub

        character(len = len(substi)) :: substnc
        integer :: i

        !   character(len=*)                 :: var_name      !< Variable name for in NetCDF variable, will be prefixed with mesh name.
        !   character(len=*)                 :: standard_name !< Standard name (CF-compliant) for 'standard_name' attribute in this variable.
        !   character(len=*)                 :: long_name     !< Long name for 'long_name' attribute in this variable (use empty string if not wanted).
        !   character(len=*)                 :: unit          !< Unit of this variable (CF-compliant) (use empty string for dimensionless quantities).

        mapncfilename = trim(filebase) // '_map.nc'
        ierr = nf90_create(mapncfilename, 0, imapfile)
        if (ierr /= nf90_noerr) then
            call mess(LEVEL_WARN, 'Could not create map file.')
            return
        end if
        call realloc(id_map_depth_averaged_particle_concentration, nosubs)

        ierr = UG_NOERR

        ! Add global attributes to NetCDF file.
        call unc_addglobalatts(imapfile)
        ierr = nf90_redef(imapfile)
        ierr = nf90_def_dim(imapfile, 'time', nf90_unlimited, id_map_timedim)
        ierr = nf90_def_var(imapfile, 'time', nf90_double, id_map_timedim, id_map_time)
        ierr = nf90_put_att(imapfile, id_map_time, 'units', trim(Tudunitstr))
        ierr = nf90_put_att(imapfile, id_map_time, 'standard_name', 'time')

        ! Note: a bit of trickery here ... we should probably define the concentrations as a 2D array
        if (num_layers > 1) then
            ierr = nf90_def_dim(imapfile, 'layer', num_layers, id_map_layersdim)
        endif

        ! Write mesh geometry.
        ierr = ug_write_mesh_struct(imapfile, meshids, networkids, crs, meshgeom)
        if (ierr /= nf90_noerr) then
            call mess(LEVEL_ERROR, 'Could not write geometry to file map')
            return
        end if

        cell_method = 'mean' !< Default cell average.
        cell_measures = ''

        ! add concentrations of all available substances
        ! adapt units for surfcace and sticky oil
        unit = 'm-3'
        if (oil) then
            do ifract = 1, nfract
                unit(1 + 3 * (ifract - 1)) = 'm-2'
                unit(3 + 3 * (ifract - 1)) = 'm-2'
            enddo
        endif

        do isub = 1, nosubs
            substnc = substi(isub)

            do i = 1, len_trim(substnc)
                if (substnc(i:i) == ' ') then
                    substnc(i:i) = '_'
                endif
            enddo

            if (num_layers == 1) then
                ierr = ug_def_var(imapfile, id_map_depth_averaged_particle_concentration(isub), [meshids%dimids(mdim_face), id_map_timedim], nf90_double, UG_LOC_FACE, &
                        trim(meshgeom%meshName), substnc, 'depth_averaged_particle_concentration', &
                        substi(isub), unit(isub), cell_method, cell_measures, crs, ifill = -999, dfill = dmiss)
            else
                ierr = ug_def_var(imapfile, id_map_depth_averaged_particle_concentration(isub), [meshids%dimids(mdim_face), id_map_layersdim, id_map_timedim], nf90_double, UG_LOC_FACE, &
                        trim(meshgeom%meshName), substnc, 'particle_concentration', &
                        substi(isub), unit(isub), cell_method, cell_measures, crs, ifill = -999, dfill = dmiss)
            endif
        end do

        if (ierr /= nf90_noerr) then
            call mess(LEVEL_ERROR, 'Could not create concentration variable in map file')
            return
        end if

        ierr = nf90_enddef(imapfile)
        ierr = nf90_sync(imapfile)

        it_map = 0
        call realloc(work, ndx, keepExisting = .false., fill = dmiss)

    end subroutine unc_init_map


    subroutine unc_write_map()

        use partmem, only: nosubs, hyd, noslay
        use m_part_transport
        use m_part_flow, only: h1
        use m_particles, only: part_iconst

        implicit none

        integer :: ierr, time_map, k, isub

        integer(kind = int_wp), save :: ithndl = 0

        if (timon) then
            call timstrt("unc_write_map", ithndl)
        endif

        ! Increment output counters in m_part_flowtimes.
        time_map = nint(time1)
        it_map = it_map + 1
        ierr = nf90_put_var(imapfile, id_map_time, time_map, [it_map])
        if (ierr /= nf90_noerr) then
            call mess(LEVEL_ERROR, 'Could not write time to file map')
            if (timon) then
                call timstop(ithndl)
            endif
            return
        end if

        ! AM: this is superfluous: we calculate the concentrations per substance, so we could
        !     simply fill the work array directly
        do isub = 1, nosubs
            call comp_concentration(h1, nosubs, isub, constituents)

            ! AM: consider using a 2D pointer p, pointing to the work array as p(nosegl,num_layers)
            !     then we can make the structure of the netCDF file nicer
            if (hyd%num_layers == 1) then
                ierr = nf90_put_var(imapfile, id_map_depth_averaged_particle_concentration(isub), &
                        constituents(isub, :, 1), start = [1, it_map])
            else
                ierr = nf90_put_var(imapfile, id_map_depth_averaged_particle_concentration(isub), &
                        constituents(isub, :, :), start = [1, 1, it_map])
            endif

            if (ierr /= nf90_noerr) then
                call mess(LEVEL_ERROR, 'Could not write concentrations to file map')
                if (timon) then
                    call timstop(ithndl)
                endif
                return
            end if
        end do
        ierr = nf90_sync(imapfile)

        if (timon) then
            call timstop (ithndl)
        endif

    end subroutine unc_write_map


    subroutine unc_close_map()

        implicit none

        integer :: ierr

        ierr = nf90_close(imapfile)
    end subroutine unc_close_map


    !> write particle tracks header to netcdf trk file
    subroutine unc_write_part_header(ifile, id_timedim, id_trk_partdim, id_trk_parttime, &
            id_trk_partx, id_trk_party, id_trk_partz)
        use m_particles
        use m_part_flow, only: kmx
        use m_sferic, only: jsferic
        implicit none

        integer, intent(in) :: ifile  !< output file identifier
        integer, intent(in) :: id_timedim
        integer, intent(inout) :: id_trk_partdim, id_trk_parttime, id_trk_partx, id_trk_party, id_trk_partz

        character(len = 128) :: mesg

        integer :: ierr
        logical :: indefine

        ! Put dataset in define mode (possibly again) to add dimensions and variables.
        ierr = nf90_redef(ifile)
        indefine = (ierr == nf90_eindefine) ! Was still in define mode.

        if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
            call mess(LEVEL_ERROR, 'Could not put header in flow geometry file.')
            return
        end if

        ierr = nf90_def_dim(ifile, 'particles', NopartTot, id_trk_partdim)

        ierr = nf90_def_var(ifile, 'particles_time', nf90_double, id_timedim, id_trk_parttime)
        ierr = nf90_put_att(ifile, id_trk_parttime, 'long_name', 'particles time')

        ierr = nf90_def_var(ifile, 'particles_x_coordinate', nf90_double, [ id_trk_partdim, id_timedim ], id_trk_partx)
        ierr = nf90_def_var(ifile, 'particles_y_coordinate', nf90_double, [ id_trk_partdim, id_timedim ], id_trk_party)

        if (jsferic == 0) then
            ierr = nf90_put_att(ifile, id_trk_partx, 'units', 'm')
            ierr = nf90_put_att(ifile, id_trk_party, 'units', 'm')
            ierr = nf90_put_att(ifile, id_trk_partx, 'standard_name', 'projection_x_coordinate')
            ierr = nf90_put_att(ifile, id_trk_party, 'standard_name', 'projection_y_coordinate')
            ierr = nf90_put_att(ifile, id_trk_partx, 'long_name', 'x-coordinate')
            ierr = nf90_put_att(ifile, id_trk_party, 'long_name', 'y-coordinate')
        else
            ierr = nf90_put_att(ifile, id_trk_partx, 'units', 'degrees_east')
            ierr = nf90_put_att(ifile, id_trk_party, 'units', 'degrees_north')
            ierr = nf90_put_att(ifile, id_trk_partx, 'standard_name', 'longitude')
            ierr = nf90_put_att(ifile, id_trk_party, 'standard_name', 'latitude')
            ierr = nf90_put_att(ifile, id_trk_partx, 'long_name', 'longitude')
            ierr = nf90_put_att(ifile, id_trk_party, 'long_name', 'latitude')
        end if
        ierr = nf90_put_att(ifile, id_trk_partx, 'long_name', 'x-coordinate of particles')
        ierr = nf90_put_att(ifile, id_trk_party, 'long_name', 'y-coordinate of particles')
        ierr = nf90_put_att(ifile, id_trk_partx, '_FillValue', dmiss)
        ierr = nf90_put_att(ifile, id_trk_party, '_FillValue', dmiss)

        if (kmx > 0) then
            ierr = nf90_def_var(ifile, 'particles_z_coordinate', nf90_double, (/ id_trk_partdim, id_timedim /), id_trk_partz)
            ierr = nf90_put_att(ifile, id_trk_partz, 'long_name', 'z-coordinate of particles')
            ierr = nf90_put_att(ifile, id_trk_partz, '_FillValue', dmiss)
        end if

        ! Leave the dataset in the same mode as we got it.
        if (.not. indefine) then
            ierr = nf90_enddef(ifile)
        end if

    end subroutine unc_write_part_header


    !> write particles to netcdf file
    subroutine unc_write_part(ifile, itime, id_trk_parttime, id_trk_partx, id_trk_party, id_trk_partz)
        use partmem, only: nopart, hyd, mpart, noslay, use_settling
        use m_part_mesh
        use m_particles, laypart => kpart
        use m_sferic
        use m_sferic_part, only: ptref
        use m_part_flow, only: kmx, h1
        use m_part_geom, only: bl
        use geometry_module, only: cart3Dtospher
        implicit none

        integer, intent(in) :: ifile  !< output file identifier
        integer, intent(in) :: itime
        integer, intent(in) :: id_trk_parttime, id_trk_partx, id_trk_party, id_trk_partz

        real(kind = dp), dimension(:), allocatable :: xx, yy, zz

        real(kind = dp) :: dis2

        integer :: i, i0, ii, iglb, k, lay, kl, klp1, layacc
        integer :: ierr

        real(kind = dp), parameter :: dtol = 1d-8

        integer, save :: icount = 0

        icount = icount + 1

        !  allocate
        call realloc(xx, NopartTot, keepExisting = .false., fill = dmiss)
        call realloc(yy, NopartTot, keepExisting = .false., fill = dmiss)
        if (kmx > 0) then
            call realloc(zz, NopartTot, keepExisting = .false., fill = dmiss)
        endif

        if (jsferic == 1) then
            do ii = 1, Nopart
                call Cart3Dtospher(xpart(ii), ypart(ii), zpart(ii), xx(ii), yy(ii), ptref)
            end do
        else
            do ii = 1, NopartTot
                xx(ii) = xpart(ii)
                yy(ii) = ypart(ii)
            end do
        end if

        ierr = nf90_put_var(ifile, id_trk_parttime, timepart, [ itime ])
        if (ierr == 0) then
            ierr = nf90_put_var(ifile, id_trk_partx, xx, start = [ 1, itime ], count = [ NopartTot, 1 ])
            if (ierr == 0) then
                ierr = nf90_put_var(ifile, id_trk_party, yy, start = [ 1, itime ], count = [ NopartTot, 1 ])
            endif
        endif

        !
        ! Compute the height of the particles in the water from the layer (laypart)
        ! and the position within the layer. Then write it to the file
        !
        ! Beware: translate from the computational, refined grid to the original
        ! underlying hydrodynamic grid via the array cell2nod
        !
        if (kmx > 0) then
            do i = 1, NopartTot
                k = mpart(i)
                lay = laypart(i)

                zz(i) = 0.0
                if (k > 0 .and. lay > 0) then
                    k = abs(cell2nod(k))
                    zz(i) = bl(k)

                    ! If the particles are still in the water, then determine
                    ! the height from the bottom
                    if (lay < noslay .or. .not. use_settling) then
                        do layacc = hyd%num_layers, lay + 1, -1
                            kl = k + (layacc - 1) * hyd%nosegl
                            zz(i) = zz(i) + h1(kl)
                        enddo

                        kl = k + (lay - 1) * hyd%nosegl
                        zz(i) = zz(i) + (1.0 - hpart(i)) * h1(kl)
                    endif
                endif
            enddo

            ierr = nf90_put_var(ifile, id_trk_partz, zz, start = [ 1, itime ], count = [ NopartTot, 1 ])
        end if

        !  error handling
        if (ierr /= 0) then
            call mess(LEVEL_ERROR, 'particles output error')
        end if

    end subroutine unc_write_part


    !> Puts global attributes in NetCDF data set.
    !! This includes: institution, Conventions, etc.
    subroutine unc_addglobalatts(ncid)
        use delwaq_version_module

        integer, intent(in) :: ncid

        character(len = 8) :: cdate
        character(len = 10) :: ctime
        character(len = 5) :: czone
        integer :: ierr
        logical :: indefine

        ierr = nf90_noerr

        ierr = nf90_redef(ncid)
        indefine = (ierr == nf90_eindefine) ! Was still in define mode.
        if (ierr /= nf90_noerr .and. ierr /= nf90_eindefine) then
            write (msgbuf, '(a,i0,a,i0,a,a)') 'Could not put global attributes in NetCDF #', ncid, '. Error code ', ierr, ': ', nf90_strerror(ierr)
            call err_flush()
            return
        end if

        ierr = nf90_put_att(ncid, nf90_global, 'institution', trim(company))
        ierr = nf90_put_att(ncid, nf90_global, 'references', trim(company_url))
        ierr = nf90_put_att(ncid, nf90_global, 'source', major_minor_buildnr)

        call date_and_time(cdate, ctime, czone)
        ierr = nf90_put_att(ncid, nf90_global, 'history', &
                'Created on ' // cdate(1:4) // '-' // cdate(5:6) // '-' // cdate(7:8) // 'T' // ctime(1:2) // ':' // ctime(3:4) // ':' // ctime(5:6) // czone(1:5) // ', PART')
        ierr = nf90_put_att(ncid, nf90_global, 'date_created', cdate(1:4) // '-' // cdate(5:6) // '-' // cdate(7:8) // 'T' // ctime(1:2) // ':' // ctime(3:4) // ':' // ctime(5:6) // czone(1:5))
        ierr = nf90_put_att(ncid, nf90_global, 'date_modified', cdate(1:4) // '-' // cdate(5:6) // '-' // cdate(7:8) // 'T' // ctime(1:2) // ':' // ctime(3:4) // ':' // ctime(5:6) // czone(1:5))

        ierr = nf90_put_att(ncid, nf90_global, 'Conventions', 'CF-1.5 Deltares-0.1')

        ! Leave the dataset in the same mode as we got it.
        if (indefine) then
            ierr = nf90_enddef(ncid)
        end if
    end subroutine unc_addglobalatts


    !> Sets the UDUnit timestring based on current model time settings.
    !! Module variable Tudunitstr can the be used in various output routines.
    subroutine setTUDUnitString()

        implicit none

        integer :: Tzonehrs
        character(len = 1) :: Tzonesgn

        Tzonehrs = int(TZone)
        Tzonesgn = merge('-', '+', Tzone < 0)

        write(Tudunitstr, '(a,i2.2,a)') 'seconds since ' // refdat(1:4) // '-' // refdat(5:6) // '-' // refdat(7:8) // ' 00:00:00 ' // Tzonesgn, abs(Tzonehrs), ':00'

    end subroutine setTUDUnitString


    !> compute concentrations of particles (parts per unit volume) in flownodes
    subroutine comp_concentration(h, nconst, iconst, c)
        use partmem, only: mpart, wpart, oil, nfract, nopart, hyd, noslay, nolayp, use_settling
        use m_particles, laypart => kpart
        use m_part_mesh
        use m_part_geom, only: Ndx, ba, bl
        use m_part_parameters, only: epshs
        use m_part_flow, only: Ndkx, kmx

        implicit none

        real(kind = dp), dimension(:), intent(in) :: h      !< water depth
        integer(kind = int_wp), intent(in) :: nconst !< number of constituents
        integer(kind = int_wp), intent(in) :: iconst !< particle tracer constituent number
        real(kind = dp), dimension(:, :, :), intent(out) :: c      !< constituents

        integer :: i, k, kl, ifract, lay

        integer(kind = int_wp) :: ithndl = 0             ! handle to time this subroutine

        if (timon) then
            call timstrt("comp_concentration", ithndl)
        endif

        c(iconst, :, :) = 0.0_dp

        !  count number of particles per cell
        do i = 1, Nopart
            k = mpart(i)
            if (k == 0) then
                cycle
            endif

            k = abs(cell2nod(k))
            lay = min(laypart(i), noslay)

            c(iconst, k, lay) = c(iconst, k, lay) + wpart(iconst, i)
        end do

        !  compute concentration (parts per unit volume) , but for the oil module should it be per m2 (ie divided by the depth of the segment), for sticky and surface oil
        do lay = 1, nolayp
            do k = 1, hyd%nosegl
                kl = k + (lay - 1) * hyd%nosegl
                if (h(kl) > epshs) then
                    c(iconst, k, lay) = c(iconst, k, lay) / (ba(kl) * h(kl))

                    if (oil) then
                        do ifract = 1, nfract
                            c(1 + 3 * (ifract - 1), k, lay) = c(1 + 3 * (ifract - 1), k, lay) * h(kl) ! surface floating oil per m2
                            c(3 + 3 * (ifract - 1), k, lay) = c(3 + 3 * (ifract - 1), k, lay) * h(kl) ! surface floating oil per m2
                        end do
                    endif
                endif
            end do
        end do

        ! compute concentration in the bottom layer - if settling is on (concentration in g/m2 not g/m3)

        if (use_settling) then
            do k = 1, hyd%nosegl
                c(iconst, k, noslay) = c(iconst, k, noslay) / ba(k)
            end do
        end if

        if (timon) then
            call timstop (ithndl)
        endif

    end subroutine comp_concentration

end module m_fm_write_unc
