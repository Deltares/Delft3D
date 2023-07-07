module HRextensions
   !----- LGPL --------------------------------------------------------------------
   !
   !  Copyright (C)  Stichting Deltares, 2011-2018.
   !
   !  This library is free software; you can redistribute it and/or
   !  modify it under the terms of the GNU Lesser General Public
   !  License as published by the Free Software Foundation version 2.1.
   !
   !  This library is distributed in the hope that it will be useful,
   !  but WITHOUT ANY WARRANTY; without even the implied warranty of
   !  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   !  Lesser General Public License for more details.
   !
   !  You should have received a copy of the GNU Lesser General Public
   !  License along with this library; if not, see <http://www.gnu.org/licenses/>.
   !
   !  contact: delft3d.support@deltares.nl
   !  Stichting Deltares
   !  P.O. Box 177
   !  2600 MH Delft, The Netherlands
   !
   !  All indications and logos of, and references to, "Delft3D" and "Deltares"
   !  are registered trademarks of Stichting Deltares, and remain the property of
   !  Stichting Deltares. All rights reserved.
   !
   !-------------------------------------------------------------------------------
   !  
   !  $URL$
   !!--description-----------------------------------------------------------------
   !! extensions for SWAN for HR (Hydraulic Boundaries / in Dutch: Hydraulische Randvoorwaarden)
    use swn_outnc
    use netcdf
    use netcdf_tools
    use nctablemd, only : nctable_record, get_nctable_record
    use M_GENARR, only: SPCSIG
    use M_PARALL, only : IAMMASTER, MXCGL, MYCGL, MCGRDGL, KGRPGL, SWREAL, MXF, MXL, MYF, MYL
    use agioncmd
    use SWCOMM2, only : NUMGRD, NBGRPT, NBSPEC, XOFFS, YOFFS
    use SWCOMM3, only : MXC, MYC, MDC, MSC, PI, PI2, DNORTH
    use SWCOMM4, only : KSPHER, LENDEG
    use OCPCOMM4, only : ITEST, PRINTF, PRTEST
    implicit none
    private
    public :: swn_hre_initva, swn_hre_bcnccf, swn_hre_get_bpnt_2d, swn_hre_get_bpnt_1d, &
              swn_hre_readarray, swn_hre_timf, swn_hre_inpgr
!
    character(len=*), parameter  ::  scale_dens_name  =  'scale_density'
    logical :: flippedY(NUMGRD) = .false.

contains
    subroutine swn_hre_initva(ncfile, KGRPNT, AC2)
        character(len=*),          intent( in) :: ncfile
        integer,                   intent( in) :: KGRPNT(MXC, MYC)
        real, dimension(:,:,:)                 :: AC2

        integer                                :: jx, jy, ix, iy, indx, iindx, &
                                                  ncid, ri, is
        type(recordaxe_type)                   :: recordaxe
        type(mapgrid_type)                     :: mapgrid
        type(pntgrid_type)                     :: pntgrid
        type(spcgrid_type)                     :: spcgrid
        real                                   :: edloc(MDC, MSC)
        real, dimension(:,:,:, :), allocatable :: density
        logical                                :: spc_as_map, STPNOW

        if ( IAMMASTER ) then
            call open_ncfile( ncfile, 'read', ncid)

            call swn_hre_get_grids(ncid, recordaxe, spcgrid, mapgrid, pntgrid, spc_as_map)
            if ( STPNOW() ) return
            
            ! For now, always pick the last field. The problem is that until the COMPUTE
            ! command is called, we have no idea at what time the computation will start
            ! and hence what record index to pick
            ri = recordaxe%ncontent

            if ( spc_as_map ) then
                allocate(density(MDC, MSC, MXCGL, MYCGL))
            else
                allocate(density(MDC, MSC, MCGRDGL-1, 1))
            end if
            call swn_hre_read_density(ncid, density, spc_as_map, ri)
            call close_ncfile(ncid)
        end if
        ! assign spectra to all members
        do jx=1,MXCGL
            do jy=1,MYCGL
                if ( IAMMASTER ) then
                    edloc(:,:) = 0.
                    iindx = KGRPGL(jx, jy)
                    if ( iindx /= 1 ) then
                        if ( spc_as_map ) then
                            edloc = density(:,:, jx, jy)
                        else
                            edloc = density(:,:, iindx - 1, 1)
                        end if
                    end if
                end if
                call SWBROADC (edloc, MDC*MSC, SWREAL)
                if ( MXF <= jx .and. MXL >= jx .and. MYF <= jy .and. MYL >= jy ) then
                    iy   = jy - MYF + 1
                    ix   = jx - MXF + 1
                    indx = KGRPNT(ix, iy)
                    if ( indx /= 1 ) then
                        do is=1, MSC
                            ! energy to action density
                            AC2(:,is, indx) = edloc(:,is) / (2.0 * PI * SPCSIG(is))
                        end do
                    end if
                end if
            end do
        end do
        if ( IAMMASTER ) deallocate(density)

    end subroutine swn_hre_initva

    subroutine swn_hre_get_grids(ncid, recordaxe, spcgrid, mapgrid, pntgrid, spc_as_map)
        integer         ,          intent( in) :: ncid
        type(recordaxe_type),      intent(out) :: recordaxe
        type(mapgrid_type),        intent(out) :: mapgrid
        type(pntgrid_type),        intent(out) :: pntgrid
        type(spcgrid_type),        intent(out) :: spcgrid
        logical,                   intent(out) :: spc_as_map

        integer                     :: pvarid
        character(len=128)          :: errmsg
        character(len=*), parameter :: fmt1 = '("Number of ", a," in file does not match spectral grid: ", i0, x, i0)'
        character(len=*), parameter :: fmt2 = '("Number of elements in ", a1, " direction in file does not match spatial grid: ", i0, x, i0)'

        !
        ! Spectral grid
        !
        call agnc_get_griddef(ncid, spcgrid)
        if ( spcgrid%ndir /= MDC ) then
            write(errmsg,fmt1) "directions", spcgrid%ndir, MDC
            call MSGERR(4, errmsg)
            return
        end if
        if ( spcgrid%nfreq /= MSC ) then
            write(errmsg,fmt1) "frequencies", spcgrid%nfreq, MSC
            call MSGERR(4, errmsg)
            return
        end if

        !
        ! Use the presence of the points dimension as selector for mapgrid (2D) or pntgrid (1D)
        !
        if ( nf90_inq_dimid(ncid, 'points', pvarid) == nf90_noerr) then
            !
            ! pntgrid (unstructured or 1D)
            !
            call agnc_get_pntgrid(ncid, pntgrid)
            spc_as_map = .false.
            if ( allocated(pntgrid%ips) ) then
                ! this is NOT a unstructured mesh, ips would always be 1:numnodes
                if ( pntgrid%xdimlen /= MXCGL ) then
                    write(errmsg,fmt2) "X", pntgrid%xdimlen, MXCGL
                    call MSGERR(4, errmsg)
                    return
                end if
                if ( pntgrid%ydimlen /= MYCGL ) then
                    write(errmsg,fmt2) "Y", pntgrid%ydimlen, MYCGL
                    call MSGERR(4, errmsg)
                    return
                end if
            end if
        else
            !
            ! mapgrid
            !
            call agnc_get_mapgrid(ncid, mapgrid)
            spc_as_map = .true.
            if ( mapgrid%nx /= MXCGL ) then
                write(errmsg,fmt2) "X", mapgrid%nx, MXCGL
                call MSGERR(4, errmsg)
                return
            end if
            if ( mapgrid%ny /= MYCGL ) then
                write(errmsg,fmt2) "Y", mapgrid%ny, MYCGL
                call MSGERR(4, errmsg)
                return
            end if
        end if
        !
        ! record axe
        !
        call agnc_get_recordaxe(ncid, recordaxe)

    end subroutine swn_hre_get_grids

    subroutine swn_hre_read_density(ncid, density, spc_as_map, ri)
        integer,                    intent(in   ) :: ncid, ri
        real, dimension(:,:,:,:),   intent(inout) :: density
        logical,                    intent(in   ) :: spc_as_map

        ! local
        real, dimension(:,:), allocatable         :: scale_factor
        integer                                   :: evarid, svarid, n, m, j, i, fill_value
        character(len=14)                         :: ename

        ename = 'density'

        ! should I search for the standard name? At the moment not usefull as agnc_get_griddef uses fixed names too
        call nccheck( nf90_inq_varid(ncid, ename, evarid) )
        if ( evarid == -1  ) then
            call MSGERR(4, "spectral file does not appear to have a variable " // trim(ename))
            return
        end if

        if ( spc_as_map ) then
            call nccheck( nf90_get_var(ncid, evarid, density, (/1, 1, 1, 1, ri/) ) )
        else
            call nccheck( nf90_get_var(ncid, evarid, density, (/1, 1, 1, ri/) ) )
        end if

        if ( nf90_inq_varid(ncid, scale_dens_name, svarid)  == nf90_noerr ) then

            call nccheck ( nf90_get_att(ncid, evarid, "_FillValue", fill_value), "_FillValue" )
            if ( spc_as_map ) then
                allocate(scale_factor(MXCGL, MYCGL))
                call nccheck( nf90_get_var(ncid, svarid, scale_factor, (/1, 1, ri/) ) )

                do n=1, MXCGL
                    do m=1, MYCGL
                    ! when density is scaled, it is stored as a int16, so is the _FillValue.
                        do j=1, MDC
                            do i=1, MSC
                                if ( nint(density(j, i, n, m)) /= fill_value ) then
                                    density(j, i, n, m) = density(j, i, n, m) * scale_factor(n, m)
                                else
                                    density = 0.
                                end if
                            end do
                        end do
                    end do
                end do
            else
                allocate(scale_factor(MCGRDGL-1, 1))
                call nccheck( nf90_get_var(ncid, svarid, scale_factor, (/1, ri/) ) )

                do n=1, MCGRDGL - 1
                    ! when density is scaled, it is stored as a int16, so is the _FillValue.
                    do j=1, MDC
                        do i=1, MSC
                            if ( nint(density(j, i, n, 1)) /= fill_value ) then
                                density(j, i, n, 1) = density(j, i, n, 1) * scale_factor(n, 1)
                            else
                                density(j, i, n, 1) = 0.
                            end if
                        end do
                    end do
                end do
            end if

            deallocate(scale_factor)
        end if

    end subroutine swn_hre_read_density

    subroutine swn_hre_inpgr(IGR1, IGR2, ncfile, isCurvi)
        use SWCOMM2, only: XPG, YPG, ALPG, COSPG, SINPG, MXG, MYG, DXG, DYG, &
                           LXOFFS, XOFFS, YOFFS, EXCFLD, &
                           IFLBEG, IFLINT, IFLEND, IFLDYN, IFLTIM
        
        character(len=*),          intent( in) :: ncfile
        integer,                   intent( in) :: IGR1, IGR2
        logical,                   intent(out) :: isCurvi

        integer                                :: ncid, varid
        real(kind=4)                           :: sf, ao, fv, minlon, minlat
        type(mapgrid_type)                     :: mapgrid
        type(recordaxe_type)                   :: recordaxe

        real, external :: DTTIME

        call open_ncfile( ncfile, 'read', ncid)
        if ( ITEST >= 50 ) write(PRINTF,'("Open ", A)') trim(ncfile)
        call agnc_get_mapgrid(ncid, mapgrid)

        if ( .not.LXOFFS ) then
            XOFFS  = mapgrid%longitude(1,1)
            YOFFS  = mapgrid%latitude(1,1)
            LXOFFS = .true.
            YPG(IGR1)   = 0.
            XPG(IGR1)   = 0.
        else
            if (mapgrid%longitude(1,1) /= mapgrid%fillValue .and. mapgrid%latitude(1,1) /= mapgrid%fillValue) then
                XPG(IGR1)   = mapgrid%longitude(1,1) - XOFFS
                YPG(IGR1)   = mapgrid%latitude(1,1)  - YOFFS
            else
                minlon = minval(mapgrid%longitude, mask=mapgrid%longitude /= mapgrid%fillValue)
                minlat = minval(mapgrid%latitude,  mask=mapgrid%latitude /= mapgrid%fillValue)
                XPG(IGR1) = minlon - XOFFS
                YPG(IGR1) = minlat - YOFFS
            endif
        end if

        MXG(IGR1)   = mapgrid%nx
        MYG(IGR1)   = mapgrid%ny
        isCurvi     = mapgrid%isCurvi
        flippedY(IGR1) = mapgrid%flippedY
        if (flippedY(IGR1)) then
            write(PRINTF,*) 'In file "', trim(ncfile), '":'
            write(PRINTF,*) 'Input grid has different orientation as grid file. Will be corrected.'
        endif
        if (isCurvi) then
            ALPG(IGR1)  = 0.0
        else
            ALPG(IGR1)  = mapgrid%alpc
        endif
        COSPG(IGR1) = cos(ALPG(IGR1))
        SINPG(IGR1) = sin(ALPG(IGR1))
        DYG(IGR1)   = mapgrid%dy
        DXG(IGR1)   = mapgrid%dx

        ! read exception value
        call swn_hre_grid_varid(ncid, IGR1, varid)
        call get_scalies_float(ncid, varid, ao, sf, fv)
        EXCFLD(IGR1) = fv
        if ( IGR2 /= 0 ) EXCFLD(IGR2) = fv
        if ( IGR2 /= 0 ) flippedY(IGR2) = mapgrid%flippedY

        !
        ! DATETIME if grid not is 1: bathymetry
        !    datevec_from_epoch in agioncmd.ftn90
        !    DTTIME in opcmix.ftn
        !
        if ( IGR1 > 1 ) then
            call agnc_get_recordaxe(ncid, recordaxe)
            if ( recordaxe%nstatm .and. recordaxe%ncontent > 0 ) then
                IFLBEG(IGR1) = DTTIME(datevec( recordaxe%content(recordaxe%first) ))
                IFLINT(IGR1) = recordaxe%delta
                IFLEND(IGR1) = DTTIME(datevec( recordaxe%content(recordaxe%last) ))
                IFLDYN(IGR1) = 1
                IFLTIM(IGR1) = IFLBEG(IGR1)
                if (IGR2 > 0) then
                    IFLBEG(IGR2) = IFLBEG(IGR1)
                    IFLINT(IGR2) = IFLINT(IGR1)
                    IFLEND(IGR2) = IFLEND(IGR1)
                    IFLDYN(IGR2) = IFLDYN(IGR1)
                    IFLTIM(IGR2) = IFLTIM(IGR1)
                end if
            end if
        end if

        call close_ncfile(ncid)

    end subroutine swn_hre_inpgr

    subroutine swn_hre_readarray(IFLNDF, TARR, MXG, MYG, IFLTIM, IFLFAC, igr, filename)
        integer,                   intent(in   ) :: MXG(NUMGRD), MYG(NUMGRD), igr
        real(kind=8),              intent(in   ) :: IFLTIM(NUMGRD)
        integer,                   intent(inout) :: IFLNDF(NUMGRD)
        real, dimension(:),        intent(inout) :: TARR(MXG(igr), MYG(igr)), IFLFAC(NUMGRD)
        character(len=*),          intent(in)    :: filename

        integer                                  :: varid, ndims, mydatevec(6), xpctime, &
                                                    ri, ncid, xtype, i, j, missing
        real(kind=4)                             :: add_offset, scale_factor, fill_value
        logical                                  :: STPNOW
        type(recordaxe_type)                     :: recordaxe
        real, allocatable                        :: temp(:,:)

        ! IFLNDF is uint16, not integer*4. Hence the introduction of an offset
        ncid = IFLNDF(igr)*-1
        if (ITEST >= 100) then
            write (PRINTF, '("TEST swn_hre_readarray: ", 4I8 )') MXG(igr), MYG(igr), ncid, igr
        end if

        call swn_hre_grid_varid(ncid, igr, varid, ndims)
        if ( STPNOW() ) return

        if ( ndims == 3 ) then
            ! compute seconds since epoch
            call DTINTI (IFLTIM(igr), mydatevec)
            xpctime = seconds_since_epoch ( mydatevec )

            ! find the record in the netcdf file that matches the time of interest
            call agnc_get_recordaxe(ncid, recordaxe)
            call record_index(ncid, recordaxe, xpctime, ri, .true., filename)

            ! read data
            if (flippedY(igr)) then
                allocate(temp(MXG(igr), MYG(igr)))
                call nccheck( nf90_get_var(ncid, varid, temp, (/1,1,ri/)) )
                do i = 1, MXG(igr)
                    do j = 1, MYG(igr)
                        TARR(i,j) = temp(i, MYG(igr)+1-j)
                    enddo
                enddo
                deallocate(temp)
            else
                call nccheck( nf90_get_var(ncid, varid, TARR, (/1,1,ri/)) )
            endif
        else
            ! flat 2D matrix for stationary runs
            call nccheck( nf90_get_var(ncid, varid, TARR) )
        end if

        ! scale the data
        call get_scalies_float(ncid, varid, add_offset, scale_factor, fill_value, xtype)
        if ( xtype == NF90_SHORT .or. xtype == NF90_BYTE .or. xtype == NF90_INT) then
           if (scale_factor /= 1.0 .or. add_offset /= 0.0) then
              do i = 1, MXG(igr)
                 do j = 1, MYG(igr)
                    if ( TARR(i,j) /= fill_value ) then
                       TARR(i,j) = TARR(i,j) * scale_factor + add_offset
                    endif
                 enddo
              enddo
           endif
        endif

        !
        ! check for only missing values
        !
        missing = 0
        do i = 1, MXG(igr)
           do j = 1, MYG(igr)
              if ( TARR(i,j) == fill_value ) then
                 missing = missing + 1
              endif
           enddo
        enddo
        if (missing == MXG(igr) * MYG(igr)) then
           write(PRINTF,*)
           write(PRINTF,'(x,a,6(x,i0))') 'ERROR: Only missing values for date / time = ', mydatevec
           write(PRINTF,'(x,2a)') 'in file: ', trim(filename)
           write(PRINTF,*)
           call swexitmpi
           stop 1
        endif

        !
        ! apply multiplication factor (if not equal to 1.0)
        !
        if (IFLFAC(igr) /= 1.0) then
            do i = 1, MXG(igr)
               do j = 1, MYG(igr)
                  if ( TARR(i,j) /= fill_value ) then
                     TARR(i,j) = TARR(i,j) * IFLFAC(igr)
                  endif
               enddo
            enddo
        endif

    end subroutine swn_hre_readarray

    subroutine swn_hre_grid_varid(ncid, igr, varid, ndims)
        integer,                   intent(in   ) :: igr, ncid
        integer,                   intent(  out) :: varid
        integer, optional,         intent(  out) :: ndims
        type(nctable_record)                     :: trecord
        character(len=256)                       :: errmsg
        character(len=9)                         :: vnames(NUMGRD), vname
        logical                                  :: found
        if ( igr > NUMGRD ) then
            write(errmsg,*) "Nr. of known grids is ", NUMGRD, " not ", igr
            call MSGERR(4, errmsg)
            return
        end if
        vnames = (/'depth    ', &
                   'xcur     ', &
                   'ycur     ', &
                   'fric     ', &
                   'xwnd     ', &
                   'ywnd     ', &
                   'ssh      ', &
                   'latitude ', &
                   'longitude', &
                   'astd     ', &
                   'nplants  ', &
                   'turb_visc', &
                   'fl_mud_lr', &
                   'icec     ', &
                   'hice     ' /)

        vname = vnames(igr)

        ! first see if vname is a variable, standard or short_name
        call agnc_get_varid_by_name(ncid, vname, varid)
        errmsg = ' '
        if ( varid < 0 ) then
            ! nope, look up by standard_name asconfigured in nctablemd.ftn90
            call get_nctable_record(vname, trecord, found)
            if ( .not. found ) then
                errmsg = "Cannot read " // trim(vname) // " from netcdf file. Unknown grid type"
            end if
            if ( trecord%standard_name /= 'none' ) then
                call agnc_get_varid_by_name(ncid, trecord%standard_name, varid)
                if ( varid < 0 ) then
                    if ( vname =='ssh' ) then
                        ! quick and dirty until I figured out how to parse lists of standard names that are not
                        ! so standard after all.....
                        call agnc_get_varid_by_name(ncid, 'sea_surface_height_above_sea_level', varid)
                        if ( varid < 0 ) then
                            errmsg = 'Could not find waterlevel variable called ssh, sea_surface_height or sea_surface_height_above_sea_level'
                        end if
                    else
                        errmsg = "Could not find a variable named " // trim(vname) // " or standard_name " // trim(trecord%standard_name)
                    end if
                end if
            else
                errmsg = "Could not find a variable named " // vname
            end if
        end if
        if ( errmsg /= ' ' ) then
            call MSGERR(4, errmsg)
            return
        end if

        if ( present(ndims) ) call nccheck( nf90_inquire_variable(ncid, varid, ndims=ndims) )

    end subroutine swn_hre_grid_varid


    subroutine swn_hre_bcnccf  (FBCNAM, BCTYPE, BSPFIL,       &
                                XCGRID, YCGRID, KGRPNT,       &
                                XYTST,  KGRBND, DONALL)
        use M_BNDSPEC
        use SWCOMM3, only : xpc, ypc

!       data concerning boundary files are stored in array BFILED
!       there is a subarray for each file; it contains:
!       1.  status; 0: stationary, 1: nonstat, -1: exhausted
!       2.  time of boundary values read one before last
!       3.  time of boundary values read last
!       4.  NDSL: unit ref. num. of file containing filenames
!       5.  NDSD: unit ref. num. of file containing data
!       6.  time coding option for reading time from data file
!       8.  number of locations for which spectra are in the file
!       9.  order of reading directional information
!       10. number of spectral directions of spectra on file
!       11. exception value of spectra.
!       12. number of spectral frequencies
!       13. ordering of data in file
!       14. number of heading lines per file
!       15. number of heading lines per time step
!       16. number of heading lines per spectrum
!       17. =1: energy dens., =2: variance density
!       18. =1: Cartesian direction, =2: Nautical dir.                    40.00
!       19. =1: direction spread in degr, =2: Power of Cos.               40.00
        ! arguments
        integer, intent(in   )                 :: KGRPNT(MXC,MYC), KGRBND(*), XYTST(*)
        real,    intent(in   )                 :: XCGRID(MXC,MYC), YCGRID(MXC,MYC)
        character(len=*), intent(in)           :: FBCNAM, BCTYPE
        logical, intent(in)                    :: DONALL
        type(BSPCDAT), intent(inout)           :: BSPFIL

        !local
        integer                                :: ncid, ibounc, NBOUNC, IIPT2
        integer                                :: ierr, nbgrpt_prev
        integer, save                          :: IENT=0
        type(recordaxe_type)                   :: recordaxe
        type(pntgrid_type)                     :: pntgrid
        type(spcgrid_type)                     :: spcgrid
        character                              :: dconv*20
        logical                                :: spc_as_map
        real(kind=8)                           :: xlon0, xlat0
        real(kind=4)                           :: xp2, yp2, xp, yp

        call STRACE (IENT, 'BCNCCF')

        if ( BSPFIL%BFILED(5) > -1 ) then
            !ncfile was not yet opened. fill BSPFIL

            call open_ncfile( FBCNAM, 'read', ncid, recordaxe)
            BSPFIL%BFILED(5) = -1*ncid

            call agnc_get_spcgrid(ncid, spcgrid)
            call agnc_get_pntgrid(ncid, pntgrid)

            NBOUNC        = pntgrid%npoints

            if ( spc_as_map ) then
                call MSGERR (4, 'Maps of spectra are not yet supported in the reading of boundary data')
                return
            end if

            ! fill in collected data
            !
            if ( recordaxe%nstatm ) then
                BSPFIL%BFILED(1) = 1            ! nonstationary
            else
                BSPFIL%BFILED(1) = 0            ! stationary
            end if
            BSPFIL%BFILED( 2) = -999999999
            BSPFIL%BFILED( 3) = -999999999
            BSPFIL%BFILED( 8) = NBOUNC
            CALL COPYCH ('NCCF', 'T', BSPFIL%BFILED(7), 1, IERR)
            BSPFIL%BFILED( 9) = 0               ! direction monotonously increasing
            BSPFIL%BFILED(10) = spcgrid%ndir    ! number of directions
            BSPFIL%BFILED(11) = -999            ! dummy exception value, _FillValue replaced with 0 on extraction
            BSPFIL%BFILED(12) = spcgrid%nfreq   ! number of frequencies
            BSPFIL%BFILED(15) = 1               ! artificial one line header to read time in RBFILE
            BSPFIL%BFILED(17) = 2               ! file contains variance density

            if ( BSPFIL%BFILED(10) > 1 ) then
                ! 2D spectral directions are converted to cartesian in this routine
                BSPFIL%BFILED(18) = 1           ! cartesian convention
            else
                ! in 1D, maintain nautical convention, will be converted while reading spectra
                BSPFIL%BFILED(18) = 2           ! nautical convention
            end if
            BSPFIL%BFILED(19) = 1               ! direction spread in degrees

            ALLOCATE(BSPFIL%BSPFRQ(spcgrid%nfreq))
            BSPFIL%BSPFRQ = spcgrid%frequency * PI2

            ALLOCATE(BSPFIL%BSPDIR(BSPFIL%BFILED(10)))
            BSPFIL%BSPDIR = 0

            if ( BSPFIL%BFILED(10) > 1 ) then
                call nccheck ( get_direction_convention(ncid, dconv) )
                if ( dconv == 'nautical') then
                    ! meteorological to cartesian in one go
                    BSPFIL%BSPDIR = PI + DNORTH*PI/180 - spcgrid%direction
                else
                    BSPFIL%BSPDIR = spcgrid%direction
                end if
            end if
        end if
        if (BCTYPE /= 'PNTS') then
            ! only find interpolation coefficients if this is a nesting file
            do ibounc = 1, NBOUNC
                !
                !       in case of nesting coordinates on file are used to determine interpolation
                !       coefficients
                !
                if (KSPHER == 0 .and. pntgrid%lunit /= 'meter') then
                    !
                    ! if SWAN uses Cartesian coordinates, then transform the spherical coordinates
                    ! of the boundary point to local Cartesian coordinates
                    !
                    if (ibounc == 1) then
                        xlon0 = (XOFFS - XPC)/LENDEG
                        xlat0 = (YOFFS - YPC)/LENDEG
                    end if

                    xp = XPC + LENDEG * cos( PI * xlat0/180.) * (pntgrid%longitude(ibounc) - xlon0)
                    yp = YPC + LENDEG * (pntgrid%latitude(ibounc) - XLAT0)
                else
                    xp = pntgrid%longitude(ibounc) - XOFFS
                    yp = pntgrid%latitude(ibounc)  - YOFFS
                end if
                XP2 = xp
                YP2 = yp
                !
                ! interpolate the boundaries points to the grid points of
                ! the SWAN computational grid
                !
                if (ITEST>=80) then
                    write (PRTEST, *) ' B. spectrum', ibounc, xp + XOFFS, yp + YOFFS
                end if
                nbgrpt_prev = NBGRPT
                call SWBCPT (  XCGRID, YCGRID, KGRPNT, XYTST,  KGRBND, xp2,yp2,ibounc, NBOUNC, DONALL )

                ! check if the grid points are on nested boundary.
                ! if not, stop the calculation and give an error message
                if (NBGRPT.ne.nbgrpt_prev) then
                    IIPT2 = IIPT2+1
                end if
        end do
        if (IIPT2 == 0) call MSGERR (2, 'no grid points on nested boundary')
      end if
      IF (ITEST>=60) write (PRTEST,'(I6, A)') NBOUNC, ' boundary locations'

      allocate(BSPFIL%BSPLOC(NBOUNC))
      DO ibounc = 1, NBOUNC
         BSPFIL%BSPLOC(ibounc) = NBSPEC + ibounc
      ENDDO
      NBSPEC = NBSPEC + NBOUNC

    end subroutine swn_hre_bcnccf

      function get_direction_convention(ncid, dconv) result(ierr)
          integer,          intent(in ) :: ncid
          character(len=*), intent(out) :: dconv

          integer :: ierr, i, dir_dimid
          character(len=*), parameter :: names_dir(4) = ['NDIR', 'ndir', 'CDIR', 'cdir']
          character :: firstChar

          ierr = nf90_get_att( ncid, NF90_GLOBAL, 'Directional_convention', dconv)
          if (ierr /= nf90_noerr) then
              do i = 1, size(names_dir)
                  ierr = nf90_inq_dimid(ncid, names_dir(i), dir_dimid)
                  if ( ierr == nf90_noerr ) then
                      firstChar = names_dir(i)(1:1)
                      if (firstChar == 'n' .or. firstChar == 'N') then
                          dconv = 'nautical'
                      else
                          dconv = 'cartesian'
                      endif
                      exit
                  endif
              enddo
          else
              dconv = ' '
          endif

      end function get_direction_convention

    subroutine swn_hre_timf(NDSD, COUNT_IT, TIMCO, TIMF)
        integer,                intent(in   )        :: NDSD, COUNT_IT
        real(kind=8),           intent(in   )        :: TIMCO
        real(kind=8),           intent(  out)        :: timf
        integer, save                                :: IENT=0
        integer                                      :: ri
        real(kind=4)                                 :: toffset
        type(recordaxe_type)                         :: recordaxe

        real, external :: DTTIME

        call STRACE (IENT, 'swn_hre_timf')

        IF (ITEST>=60) write(PRTEST,'(A,I7)') 'current time: ', nint(TIMCO)

        call agnc_get_recordaxe(NDSD * -1, recordaxe)
        toffset = DTTIME(datevec( recordaxe%content(recordaxe%first) ))

        if ( COUNT_IT > recordaxe%last ) then
            call MSGERR (2, 'boundary file excausted. taking last record')
            ri = recordaxe%last
        else
            ri = COUNT_IT
        end if

        timf = real(recordaxe%content(ri) - recordaxe%content(recordaxe%first), 8) - real(toffset,8)

    end subroutine swn_hre_timf

    subroutine swn_hre_get_bpnt_2d(NDSD, ri, IBSPEC, BAUX1)
        integer,                    intent(in   ) :: NDSD   !< unit number of the file from which to read the dataset
        integer,                    intent(in   ) :: ri     !< counter of the time entering in the boundary file
        integer,                    intent(in   ) :: IBSPEC !< counter for spectra
        real, dimension(:,:),       intent(inout) :: BAUX1  !< spectral data

        ! local
        real(kind=4)                              :: scale_factor
        real(kind=4), save                        :: fill_value
        integer                                   :: svarid, j, i, ncid
        character(len=*), parameter               :: enames(2) = ['density', 'VaDens ']
        integer, save                             :: evarid
        integer                                   :: ierr
        integer, parameter                        :: bndDims = 4
        integer, save                             :: dimIDs(bndDims), lenDims(bndDims)
        integer, save                             :: dimid_afreq, dimid_ndir
        character(len=16), save                   :: names(bndDims)
        integer, save                             :: ncidPrev = -999
        real(kind=4), allocatable                 :: tmp(:,:)
        integer, save                             :: IENT=0

        call STRACE (IENT, 'swn_hre_get_bpnt_2d')

        ncid  = NDSD * -1

        IF (ITEST >= 60) write(PRTEST,'(A,I7,I7)') 'extract 2D spectra from timeindex, pointindex: ', ri, IBSPEC

        if (ncidPrev /= ncid) then
            ! first time: retrieve meta data and store in 'saved' local parameters
            ncidPrev = ncid
            ierr = nf90_inquire_variable_list(ncid, evarid, enames)
            call nccheck ( ierr )
            ierr = nf90_inquire_variable(ncid, evarid, dimids = dimIDs)
            do i = 1, bndDims
                ierr = nf90_inquire_dimension(ncid, dimIDs(i), names(i), lenDims(i))
                call nccheck ( ierr )
            enddo
            ierr = nf90_inquire_dimension_list(ncid, dimid_afreq, names_afreq)
            call nccheck ( ierr )
            ierr = nf90_inquire_dimension_list(ncid, dimid_ndir, names_ndir)
            call nccheck ( ierr )

            call nccheck ( nf90_get_att(ncid, evarid, "_FillValue", fill_value), "_FillValue" )
        endif

        if ( nf90_inq_varid(ncid, scale_dens_name, svarid)  == nf90_noerr ) then
            call nccheck( nf90_get_var(ncid, svarid, scale_factor, (/IBSPEC, ri/) ) )
        else
            scale_factor = 1.
        end if

        if (dimIDs(1) == dimid_afreq .and. dimIDs(2) == dimid_ndir) then
            allocate(tmp(lenDims(1), lenDims(2)))
            call nccheck( nf90_get_var(ncid, evarid, tmp, (/1, 1, IBSPEC, ri/) ) )
            do i = 1, lenDims(1)
                do j = 1, lenDims(2)
                    BAUX1(j,i) = tmp(i,j)
                enddo
            enddo
            deallocate(tmp)
        else
            call nccheck( nf90_get_var(ncid, evarid, BAUX1, (/1, 1, IBSPEC, ri/) ) )
        endif

        do i=1, size(BAUX1,2)
            do j=1, size(BAUX1,1)
                if ( BAUX1(j, i) == fill_value ) then
                    BAUX1(j, i) = 0.
                end if
            end do
        end do

        if (scale_factor /= 1.0) then
            do i=1, size(BAUX1,2)
                do j=1, size(BAUX1,1)
                    BAUX1(j, i) = BAUX1(j, i) * scale_factor
                end do
            end do
        endif

    end subroutine swn_hre_get_bpnt_2d

    subroutine swn_hre_get_bpnt_1d(NDSD, ri, IBSPEC, IFRE, etot, theta, spr)
        integer,                    intent(in   ) :: NDSD, ri, IBSPEC, IFRE
        real,                       intent(inout) :: etot, theta, spr

        ! local
        real(kind=4)                              :: scale_energy, &
                                                     sf_theta, sf_spr, &
                                                     ao_theta, ao_spr, &
                                                     fv_theta, fv_spr, fill_value
        integer                                   :: varid_scale_energy, &
                                                     varid_energy, varid_theta, &
                                                     varid_spr, ncid
        integer, save                             :: IENT=0

        call STRACE (IENT, 'swn_hre_get_bpnt_1d')

        ncid  = NDSD * -1
        etot  = 0
        theta = 0
        spr   = 180 / PI

        ! pff, for each frequency collect the id's. Optimise later.
        call agnc_collect_spcmeta(ncid, varid_scale_energy, varid_energy,         &
                                        varid_theta, ao_theta, sf_theta, fv_theta,&
                                        varid_spr,   ao_spr,   sf_spr,   fv_spr)

        call nccheck ( nf90_get_var(ncid, varid_energy, etot,  (/IFRE, IBSPEC, ri/)))
        call nccheck ( nf90_get_att(ncid, varid_energy, "_FillValue", fill_value), "_FillValue" )

        if ( varid_scale_energy > 0 ) then
            call nccheck ( nf90_get_var(ncid, varid_scale_energy, scale_energy, (/IBSPEC, ri/)))
        else
            scale_energy = 1.
        end if

        if ( abs(etot - fill_value) > 4*epsilon(1.) .and. abs(etot) > 4*epsilon(1.) ) then
            etot = etot * scale_energy

            call nccheck ( nf90_get_var(ncid, varid_theta,  theta, (/IFRE, IBSPEC, ri/)))
            call nccheck ( nf90_get_var(ncid, varid_spr,    spr,   (/IFRE, IBSPEC, ri/)))
            if ( abs(theta - fv_theta) > 4*epsilon(1.) ) theta = theta * sf_theta + ao_theta
            if ( abs(spr - fv_spr) > 4*epsilon(1.) ) spr = spr * sf_spr + ao_spr
        else
            etot  = 0.
        end if

    end subroutine swn_hre_get_bpnt_1d

end module HRextensions
