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

submodule(fm_external_forcings) fm_external_forcings_init_old
   use fm_external_forcings_data, only: have_laterals_in_external_forcings_file
   use m_setfixedweirscheme3onlink, only: setfixedweirscheme3onlink

   implicit none

contains

   !> Initialize external forcings from an 'old' format ext file. Only to be called once as part of fm_initexternalforcings.
   module subroutine init_old(iresult)
      use m_setinitialverticalprofilesigma, only: setinitialverticalprofilesigma
      use m_setinitialverticalprofile, only: setinitialverticalprofile
      use precision, only: dp
      use m_addsorsin, only: addsorsin_from_polyline_file
      use m_add_tracer, only: add_tracer
      use m_setzcs, only: setzcs
      use m_getkbotktopmax
      use m_flowtimes, only: handle_extra, irefdate, tunit, tstart_user, tim1fld, ti_mba
      use m_flowgeom, only: lnx, ndx, xz, yz, xu, yu, iadv, ibot, ndxi, lnx1d, grounlay, jagrounlay, kcs
      use m_inquire_flowgeom, only: IFLTP_1D, IFLTP_ALL
      use m_netw, only: xk, yk, zk, numk, numl
      use unstruc_model, only: md_extfile_dir, md_inifieldfile, md_extfile
      use timespace, only: timespaceinitialfield, timespaceinitialfield_int, ncflow, loctp_polygon_file, loctp_polyline_file, selectelset_internal_links, selectelset_internal_nodes, getmeteoerror, readprovider
      use m_structures, only: jaoldstr
      use m_meteo
      use m_sediment, only: sedh, sed, mxgr, jaceneqtr, grainlay, jagrainlayerthicknessspecified
      use m_transport, only: ised1, const_names, constituents, itrac2const
      use m_mass_balance_areas, only: mbaname, nomba, mbadef, nammbalen
      use mass_balance_areas_routines, only: get_mbainputname
      use m_fm_wq_processes, only: wqbotnames, wqbot
      use dfm_error, only: dfm_noerr, dfm_extforcerror
      use m_sferic, only: jsferic
      use m_fm_icecover, only: ja_ice_area_fraction_read, ja_ice_thickness_read, fm_ice_activate_by_ext_forces
      use m_laterals, only: numlatsg, ILATTP_1D, ILATTP_2D, ILATTP_ALL, kclat, nlatnd, nnlat, n1latsg, n2latsg, initialize_lateraldata
      use unstruc_files, only: resolvepath, basename
      use m_ec_spatial_extrapolation, only: init_spatial_extrapolation
      use unstruc_inifields, only: set_friction_type_values
      use timers, only: timstop, timstrt
      use m_lateral_helper_fuctions, only: prepare_lateral_mask
      use fm_external_forcings_utils, only: get_tracername, get_sedfracname
      use fm_location_types, only: UNC_LOC_S, UNC_LOC_U, UNC_LOC_CN
      use m_qnerror
      use m_delpol
      use m_get_kbot_ktop
      use m_observations, only: nummovobs, addobservation
      use unstruc_inifields, only: initialfield2Dto3D
      use m_find_name, only: find_name
      use m_fm_wq_processes_sub, only: get_waqinputname

      integer, intent(inout) :: iresult !< integer error code, is preserved in case earlier errors occur.

      integer :: ja, method, lenqidnam, ierr, ilattype, isednum, kk, k, kb, kt, iconst
      integer :: ec_item, iwqbot, layer, ktmax, idum, mx, imba, itrac
      integer :: numg, numd, numgen, npum, numklep, numvalv, nlat
      real(kind=dp) :: maxSearchRadius
      character(len=256) :: filename, sourcemask
      character(len=256) :: varname
      character(len=NAMTRACLEN) :: tracnam, qidnam
      character(len=NAMSFLEN) :: sfnam
      character(len=20) :: wqinput
      character(len=NAMMBALEN) :: mbainputname
      real(kind=dp), allocatable :: viuh(:), tt(:)
      integer, dimension(:), pointer :: pkbot, pktop
      real(kind=dp) :: factor
      real(kind=dp), external :: ran0
      character(len=256) :: rec
      integer, allocatable :: mask(:)
      real(kind=dp), allocatable :: xdum(:), ydum(:)
      integer, allocatable :: kdum(:)

      ! Finish with all remaining old-style ExtForceFile quantities.
      if (mext == 0) then
         return
      end if

      allocate (xdum(1), ydum(1), kdum(1), stat=ierr)
      call aerr('xdum(1), ydum(1), kdum(1)', ierr, 3)
      xdum = 1d0; ydum = 1d0; kdum = 1

      call timstrt('Init ExtForceFile (old)', handle_extra(50)) ! extforcefile old
      ja = 1

      do while (ja == 1) ! read *.ext file
         call delpol() ! ook jammer dan
         maxSearchRadius = -1
         call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja, varname, sourcemask, maxSearchRadius)
         if (ja == 1) then
            call resolvePath(filename, md_extfile_dir)

            call mess(LEVEL_INFO, 'External Forcing or Initialising '''//trim(qid)//''' from file '''//trim(filename)//'''.')
            ! Initialize success to be .false.
            success = .false.

            qidnam = qid
            call get_tracername(qid, tracnam, qidnam)
            call get_sedfracname(qid, sfnam, qidnam)
            call get_waqinputname(qid, wqinput, qidnam)
            call get_mbainputname(qid, mbainputname, qidnam)

            lenqidnam = len_trim(qidnam)
            if (filetype == 7 .and. method == 4) then
               method = 5 ! upward compatible fix
            end if

            kx = 1 ! voorlopig vectormax = 1

            call init_spatial_extrapolation(maxSearchRadius, jsferic)

            if (qid == 'frictioncoefficient') then
               if (len_trim(md_inifieldfile) > 0) then
                  call mess(LEVEL_WARN, 'Friction coefficients should be defined in file '''//trim(md_inifieldfile)//'''. Quantity '//trim(qid)//' ignored in external forcing file '''//trim(md_extfile)//'''.')
                  cycle
               end if

               success = timespaceinitialfield(xu, yu, frcu, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)
               if (success) then
                  call set_friction_type_values()
               end if

            else if (qid == 'frictiontrtfactor') then

               if (jatrt /= 1) then
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting QUANTITY '//trim(qid)//', but [trachytopes] is not switched on in MDU file. Ignoring this block.')
               else
                  if (.not. allocated(cftrtfac)) then
                     allocate (cftrtfac(lnx), stat=ierr)
                     call aerr('cftrtfac(lnx)', ierr, lnx)
                     cftrtfac = 1d0
                  end if

                  success = timespaceinitialfield(xu, yu, cftrtfac, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)
                  if (success) then
                     jacftrtfac = 1
                  end if
               end if

            else if (qid == 'linearfrictioncoefficient') then

               jafrculin = 1
               success = timespaceinitialfield(xu, yu, frculin, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)

            else if (qid == 'internaltidesfrictioncoefficient') then
               if (jaFrcInternalTides2D /= 1) then ! not added yet
                  if (allocated(frcInternalTides2D)) then
                     deallocate (frcInternalTides2D)
                  end if
                  allocate (frcInternalTides2D(Ndx), stat=ierr)
                  call aerr('frcInternalTides2D(Ndx)', ierr, Ndx)
                  frcInternalTides2D = DMISS

                  if (allocated(DissInternalTidesPerArea)) then
                     deallocate (DissInternalTidesPerArea)
                  end if
                  allocate (DissInternalTidesPerArea(Ndx), stat=ierr)
                  call aerr(' DissInternalTidesPerArea(Ndx)', ierr, Ndx)
                  DissInternalTidesPerArea = 0d0

                  jaFrcInternalTides2D = 1
               end if
               success = timespaceinitialfield(xz, yz, frcInternalTides2D, Ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

            else if (qid == 'horizontaleddyviscositycoefficient') then

               if (javiusp == 0) then
                  if (allocated(viusp)) then
                     deallocate (viusp)
                  end if
                  allocate (viusp(lnx), stat=ierr)
                  call aerr('viusp(lnx)', ierr, lnx)
                  viusp = dmiss
                  javiusp = 1
               end if

               success = timespaceinitialfield(xu, yu, viusp, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)

            else if (qid == 'horizontaleddydiffusivitycoefficient') then

               if (jadiusp == 0) then
                  if (allocated(diusp)) then
                     deallocate (diusp)
                  end if
                  allocate (diusp(lnx), stat=ierr)
                  call aerr('diusp(lnx)', ierr, lnx)
                  diusp = dmiss
                  jadiusp = 1
               end if

               success = timespaceinitialfield(xu, yu, diusp, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)

            else if (qid == 'windstresscoefficient') then

               if (jaCdwusp == 0) then
                  if (allocated(Cdwusp)) then
                     deallocate (Cdwusp)
                  end if
                  allocate (Cdwusp(lnx), stat=ierr)
                  call aerr('Cdwusp(lnx)', ierr, lnx)
                  Cdwusp = dmiss
                  jaCdwusp = 1
               end if

               iCdtyp = 1 ! only 1 coeff
               success = timespaceinitialfield(xu, yu, Cdwusp, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)

            else if (qid == 'windspeedfactor') then

               if (ja_wind_speed_factor == 0) then
                  if (allocated(wind_speed_factor)) then
                     deallocate (wind_speed_factor)
                  end if
                  allocate (wind_speed_factor(lnx), stat=ierr)
                  call aerr('wind_speed_factor(lnx)', ierr, lnx)
                  wind_speed_factor(:) = dmiss
               end if

               ja_wind_speed_factor = 1
               success = timespaceinitialfield(xu, yu, wind_speed_factor, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)

            else if (qid == 'solarradiationfactor') then

               if (ja_solar_radiation_factor == 0) then
                  if (allocated(solar_radiation_factor)) then
                     deallocate (solar_radiation_factor)
                  end if
                  allocate (solar_radiation_factor(ndx), stat=ierr)
                  call aerr('solar_radiation_factor(ndx)', ierr, lnx)
                  solar_radiation_factor(:) = dmiss
               end if

               ja_solar_radiation_factor = 1
               success = timespaceinitialfield(xz, yz, solar_radiation_factor, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)

            else if (qid == 'secchidepth') then

               if (jaSecchisp == 0) then
                  if (allocated(Secchisp)) then
                     deallocate (Secchisp)
                  end if
                  allocate (Secchisp(ndx), stat=ierr)
                  call aerr('Secchisp(ndx)', ierr, lnx)
                  Secchisp = dmiss
                  jaSecchisp = 1
               end if

               success = timespaceinitialfield(xz, yz, Secchisp, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)

            else if (qid == 'advectiontype') then

               success = timespaceinitialfield_int(xu, yu, iadv, lnx, filename, filetype, operand, transformcoef)

            else if (qid == 'ibedlevtype') then ! Local override of bottomleveltype

               success = timespaceinitialfield_int(xu, yu, ibot, lnx, filename, filetype, operand, transformcoef)

            else if (qid(1:17) == 'initialwaterlevel') then
               if (len_trim(md_inifieldfile) > 0) then
                  call mess(LEVEL_WARN, 'Initial water level should be defined in file '''//trim(md_inifieldfile)//'''. Quantity '//trim(qid)//' ignored in external forcing file '''//trim(md_extfile)//'''.')
                  cycle
               end if

               if (allocated(mask)) then
                  deallocate (mask)
               end if
               allocate (mask(ndx))

               ! NOTE: we intentionally re-use the lateral coding here for selection of 1D and/or 2D flow nodes
               select case (trim(qid(18:)))
               case ('1d')
                  ilattype = ILATTP_1D
                  call prepare_lateral_mask(mask, ilattype)
               case ('2d')
                  ilattype = ILATTP_2D
                  call prepare_lateral_mask(mask, ilattype)
               case default
                  mask(:) = 1
               end select

               success = timespaceinitialfield(xz, yz, s1, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S, mask)

            else if (qid == 'initialvelocity') then ! both ucx and ucy component from map file in one QUANTITY

               if (filetype /= ncflow) then ! only from our own map files
                  success = .false.
               else
                  call realloc(uxini, lnx, fill=dmiss)
                  qid = 'initialvelocityx'
                  success = timespaceinitialfield(xu, yu, uxini, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)
                  if (success) then
                     call realloc(uyini, lnx, fill=dmiss)
                     qid = 'initialvelocityy'
                     success = timespaceinitialfield(xu, yu, uyini, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)
                     if (success) then
                        inivel = 1
                     end if
                  end if
               end if

            else if (qid == 'initialvelocityx') then

               call realloc(uxini, lnx, fill=dmiss)
               success = timespaceinitialfield(xu, yu, uxini, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)
               if (success) then
                  inivelx = 1
                  if (inively == 1) then
                     inivel = 1
                  end if
               end if

            else if (qid == 'initialvelocityy') then

               call realloc(uyini, lnx, fill=dmiss)
               success = timespaceinitialfield(xu, yu, uyini, lnx, filename, filetype, method, operand, transformcoef, UNC_LOC_U)
               if (success) then
                  inively = 1
                  if (inivelx == 1) then
                     inivel = 1
                  end if
               end if

            else if (qid == 'initialunsaturedzonethickness' .or. qid == 'interceptionlayerthickness') then ! HK-style, in module grw. See initialize_initial_fields() for the new hydrology module.

               if (.not. allocated(h_unsat)) then
                  allocate (h_unsat(ndx), stat=ierr)
                  call aerr('h_unsat(ndx)', ierr, ndx)
                  h_unsat = -999d0
               end if
               success = timespaceinitialfield(xz, yz, h_unsat, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
               where (h_unsat == -999d0) h_unsat = 0d0
               if (qid == 'interceptionlayerthickness') then
                  jaintercept2D = 1
               end if

            else if (qid == 'infiltrationcapacity') then
               if (infiltrationmodel == DFM_HYD_INFILT_CONST) then ! NOTE: old ext file: mm/day (iniFieldFile assumes mm/hr)
                  success = timespaceinitialfield(xz, yz, infiltcap, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
                  infiltcap = infiltcap * 1d-3 / (24d0 * 3600d0) ! mm/day => m/s
               else
                  write (msgbuf, '(a,i0,a)') 'flow_initexternalforcings: quantity '//trim(qid)//' requires ''InfiltrationModel = ', DFM_HYD_INFILT_CONST, ''' in MDU. Skipping file '''//trim(filename)//'''.'
                  call warn_flush()
               end if

            else if (qid == '__bathymetry__') then ! this is for the D-Flow FM User interface!!!

               success = timespaceinitialfield(xk, yk, zk, numk, filename, filetype, method, operand, transformcoef, UNC_LOC_CN)

            else if (index(qid, 'bedlevel') > 0) then ! to suppress error message while actually doing this in geominit

               success = .true.

            else if (qid(1:15) == 'initialsediment') then

               if (jased > 0) then
                  if (.not. allocated(sedh)) then
                     allocate (sedh(ndx))
                  end if
                  isednum = 1
                  if (qid(16:16) == '2') isednum = 2
                  if (qid(16:16) == '3') isednum = 3
                  if (qid(16:16) == '4') isednum = 4
                  if (qid(16:16) == '5') isednum = 5
                  if (qid(16:16) == '6') isednum = 6
                  if (qid(16:16) == '7') isednum = 7
                  if (qid(16:16) == '8') isednum = 8
                  if (qid(16:16) == '9') isednum = 9

                  sedh(1:ndx) = sed(isednum, 1:ndx)
                  success = timespaceinitialfield(xz, yz, sedh, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
                  if (success) then
                     do kk = 1, ndx
                        if (sedh(kk) /= dmiss) then
                           do k = kbot(kk), kbot(kk) + kmxn(kk) - 1
                              sed(isednum, k) = sedh(kk)
                           end do
                        end if
                     end do
                  end if
               else
                  success = .true. ! We allow to disable salinity without removing the quantity.
               end if

            else if (qid == 'initialsalinity') then

               if (jasal > 0) then
                  sah = dmiss
                  success = timespaceinitialfield(xz, yz, sah, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
                  if (success) then
                     call initialfield2Dto3D(sah, sa1, transformcoef(13), transformcoef(14), operand)
                  end if
               end if
               success = .true. ! We allow to disable salinity without removing the quantity.

            else if (qid == 'initialsalinitytop') then

               if (jasal > 0) then
                  if (.not. allocated(satop)) then
                     allocate (satop(ndx), stat=ierr)
                     call aerr('satop(ndx)', ierr, ndx)
                     satop = dmiss
                  end if
                  success = timespaceinitialfield(xz, yz, satop, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
                  if (success) then
                     inisal2D = 2; uniformsalinityabovez = transformcoef(3)
                  end if
               else
                  success = .true. ! We allow to disable salinity without removing the quantity.
               end if

            else if (qid == 'initialsalinitybot') then

               if (jasal > 0) then
                  if (.not. allocated(sabot)) then
                     allocate (sabot(ndx), stat=ierr)
                     call aerr('sabot(ndx)', ierr, ndx)
                     sabot = dmiss
                  end if
                  success = timespaceinitialfield(xz, yz, sabot, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
                  if (success .and. transformcoef(3) /= dmiss) then
                     inisal2D = 3; uniformsalinitybelowz = transformcoef(4)
                  end if
               else
                  success = .true. ! We allow to disable salinity without removing the quantity.
               end if

            else if (jatem > 0 .and. qid == 'initialtemperature') then

               success = timespaceinitialfield(xz, yz, tem1, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
               if (success) then
                  initem2D = 1
               end if

            else if (jatem > 0 .and. qid == 'initialverticaltemperatureprofile' .and. kmx > 0) then

               call setinitialverticalprofile(tem1, ndkx, filename); success = .true.

            else if (jasal > 0 .and. qid == 'initialverticalsalinityprofile' .and. kmx > 0) then

               call setinitialverticalprofile(sa1, ndkx, filename); success = .true.

            else if (janudge > 0 .and. qid == 'nudgetime') then

               success = timespaceinitialfield(xz, yz, nudge_time, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

            else if (janudge > 0 .and. qid == 'nudgerate') then

               success = timespaceinitialfield(xz, yz, nudge_rate, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

            else if (stm_included .and. qid(1:14) == 'initialsedfrac') then
               call get_sedfracname(qid, sfnam, qidnam)
               iconst = 0
               if (ISED1 > 0 .and. trim(sfnam) /= '') then
                  iconst = find_name(const_names, sfnam)
               end if
               if (iconst > 0) then
                  if (allocated(viuh)) then
                     deallocate (viuh)
                  end if
                  allocate (viuh(Ndkx))

                  !          copy existing values (if they existed) in temp array
                  !          this assumes uniform vertical distribution
                  do kk = 1, Ndx
                     viuh(kk) = constituents(iconst, kk)
                     call getkbotktop(kk, kb, kt)
                     do k = kb, kb + kmxn(kk) - 1
                        viuh(k) = constituents(iconst, k)
                     end do
                  end do

                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

                  if (success) then
                     do kk = 1, Ndx
                        if (viuh(kk) /= dmiss) then
                           sed(iconst - ISED1 + 1, kk) = viuh(kk)
                           call getkbotktop(kk, kb, kt)
                           do k = kb, kb + kmxn(kk) - 1
                              sed(iconst - ISED1 + 1, k) = sed(iconst - ISED1 + 1, kk) ! fill array with vertically uniform values
                           end do
                        end if
                     end do
                  end if
                  deallocate (viuh)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.', ' ', ' ')
               end if

            else if (stm_included .and. qid(1:29) == 'initialverticalsedfracprofile' .and. kmx > 0) then
               call get_sedfracname(qid, sfnam, qidnam)
               iconst = 0
               if (ISED1 > 0 .and. trim(sfnam) /= '') then
                  iconst = find_name(const_names, sfnam)
               end if
               if (iconst > 0) then
                  allocate (tt(1:ndkx))
                  tt = dmiss
                  call setinitialverticalprofile(tt, ndkx, filename); success = .true.
                  sed(iconst - ISED1 + 1, :) = tt
                  deallocate (tt)
               end if

            else if (stm_included .and. qid(1:34) == 'initialverticalsigmasedfracprofile' .and. kmx > 0) then
               call get_sedfracname(qid, sfnam, qidnam)
               iconst = 0
               if (ISED1 > 0 .and. trim(sfnam) /= '') then
                  iconst = find_name(const_names, sfnam)
               end if
               if (iconst > 0) then
                  allocate (tt(1:ndkx))
                  tt = dmiss
                  call setinitialverticalprofilesigma(tt, ndkx, filename); success = .true.
                  sed(iconst - ISED1 + 1, :) = tt
                  deallocate (tt)
               end if

            else if (qid(1:13) == 'initialtracer') then
               call get_tracername(qid, tracnam, qidnam)
               call add_tracer(tracnam, iconst) ! or just gets constituents number if tracer already exists
               itrac = find_name(trnames, tracnam)

               if (itrac == 0) then
                  call mess(LEVEL_ERROR, 'flow_initexternalforcings: tracer '//trim(tracnam)//' not found')
               end if
               iconst = itrac2const(itrac)

               if (allocated(viuh)) then
                  deallocate (viuh)
               end if
               allocate (viuh(Ndkx))

               ! copy existing tracer values (if they existed) in temp array
               do kk = 1, Ndx
                  call getkbotktop(kk, kb, kt)
                  viuh(kk) = constituents(iconst, kk)
                  do k = kb, kb + kmxn(kk) - 1
                     viuh(k) = constituents(iconst, k)
                  end do
               end do

               if (method == 3) then
                  kx = 1
                  pkbot => kbot
                  pktop => ktop
                  if (allocated(mask)) then
                     deallocate (mask)
                  end if
                  allocate (mask(ndx), source=1)
                  ec_item = ec_undef_int
                  call setzcs()
                  success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, &
                                                    filetype, method, operand, z=zcs, pkbot=pkbot, pktop=pktop, varname=varname, tgt_item1=ec_item)
                  success = success .and. ec_gettimespacevalue_by_itemID(ecInstancePtr, ec_item, irefdate, tzone, tunit, tstart_user, viuh)
                  if (.not. success) then
                     call mess(LEVEL_ERROR, 'flow_initexternalforcings: error reading '//trim(qid)//'from '//trim(filename))
                  end if
                  factor = merge(transformcoef(2), 1.0_hp, transformcoef(2) /= -999d0)
                  do k = 1, Ndkx
                     if (viuh(k) /= dmiss) then
                        constituents(iconst, k) = viuh(k) * factor
                     end if
                  end do
               else
                  ! will only fill 2D part of viuh
                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
                  if (success) then
                     do kk = 1, Ndx
                        if (viuh(kk) /= dmiss) then
                           constituents(iconst, kk) = viuh(kk)
                           call getkbotktop(kk, kb, kt)
                           do k = kb, kb + kmxn(kk) - 1
                              constituents(iconst, k) = constituents(iconst, kk)
                           end do
                        end if
                     end do
                  end if
               end if
               deallocate (viuh)

            else if (qid(1:13) == 'initialwaqbot') then
               iwqbot = find_name(wqbotnames, wqinput)

               if (iwqbot == 0) then
                  call mess(LEVEL_ERROR, 'flow_initexternalforcings: water quality bottom variable '//trim(wqinput)//' not found')
               end if

               if (transformcoef(3) == DMISS) then
                  layer = -1
               else
                  layer = nint(transformcoef(3))
                  if (layer > max(kmx, 1)) then
                     call mess(LEVEL_ERROR, 'Specified layer for '''//trim(qid)//''' is higher than kmx: ', layer, kmx)
                  end if
               end if

               if (allocated(viuh)) then
                  deallocate (viuh)
               end if
               allocate (viuh(Ndxi))

               ! copy existing tracer values (if they existed) in temp array
               do kk = 1, Ndxi
                  call getkbotktopmax(kk, kb, kt, ktmax)
                  if (layer < 0) then
                     ! only pick first layer above the bed
                     viuh(kk) = wqbot(iwqbot, kb)
                  else if (layer > 0) then
                     ! get current data from a specific layer in the same plane, counting from the deepest layer
                     k = ktmax - max(kmx, 1) + layer
                     if (k >= kb) then
                        ! but only when not below the bed
                        viuh(kk) = wqbot(iwqbot, k)
                     end if
                  else
                     ! can't get uniform value for all layers, so use current data from top layer
                     viuh(kk) = wqbot(iwqbot, kt)
                  end if
               end do

               ! will only fill 2D part of viuh
               success = timespaceinitialfield(xz, yz, viuh, Ndxi, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

               if (success) then
                  do kk = 1, Ndxi
                     if (viuh(kk) /= dmiss) then
                        call getkbotktopmax(kk, kb, kt, ktmax)
                        if (layer < 0) then
                           ! only set first layer above the bed
                           wqbot(iwqbot, kb) = viuh(kk)
                        else if (layer > 0) then
                           ! set a specific layer in the same plane, counting from the deepest layer
                           k = ktmax - max(kmx, 1) + layer
                           if (k >= kb) then
                              ! but only when not below the bed
                              wqbot(iwqbot, k) = viuh(kk)
                           end if
                        else
                           ! set uniform value for all layers
                           do k = kb, kt
                              wqbot(iwqbot, k) = viuh(kk)
                           end do
                        end if
                     end if
                  end do
               end if
               deallocate (viuh)

            else if (qid == 'stemdiameter') then

               if (.not. allocated(stemdiam)) then
                  allocate (stemdiam(ndx), stat=ierr)
                  call aerr('stemdiam(ndx)', ierr, ndx)
                  stemdiam = dmiss
               end if
               success = timespaceinitialfield(xz, yz, stemdiam, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

            else if (qid == 'stemdensity') then

               if (.not. allocated(stemdens)) then
                  allocate (stemdens(ndx), stat=ierr)
                  call aerr('stemdens(ndx)', ierr, ndx)
                  stemdens = dmiss
               end if
               success = timespaceinitialfield(xz, yz, stemdens, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

            else if (qid == 'stemheight') then

               if (.not. allocated(stemheight)) then
                  allocate (stemheight(ndx), stat=ierr)
                  call aerr('stemheight(ndx)', ierr, ndx)
                  stemheight = dmiss
               end if
               success = timespaceinitialfield(xz, yz, stemheight, ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

               if (stemheightstd > 0d0) then
                  do k = 1, ndx
                     if (stemheightstd /= dmiss) then
                        stemheight(k) = stemheight(k) * (1d0 + stemheightstd * (ran0(idum) - 0.5d0))
                     end if
                  end do
               end if
            else if (qid == 'groundlayerthickness') then

               success = timespaceinitialfield(xu, yu, grounlay, Lnx1D, filename, filetype, method, operand, transformcoef, UNC_LOC_U)
               if (success) jagrounlay = 1

            else if (.not. stm_included .and. qid == 'erodablelayerthicknessgrainsize1' .and. mxgr >= 1) then

               if (jaceneqtr == 1) then
                  success = timespaceinitialfield(xz, yz, grainlayerthickness(1, 1), ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
               else
                  mx = size(grainlay, 2)
                  success = timespaceinitialfield(xk, yk, grainlayerthickness(1, 1), mx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
               end if
               jagrainlayerthicknessspecified = 1

            else if (.not. stm_included .and. qid == 'erodablelayerthicknessgrainsize2' .and. mxgr >= 2) then

               if (jaceneqtr == 1) then
                  success = timespaceinitialfield(xz, yz, grainlayerthickness(1, 2), ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
               else
                  mx = size(grainlay, 2)
                  success = timespaceinitialfield(xk, yk, grainlayerthickness(1, 2), mx, filename, filetype, method, operand, transformcoef, UNC_LOC_CN)
               end if
               jagrainlayerthicknessspecified = 1

            else if (.not. stm_included .and. qid == 'erodablelayerthicknessgrainsize3' .and. mxgr >= 3) then

               if (jaceneqtr == 1) then
                  success = timespaceinitialfield(xz, yz, grainlayerthickness(1, 3), ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
               else
                  mx = size(grainlay, 2)
                  success = timespaceinitialfield(xk, yk, grainlayerthickness(1, 3), mx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)
               end if
               jagrainlayerthicknessspecified = 1

            else if (qid == 'windx' .or. qid == 'windy' .or. qid == 'windxy' .or. &
                     qid == 'stressxy' .or. qid == 'stressx' .or. qid == 'stressy') then

               call allocatewindarrays()

               if (allocated(mask)) then
                  deallocate (mask)
               end if
               allocate (mask(lnx), source=1)

               jawindstressgiven = merge(1, 0, qid(1:6) == 'stress') ! if (index(qid,'str') > 0) jawindstressgiven = 1

               if (len_trim(sourcemask) > 0) then
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), mask, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
               else
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), mask, kx, filename, filetype, method, operand, varname=varname)
               end if

               if (success) then
                  jawind = 1
               end if

            else if (qid == 'friction_coefficient_time_dependent') then
               if (allocated(mask)) then
                  deallocate (mask)
               end if
               allocate (mask(lnx), source=1)

               if (len_trim(sourcemask) > 0) then
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), mask, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
               else
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), mask, kx, filename, filetype, method, operand, varname=varname)
               end if
               if (success) then
                  ja_friction_coefficient_time_dependent = 1
                  if (ec_gettimespacevalue(ecInstancePtr, item_frcu, irefdate, tzone, tunit, tim1fld, frcu)) then
                     call set_friction_type_values()
                  end if
               else
                  ja_friction_coefficient_time_dependent = 0
               end if

            else if (qid == 'airpressure_windx_windy' .or. &
                     qid == 'airpressure_stressx_stressy' .or. &
                     qid == 'airpressure_windx_windy_charnock') then

               call allocatewindarrays()

               if (allocated(mask)) then
                  deallocate (mask)
               end if
               allocate (mask(ndx), source=1)

               jawindstressgiven = merge(1, 0, qid == 'airpressure_stressx_stressy')
               jaspacevarcharn = merge(1, 0, qid == 'airpressure_windx_windy_charnock')

               if (.not. allocated(patm)) then
                  allocate (patm(ndx), stat=ierr)
                  call aerr('patm(ndx)', ierr, ndx)
                  patm = 100000d0
               end if

               if (.not. allocated(ec_pwxwy_x)) then
                  allocate (ec_pwxwy_x(ndx), ec_pwxwy_y(ndx), stat=ierr)
                  call aerr('ec_pwxwy_x(ndx) , ec_pwxwy_y(ndx)', ierr, 2 * ndx)
                  ec_pwxwy_x = 0d0; ec_pwxwy_y = 0d0
               end if

               if (jaspacevarcharn == 1) then
                  if (.not. allocated(ec_pwxwy_c)) then
                     allocate (ec_pwxwy_c(ndx), wcharnock(lnx), stat=ierr)
                     call aerr('ec_pwxwy_c(ndx), wcharnock(lnx)', ierr, ndx + lnx)
                     ec_pwxwy_c = 0d0
                  end if
               end if

               if (len_trim(sourcemask) > 0) then
                  success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
               else
                  success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname)
               end if

               if (success) then
                  jawind = 1
                  japatm = 1
               end if

            else if (qid == 'charnock') then
               if (.not. allocated(ec_charnock)) then
                  allocate (ec_charnock(ndx), stat=ierr)
                  call aerr('ec_charnock(ndx)', ierr, ndx)
                  ec_charnock(:) = 0d0
               end if
               if (.not. allocated(wcharnock)) then
                  allocate (wcharnock(lnx), stat=ierr)
                  call aerr('wcharnock(lnx)', ierr, lnx)
               end if
               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jaspacevarcharn = 1
               end if

            else if (qid == 'humidity_airtemperature_cloudiness') then

               ! Meteo1
               kx = 3; itempforcingtyp = 1
               if (allocated(mask)) then
                  deallocate (mask)
               end if
               allocate (mask(ndx), source=1)

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname) ! vectormax=3

            else if (qid == 'dewpoint_airtemperature_cloudiness') then

               ! Meteo1
               kx = 3; itempforcingtyp = 3
               if (allocated(mask)) then
                  deallocate (mask)
               end if
               allocate (mask(ndx), source=1)

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname) ! vectormax = 3
               if (success) then
                  dewpoint_available = .true.
                  tair_available = .true.
               end if

            else if (qid == 'humidity_airtemperature_cloudiness_solarradiation') then

               ! Meteo1
               kx = 4; itempforcingtyp = 2
               if (allocated(mask)) then
                  deallocate (mask)
               end if
               allocate (mask(ndx), source=1)

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname) ! vectormax = 4
               if (success) then
                  tair_available = .true.
                  solrad_available = .true.
               end if

            else if (qid == 'dewpoint_airtemperature_cloudiness_solarradiation') then

               ! Meteo1
               kx = 4; itempforcingtyp = 4
               if (allocated(mask)) then
                  deallocate (mask)
               end if
               allocate (mask(ndx), source=1)

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname) ! vectormax = 4
               if (success) then
                  dewpoint_available = .true.
                  tair_available = .true.
                  solrad_available = .true.
               end if

            else if (qid == 'nudge_salinity_temperature') then
               kx = 2
               pkbot => kbot
               pktop => ktop

               if (allocated(mask)) then
                  deallocate (mask)
               end if
               allocate (mask(ndx), source=1)
               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, z=zcs, pkbot=pkbot, pktop=pktop, varname=varname)

               if (success) then
                  janudge = 1
               else
                  janudge = 0
               end if

            else if (qidnam == 'qhbnd') then ! specifically for QH-boundaries

               success = addtimespacerelation_boundaries(qid, filename, filetype, method, operand)

            else if (qidnam(max(1, lenqidnam - 2):lenqidnam) == 'bnd') then ! All-in-one handler for boundary qids

               success = addtimespacerelation_boundaries(qid, filename, filetype, method, operand)

            else if (qid == 'airpressure' .or. qid == 'atmosphericpressure') then

               if (.not. allocated(patm)) then
                  allocate (patm(ndx), stat=ierr)
                  call aerr('patm(ndx)', ierr, ndx)
                  patm = 0d0
               end if
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  japatm = 1
               end if

            else if (qid == 'air_temperature') then
               call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', please replace air_temperature by airtemperature')
               success = .false.

            else if (qid == 'airtemperature') then

               if (.not. allocated(tair)) then
                  allocate (tair(ndx), stat=ierr)
                  call aerr('tair(ndx)', ierr, ndx)
                  tair = 0d0
               end if
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jatair = 1
                  btempforcingtypA = .true.
                  tair_available = .true.
               end if

            else if (qid == 'airdensity') then

               if (.not. allocated(airdensity)) then
                  allocate (airdensity(ndx), stat=ierr)
                  call aerr('airdensity(ndx)', ierr, ndx)
                  airdensity = 0d0
               end if
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  call mess(LEVEL_INFO, 'Enabled variable airdensity for windstress while reading external forcings.')
                  ja_airdensity = 1
               end if

            else if (qid == 'humidity') then

               if (.not. allocated(rhum)) then
                  allocate (rhum(ndx), stat=ierr)
                  call aerr('rhum(ndx)', ierr, ndx)
                  rhum = 0d0
               end if
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jarhum = 1; btempforcingtypH = .true.
               end if

            else if (qid == 'dewpoint') then ! Relative humidity array used to store dewpoints

               if (.not. allocated(rhum)) then
                  allocate (rhum(ndx), stat=ierr)
                  call aerr('rhum(ndx)', ierr, ndx)
                  rhum = 0d0
               end if

               itempforcingtyp = 5
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jarhum = 1
                  dewpoint_available = .true.
               end if

            else if (qid == 'sea_ice_area_fraction' .or. qid == 'sea_ice_thickness') then

               ! if ice properties not yet read before, initialize ...
               if (ja_ice_area_fraction_read == 0 .and. ja_ice_thickness_read == 0) then
                  call fm_ice_activate_by_ext_forces(ndx)
               end if
               ! add the EC link
               if (len_trim(sourcemask) > 0) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
               else
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               end if
               ! update the administration
               if (success) then
                  if (qid == 'sea_ice_area_fraction') ja_ice_area_fraction_read = 1
                  if (qid == 'sea_ice_thickness') ja_ice_thickness_read = 1
               end if

            else if (qid == 'cloudiness') then

               if (.not. allocated(clou)) then
                  allocate (clou(ndx), stat=ierr)
                  call aerr('clou(ndx)', ierr, ndx)
                  clou = 0d0
               end if
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jaclou = 1; btempforcingtypC = .true.
               end if

            else if (qid == 'solarradiation') then

               if (.not. allocated(qrad)) then
                  allocate (qrad(ndx), stat=ierr)
                  call aerr('qrad(ndx)', ierr, ndx)
                  qrad = 0d0
               end if
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  btempforcingtypS = .true.
                  solrad_available = .true.
               end if

            else if (qid == 'longwaveradiation') then
               if (.not. allocated(longwave)) then
                  allocate (longwave(ndx), stat=ierr)
                  call aerr('longwave(ndx)', ierr, ndx)
                  longwave = 0d0
               end if
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  btempforcingtypL = .true.
                  longwave_available = .true.
               end if

            else if (qid(1:8) == 'rainfall') then

               if (.not. allocated(rain)) then
                  allocate (rain(ndx), stat=ierr)
                  call aerr('rain(ndx)', ierr, ndx)
                  rain = 0d0
               end if

               ! TODO: AvD: consider adding mask to all quantities.
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)

               if (success) then
                  jarain = 1
                  jaqin = 1
               end if

            else if (.not. have_laterals_in_external_forcings_file() .and. qid(1:16) == 'lateraldischarge') then

               call ini_alloc_laterals()

               select case (trim(qid(17:)))
               case ('1d')
                  ilattype = ILATTP_1D
               case ('2d')
                  ilattype = ILATTP_2D
               case ('1d2d')
                  ilattype = ILATTP_ALL
               case default
                  ilattype = ILATTP_ALL
               end select

               call prepare_lateral_mask(kclat, ilattype)

               numlatsg = numlatsg + 1
               call realloc(nnlat, max(2 * ndxi, nlatnd + ndxi), keepExisting=.true., fill=0)
               call selectelset_internal_nodes(xz, yz, kclat, ndxi, nnLat(nlatnd + 1:), nlat, &
                                               LOCTP_POLYGON_FILE, filename)
               call realloc(n1latsg, numlatsg)
               call realloc(n2latsg, numlatsg)
               n1latsg(numlatsg) = nlatnd + 1
               n2latsg(numlatsg) = nlatnd + nlat

               nlatnd = nlatnd + nlat

               jaqin = 1
               success = .true.

            else if (jaoldstr > 0 .and. qid == 'gateloweredgelevel') then

               call selectelset_internal_links(lnx, keg(ngate + 1:numl), numg, LOCTP_POLYLINE_FILE, filename)
               success = .true.
               write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numg, ' nr of gate links'; call msg_flush()

               ngatesg = ngatesg + 1
               call realloc(L1gatesg, ngatesg); L1gatesg(ngatesg) = ngate + 1
               call realloc(L2gatesg, ngatesg); L2gatesg(ngatesg) = ngate + numg

               ngate = ngate + numg

            else if (jaoldstr > 0 .and. qid == 'damlevel') then

               call selectelset_internal_links(lnx, ked(ncdam + 1:numl), numd, LOCTP_POLYLINE_FILE, filename)
               success = .true.
               write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numd, ' nr of dam level cells'; call msg_flush()

               ncdamsg = ncdamsg + 1
               call realloc(L1cdamsg, ncdamsg); L1cdamsg(ncdamsg) = ncdam + 1
               call realloc(L2cdamsg, ncdamsg); L2cdamsg(ncdamsg) = ncdam + numd

               ncdam = ncdam + numd

            else if (jaoldstr > 0 .and. qid == 'generalstructure') then

               call selectelset_internal_links(lnx, kegen(ncgen + 1:numl), numgen, LOCTP_POLYLINE_FILE, filename, sortLinks=1)
               success = .true.
               write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numgen, ' nr of general structure cells'; call msg_flush()

               ncgensg = ncgensg + 1
               call realloc(L1cgensg, ncgensg); L1cgensg(ncgensg) = ncgen + 1
               call realloc(L2cgensg, ncgensg); L2cgensg(ncgensg) = ncgen + numgen

               ncgen = ncgen + numgen

            else if (jaoldstr > 0 .and. (qid == 'pump1D' .or. qid == 'pump')) then

               if (qid == 'pump1D') then
                  call selectelset_internal_links(lnx1D, kep(npump + 1:numl), npum, LOCTP_POLYLINE_FILE, filename, linktype=IFLTP_1D, sortLinks=1)
               else
                  call selectelset_internal_links(lnx, kep(npump + 1:numl), npum, LOCTP_POLYLINE_FILE, filename, linktype=IFLTP_ALL, sortLinks=1)
               end if
               success = .true.
               write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), npum, ' nr of pump links'; call msg_flush()

               npumpsg = npumpsg + 1
               call realloc(L1pumpsg, npumpsg); L1pumpsg(npumpsg) = npump + 1
               call realloc(L2pumpsg, npumpsg); L2pumpsg(npumpsg) = npump + npum

               npump = npump + npum

            else if (jaoldstr > 0 .and. qid == 'checkvalve') then

               call selectelset_internal_links(lnx, keklep(nklep + 1:numl), numklep, LOCTP_POLYLINE_FILE, filename)
               success = .true.
               write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numklep, ' nr of checkvalves '; call msg_flush()

               nklep = nklep + numklep
               call realloc(Lklep, nklep); Lklep = keklep(1:nklep)

            else if (jaoldstr > 0 .and. qid == 'valve1D') then

               call selectelset_internal_links(lnx1D, kevalv(nvalv + 1:numl), numvalv, LOCTP_POLYLINE_FILE, filename, linktype=IFLTP_1D)
               success = .true.
               write (msgbuf, '(a,1x,a,i8,a)') trim(qid), trim(filename), numvalv, ' nr of valves '; call msg_flush()

               nvalv = nvalv + numvalv
               call realloc(Lvalv, nvalv); Lvalv = kevalv(1:nvalv); call realloc(valv, nvalv)

            else if (qid == 'discharge_salinity_temperature_sorsin') then

               ! 1. Prepare source-sink location (will increment numsrc, and prepare geometric position), based on .pli file (transformcoef(4)=AREA).
               call addsorsin_from_polyline_file(filename, area=transformcoef(4), ierr=ierr)
               if (ierr /= DFM_NOERR) then
                  success = .false.
               else
                  success = .true.
               end if

               ! 2. Time series hookup is done below, once counting of all numsrc is done.

            else if (qid == 'shiptxy') then
               kx = 2
               nshiptxy = nshiptxy + 1
               ! Converter will put 'x' in array(2*nshiptxy-1) and 'y' in array(2*nshiptxy). en welke array is dat?
               success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename, filetype, method, operand, targetIndex=nshiptxy)

            else if (qid == 'movingstationtxy') then
               kx = 2

               rec = ' '
               call basename(filename, rec) ! rec now contains the station name.
               call addObservation(dmiss, dmiss, rec, isMoving=.true.)

               ! Converter will put 'x' in array(2*nummovobs-1) and 'y' in array(2*nummovobs).
               success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename, filetype, method, operand, targetIndex=nummovobs)

            else if (qid(1:15) == 'massbalancearea' .or. qid(1:18) == 'waqmassbalancearea') then
               if (ti_mba > 0) then
                  if (.not. allocated(mbaname)) then
                     allocate (mbaname(0))
                  end if
                  imba = find_name(mbaname, mbainputname)

                  if (imba == 0) then
                     nomba = nomba + 1
                     imba = nomba
                     call realloc(mbaname, nomba, keepExisting=.true., fill=mbainputname)
                  end if
                  call realloc(viuh, Ndkx, keepExisting=.false., Fill=dmiss)

                  ! will only fill 2D part of viuh
                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, UNC_LOC_S)

                  if (success) then
                     do kk = 1, Ndxi
                        if (viuh(kk) /= dmiss) then
                           if (mbadef(kk) /= -999) then
                              ! warn that segment nn at xx, yy is nog mon area imba
                           end if
                           mbadef(kk) = imba
                           call getkbotktop(kk, kb, kt)
                           do k = kb, kb + kmxn(kk) - 1
                              mbadef(k) = imba
                           end do
                        end if
                     end do
                  end if
                  deallocate (viuh)
               else
                  call qnerror('Quantity massbalancearea in the ext-file, but no MbaInterval specified in the mdu-file.', ' ', ' ')
                  success = .false.
               end if

            else if (qid(1:12) == 'waqparameter' .or. qid(1:17) == 'waqmonitoringarea' .or. qid(1:16) == 'waqsegmentnumber') then
               ! Already taken care of in fm_wq_processes
               success = .true.

            else if (qid(1:11) == 'waqfunction') then
               success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename, filetype, method, operand)

            else if (qid(1:18) == 'waqsegmentfunction') then
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)

            else if (qid(1:25) == 'bedrock_surface_elevation') then
               kx = 1
               if (allocated(subsupl)) deallocate (subsupl, subsupl_t0, subsupl_tp, subsout, sdu_blp)

               select case (ibedlevtyp)
               case (1)
                  allocate (subsupl(ndx), stat=ierr)
                  call aerr('subsupl(ndx)', ierr, ndx)
                  subsupl = 0d0
                  allocate (subsupl_t0(ndx), stat=ierr)
                  call aerr('subsupl_t0(ndx)', ierr, ndx)
                  subsupl_t0 = 0d0
                  allocate (subsupl_tp(ndx), stat=ierr)
                  call aerr('subsupl_tp(ndx)', ierr, ndx)
                  subsupl_tp = 0d0
                  allocate (subsout(ndx), stat=ierr)
                  call aerr('subsout(ndx)', ierr, ndx)
                  subsout = 0d0
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand)

               case (2)
                  if (allocated(mask)) then
                     deallocate (mask)
                  end if
                  allocate (mask(lnx), source=1, stat=ierr)
                  call aerr('mask(lnx)', ierr, lnx)
                  allocate (subsupl(lnx), stat=ierr)
                  call aerr('subsupl(lnx)', ierr, lnx)
                  subsupl = 0d0
                  allocate (subsupl_t0(lnx), stat=ierr)
                  call aerr('subsupl_t0(lnx)', ierr, lnx)
                  subsupl_t0 = 0d0
                  allocate (subsupl_tp(lnx), stat=ierr)
                  call aerr('subsupl_tp(lnx)', ierr, lnx)
                  subsupl_tp = 0d0
                  allocate (subsout(lnx), stat=ierr)
                  call aerr('subsout(lnx)', ierr, lnx)
                  subsout = 0d0
                  success = ec_addtimespacerelation(qid, xu, yu, mask, kx, filename, filetype, method, operand, varname=varname)

               case (3, 4, 5, 6)
                  if (allocated(mask)) then
                     deallocate (mask)
                  end if
                  allocate (mask(numk), source=1, stat=ierr)
                  call aerr('mask(numk)', ierr, numk)
                  allocate (subsupl(numk), stat=ierr)
                  call aerr('subsupl(numk)', ierr, numk)
                  subsupl = 0d0
                  allocate (subsupl_t0(numk), stat=ierr)
                  call aerr('subsupl_t0(numk)', ierr, numk)
                  subsupl_t0 = 0d0
                  allocate (subsupl_tp(numk), stat=ierr)
                  call aerr('subsupl_tp(numk)', ierr, numk)
                  subsupl_tp = 0d0
                  allocate (subsout(numk), stat=ierr)
                  call aerr('subsout(numk)', ierr, numk)
                  subsout = 0d0
                  success = ec_addtimespacerelation(qid, xk(1:numk), yk(1:numk), mask, kx, filename, filetype, method, operand, varname=varname)
               end select
               allocate (sdu_blp(ndx), stat=ierr)
               call aerr('sdu_blp(ndx)', ierr, ndx)
               sdu_blp = 0d0

               if (success) then
                  jasubsupl = 1
               end if

            else if (trim(qid) == "spiderweb") then
               call qnerror(' ', 'Quantity SPIDERWEB must be renamed to airpressure_windx_windy in the ext-file.', ' ')
               success = .false.
            else if (trim(qid) == "windx_windy_airpressure") then
               call qnerror(' ', 'Quantity WINDX_WINDY_AIRPRESSURE must be renamed to airpressure_windx_windy in the ext-file.', ' ')
               success = .false.
            else if (trim(qid) == "wavesignificantheight") then
               if (jawave == 6 .or. jawave == 7) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "wavesignificantheight" found but "Wavemodelnr" is not 6 or 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "wavesignificantheight" found but "Wavemodelnr" is not 6 or 7', trim(qid))
                  success = .false.
               end if
            else if (trim(qid) == "waveperiod") then
               if (jawave == 6 .or. jawave == 7) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "waveperiod" found but "Wavemodelnr" is not 6 or 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "waveperiod" found but "Wavemodelnr" is not 6 or 7', trim(qid))
                  success = .false.
               end if
            else if (trim(qid) == "wavedirection") then
               if (jawave == 7) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               end if
            else if (trim(qid) == "wavebreakerdissipation") then
               ! wave forces based on dissipation at free surface and water column
               if (jawave == 7 .and. waveforcing == 3) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               end if
            else if (trim(qid) == "whitecappingdissipation") then
               ! wave forces based on dissipation at free surface and water column
               if (jawave == 7 .and. waveforcing == 3) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               end if
            else if (trim(qid) == "xwaveforce") then
               if (jawave == 7 .and. (waveforcing == 1 .or. waveforcing == 3)) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               end if
            else if (trim(qid) == "ywaveforce") then
               if (jawave == 7 .and. (waveforcing == 1 .or. waveforcing == 3)) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               end if
            else if (trim(qid) == "totalwaveenergydissipation") then
               if (jawave == 7 .and. waveforcing == 2) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               end if
            else
               call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting unknown QUANTITY '//trim(qid))
               call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'getting unknown QUANTITY ', trim(qid))
               success = .false.
            end if

            if (.not. success) then
               rec = getmeteoerror()
               if (len_trim(rec) > 0) then
                  call mess(LEVEL_WARN, rec)
               end if
               ! We do a direct return end, so qnerror for GUI is allowed here.
               call qnerror('flow_initexternalforcings: Error while initializing quantity: ', qid, '. Check preceding log lines for details.')
               iresult = DFM_EXTFORCERROR
               call timstop(handle_extra(50)) ! extforcefile old
               return
            end if

         end if

      end do
      call timstop(handle_extra(50)) ! extforcefile old

      call init_misc(iresult)

   end subroutine init_old

   !> Initialization of all extra quantities not covered by initialize_ext_old, such as structures and laterals. Only called as part of fm_initexternalforcings
   module subroutine init_misc(iresult)
      use precision, only: dp
      use m_flowgeom, only: ln, xz, yz, iadv, ba, wu, IADV_SUBGRID_WEIR, IADV_GENERAL_STRUCTURE
      use unstruc_model, only: md_extfile_dir
      use timespace, only: uniform, spaceandtime, readprovider
      use m_structures, only: jaoldstr
      use m_meteo
      use m_transport, only: numconst
      use m_strucs, only: generalstruc, idx_crestlevel, idx_gateloweredgelevel, idx_gateopeningwidth
      use dfm_error, only: dfm_extforcerror, dfm_noerr, dfm_strerror
      use m_sobekdfm, only: nbnd1d2d
      use m_partitioninfo, only: is_ghost_node, jampi, reduce_sum
      use m_laterals, only: numlatsg, ILATTP_1D, ILATTP_2D, ILATTP_ALL, kclat, nnlat, n1latsg, n2latsg, balat, qplat, lat_ids, &
                            initialize_lateraldata, apply_transport
      use m_sobekdfm, only: init_1d2d_boundary_points
      use unstruc_files, only: resolvepath
      use m_togeneral, only: togeneral
      use unstruc_messages, only: callback_msg, loglevel_StdOut

      integer, intent(inout) :: iresult !< integer error code, is preserved in case earlier errors occur.

      integer :: ierr
      integer :: k, L, LF, KB, KBI, N, ja, method, filetype0
      integer :: k1, l1, l2
      character(len=256) :: filename, filename0
      character(len=64) :: varname
      logical :: exist
      real(kind=dp), allocatable :: hulp(:, :)
      real(kind=dp), allocatable :: widths(:)
      real(kind=dp), allocatable :: xdum(:), ydum(:)
      integer, allocatable :: kdum(:)

      allocate (xdum(1), ydum(1), kdum(1), stat=ierr)
      call aerr('xdum(1), ydum(1), kdum(1)', ierr, 3)
      xdum = 1d0; ydum = 1d0; kdum = 1

      success = .true. ! default return code

      ! If no source/sink exists, then do not write related statistics to His-file
      if (numsrc < 0) then
         jahissourcesink = 0
         call mess(LEVEL_INFO, 'Source/sink does not exist, no related info to write.')
      end if

      ! initialise water level of 1d2d boundary points
      if (nbnd1d2d > 0) then
         call init_1d2d_boundary_points()
      end if

      if (jaoldstr > 0) then
         if (allocated(kgate)) then
            deallocate (kgate)
         end if
         if (allocated(xgate)) then
            deallocate (xgate)
         end if
         if (allocated(ygate)) then
            deallocate (ygate)
         end if
         if (allocated(zgate)) then
            deallocate (zgate)
         end if

         allocate (xgate(ngatesg), ygate(ngatesg), zgate(ngatesg), xy2gate(2, ngatesg), kgate(3, ngate), kdg(ngatesg), stat=ierr)
         call aerr('xgate(ngatesg), ygate(ngatesg), zgate(ngatesg), xy2gate(2,ngatesg), kgate(3,ngate), kdg(ngatesg)', ierr, ngate * 10)
         kgate = 0d0; zgate = 1d10; kdg = 1

         if (allocated(gate_ids)) then
            deallocate (gate_ids)
         end if
         allocate (gate_ids(ngatesg))

         do n = 1, ngatesg
            do k = L1gatesg(n), L2gatesg(n)
               Lf = abs(keg(k))
               kb = ln(1, Lf)
               kbi = ln(2, Lf)
               kgate(1, k) = kb
               kgate(2, k) = kbi
               kgate(3, k) = Lf

               xgate(n) = xz(kb)
               ygate(n) = yz(kb)
               xy2gate(1, n) = xz(kbi)
               xy2gate(2, n) = yz(kbi)

               if (kmx <= 1) then
                  iadv(Lf) = 0
                  call setfixedweirscheme3onlink(Lf)
               end if
            end do
         end do

         ja = 1
         if (mext /= 0) then
            rewind (mext)
         end if
         kx = 1
         ngatesg = 0
         do while (ja == 1) ! for gates again postponed read *.ext file
            call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja, varname)
            if (ja == 1 .and. qid == 'gateloweredgelevel') then
               call resolvePath(filename, md_extfile_dir)
               ngatesg = ngatesg + 1
               ! Prepare time series relation, if the .pli file has an associated .tim file.
               L = index(filename, '.', back=.true.) - 1
               filename0 = filename(1:L)//'_0001.tim'
               gate_ids(ngatesg) = filename(1:L)
               inquire (file=trim(filename0), exist=exist)
               if (exist) then
                  filetype0 = uniform ! uniform=single time series vectormax = 1
                  success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename0, filetype0, method=spaceandtime, operand='O', targetIndex=ngatesg)
               else
                  write (msgbuf, '(a,a,a)') 'No .tim-series file found for quantity gateloweredgelevel and file ''', trim(filename), '''. Keeping fixed (open) gate level.'
                  call warn_flush()
                  success = .true.
               end if
            end if
         end do
      end if

      if (jaoldstr > 0 .and. ncdamsg > 0) then
         if (allocated(xcdam)) then
            deallocate (xcdam)
         end if
         if (allocated(ycdam)) then
            deallocate (ycdam)
         end if
         if (allocated(zcdam)) then
            deallocate (zcdam)
         end if
         if (allocated(kcdam)) then
            deallocate (kcdam)
         end if
         allocate (xcdam(ncdamsg), ycdam(ncdamsg), zcdam(ncdamsg), xy2cdam(2, ncdamsg), kcdam(3, ncdam), kdd(ncdamsg), stat=ierr)
         call aerr('xcdam(ncdamsg), ycdam(ncdamsg), zcdam(ncdamsg), xy2cdam(2,ncdamsg), kcdam(3,ncdam), kdd(ncdamsg)', ierr, ncdam * 10)
         kcdam = 0d0; zcdam = 1d10; kdd = 1

         if (allocated(cdam_ids)) then
            deallocate (cdam_ids)
         end if
         allocate (cdam_ids(ncdamsg))

         do n = 1, ncdamsg
            do k = L1cdamsg(n), L2cdamsg(n)
               Lf = abs(ked(k))
               kb = ln(1, Lf)
               kbi = ln(2, Lf)
               kcdam(1, k) = kb
               kcdam(2, k) = kbi
               kcdam(3, k) = Lf

               xcdam(n) = xz(kb)
               ycdam(n) = yz(kb)
               xy2cdam(1, n) = xz(kbi)
               xy2cdam(2, n) = yz(kbi)

               iadv(Lf) = IADV_SUBGRID_WEIR
               call setfixedweirscheme3onlink(Lf)
            end do
         end do

         ja = 1; rewind (mext); kx = 1
         ncdamsg = 0
         do while (ja == 1) ! for cdams again postponed read *.ext file
            call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja, varname)
            if (ja == 1 .and. qid == 'damlevel') then
               call resolvePath(filename, md_extfile_dir)
               ncdamsg = ncdamsg + 1
               ! Prepare time series relation, if the .pli file has an associated .tim file.
               L = index(filename, '.', back=.true.) - 1
               filename0 = filename(1:L)//'_0001.tim'
               cdam_ids(ncdamsg) = filename(1:L)
               inquire (file=trim(filename0), exist=exist)
               if (exist) then
                  filetype0 = uniform ! uniform=single time series vectormax = 1
                  success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename0, filetype0, method=spaceandtime, operand='O', targetIndex=ncdamsg)
               else
                  write (msgbuf, '(a,a,a)') 'No .tim-series file found for quantity damlevel and file ''', trim(filename), '''. Keeping fixed (closed) dam level.'
                  call warn_flush()
                  success = .true.
               end if
            end if
         end do
      end if

      if (nvalv > 0) then
         ja = 1; rewind (mext); kx = 1; nvalv = 0

         do while (ja == 1) ! for cdams again postponed read *.ext file
            call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja, varname)
            if (ja == 1 .and. qid(1:7) == 'valve1D') then
               call resolvePath(filename, md_extfile_dir)
               nvalv = nvalv + 1

               L = index(filename, '.', back=.true.) - 1
               success = adduniformtimerelation_objects(qid, filename, 'valve1D', filename(1:L), 'flow', '', nvalv, 1, valv)
            end if
         end do
      end if

      ! Allow laterals from old ext, even when new extfile is present (but only when *no* [Lateral]s were in new extforce file).
      if (.not. have_laterals_in_external_forcings_file() .and. numlatsg > 0) then
         call realloc(balat, numlatsg, keepExisting=.false., fill=0d0)
         call realloc(qplat, [max(1, kmx), numlatsg], keepExisting=.false., fill=0d0)
         call realloc(apply_transport, numlatsg, fill=0)
         call realloc(lat_ids, numlatsg, keepExisting=.false., fill='')

         do n = 1, numlatsg
            balat(n) = 0d0
            do k1 = n1latsg(n), n2latsg(n)
               k = nnlat(k1)
               if (k > 0) then
                  if (.not. is_ghost_node(k)) then
                     balat(n) = balat(n) + ba(k)
                  end if
               end if
            end do
         end do

         if (jampi == 1) then
            call reduce_sum(numlatsg, balat)
         end if
         ja = 1; rewind (mext); kx = 1; numlatsg = 0

         do while (ja == 1) ! for cdams again postponed read *.ext file
            call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja, varname)
            if (ja == 1 .and. qid(1:16) == 'lateraldischarge') then
               call resolvePath(filename, md_extfile_dir)
               numlatsg = numlatsg + 1

               L = index(filename, '.', back=.true.) - 1
               success = adduniformtimerelation_objects('lateral_discharge', filename, 'lateral', filename(1:L), 'discharge', '', numlatsg, kx, qplat(1, :))
               if (success) then
                  ! assign id derived from pol file
                  lat_ids(numlatsg) = filename(1:L)
               end if
            end if
         end do
         if (allocated(kclat)) then
            deallocate (kclat)
         end if
      end if

      if (jaoldstr > 0 .and. ncgensg > 0) then
         if (allocated(xcgen)) deallocate (xcgen, ycgen, zcgen)
         if (allocated(kcgen)) then
            deallocate (kcgen)
         end if
         kx = 3
         allocate (xcgen(ncgensg), ycgen(ncgensg), zcgen(ncgensg * kx), xy2cgen(2, ncgensg), kcgen(4, ncgen), kdgen(ncgensg), stat=ierr)
         call aerr('xcgen(ncgensg), ycgen(ncgensg), zcgen(ncgensg*kx), xy2cgen(2,ncgensg), kcgen(4,ncgen), kdgen(ncgensg)', ierr, ncgen * 10)
         kcgen = 0d0; zcgen = 1d10; kdgen = 1

         if (allocated(fusav)) then
            deallocate (fusav)
         end if
         if (allocated(rusav)) then
            deallocate (rusav)
         end if
         if (allocated(ausav)) then
            deallocate (ausav)
         end if
         allocate (Fusav(3, ncgen), Rusav(3, ncgen), Ausav(3, ncgen), stat=ierr); Fusav = 0d0; Rusav = 0d0; ausav = 0d0

         if (allocated(cgen_ids)) then
            deallocate (cgen_ids)
         end if
         allocate (cgen_ids(ncgensg))

         ! Temp array width wu(L) values for all links under a single general structure
         allocate (widths(ncgen + 1)) ! +1: L1cgensg <=ncgen+1

         do n = 1, ncgensg
            ! Here allocate the structure ids for generalstructuyre
            do k = L1cgensg(n), L2cgensg(n)
               Lf = abs(kegen(k))
               widths(k) = wu(Lf)
               kb = ln(1, Lf)
               kbi = ln(2, Lf)
               if (kegen(k) > 0) then
                  kcgen(1, k) = kb
                  kcgen(2, k) = kbi
               else
                  kcgen(1, k) = kbi ! Store point left of the structure in kcgen(1,*) (in this case opposite to flow link, so kcgen(1,k)==ln(2,Lf)
                  kcgen(2, k) = kb
               end if
               kcgen(3, k) = Lf
               kcgen(4, k) = n ! pointer to general structure signal nr n

               xcgen(n) = xz(kb)
               ycgen(n) = yz(kb)
               xy2cgen(1, n) = xz(kbi)
               xy2cgen(2, n) = yz(kbi)

               iadv(Lf) = IADV_GENERAL_STRUCTURE ! iadv = general
               call setfixedweirscheme3onlink(Lf)
            end do
         end do

         allocate (hulp(26, ncgensg)); hulp = dmiss

         ja = 1
         rewind (mext)
         kx = 3
         ncgensg = 0
         do while (ja == 1) ! for cgens again postponed read *.ext file
            call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja, varname)
            if (ja == 1 .and. qid == 'generalstructure') then
               call resolvePath(filename, md_extfile_dir)
               ncgensg = ncgensg + 1
               ! Prepare time series relation, if the .pli file has an associated .tim file.
               L = index(filename, '.', back=.true.) - 1
               L1 = index(filename, '\', back=.true.)
               L2 = index(filename, '/', back=.true.)
               L1 = max(L1, L2) + 1
               filename0 = filename(1:L)//'_0001.tim'
               cgen_ids(ncgensg) = filename(L1:L)
               inquire (file=trim(filename0), exist=exist)
               if (exist) then
                  filetype0 = uniform ! uniform=single time series vectormax = kx = 3
                  success = ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename0, filetype0, method=spaceandtime, operand='O', targetIndex=ncgensg)
               else
                  write (msgbuf, '(a,a,a)') 'No .tim-series file found for quantity generalstructure and file ''', trim(filename), '''. Keeping fixed (closed) general structure.'
                  call warn_flush()
                  success = .true.
               end if
               hulp(:, ncgensg) = transformcoef(:)
            end if
         end do

         if (allocated(generalstruc)) then
            deallocate (generalstruc)
         end if
         allocate (generalstruc(ncgensg))
         if (allocated(cgen_type)) then
            deallocate (cgen_type)
         end if
         allocate (cgen_type(ncgensg))
         cgen_type(1:ncgensg) = ICGENTP_GENSTRU ! We only have true fully parameterized general structures from old ext file

         do n = 1, ncgensg
            ! Set some zcgen values to their initial scalar values (for example, zcgen((n-1)*3+1) is quickly need for updating bobs.)
            zcgen((n - 1) * 3 + 1) = hulp(idx_crestlevel, n) ! CrestLevel
            zcgen((n - 1) * 3 + 2) = hulp(idx_gateloweredgelevel, n) ! GateLowerEdgeLevel
            zcgen((n - 1) * 3 + 3) = hulp(idx_gateopeningwidth, n) ! GateOpeningWidth

            call togeneral(n, hulp(:, n), L2cgensg(n) - L1cgensg(n) + 1, widths(L1cgensg(n):L2cgensg(n))) ! orgcode
         end do
         deallocate (hulp)
         deallocate (widths)
      end if

      if (jaoldstr > 0 .and. npump > 0) then
         if (allocated(xpump)) then
            deallocate (xpump)
         end if
         if (allocated(ypump)) then
            deallocate (ypump)
         end if
         if (allocated(qpump)) then
            deallocate (qpump)
         end if
         if (allocated(kpump)) then
            deallocate (kpump)
         end if
         if (allocated(pumponoff)) then
            deallocate (pumponoff)
         end if
         allocate (xpump(npumpsg), ypump(npumpsg), qpump(npumpsg), xy2pump(2, npumpsg), kpump(3, npump), kdp(npumpsg), stat=ierr)
         call aerr('xpump(npumpsg), ypump(npumpsg), qpump(npumpsg), xy2pump(2,npumpsg), kpump(3,npump), kdp(npumpsg)', ierr, npump * 10)
         kpump = 0; qpump = 0d0; kdp = 1

         if (allocated(pump_ids)) then
            deallocate (pump_ids)
         end if
         allocate (pump_ids(npumpsg)) ! TODO: names are not stored here yet (they are in init_structure_control, but not for old ext file)
         allocate (pumponoff(5, npumpsg)); pumponoff = dmiss

         do n = 1, npumpsg
            pumponoff(5, n) = 0
            do k = L1pumpsg(n), L2pumpsg(n)
               L = kep(k)
               Lf = abs(L)
               if (L > 0) then
                  kb = ln(1, Lf)
                  kbi = ln(2, Lf)
               else
                  kb = ln(2, Lf)
                  kbi = ln(1, Lf)
               end if
               kpump(1, k) = kb
               kpump(2, k) = kbi
               kpump(3, k) = L ! f

               xpump(n) = xz(kb)
               ypump(n) = yz(kb)
               xy2pump(1, n) = xz(kbi)
               xy2pump(2, n) = yz(kbi)
            end do
         end do

         ja = 1; rewind (mext); kx = 1
         npumpsg = 0
         do while (ja == 1) ! for pumps again postponed read *.ext file
            call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja, varname)
            if (ja == 1 .and. (qid == 'pump1D' .or. qid == 'pump')) then
               call resolvePath(filename, md_extfile_dir)
               qid = 'pump'
               npumpsg = npumpsg + 1
               success = ec_addtimespacerelation(qid, xpump, ypump, kdp, kx, filename, filetype, method, operand, xy2pump, targetIndex=npumpsg)
               if (transformcoef(4) /= dmiss) pumponoff(1, npumpsg) = transformcoef(4)
               if (transformcoef(5) /= dmiss) pumponoff(2, npumpsg) = transformcoef(5)
               if (transformcoef(6) /= dmiss) pumponoff(3, npumpsg) = transformcoef(6)
               if (transformcoef(7) /= dmiss) pumponoff(4, npumpsg) = transformcoef(7)
            end if
         end do
      end if

      if (numsrc > 0) then
         ja = 1
         rewind (mext)
         kx = numconst + 1
         ! TODO: UNST-537/UNST-190: we now support timeseries, the constant values should come from new format ext file, not from transformcoef
         numsrc = 0
         success = .true.
         do while (ja == 1) ! for sorsin again read *.ext file
            call readprovider(mext, qid, filename, filetype, method, operand, transformcoef, ja, varname)
            if (ja == 1 .and. qid == 'discharge_salinity_temperature_sorsin') then
               call resolvePath(filename, md_extfile_dir)
               numsrc = numsrc + 1
               ! 2. Prepare time series relation, if the .pli file has an associated .tim file.
               L = index(filename, '.', back=.true.) - 1
               filename0 = filename(1:L)//'.tim'
               inquire (file=trim(filename0), exist=exist)
               if (exist) then
                  filetype0 = uniform ! uniform=single time series vectormax = ..
                  method = min(1, method) ! only method 0 and 1 are allowed, methods > 1 are set to 1 (no spatial interpolation possible here).
                  ! Converter will put 'qsrc, sasrc and tmsrc' values in array qstss on positions: (3*numsrc-2), (3*numsrc-1), and (3*numsrc), respectively.
                  call clearECMessage()
                  if (.not. ec_addtimespacerelation(qid, xdum, ydum, kdum, kx, filename0, filetype0, method, operand='O', targetIndex=numsrc)) then
                     msgbuf = 'Connecting time series file '''//trim(filename0)//''' and polyline file '''//trim(filename) &
                              //'''. for source/sinks failed:'//dumpECMessageStack(LEVEL_WARN, callback_msg)
                     call warn_flush()
                     success = .false.
                  end if
               else
                  write (msgbuf, '(a,a,a)') 'No .tim-series file found for source/sinks ''', trim(filename), '''. It maybe provided by a coupler if it exists.'
                  call warn_flush()
                  success = .true.
               end if
            end if
         end do
         if (.not. success) then
            msgbuf = 'One or more source/sinks entries resulted in a fatal error.'
            call warn_flush()
            iresult = DFM_EXTFORCERROR
            return
         end if
      end if

      if (loglevel_StdOut == LEVEL_DEBUG) then
         call ecInstancePrintState(ecInstancePtr, callback_msg, LEVEL_DEBUG)
      end if

      if (.not. success) then
         iresult = DFM_EXTFORCERROR
         return
      end if

      if (iresult /= DFM_NOERR) then
         call mess(LEVEL_WARN, 'Error during initialisation of External Forcings. See message:')
         call dfm_strerror(msgbuf, iresult)
         call warn_flush()
      end if

   end subroutine init_misc

end submodule fm_external_forcings_init_old
