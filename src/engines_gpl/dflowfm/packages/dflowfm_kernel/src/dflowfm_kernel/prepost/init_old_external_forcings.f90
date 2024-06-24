submodule (m_external_forcings) old_initialisation
   use fm_external_forcings_data
   
implicit none

   contains
   
   !> Initialize external forcings from an 'old' format ext file. Only to be called once as part of fm_initexternalforcings.
   module subroutine init_old(iresult)
   
   use m_flowtimes, only: handle_extra, irefdate, tunit, tstart_user, tim1fld, ti_mba
   use m_flowgeom, only: lnx, ndx, xz, yz, xu, yu, iadv, ibot, ndxi, lnx1d, grounlay, jagrounlay, kcs, ln
   use m_inquire_flowgeom, only: IFLTP_1D, IFLTP_ALL
   use m_netw, only: xk, yk, zk, numk, numl
   use unstruc_model, only: md_extfile_dir, md_inifieldfile, md_extfile
   use timespace, only: timespaceinitialfield, timespaceinitialfield_int, ncflow, loctp_polygon_file, loctp_polyline_file, selectelset_internal_links, selectelset_internal_nodes, getmeteoerror, readprovider
   use m_structures, only: jaoldstr, network
   use m_meteo
   use m_sediment, only: sedh, sed, mxgr, jaceneqtr, grainlay, jagrainlayerthicknessspecified
   use m_transport, only: ised1, numconst, const_names, constituents, itrac2const
   use m_mass_balance_areas, only: mbaname, nomba, mbadef, nammbalen
   use mass_balance_areas_routines, only: get_mbainputname
   use m_fm_wq_processes, only: numwqbots, wqbotnames, wqbot
   use dfm_error, only: dfm_noerr, dfm_extforcerror
   use m_sferic, only: jsferic
   use m_fm_icecover, only: ja_ice_area_fraction_read, ja_ice_thickness_read, fm_ice_activate_by_ext_forces
   use m_lateral, only : numlatsg, ILATTP_1D, ILATTP_2D, ILATTP_ALL, kclat, nlatnd, nnlat, n1latsg, n2latsg, balat, qplat, lat_ids, initialize_lateraldata, apply_transport
   use unstruc_files, only: basename, resolvepath
   use m_ec_spatial_extrapolation, only: init_spatial_extrapolation
   use unstruc_inifields, only: set_friction_type_values
   use timers, only: timstop, timstrt
   use m_lateral_helper_fuctions, only: prepare_lateral_mask
   
   integer, intent(out) :: iresult
   integer :: ja, method, lenqidnam, ierr, ilattype, isednum, kk, k, kb, kt, iconst
   integer :: ec_item, iwqbot, layer, ktmax, idum, mx, imba, itrac
   integer                       :: numz, numu, numq, numg, numd, numgen, npum, numklep, numvalv, nlat, jaifrcutp
   double precision               :: maxSearchRadius
   character(len=256)             :: filename, sourcemask
   character (len=64)             :: varname
   character (len=NAMTRACLEN)     :: tracnam, qidnam
   character (len=NAMSFLEN)       :: sfnam, qidsfnam
   character(len=20)              :: wqinput
   character(len=NAMMBALEN)       :: mbainputname
   integer, external              :: findname
   double precision, allocatable  :: viuh(:), tt(:)
   integer, dimension(:), pointer :: pkbot, pktop
   double precision               :: factor
   double precision,  external   :: ran0
   character (len=256)           :: rec
   integer, allocatable          :: mask(:)
   
   ! Finish with all remaining old-style ExtForceFile quantities.
   if (mext == 0) then
      return
   endif
   
      call timstrt('Init ExtForceFile (old)', handle_extra(50)) ! extforcefile old
      ja = 1

      do while (ja .eq. 1)                                ! read *.ext file
         call delpol()                                    ! ook jammer dan
         maxSearchRadius = -1
         call readprovider(mext,qid,filename,filetype,method,operand,transformcoef,ja,varname,sourcemask,maxSearchRadius)
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
               method = 5                                   ! upward compatible fix
            endif

            kx  = 1                                      ! voorlopig vectormax = 1

            call init_spatial_extrapolation(maxSearchRadius, jsferic)

            if (qid == 'frictioncoefficient') then
               if (len_trim(md_inifieldfile) > 0) then
                  call mess(LEVEL_WARN, 'Friction coefficients should be defined in file '''//trim(md_inifieldfile)//'''. Quantity '//trim(qid)//' ignored in external forcing file '''//trim(md_extfile)//'''.')
                  cycle
               end if

               success = timespaceinitialfield(xu, yu, frcu, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module
               if (success) then
                  call set_friction_type_values()
               end if

            else if (qid == 'frictiontrtfactor') then

               if (jatrt /= 1) then
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting QUANTITY '//trim(qid)//', but [trachytopes] is not switched on in MDU file. Ignoring this block.')
               else
                  if (.not. allocated(cftrtfac) ) then
                     allocate ( cftrtfac(lnx), stat=ierr)
                     call aerr('cftrtfac(lnx)', ierr, lnx)
                     cftrtfac = 1d0
                  endif

                  success = timespaceinitialfield(xu, yu, cftrtfac, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module
                  if (success) then
                     jacftrtfac = 1
                  endif
               end if

            else if (qid == 'linearfrictioncoefficient') then

               jafrculin = 1
               success = timespaceinitialfield(xu, yu, frculin, lnx, filename, filetype, method, operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'internaltidesfrictioncoefficient') then
               if ( jaFrcInternalTides2D.ne.1 ) then   ! not added yet
                  if ( allocated(frcInternalTides2D) ) deallocate(frcInternalTides2D)
                  allocate(  frcInternalTides2D(Ndx), stat=ierr)
                  call aerr('frcInternalTides2D(Ndx)', ierr, Ndx)
                  frcInternalTides2D = DMISS

                  if ( allocated( DissInternalTidesPerArea) ) deallocate( DissInternalTidesPerArea)
                  allocate(   DissInternalTidesPerArea(Ndx), stat=ierr)
                  call aerr(' DissInternalTidesPerArea(Ndx)', ierr, Ndx)
                  DissInternalTidesPerArea = 0d0

                  jaFrcInternalTides2D = 1
               end if
               success = timespaceinitialfield(xz,yz, frcInternalTides2D, Ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

            else if (qid == 'horizontaleddyviscositycoefficient') then

               if (javiusp == 0) then
                  if (allocated (viusp) ) deallocate(viusp)
                  allocate ( viusp(lnx) , stat=ierr )
                  call aerr('viusp(lnx)', ierr, lnx )
                  viusp = dmiss
                  javiusp = 1
               endif

               success = timespaceinitialfield(xu, yu, viusp, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'horizontaleddydiffusivitycoefficient') then

               if (jadiusp == 0) then
                  if (allocated (diusp) ) deallocate(diusp)
                  allocate ( diusp(lnx) , stat=ierr )
                  call aerr('diusp(lnx)', ierr, lnx )
                  diusp = dmiss
                  jadiusp = 1
               endif

               success = timespaceinitialfield(xu, yu, diusp, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'windstresscoefficient') then

               if (jaCdwusp == 0) then
                  if (allocated (Cdwusp) ) deallocate(Cdwusp)
                  allocate ( Cdwusp(lnx) , stat=ierr )
                  call aerr('Cdwusp(lnx)', ierr, lnx )
                  Cdwusp = dmiss
                  jaCdwusp = 1
               endif

               iCdtyp  = 1 ! only 1 coeff
               success = timespaceinitialfield(xu, yu, Cdwusp, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'windspeedfactor') then

               if (ja_wind_speed_factor == 0) then
                  if (allocated (wind_speed_factor) ) deallocate(wind_speed_factor)
                  allocate ( wind_speed_factor(lnx) , stat=ierr )
                  call aerr('wind_speed_factor(lnx)', ierr, lnx )
                  wind_speed_factor(:) = dmiss
               endif

               ja_wind_speed_factor = 1
               success = timespaceinitialfield(xu, yu, wind_speed_factor, lnx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'solarradiationfactor') then

               if (ja_solar_radiation_factor == 0) then
                  if (allocated (solar_radiation_factor) ) deallocate(solar_radiation_factor)
                  allocate ( solar_radiation_factor(ndx) , stat=ierr )
                  call aerr('solar_radiation_factor(ndx)', ierr, lnx )
                  solar_radiation_factor(:) = dmiss
               endif

               ja_solar_radiation_factor = 1
               success = timespaceinitialfield(xz, yz, solar_radiation_factor, ndx, filename, filetype, method, operand, transformcoef, 1)

            else if (qid == 'secchidepth') then

               if (jaSecchisp == 0) then
                  if (allocated (Secchisp) ) deallocate(Secchisp)
                  allocate ( Secchisp(ndx) , stat=ierr )
                  call aerr('Secchisp(ndx)', ierr, lnx )
                  Secchisp = dmiss
                  jaSecchisp = 1
               endif

               success = timespaceinitialfield(xz, yz, Secchisp, ndx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module

            else if (qid == 'advectiontype') then

               success = timespaceinitialfield_int(xu, yu, iadv, lnx, filename, filetype, method, operand, transformcoef) ! zie meteo module

            else if (qid == 'ibedlevtype') then ! Local override of bottomleveltype

               success = timespaceinitialfield_int(xu, yu, ibot, lnx, filename, filetype, method, operand, transformcoef) ! zie meteo module

            else if (qid(1:17) == 'initialwaterlevel') then
               if (len_trim(md_inifieldfile) > 0) then
                  call mess(LEVEL_WARN, 'Initial water level should be defined in file '''//trim(md_inifieldfile)//'''. Quantity '//trim(qid)//' ignored in external forcing file '''//trim(md_extfile)//'''.')
                  cycle
               end if

               if (allocated (mask) ) deallocate(mask)
               allocate( mask(ndx))

               ! NOTE: we intentionally re-use the lateral coding here for selection of 1D and/or 2D flow nodes
               select case (trim(qid(18:)))
               case ('1d')
                  ilattype = ILATTP_1D
                  call prepare_lateral_mask(mask, ilattype)
               case ('2d')
                  ilattype = ILATTP_2D
                  call prepare_lateral_mask(mask, ilattype)
               case default
                  mask = 1
               end select

               success = timespaceinitialfield(xz, yz, s1, ndx, filename, filetype, method, operand, transformcoef, 2, mask) ! zie meteo module

            else if (qid == 'initialvelocity') then ! both ucx and ucy component from map file in one QUANTITY

               if (filetype /= ncflow) then ! only from our own map files
                  success = .false.
               else
                  call realloc(uxini, lnx, fill=dmiss)
                  qid = 'initialvelocityx'
                  success = timespaceinitialfield(xu, yu, uxini, lnx, filename, filetype, method, operand, transformcoef, 1) ! zie meteo module
                  if (success) then
                     call realloc(uyini, lnx, fill=dmiss)
                     qid = 'initialvelocityy'
                     success = timespaceinitialfield(xu, yu, uyini, lnx, filename, filetype, method, operand, transformcoef, 1) ! zie meteo module
                     if (success) then
                        inivel = 1
                     end if
                  end if
               end if

            else if (qid == 'initialvelocityx') then

               call realloc(uxini, lnx, fill=dmiss)
               success = timespaceinitialfield(xu, yu, uxini, lnx, filename, filetype, method, operand, transformcoef, 1) ! zie meteo module
               if (success) then
                  inivelx = 1
                  if (inively == 1) then
                     inivel = 1
                  end if
               end if

            else if (qid == 'initialvelocityy') then

               call realloc(uyini, lnx, fill=dmiss)
               success = timespaceinitialfield(xu, yu, uyini, lnx, filename, filetype, method, operand, transformcoef, 1) ! zie meteo module
               if (success) then
                  inively = 1
                  if (inivelx == 1) then
                     inivel = 1
                  end if
               end if

            else if (qid == 'initialunsaturedzonethickness' .or. qid == 'interceptionlayerthickness') then ! HK-style, in module grw. See initialize_initial_fields() for the new hydrology module.

               if (.not. allocated (h_unsat) ) then
                  allocate (h_unsat(ndx), stat=ierr)
                  call aerr('h_unsat(ndx)', ierr, ndx)
                  h_unsat = -999d0
               endif
               success = timespaceinitialfield(xz, yz, h_unsat, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               where (h_unsat == -999d0) h_unsat = 0d0
               if ( qid == 'interceptionlayerthickness' ) then
                  jaintercept2D = 1
               endif

            else if (qid == 'infiltrationcapacity') then
               if (infiltrationmodel == DFM_HYD_INFILT_CONST) then ! NOTE: old ext file: mm/day (iniFieldFile assumes mm/hr)
                  success = timespaceinitialfield(xz, yz, infiltcap, ndx, filename, filetype, method,  operand, transformcoef, 1) ! zie meteo module
                  infiltcap = infiltcap*1d-3/(24d0*3600d0)            ! mm/day => m/s
               else
                  write (msgbuf, '(a,i0,a)') 'flow_initexternalforcings: quantity ' // trim(qid) // ' requires ''InfiltrationModel = ', DFM_HYD_INFILT_CONST, ''' in MDU. Skipping file '''//trim(filename)//'''.'
                  call warn_flush()
               end if

            else if (qid == '__bathymetry__') then ! this is for the D-Flow FM User interface!!!

               success = timespaceinitialfield(xk, yk, zk, numk, filename, filetype, method, operand, transformcoef, 3) ! zie meteo module

            else if (index(qid,'bedlevel') > 0) then  ! to suppress error message while actually doing this in geominit

               success = .true.

            else if (qid(1:15) == 'initialsediment') then

                  if (jased > 0) then
                     if (.not. allocated(sedh) ) then
                        allocate(sedh(ndx))
                     endif
                     isednum = 1
                     if (qid(16:16) == '2') isednum = 2
                     if (qid(16:16) == '3') isednum = 3
                     if (qid(16:16) == '4') isednum = 4
                     if (qid(16:16) == '5') isednum = 5
                     if (qid(16:16) == '6') isednum = 6
                     if (qid(16:16) == '7') isednum = 7
                     if (qid(16:16) == '8') isednum = 8
                     if (qid(16:16) == '9') isednum = 9

                     sedh(1:ndx) = sed(isednum,1:ndx)
                     success = timespaceinitialfield(xz, yz, sedh, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
                     if (success) then
                        do kk = 1,ndx
                           if (sedh(kk) .ne. dmiss) then
                              do k = kbot(kk), kbot(kk) + kmxn(kk) - 1
                                 sed(isednum,k) = sedh(kk)
                              enddo
                           endif
                        enddo
                     endif
                  else
                     success = .true. ! We allow to disable salinity without removing the quantity.
                  end if

            else if (qid == 'initialsalinity') then

               if (jasal > 0) then
                  sah     = dmiss
                  success = timespaceinitialfield(xz, yz, sah, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
                  if (success) then
                        call initialfield2Dto3D( sah, sa1, transformcoef(13), transformcoef(14) )
                  endif
               end if
               success = .true. ! We allow to disable salinity without removing the quantity.

            else if (qid == 'initialsalinitytop') then

               if (jasal > 0) then
                  if (.not. allocated(satop) ) then
                     allocate(satop(ndx), stat=ierr)
                     call aerr('satop(ndx)', ierr, ndx)
                     satop = dmiss
                  endif
                  success = timespaceinitialfield(xz, yz, satop, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
                  if (success) then
                        inisal2D = 2 ; uniformsalinityabovez = transformcoef(3)
                  endif
               else
                  success = .true. ! We allow to disable salinity without removing the quantity.
               endif

            else if (qid == 'initialsalinitybot') then

               if (jasal > 0) then
                  if (.not. allocated(sabot) ) then
                     allocate(sabot(ndx), stat=ierr)
                     call aerr('sabot(ndx)', ierr, ndx)
                     sabot = dmiss
                  endif
                  success = timespaceinitialfield(xz, yz, sabot, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
                  if (success .and. transformcoef(3) .ne. dmiss) then
                        inisal2D = 3 ; uniformsalinitybelowz = transformcoef(4)
                  endif
               else
                  success = .true. ! We allow to disable salinity without removing the quantity.
               endif

            else if (jatem > 0 .and. qid == 'initialtemperature') then

               success = timespaceinitialfield(xz, yz, tem1, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               if (success) then
                  initem2D = 1
               endif

            else if (jatem > 0 .and. qid == 'initialverticaltemperatureprofile' .and. kmx > 0) then

               call setinitialverticalprofile(tem1, ndkx, filename) ; success = .true.

            else if (jasal > 0 .and. qid == 'initialverticalsalinityprofile' .and. kmx > 0) then

               call setinitialverticalprofile(sa1 , ndkx, filename) ; success = .true.

            else if (janudge > 0 .and. qid == 'nudgetime' ) then

               success = timespaceinitialfield(xz, yz, nudge_time, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

            else if (janudge > 0 .and. qid == 'nudgerate' ) then

               success = timespaceinitialfield(xz, yz, nudge_rate, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

            else if (stm_included .and. qid(1:14) == 'initialsedfrac') then
               call get_sedfracname(qid, sfnam, qidnam)
               iconst = 0
               if ( ISED1.gt.0 .and. trim(sfnam).ne.'') then
                  iconst = findname(NUMCONST, const_names, sfnam)
               end if
               if (iconst>0) then
                  if ( allocated(viuh) ) deallocate(viuh)
                  allocate(viuh(Ndkx))

                  !          copy existing values (if they existed) in temp array
                  !          this assumes uniform vertical distribution
                  do kk=1,Ndx
                     viuh(kk) = constituents(iconst,kk)
                     call getkbotktop(kk,kb,kt)
                     do k=kb,kb+kmxn(kk)-1
                        viuh(k) = constituents(iconst,k)
                     end do
                  end do

                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, 2)

                  if (success) then
                     do kk = 1,Ndx
                        if (viuh(kk) .ne. dmiss) then
                           sed(iconst-ISED1+1,kk) = viuh(kk)
                           call getkbotktop(kk,kb,kt)
                           do k=kb,kb+kmxn(kk)-1
                              sed(iconst-ISED1+1,k) = sed(iconst-ISED1+1,kk)     ! fill array with vertically uniform values
                           end do
                        endif
                     enddo
                  endif
                  deallocate(viuh)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', getting unknown sediment fraction '''//trim(sfnam)//''' from QUANTITY '''//trim(qid)//'''.',' ',' ')
               end if

            else if (stm_included .and. qid(1:29) == 'initialverticalsedfracprofile' .and. kmx > 0) then
               call get_sedfracname(qid, sfnam, qidnam)
               iconst = 0
               if ( ISED1.gt.0 .and. trim(sfnam).ne.'') then
                  iconst = findname(NUMCONST, const_names, sfnam)
               end if
               if (iconst>0) then
                  allocate(tt(1:ndkx))
                  tt = dmiss
                  call setinitialverticalprofile(tt, ndkx, filename) ; success = .true.
                  sed(iconst-ISED1+1,:)=tt
                  deallocate(tt)
               endif

            else if (stm_included .and. qid(1:34) == 'initialverticalsigmasedfracprofile' .and. kmx > 0) then
               call get_sedfracname(qid, sfnam, qidnam)
               iconst = 0
               if ( ISED1.gt.0 .and. trim(sfnam).ne.'') then
                  iconst = findname(NUMCONST, const_names, sfnam)
               end if
               if (iconst>0) then
                  allocate(tt(1:ndkx))
                  tt = dmiss
                  call setinitialverticalprofilesigma(tt, ndkx, filename) ; success = .true.
                  sed(iconst-ISED1+1,:)=tt
                  deallocate(tt)
               endif

            else if (qid(1:13) == 'initialtracer') then
               call get_tracername(qid, tracnam, qidnam)
               call add_tracer(tracnam, iconst)  ! or just gets constituents number if tracer already exists
               itrac = findname(numtracers, trnames, tracnam)

               if ( itrac.eq.0 ) then
                  call mess(LEVEL_ERROR, 'flow_initexternalforcings: tracer ' // trim(tracnam) // ' not found')
               end if
               iconst = itrac2const(itrac)

               if ( allocated(viuh) ) deallocate(viuh)
               allocate(viuh(Ndkx))

               ! copy existing tracer values (if they existed) in temp array
               do kk=1,Ndx
                  call getkbotktop(kk,kb,kt)
                  viuh(kk) = constituents(iconst,kk)
                  do k=kb,kb+kmxn(kk)-1
                     viuh(k) = constituents(iconst,k)
                  end do
               end do

               if (method == 3) then
                  kx = 1
                  pkbot => kbot
                  pktop => ktop
                  if (allocated (mask) ) deallocate(mask)
                  allocate( mask(ndx), source = 1 )
                  ec_item = ec_undef_int
                  call setzcs()
                  success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, &
                     filetype, method, operand, z=zcs, pkbot=pkbot, pktop=pktop, varname=varname, tgt_item1=ec_item)
                  success = success .and. ec_gettimespacevalue_by_itemID(ecInstancePtr, ec_item, irefdate, tzone, tunit, tstart_user, viuh)
                  if ( .not. success ) then
                     call mess(LEVEL_ERROR, 'flow_initexternalforcings: error reading ' // trim(qid) // 'from '// trim(filename))
                  end if
                  factor = merge(transformcoef(2), 1.0_hp, transformcoef(2) /= -999d0)
                  do k = 1, Ndkx
                     if (viuh(k) /= dmiss) then
                        constituents(iconst,k) = viuh(k) * factor
                     end if
                  end do
                  deallocate(mask)
               else
                  ! will only fill 2D part of viuh
                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, 2)
                  if (success) then
                     do kk = 1,Ndx
                        if (viuh(kk) /= dmiss) then
                           constituents(iconst,kk) = viuh(kk)
                           call getkbotktop(kk,kb,kt)
                           do k=kb,kb+kmxn(kk)-1
                              constituents(iconst,k) = constituents(iconst,kk)
                           end do
                        endif
                     enddo
                  endif
               endif
               deallocate(viuh)

            else if (qid(1:13) == 'initialwaqbot') then
               iwqbot = findname(numwqbots, wqbotnames, wqinput)

               if ( iwqbot.eq.0 ) then
                  call mess(LEVEL_ERROR, 'flow_initexternalforcings: water quality bottom variable ' // trim(wqinput) // ' not found')
               end if

               if (transformcoef(3).eq.DMISS) then
                  layer = -1
               else
                  layer = nint(transformcoef(3))
                  if (layer.gt.max(kmx,1)) then
                     call mess(LEVEL_ERROR, 'Specified layer for ''' // trim(qid) // ''' is higher than kmx: ', layer, kmx)
                  endif
               endif

               if ( allocated(viuh) ) deallocate(viuh)
               allocate(viuh(Ndxi))

               ! copy existing tracer values (if they existed) in temp array
               do kk=1,Ndxi
                  call getkbotktopmax(kk,kb,kt,ktmax)
                  if (layer.lt.0) then
                     ! only pick first layer above the bed
                     viuh(kk) = wqbot(iwqbot,kb)
                  else if (layer.gt.0) then
                     ! get current data from a specific layer in the same plane, counting from the deepest layer
                     k = ktmax - max(kmx, 1) + layer
                     if (k >= kb) then
                        ! but only when not below the bed
                        viuh(kk) = wqbot(iwqbot,k)
                     endif
                  else
                     ! can't get uniform value for all layers, so use current data from top layer
                     viuh(kk) = wqbot(iwqbot,kt)
                  endif
               end do

               ! will only fill 2D part of viuh
               success = timespaceinitialfield(xz, yz, viuh, Ndxi, filename, filetype, method, operand, transformcoef, 2)

               if (success) then
                  do kk = 1,Ndxi
                     if (viuh(kk) .ne. dmiss) then
                        call getkbotktopmax(kk,kb,kt,ktmax)
                        if (layer.lt.0) then
                           ! only set first layer above the bed
                           wqbot(iwqbot,kb) = viuh(kk)
                        else if (layer.gt.0) then
                           ! set a specific layer in the same plane, counting from the deepest layer
                           k = ktmax - max(kmx, 1) + layer
                           if (k >= kb) then
                              ! but only when not below the bed
                              wqbot(iwqbot,k) = viuh(kk)
                           endif
                        else
                           ! set uniform value for all layers
                           do k=kb,kt
                              wqbot(iwqbot,k) = viuh(kk)
                           end do
                        endif
                     endif
                  enddo
               endif
               deallocate(viuh)

            else if (qid == 'stemdiameter') then

               if (.not. allocated(stemdiam) ) then
                  allocate ( stemdiam(ndx) , stat=ierr )
                  call aerr('stemdiam(ndx)', ierr, ndx )
                  stemdiam = dmiss
               endif
               success = timespaceinitialfield(xz, yz, stemdiam, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

            else if (qid == 'stemdensity') then

               if (.not. allocated(stemdens) ) then
                  allocate ( stemdens(ndx) , stat=ierr )
                  call aerr('stemdens(ndx)', ierr, ndx )
                  stemdens = dmiss
               endif
               success = timespaceinitialfield(xz, yz, stemdens, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

            else if (qid == 'stemheight') then

               if (.not. allocated(stemheight) ) then
                  allocate ( stemheight(ndx) , stat=ierr )
                  call aerr('stemheight(ndx)', ierr, ndx )
                  stemheight = dmiss
               endif
               success = timespaceinitialfield(xz, yz, stemheight, ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module

               if (stemheightstd > 0d0) then
                  do k = 1,ndx
                     if (stemheightstd .ne. dmiss) then
                        stemheight(k) = stemheight(k)*( 1d0 + stemheightstd*( ran0(idum) - 0.5d0 ) )
                     endif
                  enddo
               endif
            else if (qid == 'groundlayerthickness') then

               success = timespaceinitialfield(xu, yu, grounlay, Lnx1D, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               if (success ) jagrounlay = 1

            else if (.not. stm_included .and. qid == 'erodablelayerthicknessgrainsize1' .and. mxgr >= 1) then

               if (jaceneqtr == 1) then
                  success = timespaceinitialfield(xz, yz, grainlayerthickness(1,1), ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               else
                  mx = size(grainlay,2)
                  success = timespaceinitialfield(xk, yk, grainlayerthickness(1,1), mx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               endif
               jagrainlayerthicknessspecified = 1

            else if (.not. stm_included .and. qid == 'erodablelayerthicknessgrainsize2' .and. mxgr >= 2) then

               if (jaceneqtr == 1) then
                  success = timespaceinitialfield(xz, yz, grainlayerthickness(1,2), ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               else
                  mx = size(grainlay,2)
                  success = timespaceinitialfield(xk, yk, grainlayerthickness(1,2), mx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               endif
               jagrainlayerthicknessspecified = 1

            else if (.not. stm_included .and. qid == 'erodablelayerthicknessgrainsize3' .and. mxgr >= 3) then

               if (jaceneqtr == 1) then
                  success = timespaceinitialfield(xz, yz, grainlayerthickness(1,3), ndx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               else
                  mx = size(grainlay,2)
                  success = timespaceinitialfield(xk, yk, grainlayerthickness(1,3), mx, filename, filetype, method, operand, transformcoef, 2) ! zie meteo module
               endif
               jagrainlayerthicknessspecified = 1

            else if (qid == 'windx' .or. qid == 'windy' .or. qid == 'windxy' .or. &
                     qid == 'stressxy' .or. qid == 'stressx' .or. qid == 'stressy') then

               call allocatewindarrays()           

               if (allocated (mask) ) deallocate(mask)
               allocate( mask(lnx), source = 1 )

               jawindstressgiven = merge(1, 0, qid(1:6) == 'stress')    ! if (index(qid,'str') > 0) jawindstressgiven = 1
       
               if (len_trim(sourcemask)>0)  then
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), mask, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
               else
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), mask, kx, filename, filetype, method, operand, varname=varname)
               endif

               if (success) then 
                  jawind = 1
               endif
           
            else if (qid == 'friction_coefficient_time_dependent') then
               if (allocated (mask) ) deallocate(mask)
               allocate( mask(lnx), source = 1 )
       
               if (len_trim(sourcemask)>0)  then
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), mask, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
               else
                  success = ec_addtimespacerelation(qid, xu(1:lnx), yu(1:lnx), mask, kx, filename, filetype, method, operand, varname=varname)
               end if
               if ( success) then
                  ja_friction_coefficient_time_dependent = 1
                  if ( ec_gettimespacevalue(ecInstancePtr, item_frcu, irefdate, tzone, tunit, tim1fld, frcu) ) then
                     call set_friction_type_values()
                  end if
               else 
                  ja_friction_coefficient_time_dependent = 0
               end if
            
            else if (qid == 'airpressure_windx_windy' .or. &
                     qid == 'airpressure_stressx_stressy' .or. &
                     qid == 'airpressure_windx_windy_charnock') then

               call allocatewindarrays()           

               if (allocated (mask) ) deallocate(mask)
               allocate( mask(ndx), source =1 )

               jawindstressgiven = merge(1, 0, qid == 'airpressure_stressx_stressy')
               jaspacevarcharn   = merge(1, 0, qid == 'airpressure_windx_windy_charnock')
              
               if (.not. allocated(patm) ) then
                  allocate ( patm(ndx) , stat=ierr)
                  call aerr('patm(ndx)', ierr, ndx)
                  patm = 100000d0
               endif
    
               if (.not. allocated(ec_pwxwy_x) ) then
                  allocate ( ec_pwxwy_x(ndx) , ec_pwxwy_y(ndx)  , stat=ierr)
                  call aerr('ec_pwxwy_x(ndx) , ec_pwxwy_y(ndx)' , ierr, 2*ndx)
                  ec_pwxwy_x = 0d0 ; ec_pwxwy_y = 0d0
               endif

               if (jaspacevarcharn == 1) then
                  if (.not. allocated(ec_pwxwy_c) ) then
                        allocate ( ec_pwxwy_c(ndx) , wcharnock(lnx), stat=ierr)
                        call aerr('ec_pwxwy_c(ndx), wcharnock(lnx)' , ierr, ndx+lnx)
                        ec_pwxwy_c = 0d0
                  endif
               endif
      
               if (len_trim(sourcemask)>0)  then
                  success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
               else
                  success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname)
               endif
   
               if (success) then
                  jawind = 1
                  japatm = 1
               endif

            else if (qid == 'charnock') then
               if (.not. allocated(ec_charnock) ) then
                  allocate ( ec_charnock(ndx)  , stat=ierr)
                  call aerr('ec_charnock(ndx)' , ierr, ndx)
                  ec_charnock(:) = 0d0
               endif
               if (.not. allocated(wcharnock) ) then
                  allocate ( wcharnock(lnx)  , stat=ierr)
                  call aerr('wcharnock(lnx)' , ierr, lnx)
               endif
               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jaspacevarcharn = 1
               endif

            else if (qid == 'humidity_airtemperature_cloudiness') then

               ! Meteo1
               kx = 3 ; itempforcingtyp = 1
               if (allocated (mask) ) deallocate(mask)
               allocate( mask(ndx) )
               mask = 1

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname) ! vectormax=3

            else if (qid == 'dewpoint_airtemperature_cloudiness') then

               ! Meteo1
               kx = 3 ; itempforcingtyp = 3
               if (allocated (mask) ) deallocate(mask)
               allocate( mask(ndx), source =1 )

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname) ! vectormax = 3
               if (success) then
                  dewpoint_available = .true.
                  tair_available = .true.
               endif

            else if (qid == 'humidity_airtemperature_cloudiness_solarradiation') then

               ! Meteo1
               kx = 4 ; itempforcingtyp = 2
               if (allocated (mask) ) deallocate(mask)
               allocate( mask(ndx), source =1 )

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname) ! vectormax = 4
               if (success) then
                  tair_available = .true.
                  solrad_available = .true.
               endif

            else if (qid == 'dewpoint_airtemperature_cloudiness_solarradiation') then

               ! Meteo1
               kx = 4 ; itempforcingtyp = 4
               if (allocated (mask) ) deallocate(mask)
               allocate( mask(ndx) )
               mask = 1

               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, varname=varname) ! vectormax = 4
               if (success) then
                  dewpoint_available = .true.
                  tair_available = .true.
                  solrad_available = .true.
               endif

            else if (qid == 'nudge_salinity_temperature') then
               kx = 2
               pkbot => kbot
               pktop => ktop

               if (allocated (mask) ) deallocate(mask)
               allocate( mask(ndx) )
               mask = 1
               success = ec_addtimespacerelation(qid, xz(1:ndx), yz(1:ndx), mask, kx, filename, filetype, method, operand, z=zcs, pkbot=pkbot, pktop=pktop, varname=varname)

               if ( success ) then
                  janudge = 1
               else
                  janudge = 0
               end if

            else if (qidnam == 'qhbnd') then ! specifically for QH-boundaries

               success = addtimespacerelation_boundaries(qid, filename, filetype, method, operand)

            else if (qidnam(max(1,lenqidnam-2):lenqidnam) == 'bnd') then ! All-in-one handler for boundary qids

               success = addtimespacerelation_boundaries(qid, filename, filetype, method, operand)

            else if (qid == 'airpressure' .or. qid == 'atmosphericpressure') then

               if (.not. allocated(patm) ) then
                  allocate ( patm(ndx) , stat=ierr)
                  call aerr('patm(ndx)', ierr, ndx)
                  patm = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  japatm = 1
               endif

            else if (qid == 'air_temperature') then
               call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', please replace air_temperature by airtemperature' )
               success = .false.

            else if (qid == 'airtemperature') then

               if (.not. allocated(tair) ) then
                  allocate ( tair(ndx) , stat=ierr)
                  call aerr('tair(ndx)', ierr, ndx)
                  tair = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jatair = 1
                  btempforcingtypA = .true.
                  tair_available = .true.
               endif

            else if (qid == 'airdensity') then

               if (.not. allocated(airdensity) ) then
                  allocate ( airdensity(ndx) , stat=ierr)
                  call aerr('airdensity(ndx)', ierr, ndx)
                  airdensity = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  call mess(LEVEL_INFO, 'Enabled variable airdensity for windstress while reading external forcings.')
                  ja_airdensity = 1
               endif

            else if (qid == 'humidity') then

               if (.not. allocated(rhum) ) then
                  allocate ( rhum(ndx) , stat=ierr)
                  call aerr('rhum(ndx)', ierr, ndx)
                  rhum = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jarhum = 1  ; btempforcingtypH = .true.
               endif

            else if (qid == 'dewpoint') then ! Relative humidity array used to store dewpoints

               if (.not. allocated(rhum) ) then
                  allocate ( rhum(ndx) , stat=ierr)
                  call aerr('rhum(ndx)', ierr, ndx)
                  rhum = 0d0
               endif

               itempforcingtyp = 5
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jarhum = 1
                  dewpoint_available = .true.
               endif

        else if (qid == 'sea_ice_area_fraction' .or. qid == 'sea_ice_thickness') then

           ! if ice properties not yet read before, initialize ...
           if (.not. (ja_ice_area_fraction_read .or. ja_ice_thickness_read)) then
               call fm_ice_activate_by_ext_forces(ndx)
           endif
           ! add the EC link
           if (len_trim(sourcemask)>0)  then
              success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, srcmaskfile=sourcemask, varname=varname)
           else
              success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
           endif
           ! update the administration
           if (success) then
               if (qid == 'sea_ice_area_fraction') ja_ice_area_fraction_read = 1
               if (qid == 'sea_ice_thickness') ja_ice_thickness_read = 1
           endif

            else if (qid == 'cloudiness') then

               if (.not. allocated(clou) ) then
                  allocate ( clou(ndx) , stat=ierr)
                  call aerr('clou(ndx)', ierr, ndx)
                  clou = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  jaclou = 1 ; btempforcingtypC = .true.
               endif

            else if (qid == 'solarradiation') then

               if (.not. allocated(qrad) ) then
                  allocate ( qrad(ndx) , stat=ierr)
                  call aerr('qrad(ndx)', ierr, ndx)
                  qrad = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  btempforcingtypS = .true.
                  solrad_available = .true.
               endif

            else if (qid == 'longwaveradiation') then
               if (.not. allocated(longwave) ) then
                  allocate ( longwave(ndx) , stat=ierr)
                  call aerr('longwave(ndx)', ierr, ndx)
                  longwave = 0d0
               endif
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               if (success) then
                  btempforcingtypL = .true.
                  longwave_available = .true.
               endif

            else if (qid(1:8) == 'rainfall' ) then

               if (.not. allocated(rain) ) then
                  allocate ( rain(ndx) , stat=ierr)
                  call aerr('rain(ndx)', ierr, ndx)
                  rain = 0d0
               endif

               ! TODO: AvD: consider adding mask to all quantities.
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)

               if (success) then
                  jarain = 1
                  jaqin = 1
               endif

            else if (num_lat_ini_blocks == 0 .and. qid(1:16) == 'lateraldischarge' ) then

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
               call realloc(nnlat, max(2*ndxi, nlatnd+ndxi), keepExisting = .true., fill = 0)
               call selectelset_internal_nodes(xz, yz, kclat, ndxi, nnLat(nlatnd+1:), nlat, &
                                             LOCTP_POLYGON_FILE, filename)
               call realloc(n1latsg, numlatsg)
               call realloc(n2latsg, numlatsg)
               n1latsg(numlatsg) = nlatnd + 1
               n2latsg(numlatsg) = nlatnd + nlat

               nlatnd = nlatnd + nlat

               jaqin = 1
               success = .true.

            else if (jaoldstr > 0 .and. qid == 'gateloweredgelevel' ) then

               call selectelset_internal_links(xz, yz, ndx, ln, lnx, keg(ngate+1:numl), numg, LOCTP_POLYLINE_FILE, filename)
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , numg, ' nr of gate links' ; call msg_flush()

               ngatesg = ngatesg + 1
               call realloc(L1gatesg,ngatesg) ; L1gatesg(ngatesg) = ngate + 1
               call realloc(L2gatesg,ngatesg) ; L2gatesg(ngatesg) = ngate + numg

               ngate   = ngate   + numg

            else if (jaoldstr > 0 .and. qid == 'damlevel' ) then

               call selectelset_internal_links(xz, yz, ndx, ln, lnx, ked(ncdam+1:numl), numd, LOCTP_POLYLINE_FILE, filename)
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , numd, ' nr of dam level cells' ; call msg_flush()

               ncdamsg = ncdamsg + 1
               call realloc(L1cdamsg,ncdamsg) ; L1cdamsg(ncdamsg) = ncdam + 1
               call realloc(L2cdamsg,ncdamsg) ; L2cdamsg(ncdamsg) = ncdam + numd

               ncdam   = ncdam   + numd

            else if (jaoldstr > 0 .and. qid == 'generalstructure' ) then

               call selectelset_internal_links(xz, yz, ndx, ln, lnx, kegen(ncgen+1:numl), numgen, LOCTP_POLYLINE_FILE, filename, sortLinks = 1)
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , numgen, ' nr of general structure cells' ; call msg_flush()

               ncgensg = ncgensg + 1
               call realloc(L1cgensg,ncgensg) ; L1cgensg(ncgensg) = ncgen + 1
               call realloc(L2cgensg,ncgensg) ; L2cgensg(ncgensg) = ncgen + numgen

               ncgen   = ncgen + numgen

            else if (jaoldstr > 0 .and. (qid == 'pump1D' .or. qid == 'pump') ) then

               if (qid == 'pump1D') then
                  call selectelset_internal_links(xz, yz, ndx, ln, lnx1D, kep(npump+1:numl), npum, LOCTP_POLYLINE_FILE, filename, linktype = IFLTP_1D, sortLinks = 1)
               else
                  call selectelset_internal_links(xz, yz, ndx, ln, lnx, kep(npump+1:numl), npum, LOCTP_POLYLINE_FILE, filename, linktype = IFLTP_ALL, sortLinks = 1)
               endif
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , npum, ' nr of pump links' ; call msg_flush()

               npumpsg = npumpsg + 1
               call realloc(L1pumpsg,npumpsg) ; L1pumpsg(npumpsg) = npump + 1
               call realloc(L2pumpsg,npumpsg) ; L2pumpsg(npumpsg) = npump + npum

               npump   = npump   + npum

            else if (jaoldstr > 0 .and. qid == 'checkvalve' ) then

               call selectelset_internal_links(xz, yz, ndx, ln, lnx, keklep(nklep+1:numl), numklep, LOCTP_POLYLINE_FILE, filename)
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , numklep, ' nr of checkvalves ' ; call msg_flush()

               nklep = nklep + numklep
               call realloc(Lklep,nklep) ; Lklep = keklep(1:nklep)

            else if (jaoldstr > 0 .and. qid == 'valve1D' ) then

               call selectelset_internal_links(xz, yz, ndx, ln, lnx1D, kevalv(nvalv+1:numl), numvalv, LOCTP_POLYLINE_FILE, filename, linktype = IFLTP_1D )
               success = .true.
               write(msgbuf,'(a,1x,a,i8,a)') trim(qid), trim(filename) , numvalv, ' nr of valves ' ; call msg_flush()

               nvalv = nvalv + numvalv
               call realloc(Lvalv,nvalv) ; Lvalv = kevalv(1:nvalv) ; call realloc(valv,nvalv)

            else if (qid == 'discharge_salinity_temperature_sorsin') then

               ! 1. Prepare source-sink location (will increment numsrc, and prepare geometric position), based on .pli file (transformcoef(4)=AREA).
               call addsorsin(filename, transformcoef(4), ierr )
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
               success  = ec_addtimespacerelation(qid, x_dummy, y_dummy, k_dummy, kx, filename, filetype, method, operand, targetIndex = nshiptxy)

            else if (qid == 'movingstationtxy') then
               kx = 2

               rec = ' '
               call basename(filename, rec) ! rec now contains the station name.
               call addMovingObservation(dmiss, dmiss, rec)

               ! Converter will put 'x' in array(2*nummovobs-1) and 'y' in array(2*nummovobs).
               success  = ec_addtimespacerelation(qid, x_dummy, y_dummy, k_dummy, kx, filename, filetype, method, operand, targetIndex=nummovobs)

            else if (qid(1:15) == 'massbalancearea' .or. qid(1:18) == 'waqmassbalancearea') then
               if (ti_mba > 0) then
                  if ( .not. allocated(mbaname) ) then
                     allocate( mbaname(0) )
                  endif
                  imba = findname(nomba, mbaname, mbainputname)

                  if ( imba.eq.0 ) then
                     nomba = nomba + 1
                     imba = nomba
                     call realloc(mbaname,nomba,keepExisting=.true.,fill=mbainputname)
                  end if
                  call realloc(viuh,Ndkx,keepExisting=.false.,Fill=dmiss)

                  ! will only fill 2D part of viuh
                  success = timespaceinitialfield(xz, yz, viuh, Ndx, filename, filetype, method, operand, transformcoef, 2)

                  if (success) then
                     do kk=1,Ndxi
                        if (viuh(kk).ne.dmiss) then
                           if (mbadef(kk).ne. -999) then
                              ! warn that segment nn at xx, yy is nog mon area imba
                           endif
                           mbadef(kk) = imba
                           call getkbotktop(kk,kb,kt)
                           do k=kb,kb+kmxn(kk)-1
                              mbadef(k) = imba
                           end do
                        endif
                     end do
                  endif
                  deallocate(viuh)
               else
                  call qnerror('Quantity massbalancearea in the ext-file, but no MbaInterval specified in the mdu-file.', ' ', ' ')
                  success = .false.
               endif

            else if (qid(1:12) == 'waqparameter' .or. qid(1:17) == 'waqmonitoringarea' .or. qid(1:16) == 'waqsegmentnumber') then
               ! Already taken care of in fm_wq_processes
               success  =  .true.

            else if (qid(1:11) == 'waqfunction') then
               success = ec_addtimespacerelation(qid, x_dummy, y_dummy, k_dummy, kx, filename, filetype, method, operand)

            else if (qid(1:18) == 'waqsegmentfunction') then
               success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)

            else if (qid(1:25) == 'bedrock_surface_elevation') then
               kx=1
               if (allocated(subsupl)) deallocate(subsupl, subsupl_t0, subsupl_tp, subsout, sdu_blp)

               select case (ibedlevtyp)
                  case (1)
                     allocate ( subsupl(ndx) , stat=ierr)
                     call aerr('subsupl(ndx)', ierr, ndx)
                     subsupl = 0d0
                     allocate ( subsupl_t0(ndx) , stat=ierr)
                     call aerr('subsupl_t0(ndx)', ierr, ndx)
                     subsupl_t0 = 0d0
                     allocate ( subsupl_tp(ndx) , stat=ierr)
                     call aerr('subsupl_tp(ndx)', ierr, ndx)
                     subsupl_tp = 0d0
                     allocate ( subsout(ndx) , stat=ierr)
                     call aerr('subsout(ndx)', ierr, ndx)
                     subsout = 0d0
                     success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand)

                  case (2)
                     if (allocated(mask)) deallocate(mask)
                     allocate(mask(lnx), stat=ierr)
                     call aerr('mask(lnx)', ierr, lnx)
                     mask = 1
                     allocate ( subsupl(lnx) , stat=ierr)
                     call aerr('subsupl(lnx)', ierr, lnx)
                     subsupl = 0d0
                     allocate ( subsupl_t0(lnx) , stat=ierr)
                     call aerr('subsupl_t0(lnx)', ierr, lnx)
                     subsupl_t0 = 0d0
                     allocate ( subsupl_tp(lnx) , stat=ierr)
                     call aerr('subsupl_tp(lnx)', ierr, lnx)
                     subsupl_tp = 0d0
                     allocate ( subsout(lnx) , stat=ierr)
                     call aerr('subsout(lnx)', ierr, lnx)
                     subsout = 0d0
                     success = ec_addtimespacerelation(qid, xu, yu, mask, kx, filename, filetype, method, operand, varname=varname)

                  case (3,4,5,6)
                     if (allocated(mask)) deallocate(mask)
                     allocate(mask(numk), stat=ierr)
                     call aerr('mask(numk)', ierr, numk)
                     mask = 1
                     allocate ( subsupl(numk) , stat=ierr)
                     call aerr('subsupl(numk)', ierr, numk)
                     subsupl = 0d0
                     allocate ( subsupl_t0(numk) , stat=ierr)
                     call aerr('subsupl_t0(numk)', ierr, numk)
                     subsupl_t0 = 0d0
                     allocate ( subsupl_tp(numk) , stat=ierr)
                     call aerr('subsupl_tp(numk)', ierr, numk)
                     subsupl_tp = 0d0
                     allocate ( subsout(numk) , stat=ierr)
                     call aerr('subsout(numk)', ierr, numk)
                     subsout = 0d0
                     success = ec_addtimespacerelation(qid, xk(1:numk), yk(1:numk), mask, kx, filename, filetype, method, operand, varname=varname)
               end select
               allocate ( sdu_blp(ndx) , stat=ierr)
               call aerr('sdu_blp(ndx)', ierr, ndx)
               sdu_blp = 0d0

               if (success) then
                  jasubsupl = 1
               endif

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
               endif
           else if (trim(qid) == "waveperiod") then
               if (jawave == 6 .or. jawave == 7) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "waveperiod" found but "Wavemodelnr" is not 6 or 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "waveperiod" found but "Wavemodelnr" is not 6 or 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "wavedirection") then
               if (jawave == 7) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif    
           else if (trim(qid) == "freesurfacedissipation") then
               ! wave forces based on dissipation at free surface and water column
               if (jawave == 7 .and. waveforcing == 3) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "whitecappingdissipation") then
               ! wave forces based on dissipation at free surface and water column
               if (jawave == 7 .and. waveforcing == 3) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "xwaveforce") then
               if (jawave == 7 .and. (waveforcing == 1 .or. waveforcing == 3)) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "ywaveforce") then
               if (jawave == 7 .and. (waveforcing == 1 .or. waveforcing == 3)) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif
           else if (trim(qid) == "totalwaveenergydissipation") then
               if (jawave == 7 .and. waveforcing == 2) then
                  success = ec_addtimespacerelation(qid, xz, yz, kcs, kx, filename, filetype, method, operand, varname=varname)
               else
                  call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7')
                  call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'QUANTITY "'''//trim(qid)//'''" found but "Wavemodelnr" is not 7', trim(qid))
                  success = .false.
               endif    
           else
              call mess(LEVEL_WARN, 'Reading *.ext forcings file '''//trim(md_extfile)//''', getting unknown QUANTITY '//trim(qid) )
              call qnerror('Reading *.ext forcings file '''//trim(md_extfile)//''', ', 'getting unknown QUANTITY ', trim(qid) )
              success = .false.
           endif

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
            endif

         endif

      enddo
      call timstop(handle_extra(50)) ! extforcefile old

   end subroutine init_old
   
end submodule old_initialisation